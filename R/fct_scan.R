# Headless scan-lag for signal-gennemgang. Sidder oven på Fase A-motoren
# (parquet/signal) + DB-accessors. Ingen Shiny-state → ren + testbar.

#' Byg parquet-enhed-filter (lowercase varianter) for ét org_id ud fra
#' org_enhed_variants()-df (org-navne + tblOrganisationOversaettelse-fra-data).
#' @noRd
enhed_variants_for <- function(variants_df, org_id) {
  if (is.null(variants_df) || nrow(variants_df) == 0) {
    return(character(0))
  }
  rows <- variants_df[variants_df$org_id == org_id, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(character(0))
  }
  # teknisk/kort/langt kommer fra org-siden af LEFT JOIN → identiske for alle
  # rækker af samme org_id; derfor [1]. fra_data varierer pr. oversættelse.
  v <- c(rows$fra_data, rows$teknisk[1], rows$kort[1], rows$langt[1])
  v <- tolower(v[!is.na(v) & nzchar(v)])
  unique(v)
}

#' Nulfyld tomme perioder i en tælle-serie. Vendored fra BFHddl
#' (fill_empty_periods + pipeline §5.2a', se BFHddl DATA_CONVENTIONS §3b):
#' en tom periode i en hændelsestælling ER et 0-punkt — uden fyld beregnes
#' centerlinjen kun på perioder MED hændelser, og scannet ville se en anden
#' serie end den BFHddl tegner.
#'
#' Kun tælle-serier fyldes: nævner-serier (andele/rater) og værdi-serier må
#' aldrig 0-udfyldes. Scan-slices bærer ALLE kolonner (NA for ubrugte), så
#' guarden bruger "har data"-semantik som compute_signal — ikke kolonne-
#' tilstedeværelse som BFHddl's pipeline-guard.
#'
#' Fyldes fra seriens første observation frem til seneste AFSLUTTEDE periode
#' (også efter sidste hændelse — forudsætter levende dataleverance). Én
#' 0-række pr. manglende periode: qic summerer pr. dato, så resultatet
#' matcher BFHddl's per-gruppe-fyld.
#' @param slice slice EFTER periode-aggregering
#' @param period_en lubridate-enhed ("day"/"week"/"month"/"quarter"/"year")
#' @param today injicérbar "i dag" (test) — styrer seneste afsluttede periode
#' @noRd
scan_fill_empty_periods <- function(slice, period_en, today = Sys.Date()) {
  if (is.null(slice) || nrow(slice) == 0 || !"dato" %in% names(slice)) {
    return(slice)
  }
  has_data <- function(col) col %in% names(slice) && any(!is.na(slice[[col]]))
  if (!has_data("taeller") || has_data("naevner") || has_data("vaerdi")) {
    return(slice)
  }
  wk <- getOption("lubridate.week.start", 1)
  dates <- as.Date(slice$dato)
  # Seneste afsluttede periode: dagen før indeværende periodes start,
  # floor'et til periodestart (BFHddl: until = floor(today)-1 → floor(until))
  until_start <- lubridate::floor_date(
    lubridate::floor_date(as.Date(today), unit = period_en, week_start = wk) - 1,
    unit = period_en, week_start = wk
  )
  slut <- max(dates)
  if (until_start > slut) slut <- until_start
  full <- seq.Date(min(dates), slut, by = period_en)
  missing <- full[!full %in% dates]
  if (length(missing) == 0) {
    return(slice)
  }
  # Skabelon = første række (id-kolonner som indikator/enhed er konstante i
  # et scan-slice; måle-kolonner naevner/vaerdi er alle-NA jf. guarden)
  new_rows <- slice[rep(1L, length(missing)), , drop = FALSE]
  slice$dato <- dates # normalisér til Date så rbind ikke blander typer
  new_rows$dato <- missing
  new_rows$taeller <- 0
  out <- rbind(slice, new_rows)
  rownames(out) <- NULL
  out[order(out$dato), , drop = FALSE]
}

#' Scan ét diagram: byg enhed-filter → hent fuldt indikator-slice → filtrér
#' in-memory → (vindue) → resolve median-knæk → compute_signal. Fanger fejl
#' pr. diagram (safe_operation).
#' @param row liste/df-række med indikator_navn_teknisk, org_id, diagram_id
#' @param base_path bruger-valgt parquet-rodmappe
#' @param medians_df alle median-rækker for diagrammet (kolonner diagram, laas_median) el. NULL
#' @param variants_df org_enhed_variants()-output
#' @param window_n behold seneste N observationer (NULL = alle)
#' @param slice_loader valgfri 0-args funktion der leverer indikatorens FULDE
#'   slice (alle enheder). Bruges til per-indikator-genbrug: flere diagrammer
#'   på samme indikator deler ét arrow-load (evt. via dags-cache) i stedet for
#'   at genåbne datasættet pr. diagram. NULL = load selv fra base_path.
#' @param period diagrammets periode_aggregering — dansk værdi ("uge",
#'   "maaned", ...) el. lubridate-enhed ("week"/"month"); begge accepteres via
#'   period_to_en. NULL/tom = ingen aggregering. Uden dette beregnes signalet
#'   på en ANDEN serie end den BFHddl tegner — se fct_period.R.
#'   Styrer også hvilke median-knæk der er gyldige: knæk sat under en anden
#'   aggregering ignoreres (filter_medians_by_period).
#' @param org_struct db$org_struct()-lignende df (id, parent_id) — hele
#'   org-træet. NULL slår hierarki-oprulning fra (samme som før denne
#'   parameter fandtes).
#' @param agg_flags db$aggregation_flags()-lignende df (org_id, indikator_id,
#'   indgaar). NULL slår hierarki-oprulning fra.
#' @return list(diagram_id, status, signal, n_obs, slice, qic_result, summary)
#' @noRd
scan_diagram <- function(row, base_path, medians_df, variants_df, window_n = NULL,
                         slice_loader = NULL, period = NULL,
                         org_struct = NULL, agg_flags = NULL) {
  empty <- function(status) {
    list(
      diagram_id = row$diagram_id, status = status,
      signal = FALSE, n_obs = 0L, slice = NULL, qic_result = NULL, summary = NULL
    )
  }
  # Værdi-givende if/else (ingen non-local return ud af safe_operation-blokken):
  # blokkens sidste udtryk er resultatet → fallback="fejl" rammes kun ved fejl.
  safe_operation(sprintf("scan diagram %s", row$diagram_id),
    {
      variants <- enhed_variants_for(variants_df, row$org_id)
      if (length(variants) == 0) {
        # Seriediagrammer er org-scopede: uden enhed-varianter kan slicet ikke
        # afgrænses til rette enhed → "ingen_data" frem for signal på blandede
        # enheder. (Rigtige org'er har altid navne → rammer ej normal-flow.)
        empty("ingen_data")
      } else {
        full <- if (!is.null(slice_loader)) {
          slice_loader()
        } else {
          parquet_load_slice(
            parquet_indicator_path(base_path, row$indikator_navn_teknisk)
          )
        }
        slice <- slice_filter_enhed(full, variants)
        aggregated <- FALSE
        agg_kids <- integer(0)
        if (is.null(slice) || nrow(slice) == 0) {
          # Fallback (spejler BFHddl trin 5.1b): direkte match vandt ikke →
          # forsøg hierarki-oprulning fra det fulde slice. NULL = kan ikke
          # oprulles → ingen_data som hidtil.
          # Boerne-listen beregnes HER (ét kald) og genbruges til n_agg_units
          # nedenfor — undgaar et dobbelt find_aggregation_children-kald. Er
          # kids tom, springes adapteren helt over (den ville blot returnere
          # NULL igen, men uden at spilde vaerdi-guard-arbejdet undervejs).
          agg_kids <- if (!is.null(org_struct) && !is.null(agg_flags)) {
            find_aggregation_children(
              row$org_id, row$indikator_id,
              org_struct, agg_flags
            )
          } else {
            integer(0)
          }
          slice <- if (length(agg_kids) > 0) {
            aggregate_slice_for_center(
              full, row$org_id, row$indikator_id, row$org_teknisk,
              org_struct, agg_flags, variants_df
            )
          } else {
            NULL
          }
          aggregated <- !is.null(slice) && nrow(slice) > 0
        }
        if (is.null(slice) || nrow(slice) == 0) {
          empty("ingen_data")
        } else {
          # Aggregér FØR vindue-begrænsningen (BFHddl-orden: §5.1 → §5.2a →
          # §5.2b). Omvendt rækkefølge ville begrænse til N *dage* og derefter
          # bucke dem — målt på produktionsdata: 24 punkter → 4.
          # Ingen tryCatch-fallback til uaggregeret data (modsat BFHddl's
          # pipeline): her ville det tavst genskabe præcis den bug vi fikser.
          # Fejl bobler til safe_operation → status "fejl".
          p <- period_to_en(period)
          if (!identical(p, "day")) {
            slice <- aggregate_to_period(slice, period = p)
          }
          # drop_incomplete kan tømme et slice der HAVDE rækker (serie hvis
          # eneste data ligger i indeværende periode) → guard igen efter agg.
          # Værdi-givende if/else, ej return(): en non-local return ville springe
          # ud af safe_operation-blokken (se kommentar øverst).
          if (nrow(slice) == 0) {
            empty("ingen_data")
          } else {
            # Nulfyld tomme perioder (opt-in-flag fra tblIndikatorer) —
            # EFTER aggregering, FØR vinduet (BFHddl-orden §5.2a' → §5.2b):
            # omvendt ville "seneste N" tælles på serien uden 0-punkterne.
            if (isTRUE(as.logical(row$nulfyld_tomme_perioder %||% FALSE))) {
              slice <- scan_fill_empty_periods(slice, p)
            }
            if (!is.null(window_n)) slice <- parquet_limit_observations(slice, window_n)
            slice <- slice[order(slice$dato), , drop = FALSE]
            # Knæk sat under en ANDEN aggregering ignoreres: deres dato ville
            # lande på en fase-grænse der aldrig var tilsigtet. n_ignored
            # rapporteres, så UI'et kan vise det (ellers ændres faserne usynligt).
            meds_ok <- filter_medians_by_period(medians_df, period)
            n_ignored <- if (is.data.frame(medians_df)) {
              nrow(medians_df) - nrow(meds_ok)
            } else {
              0L
            }
            parts <- resolve_median_breaks(row$diagram_id, meds_ok, slice$dato)
            sig <- compute_signal(slice, parts = parts)
            # Genbrug af agg_kids fra ovenfor (ét find_aggregation_children-kald
            # pr. diagram, ej to) — se kommentar ved beregningen.
            n_agg_units <- if (aggregated) length(agg_kids) else 0L
            list(
              diagram_id = row$diagram_id, status = "ok", signal = isTRUE(sig$signal),
              signal_type = sig$signal_type %||% NA_character_,
              # Eksisterende knæk i DB (uanset aggregering) — driver
              # "vis også diagrammer med knæk"-visningen i gennemgangen
              n_breaks = if (is.data.frame(medians_df)) nrow(medians_df) else 0L,
              n_obs = length(unique(as.Date(slice$dato))), slice = slice,
              qic_result = sig$qic_result, summary = sig$summary_all,
              n_ignored_breaks = as.integer(n_ignored),
              aggregated = aggregated, n_agg_units = as.integer(n_agg_units)
            )
          }
        }
      }
    },
    fallback = empty("fejl")
  )
}

# De 5 filter-dimensioner (kolonnenavne i diagram-indekset).
.SIGNAL_FILTER_DIMS <- c(
  "overafdeling", "afsnit", "datapakke",
  "datasaet", "indikator_navn"
)

#' Sorterede unikke valg pr. filter-dimension (NA/tomme droppes).
#' @noRd
index_filter_choices <- function(index) {
  stats::setNames(lapply(.SIGNAL_FILTER_DIMS, function(col) {
    v <- index[[col]]
    v <- v[!is.na(v) & nzchar(v)]
    sort(unique(v))
  }), .SIGNAL_FILTER_DIMS)
}

#' Kaskade-valg til signal-filtrene: datasaet begraenses af valgt datapakke,
#' indikator_navn af datapakke + datasaet. En dimension begraenses ALDRIG af
#' sit eget valg (saa man kan skifte vaerdi uden at rydde foerst).
#' @param filters named list af multi-select-vektorer (tom/NULL = alle)
#' @return list(datasaet = chr, indikator_navn = chr) — sorterede unikke valg
#' @noRd
signal_cascade_choices <- function(index, filters) {
  under <- function(dims) {
    idx <- apply_index_filters(index, filters[intersect(dims, names(filters))])
    index_filter_choices(idx)
  }
  list(
    datasaet = under("datapakke")$datasaet,
    indikator_navn = under(c("datapakke", "datasaet"))$indikator_navn
  )
}

#' Subset diagram-indeks på et named filter (AND på tværs af dimensioner).
#' Hver dimension kan have flere værdier (multi-select) = OR inden for
#' dimensionen. Tomme/NULL-værdier ignoreres.
#' @noRd
apply_index_filters <- function(index, filters) {
  keep <- rep(TRUE, nrow(index))
  for (col in names(filters)) {
    val <- filters[[col]]
    if (is.null(val) || !col %in% names(index)) next
    # Multi-select kan indeholde tomme strenge/NA — drop dem før match;
    # helt tom vektor (ryddet felt) = intet filter for dimensionen.
    val <- val[!is.na(val) & nzchar(val)]
    if (length(val) == 0L) next
    keep <- keep & index[[col]] %in% val # NA i index matcher aldrig
  }
  index[keep, , drop = FALSE]
}

#' Split et batch-hentet median-datasæt op pr. diagram-id.
#' Diagrammer uden knæk får en TOM df (ej NULL) med samme kolonner, så
#' resolve_median_breaks() rammer sin normale nrow==0-guard.
#' @param medians_df samlet df fra db$diagram_medians_batch() (må være NULL)
#' @param diagram_ids alle id'er der skal have en indgang
#' @return named list (nøgle = as.character(diagram_id)) → df
#' @noRd
medians_by_diagram <- function(medians_df, diagram_ids) {
  ids <- as.character(diagram_ids)
  empty <- if (!is.null(medians_df) && is.data.frame(medians_df)) {
    medians_df[0, , drop = FALSE]
  } else {
    data.frame(
      id = integer(0), diagram = integer(0),
      laas_median = as.Date(character(0))
    )
  }
  if (is.null(medians_df) || !is.data.frame(medians_df) ||
    nrow(medians_df) == 0 || !"diagram" %in% names(medians_df)) {
    return(stats::setNames(rep(list(empty), length(ids)), ids))
  }
  parts <- split(medians_df, as.character(medians_df$diagram))
  stats::setNames(lapply(ids, function(k) parts[[k]] %||% empty), ids)
}

#' Part-positioner for et forhåndsvist faseskift: eksisterende median-knæk +
#' ét ekstra. Alt normaliseres til Date FØR resolve, så preview og save bruger
#' samme dato-semantik (undgår rbind-coercion af Date på en POSIXct-kolonne fra
#' DB → ingen TZ-/type-tvetydighed mellem de to stier).
#' @param base_meds df med kolonnen laas_median (Date/POSIXct/character fra DB)
#' @param extra_date ekstra knæk-dato (ISO-streng el. Date)
#' @noRd
preview_break_parts <- function(diagram_id, base_meds, extra_date, x_dates) {
  laas <- c(as.Date(base_meds$laas_median), as.Date(extra_date))
  all_meds <- data.frame(diagram = diagram_id, laas_median = laas)
  resolve_median_breaks(diagram_id, all_meds, x_dates)
}

#' Filtrér den scannede liste til visning. show_all = FALSE → kun diagrammer
#' med signal; TRUE → alle rækker (scanned indeholder kun ok-scannede —
#' ingen_data/fejl optages aldrig, de har ingen tegnbar graf).
#' show_breaks = TRUE medtager desuden diagrammer UDEN signal der har
#' eksisterende median-knæk (has_breaks-kolonnen); mangler kolonnen
#' (ældre kaldere) degraderes til kun-signal.
#' NULL ind → NULL ud (skelner "ej scannet endnu" fra "tom visning").
#' @noRd
scan_view_filter <- function(scanned, show_all = FALSE, show_breaks = FALSE) {
  if (is.null(scanned)) {
    return(NULL)
  }
  if (isTRUE(show_all)) {
    return(scanned)
  }
  keep <- scanned$signal %in% TRUE
  if (isTRUE(show_breaks) && "has_breaks" %in% names(scanned)) {
    keep <- keep | scanned$has_breaks %in% TRUE
  }
  scanned[keep, , drop = FALSE]
}

#' Sortér visningen efter signal-type: begge → serie → kryds → (andet) →
#' knæk-uden-signal → øvrige. Stabil inden for grupper (scan-rækkefølge
#' bevares). by_type = FALSE el. NULL ind → uændret retur.
#' @noRd
scan_view_sort <- function(scanned, by_type = FALSE) {
  if (is.null(scanned) || !isTRUE(by_type) || nrow(scanned) == 0) {
    return(scanned)
  }
  st <- if ("signal_type" %in% names(scanned)) {
    as.character(scanned$signal_type)
  } else {
    rep(NA_character_, nrow(scanned))
  }
  hb <- if ("has_breaks" %in% names(scanned)) {
    scanned$has_breaks %in% TRUE
  } else {
    rep(FALSE, nrow(scanned))
  }
  rank <- match(st, c("begge", "serie", "kryds", "andet"))
  rank[is.na(rank)] <- ifelse(hb[is.na(rank)], 5L, 6L)
  scanned[order(rank, seq_len(nrow(scanned))), , drop = FALSE]
}

#' Fase-statistik-df til visning under grafen. Spejler PDF-rapporternes
#' SPC-tabel: observeret serielængde mod forventet maks., observeret antal
#' kryds mod forventet min. — pr. fase, fra BFHcharts format_qic_summary
#' (bfh_qic-resultatets $summary).
#' @param summary df med fase, antal_observationer, anvendelige_observationer,
#'   laengste_loeb(_max), antal_kryds(_min), anhoej_signal — el. NULL
#' @return df med danske visningskolonner el. NULL (intet at vise)
#' @noRd
phase_stats_df <- function(summary) {
  if (is.null(summary) || !is.data.frame(summary) || nrow(summary) == 0) {
    return(NULL)
  }
  fmt <- function(x) ifelse(is.na(x), "\u2013", as.character(x))
  out <- data.frame(
    Fase = as.integer(summary$fase),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out[["Obs."]] <- sprintf(
    "%s (%s anv.)", fmt(summary$antal_observationer),
    fmt(summary$anvendelige_observationer)
  )
  out[["Seriel\u00E6ngde"]] <- sprintf(
    "%s / maks. %s",
    fmt(summary$laengste_loeb), fmt(summary$laengste_loeb_max)
  )
  out[["Antal kryds"]] <- sprintf(
    "%s / min. %s",
    fmt(summary$antal_kryds), fmt(summary$antal_kryds_min)
  )
  out[["Signal"]] <- ifelse(summary$anhoej_signal %in% TRUE,
    "\u26A0 signal", "\u2013"
  )
  out
}
