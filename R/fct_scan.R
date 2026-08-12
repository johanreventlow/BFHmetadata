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
#' @return list(diagram_id, status, signal, n_obs, slice, qic_result, summary)
#' @noRd
scan_diagram <- function(row, base_path, medians_df, variants_df, window_n = NULL,
                         slice_loader = NULL, period = NULL) {
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
            list(
              diagram_id = row$diagram_id, status = "ok", signal = isTRUE(sig$signal),
              n_obs = length(unique(as.Date(slice$dato))), slice = slice,
              qic_result = sig$qic_result, summary = sig$summary_all,
              n_ignored_breaks = as.integer(n_ignored)
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

#' Subset diagram-indeks på et named filter (AND). Tomme/NULL-værdier ignoreres.
#' @noRd
apply_index_filters <- function(index, filters) {
  keep <- rep(TRUE, nrow(index))
  for (col in names(filters)) {
    val <- filters[[col]]
    # length-0-guard FØR nzchar: tom character(0) (fx multi-select) → spring over
    if (is.null(val) || length(val) == 0L || !nzchar(val) || !col %in% names(index)) next
    keep <- keep & !is.na(index[[col]]) & index[[col]] == val
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
#' NULL ind → NULL ud (skelner "ej scannet endnu" fra "tom visning").
#' @noRd
scan_view_filter <- function(scanned, show_all = FALSE) {
  if (is.null(scanned)) {
    return(NULL)
  }
  if (isTRUE(show_all)) {
    return(scanned)
  }
  scanned[scanned$signal %in% TRUE, , drop = FALSE]
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
  fmt <- function(x) ifelse(is.na(x), "–", as.character(x))
  out <- data.frame(
    Fase = as.integer(summary$fase),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out[["Obs."]] <- sprintf(
    "%s (%s anv.)", fmt(summary$antal_observationer),
    fmt(summary$anvendelige_observationer)
  )
  out[["Serielængde"]] <- sprintf(
    "%s / maks. %s",
    fmt(summary$laengste_loeb), fmt(summary$laengste_loeb_max)
  )
  out[["Antal kryds"]] <- sprintf(
    "%s / min. %s",
    fmt(summary$antal_kryds), fmt(summary$antal_kryds_min)
  )
  out[["Signal"]] <- ifelse(summary$anhoej_signal %in% TRUE,
    "⚠ signal", "–"
  )
  out
}
