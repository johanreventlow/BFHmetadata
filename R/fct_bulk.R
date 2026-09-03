# Rene hjælpere til bulk-redigerings-UI'et (Leverance 4 af
# docs/plans/2026-08-30-bulk-redigering-design.md). Ingen reaktiv kontekst →
# testbare uden Shiny. Selve skrivningen ligger i db$bulk_update/db$bulk_undo
# (fct_db.R); her bygges kun valgmuligheder, visning og forhåndsvisning.

#' Felt-dropdownens valg: de allowlistede kolonner for tabellen, vist med
#' danske labels. Rækkefølgen følger allowlisten, så beslægtede felter (fx de
#' fire flag) står samlet. Kolonner uden label vises med deres eget navn frem
#' for at forsvinde.
#' @noRd
bulk_field_choices <- function(tabel_key, labels = character(0),
                               tables = BULK_TABLES) {
  cfg <- tables[[tabel_key]]
  if (is.null(cfg)) {
    return(character(0))
  }
  cols <- vapply(cfg$fields, function(f) f$col, "")
  navne <- vapply(cols, function(col) {
    lab <- labels[[col]]
    if (is.null(lab) || is.na(lab) || !nzchar(lab)) col else lab
  }, "")
  stats::setNames(cols, unname(navne))
}

#' Vis en typet værdi som tekst til forhåndsvisningen. FK'er vises med deres
#' label (et rå id siger brugeren intet), bool som Ja/Nej, tom værdi som
#' "(tom)" — så en tømning ikke ser ud som en manglende celle.
#' choices = named vektor (label → id) for feltet, som i grid'ets dropdowns.
#' @noRd
bulk_display_value <- function(fld, value, choices = NULL) {
  if (length(value) == 0L || is.na(value)) {
    return("(tom)")
  }
  if (identical(fld$kind, "bool")) {
    return(if (isTRUE(value)) "Ja" else "Nej")
  }
  if (identical(fld$kind, "fk") && length(choices) > 0) {
    hit <- match(as.character(value), as.character(unname(choices)))
    if (!is.na(hit)) {
      return(as.character(names(choices)[hit]))
    }
    return(sprintf("#%s (ukendt)", value)) # id peger uden for listen
  }
  as.character(value)
}

#' Forhåndsvisning af en batch: én række pr. ramt post med nuværende og ny
#' værdi. `uaendret` markerer de rækker der allerede HAR målværdien — de
#' skrives ikke (bulk_update rapporterer dem som "skipped"), og de tælles
#' derfor ikke med i knappens antal.
#'
#' d = de FROSNE rækker (data.frame), pk/navn_col = kolonnenavne i d,
#' fld = elementet fra bulk-allowlisten, target = den typede målværdi.
#' @noRd
bulk_preview_df <- function(d, pk, navn_col, fld, target, choices = NULL) {
  kol <- if (fld$col %in% names(d)) d[[fld$col]] else rep(NA, nrow(d))
  # Førværdierne typekonverteres med feltets egen kind, så sammenligningen mod
  # målværdien sker på samme grundlag som DB-lagets (allow_blank: en
  # eksisterende værdi må godt være tom, også for bool/fk).
  nuv <- lapply(seq_len(nrow(d)), function(i) {
    bulk_coerce_value(fld, kol[i], allow_blank = TRUE)
  })
  data.frame(
    id = as.character(d[[pk]]),
    indikator = if (navn_col %in% names(d)) as.character(d[[navn_col]]) else "",
    nuvaerende = vapply(nuv, function(v) bulk_display_value(fld, v, choices), ""),
    ny = rep(bulk_display_value(fld, target, choices), nrow(d)),
    uaendret = vapply(nuv, function(v) identical(v, target), logical(1)),
    stringsAsFactors = FALSE
  )
}

#' Førværdierne som bulk_update kræver dem: named liste, navne = id'erne som
#' tekst. Det er dem der sammenlignes med databasens under lås, så en række
#' der er ændret af en anden bruger siden forhåndsvisningen bliver opdaget.
#' @noRd
bulk_expected_before <- function(d, pk, fld) {
  kol <- if (fld$col %in% names(d)) d[[fld$col]] else rep(NA, nrow(d))
  stats::setNames(
    lapply(seq_len(nrow(d)), function(i) kol[i]),
    as.character(d[[pk]])
  )
}

# --- Diagram-specifikke guards ----------------------------------------------
# Et diagram er kun gyldigt som HEL række (indikator + enhed + type skal alle
# være sat), og duplikatnøglen går på tværs af tre felter. En bulk skal derfor
# vurderes på den patchede række, ikke på det ene felt der ændres.

# Felterne der tilsammen udgør duplikatnøglen. Kun når en bulk rammer ét af
# dem, kan den skabe nye dubletter.
.DIAGRAM_DUPLIKAT_NOEGLE <- c(
  "indikator", "organisatorisk_navn_teknisk", "diagram_type"
)

#' Rammer bulk-feltet duplikatnøglen? Kun da er det meningsfuldt at køre
#' duplikat-guarden bagefter.
#' @noRd
bulk_diagram_rammer_duplikatnoegle <- function(felt) {
  isTRUE(felt %in% .DIAGRAM_DUPLIKAT_NOEGLE)
}

#' Validér en diagram-bulk FØR den skrives: hver ramt række patches med
#' målværdien og valideres som HEL række (validate_diagram). Uden det kunne en
#' bulk tavst "reparere" en række der mangler obligatoriske FK'er — eller
#' omvendt gøre en gyldig række ugyldig — og fejlen ville først vise sig som en
#' constraint-fejl midt i batchen.
#'
#' d = de frosne admin-rækker, felt = kolonnen der sættes, target = den typede
#' målværdi. Returnerer data.frame(id, fejl) for de rækker der IKKE validerer
#' (0 rækker = alt er i orden).
#' @noRd
bulk_diagram_validation_errors <- function(d, felt, target) {
  tom <- data.frame(id = character(0), fejl = character(0),
                    stringsAsFactors = FALSE)
  if (is.null(d) || nrow(d) == 0) {
    return(tom)
  }
  fejl <- lapply(seq_len(nrow(d)), function(i) {
    vals <- .diagram_row_values(d[i, ])
    vals[[felt]] <- target
    errs <- validate_diagram(vals)
    if (length(errs) == 0) {
      return(NULL)
    }
    data.frame(
      id = as.character(d[["diagram_id"]][i]),
      fejl = paste(errs, collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
  fejl <- Filter(Negate(is.null), fejl)
  if (length(fejl) == 0) tom else do.call(rbind, fejl)
}

#' Kort dansk opsummering af en konflikt fra bulk_update/bulk_undo, til
#' status-linjen. Rapporten skal kunne læses uden at kende id-numrene udenad,
#' så antallet står først og id'erne begrænses.
#' @noRd
bulk_conflict_text <- function(e, maks = 10L) {
  ids <- e$ids %||% character(0)
  vist <- utils::head(ids, maks)
  hale <- if (length(ids) > maks) sprintf(" (+%d flere)", length(ids) - maks) else ""
  aarsag <- switch(e$type %||% "",
    duplicate = "samme række er valgt flere gange",
    missing = "rækkerne findes ikke længere",
    stale = "rækkerne er ændret af en anden siden forhåndsvisningen",
    undo_conflict = "rækkerne er ændret siden batchen",
    "konflikt"
  )
  sprintf("Intet skrevet — %d %s: %s%s", length(ids), aarsag,
          paste(vist, collapse = ", "), hale)
}
