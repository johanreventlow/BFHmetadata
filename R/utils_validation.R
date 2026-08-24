#' Validér indikator-værdier før gem. Returnerer char-vektor af fejl (tom = OK).
#' Konservativ: kun praktiske krav (skema tillader NULL på det meste).
#' @noRd
validate_indikator <- function(values) {
  errs <- character(0)
  nm <- values[["indikator_navn"]]
  if (is.null(nm) || is.na(nm) || !nzchar(trimws(as.character(nm %||% "")))) {
    errs <- c(errs, "indikator_navn m\u00E5 ikke v\u00E6re tom")
  }
  ao <- values[["antal_observationer"]]
  if (!is.null(ao) && !is.na(ao) && nzchar(as.character(ao))) {
    if (is.na(suppressWarnings(as.numeric(ao)))) {
      errs <- c(errs, "antal_observationer skal v\u00E6re et tal")
    }
  }
  errs
}

#' Valider diagram-form-værdier. Returnerer character() hvis OK.
#' Kun FK-felterne er obligatoriske — periode/flag må være tomme (skema tillader
#' NULL, og eksisterende data har huller).
#' @noRd
validate_diagram <- function(vals) {
  errs <- character(0)
  if (is.na(vals$indikator)) errs <- c(errs, "Indikator er obligatorisk")
  if (is.na(vals$organisatorisk_navn_teknisk))
    errs <- c(errs, "Organisatorisk enhed er obligatorisk")
  if (is.na(vals$diagram_type)) errs <- c(errs, "Diagramtype er obligatorisk")
  errs
}

#' Validér mål-form-værdier (tblDiagrammerMaal). Returnerer character() hvis OK.
#' Diagram + værdi er obligatoriske; retning/dato må være tomme (skema
#' tillader NULL).
#' @noRd
validate_maal <- function(vals) {
  errs <- character(0)
  if (is.null(vals$diagram) || is.na(vals$diagram)) {
    errs <- c(errs, "Diagram er obligatorisk")
  }
  vv <- vals$maal_vaerdi
  if (is.null(vv) || is.na(vv)) {
    errs <- c(errs, "Målværdi er obligatorisk")
  } else if (is.na(suppressWarnings(as.numeric(vv)))) {
    errs <- c(errs, "Målværdi skal være et tal")
  }
  errs
}

#' @noRd
.preserved_filter_selection <- function(current, choices, fallback = "") {
  if (length(current) != 1L || is.na(current)) return(fallback)
  current <- as.character(current)
  if (current %in% as.character(unname(choices))) current else fallback
}

#' NULL-coalesce
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
