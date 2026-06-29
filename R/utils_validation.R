#' Validér indikator-værdier før gem. Returnerer char-vektor af fejl (tom = OK).
#' Konservativ: kun praktiske krav (skema tillader NULL på det meste).
#' @noRd
validate_indikator <- function(values) {
  errs <- character(0)
  nm <- values[["indikator_navn"]]
  if (is.null(nm) || is.na(nm) || !nzchar(trimws(as.character(nm %||% "")))) {
    errs <- c(errs, "indikator_navn må ikke være tom")
  }
  ao <- values[["antal_observationer"]]
  if (!is.null(ao) && !is.na(ao) && nzchar(as.character(ao))) {
    if (is.na(suppressWarnings(as.numeric(ao)))) {
      errs <- c(errs, "antal_observationer skal være et tal")
    }
  }
  errs
}

#' NULL-coalesce
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
