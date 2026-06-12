# Vendored fra BFHddl (data_loader.R), Supabase-projekt: ingen config/cli-afhængighed.
# Parquet er folder-pr-indikator; arrow håndterer dato-partitionering automatisk.

#' Find mappen for én indikators parquet (direkte, ellers 1 niveau ned).
#' 1-niveau-grænsen undgår scan af ~67k dato-partition-mapper.
#' @noRd
parquet_indicator_path <- function(base_path, indikator_navn_teknisk) {
  direct <- file.path(base_path, indikator_navn_teknisk)
  if (dir.exists(direct)) return(direct)
  for (sub in list.dirs(base_path, recursive = FALSE, full.names = TRUE)) {
    cand <- file.path(sub, indikator_navn_teknisk)
    if (dir.exists(cand)) return(cand)
  }
  direct  # fejler downstream med klar besked
}

#' Indlæs én indikators parquet-slice, filtreret på enhed + dato.
#' Returnerer NULL hvis enhed angivet men intet matcher (eller tom).
#' @noRd
parquet_load_slice <- function(path, enhed = NULL, from = NULL, to = NULL) {
  if (!dir.exists(path)) return(NULL)
  ds <- arrow::open_dataset(path)
  if (!is.null(from)) ds <- dplyr::filter(ds, .data$dato >= as.Date(from))
  if (!is.null(to))   ds <- dplyr::filter(ds, .data$dato <= as.Date(to))
  if (!is.null(enhed)) {
    vars <- unique(tolower(enhed))
    ds <- dplyr::filter(ds, tolower(.data$enhed) %in% vars)
  }
  res <- dplyr::collect(ds)
  # Parquet kan lagre dato som tekst ("YYYY-MM-DD") → coerce til Date. bfh_qic
  # afviser character-x; uden dette fejler ALLE scans ("x must be ... Date ...").
  if ("dato" %in% names(res) && is.character(res$dato)) {
    res$dato <- as.Date(res$dato)
  }
  if (!is.null(enhed) && nrow(res) == 0) return(NULL)
  res
}

#' Behold de seneste max_obs unikke datoer (en observation = unik dato).
#' @noRd
parquet_limit_observations <- function(data, max_obs = 36L, date_col = "dato") {
  if (is.null(max_obs) || is.na(max_obs)) return(data)
  max_obs <- as.integer(max_obs)
  if (!date_col %in% names(data)) {
    if (nrow(data) <= max_obs) return(data)
    return(dplyr::slice_tail(data, n = max_obs))
  }
  ud <- sort(unique(data[[date_col]]))
  if (length(ud) <= max_obs) return(data)
  cutoff <- min(utils::tail(ud, max_obs))
  dplyr::filter(data, .data[[date_col]] >= cutoff)
}
