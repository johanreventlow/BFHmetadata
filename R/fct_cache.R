# Persistent dags-cache for indikator-slices. Parquet-lageret består af
# ~172k bittesmå dags-partitionsfiler; at åbne et dataset koster langt mere
# end selve datamængden (typisk <1 MB pr. indikator). Cachen gemmer det
# collectede fulde slice (alle enheder) som én RDS pr. indikator pr. dag,
# så gentagne scans samme dag læser én lille fil i stedet for hundredvis.
# TTL = kalenderdag: data opdateres natligt af pipelinen, så en dags-nøgle
# er både enkel og korrekt. Force-refresh er brugerens escape hatch.

#' Cache-mappe (overstyrbar via option til test/drift).
#' @noRd
slice_cache_dir <- function() {
  getOption("bfhmeta.cache_dir",
            tools::R_user_dir("BFHmetadata", which = "cache"))
}

#' Filnavn-sikker, deterministisk cache-nøgle for (lager, indikator, dag).
#' Indikatornavne indeholder ';' m.m. → saneres, og en kort hash af
#' (base_path, indikator) forhindrer kollision mellem sanerede navne
#' ("a;b" vs "a_b") og mellem forskellige parquet-lagre.
#' @noRd
slice_cache_key <- function(base_path, indikator, date = Sys.Date()) {
  san <- gsub("[^A-Za-z0-9._-]", "_", indikator)
  h <- substr(rlang::hash(list(as.character(base_path), indikator)), 1, 10)
  paste0(san, "-", h, "-", format(date, "%Y%m%d"), ".rds")
}

#' Hent indikatorens fulde slice via dags-cache; miss/force → loader() + gem.
#' NULL/tomme resultater caches ALDRIG (en tastefejl i base_path må ikke
#' blive et dags-langt "ingen data"). Korrupt/ulæselig cache-fil ignoreres
#' og overskrives. Skrivefejl (fx read-only disk) vælter ikke scannet.
#' @param loader 0-args funktion der leverer det fulde slice (alle enheder)
#' @noRd
load_indicator_slice_cached <- function(base_path, indikator, loader,
                                        cache_dir = slice_cache_dir(),
                                        force = FALSE, date = Sys.Date()) {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  f <- file.path(cache_dir, slice_cache_key(base_path, indikator, date))
  if (!force && file.exists(f)) {
    hit <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(hit)) return(hit)
  }
  res <- loader()
  if (!is.null(res) && nrow(res) > 0) {
    tryCatch(saveRDS(res, f), error = function(e) NULL)
  }
  res
}

#' Ryd cache-filer ældre end max_age_days (dags-nøgler forældes af sig selv,
#' men filerne skal ikke hobe sig op). Kaldes ved modul-opstart.
#' @noRd
slice_cache_prune <- function(cache_dir = slice_cache_dir(), max_age_days = 7) {
  if (!dir.exists(cache_dir)) return(invisible(0L))
  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(invisible(0L))
  age <- difftime(Sys.time(), file.mtime(files), units = "days")
  old <- files[!is.na(age) & age > max_age_days]
  unlink(old)
  invisible(length(old))
}
