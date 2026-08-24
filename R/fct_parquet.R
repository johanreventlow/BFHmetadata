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

#' Stop med en typed, handlingsanvisende fejl når Arrow mangler.
#' @noRd
.require_arrow <- function(available = requireNamespace("arrow", quietly = TRUE)) {
  if (!isTRUE(available)) {
    cond <- simpleError(
      "Signal-gennemgang kr\u00E6ver R-pakken 'arrow'. Database-CRUD kan bruges uden."
    )
    class(cond) <- c("bfhmeta_arrow_unavailable", class(cond))
    stop(cond)
  }
  invisible(TRUE)
}

#' Har mappen mindst én parquet-fil? Kalder aldrig Arrow.
#' @noRd
parquet_files_present <- function(path) {
  if (length(path) != 1L || is.na(path) || !dir.exists(path)) return(FALSE)
  length(list.files(
    path, pattern = "\\.parquet$", recursive = TRUE,
    full.names = FALSE, ignore.case = TRUE
  )) > 0L
}

#' Find kandidat-indikatormapper direkte eller ét gruppeniveau nede.
#' Synkron helper — brug `.parquet_indicator_dirs_init` +
#' `.parquet_indicator_dirs_step` for en chunket, ikke-blokerende variant
#' (fx i mod_compact's startup-sweep på store lagre).
#' @noRd
parquet_indicator_dirs <- function(base_path) {
  ctx <- .parquet_indicator_dirs_init(base_path)
  if (is.null(ctx)) return(character())
  while (!.parquet_indicator_dirs_step(ctx, length(ctx$candidates))) {
    # step() med chunk = alle resterende kandidater kører synkront til slut
  }
  ctx$found
}

#' Har en kandidat data direkte eller i en dato-partition?
#' Scanner højst ét niveau under kandidaten og kalder aldrig Arrow. Dette
#' ENKELT-kald er det dyre skridt på et stort lager (hundredvis af
#' kandidat-mapper), og er derfor enheden chunking'en i
#' `.parquet_indicator_dirs_step` arbejder i.
#' @noRd
parquet_indicator_dir_has_data <- function(path) {
  if (length(path) != 1L || is.na(path) || !dir.exists(path)) return(FALSE)
  has_parquet <- function(dir) {
    length(list.files(
      dir, pattern = "\\.parquet$", recursive = FALSE,
      full.names = FALSE, ignore.case = TRUE
    )) > 0L
  }
  if (has_parquet(path)) return(TRUE)
  partitions <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  any(vapply(partitions, has_parquet, logical(1)))
}

#' Init-fase: KUN de billige top+1-niveau `list.dirs`-opremsninger (hurtige
#' selv på store lagre — det er `parquet_indicator_dir_has_data`-tjekket pr.
#' kandidat der er dyrt, ikke selve opremsningen). Returnerer en
#' context-env klar til gentagne `.parquet_indicator_dirs_step`-kald, eller
#' NULL hvis base_path er ugyldig/mangler (samme kontrakt som før: tomt
#' resultat, ingen fejl).
#' @noRd
.parquet_indicator_dirs_init <- function(base_path) {
  if (length(base_path) != 1L || is.na(base_path) || !dir.exists(base_path)) {
    return(NULL)
  }
  first <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)
  first <- first[!startsWith(basename(first), "_")]
  second <- unlist(lapply(first, function(path) {
    dirs <- list.dirs(path, recursive = FALSE, full.names = TRUE)
    dirs[!startsWith(basename(dirs), "_")]
  }), use.names = FALSE)
  candidates <- unique(c(first, second))
  list2env(list(
    candidates = candidates, i = 1L, found = character(0)
  ), envir = new.env(parent = emptyenv()))
}

#' Chunket step: tjek op til `chunk` kandidater fra ctx$candidates via
#' `parquet_indicator_dir_has_data`. Muterer ctx$found in-place og rykker
#' ctx$i frem.
#' @return TRUE når alle kandidater er tjekket (færdig), ellers FALSE
#' @noRd
.parquet_indicator_dirs_step <- function(ctx, chunk) {
  n <- length(ctx$candidates)
  to <- min(ctx$i + chunk - 1L, n)
  if (ctx$i <= to) {
    batch <- ctx$candidates[ctx$i:to]
    hits <- batch[vapply(batch, parquet_indicator_dir_has_data, logical(1))]
    if (length(hits) > 0) ctx$found <- c(ctx$found, hits)
  }
  ctx$i <- to + 1L
  ctx$i > n
}

#' Beskriv om signaldata kan bruges på denne computer.
#' @noRd
signal_data_capability <- function(
    base_path,
    arrow_available = requireNamespace("arrow", quietly = TRUE)) {
  if (length(base_path) != 1L || is.na(base_path) || !nzchar(base_path) ||
      !dir.exists(base_path)) {
    return(list(
      state = "ingen_data",
      message = "Ingen lokal parquet-mappe er valgt. Database-CRUD virker fortsat."
    ))
  }
  dirs <- parquet_indicator_dirs(base_path)
  if (length(dirs) == 0L) {
    return(list(
      state = "ingen_data",
      message = "Mappen indeholder ingen lokale parquet-data. Database-CRUD virker fortsat."
    ))
  }
  if (!isTRUE(arrow_available)) {
    return(list(
      state = "arrow_mangler",
      message = paste(
        "Signal-gennemgang kr\u00E6ver R-pakken 'arrow'.",
        "Database-CRUD virker fortsat."
      )
    ))
  }
  list(state = "klar", message = "Lokale signaldata er klar til scanning.")
}

#' Indlæs én indikators parquet-slice, filtreret på enhed + dato.
#' Returnerer NULL hvis enhed angivet men intet matcher (eller tom).
#' @noRd
parquet_load_slice <- function(path, enhed = NULL, from = NULL, to = NULL,
                               arrow_available = requireNamespace(
                                 "arrow", quietly = TRUE
                               )) {
  if (!parquet_files_present(path)) return(NULL)
  .require_arrow(arrow_available)
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

#' Filtrér et allerede-loadet indikator-slice på enhed (case-insensitive).
#' In-memory-pendant til parquet_load_slice(enhed=...), så ét fuldt slice kan
#' genbruges af alle diagrammer på samme indikator (én arrow-åbning pr.
#' indikator i stedet for én pr. diagram). Samme kontrakt: NULL ved no-match.
#' Manglende enhed-kolonne → NULL (slicet kan ikke afgrænses til rette enhed;
#' aldrig signal på blandede enheder).
#' @noRd
slice_filter_enhed <- function(data, enhed) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  if (!"enhed" %in% names(data)) return(NULL)
  res <- data[tolower(data$enhed) %in% unique(tolower(enhed)), , drop = FALSE]
  if (nrow(res) == 0) return(NULL)
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
