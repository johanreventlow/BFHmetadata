# Kompaktering af parquet-lageret. Lageret skrives dags-partitioneret
# (~172k bittesmå filer — skrive-venligt for ETL'en), men læses hele tiden
# pr. indikator, hvor åbne-omkostningen pr. fil dominerer totalt (målt:
# 1.271 dagsfiler = 0,75-1,0 s mod 0,05 s for samme data i én fil).
# Kompaktering omskriver hver indikator til ÉN fil i et delt _compact/-spejl
# i selve lageret, så alle læsere (denne app, kolleger, BFHddl-pipelinen)
# deler gevinsten. Ingen server i driftmiljøet → kompaktering udføres af
# brugeren via startup-modalen i appen (mod_compact).
#
# Sikkerhedsmodel: manifestet skrives SIDST og bærer dags-TTL. Læsere bruger
# kun spejlet når manifestet er fra i dag — et afbrudt/fejlet forløb eller
# et forældet spejl ignoreres altså bare (fallback til rå lager), det kan
# aldrig give forkerte data.

#' Enumerér indikator-mapper i lageret (direkte + 1 niveau ned, som
#' parquet_indicator_path). En indikator-mappe kendes på dato=-partitioner
#' eller .parquet-filer som direkte børn — der kigges KUN på navne, aldrig
#' rekursivt ned i de ~67k partitionsmapper. Mapper med _-prefix (herunder
#' _compact selv) springes over.
#' @return data.frame med rel (relativ sti, fx "gruppe/ind") og src (absolut)
#' @noRd
compact_list_indicators <- function(base_path) {
  is_indicator_dir <- function(d) {
    kids <- list.files(d)
    any(startsWith(kids, "dato=")) || any(grepl("\\.parquet$", kids))
  }
  rel <- character(0); src <- character(0)
  for (d1 in list.dirs(base_path, recursive = FALSE, full.names = TRUE)) {
    if (startsWith(basename(d1), "_")) next
    if (is_indicator_dir(d1)) {
      rel <- c(rel, basename(d1)); src <- c(src, d1)
      next
    }
    for (d2 in list.dirs(d1, recursive = FALSE, full.names = TRUE)) {
      if (startsWith(basename(d2), "_")) next
      if (is_indicator_dir(d2)) {
        rel <- c(rel, file.path(basename(d1), basename(d2)))
        src <- c(src, d2)
      }
    }
  }
  data.frame(rel = rel, src = src, stringsAsFactors = FALSE)
}

#' Destination i spejlet for en indikators relative sti.
#' @noRd
compact_dest_path <- function(base_path, rel) {
  file.path(base_path, "_compact", paste0(rel, ".parquet"))
}

#' Kompaktér én indikator: læs alle partitioner → skriv én fil.
#' Atomisk-nok på Windows: skriv temp + rename; rename over eksisterende
#' fejler på Windows, så eksisterende fil fjernes først (læsere der rammer
#' hullet falder tilbage til rå lager via tryCatch i best-loaderen).
#' Fejl propagerer til kalderen (run_compaction/modulet tæller dem).
#' @noRd
compact_indicator <- function(src_dir, dest_file) {
  d <- parquet_load_slice(src_dir)   # håndterer partitionering + dato-coerce
  if (is.null(d) || nrow(d) == 0) {
    return(list(status = "tom", rows = 0L))
  }
  dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(dest_file, ".tmp-", Sys.getpid())
  arrow::write_parquet(d, tmp)
  if (file.exists(dest_file)) unlink(dest_file)
  if (!file.rename(tmp, dest_file)) {
    # Rename kan fejle hvis en læser holder filen — kopiér som sidste udvej
    file.copy(tmp, dest_file, overwrite = TRUE)
    unlink(tmp)
  }
  list(status = "ok", rows = nrow(d))
}

#' @noRd
compact_manifest_path <- function(base_path) {
  file.path(base_path, "_compact", "_manifest.json")
}

#' Skriv manifest (kaldes SIDST — først da må læsere bruge spejlet).
#' @noRd
compact_manifest_write <- function(base_path, n_ok, n_failed, date = Sys.Date()) {
  p <- compact_manifest_path(base_path)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(
    date = as.character(date),
    compacted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    n_ok = n_ok, n_failed = n_failed
  ), p, auto_unbox = TRUE)
  invisible(p)
}

#' Læs manifest; NULL ved manglende/korrupt (må aldrig vælte en læser).
#' @noRd
compact_manifest_read <- function(base_path) {
  p <- compact_manifest_path(base_path)
  if (!file.exists(p)) return(NULL)
  tryCatch(jsonlite::read_json(p, simplifyVector = TRUE),
           error = function(e) NULL)
}

#' Er spejlet fra i dag? (dags-TTL — data opdateres natligt, så gårsdagens
#' spejl mangler seneste dag og må ikke bruges til signal-vurdering).
#' @noRd
compact_manifest_fresh <- function(base_path, date = Sys.Date()) {
  m <- compact_manifest_read(base_path)
  !is.null(m) && identical(as.character(m$date), as.character(date))
}

#' Kør fuld kompaktering af et lager (konsol-/pipeline-venlig driver;
#' appens modal kører samme trin chunket via mod_compact). Fejl i én
#' indikator stopper ikke resten; manifest skrives til sidst.
#' @param progress valgfri callback function(i, n, rel) til statusvisning
#' @return list(n_ok, n_failed, n_empty)
#' @noRd
run_compaction <- function(base_path, progress = NULL) {
  items <- compact_list_indicators(base_path)
  n_ok <- 0L; n_failed <- 0L; n_empty <- 0L
  for (i in seq_len(nrow(items))) {
    if (!is.null(progress)) progress(i, nrow(items), items$rel[i])
    res <- safe_operation(paste("kompaktér", items$rel[i]),
      compact_indicator(items$src[i], compact_dest_path(base_path, items$rel[i])),
      fallback = list(status = "fejl"))
    if (res$status == "ok") n_ok <- n_ok + 1L
    else if (res$status == "tom") n_empty <- n_empty + 1L
    else n_failed <- n_failed + 1L
  }
  compact_manifest_write(base_path, n_ok = n_ok, n_failed = n_failed)
  list(n_ok = n_ok, n_failed = n_failed, n_empty = n_empty)
}

#' Find en indikators fil i spejlet (direkte + 1 niveau, som rå-søgningen).
#' NULL hvis den ikke findes.
#' @noRd
parquet_compact_file <- function(base_path, indikator_navn_teknisk) {
  root <- file.path(base_path, "_compact")
  direct <- file.path(root, paste0(indikator_navn_teknisk, ".parquet"))
  if (file.exists(direct)) return(direct)
  for (sub in list.dirs(root, recursive = FALSE, full.names = TRUE)) {
    cand <- file.path(sub, paste0(indikator_navn_teknisk, ".parquet"))
    if (file.exists(cand)) return(cand)
  }
  NULL
}

#' Indlæs en indikators FULDE slice ad hurtigste vej: fresh _compact-spejl
#' hvis muligt, ellers rå lager. force springer spejlet over (escape hatch
#' hvis kilden er opdateret intradag — samme semantik som dags-cachens
#' force refresh). Læsefejl på spejlfilen → rå fallback, aldrig hårdt stop.
#' @noRd
parquet_load_indicator_best <- function(base_path, indikator_navn_teknisk,
                                        force = FALSE) {
  if (!force && compact_manifest_fresh(base_path)) {
    f <- parquet_compact_file(base_path, indikator_navn_teknisk)
    if (!is.null(f)) {
      d <- tryCatch(arrow::read_parquet(f), error = function(e) NULL)
      if (!is.null(d)) {
        if ("dato" %in% names(d) && is.character(d$dato)) {
          d$dato <- as.Date(d$dato)
        }
        return(d)
      }
    }
  }
  parquet_load_slice(
    parquet_indicator_path(base_path, indikator_navn_teknisk))
}

# --- Husk sidste parquet-mappe -----------------------------------------------
# Signal-modulet gemmer stien ved scan; startup-modalen (og prefill af
# tekstfeltet) læser den ved næste app-start. Bor i cache-mappen (samme
# option-redirect som dags-cachen → isoleret i tests).

#' @noRd
last_parquet_dir_path <- function() {
  file.path(slice_cache_dir(), "last_parquet_dir.txt")
}

#' @noRd
last_parquet_dir_read <- function() {
  p <- last_parquet_dir_path()
  if (!file.exists(p)) return(NULL)
  v <- tryCatch(trimws(readLines(p, n = 1L, warn = FALSE)),
                error = function(e) "")
  if (length(v) == 0 || !nzchar(v)) NULL else v
}

#' @noRd
last_parquet_dir_write <- function(path) {
  dir.create(slice_cache_dir(), recursive = TRUE, showWarnings = FALSE)
  tryCatch(writeLines(as.character(path), last_parquet_dir_path()),
           error = function(e) NULL)   # read-only disk må ikke vælte scan
  invisible(NULL)
}
