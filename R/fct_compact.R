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

#' Billigt kilde-fingeraftryk for en indikator-mappe.
#'
#' Friskhed afgøres af fingeraftryk-match (ikke kalenderdag): så forbliver
#' spejlet af UÆNDREDE indikatorer gyldigt på tværs af dage (fx SundK, der
#' sjældent opdateres), og intradag-regenerering opdages med det samme.
#'
#' Skal være billigt (kaldes pr. indikator ved læsning + i fuld sweep ved
#' opstart — målt: 8 s for 889 indikatorer, 0,1 s for største med 7.097
#' partitioner): 1 readdir af indikator-mappen + stats på filerne i de
#' nyeste K dato-partitioner. Fanger nye dage (antal + nyeste navn) og
#' regenerering (som altid rører de nyeste dage). Kendt blindvinkel:
#' ændringer der KUN rører gammel historik uden at ændre antal/nyeste —
#' dækkes af force-refresh/manuel kompaktering.
#' @return character-streng ("antal|nyeste-partition|antal-statede|mtime"),
#'   eller NA_character_ hvis mappen ikke findes.
#' @noRd
source_fingerprint <- function(src_dir, k = 3L) {
  if (!dir.exists(src_dir)) return(NA_character_)
  kids <- list.files(src_dir)
  parts <- sort(kids[startsWith(kids, "dato=")], decreasing = TRUE)
  newest <- utils::head(parts, k)
  stat_paths <- if (length(parts) > 0) {
    unlist(lapply(file.path(src_dir, newest), list.files, full.names = TRUE))
  } else {
    file.path(src_dir, kids[grepl("\\.parquet$", kids)])
  }
  mt <- if (length(stat_paths) > 0) {
    suppressWarnings(max(as.numeric(file.mtime(stat_paths)), na.rm = TRUE))
  } else {
    0
  }
  paste(length(kids), if (length(parts) > 0) parts[1] else "",
        length(stat_paths), round(mt), sep = "|")
}

#' Skriv manifest (kaldes SIDST — først da må læsere bruge de nye entries).
#' entries: named list (rel → list(fingerprint, compacted_at)) for HELE
#' spejlet (kalderen merger uændrede entries fra forrige manifest).
#' @noRd
compact_manifest_write <- function(base_path, entries, n_ok = NA_integer_,
                                   n_failed = NA_integer_) {
  p <- compact_manifest_path(base_path)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(
    compacted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    n_ok = n_ok, n_failed = n_failed,
    indicators = entries
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

#' Manifest-entries (rel → entry-liste). Tom liste ved manglende/gammelt
#' manifest-format (v1 uden indicators-felt) — alt regnes da som ændret.
#' @noRd
compact_manifest_entries <- function(manifest) {
  e <- manifest$indicators
  if (is.null(e) || length(e) == 0) return(list())
  as.list(e)
}

#' Find ændrede/nye indikatorer: live-fingeraftryk vs manifest.
#' @param items df fra compact_list_indicators
#' @return items-subset med ekstra kolonne fp (live-fingeraftryk)
#' @noRd
compact_changed_items <- function(base_path, items = compact_list_indicators(base_path),
                                  manifest = compact_manifest_read(base_path)) {
  if (nrow(items) == 0) {
    items$fp <- character(0)
    return(items)
  }
  entries <- compact_manifest_entries(manifest)
  items$fp <- vapply(items$src, source_fingerprint, "")
  stored <- vapply(items$rel, function(r) {
    entries[[r]]$fingerprint %||% ""
  }, "")
  changed <- is.na(items$fp) | items$fp != stored
  items[changed & !is.na(items$fp), , drop = FALSE]
}

#' Kør (inkrementel) kompaktering: kun ændrede/nye indikatorer omskrives;
#' uændrede entries bevares i manifestet, så deres spejl-filer forbliver
#' gyldige uden at blive rørt. Fejl i én indikator stopper ikke resten;
#' manifest skrives til sidst (fejlede indikatorer får INGEN entry → læses
#' råt). Konsol-/pipeline-venlig driver; appens modal kører samme trin
#' chunket via mod_compact.
#' @param progress valgfri callback function(i, n, rel) til statusvisning
#' @return list(n_ok, n_failed, n_empty, n_skipped)
#' @noRd
run_compaction <- function(base_path, progress = NULL) {
  items <- compact_list_indicators(base_path)
  manifest <- compact_manifest_read(base_path)
  todo <- compact_changed_items(base_path, items, manifest)
  entries <- compact_manifest_entries(manifest)
  n_ok <- 0L; n_failed <- 0L; n_empty <- 0L
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  for (i in seq_len(nrow(todo))) {
    if (!is.null(progress)) progress(i, nrow(todo), todo$rel[i])
    res <- safe_operation(paste("kompaktér", todo$rel[i]),
      compact_indicator(todo$src[i], compact_dest_path(base_path, todo$rel[i])),
      fallback = list(status = "fejl"))
    if (res$status == "ok") {
      n_ok <- n_ok + 1L
      # Fingeraftrykket er taget FØR læsning — ændres kilden midt i
      # kompakteringen, matcher entry'et ikke næste sweep → rekompakteres.
      entries[[todo$rel[i]]] <- list(fingerprint = todo$fp[i], compacted_at = now)
    } else if (res$status == "tom") {
      n_empty <- n_empty + 1L
      entries[[todo$rel[i]]] <- NULL
    } else {
      n_failed <- n_failed + 1L
      entries[[todo$rel[i]]] <- NULL   # fejlet → ingen entry → læses råt
    }
  }
  compact_manifest_write(base_path, entries, n_ok = n_ok, n_failed = n_failed)
  list(n_ok = n_ok, n_failed = n_failed, n_empty = n_empty,
       n_skipped = nrow(items) - nrow(todo))
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

#' Relativ manifest-nøgle for en fundet spejl-fil ("gruppe/ind" el. "ind").
#' @noRd
.compact_rel_from_file <- function(base_path, file) {
  root <- paste0(file.path(base_path, "_compact"), .Platform$file.sep)
  rel <- sub("\\.parquet$", "", substring(file, nchar(root) + 1))
  gsub("\\\\", "/", rel)
}

#' Indlæs en indikators FULDE slice ad hurtigste vej: spejl-fil hvis dens
#' manifest-fingeraftryk matcher kilden NU (fanger både nye dage og
#' intradag-regenerering; uændrede indikatorer må gerne læses fra et ældre
#' spejl), ellers rå lager. force springer spejlet over (escape hatch, fx
#' ved ændringer i gammel historik som fingeraftrykket ikke ser).
#' Læse-/statfejl → rå fallback, aldrig hårdt stop.
#' @param manifest forudindlæst manifest (undgår genlæsning pr. indikator i
#'   et scan). NULL → læses her.
#' @param src forudresolvet rå kildesti (undgår dobbelt-opslag). NULL → resolves.
#' @param fp forudberegnet live-fingeraftryk. NULL → beregnes her.
#' @noRd
parquet_load_indicator_best <- function(base_path, indikator_navn_teknisk,
                                        force = FALSE, manifest = NULL,
                                        src = NULL, fp = NULL) {
  src <- src %||% parquet_indicator_path(base_path, indikator_navn_teknisk)
  if (!force) {
    f <- parquet_compact_file(base_path, indikator_navn_teknisk)
    if (!is.null(f)) {
      manifest <- manifest %||% compact_manifest_read(base_path)
      entry <- compact_manifest_entries(manifest)[[
        .compact_rel_from_file(base_path, f)]]
      fp <- fp %||% tryCatch(source_fingerprint(src), error = function(e) NA_character_)
      if (!is.null(entry) && !is.na(fp) &&
          identical(entry$fingerprint, fp)) {
        .require_arrow()
        d <- tryCatch(arrow::read_parquet(f), error = function(e) NULL)
        if (!is.null(d)) {
          if ("dato" %in% names(d) && is.character(d$dato)) {
            d$dato <- as.Date(d$dato)
          }
          return(d)
        }
      }
    }
  }
  parquet_load_slice(src)
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
