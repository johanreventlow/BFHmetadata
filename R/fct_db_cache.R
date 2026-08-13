# App-cache for read-mostly referencedata. Hver DB-accessor er en round-trip
# til Supabase (eu-west-1, ~40-80 ms). Ved app-start henter modulerne
# indikator-liste, FK-dropdowns, diagram-indeks, org-varianter, diagram-admin-
# liste og formular-options — mange af dem flere gange når modaler åbnes.
# Cachen er session-levetid, ikke tidsbaseret: den ryddes ved enhver skrivning,
# så UI aldrig viser stale data efter egne ændringer.

#' Nyt (tomt) cache-lager.
#' @noRd
new_cache_store <- function() new.env(parent = emptyenv())

#' Ryd et cache-lager.
#' @noRd
cache_invalidate <- function(store) {
  rm(list = ls(store, all.names = TRUE), envir = store)
  invisible(NULL)
}

#' Pak en funktion ind i memoisering (nøgle = navn + argumenter).
#'
#' `name` MÅ indgå i nøglen: flere accessors deler ét cache-lager, og de
#' fleste kaldes uden argumenter — uden navnet ville fx list_indikatorer()
#' og org_enhed_variants() dele nøgle og returnere hinandens data.
#' NULL-resultater caches ALDRIG — en fejlet hentning må ikke fastfryses
#' som "ingen data" resten af sessionen.
#' @noRd
cached_accessor <- function(fn, store = new_cache_store(), name = NULL) {
  nm <- name %||% rlang::hash(fn)   # unik pr. funktion når navn ej er givet
  function(...) {
    key <- paste0(nm, "|", rlang::hash(list(...)))
    if (!is.null(store[[key]])) return(store[[key]])
    res <- fn(...)
    if (!is.null(res)) assign(key, res, envir = store)
    res
  }
}

# Læse-accessors der er stabile i en session og dyre nok at gentage.
# Bevidst IKKE med: diagram_medians (ændres af signal-modulets egne skrivninger
# og har eget cache-lag), diagram_duplicate_count / diagram_median_count
# (guard-tal der skal se andre brugeres skrivninger med det samme).
.CACHED_READERS <- c(
  "list_indikatorer", "fk_options", "junction_options",
  "list_active_seriediagrammer", "org_enhed_variants",
  "list_diagrams_admin", "diagram_form_options", "diagram_periode_choices",
  "list_rows"
)

# Accessors der skriver: efter disse ryddes hele læse-cachen.
.WRITE_ACCESSORS <- c(
  "create_indikator", "create_indikator_full", "update_indikator",
  "save_indikator", "soft_delete", "set_junction",
  "create_diagram", "update_diagram", "delete_diagram",
  "add_median_break", "delete_median_break",
  "add_row", "update_cell", "delete_row",
  # Hierarki (make_hierarchy_db): org-struktur-ændringer skal straks slå
  # igennem i cachede org-læsninger (org_enhed_variants, diagram-indeks)
  "create_node", "update_node", "delete_node"
)

#' Pak en db-accessor-liste ind i app-cache-laget.
#'
#' Læse-accessors i .CACHED_READERS memoiseres; skrive-accessors i
#' .WRITE_ACCESSORS rydder cachen efter kald. Alt andet videreføres uændret,
#' så nye accessors (fx en senere fase) virker uden at skulle registreres.
#'
#' `key_prefix` SKAL angives når flere db-instanser med ens accessor-navne
#' deler ét lager (opslagstabellernes list_rows/fk_options hedder det samme
#' på tværs af tabeller) — uden præfiks får instans B instans A's cachede
#' data. Skrive-invalidering rydder stadig HELE det delte lager.
#' @noRd
make_db_cached <- function(db, store = new_cache_store(), key_prefix = NULL) {
  stats::setNames(lapply(names(db), function(nm) {
    fn <- db[[nm]]
    if (!is.function(fn)) return(fn)
    if (nm %in% .CACHED_READERS) {
      cached_accessor(fn, store = store,
        name = paste(c(key_prefix, nm), collapse = ":"))
    } else if (nm %in% .WRITE_ACCESSORS) {
      function(...) {
        res <- fn(...)
        cache_invalidate(store)   # egne skrivninger må aldrig give stale UI
        res
      }
    } else {
      fn
    }
  }), names(db))
}
