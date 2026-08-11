# App-cache for read-mostly referencedata (fct_db_cache.R). Formålet er at
# fjerne gentagne round-trips til Supabase for data der næsten aldrig ændrer
# sig i en session (FK-dropdowns, diagram-indeks, org-varianter).

test_that("cached_accessor: underliggende funktion kaldes én gang, derefter hit", {
  calls <- 0L
  f <- function() { calls <<- calls + 1L; data.frame(a = 1) }
  cf <- cached_accessor(f)
  expect_equal(cf(), data.frame(a = 1))
  expect_equal(cf(), data.frame(a = 1))
  expect_equal(calls, 1L)
})

test_that("cached_accessor: argumenter indgår i nøglen (forskellige args → egne kald)", {
  calls <- 0L
  f <- function(x) { calls <<- calls + 1L; paste0("v", x) }
  cf <- cached_accessor(f)
  expect_equal(cf("a"), "va")
  expect_equal(cf("b"), "vb")
  expect_equal(cf("a"), "va")
  expect_equal(calls, 2L)          # kun 'a' og 'b' hentet, ikke 'a' to gange
})

test_that("make_db_cached: to argumentløse accessors deler ALDRIG cache-nøgle", {
  # Regression: uden accessor-navnet i nøglen hashede alle 0-args-accessors
  # til samme nøgle i det delte lager → modul B fik modul A's data.
  store <- new_cache_store()
  raw <- list(
    list_indikatorer = function() "indikatorer",
    org_enhed_variants = function() "varianter",
    list_active_seriediagrammer = function() "diagram-indeks")
  db <- make_db_cached(raw, store = store)
  expect_equal(db$list_indikatorer(), "indikatorer")
  expect_equal(db$org_enhed_variants(), "varianter")
  expect_equal(db$list_active_seriediagrammer(), "diagram-indeks")
  # ...også ved genkald (hit-stien må ramme samme nøgle som miss-stien)
  expect_equal(db$org_enhed_variants(), "varianter")
  expect_equal(db$list_indikatorer(), "indikatorer")
})

test_that("cached_accessor: NULL-resultat caches ikke (fejl må ej fastfryses)", {
  calls <- 0L
  f <- function() { calls <<- calls + 1L; NULL }
  cf <- cached_accessor(f)
  expect_null(cf()); expect_null(cf())
  expect_equal(calls, 2L)
})

test_that("cache_invalidate: rydder cachen så næste kald rammer kilden igen", {
  calls <- 0L
  store <- new_cache_store()
  f <- cached_accessor(function() { calls <<- calls + 1L; calls }, store = store)
  expect_equal(f(), 1L)
  expect_equal(f(), 1L)            # hit
  cache_invalidate(store)
  expect_equal(f(), 2L)            # kilden igen efter invalidering
})

test_that("make_db_cached: læse-accessors caches, skrive-accessors gør ALDRIG", {
  calls <- new.env(); calls$list <- 0L; calls$create <- 0L
  raw <- list(
    list_indikatorer = function() { calls$list <- calls$list + 1L; data.frame(id = 1) },
    create_indikator = function(values) { calls$create <- calls$create + 1L; 42L },
    fk_options = function() list(a = data.frame(id = 1, label = "x")))
  db <- make_db_cached(raw)
  db$list_indikatorer(); db$list_indikatorer()
  expect_equal(calls$list, 1L)                 # læsning cachet
  db$create_indikator(list(x = 1)); db$create_indikator(list(x = 2))
  expect_equal(calls$create, 2L)               # skrivning ALDRIG cachet
})

test_that("make_db_cached: en skrivning invaliderer læse-cachen (ingen stale UI)", {
  calls <- new.env(); calls$list <- 0L
  raw <- list(
    list_indikatorer = function() { calls$list <- calls$list + 1L; data.frame(id = 1) },
    create_indikator = function(values) 42L)
  db <- make_db_cached(raw)
  db$list_indikatorer()
  db$list_indikatorer()
  expect_equal(calls$list, 1L)
  db$create_indikator(list(x = 1))             # skrivning → cachen ryddes
  db$list_indikatorer()
  expect_equal(calls$list, 2L)                 # frisk læsning efter skrivning
})

test_that("make_db_cached: ukendte accessors videreføres uændret", {
  raw <- list(noget_nyt = function(x) x * 2)
  db <- make_db_cached(raw)
  expect_equal(db$noget_nyt(21), 42)           # fase-C-accessors virker uden ændring
  expect_setequal(names(db), names(raw))
})

test_that("make_db_cached: volatile accessors caches ikke (altid friske tal)", {
  calls <- 0L
  raw <- list(diagram_duplicate_count = function(a, b, c, exclude_id = -1L) {
    calls <<- calls + 1L; 0L })
  db <- make_db_cached(raw)
  db$diagram_duplicate_count(1, 2, 3)
  db$diagram_duplicate_count(1, 2, 3)
  expect_equal(calls, 2L)   # duplikat-tjek skal se andres skrivninger med det samme
})
