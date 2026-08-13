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

test_that("make_db_cached: key_prefix adskiller ens accessor-navne på tværs af db-instanser", {
  # Regression: alle opslagstabellers list_rows() hed det samme (0 args) i det
  # DELTE lager → tabel B fik tabel A's rækker efter faneskift (samme indhold
  # under skiftende overskrifter). key_prefix gør nøglen instans-unik.
  store <- new_cache_store()
  db_a <- make_db_cached(list(list_rows = function() "rækker-A"),
    store = store, key_prefix = "tabel_a")
  db_b <- make_db_cached(list(list_rows = function() "rækker-B"),
    store = store, key_prefix = "tabel_b")
  expect_equal(db_a$list_rows(), "rækker-A")
  expect_equal(db_b$list_rows(), "rækker-B")   # IKKE A's cachede rækker
  expect_equal(db_a$list_rows(), "rækker-A")   # hit-stien rammer egen nøgle
})

test_that("make_db_cached: key_prefix bevarer delt invalidering ved skrivninger", {
  # Lageret er stadig DELT: en skrivning i én tabel skal rydde alle
  # instansers læse-cache (fx fk_options der peger på den ændrede tabel).
  calls <- new.env(); calls$a <- 0L
  store <- new_cache_store()
  db_a <- make_db_cached(list(
    list_rows = function() { calls$a <- calls$a + 1L; "A" }),
    store = store, key_prefix = "tabel_a")
  db_b <- make_db_cached(list(update_cell = function(pk, col, val) 1L),
    store = store, key_prefix = "tabel_b")
  db_a$list_rows(); db_a$list_rows()
  expect_equal(calls$a, 1L)                    # cachet
  db_b$update_cell(1L, "navn", "x")            # skrivning i ANDEN instans
  db_a$list_rows()
  expect_equal(calls$a, 2L)                    # frisk læsning efter skriv
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

test_that("hierarki-skrivninger invaliderer den delte læse-cache (ingen stale org-data)", {
  # Org-struktur-ændringer skal slå igennem i signal-/diagram-fanerne med det
  # samme: create/update/delete_node deler cache-lager med org_enhed_variants
  # m.fl., så en node-ændring skal rydde lageret.
  calls <- new.env(); calls$variants <- 0L
  store <- new_cache_store()
  db <- make_db_cached(list(
    org_enhed_variants = function() {
      calls$variants <- calls$variants + 1L
      data.frame(org_id = 1L)
    }), store = store)
  hdb <- make_db_cached(list(
    create_node = function(values) 7L,
    update_node = function(id, values) 1L,
    delete_node = function(id) 1L), store = store)
  db$org_enhed_variants(); db$org_enhed_variants()
  expect_equal(calls$variants, 1L)             # cachet
  hdb$update_node(7L, list(navn = "Nyt navn")) # org-ændring i hierarki-modulet
  db$org_enhed_variants()
  expect_equal(calls$variants, 2L)             # frisk læsning efter node-skriv
  hdb$create_node(list()); db$org_enhed_variants()
  hdb$delete_node(7L); db$org_enhed_variants()
  expect_equal(calls$variants, 4L)             # alle tre skrive-typer rydder
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
