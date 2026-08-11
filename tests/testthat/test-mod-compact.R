# Kompaktering (mod_compact): baggrunds-sweep sammenligner fingeraftryk med
# manifestet; modal vises KUN når noget er nyt/ændret, og kun de ændrede
# indikatorer kompakteres. Manuel knap kører samme flow on demand.
# Sweep + kompaktering er chunket via next_tick → tests styrer med manuel kø.

.with_queue <- function() {
  q <- new.env(parent = emptyenv())
  q$fns <- list()
  withr::local_options(list(
    bfhmeta.scan_scheduler = function(fn) q$fns[[length(q$fns) + 1]] <- fn),
    .local_envir = parent.frame())
  q
}

.run_queued <- function(q) {
  while (length(q$fns) > 0) {
    fn <- q$fns[[1]]
    q$fns <- q$fns[-1]
    fn()
  }
}

test_that("startup: aldrig kompakteret → sweep finder alt nyt → modal vises", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- make_store_fixture()
  last_parquet_dir_write(base)
  q <- .with_queue()
  shiny::testServer(mod_compact_server, args = list(), {
    expect_false(asked())          # sweep ligger i køen endnu
    .run_queued(q)                 # startup-tick + sweep-ticks
    expect_true(asked())
    expect_false(sweeping())
  })
})

test_that("startup: kompakteret og INTET ændret → ingen modal (ingen daglig nag)", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- make_store_fixture()
  run_compaction(base)
  last_parquet_dir_write(base)
  q <- .with_queue()
  shiny::testServer(mod_compact_server, args = list(), {
    .run_queued(q)
    expect_false(asked())          # uændret lager → tavshed, også i morgen
  })
})

test_that("startup: ingen kendt mappe (første brug) → hverken sweep eller modal", {
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  q <- .with_queue()
  shiny::testServer(mod_compact_server, args = list(), {
    .run_queued(q)
    expect_false(asked())
  })
})

test_that("go: kun ÆNDREDE kompakteres; uændredes spejl røres ikke; manifest merges", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- make_store_fixture()
  run_compaction(base)                           # fuldt spejl som udgangspunkt
  ma <- file.path(base, "_compact", "gruppe", "ind_a.parquet")
  mt_a <- file.info(ma)$mtime
  # Intradag-regenerering af KUN ind_b
  Sys.setFileTime(file.path(base, "ind_b", "p.parquet"), Sys.time() + 30)
  last_parquet_dir_write(base)
  q <- .with_queue()
  shiny::testServer(mod_compact_server, args = list(), {
    .run_queued(q)
    expect_true(asked())                         # ind_b er ændret → modal
    session$setInputs(go = 1)
    .run_queued(q)
    expect_false(running())
    expect_equal(result()$n_ok, 1L)              # kun ind_b rekompakteret
    expect_equal(result()$n_skipped, 1L)         # ind_a sprunget over
  })
  expect_identical(file.info(ma)$mtime, mt_a)    # ind_a-spejl urørt på disk
  # Manifestet har stadig BEGGE entries (merge, ikke overskrivning)
  entries <- compact_manifest_entries(compact_manifest_read(base))
  expect_setequal(names(entries), c("gruppe/ind_a", "ind_b"))
})

test_that("cancel: resten kompakteres ikke; gammelt manifest består intakt", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- make_store_fixture()
  last_parquet_dir_write(base)
  q <- .with_queue()
  shiny::testServer(mod_compact_server, args = list(), {
    .run_queued(q)                               # sweep: alt er nyt
    expect_true(asked())
    session$setInputs(go = 1)                    # første indikator synkront
    expect_true(running())                       # resten venter i køen
    session$setInputs(cancel = 1)
    expect_false(running())
    .run_queued(q)                               # stale ticks dør på gen-guard
  })
  # Intet manifest skrevet → ingen entries → alle spejl-filer ignoreres af læsere
  expect_equal(compact_manifest_entries(compact_manifest_read(base)), list())
})

test_that("manuel knap: sweep on demand; uændret lager giver 'intet at gøre'-besked", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- make_store_fixture()
  run_compaction(base)
  last_parquet_dir_write(base)
  q <- .with_queue()
  shiny::testServer(mod_compact_server, args = list(), {
    .run_queued(q)                               # startup-sweep: intet ændret
    expect_false(asked())
    # Regenerér data intradag og tryk på knappen
    Sys.setFileTime(file.path(base, "ind_b", "p.parquet"), Sys.time() + 60)
    session$setInputs(open = 1)
    .run_queued(q)
    expect_true(asked())                         # nu er der noget at kompakte
    session$setInputs(go = 1)
    .run_queued(q)
    expect_equal(result()$n_ok, 1L)
  })
})
