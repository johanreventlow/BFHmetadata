# Startup-kompaktering (mod_compact): modal ved app-start når spejlet er
# forældet, chunket kørsel i baggrunden, afbrydelig, manifest skrives sidst.

test_that("startup: kendt mappe + intet/forældet manifest → modal vises", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- make_store_fixture()
  last_parquet_dir_write(base)
  shiny::testServer(mod_compact_server, args = list(), {
    expect_true(asked())
    expect_false(running())
  })
})

test_that("startup: fresh manifest (kompakteret i dag) → ingen modal", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- make_store_fixture()
  run_compaction(base)
  last_parquet_dir_write(base)
  shiny::testServer(mod_compact_server, args = list(), {
    expect_false(asked())
  })
})

test_that("startup: ingen kendt mappe (første brug) → ingen modal", {
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  shiny::testServer(mod_compact_server, args = list(), {
    expect_false(asked())
  })
})

test_that("go: kompakterer chunket, skriver manifest sidst, melder resultat", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- make_store_fixture()
  last_parquet_dir_write(base)
  queue <- list()
  withr::local_options(list(
    bfhmeta.scan_scheduler = function(fn) queue[[length(queue) + 1]] <<- fn))
  run_queued <- function() {
    while (length(queue) > 0) { fn <- queue[[1]]; queue <<- queue[-1]; fn() }
  }
  shiny::testServer(mod_compact_server, args = list(), {
    session$setInputs(dir = base, go = 1)
    # Første indikator køres synkront; resten venter i køen → stadig i gang,
    # og manifest må IKKE findes endnu (skrives sidst)
    expect_true(running())
    expect_false(compact_manifest_fresh(base))
    run_queued()
    expect_false(running())
    expect_equal(result()$n_ok, 2L)
    expect_true(compact_manifest_fresh(base))
    expect_true(file.exists(file.path(base, "_compact", "gruppe", "ind_a.parquet")))
    expect_true(file.exists(file.path(base, "_compact", "ind_b.parquet")))
  })
})

test_that("cancel: resten kompakteres ikke, og manifest skrives ALDRIG", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- make_store_fixture()
  last_parquet_dir_write(base)
  queue <- list()
  withr::local_options(list(
    bfhmeta.scan_scheduler = function(fn) queue[[length(queue) + 1]] <<- fn))
  run_queued <- function() {
    while (length(queue) > 0) { fn <- queue[[1]]; queue <<- queue[-1]; fn() }
  }
  shiny::testServer(mod_compact_server, args = list(), {
    session$setInputs(dir = base, go = 1)
    expect_true(running())                 # ind_b venter stadig i køen
    session$setInputs(cancel = 1)
    expect_false(running())
    run_queued()                           # stale callback dør på gen-guarden
    expect_false(file.exists(file.path(base, "_compact", "ind_b.parquet")))
    # Uden manifest tages det halve spejl aldrig i brug → læsere går råt
    expect_false(compact_manifest_fresh(base))
  })
})

test_that("go med ugyldig mappe → afvises venligt, intet startes", {
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  shiny::testServer(mod_compact_server, args = list(), {
    session$setInputs(dir = file.path(tempdir(), "findes_ej"), go = 1)
    expect_false(running())
    expect_null(result())
  })
})
