make_fake_signal_db <- function(base, idx) {
  list(
    list_active_seriediagrammer = function() idx,
    org_enhed_variants = function() data.frame(org_id = 5L, teknisk = "E",
      kort = NA, langt = NA, fra_data = NA, stringsAsFactors = FALSE),
    diagram_medians = function(diagram_id) data.frame(
      id = integer(0), diagram = integer(0), laas_median = as.Date(character(0))),
    add_median_break = function(diagram_id, dato) 999L,
    delete_median_break = function(median_id) 1L)
}

build_fixture <- function() {
  base <- withr::local_tempdir(.local_envir = parent.frame())
  for (ind in c("ind_sig", "ind_flat")) dir.create(file.path(base, ind))
  arrow::write_parquet(data.frame(
    dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)),
    taeller = NA_real_, naevner = NA_real_, enhed = "e"),
    file.path(base, "ind_sig", "p.parquet"))
  arrow::write_parquet(data.frame(
    dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = rep(c(4, 6), 12),
    taeller = NA_real_, naevner = NA_real_, enhed = "e"),
    file.path(base, "ind_flat", "p.parquet"))
  base
}

test_that("scan finder kun diagrammer med signal", {
  skip_if_not_installed("arrow")
  base <- build_fixture()
  idx <- data.frame(diagram_id = c(1L, 2L), indikator_id = c(1L, 2L),
    indikator_navn = c("Sig", "Flad"),
    indikator_navn_teknisk = c("ind_sig", "ind_flat"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    expect_equal(signal_list()$diagram_id, 1L)        # kun "Sig"
    expect_equal(current_diagram()$diagram_id, 1L)
  })
})

test_that("næste/forrige bladrer i signal-listen", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  for (ind in c("a", "b")) {
    dir.create(file.path(base, ind))
    arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
      vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
      naevner = NA_real_, enhed = "e"), file.path(base, ind, "p.parquet"))
  }
  idx <- data.frame(diagram_id = c(10L, 20L), indikator_id = c(1L, 2L),
    indikator_navn = c("A", "B"), indikator_navn_teknisk = c("a", "b"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    expect_equal(current_diagram()$diagram_id, 10L)
    session$setInputs(next_ = 1)
    expect_equal(current_diagram()$diagram_id, 20L)
    session$setInputs(next_ = 2)          # ud over slut → bliver på sidste
    expect_equal(current_diagram()$diagram_id, 20L)
    session$setInputs(prev = 1)
    expect_equal(current_diagram()$diagram_id, 10L)
  })
})

test_that("scan uden signaler → tom liste (0 rækker) + current_diagram NULL", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "flat"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = rep(c(4, 6), 12), taeller = NA_real_, naevner = NA_real_, enhed = "e"),
    file.path(base, "flat", "p.parquet"))
  idx <- data.frame(diagram_id = 1L, indikator_id = 1L, indikator_navn = "Flad",
    indikator_navn_teknisk = "flat", datasaet = "d", datapakke = "p", org_id = 5L,
    org_teknisk = "E", org_navn = "E", org_niveau = 5L, overafdeling = "OA",
    afdeling = NA, afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    expect_equal(nrow(signal_list()), 0L)
    expect_null(current_diagram())
  })
})

test_that("re-scan (samme vindue) genbruger cache — scan-loopet henter ikke medians igen", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "a"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_, naevner = NA_real_,
    enhed = "e"), file.path(base, "a", "p.parquet"))
  idx <- data.frame(diagram_id = 1L, indikator_id = 1L, indikator_navn = "A",
    indikator_navn_teknisk = "a", datasaet = "d", datapakke = "p", org_id = 5L,
    org_teknisk = "E", org_navn = "E", org_niveau = 5L, overafdeling = "OA",
    afdeling = NA, afsnit = NA, stringsAsFactors = FALSE)
  # Tæl scan-loop-hentninger SEPARAT fra display-laget (breaks_tbl læser også
  # medians for at vise eksisterende knæk). Cache-genbrugs-invarianten gælder
  # KUN scan-loopet: et cache-hit må aldrig udløse en ny scan-sti-hentning.
  calls <- new.env(); calls$scan_loop <- 0L
  db <- make_fake_signal_db(base, idx)
  db$diagram_medians <- function(diagram_id) {
    fns <- paste(unlist(lapply(sys.calls(), function(x) deparse(x[[1]]))), collapse = " ")
    if (grepl("withProgress|incProgress", fns)) calls$scan_loop <- calls$scan_loop + 1L
    data.frame(id = integer(0), diagram = integer(0), laas_median = as.Date(character(0)))
  }
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    expect_equal(calls$scan_loop, 1L)      # første scan henter medians 1x pr. diagram
    session$setInputs(scan = 2)            # re-scan samme vindue → cache-hit
    expect_equal(calls$scan_loop, 1L)      # cache-hit → ingen ny scan-sti-hentning
  })
})

test_that("vindue-skifte EFTER scan gør ikke cache-opslag stale (C1-regression)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "a"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_, naevner = NA_real_,
    enhed = "e"), file.path(base, "a", "p.parquet"))
  idx <- data.frame(diagram_id = 1L, indikator_id = 1L, indikator_navn = "A",
    indikator_navn_teknisk = "a", datasaet = "d", datapakke = "p", org_id = 5L,
    org_teknisk = "E", org_navn = "E", org_niveau = 5L, overafdeling = "OA",
    afdeling = NA, afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    expect_equal(.scan_of_current()$status, "ok")
    # Skift vindue UDEN at re-scanne → opslaget skal stadig finde det scannede
    session$setInputs(window_mode = "latest", window_n = 12)
    expect_false(is.null(.scan_of_current()))
    expect_equal(.scan_of_current()$status, "ok")
  })
})

test_that("Gem faseskift kalder add_median_break med valgt dato + invaliderer cache", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "a"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
    naevner = NA_real_, enhed = "e"), file.path(base, "a", "p.parquet"))
  idx <- data.frame(diagram_id = 7L, indikator_id = 1L, indikator_navn = "A",
    indikator_navn_teknisk = "a", datasaet = "d", datapakke = "p", org_id = 5L,
    org_teknisk = "E", org_navn = "E", org_niveau = 5L, overafdeling = "OA",
    afdeling = NA, afsnit = NA, stringsAsFactors = FALSE)
  saved <- new.env(); saved$args <- NULL
  db <- make_fake_signal_db(base, idx)
  db$add_median_break <- function(diagram_id, dato) {
    saved$args <- list(diagram_id = diagram_id, dato = dato); 555L }

  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    # Simulér klik på en gyldig (ikke-første) observation
    session$setInputs(chart_selected = "2020-07-28")
    session$setInputs(save_break = 1)
    expect_equal(saved$args$diagram_id, 7L)
    expect_equal(as.Date(saved$args$dato), as.Date("2020-07-28"))
  })
})

test_that("klik på første observation → ingen skrivning (kan ikke splitte)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "a"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
    naevner = NA_real_, enhed = "e"), file.path(base, "a", "p.parquet"))
  idx <- data.frame(diagram_id = 7L, indikator_id = 1L, indikator_navn = "A",
    indikator_navn_teknisk = "a", datasaet = "d", datapakke = "p", org_id = 5L,
    org_teknisk = "E", org_navn = "E", org_niveau = 5L, overafdeling = "OA",
    afdeling = NA, afsnit = NA, stringsAsFactors = FALSE)
  called <- new.env(); called$n <- 0
  db <- make_fake_signal_db(base, idx)
  db$add_median_break <- function(diagram_id, dato) { called$n <- called$n + 1; 1L }
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    session$setInputs(chart_selected = "2020-01-01")   # første obs
    session$setInputs(save_break = 1)
    expect_equal(called$n, 0)
  })
})

test_that("valg fra ét diagram skrives ALDRIG på et andet efter navigation", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  for (ind in c("a", "b")) {
    dir.create(file.path(base, ind))
    arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
      vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
      naevner = NA_real_, enhed = "e"), file.path(base, ind, "p.parquet"))
  }
  idx <- data.frame(diagram_id = c(10L, 20L), indikator_id = c(1L, 2L),
    indikator_navn = c("A", "B"), indikator_navn_teknisk = c("a", "b"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  called <- new.env(); called$n <- 0
  db <- make_fake_signal_db(base, idx)
  db$add_median_break <- function(diagram_id, dato) { called$n <- called$n + 1; 1L }
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    session$setInputs(chart_selected = "2020-07-28")  # valgt på diagram 10
    session$setInputs(next_ = 1)                      # naviger til diagram 20
    session$setInputs(save_break = 1)                 # stale valg → ingen skrivning
    expect_equal(called$n, 0)
  })
})
