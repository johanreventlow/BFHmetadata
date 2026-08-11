make_fake_signal_db <- function(base, idx) {
  list(
    list_active_seriediagrammer = function() idx,
    org_enhed_variants = function() data.frame(org_id = 5L, teknisk = "E",
      kort = NA, langt = NA, fra_data = NA, stringsAsFactors = FALSE),
    diagram_medians = function(diagram_id) data.frame(
      id = integer(0), diagram = integer(0), laas_median = as.Date(character(0))),
    diagram_medians_batch = function(diagram_ids) data.frame(
      id = integer(0), diagram = integer(0), laas_median = as.Date(character(0))),
    add_median_break = function(diagram_id, dato, aggregering = NA_character_) 999L,
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
    drain_scan()
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
    drain_scan()
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
    drain_scan()
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
  # Tæl scan-stiens median-hentninger SEPARAT fra display-laget (breaks_tbl
  # læser også medians for at vise eksisterende knæk). Scannet henter nu
  # medians i ÉT batch-kald pr. scan; per-diagram-stien må slet ikke rammes.
  calls <- new.env(); calls$batch <- 0L; calls$per_diagram <- 0L
  db <- make_fake_signal_db(base, idx)
  db$diagram_medians_batch <- function(diagram_ids) {
    calls$batch <- calls$batch + 1L
    data.frame(id = integer(0), diagram = integer(0), laas_median = as.Date(character(0)))
  }
  db$diagram_medians <- function(diagram_id) {
    fns <- paste(unlist(lapply(sys.calls(), function(x) deparse(x[[1]]))), collapse = " ")
    if (grepl("scan_process_group", fns)) calls$per_diagram <- calls$per_diagram + 1L
    data.frame(id = integer(0), diagram = integer(0), laas_median = as.Date(character(0)))
  }
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    expect_equal(calls$batch, 1L)          # ét batch-kald for hele scannet
    expect_equal(calls$per_diagram, 0L)    # ingen per-diagram-round-trips
    session$setInputs(scan = 2)            # re-scan samme vindue → cache-hit
    drain_scan()
    expect_equal(calls$per_diagram, 0L)    # cache-hit → ingen ny scan-sti-hentning
  })
})

test_that("batch-medians: ÉT kald uanset antal diagrammer, og knæk havner rigtigt", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  for (ind in c("a", "b", "c")) {
    dir.create(file.path(base, ind))
    arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
      vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
      naevner = NA_real_, enhed = "e"), file.path(base, ind, "p.parquet"))
  }
  idx <- data.frame(diagram_id = c(10L, 20L, 30L), indikator_id = 1:3,
    indikator_navn = c("A", "B", "C"), indikator_navn_teknisk = c("a", "b", "c"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  seen <- new.env(); seen$n <- 0L; seen$ids <- NULL
  db <- make_fake_signal_db(base, idx)
  # Kun diagram 20 har et knæk (på 13. observation) → fase-split netop dér.
  db$diagram_medians_batch <- function(diagram_ids) {
    seen$n <- seen$n + 1L; seen$ids <- diagram_ids
    data.frame(id = 1L, diagram = 20L,
               laas_median = as.Date("2020-01-01") + 12 * 30)
  }
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    expect_equal(seen$n, 1L)                       # ét kald for 3 diagrammer
    expect_setequal(seen$ids, c(10L, 20L, 30L))
    # Knækket ramte KUN diagram 20 (to faser); de andre har én fase
    expect_equal(max(cache()[["20|all"]]$summary$fase), 2)
    expect_equal(max(cache()[["10|all"]]$summary$fase), 1)
    expect_equal(max(cache()[["30|all"]]$summary$fase), 1)
  })
})

test_that("batch-medians fejler → fallback til per-diagram (scan overlever)", {
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
  fallback <- new.env(); fallback$n <- 0L
  db <- make_fake_signal_db(base, idx)
  db$diagram_medians_batch <- function(diagram_ids) stop("DB nede")
  db$diagram_medians <- function(diagram_id) {
    fallback$n <- fallback$n + 1L
    data.frame(id = integer(0), diagram = integer(0), laas_median = as.Date(character(0)))
  }
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    expect_gte(fallback$n, 1L)                     # degraderede, faldt ej om
    expect_equal(signal_list()$diagram_id, 1L)     # scannet gav stadig resultat
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
    drain_scan()
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
  db$add_median_break <- function(diagram_id, dato, aggregering = NA_character_) {
    saved$args <- list(diagram_id = diagram_id, dato = dato,
                       aggregering = aggregering); 555L }

  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
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
    drain_scan()
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
    drain_scan()
    session$setInputs(chart_selected = "2020-07-28")  # valgt på diagram 10
    session$setInputs(next_ = 1)                      # naviger til diagram 20
    session$setInputs(save_break = 1)                 # stale valg → ingen skrivning
    expect_equal(called$n, 0)
  })
})

test_that("delete_break kalder delete_median_break med valgt knæk-id", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "a"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_, naevner = NA_real_,
    enhed = "e"), file.path(base, "a", "p.parquet"))
  idx <- data.frame(diagram_id = 7L, indikator_id = 1L, indikator_navn = "A",
    indikator_navn_teknisk = "a", datasaet = "d", datapakke = "p", org_id = 5L,
    org_teknisk = "E", org_navn = "E", org_niveau = 5L, overafdeling = "OA",
    afdeling = NA, afsnit = NA, stringsAsFactors = FALSE)
  deleted <- new.env(); deleted$id <- NULL
  db <- make_fake_signal_db(base, idx)
  # POSIXct UTC som produktion; knæk FØR data → droppes i scan (signal bevares),
  # men vises i breaks_tbl så det kan vælges + slettes.
  db$diagram_medians <- function(diagram_id) data.frame(
    id = 42L, diagram = 7L, laas_median = as.POSIXct("2019-01-01", tz = "UTC"))
  db$delete_median_break <- function(median_id) { deleted$id <- median_id; 1L }
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    session$setInputs(breaks_tbl_rows_selected = 1L, delete_break = 1)
    expect_equal(deleted$id, 42L)
  })
})

test_that("degenereret qic-data i cache → chart-render giver venlig besked, ALDRIG hård fejl", {
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
    drain_scan()
    expect_s3_class(output$chart, "json")        # normal render virker (girafe→json)
    # Forgift cachen som produktion: qic_data uden ét eneste endeligt y
    cc <- cache()
    cc[["1|all"]]$qic_result$qic_data$y <- NA_real_
    cache(cc)
    # Gammel adfærd: hård ggplot-fejl ("diff.default(continuous_range_coord)")
    # ville rethrowes her og fejle testen. Ny adfærd: enten stille NULL eller
    # vores venlige validate-besked — begge OK, alt andet er en regression.
    err <- tryCatch({ output$chart; NULL }, error = function(e) e)
    if (!is.null(err)) {
      expect_match(conditionMessage(err), "kan ikke tegnes")
    } else {
      succeed("render overlevede degenereret data uden hård fejl")
    }
  })
})

test_that("session lukkes midt i scan → ventende ticks dør stille (ingen destroyed-fejl)", {
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
  queue <- list()
  withr::local_options(list(
    bfhmeta.scan_scheduler = function(fn) queue[[length(queue) + 1]] <<- fn))
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    expect_true(scan_running())            # gruppe "b" venter i køen
    session$close()                        # browser lukket/reloadet midt i scan
  })
  # Produktions-crashen: ventende later-callbacks fyrer EFTER session-destroy
  # og læste reaktiver → "Can't access reactive scan_gen; its module session
  # has been destroyed". Nu skal de dø stille.
  expect_no_error({
    while (length(queue) > 0) { fn <- queue[[1]]; queue <- queue[-1]; fn() }
  })
})

test_that("progressivt scan: første indikator-gruppe vises FØR resten er scannet", {
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
  # Manuel kø i stedet for later::later — testServer afvikler selv later-køen
  # under flush, så mellemtilstande kan kun observeres med styret skedulering.
  queue <- list()
  withr::local_options(list(
    bfhmeta.scan_scheduler = function(fn) queue[[length(queue) + 1]] <<- fn))
  run_queued <- function() {
    while (length(queue) > 0) { fn <- queue[[1]]; queue <<- queue[-1]; fn() }
  }
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    # Kun første gruppe (indikator "a") er processeret — brugeren kan allerede
    # arbejde mens resten ligger i køen.
    expect_equal(signal_list()$diagram_id, 10L)
    expect_true(scan_running())
    run_queued()
    expect_equal(signal_list()$diagram_id, c(10L, 20L))
    expect_false(scan_running())
  })
})

test_that("stop scan: ventende grupper processeres ikke; fundne signaler består", {
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
  queue <- list()
  withr::local_options(list(
    bfhmeta.scan_scheduler = function(fn) queue[[length(queue) + 1]] <<- fn))
  run_queued <- function() {
    while (length(queue) > 0) { fn <- queue[[1]]; queue <<- queue[-1]; fn() }
  }
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    expect_true(scan_running())            # gruppe "b" venter i køen
    session$setInputs(stop_scan = 1)
    expect_false(scan_running())
    run_queued()                           # stale callback dør på gen-guarden
    expect_equal(signal_list()$diagram_id, 10L)   # "b" blev ALDRIG scannet
  })
})

test_that("scan husker parquet-mappen til næste session (bruges af startup-kompaktering)", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
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
  expect_null(last_parquet_dir_read())
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
  })
  expect_equal(last_parquet_dir_read(), base)
})

test_that("scan læser fra fp-matchende _compact-spejl (bevist via poisonet spejl)", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- withr::local_tempdir()
  dir.create(file.path(base, "a"))
  # Rå kilde: STABIL serie (intet signal)
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = rep(c(4, 6), 12), taeller = NA_real_, naevner = NA_real_,
    enhed = "e"), file.path(base, "a", "p.parquet"))
  run_compaction(base)
  # Poison spejlet med SIGNAL-data. Kildens fingeraftryk er uændret →
  # scannet skal vælge spejlet, og signalet beviser hvilken sti der læstes.
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_, naevner = NA_real_,
    enhed = "e"), file.path(base, "_compact", "a.parquet"))
  idx <- data.frame(diagram_id = 1L, indikator_id = 1L, indikator_navn = "A",
    indikator_navn_teknisk = "a", datasaet = "d", datapakke = "p", org_id = 5L,
    org_teknisk = "E", org_navn = "E", org_niveau = 5L, overafdeling = "OA",
    afdeling = NA, afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    expect_equal(signal_list()$diagram_id, 1L)     # signal ⇒ læste spejlet
    expect_equal(.scan_of_current()$status, "ok")
  })
})

test_that("scan opdager intradag-regenerering: ændret kilde → spejl forbigås automatisk", {
  skip_if_not_installed("arrow")
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- withr::local_tempdir()
  dir.create(file.path(base, "a"))
  # Første version: stabil serie → kompaktér (spejl = intet signal)
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = rep(c(4, 6), 12), taeller = NA_real_, naevner = NA_real_,
    enhed = "e"), file.path(base, "a", "p.parquet"))
  run_compaction(base)
  # Intradag-regenerering: SAMME fil overskrives med SIGNAL-data. mtime
  # skubbes eksplicit frem — fingeraftrykket afrunder til hele sekunder, så
  # en overskrivning i samme sekund ellers kunne gå ubemærket i testen.
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_, naevner = NA_real_,
    enhed = "e"), file.path(base, "a", "p.parquet"))
  Sys.setFileTime(file.path(base, "a", "p.parquet"), Sys.time() + 30)
  idx <- data.frame(diagram_id = 1L, indikator_id = 1L, indikator_navn = "A",
    indikator_navn_teknisk = "a", datasaet = "d", datapakke = "p", org_id = 5L,
    org_teknisk = "E", org_navn = "E", org_niveau = 5L, overafdeling = "OA",
    afdeling = NA, afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    # Signal fundet ⇒ de NYE rå data blev læst, ikke det forældede spejl
    expect_equal(signal_list()$diagram_id, 1L)
  })
})

test_that("scan skriver dags-cache-filer pr. indikator (persistent på tværs af sessioner)", {
  skip_if_not_installed("arrow")
  cache_dir <- withr::local_tempdir()
  withr::local_options(list(bfhmeta.cache_dir = cache_dir))
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
    drain_scan()
    expect_equal(signal_list()$diagram_id, c(10L, 20L))
  })
  # Én RDS pr. indikator — næste session (samme dag) læser disse i stedet
  # for parquet-lageret.
  expect_equal(length(list.files(cache_dir, pattern = "\\.rds$")), 2L)
})

test_that("gem faseskift stempler diagrammets aggregering", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "ind_uge"))
  # Daglig serie med taeller/naevner (kan aggregeres — modsat vaerdi-fixtures)
  dates <- seq(as.Date("2024-01-01"), as.Date("2025-06-30"), by = "day")
  arrow::write_parquet(data.frame(
    dato = dates, indikator = "ind_uge", enhed = "e",
    taeller = rep(c(2, 8), length.out = length(dates)), naevner = 10),
    file.path(base, "ind_uge", "p.parquet"))
  idx <- data.frame(diagram_id = 11L, indikator_id = 1L,
    indikator_navn = "Uge", indikator_navn_teknisk = "ind_uge",
    periode_aggregering = "uge",
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  saved <- new.env(); saved$args <- NULL
  db <- make_fake_signal_db(base, idx)
  db$add_median_break <- function(diagram_id, dato, aggregering = NA_character_) {
    saved$args <- list(diagram_id = diagram_id, dato = dato,
                       aggregering = aggregering); 555L }

  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 36,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    sc <- session$returned$scan_of_current()
    skip_if(is.null(sc) || is.null(sc$slice), "ingen signal i fixture")
    # Serien SKAL være uge-aggregeret: mandags-datoer, ej alle dage
    expect_true(all(as.integer(format(sc$slice$dato, "%u")) == 1))
    # Vælg en gyldig (ikke-første) observation og gem
    d2 <- as.character(sort(unique(sc$slice$dato))[2])
    session$setInputs(chart_selected = d2)
    session$setInputs(save_break = 1)
    expect_equal(saved$args$aggregering, "uge")
  })
})
