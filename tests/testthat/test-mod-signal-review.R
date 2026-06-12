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
