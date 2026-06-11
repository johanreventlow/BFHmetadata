test_that("enhed_variants_for: dedup + lowercase + drop tomme/NA", {
  vdf <- data.frame(
    org_id  = c(12L, 12L, 12L, 99L),
    teknisk = c("Y Hjerte", "Y Hjerte", "Y Hjerte", "Z"),
    kort    = c("YHJ", "YHJ", "YHJ", NA),
    langt   = c("Y Hjerteafdeling", "Y Hjerteafdeling", "Y Hjerteafdeling", ""),
    fra_data = c("HJERTE", "Y HJ", NA, NA),
    stringsAsFactors = FALSE)
  v <- enhed_variants_for(vdf, 12L)
  expect_true(all(v == tolower(v)))                    # alt lowercase
  expect_true(all(c("hjerte", "y hj", "y hjerte", "yhj",
                    "y hjerteafdeling") %in% v))
  expect_false(any(is.na(v) | v == ""))                # ingen tomme/NA
  expect_equal(length(v), length(unique(v)))           # dedup
})

test_that("enhed_variants_for: ukendt org_id → character(0)", {
  vdf <- data.frame(org_id = 1L, teknisk = "a", kort = "b", langt = "c",
                    fra_data = NA, stringsAsFactors = FALSE)
  expect_equal(enhed_variants_for(vdf, 777L), character(0))
})

test_that("enhed_variants_for: NULL df → character(0)", {
  expect_equal(enhed_variants_for(NULL, 1L), character(0))
})

test_that("scan_diagram: fixture-parquet med langt løb → signal=TRUE", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "test_ind"
  dir.create(file.path(base, ind))
  df <- data.frame(
    dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)),
    taeller = NA_real_, naevner = NA_real_,
    enhed = "afd x", stringsAsFactors = FALSE)
  arrow::write_parquet(df, file.path(base, ind, "part-0.parquet"))

  row <- list(diagram_id = 1L, indikator_navn_teknisk = ind, org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "Afd X", kort = NA,
                    langt = NA, fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf)

  expect_equal(res$status, "ok")
  expect_true(res$signal)
  expect_equal(res$n_obs, 24L)
  expect_s3_class(res$qic_result, "bfh_qic_result")
})

test_that("scan_diagram: manglende mappe → status 'ingen_data', intet hårdt fald", {
  base <- withr::local_tempdir()
  row <- list(diagram_id = 2L, indikator_navn_teknisk = "findes_ikke", org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "x", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf)
  expect_equal(res$status, "ingen_data")
  expect_false(res$signal)
})

test_that("scan_diagram: window_n begrænser til seneste N observationer", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "win_ind"; dir.create(file.path(base, ind))
  df <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                   vaerdi = c(rep(10, 12), rep(2, 12)),
                   taeller = NA_real_, naevner = NA_real_, enhed = "e")
  arrow::write_parquet(df, file.path(base, ind, "p.parquet"))
  row <- list(diagram_id = 3L, indikator_navn_teknisk = ind, org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "E", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf, window_n = 6L)
  expect_equal(res$n_obs, 6L)
})

test_that("scan_diagram: median-knæk splitter i faser (parts-stien)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "med_ind"; dir.create(file.path(base, ind))
  df <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                   vaerdi = c(rep(10, 12), rep(2, 12)),
                   taeller = NA_real_, naevner = NA_real_, enhed = "e")
  arrow::write_parquet(df, file.path(base, ind, "p.parquet"))
  row <- list(diagram_id = 42L, indikator_navn_teknisk = ind, org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "E", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  # Knæk på 13. observation → fase 1 (1:12) + fase 2 (13:24)
  meds <- data.frame(diagram = 42L, laas_median = df$dato[13])
  res <- scan_diagram(row, base, medians_df = meds, variants_df = vdf)
  expect_equal(res$status, "ok")
  expect_equal(length(unique(res$summary$fase)), 2L)
  expect_equal(max(res$summary$fase), 2)
})

test_that("scan_diagram: ingen enhed-varianter → ingen_data (ingen blandet-enhed-load)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "noorg_ind"; dir.create(file.path(base, ind))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:5 * 30,
    vaerdi = 1:6, taeller = NA_real_, naevner = NA_real_, enhed = "e"),
    file.path(base, ind, "p.parquet"))
  row <- list(diagram_id = 9L, indikator_navn_teknisk = ind, org_id = 5L)
  vdf <- data.frame(org_id = 99L, teknisk = "X", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)  # ingen match på org_id 5
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf)
  expect_equal(res$status, "ingen_data")
})

test_that("index_filter_choices: sorterede unikke valg pr. dimension (drop NA)", {
  idx <- data.frame(
    overafdeling = c("B", "A", "A", NA),
    afsnit = NA_character_,
    datapakke = c("P", "P", "Q", "P"),
    datasaet = c("d1", "d2", "d1", "d3"),
    indikator_navn = c("i2", "i1", "i1", "i3"),
    stringsAsFactors = FALSE)
  ch <- index_filter_choices(idx)
  expect_equal(ch$overafdeling, c("A", "B"))     # sorteret, NA væk
  expect_equal(ch$afsnit, character(0))           # helt NA → tom
  expect_equal(ch$indikator_navn, c("i1", "i2", "i3"))
})

test_that("apply_index_filters: AND på tværs af dimensioner; tom filter = alt", {
  idx <- data.frame(
    diagram_id = 1:4,
    overafdeling = c("A", "A", "B", "A"),
    afsnit = NA_character_,
    datapakke = c("P", "Q", "P", "P"),
    datasaet = c("d1", "d1", "d1", "d2"),
    indikator_navn = c("i1", "i1", "i1", "i1"),
    stringsAsFactors = FALSE)
  expect_equal(nrow(apply_index_filters(idx, list())), 4)
  r <- apply_index_filters(idx, list(overafdeling = "A", datapakke = "P"))
  expect_equal(r$diagram_id, c(1L, 4L))
  # tom streng/NULL pr. dimension ignoreres
  expect_equal(nrow(apply_index_filters(idx, list(overafdeling = ""))), 4)
})
