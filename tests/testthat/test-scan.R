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
