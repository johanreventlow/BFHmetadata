# Bygger en fixture-parquet med folder-pr-indikator-struktur
make_parquet_fixture <- function(env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = env)
  ind <- file.path(base, "test_ind")
  dir.create(ind, recursive = TRUE)
  d <- data.frame(
    dato = as.Date("2020-01-01") + 0:5 * 30,
    vaerdi = c(1, 2, 3, 4, 5, 6),
    taeller = NA_real_, naevner = NA_real_,
    enhed = rep("Afd X", 6), stringsAsFactors = FALSE)
  arrow::write_parquet(d, file.path(ind, "part-0.parquet"))
  base
}

test_that("parquet_indicator_path finder direkte + 1-niveau", {
  base <- make_parquet_fixture()
  expect_equal(parquet_indicator_path(base, "test_ind"), file.path(base, "test_ind"))
  # 1-niveau ned
  sub <- file.path(base, "gruppe"); dir.create(file.path(sub, "ind2"), recursive = TRUE)
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01"), vaerdi = 1),
                       file.path(sub, "ind2", "p.parquet"))
  expect_equal(parquet_indicator_path(base, "ind2"), file.path(sub, "ind2"))
})

test_that("parquet_load_slice filtrerer på enhed + dato", {
  base <- make_parquet_fixture()
  p <- parquet_indicator_path(base, "test_ind")
  all <- parquet_load_slice(p)
  expect_equal(nrow(all), 6)
  # enhed-match (case-insensitive)
  expect_equal(nrow(parquet_load_slice(p, enhed = "afd x")), 6)
  expect_null(parquet_load_slice(p, enhed = "Ukendt"))
  # dato-filter
  expect_equal(nrow(parquet_load_slice(p, from = "2020-03-01")), 4)
})

test_that("parquet_limit_observations beholder seneste N unikke datoer", {
  d <- data.frame(dato = as.Date("2020-01-01") + 0:9 * 30, vaerdi = 1:10)
  expect_equal(nrow(parquet_limit_observations(d, 3)), 3)
  expect_equal(max(parquet_limit_observations(d, 3)$dato), max(d$dato))
  expect_equal(nrow(parquet_limit_observations(d, NULL)), 10)
})
