test_that("resolve_median_breaks: datoer → række-index (drop første/sidste/uden-for)", {
  meds <- data.frame(diagram = c(7, 7, 9),
    laas_median = as.Date(c("2020-03-01", "2020-05-01", "2020-01-01")))
  x <- as.Date("2020-01-01") + 0:5 * 30  # 6 unikke datoer
  pos <- resolve_median_breaks(7, meds, x)
  # 2020-03-01 → første dato >= = index 3; 2020-05-01 → index 5
  expect_equal(pos, c(3L, 5L))
})

test_that("resolve_median_breaks returnerer NULL uden data/match", {
  expect_null(resolve_median_breaks(7, NULL, as.Date("2020-01-01")))
  expect_null(resolve_median_breaks(99, data.frame(diagram = 7,
    laas_median = as.Date("2020-03-01")), as.Date("2020-01-01") + 0:5 * 30))
})
