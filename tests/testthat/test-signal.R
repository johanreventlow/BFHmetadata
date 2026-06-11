test_that("resolve_median_breaks: datoer → række-index (drop første/sidste/uden-for)", {
  meds <- data.frame(diagram = c(7, 7, 9),
    laas_median = as.Date(c("2020-03-01", "2020-05-01", "2020-01-01")))
  # x: 01-01, 01-31, 03-01, 03-31, 04-30, 05-30 (6 unikke datoer)
  x <- as.Date("2020-01-01") + 0:5 * 30
  pos <- resolve_median_breaks(7, meds, x)
  # part = første række >= knæk: 2020-03-01 → index 3; 2020-05-01 → index 6
  # (x[5]=04-30 < 05-01, x[6]=05-30 ≥ 05-01). BFHddl/qicharts2-konvention.
  expect_equal(pos, c(3L, 6L))
})

test_that("resolve_median_breaks returnerer NULL uden data/match", {
  expect_null(resolve_median_breaks(7, NULL, as.Date("2020-01-01")))
  expect_null(resolve_median_breaks(99, data.frame(diagram = 7,
    laas_median = as.Date("2020-03-01")), as.Date("2020-01-01") + 0:5 * 30))
})
