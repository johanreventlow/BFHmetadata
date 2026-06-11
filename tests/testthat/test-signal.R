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

test_that("compute_signal flagger langt løb (seneste fase ustabil)", {
  # 12 høje + 12 lave → langt løb, signal i (eneste) fase
  d <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                  vaerdi = c(rep(10, 12), rep(2, 12)), naevner = NA_real_)
  r <- compute_signal(d)
  expect_true(r$signal)
  expect_equal(max(r$summary_all$fase), 1)
})

test_that("compute_signal: stabil serie giver intet signal", {
  set.seed(1)
  d <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                  vaerdi = rep(c(4, 6), 12), naevner = NA_real_)  # krydser median tit
  expect_false(compute_signal(d)$signal)
})

test_that("compute_signal: proportion-serie bruger rate (taeller/naevner), ej rå tal", {
  # Rå taeller = rep(c(4,6),12) krydser median 5 → intet signal hvis brugt rå.
  # naevner valgt så rate = taeller/naevner*100 danner langt løb (50/75 vs 10/15):
  # seks 10, seks 15, seks 50, seks 75 → median 32.5 → fase 1-12 over, 13-24 under.
  d <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                  taeller = rep(c(4, 6), 12),
                  naevner = c(rep(8, 12), rep(40, 12)))
  r <- compute_signal(d)
  expect_equal(head(r$qic_result$qic_data$y, 2), c(50, 75))  # rate, ej rå 4/6
  expect_true(r$signal)
  expect_equal(max(r$summary_all$fase), 1)
})

test_that("compute_signal: kun seneste fase afgør (tidligt løb ignoreres)", {
  # Fase 1 (1:12) langt løb; fase 2 (13:24) stabil krydsende → seneste = stabil
  d <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                  vaerdi = c(rep(10, 12), rep(c(4, 6), 6)), naevner = NA_real_)
  r <- compute_signal(d, parts = 13L)
  expect_equal(max(r$summary_all$fase), 2)
  expect_false(r$signal)
})
