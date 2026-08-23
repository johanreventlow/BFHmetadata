test_that("build_confirm_modal viser advarsel kun når angivet", {
  m1 <- build_confirm_modal("Titel", p("krop"), "ns-confirm", "OK")
  html1 <- as.character(m1)
  expect_match(html1, "ns-confirm", fixed = TRUE)
  expect_no_match(html1, "alert-warning")

  m2 <- build_confirm_modal("Titel", p("krop"), "ns-confirm", "OK",
                            warning = "3 ting påvirkes")
  html2 <- as.character(m2)
  expect_match(html2, "alert-warning")
  expect_match(html2, "3 ting påvirkes", fixed = TRUE)
})

test_that("tom-tilstand tilbyder ryd-filtre kun når der ER filtre", {
  ns <- shiny::NS("x")
  med <- as.character(tom_tilstand_ui(ns, has_filters = TRUE))
  uden <- as.character(tom_tilstand_ui(ns, has_filters = FALSE))
  expect_match(med, "Ryd filtre")
  expect_no_match(uden, "Ryd filtre")
  expect_match(uden, "Ingen indikatorer")
})

test_that("har_aktive_filtre er FALSE når alle tre er tomme/default", {
  expect_false(har_aktive_filtre("", "", "alle"))
})

test_that("har_aktive_filtre er TRUE når kun datapakke er sat", {
  expect_true(har_aktive_filtre("Infektionshygiejne", "", "alle"))
})

test_that("har_aktive_filtre er TRUE når kun datasæt er sat", {
  expect_true(har_aktive_filtre("", "Inf.hyg", "alle"))
})

test_that("har_aktive_filtre er TRUE når kun status er ændret fra alle", {
  expect_true(har_aktive_filtre("", "", "aktiv"))
})

test_that("har_aktive_filtre håndterer NULL for hver af de tre uden at fejle, NULL alene giver FALSE", {
  expect_false(har_aktive_filtre(NULL, NULL, NULL))
  expect_false(har_aktive_filtre(NULL, "", "alle"))
  expect_false(har_aktive_filtre("", NULL, "alle"))
  expect_false(har_aktive_filtre("", "", NULL))
})

test_that("har_aktive_filtre: tom streng tæller som ikke-sat, ikke som en værdi", {
  expect_false(har_aktive_filtre("", "", "alle"))
  expect_true(har_aktive_filtre("x", "", "alle"))
  expect_true(har_aktive_filtre("", "y", "alle"))
})
