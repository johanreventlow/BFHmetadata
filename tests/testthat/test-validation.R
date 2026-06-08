test_that("validate_indikator kræver ikke-tomt indikator_navn", {
  errs <- validate_indikator(list(indikator_navn = ""))
  expect_true(any(grepl("indikator_navn", errs)))
})

test_that("validate_indikator accepterer gyldig række", {
  errs <- validate_indikator(list(indikator_navn = "Genindlæggelser",
                                  antal_observationer = 30))
  expect_length(errs, 0)
})

test_that("validate_indikator afviser ikke-numerisk antal_observationer", {
  errs <- validate_indikator(list(indikator_navn = "X",
                                  antal_observationer = "abc"))
  expect_true(any(grepl("antal_observationer", errs)))
})

test_that("validate_indikator tillader NA/NULL antal_observationer", {
  errs <- validate_indikator(list(indikator_navn = "X", antal_observationer = NA))
  expect_length(errs, 0)
})
