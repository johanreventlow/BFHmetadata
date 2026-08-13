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

# --- Diagram-validering (Fase B) ---------------------------------------------

test_that("validate_diagram accepterer gyldigt input (NA-periode OK)", {
  errs <- validate_diagram(list(indikator = 1L, organisatorisk_navn_teknisk = 2L,
                                diagram_type = 1L,
                                periode_aggregering = NA_character_))
  expect_length(errs, 0)
})

test_that("validate_diagram kraever indikator, org og type", {
  errs <- validate_diagram(list(indikator = NA_integer_,
                                organisatorisk_navn_teknisk = NA_integer_,
                                diagram_type = NA_integer_,
                                periode_aggregering = NA_character_))
  expect_length(errs, 3)
  expect_true(any(grepl("Indikator", errs)))
  expect_true(any(grepl("enhed", errs)))
  expect_true(any(grepl("Diagramtype", errs)))
})

test_that("validate_diagram fanger enkelt manglende felt", {
  errs <- validate_diagram(list(indikator = 5L,
                                organisatorisk_navn_teknisk = NA_integer_,
                                diagram_type = 1L,
                                periode_aggregering = "måned"))
  expect_length(errs, 1)
  expect_match(errs, "enhed")
})

test_that("filtervalg bevares kun mens det fortsat er gyldigt", {
  choices <- c("Alle" = "", "Pakke A" = "A", "Pakke B" = "B")
  expect_identical(.preserved_filter_selection("B", choices), "B")
  expect_identical(.preserved_filter_selection("Slettet", choices), "")
  expect_identical(.preserved_filter_selection(NULL, choices), "")
  expect_identical(.preserved_filter_selection(NA_character_, choices), "")
})
