# Rene hjælpere til bulk-redigerings-UI'et (R/fct_bulk.R) — ingen Shiny, ingen DB.

test_that("bulk_field_choices viser danske labels og kun allowlistede felter", {
  ch <- bulk_field_choices("indikator", INDIKATOR_MODAL_LABELS)
  expect_true("kontaktperson" %in% unname(ch))
  expect_true("Kontaktperson" %in% names(ch))
  # Navnefelterne er bevidst UDE af bulk — et fælles navn på N rækker giver
  # ingen mening, og indikator-id er parquet-nøglen.
  expect_false("indikator_navn" %in% unname(ch))
  expect_false("indikator_navn_teknisk" %in% unname(ch))
  expect_identical(ch, bulk_field_choices("indikator", INDIKATOR_MODAL_LABELS))
})

test_that("bulk_field_choices falder tilbage til kolonnenavnet uden label", {
  ch <- bulk_field_choices("indikator", labels = character(0))
  expect_true(all(names(ch) == unname(ch)))
  expect_length(bulk_field_choices("findes_ikke"), 0)
})

test_that("bulk_display_value oversætter fk-id til label og bool til Ja/Nej", {
  fk <- list(col = "kontaktperson", kind = "fk")
  valg <- stats::setNames(c(4L, 7L), c("Anna", "Bo"))
  expect_identical(bulk_display_value(fk, 7L, valg), "Bo")
  # Et id uden for listen maa ikke vises som et bart tal uden forklaring
  expect_match(bulk_display_value(fk, 99L, valg), "ukendt")

  b <- list(col = "aktiv_indikator", kind = "bool")
  expect_identical(bulk_display_value(b, TRUE), "Ja")
  expect_identical(bulk_display_value(b, FALSE), "Nej")

  # Tom vaerdi skal laese som "tom", ikke som en manglende celle
  expect_identical(bulk_display_value(b, NA), "(tom)")
  expect_identical(bulk_display_value(list(kind = "text"), NA_character_), "(tom)")
})

test_that("bulk_preview_df markerer raekker der allerede har maalvaerdien", {
  d <- data.frame(
    id = c(1L, 2L, 3L), indikator_navn = c("A", "B", "C"),
    aktiv_indikator = c(TRUE, FALSE, TRUE), stringsAsFactors = FALSE
  )
  fld <- list(col = "aktiv_indikator", kind = "bool")
  pv <- bulk_preview_df(d, "id", "indikator_navn", fld, target = FALSE)

  expect_identical(nrow(pv), 3L)
  expect_identical(pv$uaendret, c(FALSE, TRUE, FALSE)) # id 2 er allerede FALSE
  expect_identical(pv$nuvaerende, c("Ja", "Nej", "Ja"))
  expect_true(all(pv$ny == "Nej"))
  expect_identical(pv$indikator, c("A", "B", "C"))
})

test_that("bulk_preview_df taaler at feltet mangler i de frosne raekker", {
  # Grid'et viser ikke alle kolonner; en kolonne der mangler skal give tomme
  # foervaerdier — ikke en fejl der vaelter modalen.
  d <- data.frame(id = 1L, indikator_navn = "A", stringsAsFactors = FALSE)
  pv <- bulk_preview_df(d, "id", "indikator_navn",
                        list(col = "datakilde", kind = "fk"), target = 3L)
  expect_identical(pv$nuvaerende, "(tom)")
  expect_false(pv$uaendret)
})

test_that("bulk_expected_before giver praecis én foervaerdi pr. id", {
  d <- data.frame(
    id = c(5L, 9L), aktiv_indikator = c(TRUE, FALSE), stringsAsFactors = FALSE
  )
  ex <- bulk_expected_before(d, "id", list(col = "aktiv_indikator", kind = "bool"))
  expect_named(ex, c("5", "9"))
  expect_identical(ex[["5"]], TRUE)
  expect_identical(ex[["9"]], FALSE)
})

test_that("bulk_conflict_text saetter antal foerst og begraenser id-listen", {
  e <- bulk_conflict("stale", as.character(1:20))
  txt <- bulk_conflict_text(e, maks = 3L)
  expect_match(txt, "^Intet skrevet")
  expect_match(txt, "20")
  expect_match(txt, "\\+17 flere")
  expect_match(txt, "ændret af en anden")

  expect_match(bulk_conflict_text(bulk_conflict("missing", "7")), "findes ikke")
  expect_match(bulk_conflict_text(bulk_conflict("undo_conflict", "7")), "siden batchen")
})
