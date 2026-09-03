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

test_that("diagram-allowlisten daekker grid-felterne og periode er ej fritekst", {
  cols <- vapply(BULK_DIAGRAM_FIELDS, function(f) f$col, "")
  # Alle bulk-felter skal have en label i grid'et — ellers staar de navnloese
  # i dropdownen.
  expect_true(all(cols %in% unname(.DIAGRAM_GRID_FIELDS)))
  # Indikator/enhed er bevidst UDE: at flytte N diagrammer til samme
  # indikator/enhed kolliderer med duplikat-reglen.
  expect_false("indikator" %in% cols)
  expect_false("organisatorisk_navn_teknisk" %in% cols)
  # Periode SKAL vaere et fast vaerdisaet: en stavevariant paa N raekker ville
  # pipelinen ikke forstaa.
  per <- Find(function(f) identical(f$col, "periode_aggregering"),
              BULK_DIAGRAM_FIELDS)
  expect_identical(per$kind, "choice")
  expect_identical(per$choices, PERIODE_AGGREGERING_CHOICES)
})

test_that("bulk_diagram_rammer_duplikatnoegle kender de tre noeglefelter", {
  expect_true(bulk_diagram_rammer_duplikatnoegle("diagram_type"))
  expect_true(bulk_diagram_rammer_duplikatnoegle("indikator"))
  expect_false(bulk_diagram_rammer_duplikatnoegle("diagram_aktivt"))
  expect_false(bulk_diagram_rammer_duplikatnoegle("maalgruppe"))
})

test_that("bulk_diagram_validation_errors fanger raekker der bliver ugyldige", {
  d <- data.frame(
    diagram_id = c(1L, 2L), indikator = c(5L, 5L),
    organisatorisk_navn_teknisk = c(9L, 9L), diagram_type = c(1L, 1L),
    periode_aggregering = c("uge", "uge"), indgaar_i_aggregering = c(TRUE, TRUE),
    aggreger_egne_og_boern = c(FALSE, FALSE), diagram_aktivt = c(TRUE, TRUE),
    direktionens_tavle = c(FALSE, FALSE), maalgruppe = c(NA_integer_, NA_integer_),
    stringsAsFactors = FALSE
  )
  # Et harmloest bool-felt: ingen raekker bliver ugyldige
  expect_identical(
    nrow(bulk_diagram_validation_errors(d, "diagram_aktivt", FALSE)), 0L
  )
  # Diagramtype ER obligatorisk — at saette den til NA goer BEGGE ugyldige
  fejl <- bulk_diagram_validation_errors(d, "diagram_type", NA_integer_)
  expect_identical(nrow(fejl), 2L)
  expect_setequal(fejl$id, c("1", "2"))
  expect_true(all(grepl("Diagramtype", fejl$fejl)))
})

test_that("bulk_diagram_validation_errors ser paa den PATCHEDE raekke", {
  # Raekke 2 mangler enhed i forvejen. En bulk paa et andet felt maa ikke
  # tavst skrive den videre som gyldig — den skal rapporteres.
  d <- data.frame(
    diagram_id = c(1L, 2L), indikator = c(5L, 5L),
    organisatorisk_navn_teknisk = c(9L, NA_integer_), diagram_type = c(1L, 1L),
    periode_aggregering = c("uge", "uge"), indgaar_i_aggregering = c(TRUE, TRUE),
    aggreger_egne_og_boern = c(FALSE, FALSE), diagram_aktivt = c(TRUE, TRUE),
    direktionens_tavle = c(FALSE, FALSE), maalgruppe = c(NA_integer_, NA_integer_),
    stringsAsFactors = FALSE
  )
  fejl <- bulk_diagram_validation_errors(d, "diagram_aktivt", FALSE)
  expect_identical(nrow(fejl), 1L)
  expect_identical(fejl$id, "2")
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
