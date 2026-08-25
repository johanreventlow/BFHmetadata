# Validerer LOOKUP_TABLES-config mod forventet skema (fanger tastefejl i
# tabel-/kolonnenavne før de rammer DB).

test_that("kun faggrupper er optet ind i excel-adapter batch 1", {
  enabled <- Filter(function(cfg) isTRUE(cfg$excel_adapter), LOOKUP_TABLES)
  expect_equal(vapply(enabled, `[[`, "", "id"), "faggrupper")
})

test_that("org_oversaettelse-entry findes med korrekte kolonner", {
  ids <- vapply(LOOKUP_TABLES, function(x) x$id, "")
  expect_true("org_oversaettelse" %in% ids)
  cfg <- LOOKUP_TABLES[[which(ids == "org_oversaettelse")]]
  expect_identical(cfg$table, "tblOrganisationOversaettelse")
  expect_identical(cfg$pk, "Id")
  cols <- vapply(cfg$cols, function(c) c$col, "")
  expect_setequal(cols, c("organisatorisk_navn_fra_data",
                          "organisatorisk_navn_teknisk"))
  fk <- Filter(function(c) identical(c$type, "fk"), cfg$cols)[[1]]
  expect_identical(fk$parent, "tblOrganisationStruktur")
  expect_identical(fk$parent_pk, "Id")
  expect_true(grepl("organisatorisk_navn_langt", fk$label_expr))
})

test_that("org_oversaettelse-dropdown viser teknisk navn + langt navn i trae", {
  ids <- vapply(LOOKUP_TABLES, function(x) x$id, "")
  cfg <- LOOKUP_TABLES[[which(ids == "org_oversaettelse")]]
  fk <- Filter(function(c) identical(c$type, "fk"), cfg$cols)[[1]]
  # parent_col => hierarchy_indent_options kan indrykke valgene
  expect_identical(fk$parent_col, "parent_Id")
  # teknisk navn foerst, derefter separator + langt navn
  expect_true(grepl("organisatorisk_navn_teknisk", fk$label_expr))
  expect_lt(regexpr("organisatorisk_navn_teknisk", fk$label_expr, fixed = TRUE),
            regexpr("organisatorisk_navn_langt", fk$label_expr, fixed = TRUE))
  expect_true(grepl(LABEL_SEPARATOR, fk$label_expr, fixed = TRUE))
})

# --- HIERARCHY_TABLES (Fase C/D) ---------------------------------------------

test_that("org_struktur-entry har korrekte kolonner og parent-casing", {
  cfg <- HIERARCHY_TABLES$org_struktur
  expect_identical(cfg$table, "tblOrganisationStruktur")
  expect_identical(cfg$pk, "Id")
  expect_identical(cfg$parent_col, "parent_Id")   # stort I — casing kritisk
  expect_identical(cfg$display_col, "organisatorisk_navn_langt")
  expect_null(cfg$aktiv_col)
  expect_identical(cfg$level$parent, "tblOrganisationNiveauer")
  cols <- vapply(cfg$fields, function(f) f$col, "")
  expect_setequal(cols, c("organisatorisk_navn_teknisk",
                          "organisatorisk_navn_langt",
                          "organisatorisk_navn_kort"))
})

test_that("indikator_hierarki-entry har korrekte kolonner og aktiv-flag", {
  cfg <- HIERARCHY_TABLES$indikator_hierarki
  expect_identical(cfg$table, "tblIndikatorHierarki")
  expect_identical(cfg$pk, "Id")
  expect_identical(cfg$parent_col, "parent_id")   # lille i — modsat org-tabellen
  expect_identical(cfg$display_col, "hierarki_navn")
  expect_identical(cfg$aktiv_col, "aktiv")
  expect_identical(cfg$level$parent, "tblIndikatorNiveauer")
  expect_identical(cfg$level$col, "indikator_niveau")
  cols <- vapply(cfg$fields, function(f) f$col, "")
  expect_setequal(cols, c("hierarki_navn", "hierarki_navn_kort",
                          "brug_kort_navn_i_titel",
                          "beskrivelse_kort", "beskrivelse_lang", "kilde_id"))
  # 6 felter + parent + niveau + aktiv = 9 edit-kolonner
  expect_length(hierarchy_edit_cols(cfg), 9)
  # titel-flaget er et checkbox-felt (boolean i DB)
  typer <- stats::setNames(vapply(cfg$fields, function(f) f$type, ""), cols)
  expect_identical(unname(typer["brug_kort_navn_i_titel"]), "checkbox")
})

test_that("indikator_hierarki har kaskade-filtre paa Datapakke/Datasæt-niveau", {
  filters <- HIERARCHY_TABLES$indikator_hierarki$filters
  expect_length(filters, 2)
  expect_identical(vapply(filters, function(f) f$label, ""),
                   c("Datapakke", "Datasæt"))
  expect_identical(vapply(filters, function(f) f$niveau_navn, ""),
                   c("Datapakke", "Datasæt"))
  # org-instansen har ingen filtre (opt-in)
  expect_null(HIERARCHY_TABLES$org_struktur$filters)
})

test_that("alle HIERARCHY_TABLES-entries har paakraevede felter", {
  for (cfg in HIERARCHY_TABLES) {
    expect_true(all(c("id", "table", "pk", "parent_col", "display_col",
                      "label", "fields", "level") %in% names(cfg)),
                info = cfg$id)
    expect_true(all(c("col", "parent", "parent_pk", "num_col", "name_col",
                      "label_expr") %in% names(cfg$level)), info = cfg$id)
  }
})

test_that("hierarkiske fk-felter har parent_col til indrykkede dropdowns", {
  f <- Find(function(x) x$col == "indikator_hierarki", INDIKATOR_FIELDS)
  expect_identical(f$parent_col, "parent_id")
  pe <- Find(function(c) c$id == "personer", LOOKUP_TABLES)
  fk <- Find(function(c) identical(c$type, "fk"), pe$cols)
  expect_identical(fk$parent_col, "parent_Id")
})
