# Validerer LOOKUP_TABLES-config mod forventet skema (fanger tastefejl i
# tabel-/kolonnenavne før de rammer DB).

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

test_that("alle HIERARCHY_TABLES-entries har paakraevede felter", {
  for (cfg in HIERARCHY_TABLES) {
    expect_true(all(c("id", "table", "pk", "parent_col", "display_col",
                      "label", "fields", "level") %in% names(cfg)),
                info = cfg$id)
    expect_true(all(c("col", "parent", "parent_pk", "num_col", "name_col",
                      "label_expr") %in% names(cfg$level)), info = cfg$id)
  }
})
