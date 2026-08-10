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

test_that("alle LOOKUP_TABLES-entries har paakraevede felter", {
  for (cfg in LOOKUP_TABLES) {
    expect_true(all(c("id", "table", "pk", "label", "cols") %in% names(cfg)),
                info = cfg$id)
    for (c in cfg$cols) {
      expect_true(all(c("col", "type", "label") %in% names(c)),
                  info = paste(cfg$id, c$col))
      if (identical(c$type, "fk")) {
        expect_true(all(c("parent", "parent_pk", "label_expr") %in% names(c)),
                    info = paste(cfg$id, c$col))
      }
    }
  }
})
