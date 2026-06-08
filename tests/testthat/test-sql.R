test_that("build_list_sql joiner alle 3 FK-parents med labels", {
  sql <- build_list_sql()
  expect_match(sql, 'FROM "tblIndikatorer"')
  expect_match(sql, '"tblIndikatorHierarki"')
  expect_match(sql, '"tblPersoner"')
  expect_match(sql, '"tblDatakilder"')
  expect_match(sql, "hierarki_navn")
  expect_match(sql, "datakilde_navn")
})

test_that("build_fk_options_sql bygger id+label select for parent", {
  sql <- build_fk_options_sql("tblDatakilder", "datakilde_navn")
  expect_match(sql, '"Id"')
  expect_match(sql, "datakilde_navn")
  expect_match(sql, 'FROM "tblDatakilder"')
})

test_that("build_update_sql bruger parametriserede placeholders", {
  res <- build_update_sql(c("indikator_navn", "mål"))
  expect_match(res, 'UPDATE "tblIndikatorer" SET')
  expect_match(res, '"indikator_navn" = \\$1')
  expect_match(res, '"mål" = \\$2')
  expect_match(res, 'WHERE "id" = \\$3')
})

test_that("build_insert_sql returnerer RETURNING id", {
  res <- build_insert_sql(c("indikator_navn", "datakilde"))
  expect_match(res, 'INSERT INTO "tblIndikatorer"')
  expect_match(res, "RETURNING \"id\"")
  expect_match(res, "\\$1, \\$2")
})

test_that("INDIKATOR_JUNCTIONS har 3 relationer med påkrævede felter", {
  expect_named(INDIKATOR_JUNCTIONS, c("faggrupper", "dataprodukter", "organisation"))
  for (j in INDIKATOR_JUNCTIONS) {
    expect_true(all(c("table", "fk", "parent", "parent_pk", "label") %in% names(j)))
  }
  expect_equal(INDIKATOR_JUNCTIONS$faggrupper$table, "tblForbindIndikatorerFaggrupper")
  expect_equal(INDIKATOR_JUNCTIONS$dataprodukter$fk, "dataprodukt_id")
})

test_that("junction-byggere bygger parametriseret SQL", {
  j <- INDIKATOR_JUNCTIONS$faggrupper
  expect_match(build_junction_select_sql(j),
    'SELECT "faggruppe_id" FROM "tblForbindIndikatorerFaggrupper" WHERE "indikator_id" = \\$1')
  expect_match(build_junction_delete_sql(j),
    'DELETE FROM "tblForbindIndikatorerFaggrupper" WHERE "indikator_id" = \\$1')
  # 2 parent-ids → $1 (indikator) genbrugt, $2+$3 = parents
  ins <- build_junction_insert_sql(j, 2)
  expect_match(ins, 'INSERT INTO "tblForbindIndikatorerFaggrupper" \\("indikator_id", "faggruppe_id"\\)')
  expect_match(ins, 'VALUES \\(\\$1, \\$2\\), \\(\\$1, \\$3\\)')
  opt <- build_junction_options_sql(j)
  expect_match(opt, '"Id" AS id')
  expect_match(opt, 'FROM "tblFaggrupper"')
})

test_that("organisation-options bruger COALESCE-label", {
  expect_match(build_junction_options_sql(INDIKATOR_JUNCTIONS$organisation),
    "COALESCE")
})
