test_that("lookup-byggere bruger pk-kolonne (Id) korrekt parametriseret", {
  expect_match(build_lookup_list_sql("tblFaggrupper", "Id"),
    'SELECT \\* FROM "tblFaggrupper" ORDER BY "Id"')
  expect_match(build_lookup_update_sql("tblFaggrupper", "Id", "faggruppe"),
    'UPDATE "tblFaggrupper" SET "faggruppe" = \\$1 WHERE "Id" = \\$2')
  expect_no_match(build_lookup_update_sql("tblFaggrupper", "Id", "faggruppe"),
    'WHERE "id"')   # MÅ ej bruge lille-bogstav id
  expect_match(build_lookup_insert_sql("tblFaggrupper", "Id"),
    'INSERT INTO "tblFaggrupper" DEFAULT VALUES RETURNING "Id"')
  expect_match(build_lookup_delete_sql("tblFaggrupper", "Id"),
    'DELETE FROM "tblFaggrupper" WHERE "Id" = \\$1')
  expect_match(build_lookup_refcount_sql("tblIndikatorer", "datakilde"),
    'FROM "tblIndikatorer" WHERE "datakilde" = \\$1')
})

test_that("LOOKUP_TABLES har pk=Id + datakilder-refcheck + personer-fk", {
  expect_gte(length(LOOKUP_TABLES), 7)
  for (cfg in LOOKUP_TABLES) {
    expect_true(all(c("id", "table", "pk", "label", "cols") %in% names(cfg)))
    expect_equal(cfg$pk, "Id")
    expect_true(length(cfg$cols) >= 1)
  }
  dk <- Find(function(c) c$id == "datakilder", LOOKUP_TABLES)
  expect_equal(dk$ref_check$child, "tblIndikatorer")
  expect_equal(dk$ref_check$col, "datakilde")
  # Personer: FK-kolonne med parent + label_expr
  pe <- Find(function(c) c$id == "personer", LOOKUP_TABLES)
  fk <- Find(function(c) identical(c$type, "fk"), pe$cols)
  expect_equal(fk$col, "organisatorisk_enhed")
  expect_equal(fk$parent, "tblOrganisationStruktur")
  expect_true(grepl("COALESCE", fk$label_expr))
})

test_that("build_list_sql joiner alle 3 FK-parents med labels", {
  sql <- build_list_sql()
  expect_match(sql, 'FROM "tblIndikatorer"')
  expect_match(sql, '"tblIndikatorHierarki"')
  expect_match(sql, '"tblPersoner"')
  expect_match(sql, '"tblDatakilder"')
  expect_match(sql, "hierarki_navn")
  expect_match(sql, "datakilde_navn")
})

test_that("build_list_sql joiner datapakke (forælder-hierarki) som label", {
  sql <- build_list_sql()
  expect_match(sql, 'LEFT JOIN "tblIndikatorHierarki" dp')
  expect_match(sql, 'dp."Id" = p_indikator_hierarki."parent_id"')
  expect_match(sql, '"label_datapakke"')
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

test_that("build_diagram_index_sql joiner indikator/hierarki/datapakke/org + org-niveauer", {
  sql <- build_diagram_index_sql()
  expect_match(sql, 'FROM "tblDiagrammer"')
  expect_match(sql, '"diagram_type" = 1')
  expect_match(sql, '"diagram_aktivt"')
  expect_match(sql, '"tblIndikatorer"')
  expect_match(sql, '"tblIndikatorHierarki"')
  expect_match(sql, '"tblOrganisationStruktur"')
  expect_match(sql, "datapakke")       # forælder-hierarki
  expect_match(sql, "datasaet")
  expect_match(sql, "indikator_navn_teknisk")
  # Org-niveau-ancestry (rekursiv CTE)
  expect_match(sql, "WITH RECURSIVE")
  expect_match(sql, "overafdeling")
  expect_match(sql, "afdeling")
  expect_match(sql, "afsnit")
})

test_that("median SQL-byggere er parametriserede", {
  expect_match(build_median_list_sql(),
    'FROM "tblDiagrammerMedian" WHERE "diagram" = \\$1')
  expect_match(build_median_insert_sql(),
    'INSERT INTO "tblDiagrammerMedian" \\("diagram", "laas_median"\\) VALUES \\(\\$1, \\$2\\) RETURNING "id"')
  expect_match(build_median_delete_sql(),
    'DELETE FROM "tblDiagrammerMedian" WHERE "id" = \\$1')
})

test_that("build_org_enhed_variants_sql joiner org + oversaettelse på int-FK", {
  sql <- build_org_enhed_variants_sql()
  expect_match(sql, '"tblOrganisationStruktur"')
  expect_match(sql, '"tblOrganisationOversaettelse"')
  # ov."organisatorisk_navn_teknisk" er INTEGER FK til tblOrganisationStruktur."Id"
  # (trods det forvirrende kolonnenavn) → joines på o."Id", ikke på et strengnavn.
  expect_match(sql, 'ov\\."organisatorisk_navn_teknisk" = o\\."Id"')
  expect_match(sql, 'organisatorisk_navn_fra_data')
  expect_match(sql, 'organisatorisk_navn_kort')
  expect_match(sql, 'LEFT JOIN')   # org uden oversaettelse bevares
})
