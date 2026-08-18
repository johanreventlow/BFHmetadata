test_that("lookup-byggere bruger pk-kolonne (Id) korrekt parametriseret", {
  expect_match(
    build_lookup_list_sql("tblFaggrupper", "Id"),
    'SELECT \\* FROM "tblFaggrupper" ORDER BY "Id"'
  )
  expect_match(
    build_lookup_update_sql("tblFaggrupper", "Id", "faggruppe"),
    'UPDATE "tblFaggrupper" SET "faggruppe" = \\$1 WHERE "Id" = \\$2'
  )
  expect_no_match(
    build_lookup_update_sql("tblFaggrupper", "Id", "faggruppe"),
    'WHERE "id"'
  ) # MÅ ej bruge lille-bogstav id
  expect_match(
    build_lookup_insert_sql("tblFaggrupper", "Id"),
    'INSERT INTO "tblFaggrupper" DEFAULT VALUES RETURNING "Id"'
  )
  expect_match(
    build_lookup_delete_sql("tblFaggrupper", "Id"),
    'DELETE FROM "tblFaggrupper" WHERE "Id" = \\$1'
  )
  expect_match(
    build_lookup_refcount_sql("tblIndikatorer", "datakilde"),
    'FROM "tblIndikatorer" WHERE "datakilde" = \\$1'
  )
})

test_that("LOOKUP_TABLES har pk=Id + datakilder-refcheck + personer-fk", {
  expect_gte(length(LOOKUP_TABLES), 7)
  for (cfg in LOOKUP_TABLES) {
    expect_true(all(c("id", "table", "pk", "label", "cols") %in% names(cfg)),
      info = cfg$id
    )
    expect_equal(cfg$pk, "Id")
    expect_true(length(cfg$cols) >= 1)
    for (c in cfg$cols) {
      expect_true(all(c("col", "type", "label") %in% names(c)),
        info = paste(cfg$id, c$col)
      )
      if (identical(c$type, "fk")) {
        expect_true(all(c("parent", "parent_pk", "label_expr") %in% names(c)),
          info = paste(cfg$id, c$col)
        )
      }
    }
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

test_that("build_list_sql udleder datapakke/datasæt niveau-bevidst (CTE)", {
  sql <- build_list_sql()
  # Forfader-opslag på NIVEAU-NAVN — ikke naiv "node + forælder": indikatorer
  # peger på blandede niveauer (Indikatorsamling/Datasæt), så forælderen er
  # IKKE altid en datapakke.
  expect_match(sql, "^WITH RECURSIVE")
  expect_match(sql, '"tblIndikatorNiveauer"', fixed = TRUE)
  expect_match(sql, "'Datapakke'", fixed = TRUE)
  expect_match(sql, "'Datasæt'", fixed = TRUE)
  expect_match(sql, "label_datapakke", fixed = TRUE)
  expect_match(sql, "label_datasaet", fixed = TRUE)
  expect_no_match(sql, 'p_indikator_hierarki\\."parent_id"')
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
  expect_match(
    build_junction_select_sql(j),
    'SELECT "faggruppe_id" FROM "tblForbindIndikatorerFaggrupper" WHERE "indikator_id" = \\$1'
  )
  expect_match(
    build_junction_delete_sql(j),
    'DELETE FROM "tblForbindIndikatorerFaggrupper" WHERE "indikator_id" = \\$1'
  )
  # 2 parent-ids → $1 (indikator) genbrugt, $2+$3 = parents
  ins <- build_junction_insert_sql(j, 2)
  expect_match(ins, 'INSERT INTO "tblForbindIndikatorerFaggrupper" \\("indikator_id", "faggruppe_id"\\)')
  expect_match(ins, "VALUES \\(\\$1, \\$2\\), \\(\\$1, \\$3\\)")
  opt <- build_junction_options_sql(j)
  expect_match(opt, '"Id" AS id')
  expect_match(opt, 'FROM "tblFaggrupper"')
})

test_that("organisation-options bruger COALESCE-label", {
  expect_match(
    build_junction_options_sql(INDIKATOR_JUNCTIONS$organisation),
    "COALESCE"
  )
})

test_that("build_diagram_index_sql joiner indikator/hierarki/datapakke/org + org-niveauer", {
  sql <- build_diagram_index_sql()
  expect_match(sql, 'FROM "tblDiagrammer"')
  expect_match(sql, '"diagram_type" = 1')
  expect_match(sql, '"diagram_aktivt"')
  expect_match(sql, '"tblIndikatorer"')
  expect_match(sql, '"tblIndikatorHierarki"')
  expect_match(sql, '"tblOrganisationStruktur"')
  expect_match(sql, "datapakke") # forælder-hierarki
  expect_match(sql, "datasaet")
  expect_match(sql, "indikator_navn_teknisk")
  # Perioden STYRER signalberegningen (aggregering før signal) — uden den
  # beregnes signalet på en anden serie end BFHddl tegner.
  expect_match(sql, '"periode_aggregering"')
  # Org-niveau-ancestry (rekursiv CTE)
  expect_match(sql, "WITH RECURSIVE")
  expect_match(sql, "overafdeling")
  expect_match(sql, "afdeling")
  expect_match(sql, "afsnit")
  # Datapakke/datasæt via niveau-bevidst forfader-opslag (ikke node+forælder)
  expect_match(sql, "'Datapakke'", fixed = TRUE)
  expect_match(sql, "'Datasæt'", fixed = TRUE)
  expect_no_match(sql, 'dp\\."hierarki_navn"')
})

test_that("median SQL-byggere er parametriserede", {
  expect_match(
    build_median_list_sql(),
    'FROM "tblDiagrammerMedian" WHERE "diagram" = \\$1'
  )
  # aggregering gemmes med: uden den kan et knæk ikke valideres senere
  # (samme dato = forskellig fase-position ved forskellig periode)
  expect_match(
    build_median_insert_sql(),
    paste0(
      'INSERT INTO "tblDiagrammerMedian" \\("diagram", "laas_median", ',
      '"aggregering"\\) VALUES \\(\\$1, \\$2, \\$3\\) RETURNING "id"'
    )
  )
  expect_match(
    build_median_delete_sql(),
    'DELETE FROM "tblDiagrammerMedian" WHERE "id" = \\$1'
  )
})

test_that("build_median_batch_sql henter medians for MANGE diagrammer i ét kald", {
  sql <- build_median_batch_sql()
  expect_match(sql, 'FROM "tblDiagrammerMedian"')
  # ANY($1::int[]) med array-LITERAL som parameter → ét round-trip uanset
  # antal diagrammer. Casten er påkrævet: RPostgres kan ikke binde en
  # R-vektor som Postgres-array — en vektor som param betyder "kør statement
  # N gange", og hvert kald fik så ét nøgent tal til en array-parameter
  # ("malformed array literal: 1727", set i produktion).
  expect_match(sql, 'WHERE "diagram" = ANY\\(\\$1::int\\[\\]\\)', fixed = FALSE)
  expect_match(sql, "ORDER BY")
})

test_that("pg_int_array: R-vektor → Postgres array-literal", {
  expect_equal(pg_int_array(c(1L, 2L, 3L)), "{1,2,3}")
  expect_equal(pg_int_array(1727L), "{1727}") # ét element virker også
  expect_equal(pg_int_array(integer(0)), "{}")
  # Ikke-heltal og NA må aldrig ende i literalen (SQL-sikkerhed + gyldighed)
  expect_equal(pg_int_array(c(1L, NA, 2L)), "{1,2}")
  expect_equal(pg_int_array(c(10.0, 20.0)), "{10,20}")
  expect_error(pg_int_array("1; DROP TABLE x"))
})

test_that("build_org_enhed_variants_sql joiner org + oversaettelse på int-FK", {
  sql <- build_org_enhed_variants_sql()
  expect_match(sql, '"tblOrganisationStruktur"')
  expect_match(sql, '"tblOrganisationOversaettelse"')
  # ov."organisatorisk_navn_teknisk" er INTEGER FK til tblOrganisationStruktur."Id"
  # (trods det forvirrende kolonnenavn) → joines på o."Id", ikke på et strengnavn.
  expect_match(sql, 'ov\\."organisatorisk_navn_teknisk" = o\\."Id"')
  expect_match(sql, "organisatorisk_navn_fra_data")
  expect_match(sql, "organisatorisk_navn_kort")
  expect_match(sql, "LEFT JOIN") # org uden oversaettelse bevares
})

# --- Diagram-CRUD (Fase B) ---------------------------------------------------

test_that("build_diagram_admin_sql joiner labels og har intet aktiv-filter", {
  sql <- build_diagram_admin_sql()
  expect_match(sql, 'FROM "tblDiagrammer" d', fixed = TRUE)
  expect_match(sql, '"tblIndikatorer"', fixed = TRUE)
  expect_match(sql, '"tblOrganisationStruktur"', fixed = TRUE)
  expect_match(sql, '"tblDiagramTyper"', fixed = TRUE)
  expect_match(sql, '"periode_aggregering"', fixed = TRUE)
  expect_no_match(sql, "diagram_aktivt\\s*(=|AND)") # admin ser ALT
  expect_no_match(sql, 'WHERE d\\."diagram_type"')
})

test_that("build_diagram_admin_sql joiner hierarki (datasaet/datapakke)", {
  sql <- build_diagram_admin_sql()
  expect_match(sql, "AS datasaet", fixed = TRUE)
  expect_match(sql, "AS datapakke", fixed = TRUE)
  expect_match(sql, '"tblIndikatorHierarki"', fixed = TRUE)
  # Niveau-bevidst forfader-opslag (samme CTE som build_list_sql)
  expect_match(sql, "^WITH RECURSIVE")
  expect_match(sql, "'Datapakke'", fixed = TRUE)
  expect_match(sql, "'Datasæt'", fixed = TRUE)
  expect_no_match(sql, 'dp\\."hierarki_navn"')
})

test_that("build_diagram_insert_sql parametriserer alle kolonner + RETURNING", {
  sql <- build_diagram_insert_sql()
  for (col in DIAGRAM_COLS) expect_match(sql, sprintf('"%s"', col), fixed = TRUE)
  expect_match(sql, "\\$7") # 7 kolonner -> $1..$7
  expect_match(sql, 'RETURNING "id"', fixed = TRUE)
})

test_that("build_diagram_update_sql saetter alle kolonner, id sidst", {
  sql <- build_diagram_update_sql()
  expect_match(sql, 'UPDATE "tblDiagrammer" SET', fixed = TRUE)
  expect_match(sql, '"id" = \\$8') # 7 kolonner + id
})

test_that("build_diagram_delete_sql og duplicate/periode-byggere", {
  expect_identical(
    build_diagram_delete_sql(),
    'DELETE FROM "tblDiagrammer" WHERE "id" = $1'
  )
  dup <- build_diagram_duplicate_sql()
  expect_match(dup, '"indikator" = \\$1')
  expect_match(dup, '"organisatorisk_navn_teknisk" = \\$2')
  expect_match(dup, '"diagram_type" = \\$3')
  expect_match(dup, '"id" <> \\$4') # ekskludér egen række ved update
  per <- build_diagram_periode_sql()
  expect_match(per, "DISTINCT", fixed = TRUE)
  expect_match(per, "IS NOT NULL", fixed = TRUE)
  cnt <- build_median_count_sql()
  expect_match(cnt, 'FROM "tblDiagrammerMedian" WHERE "diagram" = \\$1')
})

# --- Hierarki-CRUD (Fase C) --------------------------------------------------

test_that("build_hierarchy_list_sql normaliserer aliaser og joiner niveau", {
  cfg <- HIERARCHY_TABLES$org_struktur
  sql <- build_hierarchy_list_sql(cfg)
  expect_match(sql, 'h."Id" AS id', fixed = TRUE)
  expect_match(sql, 'h."parent_Id" AS parent_id_raw', fixed = TRUE)
  expect_match(sql, '"tblOrganisationNiveauer"', fixed = TRUE)
  expect_match(sql, "AS niveau_num")
  expect_match(sql, "AS niveau_navn")
  expect_match(sql, "LEFT JOIN") # noder uden niveau bevares
})

test_that("hierarchy insert/update/delete parametriserer alle edit-kolonner", {
  cfg <- HIERARCHY_TABLES$org_struktur
  cols <- hierarchy_edit_cols(cfg) # 3 felter + parent + niveau = 5
  expect_length(cols, 5)
  ins <- build_hierarchy_insert_sql(cfg)
  for (col in cols) expect_match(ins, sprintf('"%s"', col), fixed = TRUE)
  expect_match(ins, 'RETURNING "Id"', fixed = TRUE)
  upd <- build_hierarchy_update_sql(cfg)
  expect_match(upd, '"Id" = \\$6') # 5 kolonner + id
  expect_identical(
    build_hierarchy_delete_sql(cfg),
    'DELETE FROM "tblOrganisationStruktur" WHERE "Id" = $1'
  )
  expect_identical(
    build_hierarchy_child_count_sql(cfg),
    'SELECT count(*) AS n FROM "tblOrganisationStruktur" WHERE "parent_Id" = $1'
  )
})

test_that("hierarki-SQL for indikator_hierarki medtager aktiv-kolonnen", {
  cfg <- HIERARCHY_TABLES$indikator_hierarki
  sql <- build_hierarchy_list_sql(cfg)
  expect_match(sql, 'h."Id" AS id', fixed = TRUE)
  expect_match(sql, 'h."parent_id" AS parent_id_raw', fixed = TRUE)
  expect_match(sql, 'h."aktiv" AS aktiv', fixed = TRUE)
  expect_match(sql, '"tblIndikatorNiveauer"', fixed = TRUE)
  cols <- hierarchy_edit_cols(cfg) # 5 felter + parent + niveau + aktiv = 8
  expect_length(cols, 8)
  ins <- build_hierarchy_insert_sql(cfg)
  for (col in cols) expect_match(ins, sprintf('"%s"', col), fixed = TRUE)
  upd <- build_hierarchy_update_sql(cfg)
  expect_match(upd, '"Id" = \\$9') # 8 kolonner + id
  expect_identical(
    build_hierarchy_child_count_sql(cfg),
    'SELECT count(*) AS n FROM "tblIndikatorHierarki" WHERE "parent_id" = $1'
  )
})

test_that("build_fk_options_aktiv_sql medtager aktiv-kolonnen", {
  sql <- build_fk_options_aktiv_sql(
    "tblIndikatorHierarki",
    '"hierarki_navn"', "aktiv"
  )
  expect_match(sql, '"Id" AS id', fixed = TRUE)
  expect_match(sql, '("hierarki_navn") AS label', fixed = TRUE)
  expect_match(sql, '"aktiv" AS aktiv', fixed = TRUE)
  expect_match(sql, '"tblIndikatorHierarki"', fixed = TRUE)
})

# --- Hierarki-oprulning (org-træ + aggregerings-flag) -----------------------

test_that("build_org_struct_sql henter id + parent fra org-tabellen", {
  s <- build_org_struct_sql()
  expect_match(s, "tblOrganisationStruktur")
  expect_match(s, '"Id" AS id', fixed = TRUE)
  expect_match(s, '"parent_Id" AS parent_id', fixed = TRUE)
})

test_that("build_aggregation_flags_sql henter flag pr. diagram-raekke UDEN aktiv-filter", {
  s <- build_aggregation_flags_sql()
  expect_match(s, "tblDiagrammer")
  expect_match(s, '"organisatorisk_navn_teknisk" AS org_id', fixed = TRUE)
  expect_match(s, '"indikator" AS indikator_id', fixed = TRUE)
  expect_match(s, '"indgaar_i_aggregering" AS indgaar', fixed = TRUE)
  expect_no_match(s, "diagram_aktivt") # BFHddl laeser flag med active_only=FALSE
})

test_that("diagram-indikator-options medtager niveau-udledt datasaet", {
  sql <- build_diagram_indikator_options_sql()
  expect_match(sql, "^WITH RECURSIVE")
  expect_match(sql, '"indikator_navn" AS label', fixed = TRUE)
  expect_match(sql, "AS datasaet", fixed = TRUE)
  expect_match(sql, "'Datasæt'", fixed = TRUE)
  expect_match(sql, 'FROM "tblIndikatorer"', fixed = TRUE)
  expect_match(sql, "ORDER BY 2", fixed = TRUE)
})
