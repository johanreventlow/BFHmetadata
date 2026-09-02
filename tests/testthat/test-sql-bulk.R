# Bulk-redigering, Leverance 2 (docs/plans/2026-08-30-bulk-redigering-design.md):
# rene unit-tests uden DB — allowlist-form, type-konvertering, SQL-strenge,
# konflikt-condition. DB-integration (transaktion/rollback/fortryd) ligger i
# tests/testthat/test-db-bulk.R (BFHMETA_WRITE-gated).

test_that("BULK_TABLES peger på de rigtige tabeller/pk'er", {
  expect_identical(BULK_TABLES$indikator$table, "tblIndikatorer")
  expect_identical(BULK_TABLES$indikator$pk, "id")
  expect_identical(BULK_TABLES$diagram$table, "tblDiagrammer")
  expect_identical(BULK_TABLES$diagram$pk, "id")
})

test_that("bulk-allowlisterne har kun kendte kinds og gyldige col-navne", {
  for (fields in list(BULK_INDIKATOR_FIELDS, BULK_DIAGRAM_FIELDS)) {
    for (f in fields) {
      expect_true(nzchar(f$col))
      expect_true(f$kind %in% c("bool", "fk", "choice", "text"), info = f$col)
      if (identical(f$kind, "choice")) expect_true(length(f$choices) > 0, info = f$col)
    }
  }
  # indikator_navn/indikator_navn_teknisk må ALDRIG kunne bulk-ændres
  cols <- vapply(BULK_INDIKATOR_FIELDS, function(f) f$col, "")
  expect_false("indikator_navn" %in% cols)
  expect_false("indikator_navn_teknisk" %in% cols)
})

test_that("bulk_field_config finder kendte felter og afviser ukendte", {
  fk <- bulk_field_config("indikator", "kontaktperson")
  expect_identical(fk$kind, "fk")
  expect_null(bulk_field_config("indikator", "indikator_navn"))
  expect_null(bulk_field_config("indikator", "ikke_et_felt"))
  expect_null(bulk_field_config("ukendt_tabel", "kontaktperson"))

  db <- bulk_field_config("diagram", "diagram_aktivt")
  expect_identical(db$kind, "bool")
})

test_that("bulk_coerce_value: bool", {
  fld <- list(kind = "bool")
  expect_true(bulk_coerce_value(fld, TRUE))
  expect_false(bulk_coerce_value(fld, FALSE))
  expect_true(bulk_coerce_value(fld, "TRUE"))
  expect_false(bulk_coerce_value(fld, "FALSE"))
  expect_error(bulk_coerce_value(fld, NA), "tom")
  expect_error(bulk_coerce_value(fld, NA_character_), "tom")
  expect_identical(bulk_coerce_value(fld, NA, allow_blank = TRUE), NA)
})

test_that("bulk_coerce_value: fk", {
  fld <- list(kind = "fk")
  expect_identical(bulk_coerce_value(fld, "42"), 42L)
  expect_identical(bulk_coerce_value(fld, 42), 42L)
  expect_error(bulk_coerce_value(fld, "ikke-et-tal"), "listen")
  expect_error(bulk_coerce_value(fld, NA), "tom")
  expect_identical(bulk_coerce_value(fld, NA, allow_blank = TRUE), NA_integer_)
})

test_that("bulk_coerce_value: choice validerer mod choices", {
  fld <- list(kind = "choice", choices = OUTPUT_ENHED_CHOICES)
  expect_identical(bulk_coerce_value(fld, "Procent"), "Procent")
  expect_error(bulk_coerce_value(fld, "Ikke-en-gyldig-enhed"), "gyldig")
  expect_error(bulk_coerce_value(fld, NA), "tom")
})

test_that("bulk_coerce_value: text tillader blank uden allow_blank", {
  fld <- list(kind = "text")
  expect_identical(bulk_coerce_value(fld, "hej"), "hej")
  expect_identical(bulk_coerce_value(fld, NA), NA_character_)
  expect_identical(bulk_coerce_value(fld, NA_character_, allow_blank = TRUE), NA_character_)
})

test_that("bulk_value_to_text / bulk_untext_value er rundtursstabile", {
  expect_identical(bulk_value_to_text("bool", TRUE), "TRUE")
  expect_identical(bulk_value_to_text("bool", FALSE), "FALSE")
  expect_identical(bulk_value_to_text("bool", NA), NA_character_)
  expect_identical(bulk_untext_value("bool", "TRUE"), TRUE)
  expect_identical(bulk_untext_value("bool", "FALSE"), FALSE)
  expect_identical(bulk_untext_value("bool", NA_character_), NA)

  expect_identical(bulk_value_to_text("fk", 7L), "7")
  expect_identical(bulk_untext_value("fk", "7"), 7L)
  expect_identical(bulk_untext_value("fk", NA_character_), NA_integer_)

  expect_identical(bulk_value_to_text("text", "hej"), "hej")
  expect_identical(bulk_untext_value("text", "hej"), "hej")
  expect_identical(bulk_untext_value("text", NA_character_), NA_character_)
})

test_that("build_bulk_lock_sql/build_bulk_update_sql bruger FOR UPDATE + array-literal", {
  sql_lock <- build_bulk_lock_sql("tblIndikatorer", "id", "kontaktperson")
  expect_match(sql_lock, 'SELECT "id", "kontaktperson" FROM "tblIndikatorer"')
  expect_match(sql_lock, 'WHERE "id" = ANY\\(\\$1::int\\[\\]\\)')
  expect_match(sql_lock, 'ORDER BY "id" FOR UPDATE')

  sql_upd <- build_bulk_update_sql("tblIndikatorer", "id", "kontaktperson")
  expect_identical(sql_upd,
    'UPDATE "tblIndikatorer" SET "kontaktperson" = $1 WHERE "id" = ANY($2::int[])')
})

test_that("build_audit_batch_insert_sql genererer batch_id server-side", {
  sql <- build_audit_batch_insert_sql('"audit"."tbl_batch"')
  expect_match(sql, "gen_random_uuid\\(\\)")
  expect_match(sql, 'INSERT INTO "audit"\\."tbl_batch"')
  expect_match(sql, "RETURNING \"batch_id\"")
})

test_that("build_audit_row_insert_sql bygger N tupler med korrekt placeholder-offset", {
  sql1 <- build_audit_row_insert_sql(1, '"audit"."tbl_batch_raekke"')
  expect_identical(sql1, paste0(
    'INSERT INTO "audit"."tbl_batch_raekke" ',
    '("batch_id", "row_id", "vaerdi_foer", "vaerdi_efter") VALUES ($1, $2, $3, $4)'
  ))

  sql3 <- build_audit_row_insert_sql(3, '"audit"."tbl_batch_raekke"')
  expect_match(sql3, "\\(\\$1, \\$2, \\$3, \\$4\\), \\(\\$1, \\$5, \\$6, \\$7\\), \\(\\$1, \\$8, \\$9, \\$10\\)")
})

test_that("audit-select/mark-undone-buildere refererer den rette batch_id-kolonne", {
  expect_match(build_audit_batch_select_sql('"audit"."tbl_batch"'), 'WHERE "batch_id" = \\$1')
  expect_match(build_audit_rows_select_sql('"audit"."tbl_batch_raekke"'),
               'ORDER BY "row_id"')
  expect_match(build_audit_mark_undone_sql('"audit"."tbl_batch"'),
               'SET "fortrudt_ts" = now\\(\\) WHERE "batch_id" = \\$1')
})

test_that(".bulk_update_impl afviser ukendt tabel/felt FØR nogen DB-forbindelse bruges", {
  # pool = NULL beviser at disse tjek sker inden pool::poolWithTransaction
  # overhovedet kaldes (allowlist-opslag før DB-kald, krav 5).
  withr::with_envvar(c(BFHMETA_WRITE = "1"), {
    expect_error(
      .bulk_update_impl(NULL, "ukendt_tabel", 1, "v", "x", list(`1` = "y")),
      "Ukendt bulk-tabel"
    )
    expect_error(
      .bulk_update_impl(NULL, "indikator", 1, "ikke_et_felt", "x", list(`1` = "y")),
      "ikke tilladt"
    )
  })
})

test_that(".bulk_update_impl afviser dubletter og expected_before-mismatch uden DB", {
  withr::with_envvar(c(BFHMETA_WRITE = "1"), {
    err <- tryCatch(
      .bulk_update_impl(NULL, "indikator", c(1, 1, 2), "kontaktperson", 5,
                        list(`1` = 3, `2` = 3)),
      bulk_conflict = function(e) e
    )
    expect_s3_class(err, "bulk_conflict")
    expect_identical(err$type, "duplicate")
    expect_identical(err$ids, "1")

    expect_error(
      .bulk_update_impl(NULL, "indikator", c(1, 2), "kontaktperson", 5,
                        list(`1` = 3)),
      "expected_before"
    )
  })
})

test_that(".bulk_update_impl kræver skrivning aktiveret (assert_write_enabled)", {
  withr::with_envvar(c(BFHMETA_WRITE = ""), {
    withr::with_options(list(bfhmeta.write_enabled = NULL), {
      expect_error(
        .bulk_update_impl(NULL, "indikator", 1, "kontaktperson", 5, list(`1` = 3)),
        "skrivning"
      )
    })
  })
})

test_that("bulk_conflict() bærer type + ids som strukturerede felter", {
  e <- bulk_conflict("stale", c("3", "7"))
  expect_s3_class(e, "bulk_conflict")
  expect_s3_class(e, "error")
  expect_identical(e$type, "stale")
  expect_identical(e$ids, c("3", "7"))
  expect_match(e$message, "3, 7")

  caught <- tryCatch(stop(e), bulk_conflict = function(err) err)
  expect_identical(caught$type, "stale")
})
