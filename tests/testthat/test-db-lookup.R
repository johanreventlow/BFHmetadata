# Integration: kræver rigtig Supabase + skrivning aktiveret. Skippes ellers.
# Verificerer skrive-stien for opslagstabeller (add/update/delete) som fake_db
# ikke kan dække: DEFAULT VALUES-insert, NULL-skrivning, FK-RESTRICT, ref-guard.
skip_if_no_db <- function() {
  testthat::skip_if_not(identical(Sys.getenv("BFHMETA_WRITE"), "1"),
                        "BFHMETA_WRITE!=1 — springer DB-integration over")
}

test_that("lookup add → update → NULL → delete round-trip (tblFaggrupper)", {
  skip_if_no_db()
  pool <- db_connect()
  cfg <- Find(function(c) c$id == "faggrupper", LOOKUP_TABLES)
  db <- make_lookup_db(pool, cfg)
  newid <- db$add_row()                                   # DEFAULT VALUES insert
  # Oprydning FØR poolClose (FIFO): slet test-rækken, luk så pool
  on.exit(try(db$delete_row(newid), silent = TRUE), add = TRUE)
  on.exit(pool::poolClose(pool), add = TRUE)

  expect_true(newid %in% db$list_rows()$Id)
  db$update_cell(newid, "faggruppe", "ZZ_TESTGRUPPE")
  d <- db$list_rows()
  expect_equal(d$faggruppe[d$Id == newid], "ZZ_TESTGRUPPE")
  # Tom værdi (NA) → skrives som NULL uden fejl
  db$update_cell(newid, "faggruppe", NA)
  d <- db$list_rows()
  expect_true(is.na(d$faggruppe[d$Id == newid]))
  db$delete_row(newid)
  expect_false(newid %in% db$list_rows()$Id)
})

test_that("datakilde i brug → ref_count > 0 (app-niveau slet-guard)", {
  skip_if_no_db()
  pool <- db_connect(); on.exit(pool::poolClose(pool))
  cfg <- Find(function(c) c$id == "datakilder", LOOKUP_TABLES)
  db <- make_lookup_db(pool, cfg)
  inuse <- db$list_rows()$Id[1]
  expect_gt(db$ref_count(inuse), 0)   # modulet ville blokere sletning
})

test_that("slet af brugt faggruppe afvises af DB (FK-RESTRICT)", {
  skip_if_no_db()
  pool <- db_connect(); on.exit(pool::poolClose(pool))
  cfg <- Find(function(c) c$id == "faggrupper", LOOKUP_TABLES)
  db <- make_lookup_db(pool, cfg)
  used <- DBI::dbGetQuery(pool,
    'SELECT "faggruppe_id" FROM "tblForbindIndikatorerFaggrupper" LIMIT 1')[[1]][1]
  expect_error(db$delete_row(used))   # modulets tryCatch oversætter → "i brug"
})
