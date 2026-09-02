# Bulk-redigering, Leverance 2 (docs/plans/2026-08-30-bulk-redigering-design.md):
# integrationstests af .bulk_update_impl()/.bulk_undo_impl() mod en rigtig
# Supabase-forbindelse, engangstabeller efter dev/bulk_probe.R-mønstret
# (unikt navn = PID + timestamp, oprettes/droppes pr. test).
#
# audit.tbl_batch/audit.tbl_batch_raekke findes IKKE endnu (Leverance 3's
# migration er ikke kørt) — derfor peger hvert kald eksplicit på
# engangs-audit-tabeller (audit_batch_tbl/audit_row_tbl) og en engangs-
# BULK_TABLES-erstatning (tables_cfg), i stedet for de rigtige defaults.
# Kun .bulk_update_impl()/.bulk_undo_impl() (ikke db$bulk_update()/
# db$bulk_undo() fra make_db()) kan parametriseres sådan.

skip_if_no_db <- function() {
  testthat::skip_if_not(identical(Sys.getenv("BFHMETA_WRITE"), "1"),
                        "BFHMETA_WRITE!=1 — springer DB-integration over")
  testthat::skip_if_not(nzchar(Sys.getenv("SUPABASE_DB_PASSWORD")),
                        "SUPABASE_DB_PASSWORD mangler — springer DB-integration over")
}

#' Opretter én domænetabel (id, v) + audit.tbl_batch/tbl_batch_raekke-
#' erstatninger i "public", alle med unikt navn (PID + timestamp, som
#' dev/bulk_probe.R). Returnerer quoted tabelnavne + en tables_cfg der
#' peger .bulk_update_impl/.bulk_undo_impl på domænetabellen under
#' tabel_key "test", + en cleanup()-funktion kalderen selv defer'er.
#' @noRd
bulk_probe_setup <- function(pool) {
  suffix <- sprintf("%d_%d", Sys.getpid(), as.integer(Sys.time()))
  dom_tbl <- sprintf("_bulk_test_dom_%s", suffix)
  batch_tbl <- sprintf("_bulk_test_batch_%s", suffix)
  row_tbl <- sprintf("_bulk_test_row_%s", suffix)
  dom_q <- sprintf('"public"."%s"', dom_tbl)
  batch_q <- sprintf('"public"."%s"', batch_tbl)
  row_q <- sprintf('"public"."%s"', row_tbl)

  cleanup <- function() {
    try(DBI::dbExecute(pool, sprintf("DROP TABLE IF EXISTS %s", row_q)), silent = TRUE)
    try(DBI::dbExecute(pool, sprintf("DROP TABLE IF EXISTS %s", batch_q)), silent = TRUE)
    try(DBI::dbExecute(pool, sprintf("DROP TABLE IF EXISTS %s", dom_q)), silent = TRUE)
  }
  cleanup() # defensiv oprydning af en evt. tidligere efterladt kørsel

  DBI::dbExecute(pool, sprintf('CREATE TABLE %s ("id" int primary key, "v" text)', dom_q))
  DBI::dbExecute(pool, sprintf(paste(
    'CREATE TABLE %s ("batch_id" uuid primary key, "tabel" text not null,',
    '"felt" text not null, "udfoert_ts" timestamptz not null default now(),',
    '"fortrudt_ts" timestamptz)'
  ), batch_q))
  DBI::dbExecute(pool, sprintf(paste(
    'CREATE TABLE %s ("batch_id" uuid not null references %s("batch_id"),',
    '"row_id" int not null, "vaerdi_foer" text, "vaerdi_efter" text,',
    'primary key ("batch_id", "row_id"))'
  ), row_q, batch_q))
  DBI::dbExecute(pool, sprintf(
    "INSERT INTO %s (\"id\", \"v\") SELECT g, 'start' FROM generate_series(1, 5) g",
    dom_q
  ))

  list(
    dom_q = dom_q, batch_q = batch_q, row_q = row_q,
    tables_cfg = list(test = list(
      table = dom_tbl, pk = "id",
      fields = list(list(col = "v", kind = "text"))
    )),
    cleanup = cleanup
  )
}

test_that("bulk_update: happy path skriver + auditerer, springer uændrede rækker over", {
  skip_if_no_db()
  pool <- db_connect()
  withr::defer(pool::poolClose(pool))
  setup <- bulk_probe_setup(pool)
  withr::defer(setup$cleanup())

  # id 3 har allerede målværdien — skal rapporteres "skipped", ikke skrives/auditeres
  DBI::dbExecute(pool, sprintf('UPDATE %s SET "v" = $1 WHERE "id" = 3', setup$dom_q),
                 params = list("ny"))
  expected_before <- list(`1` = "start", `2` = "start", `3` = "ny")

  res <- .bulk_update_impl(pool, "test", c(1, 2, 3), "v", "ny", expected_before,
                           audit_batch_tbl = setup$batch_q, audit_row_tbl = setup$row_q,
                           tables_cfg = setup$tables_cfg)

  expect_identical(res$n, 2L)
  expect_setequal(res$skipped, 3L)
  expect_true(nzchar(res$batch_id))

  after <- DBI::dbGetQuery(pool, sprintf('SELECT "id", "v" FROM %s ORDER BY "id"', setup$dom_q))
  expect_identical(after$v[after$id %in% c(1, 2, 3)], c("ny", "ny", "ny"))
  expect_identical(after$v[after$id %in% c(4, 5)], c("start", "start"))

  audit_rows <- DBI::dbGetQuery(pool,
    sprintf('SELECT * FROM %s WHERE "batch_id" = $1', setup$row_q),
    params = list(res$batch_id))
  expect_identical(nrow(audit_rows), 2L)
  expect_setequal(audit_rows$row_id, c(1L, 2L))
  expect_true(all(audit_rows$vaerdi_foer == "start"))
  expect_true(all(audit_rows$vaerdi_efter == "ny"))
})

test_that("bulk_update: manglende id afviser hele batchen — ingen skrivning", {
  skip_if_no_db()
  pool <- db_connect()
  withr::defer(pool::poolClose(pool))
  setup <- bulk_probe_setup(pool)
  withr::defer(setup$cleanup())

  expected_before <- list(`1` = "start", `999` = "start")
  err <- tryCatch(
    .bulk_update_impl(pool, "test", c(1, 999), "v", "ny", expected_before,
                      audit_batch_tbl = setup$batch_q, audit_row_tbl = setup$row_q,
                      tables_cfg = setup$tables_cfg),
    bulk_conflict = function(e) e
  )
  expect_s3_class(err, "bulk_conflict")
  expect_identical(err$type, "missing")
  expect_identical(err$ids, "999")

  after <- DBI::dbGetQuery(pool, sprintf('SELECT "v" FROM %s WHERE "id" = 1', setup$dom_q))
  expect_identical(after$v, "start")
})

test_that("bulk_update: stale førværdi afviser hele batchen — ingen skrivning", {
  skip_if_no_db()
  pool <- db_connect()
  withr::defer(pool::poolClose(pool))
  setup <- bulk_probe_setup(pool)
  withr::defer(setup$cleanup())

  # simulerer en anden bruger der allerede har ændret id 2 siden forhåndsvisningen
  DBI::dbExecute(pool, sprintf('UPDATE %s SET "v" = $1 WHERE "id" = 2', setup$dom_q),
                 params = list("aendret-af-andre"))
  expected_before <- list(`1` = "start", `2` = "start") # UI'et så den GAMLE værdi

  err <- tryCatch(
    .bulk_update_impl(pool, "test", c(1, 2), "v", "ny", expected_before,
                      audit_batch_tbl = setup$batch_q, audit_row_tbl = setup$row_q,
                      tables_cfg = setup$tables_cfg),
    bulk_conflict = function(e) e
  )
  expect_s3_class(err, "bulk_conflict")
  expect_identical(err$type, "stale")
  expect_identical(err$ids, "2")

  after <- DBI::dbGetQuery(pool,
    sprintf('SELECT "id", "v" FROM %s WHERE "id" IN (1, 2) ORDER BY "id"', setup$dom_q))
  expect_identical(after$v, c("start", "aendret-af-andre")) # id 1 uændret — rullet tilbage
})

test_that(paste(
  "bulk_update: tvungen fejl EFTER domæne-UPDATE'en (audit-skrivning fejler)",
  "ruller ALT tilbage"
), {
  skip_if_no_db()
  pool <- db_connect()
  withr::defer(pool::poolClose(pool))
  setup <- bulk_probe_setup(pool)
  withr::defer(setup$cleanup())

  before <- DBI::dbGetQuery(pool, sprintf('SELECT "id", "v" FROM %s ORDER BY "id"', setup$dom_q))

  # audit_row_tbl peger på en tabel der ikke findes: domæne-UPDATE'en og
  # audit-batch-header'en når begge at eksekvere INDE i transaktionen, FØR
  # audit-row-INSERT'et fejler — beviser at et senere statement-fejl i
  # samme transaktion ruller ALLE forudgående writes tilbage, ikke kun sit eget.
  bad_row_tbl <- '"public"."findes_slet_ikke_bulk_test"'
  expected_before <- list(`1` = "start", `2` = "start")

  expect_error(
    .bulk_update_impl(pool, "test", c(1, 2), "v", "ny", expected_before,
                      audit_batch_tbl = setup$batch_q, audit_row_tbl = bad_row_tbl,
                      tables_cfg = setup$tables_cfg)
  )

  after <- DBI::dbGetQuery(pool, sprintf('SELECT "id", "v" FROM %s ORDER BY "id"', setup$dom_q))
  expect_identical(after, before) # BEVIS: den "lykkedes" UPDATE er fuldt rullet tilbage

  headers <- DBI::dbGetQuery(pool, sprintf('SELECT count(*) AS n FROM %s', setup$batch_q))
  expect_identical(as.integer(headers$n[1]), 0L) # batch-headeren blev også rullet tilbage
})

test_that("bulk_undo: gendanner original værdi og stempler fortrudt_ts", {
  skip_if_no_db()
  pool <- db_connect()
  withr::defer(pool::poolClose(pool))
  setup <- bulk_probe_setup(pool)
  withr::defer(setup$cleanup())

  expected_before <- list(`1` = "start", `2` = "start")
  res <- .bulk_update_impl(pool, "test", c(1, 2), "v", "ny", expected_before,
                           audit_batch_tbl = setup$batch_q, audit_row_tbl = setup$row_q,
                           tables_cfg = setup$tables_cfg)

  undo <- .bulk_undo_impl(pool, res$batch_id,
                          audit_batch_tbl = setup$batch_q, audit_row_tbl = setup$row_q,
                          tables_cfg = setup$tables_cfg)
  expect_identical(undo$n, 2L)

  after <- DBI::dbGetQuery(pool,
    sprintf('SELECT "id", "v" FROM %s WHERE "id" IN (1, 2) ORDER BY "id"', setup$dom_q))
  expect_identical(after$v, c("start", "start"))

  hdr <- DBI::dbGetQuery(pool,
    sprintf('SELECT "fortrudt_ts" FROM %s WHERE "batch_id" = $1', setup$batch_q),
    params = list(res$batch_id))
  expect_false(is.na(hdr$fortrudt_ts[1]))

  expect_error(
    .bulk_undo_impl(pool, res$batch_id, audit_batch_tbl = setup$batch_q,
                    audit_row_tbl = setup$row_q, tables_cfg = setup$tables_cfg),
    "allerede fortrudt"
  )
})

test_that("bulk_undo: konflikt (række ændret siden batchen) afviser HELE fortrydelsen", {
  skip_if_no_db()
  pool <- db_connect()
  withr::defer(pool::poolClose(pool))
  setup <- bulk_probe_setup(pool)
  withr::defer(setup$cleanup())

  expected_before <- list(`1` = "start", `2` = "start")
  res <- .bulk_update_impl(pool, "test", c(1, 2), "v", "ny", expected_before,
                           audit_batch_tbl = setup$batch_q, audit_row_tbl = setup$row_q,
                           tables_cfg = setup$tables_cfg)

  # nogen retter id 2 manuelt EFTER batchen, uden om bulk-flowet
  DBI::dbExecute(pool, sprintf('UPDATE %s SET "v" = $1 WHERE "id" = 2', setup$dom_q),
                 params = list("manuelt-rettet"))

  err <- tryCatch(
    .bulk_undo_impl(pool, res$batch_id, audit_batch_tbl = setup$batch_q,
                    audit_row_tbl = setup$row_q, tables_cfg = setup$tables_cfg),
    bulk_conflict = function(e) e
  )
  expect_s3_class(err, "bulk_conflict")
  expect_identical(err$type, "undo_conflict")
  expect_identical(err$ids, "2")

  after <- DBI::dbGetQuery(pool,
    sprintf('SELECT "id", "v" FROM %s WHERE "id" IN (1, 2) ORDER BY "id"', setup$dom_q))
  # INGEN delvis fortryd: id 1 er STADIG "ny" (ikke gendannet), selvom kun id 2 konfliktede
  expect_identical(after$v, c("ny", "manuelt-rettet"))

  hdr <- DBI::dbGetQuery(pool,
    sprintf('SELECT "fortrudt_ts" FROM %s WHERE "batch_id" = $1', setup$batch_q),
    params = list(res$batch_id))
  expect_true(is.na(hdr$fortrudt_ts[1])) # batchen er IKKE markeret fortrudt
})
