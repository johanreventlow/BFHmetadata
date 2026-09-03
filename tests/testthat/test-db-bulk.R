# Bulk-redigering (docs/plans/2026-08-30-bulk-redigering-design.md):
# integrationstests af .bulk_update_impl()/.bulk_undo_impl() mod en rigtig
# Supabase-forbindelse, engangstabeller efter dev/bulk_probe.R-mønstret
# (unikt navn = PID + timestamp, oprettes/droppes pr. test).
#
# Audit-tabellen i testen spejler audit."tblAendringslog" fra
# migration/07_migration.sql — den log der FAKTISK er deployeret. (Designets
# oprindelige skitse, tbl_batch + tbl_batch_raekke, blev aldrig oprettet.)
# Testen skriver aldrig i produktionens log: audit_log_tbl peger på
# engangstabellen, og tables_cfg på en engangs-domænetabel.

skip_if_no_db <- function() {
  testthat::skip_if_not(identical(Sys.getenv("BFHMETA_WRITE"), "1"),
                        "BFHMETA_WRITE!=1 — springer DB-integration over")
  testthat::skip_if_not(nzchar(Sys.getenv("SUPABASE_DB_PASSWORD")),
                        "SUPABASE_DB_PASSWORD mangler — springer DB-integration over")
}

#' Opretter én domænetabel (id, v) + en kopi af audit."tblAendringslog"s skema
#' i "public", begge med unikt navn (PID + timestamp, som dev/bulk_probe.R).
#' Returnerer quoted tabelnavne + en tables_cfg der peger
#' .bulk_update_impl/.bulk_undo_impl på domænetabellen under tabel_key "test",
#' + en cleanup()-funktion kalderen selv defer'er.
#' @noRd
bulk_probe_setup <- function(pool) {
  suffix <- sprintf("%d_%d", Sys.getpid(), as.integer(Sys.time()))
  dom_tbl <- sprintf("_bulk_test_dom_%s", suffix)
  log_tbl <- sprintf("_bulk_test_log_%s", suffix)
  dom_q <- sprintf('"public"."%s"', dom_tbl)
  log_q <- sprintf('"public"."%s"', log_tbl)

  cleanup <- function() {
    try(DBI::dbExecute(pool, sprintf("DROP TABLE IF EXISTS %s", log_q)), silent = TRUE)
    try(DBI::dbExecute(pool, sprintf("DROP TABLE IF EXISTS %s", dom_q)), silent = TRUE)
  }
  cleanup() # defensiv oprydning af en evt. tidligere efterladt kørsel

  DBI::dbExecute(pool, sprintf('CREATE TABLE %s ("id" int primary key, "v" text)', dom_q))
  # Skemaet SKAL spejle produktionens, saerligt jsonb NOT NULL paa
  # vaerdi_foer/vaerdi_efter — det er praecis dér en manglende vaerdi ville
  # fejle, hvis den blev skrevet som SQL NULL i stedet for JSON-null.
  DBI::dbExecute(pool, sprintf(paste(
    'CREATE TABLE %s ("id" bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,',
    '"batch_id" uuid NOT NULL, "tidspunkt" timestamptz NOT NULL DEFAULT now(),',
    '"bruger" text, "tabel" text NOT NULL, "post_id" bigint NOT NULL,',
    '"kolonne" text NOT NULL, "vaerdi_foer" jsonb NOT NULL,',
    '"vaerdi_efter" jsonb NOT NULL, "vaerdi_type" text NOT NULL,',
    '"fortrudt_tidspunkt" timestamptz, "fortrudt_af" text)'
  ), log_q))
  DBI::dbExecute(pool, sprintf(
    "INSERT INTO %s (\"id\", \"v\") SELECT g, 'start' FROM generate_series(1, 5) g",
    dom_q
  ))

  list(
    dom_q = dom_q, log_q = log_q,
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
                           audit_log_tbl = setup$log_q, tables_cfg = setup$tables_cfg)

  expect_identical(res$n, 2L)
  expect_setequal(res$skipped, 3L)
  expect_true(nzchar(res$batch_id))

  after <- DBI::dbGetQuery(pool, sprintf('SELECT "id", "v" FROM %s ORDER BY "id"', setup$dom_q))
  expect_identical(after$v[after$id %in% c(1, 2, 3)], c("ny", "ny", "ny"))
  expect_identical(after$v[after$id %in% c(4, 5)], c("start", "start"))

  audit <- DBI::dbGetQuery(pool, sprintf(paste(
    'SELECT "post_id", "vaerdi_foer"::text AS f, "vaerdi_efter"::text AS e,',
    '"vaerdi_type", "tabel", "kolonne" FROM %s WHERE "batch_id" = $1',
    'ORDER BY "post_id"'
  ), setup$log_q), params = list(res$batch_id))

  expect_identical(nrow(audit), 2L)          # kun de FAKTISK ændrede rækker
  expect_setequal(as.integer(audit$post_id), c(1L, 2L))
  expect_true(all(audit$f == '"start"'))     # jsonb: tekst gemmes med anførselstegn
  expect_true(all(audit$e == '"ny"'))
  expect_true(all(audit$vaerdi_type == "text"))
  expect_true(all(audit$kolonne == "v"))
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
                      audit_log_tbl = setup$log_q, tables_cfg = setup$tables_cfg),
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
                      audit_log_tbl = setup$log_q, tables_cfg = setup$tables_cfg),
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

  # audit_log_tbl peger på en tabel der ikke findes: domæne-UPDATE'en når at
  # eksekvere INDE i transaktionen, FØR audit-INSERT'et fejler — beviser at en
  # senere statement-fejl ruller ALLE forudgående writes tilbage, ikke kun sin egen.
  bad_log_tbl <- '"public"."findes_slet_ikke_bulk_test"'
  expected_before <- list(`1` = "start", `2` = "start")

  expect_error(
    .bulk_update_impl(pool, "test", c(1, 2), "v", "ny", expected_before,
                      audit_log_tbl = bad_log_tbl, tables_cfg = setup$tables_cfg)
  )

  after <- DBI::dbGetQuery(pool, sprintf('SELECT "id", "v" FROM %s ORDER BY "id"', setup$dom_q))
  expect_identical(after, before) # BEVIS: den "lykkedes" UPDATE er fuldt rullet tilbage

  n <- DBI::dbGetQuery(pool, sprintf('SELECT count(*) AS n FROM %s', setup$log_q))
  expect_identical(as.integer(n$n[1]), 0L) # intet auditeret
})

test_that("bulk_undo: gendanner original værdi og stempler fortrudt", {
  skip_if_no_db()
  pool <- db_connect()
  withr::defer(pool::poolClose(pool))
  setup <- bulk_probe_setup(pool)
  withr::defer(setup$cleanup())

  expected_before <- list(`1` = "start", `2` = "start")
  res <- .bulk_update_impl(pool, "test", c(1, 2), "v", "ny", expected_before,
                           audit_log_tbl = setup$log_q, tables_cfg = setup$tables_cfg)

  undo <- .bulk_undo_impl(pool, res$batch_id,
                          audit_log_tbl = setup$log_q, tables_cfg = setup$tables_cfg)
  expect_identical(undo$n, 2L)

  after <- DBI::dbGetQuery(pool,
    sprintf('SELECT "id", "v" FROM %s WHERE "id" IN (1, 2) ORDER BY "id"', setup$dom_q))
  expect_identical(after$v, c("start", "start"))

  stempel <- DBI::dbGetQuery(pool, sprintf(
    'SELECT "fortrudt_tidspunkt" FROM %s WHERE "batch_id" = $1', setup$log_q),
    params = list(res$batch_id))
  expect_true(all(!is.na(stempel$fortrudt_tidspunkt)))

  expect_error(
    .bulk_undo_impl(pool, res$batch_id, audit_log_tbl = setup$log_q,
                    tables_cfg = setup$tables_cfg),
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
                           audit_log_tbl = setup$log_q, tables_cfg = setup$tables_cfg)

  # nogen retter id 2 manuelt EFTER batchen, uden om bulk-flowet
  DBI::dbExecute(pool, sprintf('UPDATE %s SET "v" = $1 WHERE "id" = 2', setup$dom_q),
                 params = list("manuelt-rettet"))

  err <- tryCatch(
    .bulk_undo_impl(pool, res$batch_id, audit_log_tbl = setup$log_q,
                    tables_cfg = setup$tables_cfg),
    bulk_conflict = function(e) e
  )
  expect_s3_class(err, "bulk_conflict")
  expect_identical(err$type, "undo_conflict")
  expect_identical(err$ids, "2")

  after <- DBI::dbGetQuery(pool,
    sprintf('SELECT "id", "v" FROM %s WHERE "id" IN (1, 2) ORDER BY "id"', setup$dom_q))
  # INGEN delvis fortryd: id 1 er STADIG "ny" (ikke gendannet), selvom kun id 2 konfliktede
  expect_identical(after$v, c("ny", "manuelt-rettet"))

  stempel <- DBI::dbGetQuery(pool, sprintf(
    'SELECT "fortrudt_tidspunkt" FROM %s WHERE "batch_id" = $1', setup$log_q),
    params = list(res$batch_id))
  expect_true(all(is.na(stempel$fortrudt_tidspunkt))) # batchen er IKKE stemplet
})

test_that("bulk_update: tom værdi gemmes som JSON-null (jsonb NOT NULL holder)", {
  skip_if_no_db()
  pool <- db_connect()
  withr::defer(pool::poolClose(pool))
  setup <- bulk_probe_setup(pool)
  withr::defer(setup$cleanup())

  # Et text-felt MÅ ryddes. Auditens vaerdi_efter er jsonb NOT NULL, så den
  # tomme værdi skal lande som JSON-null — ellers afviser databasen batchen.
  res <- .bulk_update_impl(pool, "test", c(1, 2), "v", NA,
                           list(`1` = "start", `2` = "start"),
                           audit_log_tbl = setup$log_q, tables_cfg = setup$tables_cfg)
  expect_identical(res$n, 2L)

  after <- DBI::dbGetQuery(pool,
    sprintf('SELECT "v" FROM %s WHERE "id" IN (1, 2)', setup$dom_q))
  expect_true(all(is.na(after$v)))

  audit <- DBI::dbGetQuery(pool, sprintf(
    'SELECT "vaerdi_efter"::text AS e FROM %s WHERE "batch_id" = $1', setup$log_q),
    params = list(res$batch_id))
  expect_true(all(audit$e == "null"))

  # Og fortryd kan laese den tilbage igen
  undo <- .bulk_undo_impl(pool, res$batch_id, audit_log_tbl = setup$log_q,
                          tables_cfg = setup$tables_cfg)
  expect_identical(undo$n, 2L)
  restored <- DBI::dbGetQuery(pool,
    sprintf('SELECT "v" FROM %s WHERE "id" IN (1, 2)', setup$dom_q))
  expect_true(all(restored$v == "start"))
})
