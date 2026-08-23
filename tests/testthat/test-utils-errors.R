test_that("kendte Postgres-fejl oversættes til dansk (realistiske RPostgres-fejltekster)", {
  # RPostgres pakker fejlen ind i "Failed to prepare query: ..." og medtager
  # ALDRIG SQLSTATE — kun den engelske constraint-tekst fra Postgres-serveren.
  # Disse tekster afprøver derfor kun den engelske gren i pg_besked(), ikke
  # SQLSTATE-grenene (se kommentar i R/utils_errors.R).
  expect_match(pg_besked(paste0(
    'Failed to prepare query: ERROR:  update or delete on table "indikator" ',
    'violates foreign key constraint "fk_faggruppe_indikator" on table ',
    '"indikator_faggruppe"\nDETAIL:  Key (id)=(12) is still referenced from ',
    'table "indikator_faggruppe".')),
    "i brug")
  expect_match(pg_besked(paste0(
    'Failed to prepare query: ERROR:  duplicate key value violates unique ',
    'constraint "indikator_navn_teknisk_key"\nDETAIL:  Key ',
    '(indikator_navn_teknisk)=(test) already exists.')),
    "findes allerede")
  expect_match(pg_besked(paste0(
    'Failed to prepare query: ERROR:  null value in column "indikator_navn" ',
    'violates not-null constraint')),
    "ikke.*tomt")
  expect_match(pg_besked("server closed the connection unexpectedly"), "afbrudt")
  expect_match(pg_besked(paste0(
    'could not connect to server: Connection refused\n\tIs the server ',
    'running on host "db.example.dk" and accepting\n\tTCP/IP connections ',
    'on port 5432?')),
    "afbrudt")
})

test_that("ukendt/uoversat fejltekst giver NULL fra pg_besked (gennemfaldssti)", {
  # Rammes hvis Postgres' formuleringer ændrer sig — kaldere falder da
  # tilbage til deres egen generiske besked.
  expect_null(pg_besked('Failed to prepare query: ERROR:  syntax error at or near "SELECT"'))
})

test_that("med_ventevisning returnerer værdien uændret uden for Shiny", {
  expect_equal(med_ventevisning("Gemmer…", 7), 7)
})

test_that("med_ventevisning propagerer fejl", {
  expect_error(med_ventevisning("Gemmer…", stop("bang")), "bang")
})

test_that("med_ventevisning viser og fjerner notifikation omkring et succesfuldt kald", {
  calls <- character(0)
  # NB: intet .package her — showNotification()/removeNotification() kaldes
  # UPRÆFIKSEREDE inde i med_ventevisning() (import(shiny) i NAMESPACE), så
  # bindingen der skal mockes sidder i BFHmetadatas EGEN imports-env, ikke i
  # shiny's. Empirisk verificeret: med .package = "shiny" fanger mocket intet
  # (calls forblev tom), fordi det da mocker shiny::-kald, ikke bare kald.
  testthat::local_mocked_bindings(
    getDefaultReactiveDomain = function() TRUE,
    showNotification = function(...) { calls <<- c(calls, "show"); "id1" },
    removeNotification = function(...) calls <<- c(calls, "remove")
  )
  res <- med_ventevisning("Gemmer…", 42)
  expect_equal(res, 42)
  expect_equal(calls, c("show", "remove"))
})

test_that("med_ventevisning fjerner notifikationen SELV OM kaldet fejler", {
  calls <- character(0)
  testthat::local_mocked_bindings(
    getDefaultReactiveDomain = function() TRUE,
    showNotification = function(...) { calls <<- c(calls, "show"); "id1" },
    removeNotification = function(...) calls <<- c(calls, "remove")
  )
  expect_error(med_ventevisning("Gemmer…", stop("bang")), "bang")
  expect_equal(calls, c("show", "remove"))
})
