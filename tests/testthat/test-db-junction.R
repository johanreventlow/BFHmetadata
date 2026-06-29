# Integration: kræver rigtig Supabase + skrivning aktiveret. Skippes ellers.
skip_if_no_db <- function() {
  testthat::skip_if_not(identical(Sys.getenv("BFHMETA_WRITE"), "1"),
                        "BFHMETA_WRITE!=1 — springer DB-integration over")
}

test_that("set_junction replace-roundtrip + tom selektion + rollback", {
  skip_if_no_db()
  pool <- db_connect()
  db <- make_db(pool)
  # Vælg en eksisterende indikator-id (mindste aktive)
  id <- DBI::dbGetQuery(pool, 'SELECT MIN("id") AS id FROM "tblIndikatorer"')$id[1]
  before <- db$get_junction(id, "faggrupper")
  # Gendan original state FØR pool lukkes (FIFO: restore kører før poolClose)
  on.exit(db$set_junction(id, "faggrupper", before), add = TRUE)
  on.exit(pool::poolClose(pool), add = TRUE)

  opts <- db$junction_options("faggrupper")$id
  pick <- head(opts, 2)
  db$set_junction(id, "faggrupper", pick)
  expect_setequal(db$get_junction(id, "faggrupper"), pick)

  # Tom selektion → kun delete
  db$set_junction(id, "faggrupper", integer(0))
  expect_length(db$get_junction(id, "faggrupper"), 0)

  # Rollback: ugyldig parent-id (FK-violation) → ingen ændring
  db$set_junction(id, "faggrupper", pick)            # sæt kendt udgangspunkt
  expect_error(db$set_junction(id, "faggrupper", c(pick[1], -999999L)))
  expect_setequal(db$get_junction(id, "faggrupper"), pick)  # uændret efter rollback
})

test_that("save_indikator: samlet scalar+junction-gem er atomisk", {
  skip_if_no_db()
  pool <- db_connect()
  db <- make_db(pool)
  id <- DBI::dbGetQuery(pool, 'SELECT MIN("id") AS id FROM "tblIndikatorer"')$id[1]
  orig_navn <- DBI::dbGetQuery(pool,
    'SELECT "indikator_navn" AS n FROM "tblIndikatorer" WHERE "id" = $1',
    params = list(id))$n[1]
  before_fag <- db$get_junction(id, "faggrupper")
  # Gendan original state FØR pool lukkes
  on.exit(db$save_indikator(id, list(indikator_navn = orig_navn),
                            list(faggrupper = before_fag)), add = TRUE)
  on.exit(pool::poolClose(pool), add = TRUE)

  pick <- head(db$junction_options("faggrupper")$id, 2)

  # Succesfuld samlet gem: scalar + junction commit'er sammen
  db$save_indikator(id, list(indikator_navn = "TEST_ATOMIC"),
                    list(faggrupper = pick))
  navn <- DBI::dbGetQuery(pool,
    'SELECT "indikator_navn" AS n FROM "tblIndikatorer" WHERE "id" = $1',
    params = list(id))$n[1]
  expect_equal(navn, "TEST_ATOMIC")
  expect_setequal(db$get_junction(id, "faggrupper"), pick)

  # Rollback: junction-INSERT fejler (FK-violation) → scalar-UPDATE ruller
  # OGSÅ tilbage (samme transaktion). Navn forbliver "TEST_ATOMIC".
  expect_error(db$save_indikator(id, list(indikator_navn = "SKAL_RULLE_TILBAGE"),
                                 list(faggrupper = c(pick[1], -999999L))))
  navn2 <- DBI::dbGetQuery(pool,
    'SELECT "indikator_navn" AS n FROM "tblIndikatorer" WHERE "id" = $1',
    params = list(id))$n[1]
  expect_equal(navn2, "TEST_ATOMIC")  # scalar rullede tilbage med junction
})
