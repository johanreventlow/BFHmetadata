# Integration: kræver rigtig Supabase + skrivning aktiveret. Skippes ellers.
skip_if_no_db <- function() {
  testthat::skip_if_not(identical(Sys.getenv("BFHMETA_WRITE"), "1"),
                        "BFHMETA_WRITE!=1 — springer DB-integration over")
  testthat::skip_if_not(nzchar(Sys.getenv("SUPABASE_DB_PASSWORD")),
                        "SUPABASE_DB_PASSWORD mangler — springer DB-integration over")
}

test_that("diagram-CRUD roundtrip: create -> update -> read -> delete", {
  skip_if_no_db()
  pool <- db_connect()
  withr::defer(pool::poolClose(pool))
  db <- make_db(pool)

  # Kendte FK-ids fra DB (mindste eksisterende — stabile referencer)
  ind <- DBI::dbGetQuery(pool, 'SELECT MIN("id") AS id FROM "tblIndikatorer"')$id[1]
  org <- DBI::dbGetQuery(pool,
    'SELECT MIN("Id") AS id FROM "tblOrganisationStruktur"')$id[1]
  typ <- DBI::dbGetQuery(pool, 'SELECT MIN("Id") AS id FROM "tblDiagramTyper"')$id[1]

  vals <- list(indikator = ind, organisatorisk_navn_teknisk = org,
               diagram_type = typ, periode_aggregering = NA_character_,
               indgaar_i_aggregering = FALSE, diagram_aktivt = FALSE,
               direktionens_tavle = FALSE)

  new_id <- db$create_diagram(vals)
  # Oprydning selv hvis assertions fejler undervejs
  withr::defer(try(db$delete_diagram(new_id), silent = TRUE))
  expect_true(is.numeric(new_id) && new_id > 0)

  # Duplikat-guard ser den nye række (ekskludér egen id → 0)
  expect_gte(db$diagram_duplicate_count(ind, org, typ, exclude_id = -1L), 1L)
  expect_identical(db$diagram_duplicate_count(ind, org, typ,
                                              exclude_id = new_id), 0L)

  # Opdatér periode og læs tilbage via admin-listen
  vals$periode_aggregering <- "måned"
  db$update_diagram(new_id, vals)
  adm <- db$list_diagrams_admin()
  row <- adm[adm$diagram_id == new_id, , drop = FALSE]
  expect_identical(nrow(row), 1L)
  expect_identical(row$periode_aggregering, "måned")

  # Nyt diagram har ingen median-knæk
  expect_identical(db$diagram_median_count(new_id), 0L)

  # Periode-choices indeholder mindst den netop skrevne værdi
  expect_true("måned" %in% db$diagram_periode_choices())

  # Slet og verificér væk
  db$delete_diagram(new_id)
  adm2 <- db$list_diagrams_admin()
  expect_false(new_id %in% adm2$diagram_id)
})
