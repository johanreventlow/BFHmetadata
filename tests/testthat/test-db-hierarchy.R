# Integration: kræver rigtig Supabase + skrivning aktiveret. Skippes ellers.
skip_if_no_db <- function() {
  testthat::skip_if_not(identical(Sys.getenv("BFHMETA_WRITE"), "1"),
                        "BFHMETA_WRITE!=1 — springer DB-integration over")
  testthat::skip_if_not(nzchar(Sys.getenv("SUPABASE_DB_PASSWORD")),
                        "SUPABASE_DB_PASSWORD mangler — springer DB-integration over")
}

test_that("hierarki-CRUD roundtrip: create -> update -> flyt -> read -> delete", {
  skip_if_no_db()
  pool <- db_connect()
  withr::defer(pool::poolClose(pool))
  cfg <- HIERARCHY_TABLES$org_struktur
  db <- make_hierarchy_db(pool, cfg)

  nodes <- db$list_nodes()
  # Kendt rod-node (parent er NA) — stabil reference
  root_id <- nodes$id[is.na(nodes$parent_id_raw)][1]
  other_id <- nodes$id[!is.na(nodes$parent_id_raw)][1]
  niveauer <- db$niveau_options()
  niveau_id <- niveauer$id[1]

  vals <- list(organisatorisk_navn_teknisk = "TEST_hierarki_roundtrip",
               organisatorisk_navn_langt = "Test hierarki roundtrip",
               organisatorisk_navn_kort = "Test",
               parent_Id = root_id, organisatorisk_niveau = niveau_id)

  new_id <- db$create_node(vals)
  withr::defer(try(db$delete_node(new_id), silent = TRUE))
  expect_true(is.numeric(new_id) && new_id > 0)

  expect_gte(db$child_count(root_id), 1L)

  # Opdatér navn + flyt til anden kendt node
  vals$organisatorisk_navn_langt <- "Test hierarki roundtrip (opdateret)"
  vals$parent_Id <- other_id
  db$update_node(new_id, vals)

  refreshed <- db$list_nodes()
  row <- refreshed[refreshed$id == new_id, , drop = FALSE]
  expect_identical(nrow(row), 1L)
  expect_identical(row$organisatorisk_navn_langt,
                   "Test hierarki roundtrip (opdateret)")
  expect_identical(row$parent_id_raw, other_id)

  # Slet og verificér væk
  db$delete_node(new_id)
  refreshed2 <- db$list_nodes()
  expect_false(new_id %in% refreshed2$id)
})

test_that("delete_node fejler loud på node med børn (DB-FK self-join)", {
  skip_if_no_db()
  pool <- db_connect()
  withr::defer(pool::poolClose(pool))
  cfg <- HIERARCHY_TABLES$org_struktur
  db <- make_hierarchy_db(pool, cfg)

  nodes <- db$list_nodes()
  # Node med mindst ét barn (findes med sikkerhed jf. 362 rækker/2 rødder)
  parent_ids <- nodes$parent_id_raw[!is.na(nodes$parent_id_raw)]
  node_with_children <- parent_ids[1]

  expect_error(db$delete_node(node_with_children))
})
