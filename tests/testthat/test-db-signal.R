# Integration: kræver rigtig Supabase + skrivning aktiveret. Skippes ellers.
# Verificerer diagram-indeks + median-knæk-skrivesti (INSERT/DELETE) som ikke
# kan dækkes uden DB: DEFAULT/identity-INSERT, RETURNING id, org-niveau-ancestry.
skip_if_no_db <- function() {
  testthat::skip_if_not(identical(Sys.getenv("BFHMETA_WRITE"), "1"),
                        "BFHMETA_WRITE!=1 — springer DB-integration over")
}

test_that("diagram-indeks returnerer aktive Seriediagrammer med labels + org-niveauer", {
  skip_if_no_db()
  pool <- db_connect(); on.exit(pool::poolClose(pool))
  db <- make_db(pool)
  idx <- db$list_active_seriediagrammer()
  expect_gt(nrow(idx), 100)
  expect_true(all(c("diagram_id", "indikator_navn_teknisk", "datasaet",
                    "datapakke", "org_teknisk", "overafdeling", "afdeling",
                    "afsnit") %in% names(idx)))
  # Org-niveau-ancestry: ~268 diagrammer på Overafdeling-niveau har overafdeling
  expect_gt(sum(!is.na(idx$overafdeling)), 100)
})

test_that("median-knæk INSERT → læs → DELETE round-trip", {
  skip_if_no_db()
  pool <- db_connect()
  db <- make_db(pool)
  did <- db$list_active_seriediagrammer()$diagram_id[1]
  newid <- db$add_median_break(did, as.Date("2099-01-01"))  # sikker test-dato
  # Oprydning FØR poolClose (FIFO): slet test-rækken, luk så pool
  on.exit(try(db$delete_median_break(newid), silent = TRUE), add = TRUE)
  on.exit(pool::poolClose(pool), add = TRUE)
  meds <- db$diagram_medians(did)
  expect_true(newid %in% meds$id)
  expect_true(as.Date("2099-01-01") %in% as.Date(meds$laas_median))
  db$delete_median_break(newid)
  expect_false(newid %in% db$diagram_medians(did)$id)
})

test_that("org_enhed_variants returnerer org-navne + fra-data-varianter", {
  skip_if_no_db()
  pool <- db_connect(); on.exit(pool::poolClose(pool))
  db <- make_db(pool)
  vdf <- db$org_enhed_variants()
  expect_true(all(c("org_id", "teknisk", "kort", "langt", "fra_data") %in% names(vdf)))
  expect_gt(nrow(vdf), 100)
  # Mindst én org har en fra-data-oversættelse
  expect_gt(sum(!is.na(vdf$fra_data)), 0)
})
