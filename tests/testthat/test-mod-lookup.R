cfg_test <- list(id = "t", table = "tblTest", pk = "Id", label = "Test",
  ref_check = list(child = "tblBruger", col = "test_id"),
  cols = list(list(col = "navn", type = "text", label = "Navn"),
              list(col = "niveau", type = "int", label = "Niveau")))

fake_lookup_db <- function(ref = 0L) {
  store <- data.frame(Id = 1:2, navn = c("A", "B"), niveau = c(1L, 2L),
                      stringsAsFactors = FALSE)
  calls <- list(updated = NULL, added = FALSE, deleted = NULL)
  list(
    list_rows = function() store,
    add_row = function() { calls$added <<- TRUE; 3L },
    update_cell = function(pk_val, col, value) {
      calls$updated <<- list(pk = pk_val, col = col, value = value); 1L },
    delete_row = function(pk_val) { calls$deleted <<- pk_val; 1L },
    ref_count = function(pk_val) ref,
    .calls = function() calls
  )
}

test_that("opslagsmodul indlæser data ved start", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    expect_equal(nrow(rows()), 2)
  })
})

test_that("inline-edit på tekstcelle kalder update_cell", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    # col 1 (0-baseret) = navn; row 1 → pk 1
    session$setInputs(tbl_cell_edit = list(row = 1, col = 1, value = "Nyt navn"))
    u <- db$.calls()$updated
    expect_equal(u$col, "navn"); expect_equal(u$value, "Nyt navn"); expect_equal(u$pk, 1L)
  })
})

test_that("int-celle med ikke-tal afvises uden update", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl_cell_edit = list(row = 1, col = 2, value = "abc"))
    expect_match(status_msg(), "heltal")
    expect_null(db$.calls()$updated)
  })
})

test_that("int-celle med tal coerces til integer", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl_cell_edit = list(row = 2, col = 2, value = "7"))
    u <- db$.calls()$updated
    expect_identical(u$value, 7L); expect_equal(u$col, "niveau"); expect_equal(u$pk, 2L)
  })
})

test_that("ny række kalder add_row", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(add_row = 1)
    expect_true(db$.calls()$added)
  })
})

test_that("slet valgte række kalder delete_row", {
  db <- fake_lookup_db(ref = 0L)
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl_rows_selected = 1, delete = 1)
    expect_equal(db$.calls()$deleted, 1L)
  })
})

test_that("slet blokeres når posten er i brug (ref_count > 0)", {
  db <- fake_lookup_db(ref = 5L)
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl_rows_selected = 1, delete = 1)
    expect_match(status_msg(), "i brug")
    expect_null(db$.calls()$deleted)
  })
})

test_that("slet uden valgt række beder om valg", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(delete = 1)
    expect_match(status_msg(), "Vælg en række")
    expect_null(db$.calls()$deleted)
  })
})

# --- FK-kolonne (dropdown-redigering) ----------------------------------------
cfg_fk <- list(id = "p", table = "tblP", pk = "Id", label = "P",
  cols = list(
    list(col = "navn", type = "text", label = "Navn"),
    list(col = "enhed", type = "fk", label = "Enhed",
         parent = "tblOrg", parent_pk = "Id", label_expr = '"navn"')))

fake_fk_db <- function() {
  store <- data.frame(Id = 1:2, navn = c("A", "B"), enhed = c(10L, 20L),
                      stringsAsFactors = FALSE)
  calls <- list(updated = NULL)
  list(
    list_rows = function() store,
    add_row = function() 3L,
    update_cell = function(pk_val, col, value) {
      calls$updated <<- list(pk = pk_val, col = col, value = value); 1L },
    delete_row = function(pk_val) 1L,
    ref_count = function(pk_val) 0L,
    fk_options = function(col) data.frame(id = c(10L, 20L, 30L),
                                          label = c("E10", "E20", "E30")),
    .calls = function() calls
  )
}

test_that("fk_edit opdaterer relation med integer parent-id", {
  db <- fake_fk_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_fk), {
    session$setInputs(fk_edit = list(id = 1, col = "enhed", value = "30"))
    u <- db$.calls()$updated
    expect_identical(u$value, 30L)
    expect_equal(u$col, "enhed"); expect_equal(u$pk, 1)
  })
})

test_that(".fk_select_html bygger select med valgt option + onchange", {
  opts <- data.frame(id = c(10L, 20L), label = c("E10", "E20"))
  html <- .fk_select_html(function(x) paste0("ns-", x), 5, "enhed", 20L, opts)
  expect_match(html, "<select")
  expect_match(html, 'value="20" selected')
  expect_match(html, "ns-fk_edit")
  expect_match(html, "Shiny.setInputValue")
})
