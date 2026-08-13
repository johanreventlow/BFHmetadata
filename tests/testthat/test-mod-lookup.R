# testServer-tests for mod_lookup_table (excelR/jspreadsheet-grid) med fake-db.
# Redigeringer simuleres som excelR's onChange-payload: HELE tabellen (data +
# colHeaders); modulet diffar mod rows() og skriver ændrede celler enkeltvis.

cfg_test <- list(id = "t", table = "tblTest", pk = "Id", label = "Test",
  ref_check = list(child = "tblBruger", col = "test_id"),
  cols = list(list(col = "navn", type = "text", label = "Navn"),
              list(col = "niveau", type = "int", label = "Niveau")))

fake_lookup_db <- function(ref = 0L) {
  store <- data.frame(Id = 1:2, navn = c("A", "B"), niveau = c(1L, 2L),
                      stringsAsFactors = FALSE)
  calls <- list(updated = NULL, all_updates = list(), added = FALSE,
                deleted = NULL)
  list(
    list_rows = function() store,
    add_row = function() { calls$added <<- TRUE; 3L },
    update_cell = function(pk_val, col, value) {
      u <- list(pk = pk_val, col = col, value = value)
      calls$updated <<- u
      calls$all_updates <<- c(calls$all_updates, list(u))
      1L },
    delete_row = function(pk_val) { calls$deleted <<- pk_val; 1L },
    ref_count = function(pk_val) ref,
    .calls = function() calls
  )
}

# excelR onChange-payload: rækker som liste-af-lister i grid-rækkefølge
change_payload <- function(rows, headers = c("Id", "navn", "niveau")) {
  list(colHeaders = as.list(headers), data = rows, forSelectedVals = FALSE)
}

# excelR onSelection-payload: borderTop er 0-baseret række
select_payload <- function(row0) {
  list(forSelectedVals = TRUE,
       selectedDataBoundary = list(borderTop = row0, borderBottom = row0,
                                   borderLeft = 0, borderRight = 0))
}

test_that("opslagsmodul indlæser data ved start", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    expect_equal(nrow(rows()), 2)
  })
})

test_that("celle-redigering diffes mod rows() og kalder update_cell", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl = change_payload(list(
      list(1, "Nyt navn", 1), list(2, "B", 2))))
    u <- db$.calls()$updated
    expect_equal(u$col, "navn")
    expect_equal(u$value, "Nyt navn")
    expect_equal(u$pk, 1L)
    expect_equal(rows()$navn[1], "Nyt navn")   # lokal tilstand fulgte med
  })
})

test_that("flere celler ændret i samme payload gemmes enkeltvis", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl = change_payload(list(
      list(1, "A2", 1), list(2, "B", 9))))
    ups <- db$.calls()$all_updates
    expect_length(ups, 2L)
    expect_setequal(vapply(ups, function(u) u$col, ""), c("navn", "niveau"))
    expect_equal(rows()$niveau[2], 9L)
  })
})

test_that("tømt celle gemmes som NA", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl = change_payload(list(
      list(1, "", 1), list(2, "B", 2))))
    u <- db$.calls()$updated
    expect_equal(u$col, "navn")
    expect_true(is.na(u$value))
  })
})

test_that("widgetten renderes som excelR-grid med skjult pk og faste bredder", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    cols <- w$x$columns
    expect_equal(cols[[1]]$title, "Id")
    expect_equal(cols[[1]]$type, "hidden")     # pk aldrig synlig
    expect_equal(cols[[3]]$type, "numeric")
    expect_true(is.numeric(cols[[2]]$width))   # fraktil-bredde sat
    expect_false(isTRUE(w$x$autoWidth))        # ellers ignoreres bredderne
    expect_false(isTRUE(w$x$allowInsertRow))
    expect_false(isTRUE(w$x$columnSorting))
  })
})

test_that("int-celle med ikke-tal afvises uden update og snapper tilbage", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl = change_payload(list(
      list(1, "A", "abc"), list(2, "B", 2))))
    expect_match(status_msg(), "heltal")
    expect_null(db$.calls()$updated)
    expect_equal(rows()$niveau[1], 1L)          # uændret lokal tilstand
  })
})

test_that("int-celle med tal coerces til integer", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl = change_payload(list(
      list(1, "A", 1), list(2, "B", "7"))))
    u <- db$.calls()$updated
    expect_identical(u$value, 7L)
    expect_equal(u$col, "niveau")
    expect_equal(u$pk, 2L)
  })
})

test_that("ny række kalder add_row", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(add_row = 1)
    expect_true(db$.calls()$added)
  })
})

test_that("slet bruger seneste celle-selektion (0-baseret → række)", {
  db <- fake_lookup_db(ref = 0L)
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl = select_payload(0))
    expect_equal(sel_row(), 1L)
    session$setInputs(delete = 1)
    expect_equal(db$.calls()$deleted, 1L)
    expect_null(sel_row())                      # stale selektion ryddes
  })
})

test_that("slet blokeres når posten er i brug (ref_count > 0)", {
  db <- fake_lookup_db(ref = 5L)
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl = select_payload(0), delete = 1)
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

test_that("fk-dropdown-ændring gemmer integer parent-id", {
  db <- fake_fk_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_fk), {
    session$setInputs(tbl = list(
      colHeaders = list("Id", "navn", "enhed"),
      data = list(list(1, "A", "30"), list(2, "B", 20)),
      forSelectedVals = FALSE))
    u <- db$.calls()$updated
    expect_identical(u$value, 30L)
    expect_equal(u$col, "enhed")
    expect_equal(u$pk, 1L)
  })
})

test_that("fk-kolonne renderes som dropdown med {id, name}-source", {
  db <- fake_fk_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_fk), {
    w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    fk_col <- w$x$columns[[3]]
    expect_equal(fk_col$type, "dropdown")
    expect_equal(fk_col$source[[3]]$id, 30L)
    expect_equal(fk_col$source[[3]]$name, "E30")
  })
})
