# testServer-tests for legacy-fuldtabelpayloads og opt-in adapterens separate
# celle-, selektions- og statuskanaler med en mutérbar fake-db.

cfg_test <- list(id = "t", table = "tblTest", pk = "Id", label = "Test",
  ref_check = list(child = "tblBruger", col = "test_id"),
  cols = list(list(col = "navn", type = "text", label = "Navn"),
              list(col = "niveau", type = "int", label = "Niveau")))

cfg_adapter <- c(cfg_test, list(excel_adapter = TRUE))

fake_lookup_db <- function(ref = 0L, fail = c("none", "before", "after", "reload"),
                           get_row_result = c(
                             "match", "duplicate", "none", "missing_field",
                             "wrong_field", "wrong_pk", "wrong_type"
                           )) {
  fail_mode <- match.arg(fail)
  get_row_result <- match.arg(get_row_result)
  store <- data.frame(Id = 1:2, navn = c("A", "B"), niveau = c(1L, 2L),
                      stringsAsFactors = FALSE)
  calls <- list(updated = NULL, all_updates = list(), added = FALSE,
                deleted = NULL, get_row = list(), list_rows = 0L)
  list(
    list_rows = function() {
      calls$list_rows <<- calls$list_rows + 1L
      store
    },
    get_row = function(pk_val) {
      calls$get_row <<- c(calls$get_row, list(pk_val))
      if (identical(fail_mode, "reload")) stop("test reload failure")
      row <- store[as.character(store$Id) == as.character(pk_val), , drop = FALSE]
      if (identical(get_row_result, "duplicate") && nrow(row) == 1L) {
        row <- rbind(row, row)
      } else if (identical(get_row_result, "none")) {
        row <- row[FALSE, , drop = FALSE]
      } else if (identical(get_row_result, "missing_field")) {
        row$navn <- NULL
      } else if (identical(get_row_result, "wrong_field")) {
        names(row)[names(row) == "navn"] <- "forkert_felt"
      } else if (identical(get_row_result, "wrong_pk")) {
        row$Id <- row$Id + 100L
      } else if (identical(get_row_result, "wrong_type")) {
        row$navn <- rep(42L, nrow(row))
      }
      row
    },
    add_row = function() {
      calls$added <<- TRUE
      store <<- rbind(store, data.frame(Id = 3L, navn = NA_character_,
                                        niveau = NA_integer_))
      3L
    },
    update_cell = function(pk_val, col, value) {
      u <- list(pk = pk_val, col = col, value = value)
      calls$updated <<- u
      calls$all_updates <<- c(calls$all_updates, list(u))
      if (fail_mode %in% c("before", "reload")) stop("test write failure")
      j <- match(as.character(pk_val), as.character(store$Id))
      store[j, col] <<- value
      if (identical(fail_mode, "after")) stop("test post-commit failure")
      1L
    },
    delete_row = function(pk_val) {
      calls$deleted <<- pk_val
      store <<- store[as.character(store$Id) != as.character(pk_val), , drop = FALSE]
      1L
    },
    ref_count = function(pk_val) ref,
    .calls = function() calls,
    .store = function() store,
    .set_fail = function(value) {
      fail_mode <<- match.arg(value, c("none", "before", "after", "reload"))
      invisible(NULL)
    }
  )
}

test_that("adaptercfg indlaeser kun den opt-in grid-wrapper og dependency", {
  dependency <- .excel_adapter_dependency()
  expect_s3_class(dependency, "html_dependency")
  expect_equal(dependency$script, "bfh-excel-adapter.js")
  expect_match(as.character(mod_lookup_table_ui("x", cfg_adapter)),
               "bfh-excel-grid")
  expect_false(grepl("bfh-excel-grid",
                     as.character(mod_lookup_table_ui("x", cfg_test))))
})

test_that("lookup adapter-map kommer fra serverkolonner og låser ukendte felter", {
  map <- lookup_excel_adapter_map(cfg_adapter, c("Id", "navn", "server_only"))
  expect_identical(map, data.frame(
    column_index = 0:2,
    field = c("Id", "navn", "server_only"),
    value_type = c("text", "text", "text"),
    editable = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  ))
  expect_silent(validate_excel_adapter_map(
    map, c("Id", "navn", "server_only"), "Id"
  ))
})

# excelR onChange-payload: rækker som liste-af-lister i grid-rækkefølge
change_payload <- function(rows, headers = c("Id", "navn", "niveau")) {
  list(colHeaders = as.list(headers), data = rows, forSelectedVals = FALSE)
}

# excelR onSelection-payload: borderTop er 0-baseret række; fullData bærer
# grid'ets aktuelle rækkefølge (pk i første kolonne) — pks kan omordnes for
# at simulere klient-side sortering
select_payload <- function(row0, pks = c("1", "2")) {
  list(forSelectedVals = TRUE,
       selectedDataBoundary = list(borderTop = row0, borderBottom = row0,
                                   borderLeft = 0, borderRight = 0),
       fullData = list(data = lapply(pks, function(p) list(p))))
}

adapter_cell <- function(event_id = "1:1", generation = 1L, row_pk = "1",
                         column_index = 1L, raw_value = "Nyt") {
  list(event_id = event_id, grid_generation = generation, row_pk = row_pk,
       column_index = column_index, raw_value = raw_value)
}

adapter_reply_recorder <- function() {
  replies <- list()
  list(
    reply = function(session, output_id, result) {
      replies[[length(replies) + 1L]] <<- list(output_id = output_id, result = result)
    },
    results = function() replies
  )
}

test_that("opslagsmodul indlæser data ved start", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    expect_equal(nrow(rows()), 2)
  })
})

test_that("adaptercelle gemmes én gang uden legacy-diff eller fuld render", {
  db <- fake_lookup_db()
  replies <- list()
  reply <- function(session, output_id, result) {
    replies[[length(replies) + 1L]] <<- list(output_id = output_id, result = result)
  }
  testServer(mod_lookup_table_server,
             args = list(db = db, cfg = cfg_adapter, adapter_reply = reply), {
    session$flushReact()
    before_generation <- grid_generation()
    before_revision <- render_revision()
    payload <- adapter_cell()
    payload$field <- "niveau"
    session$setInputs(tbl_cell = payload)

    expect_length(db$.calls()$all_updates, 1L)
    expect_identical(db$.calls()$updated,
      list(pk = 1L, col = "navn", value = "Nyt"))
    expect_identical(rows()$navn[1], "Nyt")
    expect_identical(db$.store()$navn[1], "Nyt")
    expect_length(replies, 1L)
    expect_identical(replies[[1]]$output_id, "tbl")
    expect_identical(replies[[1]]$result$status, "saved")
    expect_identical(replies[[1]]$result$event_id, "1:1")
    expect_identical(replies[[1]]$result$grid_generation, 1L)
    expect_identical(replies[[1]]$result$value, "Nyt")
    expect_identical(grid_generation(), before_generation)
    expect_identical(render_revision(), before_revision)

    session$setInputs(tbl = change_payload(list(
      list(1, "Browser-fuldtabel", 1), list(2, "B", 2))))
    expect_length(db$.calls()$all_updates, 1L)
    expect_identical(rows()$navn[1], "Nyt")
  })
})

test_that("before-write exception genlæser kun rækken og afviser med DB-værdi", {
  db <- fake_lookup_db(fail = "before")
  replies <- adapter_reply_recorder()
  testServer(mod_lookup_table_server,
             args = list(db = db, cfg = cfg_adapter,
                         adapter_reply = replies$reply), {
    session$flushReact()
    list_reads <- db$.calls()$list_rows
    generation <- grid_generation()
    revision <- render_revision()
    session$setInputs(tbl_cell = adapter_cell(raw_value = "Ikke gemt"))

    expect_length(db$.calls()$all_updates, 1L)
    expect_identical(db$.calls()$get_row, list(1L))
    expect_identical(db$.calls()$list_rows, list_reads)
    expect_identical(rows()$navn, c("A", "B"))
    result <- replies$results()[[1]]$result
    expect_identical(result$status, "rejected")
    expect_identical(result$value, "A")
    expect_false(result$lock_grid)
    expect_false(grepl("test|failure|password|SQL", result$message %||% "",
                       ignore.case = TRUE))
    expect_identical(grid_generation(), generation)
    expect_identical(render_revision(), revision)
  })
})

test_that("post-commit exception accepteres efter én frisk målrettet genlæsning", {
  db <- fake_lookup_db(fail = "after")
  replies <- adapter_reply_recorder()
  testServer(mod_lookup_table_server,
             args = list(db = db, cfg = cfg_adapter,
                         adapter_reply = replies$reply), {
    session$flushReact()
    list_reads <- db$.calls()$list_rows
    session$setInputs(tbl_cell = adapter_cell(raw_value = "Gemt trods fejl"))

    expect_length(db$.calls()$all_updates, 1L)
    expect_identical(db$.calls()$get_row, list(1L))
    expect_identical(db$.calls()$list_rows, list_reads)
    expect_identical(rows()$navn, c("Gemt trods fejl", "B"))
    expect_identical(db$.store()$navn, c("Gemt trods fejl", "B"))
    result <- replies$results()[[1]]$result
    expect_identical(result$status, "saved")
    expect_identical(result$value, "Gemt trods fejl")
    expect_false(result$lock_grid)
  })
})

test_that("write og målrettet genlæsning der fejler låser fail-closed", {
  db <- fake_lookup_db(fail = "reload")
  replies <- adapter_reply_recorder()
  testServer(mod_lookup_table_server,
             args = list(db = db, cfg = cfg_adapter,
                         adapter_reply = replies$reply), {
    session$flushReact()
    list_reads <- db$.calls()$list_rows
    session$setInputs(tbl_cell = adapter_cell(raw_value = "Uafklaret"))

    expect_length(db$.calls()$all_updates, 1L)
    expect_identical(db$.calls()$get_row, list(1L))
    expect_identical(db$.calls()$list_rows, list_reads)
    expect_identical(rows()$navn, c("A", "B"))
    result <- replies$results()[[1]]$result
    expect_identical(result$status, "rejected")
    expect_true(result$lock_grid)
    expect_identical(result$message,
      "Databasestatus kunne ikke bekræftes. Genindlæs siden.")
  })
})

test_that("tvetydig målrettet genlæsning låser uden at patche naboer", {
  db <- fake_lookup_db(fail = "before", get_row_result = "duplicate")
  replies <- adapter_reply_recorder()
  testServer(mod_lookup_table_server,
             args = list(db = db, cfg = cfg_adapter,
                         adapter_reply = replies$reply), {
    session$flushReact()
    session$setInputs(tbl_cell = adapter_cell(raw_value = "Uafklaret"))

    expect_identical(db$.calls()$get_row, list(1L))
    expect_identical(rows()$navn, c("A", "B"))
    expect_identical(rows()$niveau, c(1L, 2L))
    result <- replies$results()[[1]]$result
    expect_identical(result$status, "rejected")
    expect_true(result$lock_grid)
    expect_identical(result$message,
      "Databasestatus kunne ikke bekræftes. Genindlæs siden.")
  })
})

test_that("serverlås afviser senere celleevents uden ny write eller genlæsning", {
  db <- fake_lookup_db(fail = "reload")
  replies <- adapter_reply_recorder()
  testServer(mod_lookup_table_server,
             args = list(db = db, cfg = cfg_adapter,
                         adapter_reply = replies$reply), {
    session$flushReact()
    local_before <- rows()
    store_before <- db$.store()
    session$setInputs(tbl_cell = adapter_cell(
      event_id = "first", row_pk = "1", raw_value = "Uafklaret"
    ))
    expect_true(replies$results()[[1]]$result$lock_grid)
    writes_after_lock <- length(db$.calls()$all_updates)
    reads_after_lock <- length(db$.calls()$get_row)

    # Uden en server-latch ville dette mutere række 2 og reconciles som saved.
    db$.set_fail("after")
    session$setInputs(tbl_cell = adapter_cell(
      event_id = "queued", row_pk = "2", raw_value = "Y"
    ))

    expect_length(db$.calls()$all_updates, writes_after_lock)
    expect_length(db$.calls()$get_row, reads_after_lock)
    expect_identical(rows(), local_before)
    expect_identical(db$.store(), store_before)
    result <- replies$results()[[2]]$result
    expect_identical(result$status, "rejected")
    expect_true(result$lock_grid)
    expect_identical(result$message,
      "Databasestatus kunne ikke bekræftes. Genindlæs siden.")
  })
})

test_that("alle tvetydige get_row-former låser sessionen fail-closed", {
  shapes <- c(
    "duplicate", "none", "missing_field", "wrong_field", "wrong_pk",
    "wrong_type"
  )
  for (shape in shapes) {
    db <- fake_lookup_db(fail = "before", get_row_result = shape)
    replies <- adapter_reply_recorder()
    testServer(mod_lookup_table_server,
               args = list(db = db, cfg = cfg_adapter,
                           adapter_reply = replies$reply), {
      session$flushReact()
      local_before <- rows()
      store_before <- db$.store()
      session$setInputs(tbl_cell = adapter_cell(
        event_id = paste0("ambiguous-", shape), row_pk = "1",
        raw_value = "Uafklaret"
      ))

      result <- replies$results()[[1]]$result
      expect_identical(result$status, "rejected", info = shape)
      expect_true(result$lock_grid, info = shape)
      expect_identical(result$message,
        "Databasestatus kunne ikke bekræftes. Genindlæs siden.", info = shape)
      expect_identical(rows(), local_before, info = shape)
      expect_identical(db$.store(), store_before, info = shape)
      expect_identical(length(db$.calls()$all_updates), 1L, info = shape)
      expect_identical(length(db$.calls()$get_row), 1L, info = shape)

      writes_after_lock <- length(db$.calls()$all_updates)
      reads_after_lock <- length(db$.calls()$get_row)
      db$.set_fail("after")
      session$setInputs(tbl_cell = adapter_cell(
        event_id = paste0("queued-", shape), row_pk = "2", raw_value = "Y"
      ))

      expect_identical(length(db$.calls()$all_updates), writes_after_lock,
                       info = shape)
      expect_identical(length(db$.calls()$get_row), reads_after_lock,
                       info = shape)
      expect_identical(rows(), local_before, info = shape)
      expect_identical(db$.store(), store_before, info = shape)
      queued <- replies$results()[[2]]$result
      expect_identical(queued$status, "rejected", info = shape)
      expect_true(queued$lock_grid, info = shape)
      expect_identical(queued$message,
        "Databasestatus kunne ikke bekræftes. Genindlæs siden.", info = shape)
    })
  }
})

test_that("read-only ukendt og stale celleevent afvises uden DB-write", {
  db <- fake_lookup_db()
  replies <- adapter_reply_recorder()
  testServer(mod_lookup_table_server,
             args = list(db = db, cfg = cfg_adapter,
                         adapter_reply = replies$reply), {
    session$flushReact()
    session$setInputs(tbl_cell = adapter_cell(event_id = "readonly", column_index = 0L))
    session$setInputs(tbl_cell = adapter_cell(event_id = "unknown", column_index = 99L))
    session$setInputs(tbl_cell = adapter_cell(event_id = "stale", generation = 0L))

    expect_length(db$.calls()$all_updates, 0L)
    expect_length(db$.calls()$get_row, 0L)
    results <- lapply(replies$results(), `[[`, "result")
    expect_identical(vapply(results, `[[`, "", "status"),
                     rep("rejected", 3L))
    expect_identical(results[[1]]$value, 1L)
    expect_false(results[[1]]$lock_grid)
    expect_true(results[[2]]$lock_grid)
    expect_identical(results[[3]]$value, "A")
    expect_false(results[[3]]$lock_grid)
  })
})

test_that("adapterselektion bruger første PK i klientens aktuelle rækkefølge", {
  db <- fake_lookup_db(ref = 0L)
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_adapter), {
    session$flushReact()
    session$setInputs(tbl_selection = list(
      grid_generation = 1L,
      boundaries = list(top = 0L, bottom = 1L, left = 0L, right = 2L),
      row_pks = c("2", "1")
    ))
    expect_identical(sel_pk(), "2")
    session$setInputs(delete = 1L)
    expect_identical(db$.calls()$deleted, 2L)
    expect_null(sel_pk())
  })
})

test_that("adapter klientstatus accepterer kun serverens sikre allowlist", {
  db <- fake_lookup_db()
  safe <- "Indsætning af flere celler understøttes ikke endnu."
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_adapter), {
    session$flushReact()
    session$setInputs(tbl_client_status = list(message = safe))
    expect_identical(status_msg(), safe)
    session$setInputs(tbl_client_status = list(
      message = "SQL password=hemmelig; send credentials"
    ))
    expect_identical(status_msg(), safe)
  })
})

test_that("adapter add og delete bumper generation og revision præcis én gang", {
  db <- fake_lookup_db(ref = 0L)
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_adapter), {
    session$flushReact()
    generation <- grid_generation()
    revision <- render_revision()
    session$setInputs(add_row = 1L)
    expect_identical(grid_generation(), generation + 1L)
    expect_identical(render_revision(), revision + 1L)

    session$setInputs(tbl_selection = list(
      grid_generation = grid_generation(), row_pks = "3"
    ))
    generation <- grid_generation()
    revision <- render_revision()
    session$setInputs(delete = 1L)
    expect_identical(db$.calls()$deleted, 3L)
    expect_identical(grid_generation(), generation + 1L)
    expect_identical(render_revision(), revision + 1L)
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
    expect_true(isTRUE(w$x$columnSorting))     # klik-sortering på overskrifter
  })
})

test_that("adaptercfg bruger kun dokumenterede widgetparametre", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_adapter), {
    w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    expect_true(isTRUE(w$x$tableOverflow))
    expect_false(isTRUE(w$x$pagination))
    expect_false(isTRUE(w$x$columnDrag))
    expect_true(isTRUE(w$x$selectionCopy))
    expect_equal(w$x$tableHeight, "calc(100vh - 250px)")
    expect_false("bfhGeneration" %in% names(w$x))
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

test_that("slet bruger seneste celle-selektion (pk fra fullData)", {
  db <- fake_lookup_db(ref = 0L)
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl = select_payload(0))
    expect_equal(sel_pk(), "1")
    session$setInputs(delete = 1)
    expect_equal(db$.calls()$deleted, 1L)
    expect_null(sel_pk())                       # stale selektion ryddes
  })
})

test_that("selektion overlever klient-side sortering (pk følger rækken)", {
  db <- fake_lookup_db(ref = 0L)
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    # Grid sorteret omvendt: position 0 er nu rækken med pk 2
    session$setInputs(tbl = select_payload(0, pks = c("2", "1")))
    session$setInputs(delete = 1)
    expect_equal(db$.calls()$deleted, 2L)       # IKKE server-ordenens række 1
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

test_that("fk-kolonne renderes som soegbar dropdown med {id, name}-source", {
  db <- fake_fk_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_fk), {
    w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    fk_col <- w$x$columns[[3]]
    # "autocomplete": jexcel-dropdown med skriv-for-at-filtrere
    expect_equal(fk_col$type, "autocomplete")
    expect_equal(fk_col$source[[3]]$id, 30L)
    expect_equal(fk_col$source[[3]]$name, "E30")
  })
})

test_that("lookup: aendring der kun ankommer via selektionens fullData gemmes", {
  db <- fake_lookup_db()
  testServer(mod_lookup_table_server, args = list(db = db, cfg = cfg_test), {
    session$setInputs(tbl = list(
      forSelectedVals = TRUE,
      selectedDataBoundary = list(borderTop = 0, borderBottom = 0,
                                  borderLeft = 0, borderRight = 0),
      fullData = list(colHeaders = list("Id", "navn", "niveau"),
                      data = list(list(1, "A aendret", 1), list(2, "B", 2)))))
    u <- db$.calls()$updated
    expect_false(is.null(u))
    expect_equal(u$col, "navn")
    expect_equal(u$value, "A aendret")
    expect_equal(u$pk, 1L)
  })
})
