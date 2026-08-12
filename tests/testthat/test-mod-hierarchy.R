test_that(".node_label falder tilbage til teknisk navn og id ved manglende display_col", {
  cfg <- .hierarchy_cfg()
  expect_identical(.node_label(cfg, "Langt navn", "teknisk", 5L), "Langt navn")
  expect_identical(.node_label(cfg, NA_character_, "teknisk", 5L), "teknisk")
  expect_identical(.node_label(cfg, "", "teknisk", 5L), "teknisk")
  expect_identical(.node_label(cfg, NA_character_, NA_character_, 5L),
                   "(uden navn #5)")
  expect_identical(.node_label(cfg, NA_character_, "", 5L), "(uden navn #5)")
})

test_that("tree() giver korrekt depth-first-orden", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
    expect_identical(tree()$id, c(1L, 2L, 3L, 4L))
    expect_identical(tree()$depth, c(0L, 1L, 2L, 0L))
  })
})

test_that("slet med boern blokeres (delete_node IKKE kaldt)", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(selected_node_id = 1, delete_selected = 1)
      session$setInputs(confirm_delete = 1)
    expect_null(db$.calls()$deleted)
    expect_match(warn_msg(), "børn")
  })
})

test_that("slet valgt kraever valg og bekraeftelse", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(delete_selected = 1)
      expect_match(warn_msg(), "V\u00e6lg", fixed = TRUE)
      session$setInputs(selected_node_id = 4, delete_selected = 2)
      expect_identical(delete_id(), 4L)
      expect_null(db$.calls()$deleted)
      session$setInputs(confirm_delete = 1)
      expect_identical(db$.calls()$deleted, 4L)
      expect_false(4L %in% nodes()$id)
    })
})

test_that("referencefejl ved sletning genindlaeser autoritative rækker", {
  db <- fake_hierarchy_db()
  db$.set_delete_error(TRUE)
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(selected_node_id = 4, delete_selected = 1)
      session$setInputs(confirm_delete = 1)
      expect_identical(db$.calls()$deleted, 4L)
      expect_identical(nodes(), db$.nodes())
      expect_true(4L %in% nodes()$id)
      expect_match(warn_msg(), "i brug", fixed = TRUE)
    })
})

test_that("sletning bruger stabilt node-id efter at traeet er genordnet", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(selected_node_id = 3)
      session$setInputs(inline_edit = list(
        id = 3, field = "parent_Id", oldValue = "2", value = "4", nonce = 1))
      expect_identical(tree()$id, c(1L, 2L, 4L, 3L))
      widget <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
      expect_identical(widget$x$selection$selected, 4L)

      session$setInputs(delete_selected = 1)
      expect_identical(delete_id(), 3L)
      session$setInputs(confirm_delete = 1)
      expect_identical(db$.calls()$deleted, 3L)
      expect_false(3L %in% nodes()$id)
      expect_true(4L %in% nodes()$id)
    })
})

test_that("raekkevalg alene genopbygger ikke DT-widgeten", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      before <- output$tbl
      session$setInputs(selected_node_id = 3)
      expect_identical(output$tbl, before)
      expect_identical(selected_id(), 3L)
    })
})

test_that("opret ny med tom foraelder kalder create_node med NA-parent", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
    session$setInputs(new_node = 1)
    session$setInputs(
      h_organisatorisk_navn_teknisk = "ny_node",
      h_organisatorisk_navn_langt = "Ny node",
      h_organisatorisk_navn_kort = "Ny",
      h_parent = "", h_niveau = "10", h_save = 1)
    created <- db$.calls()$created
    expect_false(is.null(created))
    expect_true(is.na(created$parent_Id))
    expect_identical(created$organisatorisk_navn_teknisk, "ny_node")
  })
})

test_that("hierarki-tabel viser fem permanente editor-kolonner", {
  db <- fake_hierarchy_db()
  d <- hierarchy_order(db$list_nodes(), "id", "parent_id_raw",
                       .hierarchy_cfg()$display_col)
  out <- .hierarchy_editor_data(d, .hierarchy_cfg(), identity,
                                db$niveau_options())
  expect_named(out, c("Teknisk navn", "Langt navn", "Kort navn",
                      "Forælder", "Niveau"))
  expect_true(all(grepl("hierarchy-editor", out[["Langt navn"]], fixed = TRUE)))
  expect_match(out[["Langt navn"]][3], "padding-left:3rem", fixed = TRUE)
  expect_false(grepl('value="3"', out[["Forælder"]][1], fixed = TRUE))
})

test_that("hierarki-tabel viser manglende tekst som et tomt editorfelt", {
  db <- fake_hierarchy_db()
  d <- hierarchy_order(db$list_nodes(), "id", "parent_id_raw",
                       .hierarchy_cfg()$display_col)
  d$organisatorisk_navn_kort[2] <- NA_character_
  out <- .hierarchy_editor_data(d, .hierarchy_cfg(), identity,
                                db$niveau_options())
  expect_match(out[["Kort navn"]][2], 'value=""', fixed = TRUE)
  expect_false(grepl('value="NA"', out[["Kort navn"]][2], fixed = TRUE))
})

test_that("hierarki-tabel bevarer manglende niveau som tomt valgt vaerdi", {
  db <- fake_hierarchy_db()
  d <- hierarchy_order(db$list_nodes(), "id", "parent_id_raw",
                       .hierarchy_cfg()$display_col)
  d$niveau_id[2] <- NA_integer_
  out <- .hierarchy_editor_data(d, .hierarchy_cfg(), identity,
                                db$niveau_options())
  expect_true(grepl('<option value="" selected>(vælg)</option>',
                    out[["Niveau"]][2], fixed = TRUE))
})

test_that("inline tekstaendring gemmer straks hele noden og genindlaeser", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(inline_edit = list(
        id = 2, field = "organisatorisk_navn_langt",
        oldValue = "Barn B", value = "Barn B ny", nonce = 1))
      upd <- tail(db$.calls()$updates, 1)[[1]]
      expect_identical(upd$id, 2L)
      expect_identical(upd$values$organisatorisk_navn_langt, "Barn B ny")
      expect_identical(nodes()$organisatorisk_navn_langt[nodes()$id == 2L],
                       "Barn B ny")
      expect_match(status_msg(), "Gemt")
    })
})

test_that("inline dropdowns gemmer integer-id og flytning genordner traeet", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(inline_edit = list(
        id = 3, field = "parent_Id", oldValue = "2", value = "4", nonce = 1))
      expect_identical(db$.nodes()$parent_id_raw[db$.nodes()$id == 3L], 4L)
      expect_identical(tree()$id, c(1L, 2L, 4L, 3L))
      session$setInputs(inline_edit = list(
        id = 3, field = "organisatorisk_niveau",
        oldValue = "10", value = "20", nonce = 2))
      expect_identical(db$.nodes()$niveau_id[db$.nodes()$id == 3L], 20L)
    })
})

test_that("afviste inline-events skriver ikke og genindlaeser autoritativ vaerdi", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(inline_edit = list(
        id = 2, field = "id", oldValue = "2", value = "99", nonce = 1))
      expect_length(db$.calls()$updates, 0)
      expect_match(warn_msg(), "ukendt felt", ignore.case = TRUE)
      session$setInputs(inline_edit = list(
        id = 1, field = "parent_Id", oldValue = "", value = "3", nonce = 2))
      expect_length(db$.calls()$updates, 0)
      expect_match(warn_msg(), "cyklus", ignore.case = TRUE)
      expect_identical(nodes(), db$.nodes())
      expect_identical(table_revision(), 2L)
    })
})

test_that("uaendret inline-vaerdi genindlaeser uden databaseopdatering", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(inline_edit = list(
        id = 2, field = "organisatorisk_navn_kort",
        oldValue = "B", value = "B", nonce = 1))
      expect_length(db$.calls()$updates, 0)
      expect_identical(nodes(), db$.nodes())
      expect_identical(table_revision(), 1L)
    })
})

test_that("tomt langt navn giver obligatorisk advarsel uden databaseopdatering", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(inline_edit = list(
        id = 2, field = "organisatorisk_navn_langt",
        oldValue = "Barn B", value = "", nonce = 1))
      expect_length(db$.calls()$updates, 0)
      expect_match(warn_msg(), "obligatorisk", ignore.case = TRUE)
      expect_identical(nodes(), db$.nodes())
    })
})

test_that("niveau der ikke er dybere end foraelder gemmes med advarsel", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(inline_edit = list(
        id = 2, field = "organisatorisk_niveau",
        oldValue = "20", value = "10", nonce = 1))
      expect_length(db$.calls()$updates, 1)
      expect_identical(db$.nodes()$niveau_id[db$.nodes()$id == 2L], 10L)
      expect_match(warn_msg(), "ikke dybere", ignore.case = TRUE)
    })
})

test_that("databasefejl genindlaeser autoritativ vaerdi og melder rollback", {
  db <- fake_hierarchy_db()
  db$.set_update_error(TRUE)
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(inline_edit = list(
        id = 2, field = "organisatorisk_navn_langt",
        oldValue = "Barn B", value = "Fejler", nonce = 1))
      expect_length(db$.calls()$updates, 1)
      expect_identical(nodes(), db$.nodes())
      expect_match(warn_msg(), "gendannet", ignore.case = TRUE)
      expect_identical(table_revision(), 1L)
    })
})

test_that("identiske status- og advarselsbeskeder udloeser nye events", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(inline_edit = list(
        id = 2, field = "organisatorisk_navn_kort",
        oldValue = "B", value = "B1", nonce = 1))
      session$setInputs(inline_edit = list(
        id = 2, field = "organisatorisk_navn_kort",
        oldValue = "B1", value = "B2", nonce = 2))
      expect_identical(status_msg(), "Gemt")
      expect_identical(status_event()$nonce, 2L)

      session$setInputs(inline_edit = list(
        id = 2, field = "id", oldValue = "2", value = "99", nonce = 3))
      session$setInputs(inline_edit = list(
        id = 2, field = "id", oldValue = "2", value = "99", nonce = 4))
      expect_identical(warn_msg(), "Ukendt felt")
      expect_identical(warn_event()$nonce, 2L)
    })
})

test_that("hierarki-tabel kan rendere et tomt traee", {
  db <- fake_hierarchy_db()
  d <- hierarchy_order(db$list_nodes()[FALSE, ], "id", "parent_id_raw",
                       .hierarchy_cfg()$display_col)
  out <- .hierarchy_editor_data(d, .hierarchy_cfg(), identity,
                                db$niveau_options())
  expect_equal(nrow(out), 0)
  expect_named(out, c("Teknisk navn", "Langt navn", "Kort navn",
                      "Forælder", "Niveau"))
})
