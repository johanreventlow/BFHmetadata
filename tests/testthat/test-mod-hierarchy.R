# testServer-tests for mod_hierarchy (excelR/jspreadsheet-grid) med fake-db.
# Redigeringer simuleres som excelR's onChange-payload bygget af
# .hierarchy_grid_edit (helper-hierarchy.R); modulet diffar mod grid-data,
# validerer via .prepare_hierarchy_inline_update og skriver via update_node.

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

test_that("grid'et renderes med Struktur-kolonne og dropdown-kolonner", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
      cols <- w$x$columns
      titles <- vapply(cols, function(c) c$title, "")
      expect_identical(titles[1:2], c("id", "Struktur"))
      expect_true(all(vapply(cols[1:2], function(c) isTRUE(c$readOnly),
                             logical(1))))
      types <- vapply(cols, function(c) c$type, "")
      expect_identical(types[titles == "id"], "hidden")  # pk aldrig synlig
      expect_identical(types[titles %in% c("Forælder", "Niveau")],
                       c("dropdown", "dropdown"))
      expect_false(isTRUE(w$x$autoWidth))
      expect_false(isTRUE(w$x$allowInsertRow))
      expect_false(isTRUE(w$x$columnSorting))
    })
})

test_that("slet med boern blokeres (delete_node IKKE kaldt)", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(tbl = .hierarchy_grid_select(0))   # node 1 (rod med børn)
      session$setInputs(delete_selected = 1)
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
      expect_match(warn_msg(), "Vælg", fixed = TRUE)
      session$setInputs(tbl = .hierarchy_grid_select(3))   # node 4 (sidste række)
      session$setInputs(delete_selected = 2)
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
      session$setInputs(tbl = .hierarchy_grid_select(3))   # node 4
      session$setInputs(delete_selected = 1)
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
      session$setInputs(tbl = .hierarchy_grid_select(2))   # node 3 (position 3)
      expect_identical(selected_id(), 3L)
      # Flyt node 3 under node 4 → traeet genordnes til c(1, 2, 4, 3)
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 3L, "Forælder", "4"))
      expect_identical(tree()$id, c(1L, 2L, 4L, 3L))
      # Selektionen er ID-baseret — reorder må ikke flytte den til node 4
      session$setInputs(delete_selected = 1)
      expect_identical(delete_id(), 3L)
      session$setInputs(confirm_delete = 1)
      expect_identical(db$.calls()$deleted, 3L)
      expect_false(3L %in% nodes()$id)
      expect_true(4L %in% nodes()$id)
    })
})

test_that("raekkevalg alene genopbygger ikke grid-widgeten", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      before <- output$tbl
      session$setInputs(tbl = .hierarchy_grid_select(2))
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

test_that("inline tekstaendring gemmer straks hele noden og genindlaeser", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 2L, "Langt navn", "Barn B ny"))
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
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 3L, "Forælder", "4"))
      expect_identical(db$.nodes()$parent_id_raw[db$.nodes()$id == 3L], 4L)
      expect_identical(tree()$id, c(1L, 2L, 4L, 3L))
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 3L, "Niveau", "20"))
      expect_identical(db$.nodes()$niveau_id[db$.nodes()$id == 3L], 20L)
    })
})

test_that("cyklisk foraelder-aendring afvises og genindlaeser", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 1L, "Forælder", "3"))
      expect_length(db$.calls()$updates, 0)
      expect_match(warn_msg(), "cyklus", ignore.case = TRUE)
      expect_identical(nodes(), db$.nodes())
      expect_identical(table_revision(), 1L)
    })
})

test_that("aendring i readOnly-kolonner (id/Struktur) ignoreres tavst", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 2L, "Struktur", "Hacket"))
      expect_length(db$.calls()$updates, 0)
      expect_identical(table_revision(), 0L)     # intet reload nødvendigt
    })
})

test_that("uaendret payload skriver ikke og genindlaeser ikke", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 2L, "Kort navn", "B"))  # uændret
      expect_length(db$.calls()$updates, 0)
      expect_identical(nodes(), db$.nodes())
      expect_identical(table_revision(), 0L)
    })
})

test_that("tomt langt navn giver obligatorisk advarsel uden databaseopdatering", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 2L, "Langt navn", NULL))
      expect_length(db$.calls()$updates, 0)
      expect_match(warn_msg(), "obligatorisk", ignore.case = TRUE)
      expect_identical(nodes(), db$.nodes())
    })
})

test_that("niveau der ikke er dybere end foraelder gemmes med advarsel", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 2L, "Niveau", "10"))
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
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 2L, "Langt navn", "Fejler"))
      expect_length(db$.calls()$updates, 1)
      expect_identical(nodes(), db$.nodes())
      expect_match(warn_msg(), "gendannet", ignore.case = TRUE)
      expect_identical(table_revision(), 1L)
    })
})

test_that("inline aktiv-flueben gemmer boolean via update_node (indikator-hierarki)", {
  db <- fake_ih_db()
  cfg <- HIERARCHY_TABLES$indikator_hierarki
  testServer(mod_hierarchy_server, args = list(db = db, cfg = cfg), {
    session$setInputs(tbl = .hierarchy_grid_edit(
      isolate(tree()), cfg, 2L, "Aktiv", "FALSE"))
    upd <- tail(db$.calls()$updates, 1)[[1]]
    expect_identical(upd$id, 2L)
    expect_identical(upd$values$aktiv, FALSE)
    expect_false(db$.nodes()$aktiv[db$.nodes()$id == 2L])
    expect_match(status_msg(), "Gemt")
  })
})

test_that("ny node i indikator-hierarkiet gemmer aktiv-flag fra formularen", {
  db <- fake_ih_db()
  cfg <- HIERARCHY_TABLES$indikator_hierarki
  testServer(mod_hierarchy_server, args = list(db = db, cfg = cfg), {
    session$setInputs(new_node = 1)
    session$setInputs(
      h_hierarki_navn = "Nyt datasaet", h_hierarki_navn_kort = "Nyt",
      h_brug_kort_navn_i_titel = TRUE,
      h_beskrivelse_kort = "", h_beskrivelse_lang = "", h_kilde_id = "",
      h_parent = "1", h_niveau = "20", h_aktiv = FALSE, h_save = 1)
    created <- db$.calls()$created
    expect_false(is.null(created))
    expect_identical(created$hierarki_navn, "Nyt datasaet")
    expect_identical(created$parent_id, 1L)
    expect_false(created$aktiv)
    expect_true(created$brug_kort_navn_i_titel)
  })
})

test_that("datapakke/datasaet-filtre beskaerer grid'et til valgt gren", {
  db <- fake_ih_db()
  cfg <- HIERARCHY_TABLES$indikator_hierarki
  testServer(mod_hierarchy_server, args = list(db = db, cfg = cfg), {
    expect_identical(shown()$id, c(1L, 2L, 4L, 3L))       # fuldt trae
    session$setInputs(filter_datapakke = "1")
    expect_identical(shown()$id, c(1L, 2L, 4L, 3L))       # rod-gren = alt
    session$setInputs(filter_datasaet = "2")
    expect_identical(shown()$id, c(2L, 4L))               # datasaet + subtree
    expect_identical(shown()$depth, c(0L, 1L))            # renormaliseret
    # mest specifikke filter vinder; tom datasaet → tilbage til datapakke-gren
    session$setInputs(filter_datasaet = "")
    expect_identical(shown()$id, c(1L, 2L, 4L, 3L))
  })
})

test_that("inline-redigering under aktivt filter rammer korrekt node", {
  db <- fake_ih_db()
  cfg <- HIERARCHY_TABLES$indikator_hierarki
  testServer(mod_hierarchy_server, args = list(db = db, cfg = cfg), {
    session$setInputs(filter_datasaet = "2")
    session$setInputs(tbl = .hierarchy_grid_edit(
      isolate(shown()), cfg, 4L, "Navn", "Samling ny"))
    upd <- tail(db$.calls()$updates, 1)[[1]]
    expect_identical(upd$id, 4L)
    expect_identical(upd$values$hierarki_navn, "Samling ny")
    expect_match(status_msg(), "Gemt")
  })
})

test_that("hierarki-UI viser kun filtre for cfg med filters", {
  ih_html <- as.character(mod_hierarchy_ui(
    "ih", HIERARCHY_TABLES$indikator_hierarki))
  expect_match(ih_html, "ih-filter_datapakke_ui", fixed = TRUE)
  expect_match(ih_html, "ih-filter_datasaet_ui", fixed = TRUE)
  org_html <- as.character(mod_hierarchy_ui(
    "org", HIERARCHY_TABLES$org_struktur))
  expect_false(grepl("filter_", org_html, fixed = TRUE))
})

test_that("identiske status- og advarselsbeskeder udloeser nye events", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 2L, "Kort navn", "B1"))
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 2L, "Kort navn", "B2"))
      expect_identical(status_msg(), "Gemt")
      expect_identical(status_event()$nonce, 2L)

      # To forskellige cykliske flyt → samme advarsels-tekst, to events
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 1L, "Forælder", "2"))
      session$setInputs(tbl = .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 1L, "Forælder", "3"))
      expect_match(warn_msg(), "cyklus", ignore.case = TRUE)
      expect_identical(warn_event()$nonce, 2L)
    })
})

test_that("hierarki: aendring der kun ankommer via fullData gemmes", {
  # Samme excelR-batch-clobbering som indikator-grid'et: markør-flytningen
  # efter celle-commit overskriver change-eventet — fullData baerer aendringen.
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      edit <- .hierarchy_grid_edit(
        isolate(tree()), .hierarchy_cfg(), 2L, "Langt navn", "Barn B ny")
      session$setInputs(tbl = list(
        forSelectedVals = TRUE,
        selectedDataBoundary = list(borderTop = 1, borderBottom = 1,
                                    borderLeft = 0, borderRight = 0),
        fullData = list(colHeaders = edit$colHeaders, data = edit$data)))
      upd <- tail(db$.calls()$updates, 1)[[1]]
      expect_identical(upd$id, 2L)
      expect_identical(upd$values$organisatorisk_navn_langt, "Barn B ny")
      expect_identical(selected_id(), 2L)   # selektionen registreres stadig
    })
})

test_that("identisk fantom-diff efter reload skippes som ekko (loop-værn)", {
  # Checkbox-payloadens "true" ≠ grid-datas "TRUE" giver et diff, som
  # .prepare_hierarchy_inline_update normaliserer til 'unchanged' → tavst
  # reload. excelR gen-sender payloads efter hvert re-render, så uden værn
  # kører unchanged→reload→ekko i ring uden fejlmeddelelse. Værnet skipper
  # den identiske diff lige efter reloadet.
  db <- fake_ih_db()
  cfg <- HIERARCHY_TABLES$indikator_hierarki
  testServer(mod_hierarchy_server, args = list(db = db, cfg = cfg), {
    p <- .hierarchy_grid_edit(isolate(tree()), cfg, 2L, "Aktiv", "true")
    session$setInputs(tbl = p)
    expect_length(db$.calls()$updates, 0)      # unchanged → ingen skrivning
    expect_identical(table_revision(), 1L)     # men ét reload
    session$setInputs(tbl = p)                 # ekkoet
    expect_identical(table_revision(), 1L)     # intet nyt reload → loop brudt
    expect_length(db$.calls()$updates, 0)
    session$setInputs(tbl = p)                 # ægte gentagelse (værn forbrugt)
    expect_identical(table_revision(), 2L)     # → behandles igen som normalt
  })
})
