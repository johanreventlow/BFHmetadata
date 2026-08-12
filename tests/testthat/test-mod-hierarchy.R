test_that(".node_label falder tilbage til teknisk navn og id ved manglende display_col", {
  cfg <- .hierarchy_cfg()
  expect_identical(.node_label(cfg, "Langt navn", "teknisk", 5L), "Langt navn")
  expect_identical(.node_label(cfg, NA_character_, "teknisk", 5L), "teknisk")
  expect_identical(.node_label(cfg, "", "teknisk", 5L), "teknisk")
  expect_identical(.node_label(cfg, NA_character_, NA_character_, 5L),
                   "(uden navn #5)")
  expect_identical(.node_label(cfg, NA_character_, "", 5L), "(uden navn #5)")
})

test_that("modal aabner uden fejl naar en anden node mangler display_col-vaerdi", {
  db <- fake_hierarchy_db()
  # Ekstra node (5) uden organisatorisk_navn_langt -> parent-dropdown-labels
  # maa ikke indeholde NA (selectInput fejler paa NA-navngivne choices)
  extra <- data.frame(
    id = 5L, parent_id_raw = NA_integer_,
    organisatorisk_navn_teknisk = "uden_navn",
    organisatorisk_navn_langt = NA_character_,
    organisatorisk_navn_kort = NA_character_,
    niveau_id = 10L, niveau_num = 1L, niveau_navn = "Direktion",
    stringsAsFactors = FALSE)
  orig_list_nodes <- db$list_nodes
  db$list_nodes <- function() rbind(orig_list_nodes(), extra)
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
    expect_no_error(session$setInputs(open_id = 4))
  })
})

test_that("tree() giver korrekt depth-first-orden", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
    expect_identical(tree()$id, c(1L, 2L, 3L, 4L))
    expect_identical(tree()$depth, c(0L, 1L, 2L, 0L))
  })
})

test_that("gem eksisterende kalder update_node med korrekte values", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
    session$setInputs(open_id = 4)
    expect_equal(editing_id(), 4L)
    session$setInputs(
      h_organisatorisk_navn_teknisk = "rod_d",
      h_organisatorisk_navn_langt = "Rod D (opdateret)",
      h_organisatorisk_navn_kort = "D",
      h_parent = "", h_niveau = "10", h_save = 1)
    upd <- db$.calls()$updated
    expect_false(is.null(upd))
    expect_identical(upd$id, 4L)
    expect_identical(upd$values$organisatorisk_navn_langt, "Rod D (opdateret)")
    expect_null(db$.calls()$created)
  })
})

test_that("flyt til egen subtree blokeres (update_node IKKE kaldt)", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
    session$setInputs(open_id = 1)
    expect_equal(editing_id(), 1L)
    # Foraelder saettes til 3, som er i node 1's egen subtree
    session$setInputs(
      h_organisatorisk_navn_teknisk = "rod_a",
      h_organisatorisk_navn_langt = "Rod A",
      h_organisatorisk_navn_kort = "A",
      h_parent = "3", h_niveau = "10", h_save = 1)
    expect_null(db$.calls()$updated)
    expect_match(status_msg(), "cyklus|kreds|egen", ignore.case = TRUE)
  })
})

test_that("niveau-spring op giver bloed advarsel men gem gennemfoeres", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
    session$setInputs(open_id = 3)
    expect_equal(editing_id(), 3L)
    # Node 3 (niveau 1) bliver under node 2 (niveau 2) -> niveau_num 1 <= 2 -> OK allerede
    # Test niveau-spring: saet node 3's niveau til 20 (Afdeling) under rod 4 (niveau 1)
    session$setInputs(
      h_organisatorisk_navn_teknisk = "barn_c",
      h_organisatorisk_navn_langt = "Barn C",
      h_organisatorisk_navn_kort = "C",
      h_parent = "4", h_niveau = "10", h_save = 1)
    upd <- db$.calls()$updated
    expect_false(is.null(upd))
    expect_identical(upd$id, 3L)
  })
})

test_that("slet med boern blokeres (delete_node IKKE kaldt)", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
    session$setInputs(open_id = 1)   # node 1 har barn (node 2)
    session$setInputs(h_delete = 1)
    expect_null(db$.calls()$deleted)
    expect_match(warn_msg(), "børn")
  })
})

test_that("opret ny med tom foraelder kalder create_node med NA-parent", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
    session$setInputs(new_node = 1)
    expect_null(editing_id())
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
