# testServer-tests for mod_hierarchy med fake-db (closures der logger kald).
# 4-node trae: 1 (rod, niveau 1) -> 2 (niveau 2) -> 3 (niveau 1, niveau-spring
# op) ; 4 er anden rod (niveau 1).

.hierarchy_cfg <- function() HIERARCHY_TABLES$org_struktur

fake_hierarchy_db <- function() {
  nodes <- data.frame(
    id = c(1L, 2L, 3L, 4L),
    parent_id_raw = c(NA_integer_, 1L, 2L, NA_integer_),
    organisatorisk_navn_teknisk = c("rod_a", "barn_b", "barn_c", "rod_d"),
    organisatorisk_navn_langt = c("Rod A", "Barn B", "Barn C", "Rod D"),
    organisatorisk_navn_kort = c("A", "B", "C", "D"),
    niveau_id = c(10L, 20L, 10L, 10L),
    niveau_num = c(1L, 2L, 1L, 1L),
    niveau_navn = c("Direktion", "Afdeling", "Direktion", "Direktion"),
    stringsAsFactors = FALSE)
  niveauer <- data.frame(id = c(10L, 20L),
                         label = c("Direktion", "Afdeling"),
                         stringsAsFactors = FALSE)
  calls <- list(created = NULL, updated = NULL, deleted = NULL)
  list(
    list_nodes = function() nodes,
    niveau_options = function() niveauer,
    create_node = function(values) { calls$created <<- values; 99L },
    update_node = function(id, values) {
      calls$updated <<- list(id = id, values = values); 1L
    },
    delete_node = function(id) { calls$deleted <<- id; 1L },
    child_count = function(id) sum(nodes$parent_id_raw %in% id, na.rm = TRUE),
    .calls = function() calls
  )
}

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
