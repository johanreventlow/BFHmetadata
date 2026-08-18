# Rene trae-funktioner: depth-first orden + subtree. Kritiske paths: cyklus,
# multi-rod, orphans — fuld unit-daekning uden DB.

.tree_df <- function() data.frame(
  id = c(1L, 2L, 3L, 4L, 5L, 6L),
  parent = c(NA, 1L, 1L, 3L, NA, 999L),   # 2 roedder + orphan (999 findes ej)
  navn = c("RodA", "B", "A-child", "D", "RodB", "Orphan"),
  stringsAsFactors = FALSE)

test_that("hierarchy_order giver depth-first orden med dybder", {
  out <- hierarchy_order(.tree_df(), "id", "parent", sort_col = "navn")
  # RodA(0) -> A-child(1) -> D(2) -> B(1), saa RodB(0), orphan behandles som rod
  expect_identical(out$id, c(1L, 3L, 4L, 2L, 5L, 6L))
  expect_identical(out$depth, c(0L, 1L, 2L, 1L, 0L, 0L))
})

test_that("hierarchy_order overlever cyklus uden at haenge", {
  df <- data.frame(id = 1:3, parent = c(2L, 1L, NA))  # 1<->2 cyklus, 3 rod
  out <- hierarchy_order(df, "id", "parent")
  expect_setequal(out$id, 1:3)                        # alle noder med, praecis en gang
  expect_identical(nrow(out), 3L)
})

test_that("hierarchy_order haandterer tom df", {
  df <- data.frame(id = integer(0), parent = integer(0))
  out <- hierarchy_order(df, "id", "parent")
  expect_identical(nrow(out), 0L)
  expect_true("depth" %in% names(out))
})

test_that("hierarchy_descendants returnerer subtree inkl. noden selv", {
  df <- .tree_df()
  expect_setequal(hierarchy_descendants(df, "id", "parent", 1L),
                  c(1L, 2L, 3L, 4L))
  expect_setequal(hierarchy_descendants(df, "id", "parent", 3L), c(3L, 4L))
  expect_identical(hierarchy_descendants(df, "id", "parent", 5L), 5L)
})

test_that("hierarchy_descendants overlever cyklus", {
  df <- data.frame(id = 1:2, parent = c(2L, 1L))
  expect_setequal(hierarchy_descendants(df, "id", "parent", 1L), c(1L, 2L))
})

test_that("hierarchy_subtree beskaerer til gren og renormaliserer dybden", {
  d <- hierarchy_order(.tree_df(), "id", "parent", sort_col = "navn")
  sub <- hierarchy_subtree(d, "id", "parent", 3L)
  expect_identical(sub$id, c(3L, 4L))         # noden selv + efterkommere
  expect_identical(sub$depth, c(0L, 1L))      # 1/2 → 0/1 (uindrykket rod)
  # blad → kun noden selv; raekkeorden (depth-first) bevares
  leaf <- hierarchy_subtree(d, "id", "parent", 5L)
  expect_identical(leaf$id, 5L)
  expect_identical(leaf$depth, 0L)
})

# --- Org-filter-helpers (Diagrammer-sidens organisations-dropdown) ----------

test_that("org_hierarchy_choices: depth-first orden med indrykning", {
  tree <- data.frame(id = c(1L, 2L, 3L, 4L),
                     parent_id = c(NA, 1L, 1L, 3L))
  labels <- data.frame(id = c(1L, 2L, 3L, 4L),
                       label = c("Hospital", "Kirurgi", "Medicin", "Afsnit M1"),
                       stringsAsFactors = FALSE)
  ch <- org_hierarchy_choices(tree, labels)
  expect_identical(unname(ch), c("1", "2", "3", "4"))
  expect_identical(names(ch)[1], "Hospital")               # rod uindrykket
  expect_match(names(ch)[2], "^\u00a0+Kirurgi$")           # barn indrykket
  expect_match(names(ch)[4], "^\u00a0+\u00a0+Afsnit M1$")  # barnebarn dybere
})

test_that("org_hierarchy_choices: NULL/tomt trae -> flad alfabetisk fallback", {
  labels <- data.frame(id = c(2L, 1L), label = c("B", "A"),
                       stringsAsFactors = FALSE)
  ch <- org_hierarchy_choices(NULL, labels)
  expect_identical(ch, c("A" = "1", "B" = "2"))
  expect_identical(org_hierarchy_choices(data.frame(), labels),
                   c("A" = "1", "B" = "2"))
})

test_that("org_hierarchy_choices: enheder uden traerække appendes fladt", {
  tree <- data.frame(id = 1L, parent_id = NA_integer_)
  labels <- data.frame(id = c(1L, 9L), label = c("Hospital", "Løsrevet"),
                       stringsAsFactors = FALSE)
  ch <- org_hierarchy_choices(tree, labels)
  expect_identical(unname(ch), c("1", "9"))
  expect_identical(names(ch)[2], "Løsrevet")
})

test_that("org_subtree_ids: valgt enhed + alle underliggende; NULL-trae = kun selv", {
  tree <- data.frame(id = c(1L, 2L, 3L, 4L),
                     parent_id = c(NA, 1L, 1L, 3L))
  expect_setequal(org_subtree_ids(tree, 1L), 1:4)
  expect_setequal(org_subtree_ids(tree, 3L), c(3L, 4L))
  expect_identical(org_subtree_ids(NULL, 3L), 3L)
})

test_that("hierarchy_indent_options ordner depth-first med nbsp-indrykning", {
  opts <- data.frame(
    id = c(3L, 1L, 2L), label = c("Barn", "Rod", "Datasaet"),
    aktiv = c(TRUE, TRUE, FALSE),
    parent_id = c(2L, NA, 1L), stringsAsFactors = FALSE)
  out <- hierarchy_indent_options(opts)
  expect_identical(out$id, c(1L, 2L, 3L))
  expect_identical(out$label,
    c("Rod", paste0(strrep(" ", 2), "Datasaet"),
      paste0(strrep(" ", 4), "Barn")))
  expect_identical(out$aktiv, c(TRUE, FALSE, TRUE))  # oevrige kolonner foelger
  expect_false("depth" %in% names(out))
  # Orphan (parent findes ikke): vises fladt som rod — tabes aldrig
  orphan <- data.frame(id = 5L, label = "O", parent_id = 99L)
  expect_identical(hierarchy_indent_options(orphan)$label, "O")
  # Uden parent_id-kolonne (flad fk-liste): uaendret passthrough
  flat <- data.frame(id = 1:2, label = c("A", "B"))
  expect_identical(hierarchy_indent_options(flat), flat)
  expect_null(hierarchy_indent_options(NULL))
})
