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
