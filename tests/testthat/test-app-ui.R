test_that(".write_badge_ui viser status for skrive-guard", {
  on <- as.character(.write_badge_ui(TRUE))
  off <- as.character(.write_badge_ui(FALSE))
  expect_match(on, "Skrivning aktiv")
  expect_match(on, "text-bg-danger")        # roed: writes rammer prod-DB
  expect_match(off, "Skrivebeskyttet")
  expect_match(off, "text-bg-secondary")
})

test_that("app_ui konstruerer uden fejl", {
  expect_no_error(app_ui(NULL))
})

test_that("hierarki-UI har opret og slet men ingen aabn-knap", {
  html <- as.character(mod_hierarchy_ui(
    "org", HIERARCHY_TABLES$org_struktur))
  expect_match(html, "org-new_node", fixed = TRUE)
  expect_match(html, "org-delete_selected", fixed = TRUE)
  expect_false(grepl("open_id", html, fixed = TRUE))
})
