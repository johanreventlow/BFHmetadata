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
