expect_session_dt_state <- function(widget, output_id) {
  options <- widget$x$options
  key <- paste0("BFHmetadata:dt-state:", output_id)
  save_callback <- options$stateSaveCallback
  load_callback <- options$stateLoadCallback

  expect_true(options$stateSave)
  expect_identical(options$stateDuration, -1L)
  expect_type(save_callback, "character")
  expect_length(save_callback, 1L)
  expect_type(load_callback, "character")
  expect_length(load_callback, 1L)
  expect_match(save_callback, "window.sessionStorage.setItem", fixed = TRUE)
  expect_match(load_callback, "window.sessionStorage.getItem", fixed = TRUE)
  expect_match(save_callback, key, fixed = TRUE)
  expect_match(load_callback, key, fixed = TRUE)
  expect_false(grepl("localStorage", save_callback, fixed = TRUE))
  expect_false(grepl("localStorage", load_callback, fixed = TRUE))
  expect_false(grepl("sInstance", save_callback, fixed = TRUE))
  expect_false(grepl("sInstance", load_callback, fixed = TRUE))

  invisible(list(
    key = key,
    save_callback = save_callback,
    load_callback = load_callback
  ))
}
