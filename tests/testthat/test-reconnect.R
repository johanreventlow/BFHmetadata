test_that("reconnect-dependency peger paa script og stylesheet", {
  dep <- .reconnect_dependency()
  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$script, "bfh-reconnect.js")
  expect_equal(dep$stylesheet, "bfh-reconnect.css")
  # Assets skal faktisk findes i installationen — en tavs 404 ville betyde
  # at det moerke overlay kommer tilbage uden at nogen test fanger det.
  expect_true(file.exists(file.path(dep$src$file, dep$script)))
  expect_true(file.exists(file.path(dep$src$file, dep$stylesheet)))
})

test_that("app_ui inkluderer reconnect-dependency", {
  deps <- htmltools::findDependencies(app_ui(NULL))
  expect_true("bfh-reconnect" %in%
                vapply(deps, function(d) d$name, character(1)))
})

test_that("reconnect-js undertrykker overlay og genaabner gemt fane", {
  js <- readLines(file.path(.reconnect_dependency()$src$file,
                            "bfh-reconnect.js"), warn = FALSE)
  js <- paste(js, collapse = "\n")
  expect_match(js, "shiny:disconnected", fixed = TRUE)
  expect_match(js, "preventDefault", fixed = TRUE)      # intet moerkt overlay
  expect_match(js, "bfh_restore_nav", fixed = TRUE)     # fane genaabnes
  expect_match(js, "location.reload", fixed = TRUE)     # stille genoptagelse
})
