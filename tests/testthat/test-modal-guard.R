test_that("modal-scroll-guard-dependency peger paa et script der findes", {
  dep <- .modal_scroll_guard_dependency()
  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$script, "bfh-modal-scroll-guard.js")
  # En tavs 404 ville betyde at scroll-laasen kommer tilbage uden at nogen
  # test fanger det (samme faelde som reconnect-overlayet).
  expect_true(file.exists(file.path(dep$src$file, dep$script)))
})

test_that("app_ui inkluderer modal-scroll-guard-dependency", {
  deps <- htmltools::findDependencies(app_ui(NULL))
  expect_true("bfh-modal-scroll-guard" %in%
                vapply(deps, function(d) d$name, character(1)))
})

test_that("modal-guard-js rydder body-laas og efterladt backdrop", {
  js <- readLines(file.path(.modal_scroll_guard_dependency()$src$file,
                            "bfh-modal-scroll-guard.js"), warn = FALSE)
  js <- paste(js, collapse = "\n")
  expect_match(js, "hidden.bs.modal", fixed = TRUE)   # lytter paa lukning
  expect_match(js, "modal-open", fixed = TRUE)        # body-klassen fjernes
  expect_match(js, "overflow", fixed = TRUE)          # inline-laasen fjernes
  expect_match(js, "modal-backdrop", fixed = TRUE)    # efterladt backdrop ryddes
  # Vagten MAA kun roere body naar ingen modal er tilbage — ellers ville den
  # rive scroll-laasen vaek under en modal der stadig er aaben (swap-flowet).
  expect_match(js, ".modal.show", fixed = TRUE)
})

test_that("modal-guard-js haenger ikke ALENE paa hidden.bs.modal", {
  js <- readLines(file.path(.modal_scroll_guard_dependency()$src$file,
                            "bfh-modal-scroll-guard.js"), warn = FALSE)
  js <- paste(js, collapse = "\n")
  # Kernen i fejlen er, at lukke-eventet kan udeblive helt (modalen rives ud af
  # DOM'en uden at Bootstrap afslutter sin hide). Lyttede vagten kun paa
  # hidden.bs.modal, ville den aldrig koere i netop det tilfaelde — derfor
  # skal den ogsaa se direkte paa <body>.
  expect_match(js, "MutationObserver", fixed = TRUE)
  expect_match(js, "attributeFilter", fixed = TRUE)
})
