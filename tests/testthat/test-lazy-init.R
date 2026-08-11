# Lazy-init: modulernes server-funktioner (og dermed deres opstart-queries)
# må først køre når brugeren faktisk åbner fanen. Appen lander på "Start", så
# uden dette betaler ALLE brugere for alle modulers DB-kald ved hver opstart.

test_that("lazy_module: init kaldes ikke før fanen vælges", {
  inits <- 0L
  sel <- shiny::reactiveVal("start")
  shiny::isolate({
    lazy_module("indikatorer", sel, function() inits <<- inits + 1L)
  })
  expect_equal(inits, 0L)
})

test_that("lazy_module: init kaldes ÉN gang ved første besøg og aldrig igen", {
  inits <- 0L
  shiny::testServer(function(input, output, session) {
    sel <- reactive(input$nav)
    lazy_module("indikatorer", sel, function() inits <<- inits + 1L)
  }, {
    session$setInputs(nav = "start")
    expect_equal(inits, 0L)
    session$setInputs(nav = "indikatorer")
    expect_equal(inits, 1L)
    session$setInputs(nav = "signal")      # væk fra fanen
    session$setInputs(nav = "indikatorer") # og tilbage → ingen re-init
    expect_equal(inits, 1L)
  })
})

test_that("make_db_cached + lazy_module: app-start koster ingen læse-queries", {
  # Integrations-agtig: tæl faktiske accessor-kald gennem cache-laget mens
  # brugeren står på "Start" og derefter åbner ÉN fane.
  calls <- new.env(); calls$n <- 0L
  bump <- function(v) function(...) { calls$n <- calls$n + 1L; v }
  raw <- list(
    list_indikatorer = bump(data.frame(id = 1)),
    fk_options = bump(list()),
    list_active_seriediagrammer = bump(data.frame(diagram_id = 1L)),
    org_enhed_variants = bump(data.frame(org_id = 1L)))
  db <- make_db_cached(raw)
  shiny::testServer(function(input, output, session) {
    sel <- reactive(input$nav)
    lazy_module("indikatorer", sel, function() {
      db$list_indikatorer(); db$fk_options()
    })
    lazy_module("signal", sel, function() {
      db$list_active_seriediagrammer(); db$org_enhed_variants()
    })
  }, {
    session$setInputs(nav = "start")
    expect_equal(calls$n, 0L)            # landing → ingen DB-kald overhovedet
    session$setInputs(nav = "signal")
    expect_equal(calls$n, 2L)            # kun signal-modulets to opslag
    session$setInputs(nav = "indikatorer")
    expect_equal(calls$n, 4L)            # + indikator-modulets to
    session$setInputs(nav = "signal")
    expect_equal(calls$n, 4L)            # retur → hverken re-init eller ny query
  })
})

test_that("lazy_module: kun den valgte fanes modul initialiseres", {
  hits <- character(0)
  shiny::testServer(function(input, output, session) {
    sel <- reactive(input$nav)
    for (id in c("indikatorer", "signal", "diagrammer")) local({
      i <- id
      lazy_module(i, sel, function() hits <<- c(hits, i))
    })
  }, {
    session$setInputs(nav = "start")
    expect_equal(hits, character(0))
    session$setInputs(nav = "signal")
    expect_equal(hits, "signal")           # KUN signal-modulet betalte
    session$setInputs(nav = "diagrammer")
    expect_setequal(hits, c("signal", "diagrammer"))
  })
})
