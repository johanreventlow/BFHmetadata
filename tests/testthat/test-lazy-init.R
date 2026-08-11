# Lazy-init: modulernes server-funktioner (og dermed deres opstart-queries)
# må først køre når brugeren faktisk åbner fanen. Appen lander på "Start", så
# uden dette betaler ALLE brugere for alle modulers DB-kald ved hver opstart.

test_that("next_tick_session: tick-kode med reaktive læsninger kører UDEN reaktiv kontekst", {
  # Produktionsfejl: later-callbacks har ingen reaktiv kontekst, så en bar
  # reaktiv læsning i tick-koden kastede "Operation not allowed without an
  # active reactive context". testServer maskerer det (wrapper alt i
  # isolate), så denne test kører ticket RÅT udenfor — som i produktion.
  queue <- list()
  withr::local_options(list(
    bfhmeta.scan_scheduler = function(fn) queue[[length(queue) + 1]] <<- fn))
  rv <- shiny::reactiveVal(41L)
  fake_session <- list(isClosed = function() FALSE)
  hit <- NULL
  next_tick_session(fake_session, function() hit <<- rv() + 1L)  # bar læsning
  queue[[1]]()                       # kør ticket uden kontekst (som later gør)
  expect_equal(hit, 42L)             # isolate-wrap i helperen redder læsningen
})

test_that("next_tick_session: lukket session → fn røres aldrig; fejl når aldrig top-level", {
  queue <- list()
  withr::local_options(list(
    bfhmeta.scan_scheduler = function(fn) queue[[length(queue) + 1]] <<- fn))
  ran <- FALSE
  next_tick_session(list(isClosed = function() TRUE), function() ran <<- TRUE)
  queue[[1]]()
  expect_false(ran)                  # lukket session → stille død
  # Session-objekt der selv fejler på isClosed → regnes som lukket
  next_tick_session(list(isClosed = function() stop("boom")),
                    function() ran <<- TRUE)
  queue[[2]]()
  expect_false(ran)
  # Fejl i selve ticket logges men kastes ALDRIG videre
  next_tick_session(list(isClosed = function() FALSE),
                    function() stop("tick-fejl"))
  expect_no_error(queue[[3]]())
})

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

test_that("lazy_module med loading: init udskydes til næste tick (fanen skifter straks)", {
  # Uden udskydelse blokerer modulets opstart-queries selve fane-skiftet —
  # klik på en flise føltes "dødt" i sekunder. Med loading-tekst vises en
  # notifikation, fanen renderes med det samme, og init kører i næste tick
  # MED korrekt reactive domain (ellers fejler moduleServer i init).
  queue <- list()
  withr::local_options(list(
    bfhmeta.scan_scheduler = function(fn) queue[[length(queue) + 1]] <<- fn))
  inits <- 0L; dom <- NULL
  shiny::testServer(function(input, output, session) {
    sel <- reactive(input$nav)
    lazy_module("signal", sel, function() {
      inits <<- inits + 1L
      dom <<- shiny::getDefaultReactiveDomain()
    }, session = session, loading = "Henter diagram-oversigt…")
  }, {
    session$setInputs(nav = "signal")
    expect_equal(inits, 0L)                  # fane-skiftet betalte IKKE for init
    while (length(queue) > 0) { fn <- queue[[1]]; queue <- queue[-1]; fn() }
    expect_equal(inits, 1L)                  # init kørte i næste tick...
    expect_identical(dom, session)           # ...med korrekt domain (moduleServer)
    # Retur til fanen → ingen re-init
    session$setInputs(nav = "start")
    session$setInputs(nav = "signal")
    while (length(queue) > 0) { fn <- queue[[1]]; queue <- queue[-1]; fn() }
    expect_equal(inits, 1L)
  })
})

test_that("lazy_module uden loading: uændret synkron adfærd (bagudkompatibel)", {
  inits <- 0L
  shiny::testServer(function(input, output, session) {
    sel <- reactive(input$nav)
    lazy_module("x", sel, function() inits <<- inits + 1L)
  }, {
    session$setInputs(nav = "x")
    expect_equal(inits, 1L)                  # straks, ingen kø involveret
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
