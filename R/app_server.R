#' @noRd
app_server <- function(input, output, session) {
  pool <- db_connect()
  onStop(function() pool::poolClose(pool))
  # Delt cache-lager: alle modulers læse-accessors deler cache, og enhver
  # skrivning (uanset modul) rydder den → ingen stale visning på tværs af faner.
  store <- new_cache_store()
  db <- make_db_cached(make_db(pool), store = store)

  # Startup-kompaktering: spørg (modal) om det delte _compact-spejl skal
  # opfriskes, når det er forældet. Eager — skal kunne spørge på landings-
  # siden; koster ingen DB-kald, kun et lokalt manifest-tjek.
  mod_compact_server("compact")

  # Lazy-init: modulernes opstart-queries køres først når fanen åbnes.
  # Appen lander på "Start", så en app-start koster nu ingen DB-kald ud over
  # forbindelsen selv.
  selected_tab <- reactive(input$nav)
  lazy_module("indikatorer", selected_tab,
              function() mod_indikator_crud_server("indik", db),
              session = session, loading = "Henter indikator-oversigt…")
  lazy_module("signal", selected_tab,
              function() mod_signal_review_server("signal", db),
              session = session, loading = "Henter diagram-oversigt…")
  lazy_module("diagrammer", selected_tab,
              function() mod_diagram_server("diagram", db),
              session = session, loading = "Henter diagram-liste…")
  lazy_module("org_struktur", selected_tab, function() {
    # make_db_cached: node-skrivninger rydder det DELTE cache-lager, så
    # org-ændringer straks slår igennem i cachede org-læsninger (fx
    # org_enhed_variants i signal-fanen). Hierarkiets egne læsninger
    # (list_nodes m.fl.) er ikke registreret som cachede → passthrough.
    mod_hierarchy_server("org_struktur",
      make_db_cached(make_hierarchy_db(pool, HIERARCHY_TABLES$org_struktur),
                     store = store),
      HIERARCHY_TABLES$org_struktur)
  }, session = session, loading = "Henter organisations-træ…")

  # Opslagstabeller: ét generisk modul pr. LOOKUP_TABLES-element
  for (cfg in LOOKUP_TABLES) local({
    cc <- cfg
    lazy_module(cc$id, selected_tab, function() {
      mod_lookup_table_server(cc$id, make_db_cached(make_lookup_db(pool, cc),
                                                    store = store), cc)
    }, session = session, loading = paste0("Henter ", cc$label, "…"))
  })

  # Landing-fliser → naviger til valgt fane
  observeEvent(input$go_indikatorer, bslib::nav_select("nav", "indikatorer"))
  observeEvent(input$go_signal, bslib::nav_select("nav", "signal"))
  observeEvent(input$go_diagrammer, bslib::nav_select("nav", "diagrammer"))
  observeEvent(input$go_org_struktur, bslib::nav_select("nav", "org_struktur"))

  output$write_badge <- renderUI(.write_badge_ui(write_enabled()))
  for (cfg in LOOKUP_TABLES) local({
    cc <- cfg
    observeEvent(input[[paste0("go_", cc$id)]], bslib::nav_select("nav", cc$id))
  })
}
