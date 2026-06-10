#' @noRd
app_server <- function(input, output, session) {
  pool <- db_connect()
  onStop(function() pool::poolClose(pool))
  db <- make_db(pool)
  mod_indikator_crud_server("indik", db)

  # Opslagstabeller: ét generisk modul pr. LOOKUP_TABLES-element
  for (cfg in LOOKUP_TABLES) {
    mod_lookup_table_server(cfg$id, make_lookup_db(pool, cfg), cfg)
  }

  # Landing-fliser → naviger til valgt fane
  observeEvent(input$go_indikatorer, bslib::nav_select("nav", "indikatorer"))
  for (cfg in LOOKUP_TABLES) local({
    cc <- cfg
    observeEvent(input[[paste0("go_", cc$id)]], bslib::nav_select("nav", cc$id))
  })
}
