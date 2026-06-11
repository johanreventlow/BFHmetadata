# Minimal probe: monter modul-UI'ets graf-output i dets ns + en triviel server
# der printer input$chart_selected. Klik et punkt → ISO-dato skal printes.
# Verificerer ggiraph-input-wiring (input$chart_selected) FØR server-logikken
# i Task 6-7 bygger på navnet. Kør: Rscript dev/signal_chart_probe.R
pkgload::load_all(".")
library(shiny)
ui <- fluidPage(ggiraph::girafeOutput("signal-chart"))
server <- function(input, output, session) {
  output[["signal-chart"]] <- ggiraph::renderGirafe({
    d <- data.frame(dato = as.Date("2020-01-01") + 0:9 * 30, vaerdi = 1:10,
                    naevner = NA_real_)
    interactive_run_chart(compute_signal(d)$qic_result)
  })
  # Modul-namespace 'signal' → fuldt input-id er 'signal-chart_selected'
  observe(message("chart_selected = ", input[["signal-chart_selected"]]))
}
shiny::runApp(shinyApp(ui, server), port = 3902, launch.browser = TRUE)
