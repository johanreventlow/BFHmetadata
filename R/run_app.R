#' Kør CRUD-appen (kun lokalt — host 127.0.0.1)
#' @export
run_app <- function(...) {
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    options = list(host = "127.0.0.1", ...)
  )
}
