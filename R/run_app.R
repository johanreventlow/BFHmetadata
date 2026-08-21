#' Kør CRUD-appen (kun lokalt — host 127.0.0.1)
#' @param ... Additional options passed to [shiny::shinyApp()].
#' @return A shiny app object.
#' @export
run_app <- function(...) {
  # Windows: arrow's mimalloc-allokator korrumperer heapen under parquet-scan
  # (0xC0000005 i ntdll.dll, se commit d502e6e). Sat defensivt her — ikke kun
  # i .Renviron — så fixet virker uanset launch-kontekst/working directory.
  if (identical(Sys.info()[["sysname"]], "Windows") &&
      Sys.getenv("ARROW_DEFAULT_MEMORY_POOL") == "") {
    Sys.setenv(ARROW_DEFAULT_MEMORY_POOL = "system")
  }

  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    options = list(host = "127.0.0.1", ...)
  )
}
