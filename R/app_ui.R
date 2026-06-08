#' @import shiny
#' @noRd
app_ui <- function(request) {
  bslib::page_navbar(
    title = "BFH Metadata — Indikatorer",
    bslib::nav_panel("Indikatorer", mod_indikator_crud_ui("indik"))
  )
}
