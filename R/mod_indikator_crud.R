#' Bygger ét form-input baseret på felt-kind
#' @noRd
.field_input <- function(ns, f, fk_choices = list()) {
  id <- ns(f$col)
  switch(f$kind,
    "pk"       = NULL,
    "fk"       = selectInput(id, f$col, choices = c("(ingen)" = "", fk_choices[[f$col]])),
    "bool"     = checkboxInput(id, f$col, value = FALSE),
    "date"     = dateInput(id, f$col, value = NA),
    "int"      = numericInput(id, f$col, value = NA),
    "textarea" = textAreaInput(id, f$col, value = ""),
    textInput(id, f$col, value = "")  # text (default)
  )
}

#' @noRd
mod_indikator_crud_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 420, position = "right", open = TRUE,
      h5("Redigér / opret"),
      uiOutput(ns("form")),
      div(class = "d-flex gap-2 mt-2",
        actionButton(ns("new"), "Ny", class = "btn-secondary"),
        actionButton(ns("save"), "Gem", class = "btn-primary"),
        actionButton(ns("soft_delete"), "Deaktivér", class = "btn-warning")
      ),
      verbatimTextOutput(ns("status"))
    ),
    div(
      checkboxInput(ns("show_inactive"), "Vis inaktive", value = TRUE),
      DT::DTOutput(ns("tbl"))
    )
  )
}
