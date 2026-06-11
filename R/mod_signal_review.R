# Modul: Signal-gennemgang. Peg på parquet-mappe → scan filtrerede aktive
# Seriediagrammer → vis signal-diagrammer interaktivt → registrér faseskift.

#' @noRd
mod_signal_review_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(width = 320, open = TRUE,
      textInput(ns("parquet_dir"), "Parquet-mappe", placeholder = "/sti/til/parquet"),
      div(class = "d-flex gap-2 align-items-end",
        radioButtons(ns("window_mode"), "Datavindue",
          c("Alle data" = "all", "Seneste N" = "latest"), inline = TRUE),
        numericInput(ns("window_n"), "N", value = 24, min = 6, max = 200, width = "90px")),
      hr(),
      selectizeInput(ns("f_overafdeling"), "Overafdeling", NULL, multiple = FALSE),
      selectizeInput(ns("f_afsnit"), "Afsnit", NULL, multiple = FALSE),
      selectizeInput(ns("f_datapakke"), "Datapakke", NULL, multiple = FALSE),
      selectizeInput(ns("f_datasaet"), "Datasæt", NULL, multiple = FALSE),
      selectizeInput(ns("f_indikator_navn"), "Indikator", NULL, multiple = FALSE),
      actionButton(ns("scan"), "Scan", class = "btn-primary w-100"),
      uiOutput(ns("scan_summary"))),
    # Hovedområde: navigation + graf + faseskift
    div(class = "d-flex justify-content-between align-items-center mb-2",
      uiOutput(ns("nav_label")),
      div(class = "btn-group",
        actionButton(ns("prev"), "‹ Forrige", class = "btn-outline-secondary btn-sm"),
        actionButton(ns("next_"), "Næste ›", class = "btn-outline-secondary btn-sm"))),
    ggiraph::girafeOutput(ns("chart"), height = "420px"),
    hr(),
    div(class = "d-flex gap-2 align-items-center flex-wrap",
      uiOutput(ns("selected_label")),
      actionButton(ns("preview"), "Forhåndsvis faseskift", class = "btn-outline-primary btn-sm"),
      actionButton(ns("save_break"), "Gem faseskift", class = "btn-success btn-sm")),
    div(class = "mt-3",
      h6("Eksisterende median-knæk"),
      DT::DTOutput(ns("breaks_tbl")),
      actionButton(ns("delete_break"), "Fjern valgt knæk", class = "btn-outline-danger btn-sm mt-1"))
  )
}
