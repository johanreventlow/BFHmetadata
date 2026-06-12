#' @import shiny
#' @noRd
app_ui <- function(request) {
  bslib::page_navbar(id = "nav", title = "BFH Metadata",
    bslib::nav_panel("Start", value = "start", .landing_ui()),
    bslib::nav_panel("Indikatorer", value = "indikatorer",
      mod_indikator_crud_ui("indik")),
    bslib::nav_panel("Signal-gennemgang", value = "signal",
      mod_signal_review_ui("signal")),
    do.call(bslib::nav_menu, c(list(title = "Opslagstabeller"),
      lapply(LOOKUP_TABLES, function(cfg)
        bslib::nav_panel(cfg$label, value = cfg$id,
          mod_lookup_table_ui(cfg$id, cfg)))))
  )
}

#' Startside med flise-grid (vælg tabel/område). Flise-knapper er ej namespacede
#' (root-input) → håndteres i app_server via input$go_<value>.
#' @noRd
.landing_ui <- function() {
  tile <- function(value, title, desc) bslib::card(class = "h-100",
    bslib::card_body(
      h5(title, class = "mb-1"),
      p(desc, class = "text-muted small flex-grow-1"),
      actionButton(paste0("go_", value), "Åbn ›",
        class = "btn-sm btn-outline-primary align-self-start")))
  sect <- function(txt) div(class = "mt-4 mb-2",
    h6(txt, class = "text-uppercase text-primary",
       style = "font-size:.8rem;letter-spacing:.06em;"))
  tagList(
    sect("Indikatorer"),
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      tile("indikatorer", "Indikatorer",
        "Fuld redigering: oversigt, modal og relationer.")),
    sect("Signal-gennemgang"),
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      tile("signal", "Signal-gennemgang",
        "Scan parquet for Anhøj-signaler og registrér faseskift.")),
    sect("Opslagstabeller"),
    do.call(bslib::layout_column_wrap, c(list(width = 1/3, fill = FALSE),
      lapply(LOOKUP_TABLES, function(cfg)
        tile(cfg$id, cfg$label, "Inline-redigering direkte i tabellen."))))
  )
}
