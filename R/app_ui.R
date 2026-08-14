#' @import shiny
#' @noRd
app_ui <- function(request) {
  bslib::page_navbar(id = "nav", title = "BFH Metadata",
    header = .jexcel_theme_css(),
    bslib::nav_panel("Start", value = "start", .landing_ui()),
    bslib::nav_panel("Indikatorer", value = "indikatorer",
      mod_indikator_crud_ui("indik")),
    bslib::nav_panel("Indikator-hierarki", value = "indikator_hierarki",
      mod_hierarchy_ui("indikator_hierarki",
                       HIERARCHY_TABLES$indikator_hierarki)),
    bslib::nav_panel("Signal-gennemgang", value = "signal",
      mod_signal_review_ui("signal")),
    bslib::nav_panel("Diagrammer", value = "diagrammer",
      mod_diagram_ui("diagram")),
    bslib::nav_panel("Organisation", value = "org_struktur",
      mod_hierarchy_ui("org_struktur", HIERARCHY_TABLES$org_struktur)),
    do.call(bslib::nav_menu, c(list(title = "Opslagstabeller"),
      lapply(LOOKUP_TABLES, function(cfg)
        bslib::nav_panel(cfg$label, value = cfg$id,
          mod_lookup_table_ui(cfg$id, cfg))))),
    bslib::nav_spacer(),
    bslib::nav_item(uiOutput("write_badge"))
  )
}

#' Badge der viser om DB-skrivning er aktiv (roed = writes rammer prod).
#' Ren funktion — unit-testbar uden session.
#' @noRd
.write_badge_ui <- function(enabled) {
  if (enabled) {
    tags$span(class = "badge text-bg-danger align-self-center",
              title = "BFHMETA_WRITE=1 — aendringer skrives til Supabase",
              "Skrivning aktiv")
  } else {
    tags$span(class = "badge text-bg-secondary align-self-center",
              title = "Saet BFHMETA_WRITE=1 for at aktivere skrivning",
              "Skrivebeskyttet")
  }
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
        "Fuld redigering: oversigt, modal og relationer."),
      tile("indikator_hierarki", "Indikator-hierarki",
        "Træ-redigering af datasæt og datapakker: felter, flyt, aktiv-flag.")),
    sect("Signal-gennemgang"),
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      tile("signal", "Signal-gennemgang",
        "Scan parquet for Anhøj-signaler og registrér faseskift.")),
    sect("Diagrammer"),
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      tile("diagrammer", "Diagrammer",
        "Filterbar oversigt og redigering af alle diagrammer.")),
    sect("Organisation"),
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      tile("org_struktur", "Organisations-struktur",
        "Træ-redigering: felter, flyt og opret/slet."),
      tile("org_oversaettelse",
        Find(function(cfg) cfg$id == "org_oversaettelse", LOOKUP_TABLES)$label,
        "Inline-redigering direkte i tabellen.")),
    sect("Opslagstabeller"),
    do.call(bslib::layout_column_wrap, c(list(width = 1/3, fill = FALSE),
      lapply(Filter(function(cfg) cfg$id != "org_oversaettelse", LOOKUP_TABLES),
        function(cfg)
        tile(cfg$id, cfg$label, "Inline-redigering direkte i tabellen.")))),
    sect("Vedligeholdelse"),
    div(class = "mb-4",
      # Manuel kompaktering (intradag-workflows: "jeg har lige regenereret
      # data"). Sweep afgør selv om der er noget at gøre.
      mod_compact_btn_ui("compact"))
  )
}
