#' @import shiny
#' @noRd
app_ui <- function(request) {
  bslib::page_navbar(id = "nav", title = "BFH Metadata",
    header = tagList(.jexcel_theme_css(), .excel_adapter_dependency(),
                     .excel_echo_guard_dependency(),
                     .reconnect_dependency(), .hidden_nav_css()),
    bslib::nav_panel("Start", value = "start", .landing_ui()),
    bslib::nav_panel("Signal-gennemgang", value = "signal",
      mod_signal_review_ui("signal")),
    bslib::nav_panel("Indikatorer", value = "indikatorer",
      mod_indikator_crud_ui("indik")),
    bslib::nav_panel("Diagrammer", value = "diagrammer",
      mod_diagram_ui("diagram")),
    bslib::nav_panel("Mål", value = "maal",
      mod_diagram_maal_ui("maal")),
    bslib::nav_panel("Organisation", value = "org_struktur",
      mod_hierarchy_ui("org_struktur", HIERARCHY_TABLES$org_struktur)),
    do.call(bslib::nav_menu, c(list(title = "Opslagstabeller"),
      lapply(LOOKUP_TABLES, function(cfg)
        bslib::nav_panel(cfg$label, value = cfg$id,
          mod_lookup_table_ui(cfg$id, cfg))))),
    # Fane UDEN menu-punkt: naas via Start-flisen (nav_select). Menu-linket
    # skjules af .hidden_nav_css — panelet skal stadig ligge i navbar'ens
    # tabset for at nav_select/lazy_module virker.
    bslib::nav_panel("Indikator-hierarki", value = "indikator_hierarki",
      mod_hierarchy_ui("indikator_hierarki",
                       HIERARCHY_TABLES$indikator_hierarki)),
    bslib::nav_spacer(),
    bslib::nav_item(uiOutput("write_badge"))
  )
}

#' CSS der skjuler navbar-links for faner der kun skal naas via Start-siden.
#' Panelet forbliver i tabsettet (nav_select virker); kun menu-linket
#' forsvinder.
#' @noRd
.hidden_nav_css <- function() {
  tags$style(HTML(paste0(
    '.navbar a[data-value="indikator_hierarki"] { display: none; }')))
}

#' Badge der viser om DB-skrivning er aktiv (roed = writes rammer prod).
#' Ren funktion — unit-testbar uden session.
#' @noRd
.write_badge_ui <- function(enabled) {
  if (enabled) {
    tags$span(class = "badge text-bg-danger align-self-center",
              title = "BFHMETA_WRITE=1 \u2014 aendringer skrives til Supabase",
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
      actionButton(paste0("go_", value), "\u00C5bn \u203A",
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
        "Tr\u00E6-redigering af datas\u00E6t og datapakker: felter, flyt, aktiv-flag.")),
    sect("Signal-gennemgang"),
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      tile("signal", "Signal-gennemgang",
        "Scan parquet for Anh\u00F8j-signaler og registr\u00E9r faseskift.")),
    sect("Diagrammer"),
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      tile("diagrammer", "Diagrammer",
        "Filterbar oversigt og redigering af alle diagrammer."),
      tile("maal", "Mål",
        "Oversigt og redigering af diagrammers mål (retning/værdi/dato).")),
    sect("Organisation"),
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      tile("org_struktur", "Organisations-struktur",
        "Tr\u00E6-redigering: felter, flyt og opret/slet."),
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
