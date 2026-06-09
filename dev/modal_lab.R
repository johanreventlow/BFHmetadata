# =============================================================================
# modal_lab.R — Eksperiment-harness for modal-layout (INGEN DB)
# =============================================================================
# Hurtigt iteration-loop til at finde bedste placering af redigerings-felterne.
# Kør:   Rscript dev/modal_lab.R     → browser på 127.0.0.1:3900
# Rediger build_body() nedenfor → gem → browser reloader → modal gentegnes.
#
# Bruger fake-data + de RIGTIGE felt-widgets (.field_input) så det ser ud som
# den faktiske modal — men uden Supabase og uden at klikke [Åbn].
# Skift layout-variant i dropdownen øverst. Tilføj/ret varianter frit.
# =============================================================================

options(shiny.autoreload = TRUE)
pkgload::load_all(".", reset = TRUE, helpers = FALSE)

library(shiny)

# --- Fake-data (en plausibel indikator) --------------------------------------
fk_choices <- list(
  indikator_hierarki = c("Atrieflimren i Danmark" = 27, "Udredningsret" = 1),
  kontaktperson      = c("Louise Zølck" = 5, "Per Sen" = 2),
  datakilde          = c("LPR3" = 3, "SP" = 1)
)

vals <- list(
  id = 42,
  indikator_hierarki = 27, indikator_navn = "30-dages dødelighed",
  indikator_navn_teknisk = "ind_30d_mort", kontaktperson = 5,
  sp_rapport_id = "RPT-100", tillad_auto_opdatering = TRUE,
  aktiv_indikator = TRUE, nøgleindikator = FALSE,
  definition_kort = "Andel patienter døde inden for 30 dage.",
  definition_dataportal = "Beregnes som tæller/nævner pr. periode.",
  tæller_beskrivelse = "Antal døde inden 30 dage", nævner_beskrivelse = "Alle indlagte",
  indikator_ukompatibel_med = "—", mål = "< 5%", datakilde = 3,
  direkte_link = "https://eksempel.dk", ønsket_tendens = "Faldende",
  antal_observationer = 1200L, periode_fra = as.Date("2020-01-01"),
  output_enhed = "%"
)

# Fake m2m: navngivne options (label→id) + forvalgte ids
junction_opts <- list(
  faggrupper    = c("Kardiologi" = 1, "Anæstesi" = 2, "Kirurgi" = 3),
  dataprodukter = c("DP Hjerte" = 1, "DP Akut" = 2),
  organisation  = c("Afd. A" = 1, "Afd. B" = 2, "Klinik C" = 3)
)
junction_sel <- list(faggrupper = c(1, 3), dataprodukter = 1, organisation = 2)

# --- Hjælpere ----------------------------------------------------------------
ns <- function(x) x  # standalone app → ingen modul-namespace

# Byg ét skalar/FK-input efter kolonnenavn (genbruger .field_input fra pakken)
fld <- function(col) {
  f <- Find(function(x) x$col == col, INDIKATOR_FIELDS)
  if (is.null(f)) NULL else .field_input(ns, f, fk_choices, values = vals, prefix = "m_")
}
flds <- function(cols) do.call(tagList, lapply(cols, fld))

# Byg én m2m-multiselect
m2m_input <- function(key) {
  selectInput(paste0("m_j_", key), key, choices = junction_opts[[key]],
              selected = junction_sel[[key]], multiple = TRUE)
}
m2m_all <- function() lapply(names(junction_opts), m2m_input)

# Feltgrupper (juster frit under eksperimentet)
LEFT  <- c("indikator_navn", "indikator_navn_teknisk", "indikator_hierarki",
           "datakilde", "kontaktperson", "ønsket_tendens", "mål")
DEFS  <- c("definition_kort", "definition_dataportal", "tæller_beskrivelse",
           "nævner_beskrivelse", "indikator_ukompatibel_med")
REST  <- c("sp_rapport_id", "tillad_auto_opdatering", "aktiv_indikator",
           "nøgleindikator", "direkte_link", "antal_observationer",
           "periode_fra", "output_enhed")

# =============================================================================
# LAYOUT-VARIANTER — rediger her og se ændringen live
# =============================================================================
build_body <- function(variant) {
  if (variant == "to-kolonner") {
    div(class = "bfh-inline-labels",
      bslib::layout_columns(col_widths = c(6, 6), flds(LEFT), flds(DEFS)),
      tags$h6("Øvrige felter", class = "mt-3 text-muted"),
      do.call(bslib::layout_columns,
              c(list(col_widths = c(6, 6)), lapply(REST, fld))),
      hr(), h5("Relationer"),
      do.call(bslib::layout_columns, c(list(col_widths = c(4, 4, 4)), m2m_all())))

  } else if (variant == "faner") {
    div(class = "bfh-inline-labels",
      bslib::navset_tab(
        bslib::nav_panel("Stamdata",
          bslib::layout_columns(col_widths = c(6, 6), flds(LEFT),
            do.call(tagList, c(lapply(REST, fld))))),
        bslib::nav_panel("Definitioner", flds(DEFS)),
        bslib::nav_panel("Relationer",
          do.call(bslib::layout_columns,
                  c(list(col_widths = c(4, 4, 4)), m2m_all())))
      ))

  } else if (variant == "accordion") {
    div(class = "bfh-inline-labels",
      bslib::accordion(open = "Stamdata",
        bslib::accordion_panel("Stamdata",
          bslib::layout_columns(col_widths = c(6, 6), flds(LEFT), flds(c("ønsket_tendens","mål")))),
        bslib::accordion_panel("Definitioner", flds(DEFS)),
        bslib::accordion_panel("Øvrige felter",
          do.call(bslib::layout_columns, c(list(col_widths = c(6, 6)), lapply(REST, fld)))),
        bslib::accordion_panel("Relationer",
          do.call(bslib::layout_columns, c(list(col_widths = c(4, 4, 4)), m2m_all())))
      ))
  }
}

modal_css <- tags$style(HTML(paste(
  ".modal-dialog{margin-top:24px;}",
  ".modal-body{max-height:78vh;overflow-y:auto;}",
  ".bfh-inline-labels .shiny-input-container{display:flex;align-items:baseline;gap:.5rem;margin-bottom:.5rem;}",
  ".bfh-inline-labels .shiny-input-container>label{flex:0 0 40%;max-width:40%;text-align:right;margin:0;}",
  ".bfh-inline-labels .shiny-input-container>:not(label){flex:1 1 auto;min-width:0;}")))

show_lab_modal <- function(variant) {
  showModal(modalDialog(
    title = paste("Redigér indikator (lab) —", variant), size = "xl",
    easyClose = TRUE, modal_css, build_body(variant),
    footer = modalButton("Luk")))
}

# --- App ---------------------------------------------------------------------
ui <- bslib::page_fluid(
  h4("Modal-layout lab", class = "mt-3"),
  p(class = "text-muted",
    "Vælg variant → modal åbner. Rediger build_body() i dev/modal_lab.R, gem, browser reloader."),
  selectInput("variant", "Layout-variant",
    choices = c("to-kolonner", "faner", "accordion"), selected = "to-kolonner"),
  actionButton("open", "Åbn / genåbn modal", class = "btn-primary")
)

server <- function(input, output, session) {
  # Auto-åbn ved start + ved reload (autoreload genstarter session)
  observe(show_lab_modal(input$variant)) |>
    bindEvent(input$variant, ignoreInit = FALSE)
  observeEvent(input$open, show_lab_modal(input$variant))
}

shiny::runApp(shinyApp(ui, server), host = "127.0.0.1", port = 3900,
              launch.browser = TRUE)
