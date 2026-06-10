# =============================================================================
# lookup_lab.R — Prototype for landing-side + in-celle redigering (INGEN DB)
# =============================================================================
# Demonstrerer UX'en FØR rigtig implementering:
#  - Landing-side med flise-grid (vælg tabel/område)
#  - Ægte in-celle redigering (DT editable) med gem-kvittering
#  - Type-coercion: int-kolonne afviser ikke-tal + reverterer cellen
#  - Ny række (blank → redigér inline)
#  - Slet valgte række; blokeret med "i brug"-besked hvis refereret
#
# Kør:  Rscript dev/lookup_lab.R   → browser 127.0.0.1:3901
# Fake in-memory store; ingen Supabase. Selvstændig (genbruger IKKE rigtige
# moduler endnu — de bygges test-først efter godkendelse).
# =============================================================================

options(shiny.autoreload = TRUE)
suppressMessages({library(shiny); library(bslib); library(rhandsontable); library(DT)})

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Prototype-config (delmængde af planlagt LOOKUP_TABLES) ------------------
LAB_TABLES <- list(
  list(id = "faggrupper", table = "tblFaggrupper", pk = "Id",
       label = "Faggrupper", icon = "people",
       cols = list(list(col = "faggruppe", type = "text", label = "Faggruppe"))),
  list(id = "indikator_niveauer", table = "tblIndikatorNiveauer", pk = "Id",
       label = "Indikator-niveauer", icon = "diagram-3",
       cols = list(
         list(col = "indikator_niveau", type = "int", label = "Niveau (tal)"),
         list(col = "indikator_niveau_navn", type = "text", label = "Niveau-navn"))),
  list(id = "datakilder", table = "tblDatakilder", pk = "Id",
       label = "Datakilder", icon = "database",
       ref_demo = c(1),  # id 1 "i brug" → slet blokeres (demo)
       cols = list(
         list(col = "datakilde_navn", type = "text", label = "Navn"),
         list(col = "datakilde_beskrivelse", type = "text", label = "Beskrivelse")))
)

# --- Fake store + db-accessors (samme form som planlagt make_lookup_db) ------
# Plain environment (ikke reactiveValues) → kan læses/skrives uden reaktiv
# kontekst, præcis som den rigtige db læser fra pool. reload()+replaceData
# driver UI-opdatering.
make_fake_store <- function() {
  e <- new.env(parent = emptyenv())
  e$faggrupper <- data.frame(Id = 1:3,
    faggruppe = c("Læger", "Sygeplejersker", "Bioanalytikere"),
    stringsAsFactors = FALSE)
  e$indikator_niveauer <- data.frame(Id = 1:3,
    indikator_niveau = c(1L, 2L, 3L),
    indikator_niveau_navn = c("Region", "Hospital", "Afdeling"),
    stringsAsFactors = FALSE)
  e$datakilder <- data.frame(Id = 1:2,
    datakilde_navn = c("Sundhedsplatformen", "LABKA"),
    datakilde_beskrivelse = c("EPJ-data via SP", "Laboratoriesvar"),
    stringsAsFactors = FALSE)
  e
}

fake_lookup_db <- function(store, cfg) {
  list(
    list_rows = function() store[[cfg$id]],
    add_row = function() {
      d <- store[[cfg$id]]
      newid <- if (nrow(d)) max(d[[cfg$pk]]) + 1L else 1L
      blank <- d[1, , drop = FALSE]
      blank[] <- lapply(blank, function(x) if (is.character(x)) NA_character_ else NA)
      blank[[cfg$pk]] <- newid
      store[[cfg$id]] <- rbind(d, blank); newid
    },
    update_cell = function(pk_val, col, value) {
      d <- store[[cfg$id]]; d[d[[cfg$pk]] == pk_val, col] <- value
      store[[cfg$id]] <- d; invisible(TRUE)
    },
    delete_row = function(pk_val) {
      d <- store[[cfg$id]]; store[[cfg$id]] <- d[d[[cfg$pk]] != pk_val, , drop = FALSE]
      invisible(TRUE)
    },
    ref_count = function(pk_val) if (!is.null(cfg$ref_demo) && pk_val %in% cfg$ref_demo) 7L else 0L
  )
}

col_type <- function(cfg, col) {
  m <- Find(function(c) c$col == col, cfg$cols)
  if (is.null(m)) "text" else m$type
}

# --- Generisk tabel-UI/server med motor-skifter (DT vs rhandsontable) --------
lab_table_ui <- function(id, cfg) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex justify-content-between align-items-center mb-2 flex-wrap gap-2",
      h4(cfg$label, class = "m-0"),
      div(class = "d-flex gap-3 align-items-center",
        radioButtons(ns("engine"), NULL, inline = TRUE,
          choices = c("Tabel (DT)" = "dt", "Excel-grid" = "rhot"), selected = "dt"),
        actionButton(ns("add_row"), "Ny række", class = "btn-success btn-sm"),
        actionButton(ns("delete"), "Slet valgte række", class = "btn-outline-danger btn-sm"))),
    p(class = "text-muted small", textOutput(ns("hint"), inline = TRUE)),
    uiOutput(ns("grid"))
  )
}

lab_table_server <- function(id, db, cfg) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rows <- reactiveVal(db$list_rows())
    refresh <- reactiveVal(0)
    engine <- reactive(input$engine %||% "dt")

    output$hint <- renderText(if (engine() == "rhot")
      "Excel-grid: alle celler åbne på én gang; tal-kolonner accepterer kun tal; Id låst."
      else "DT: dobbeltklik en celle for at redigere; vælg en række for at slette; Id låst.")

    output$grid <- renderUI(if (engine() == "rhot")
      rhandsontable::rHandsontableOutput(ns("hot")) else DT::DTOutput(ns("dt")))

    # --- DT-motor (konsistent med indikator-oversigten) ----------------------
    output$dt <- DT::renderDT({
      refresh(); d <- isolate(rows())
      DT::datatable(d, rownames = FALSE, selection = "single",
        editable = list(target = "cell", disable = list(columns = 0)),
        options = list(dom = "t", paging = FALSE, scrollY = "380px", scrollCollapse = TRUE))
    })
    observeEvent(input$dt_cell_edit, {
      info <- input$dt_cell_edit; d <- rows()
      col <- names(d)[info$col + 1]; pk_val <- d[[cfg$pk]][info$row]
      val <- info$value
      if (identical(col_type(cfg, col), "int")) {
        val <- suppressWarnings(as.integer(val))
        if (is.na(val)) { showNotification("Forventet et heltal", type = "error", duration = 3)
          refresh(refresh() + 1); return() }   # snap tilbage
      }
      db$update_cell(pk_val, col, val); d[info$row, col] <- val; rows(d)
      showNotification("Gemt", type = "message", duration = 1)
    })

    # --- rhandsontable-motor (Excel-agtig) -----------------------------------
    output$hot <- rhandsontable::renderRHandsontable({
      refresh(); d <- isolate(rows())
      ht <- rhandsontable::rhandsontable(d, rowHeaders = NULL, stretchH = "all",
        height = 420, selectCallback = TRUE)
      ht <- rhandsontable::hot_col(ht, cfg$pk, readOnly = TRUE, colWidths = 60)
      for (cc in cfg$cols) if (identical(cc$type, "int"))
        ht <- rhandsontable::hot_col(ht, cc$col, type = "numeric", format = "0")
      ht
    })
    observeEvent(input$hot$changes$changes, {
      chs <- input$hot$changes$changes; d <- rows(); changed <- FALSE
      for (ch in chs) {
        r <- ch[[1]] + 1L; cidx <- ch[[2]] + 1L
        if (identical(ch[[3]], ch[[4]])) next
        col <- names(d)[cidx]; pk_val <- d[[cfg$pk]][r]; val <- ch[[4]]
        if (identical(col_type(cfg, col), "int") && !is.null(val))
          val <- suppressWarnings(as.integer(val))
        db$update_cell(pk_val, col, val); d[r, col] <- if (is.null(val)) NA else val
        changed <- TRUE
      }
      if (changed) { rows(d); showNotification("Gemt", type = "message", duration = 1) }
    }, ignoreNULL = TRUE)

    # --- Fælles: ny / slet række ---------------------------------------------
    observeEvent(input$add_row, {
      db$add_row(); rows(db$list_rows()); refresh(refresh() + 1)
      showNotification("Ny række tilføjet — udfyld felterne", duration = 2)
    })
    observeEvent(input$delete, {
      r <- if (engine() == "rhot") {
        s <- input$hot_select; if (is.null(s$select$r)) NULL else s$select$r + 1L
      } else input$dt_rows_selected
      if (is.null(r) || length(r) == 0) {
        showNotification("Vælg en række først", type = "warning", duration = 2); return()
      }
      pk_val <- rows()[[cfg$pk]][r]; n <- db$ref_count(pk_val)
      if (n > 0) { showNotification(sprintf("Kan ikke slettes — i brug af %d post(er)", n),
        type = "error", duration = 4); return() }
      db$delete_row(pk_val); rows(db$list_rows()); refresh(refresh() + 1)
      showNotification("Slettet", duration = 2)
    })
  })
}

# --- Landing-side ------------------------------------------------------------
tile <- function(inputId, title, desc, icon = NULL, class = "btn-primary") {
  bslib::card(class = "h-100",
    bslib::card_body(
      h5(title, class = "mb-1"),
      p(desc, class = "text-muted small flex-grow-1"),
      actionButton(inputId, "Åbn ›", class = paste("btn-sm align-self-start", class))))
}

landing <- function() {
  tagList(
    div(class = "mt-3 mb-2", h5("Indikatorer", class = "text-uppercase text-primary",
      style = "font-size:.8rem;letter-spacing:.06em;")),
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      tile("go_indikatorer", "Indikatorer",
        "Fuld redigering: oversigt, modal, relationer.", class = "btn-primary")),
    div(class = "mt-4 mb-2", h5("Opslagstabeller", class = "text-uppercase text-primary",
      style = "font-size:.8rem;letter-spacing:.06em;")),
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      !!!lapply(LAB_TABLES, function(cfg)
        tile(paste0("go_", cfg$id), cfg$label,
          "Inline-redigering direkte i tabellen.", class = "btn-outline-primary")))
  )
}

# --- App ---------------------------------------------------------------------
ui <- bslib::page_navbar(id = "nav", title = "BFH Metadata",
  bslib::nav_panel("Start", value = "start", landing()),
  bslib::nav_panel("Indikatorer", value = "indikatorer",
    div(class = "text-muted mt-3", "(eksisterende indikator-CRUD — ikke i prototypen)")),
  bslib::nav_menu("Opslagstabeller",
    !!!lapply(LAB_TABLES, function(cfg)
      bslib::nav_panel(cfg$label, value = cfg$id, lab_table_ui(cfg$id, cfg))))
)

server <- function(input, output, session) {
  store <- make_fake_store()
  for (cfg in LAB_TABLES) lab_table_server(cfg$id, fake_lookup_db(store, cfg), cfg)
  # Landing-fliser → naviger
  observeEvent(input$go_indikatorer, bslib::nav_select("nav", "indikatorer"))
  for (cfg in LAB_TABLES) local({
    cc <- cfg
    observeEvent(input[[paste0("go_", cc$id)]], bslib::nav_select("nav", cc$id))
  })
}

shiny::runApp(shinyApp(ui, server), host = "127.0.0.1", port = 3901, launch.browser = TRUE)
