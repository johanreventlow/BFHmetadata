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

#' @noRd
mod_signal_review_server <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    index <- reactiveVal(db$list_active_seriediagrammer())
    variants <- reactiveVal(db$org_enhed_variants())
    cache <- reactiveVal(list())          # nøgle "<diagram_id>|<window>" → scan-res
    signal_list <- reactiveVal(NULL)      # df: diagrammer med signal
    cursor <- reactiveVal(1L)
    preview_parts <- reactiveVal(NULL)    # ekstra forhåndsvist knæk (dato)
    selected_cursor <- reactiveVal(NULL)  # cursor-stempel da punkt sidst blev klikket

    # ggiraph-input kan ikke nulstilles fra server → stempl med cursor ved klik,
    # så et valg fra ET diagram aldrig læses som gyldigt på et ANDET (Task 7-guard).
    observeEvent(input$chart_selected, selected_cursor(cursor()))

    # Populér filter-valg ved start
    observeEvent(index(), {
      ch <- index_filter_choices(index())
      for (dim in names(ch)) {
        updateSelectizeInput(session, paste0("f_", dim),
          choices = c("(alle)" = "", ch[[dim]]), server = FALSE)
      }
    }, once = TRUE)

    current_filters <- reactive(list(
      overafdeling = input$f_overafdeling, afsnit = input$f_afsnit,
      datapakke = input$f_datapakke, datasaet = input$f_datasaet,
      indikator_navn = input$f_indikator_navn))

    window_n <- reactive(
      if (identical(input$window_mode, "latest")) as.integer(input$window_n) else NULL)

    .window_key <- reactive(if (is.null(window_n())) "all" else as.character(window_n()))

    observeEvent(input$scan, {
      base <- input$parquet_dir
      if (is.null(base) || !nzchar(base) || !dir.exists(base)) {
        showNotification("Angiv en eksisterende parquet-mappe", type = "warning")
        return()
      }
      cand <- apply_index_filters(index(), current_filters())
      if (nrow(cand) == 0) { showNotification("Ingen diagrammer matcher filtrene"); return() }
      wk <- .window_key(); vdf <- variants(); cc <- cache()
      results <- list()
      withProgress(message = "Scanner diagrammer", value = 0, {
        n <- nrow(cand)
        for (i in seq_len(n)) {
          row <- as.list(cand[i, ])
          key <- paste0(row$diagram_id, "|", wk)
          res <- cc[[key]]
          if (is.null(res)) {
            meds <- db$diagram_medians(row$diagram_id)
            res <- scan_diagram(row, base, meds, vdf, window_n = window_n())
            res$row <- row
            cc[[key]] <- res
          }
          results[[length(results) + 1]] <- res
          incProgress(1 / n, detail = sprintf("%d/%d", i, n))
        }
      })
      cache(cc)
      sig_ids <- vapply(results, function(r) isTRUE(r$signal), logical(1))
      sl <- cand[cand$diagram_id %in%
                   vapply(results[sig_ids], function(r) r$diagram_id, integer(1)), ,
                 drop = FALSE]
      signal_list(sl)
      cursor(1L)
      preview_parts(NULL)
      showNotification(sprintf("%d af %d diagrammer har signal", nrow(sl), nrow(cand)))
    })

    current_diagram <- reactive({
      sl <- signal_list()
      if (is.null(sl) || nrow(sl) == 0) return(NULL)
      as.list(sl[cursor(), ])
    })

    .scan_of_current <- reactive({
      cd <- current_diagram(); if (is.null(cd)) return(NULL)
      cache()[[paste0(cd$diagram_id, "|", .window_key())]]
    })

    observeEvent(input$next_, {
      sl <- signal_list(); if (is.null(sl) || nrow(sl) == 0) return()
      cursor(min(cursor() + 1L, nrow(sl))); preview_parts(NULL)
    })
    observeEvent(input$prev, {
      if (is.null(signal_list())) return()
      cursor(max(cursor() - 1L, 1L)); preview_parts(NULL)
    })

    output$nav_label <- renderUI({
      sl <- signal_list()
      if (is.null(sl) || nrow(sl) == 0) return(span("Ingen scan endnu", class = "text-muted"))
      cd <- current_diagram()
      strong(sprintf("%d/%d — %s · %s", cursor(), nrow(sl),
                     cd$indikator_navn, cd$org_navn))
    })

    output$scan_summary <- renderUI({
      sl <- signal_list(); if (is.null(sl)) return(NULL)
      div(class = "small text-muted mt-2", sprintf("%d med signal", nrow(sl)))
    })

    # Eksponér til test
    list(signal_list = signal_list, current_diagram = current_diagram,
         cursor = cursor, cache = cache, preview_parts = preview_parts,
         scan_of_current = .scan_of_current)
  })
}
