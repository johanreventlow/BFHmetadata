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
    scanned_n <- reactiveVal(NULL)        # vindue-værdi (NULL/int) brugt ved SIDSTE scan

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

    # Cache-nøgle af en vindue-værdi. Bruges med scanned_n() efter scan, så et
    # senere vindue-skifte (uden re-scan) ikke gør cache-opslaget stale (C1).
    .wkey <- function(n) if (is.null(n)) "all" else as.character(n)

    observeEvent(input$scan, {
      base <- input$parquet_dir
      if (is.null(base) || !nzchar(base) || !dir.exists(base)) {
        showNotification("Angiv en eksisterende parquet-mappe", type = "warning")
        return()
      }
      cand <- apply_index_filters(index(), current_filters())
      if (nrow(cand) == 0) { showNotification("Ingen diagrammer matcher filtrene"); return() }
      wn <- window_n(); wk <- .wkey(wn); vdf <- variants(); cc <- cache()
      results <- list()
      withProgress(message = "Scanner diagrammer", value = 0, {
        n <- nrow(cand)
        for (i in seq_len(n)) {
          row <- as.list(cand[i, ])
          key <- paste0(row$diagram_id, "|", wk)
          res <- cc[[key]]
          if (is.null(res)) {
            meds <- db$diagram_medians(row$diagram_id)
            res <- scan_diagram(row, base, meds, vdf, window_n = wn)
            res$row <- row
            cc[[key]] <- res
          }
          results[[length(results) + 1]] <- res
          incProgress(1 / n, detail = sprintf("%d/%d", i, n))
        }
      })
      cache(cc)
      # results er 1:1 positionelt med cand (ingen skip i loopet) → positionelt
      # subset er korrekt + robust mod dublerede diagram_id (I1).
      sig_ids <- vapply(results, function(r) isTRUE(r$signal), logical(1))
      sl <- cand[sig_ids, , drop = FALSE]
      signal_list(sl)
      scanned_n(wn)          # vindue låst til denne scan (bruges af .scan_of_current/Task 7)
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
      # Brug vinduet fra SCAN-tidspunktet (scanned_n), ej live window_n — ellers
      # gør et vindue-skifte efter scan cache-opslaget stale → blank graf (C1).
      cache()[[paste0(cd$diagram_id, "|", .wkey(scanned_n()))]]
    })

    observeEvent(input$next_, {
      sl <- signal_list(); if (is.null(sl) || nrow(sl) == 0) return()
      cursor(min(cursor() + 1L, nrow(sl))); preview_parts(NULL)
    })
    observeEvent(input$prev, {
      sl <- signal_list(); if (is.null(sl) || nrow(sl) == 0) return()
      cursor(max(cursor() - 1L, 1L)); preview_parts(NULL)
    })

    output$nav_label <- renderUI({
      sl <- signal_list()
      if (is.null(sl)) return(span("Ingen scan endnu", class = "text-muted"))
      if (nrow(sl) == 0) return(span("Scannet — 0 diagrammer med signal", class = "text-muted"))
      cd <- current_diagram()
      strong(sprintf("%d/%d — %s · %s", cursor(), nrow(sl),
                     cd$indikator_navn, cd$org_navn))
    })

    output$scan_summary <- renderUI({
      sl <- signal_list(); if (is.null(sl)) return(NULL)
      div(class = "small text-muted mt-2", sprintf("%d med signal", nrow(sl)))
    })

    # Cursor-stemplet valg: returnér KUN den valgte dato hvis klikket skete på
    # det diagram der vises NU. ggiraph-input kan ikke nulstilles fra server, så
    # uden dette stempel ville et valg fra forrige diagram læses som gyldigt på
    # det aktuelle → faseskift på FORKERT diagram (korrekthedsfejl, ikke nit).
    valid_selected_date <- reactive({
      sel <- input$chart_selected
      if (is.null(sel) || !nzchar(sel) || !identical(selected_cursor(), cursor()))
        return(NULL)
      sel
    })

    # Ryd diagrammets cache-nøgler + re-scan det scannede vindue, så grafen
    # opdateres i stedet for at gå blank efter en knæk-ændring (gem/fjern).
    .refresh_diagram <- function(cd) {
      cc <- cache()
      cc[grepl(paste0("^", cd$diagram_id, "\\|"), names(cc))] <- NULL
      meds <- db$diagram_medians(cd$diagram_id)
      cc[[paste0(cd$diagram_id, "|", .wkey(scanned_n()))]] <-
        c(scan_diagram(as.list(cd), input$parquet_dir, meds, variants(),
                       window_n = scanned_n()), list(row = as.list(cd)))
      cache(cc)
    }

    # --- Graf -------------------------------------------------------------
    output$chart <- ggiraph::renderGirafe({
      sc <- .scan_of_current(); if (is.null(sc) || is.null(sc$qic_result)) return(NULL)
      qr <- sc$qic_result
      # Forhåndsvis: re-beregn med ekstra knæk hvis valgt + gyldigt. Date-
      # normaliseret via preview_break_parts (ingen rbind af Date på POSIXct).
      pv <- preview_parts()
      if (!is.null(pv) && !is.null(sc$slice)) {
        base_meds <- db$diagram_medians(current_diagram()$diagram_id)
        parts <- preview_break_parts(current_diagram()$diagram_id, base_meds,
                                     pv, sc$slice$dato)
        qr <- compute_signal(sc$slice, parts = parts)$qic_result
      }
      interactive_run_chart(qr, selected_date = valid_selected_date())
    })

    output$selected_label <- renderUI({
      sel <- valid_selected_date()   # stale valg fra andet diagram → "klik en obs"
      if (is.null(sel)) return(span("Klik en observation", class = "text-muted"))
      span(sprintf("Valgt: %s", sel), class = "fw-bold")
    })

    # --- Forhåndsvis ------------------------------------------------------
    observeEvent(input$preview, {
      sel <- valid_selected_date()
      sc <- .scan_of_current()
      if (is.null(sel) || is.null(sc) || is.null(sc$slice)) {
        showNotification("Vælg en observation på dette diagram", type = "warning"); return()
      }
      parts <- resolve_median_breaks(current_diagram()$diagram_id,
        data.frame(diagram = current_diagram()$diagram_id,
                   laas_median = as.Date(sel)), sc$slice$dato)
      if (is.null(parts)) {
        showNotification("Kan ikke lave faseskift her (første/ugyldig observation)",
                         type = "warning"); return()
      }
      preview_parts(sel)
    })

    # --- Gem faseskift ----------------------------------------------------
    observeEvent(input$save_break, {
      sel <- valid_selected_date()
      cd <- current_diagram(); sc <- .scan_of_current()
      if (is.null(sel) || is.null(cd) || is.null(sc$slice)) {
        showNotification("Vælg en observation på dette diagram", type = "warning"); return()
      }
      # Valider at det er et lovligt knæk (ikke første obs / uden for data)
      parts <- resolve_median_breaks(cd$diagram_id,
        data.frame(diagram = cd$diagram_id, laas_median = as.Date(sel)),
        sc$slice$dato)
      if (is.null(parts)) {
        showNotification("Kan ikke lave faseskift her (første/ugyldig observation)",
                         type = "warning"); return()
      }
      ok <- safe_operation("gem faseskift", {
        db$add_median_break(cd$diagram_id, as.Date(sel)); TRUE
      }, fallback = FALSE)
      if (!isTRUE(ok)) { showNotification("Fejl ved gem (se log)", type = "error"); return() }
      .refresh_diagram(cd)   # invalidér + re-scan → graf viser ny signal-status
      preview_parts(NULL)
      showNotification("Faseskift gemt")
    })

    # --- Eksisterende median-knæk + fjern ---------------------------------
    output$breaks_tbl <- DT::renderDT({
      cd <- current_diagram(); if (is.null(cd)) return(DT::datatable(data.frame()))
      m <- db$diagram_medians(cd$diagram_id)
      DT::datatable(m[, intersect(c("id", "laas_median"), names(m)), drop = FALSE],
        rownames = FALSE, selection = "single",
        options = list(dom = "t", paging = FALSE))
    })

    observeEvent(input$delete_break, {
      cd <- current_diagram(); if (is.null(cd)) return()
      sel <- input$breaks_tbl_rows_selected
      if (is.null(sel)) { showNotification("Vælg et knæk først", type = "warning"); return() }
      m <- db$diagram_medians(cd$diagram_id)
      mid <- m$id[sel]
      ok <- safe_operation("fjern faseskift", {
        db$delete_median_break(mid); TRUE
      }, fallback = FALSE)
      if (!isTRUE(ok)) { showNotification("Fejl ved fjern (se log)", type = "error"); return() }
      .refresh_diagram(cd)   # re-scan så grafen opdateres (ej blank) efter fjern
      preview_parts(NULL)
      showNotification("Knæk fjernet")
    })

    # Eksponér til test
    list(signal_list = signal_list, current_diagram = current_diagram,
         cursor = cursor, cache = cache, preview_parts = preview_parts,
         scan_of_current = .scan_of_current)
  })
}
