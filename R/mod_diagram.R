# Diagram-CRUD (admin): filterbar oversigt over ALLE diagrammer + delt
# formular-modal. Formularen (.diagram_form_ui) genbruges af indikator-modalen
# (lock_indikator = TRUE) — deraf ren funktion frem for modul-intern UI.

#' Delt formular-UI for diagram. vals = named list (NULL → ny), opts =
#' list(indikator=df, org=df, type=df, periode=chr). Returnerer tagList —
#' kalderen wrapper i modalDialog.
#' @noRd
.diagram_indicator_initial_choices <- function(indicators, selected = NULL) {
  selected <- suppressWarnings(as.integer(selected))
  empty <- c("(vælg)" = "")
  if (length(selected) != 1L || is.na(selected)) return(empty)

  idx <- match(selected, indicators$id)
  label <- if (is.na(idx)) {
    sprintf("Ukendt indikator #%d", selected)
  } else {
    as.character(indicators$label[[idx]])
  }
  c(empty, stats::setNames(as.character(selected), label))
}

#' @noRd
.diagram_indicator_choices <- function(indicators) {
  stats::setNames(as.character(indicators$id), as.character(indicators$label))
}

#' @noRd
.update_diagram_indicator <- function(session, indicators, selected = "") {
  choices <- .diagram_indicator_choices(indicators)
  selected_choice <- .diagram_indicator_initial_choices(indicators, selected)
  selected_choice <- selected_choice[unname(selected_choice) != ""]
  if (length(selected_choice) > 0L &&
      !unname(selected_choice) %in% unname(choices)) {
    choices <- c(choices, selected_choice)
  }
  updateSelectizeInput(session, "d_indikator",
    choices = choices, selected = selected,
    server = TRUE)
}

#' @noRd
.diagram_form_ui <- function(ns, vals = NULL, opts, lock_indikator = FALSE) {
  is_new <- is.null(vals)
  v <- function(col, default = NULL) if (is_new) default else vals[[col]]
  ch <- function(d) stats::setNames(d$id, d$label)

  ind_value <- v("indikator")
  ind_choices <- .diagram_indicator_initial_choices(opts$indikator, ind_value)
  ind_sel <- selectizeInput(ns("d_indikator"), "Indikator",
    choices = ind_choices, selected = ind_value %||% "")
  if (lock_indikator) {
    # Låst: disabled inputs sender ikke værdi til Shiny → send via skjult
    # select og vis navnet i et disabled tekstfelt i stedet.
    locked_choices <- ind_choices[unname(ind_choices) != ""]
    ind_sel <- selectizeInput(ns("d_indikator"), "Indikator",
      choices = locked_choices, selected = ind_value %||% "")
    lbl <- if (length(locked_choices) == 0L) "" else names(locked_choices)[[1]]
    vis <- textInput(ns("d_indikator_vis"), "Indikator", value = lbl)
    vis <- htmltools::tagQuery(vis)$find("input")$addAttrs(disabled = NA)$allTags()
    ind_sel <- tagList(div(style = "display:none", ind_sel), vis)
  }

  tagList(
    ind_sel,
    selectInput(ns("d_organisatorisk_navn_teknisk"), "Organisatorisk enhed",
      choices = c("(vælg)" = "", ch(opts$org)),
      selected = v("organisatorisk_navn_teknisk") %||% ""),
    selectInput(ns("d_diagram_type"), "Diagramtype",
      choices = c("(vælg)" = "", ch(opts$type)),
      selected = v("diagram_type") %||% ""),
    selectInput(ns("d_periode_aggregering"), "Periode-aggregering",
      choices = c("(ingen)" = "", opts$periode),
      selected = v("periode_aggregering") %||% ""),
    div(class = "d-flex flex-wrap gap-4 pt-1",
      checkboxInput(ns("d_indgaar_i_aggregering"), "Indgår i aggregering",
        value = isTRUE(v("indgaar_i_aggregering"))),
      checkboxInput(ns("d_diagram_aktivt"), "Diagram aktivt",
        value = isTRUE(v("diagram_aktivt", default = TRUE))),
      checkboxInput(ns("d_direktionens_tavle"), "Direktionens tavle",
        value = isTRUE(v("direktionens_tavle")))))
}

#' Saml formular-inputs → named list i DIAGRAM_COLS-orden. Tomme selects → NA.
#' @noRd
.collect_diagram_form <- function(input, prefix = "d_") {
  gv <- function(col) input[[paste0(prefix, col)]]
  int_or_na <- function(x) {
    if (is.null(x) || identical(x, "")) NA_integer_ else as.integer(x)
  }
  chr_or_na <- function(x) {
    if (is.null(x) || identical(x, "")) NA_character_ else as.character(x)
  }
  list(
    indikator = int_or_na(gv("indikator")),
    organisatorisk_navn_teknisk = int_or_na(gv("organisatorisk_navn_teknisk")),
    diagram_type = int_or_na(gv("diagram_type")),
    periode_aggregering = chr_or_na(gv("periode_aggregering")),
    indgaar_i_aggregering = isTRUE(gv("indgaar_i_aggregering")),
    diagram_aktivt = isTRUE(gv("diagram_aktivt")),
    direktionens_tavle = isTRUE(gv("direktionens_tavle")))
}

#' @noRd
mod_diagram_ui <- function(id) {
  ns <- NS(id)
  div(class = "mt-2",
    div(class = "d-flex justify-content-end mb-2",
      actionButton(ns("new_diagram"), "Nyt diagram", class = "btn-success")),
    bslib::layout_columns(
      col_widths = c(3, 3, 2, 2, 2),
      uiOutput(ns("filter_indikator_ui")),
      uiOutput(ns("filter_org_ui")),
      uiOutput(ns("filter_datapakke_ui")),
      uiOutput(ns("filter_datasaet_ui")),
      selectInput(ns("filter_status"), "Status",
        choices = c("Kun aktive" = "aktive", "Alle" = "alle",
                    "Kun inaktive" = "inaktive"),
        selected = "aktive")),
    DT::DTOutput(ns("tbl")))
}

#' @noRd
mod_diagram_server <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    admin <- reactiveVal(db$list_diagrams_admin())
    opts <- db$diagram_form_options()
    opts$periode <- db$diagram_periode_choices()
    status_msg <- reactiveVal("")
    warn_msg <- reactiveVal("")
    editing_id <- reactiveVal(NULL)
    reload <- function() admin(db$list_diagrams_admin())

    # Flydende notifikationer (synlige over modal, jf. mod_indikator_crud)
    observeEvent(status_msg(), {
      if (nzchar(status_msg())) showNotification(status_msg(), duration = 5)
    }, ignoreInit = TRUE)
    observeEvent(warn_msg(), {
      if (nzchar(warn_msg()))
        showNotification(warn_msg(), type = "warning", duration = 8)
    }, ignoreInit = TRUE)

    # Filter-valg afledt af admin-data (kun værdier der faktisk findes)
    .filter_ui <- function(input_id, lab, col) {
      ns <- session$ns
      vals <- sort(unique(stats::na.omit(admin()[[col]])))
      choices <- c("Alle" = "", stats::setNames(vals, vals))
      selected <- .preserved_filter_selection(
        isolate(input[[input_id]]), choices)
      selectInput(ns(input_id), lab, choices = choices, selected = selected)
    }
    output$filter_indikator_ui <- renderUI(
      .filter_ui("filter_indikator", "Indikator", "indikator_navn"))
    output$filter_org_ui <- renderUI(
      .filter_ui("filter_org", "Organisatorisk enhed", "org_navn"))
    output$filter_datapakke_ui <- renderUI(
      .filter_ui("filter_datapakke", "Datapakke", "datapakke"))
    output$filter_datasaet_ui <- renderUI(
      .filter_ui("filter_datasaet", "Datasæt", "datasaet"))

    filtered <- reactive({
      d <- admin()
      status <- input$filter_status %||% "aktive"
      if (identical(status, "aktive"))
        d <- d[d$diagram_aktivt %in% TRUE, , drop = FALSE]
      if (identical(status, "inaktive"))
        d <- d[!(d$diagram_aktivt %in% TRUE), , drop = FALSE]
      fi <- input$filter_indikator
      if (!is.null(fi) && nzchar(fi))
        d <- d[d$indikator_navn %in% fi, , drop = FALSE]
      fo <- input$filter_org
      if (!is.null(fo) && nzchar(fo)) d <- d[d$org_navn %in% fo, , drop = FALSE]
      fdp <- input$filter_datapakke
      if (!is.null(fdp) && nzchar(fdp))
        d <- d[d$datapakke %in% fdp, , drop = FALSE]
      fds <- input$filter_datasaet
      if (!is.null(fds) && nzchar(fds))
        d <- d[d$datasaet %in% fds, , drop = FALSE]
      d
    })

    output$tbl <- DT::renderDT({
      d <- filtered()
      ns <- session$ns
      flag <- function(x) ifelse(x %in% TRUE,
        '<span style="color:#198754;font-weight:700;">&#10003;</span>',
        '<span style="color:#adb5bd;">&mdash;</span>')
      btn <- sprintf(paste0(
        '<button class="btn btn-outline-secondary btn-sm" ',
        'onclick="Shiny.setInputValue(\'%s\', %d, {priority: \'event\'})">',
        'Åbn &rsaquo;</button>'), ns("open_id"), d$diagram_id)
      out <- data.frame(
        ` ` = btn,
        Indikator = d$indikator_navn,
        Enhed = d$org_navn,
        Type = d$type_navn,
        Periode = d$periode_aggregering,
        Aggregering = flag(d$indgaar_i_aggregering),
        Aktiv = flag(d$diagram_aktivt),
        Tavle = flag(d$direktionens_tavle),
        check.names = FALSE, stringsAsFactors = FALSE)
      # Knap/flag-kolonner indeholder bevidst HTML; escape tekstkolonner (XSS)
      esc <- which(names(out) %in% c("Indikator", "Enhed", "Type", "Periode"))
      DT::datatable(out, escape = esc, rownames = FALSE, selection = "none",
        options = list(stateSave = TRUE, stateDuration = -1,
                       pageLength = 15, columnDefs = list(
          list(orderable = FALSE, targets = 0))))
    })

    .show_form_modal <- function(vals = NULL) {
      ns <- session$ns
      is_new <- is.null(vals)
      showModal(modalDialog(
        title = if (is_new) "Nyt diagram" else "Redigér diagram",
        size = "m", easyClose = FALSE,
        .diagram_form_ui(ns, vals, opts),
        footer = div(class = "d-flex justify-content-between w-100",
          if (is_new) span() else
            actionButton(ns("d_delete"), "Slet", class = "btn-outline-danger"),
          div(class = "d-flex gap-2",
            modalButton("Annullér"),
            actionButton(ns("d_save"), "Gem", class = "btn-primary")))))
      session$onFlushed(function() {
        .update_diagram_indicator(session, opts$indikator,
                                  if (is_new) "" else vals$indikator)
      }, once = TRUE)
    }

    observeEvent(input$open_id, {
      rid <- as.integer(input$open_id)
      row <- admin()[admin()$diagram_id == rid, , drop = FALSE]
      if (nrow(row) == 0) { status_msg("Diagram ikke fundet"); return() }
      editing_id(rid)
      .show_form_modal(as.list(row[1, , drop = FALSE]))
    })

    observeEvent(input$new_diagram, {
      editing_id(NULL)
      .show_form_modal(NULL)
    })

    observeEvent(input$d_save, {
      vals <- .collect_diagram_form(input)
      errs <- validate_diagram(vals)
      if (length(errs) > 0) { status_msg(paste(errs, collapse = "; ")); return() }
      rid <- editing_id()
      # Blød duplikat-guard: advar men blokér ikke (bevidste dubletter findes)
      dup <- db$diagram_duplicate_count(vals$indikator,
        vals$organisatorisk_navn_teknisk, vals$diagram_type,
        exclude_id = rid %||% -1L)
      if (dup > 0) {
        warn_msg(paste("Findes allerede: samme indikator/enhed/type har",
                       "et diagram i forvejen."))
      }
      safe_operation("diagram-gem", {
        if (is.null(rid)) {
          newid <- db$create_diagram(vals)
          status_msg(paste("Oprettet diagram", newid))
        } else {
          db$update_diagram(rid, vals)
          status_msg(paste("Gemt diagram", rid))
        }
        removeModal(); reload()
      }, fallback = status_msg("Fejl ved gem af diagram (se log)"))
    })

    observeEvent(input$d_delete, {
      rid <- editing_id()
      if (is.null(rid)) return()
      # Pre-check: median-knæk gør sletning destruktiv → venlig blokering
      n <- db$diagram_median_count(rid)
      if (n > 0) {
        warn_msg(sprintf(paste("Diagrammet har %d median-knæk — deaktivér i",
                               "stedet, eller slet knækkene først."), n))
        return()
      }
      safe_operation("diagram-slet", {
        db$delete_diagram(rid)
        status_msg(paste("Slettet diagram", rid))
        removeModal(); editing_id(NULL); reload()
      }, fallback = status_msg("Fejl ved sletning (se log)"))
    })

    # eksponér til test
    list(admin = admin, filtered = filtered, status_msg = status_msg,
         warn_msg = warn_msg, editing_id = editing_id)
  })
}
