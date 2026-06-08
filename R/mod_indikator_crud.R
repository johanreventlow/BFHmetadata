#' Bygger ét form-input baseret på felt-kind. prefix giver distinkt id-rum
#' (modal vs sidebar). values pre-udfylder.
#' @noRd
.field_input <- function(ns, f, fk_choices = list(), values = list(), prefix = "") {
  id <- ns(paste0(prefix, f$col))
  v <- values[[f$col]]
  switch(f$kind,
    "pk"       = NULL,
    "fk"       = selectInput(id, f$col, choices = c("(ingen)" = "", fk_choices[[f$col]]),
                             selected = v %||% ""),
    "bool"     = checkboxInput(id, f$col, value = isTRUE(v)),
    "date"     = dateInput(id, f$col,
                           value = if (is.null(v) || is.na(v)) NULL else as.Date(v)),
    "int"      = numericInput(id, f$col, value = if (is.null(v)) NA else v),
    "textarea" = textAreaInput(id, f$col, value = v %||% ""),
    textInput(id, f$col, value = v %||% "")  # text (default)
  )
}

#' @noRd
mod_indikator_crud_ui <- function(id) {
  ns <- NS(id)
  bslib::navset_tab(
    bslib::nav_panel("Oversigt",
      div(class = "mt-2",
        checkboxInput(ns("show_inactive_ov"), "Vis inaktive", value = TRUE),
        DT::DTOutput(ns("oversigt")),
        verbatimTextOutput(ns("status"))
      )
    ),
    bslib::nav_panel("Inline-redigering",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 420, position = "right", open = TRUE,
          h5("Redigér / opret"),
          uiOutput(ns("form")),
          div(class = "d-flex gap-2 mt-2",
            actionButton(ns("new"), "Ny", class = "btn-secondary"),
            actionButton(ns("save"), "Gem", class = "btn-primary"),
            actionButton(ns("soft_delete"), "Deaktivér", class = "btn-warning")
          )
        ),
        div(
          checkboxInput(ns("show_inactive"), "Vis inaktive", value = TRUE),
          DT::DTOutput(ns("tbl"))
        )
      )
    )
  )
}

#' @noRd
.collect_form <- function(input, fields, prefix = "") {
  vals <- list()
  for (f in fields) {
    if (f$kind == "pk") next
    v <- input[[paste0(prefix, f$col)]]
    if (f$kind == "bool") v <- isTRUE(v)
    if (f$kind %in% c("text", "textarea", "fk") && identical(v, "")) v <- NA
    vals[[f$col]] <- v
  }
  vals
}

#' @noRd
mod_indikator_crud_server <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    rows <- reactiveVal(db$list_indikatorer())
    status_msg <- reactiveVal("")
    fk <- db$fk_options()
    fk_choices <- lapply(fk, function(d) stats::setNames(d$id, d$label))

    reload <- function() rows(db$list_indikatorer())

    editing_id <- reactiveVal(NULL)

    # Bygger modal-indhold for én indikator (pre-udfyldt form + m2m-multiselect)
    .build_modal <- function(row) {
      ns <- session$ns
      vals <- as.list(row)
      scalar_fk <- tagList(lapply(INDIKATOR_FIELDS, function(f)
        .field_input(ns, f, fk_choices, values = vals, prefix = "m_")))
      m2m <- lapply(names(INDIKATOR_JUNCTIONS), function(key) {
        opts <- db$junction_options(key)
        sel <- db$get_junction(vals$id, key)
        selectInput(ns(paste0("m_j_", key)), key,
          choices = stats::setNames(opts$id, opts$label),
          selected = sel, multiple = TRUE)
      })
      modalDialog(title = paste("Redigér indikator", vals$id), size = "l",
        easyClose = FALSE,
        scalar_fk, hr(), h5("Relationer"), tagList(m2m),
        footer = tagList(
          actionButton(ns("modal_save"), "Gem", class = "btn-primary"),
          modalButton("Annullér")))
    }

    observeEvent(input$open_id, {
      rid <- as.integer(input$open_id)
      editing_id(rid)
      row <- rows()[rows()[["id"]] == rid, , drop = FALSE]
      if (nrow(row) == 0) { status_msg("Indikator ikke fundet"); return() }
      showModal(.build_modal(row[1, , drop = FALSE]))
    })

    observeEvent(input$modal_save, {
      rid <- editing_id()
      vals <- .collect_form(input, INDIKATOR_FIELDS, prefix = "m_")
      errs <- validate_indikator(vals)
      if (length(errs) > 0) { status_msg(paste(errs, collapse = "; ")); return() }
      safe_operation("modal-gem", {
        db$update_indikator(rid, vals)
        for (key in names(INDIKATOR_JUNCTIONS)) {
          picked <- as.integer(input[[paste0("m_j_", key)]])
          db$set_junction(rid, key, picked)
        }
        removeModal(); status_msg(paste("Gemt indikator", rid)); reload()
      }, fallback = status_msg("Fejl ved modal-gem (se log)"))
    })

    output$form <- renderUI({
      ns <- session$ns
      tagList(lapply(INDIKATOR_FIELDS, function(f) .field_input(ns, f, fk_choices)))
    })

    output$tbl <- DT::renderDT({
      d <- rows()
      if (!isTRUE(input$show_inactive) && "aktiv_indikator" %in% names(d))
        d <- d[d$aktiv_indikator %in% TRUE, , drop = FALSE]
      editable_cols <- which(names(d) %in% INLINE_EDITABLE) - 1
      DT::datatable(d, selection = "single", rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = setdiff(seq_len(ncol(d))-1, editable_cols))))
    })

    output$oversigt <- DT::renderDT({
      ns <- session$ns
      d <- rows()
      if (!isTRUE(input$show_inactive_ov) && "aktiv_indikator" %in% names(d))
        d <- d[d$aktiv_indikator %in% TRUE, , drop = FALSE]
      btn <- vapply(d[["id"]], function(i) sprintf(
        '<button class="btn btn-sm btn-primary" onclick="Shiny.setInputValue(\'%s\', %d, {priority:\'event\'})">Åbn</button>',
        ns("open_id"), i), "")
      out <- data.frame(
        Aktiv = ifelse(d[["aktiv_indikator"]] %in% TRUE, "✓", "✗"),
        Datasæt = d[["label_indikator_hierarki"]],
        Id = d[["id"]],
        Navn = d[["indikator_navn"]],
        Handling = btn,
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(out, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(pageLength = 25))
    })

    selected_id <- reactive({
      sel <- input$tbl_rows_selected
      if (is.null(sel)) return(NULL)
      rows()[["id"]][sel]
    })

    observeEvent(input$save, {
      vals <- .collect_form(input, INDIKATOR_FIELDS)
      errs <- validate_indikator(vals)
      if (length(errs) > 0) { status_msg(paste(errs, collapse = "; ")); return() }
      safe_operation("gem indikator", {
        sid <- selected_id()
        if (is.null(sid)) {
          newid <- db$create_indikator(vals); status_msg(paste("Oprettet id", newid))
        } else {
          db$update_indikator(sid, vals); status_msg("Gemt")
        }
        reload()
      }, fallback = status_msg("Fejl ved gem (se log)"))
    })

    observeEvent(input$soft_delete, {
      sid <- selected_id()
      if (is.null(sid)) { status_msg("Vælg en række først"); return() }
      safe_operation("soft-delete", {
        db$soft_delete(sid, active = FALSE); status_msg("Deaktiveret"); reload()
      }, fallback = status_msg("Fejl ved deaktivering"))
    })

    observeEvent(input$tbl_cell_edit, {
      info <- input$tbl_cell_edit
      d <- rows(); col <- names(d)[info$col + 1]
      if (!col %in% INLINE_EDITABLE) { status_msg("Kolonne ej redigerbar inline"); return() }
      rid <- d[["id"]][info$row]
      safe_operation("inline-update", {
        db$update_indikator(rid, stats::setNames(list(info$value), col))
        status_msg(paste("Opdateret", col)); reload()
      }, fallback = status_msg("Fejl ved inline-update"))
    })

    output$status <- renderText(status_msg())

    # eksponér til test
    list(rows = rows, status_msg = status_msg, editing_id = editing_id)
  })
}

#' Minimal safe_operation (logger + fallback)
#' @noRd
safe_operation <- function(op, code, fallback = NULL) {
  tryCatch(force(code), error = function(e) {
    message(sprintf("[ERROR] %s: %s", op, conditionMessage(e)))
    force(fallback)
  })
}
