# Generisk modul til inline-redigering af én simpel opslagstabel (Class A).
# Drevet af et LOOKUP_TABLES-cfg-element. Bruger DT editable celler (konsistent
# med indikator-oversigten). Genbruger safe_operation() fra mod_indikator_crud.R.

#' @noRd
mod_lookup_table_ui <- function(id, cfg) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex justify-content-between align-items-center mb-2 flex-wrap gap-2",
      h4(cfg$label, class = "m-0"),
      div(class = "d-flex gap-2",
        actionButton(ns("add_row"), "Ny række", class = "btn-success btn-sm"),
        actionButton(ns("delete"), "Slet valgte række", class = "btn-outline-danger btn-sm"))),
    p(class = "text-muted small",
      "Dobbeltklik en celle for at redigere. Vælg en række og tryk Slet. Id er låst."),
    DT::DTOutput(ns("tbl"))
  )
}

#' @noRd
mod_lookup_table_server <- function(id, db, cfg) {
  moduleServer(id, function(input, output, session) {
    rows <- reactiveVal(db$list_rows())
    refresh <- reactiveVal(0)        # bump → re-render (ny/slet række + revert)
    status_msg <- reactiveVal("")
    editable_cols <- vapply(cfg$cols, function(c) c$col, "")

    # Status som flydende notifikation (samme mønster som indikator-modulet)
    observeEvent(status_msg(), {
      m <- status_msg(); if (nzchar(m)) showNotification(m, duration = 3)
    }, ignoreInit = TRUE)

    output$tbl <- DT::renderDT({
      refresh()
      d <- isolate(rows())
      # Lås pk + alle ikke-editerbare kolonner (0-baseret, rownames=FALSE)
      disable <- which(!(names(d) %in% editable_cols)) - 1
      DT::datatable(d, rownames = FALSE, selection = "single",
        editable = list(target = "cell", disable = list(columns = disable)),
        options = list(dom = "t", paging = FALSE, scrollY = "420px",
                       scrollCollapse = TRUE))
    })

    observeEvent(input$tbl_cell_edit, {
      info <- input$tbl_cell_edit
      d <- rows()
      col <- names(d)[info$col + 1]
      pk_val <- d[[cfg$pk]][info$row]
      meta <- Find(function(c) c$col == col, cfg$cols)
      val <- info$value
      # Type-coercion: int-kolonne skal være et tal, ellers afvis + snap tilbage
      if (!is.null(meta) && identical(meta$type, "int")) {
        val <- suppressWarnings(as.integer(val))
        if (is.na(val) && nzchar(info$value)) {
          status_msg("Forventet et heltal"); refresh(refresh() + 1); return()
        }
      }
      if (identical(val, "")) val <- NA
      safe_operation("opdatér celle", {
        db$update_cell(pk_val, col, val)
        d[info$row, col] <- if (length(val) && is.na(val)) NA else val
        rows(d); status_msg("Gemt")
      }, fallback = { status_msg("Fejl ved gem (se log)"); refresh(refresh() + 1) })
    })

    observeEvent(input$add_row, {
      safe_operation("ny række", {
        db$add_row(); rows(db$list_rows()); refresh(refresh() + 1)
        status_msg("Ny række tilføjet — udfyld felterne")
      }, fallback = status_msg("Fejl ved oprettelse (se log)"))
    })

    observeEvent(input$delete, {
      sel <- input$tbl_rows_selected
      if (is.null(sel) || length(sel) == 0) { status_msg("Vælg en række først"); return() }
      pk_val <- rows()[[cfg$pk]][sel]
      # App-niveau ref-tjek (kun hvor DB ej enforcer FK)
      if (db$ref_count(pk_val) > 0) {
        status_msg("Kan ikke slettes — posten er i brug"); return()
      }
      # Ellers forsøg slet; DB-RESTRICT (FK) fanges og rapporteres pænt
      res <- tryCatch({ db$delete_row(pk_val); "ok" }, error = function(e) e)
      if (inherits(res, "error")) {
        msg <- conditionMessage(res)
        status_msg(if (grepl("foreign key|23503|violates", msg, ignore.case = TRUE))
          "Kan ikke slettes — posten er i brug" else "Fejl ved sletning (se log)")
        return()
      }
      rows(db$list_rows()); refresh(refresh() + 1); status_msg("Slettet")
    })

    # eksponér til test
    list(rows = rows, status_msg = status_msg)
  })
}
