# Generisk modul til inline-redigering af én opslags-/stamtabel.
# Drevet af et LOOKUP_TABLES-cfg-element. Redigering i et jspreadsheet-grid
# (excelR) — samme editor som biSPCharts: tekst/tal-celler redigeres direkte,
# FK-kolonner er native dropdowns ({id, name}-source: label vises, id gemmes).
# Ændringer ankommer som HELE tabellen; excel_diff_cells finder de redigerede
# celler via pk-match og skriver dem enkeltvis (db$update_cell). Genbruger
# safe_operation() fra mod_indikator_crud.R.

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
      "Dobbeltklik en celle for at redigere. Klik en række og tryk Slet. Id er låst."),
    excelR::excelOutput(ns("tbl"), width = "100%", height = "auto")
  )
}

#' @noRd
mod_lookup_table_server <- function(id, db, cfg) {
  moduleServer(id, function(input, output, session) {
    rows <- reactiveVal(db$list_rows())
    refresh <- reactiveVal(0) # bump → re-render (ny/slet række + revert)
    status_msg <- reactiveVal("")
    sel_row <- reactiveVal(NULL) # 1-baseret række fra seneste celle-selektion
    fk_cols <- Filter(function(c) identical(c$type, "fk"), cfg$cols)
    col_meta <- stats::setNames(cfg$cols, vapply(cfg$cols, function(c) c$col, ""))

    # Status som flydende notifikation (samme mønster som indikator-modulet)
    observeEvent(status_msg(), {
      m <- status_msg()
      if (nzchar(m)) showNotification(m, duration = 3)
    }, ignoreInit = TRUE)

    # FK-sources til dropdown-kolonner. NULL ved fejl → kolonnen låses som
    # readOnly text (se lookup_excel_columns) frem for en tom dropdown.
    .fk_sources <- function() {
      out <- lapply(fk_cols, function(fc) {
        safe_operation(paste0("hent fk-options (", fc$col, ")"),
          db$fk_options(fc$col),
          fallback = NULL
        )
      })
      stats::setNames(out, vapply(fk_cols, function(fc) fc$col, ""))
    }

    output$tbl <- excelR::renderExcel({
      refresh()
      d <- isolate(rows())
      excelR::excelTable(
        data = d,
        columns = lookup_excel_columns(cfg, names(d), .fk_sources()),
        autoColTypes = FALSE,
        # Række/kolonne-operationer styres af knapperne + DB — ikke af grid'et.
        # Sortering/drag er slået fra så grid-rækkefølgen ALTID matcher rows()
        # (sel_row er positionsbaseret).
        allowInsertRow = FALSE, allowInsertColumn = FALSE,
        allowDeleteRow = FALSE, allowDeleteColumn = FALSE,
        allowRenameColumn = FALSE, columnSorting = FALSE,
        rowDrag = FALSE, columnDrag = FALSE,
        getSelectedData = TRUE
      )
    })

    # excelR sender BÅDE celle-ændringer og selektioner på input$tbl —
    # forSelectedVals skelner. Selektion: gem 1-baseret række til Slet-knappen.
    observeEvent(input$tbl, {
      p <- input$tbl
      if (isTRUE(p$forSelectedVals)) {
        top <- p$selectedDataBoundary$borderTop
        sel_row(if (is.null(top)) NULL else as.integer(top) + 1L)
        return()
      }
      new_df <- excel_payload_to_df(p)
      d <- rows()
      changes <- excel_diff_cells(d, new_df, cfg$pk)
      if (nrow(changes) == 0) {
        return()
      }
      revert <- FALSE
      for (k in seq_len(nrow(changes))) {
        col <- changes$col[k]
        val <- changes$value[k]
        j <- match(changes$pk[k], as.character(d[[cfg$pk]]))
        pk_val <- d[[cfg$pk]][j]
        meta <- col_meta[[col]]
        # Type-koercion: int- og fk-kolonner skal være tal (fk-værdier er
        # parent-ids fra dropdown-sourcen). Ugyldigt tal → afvis + snap
        # tilbage til DB-tilstanden.
        if (!is.null(meta) && meta$type %in% c("int", "fk")) {
          coerced <- suppressWarnings(as.integer(val))
          if (!is.na(val) && is.na(coerced)) {
            status_msg("Forventet et heltal")
            revert <- TRUE
            next
          }
          val <- coerced
        }
        ok <- safe_operation("opdatér celle", {
          db$update_cell(pk_val, col, val)
          TRUE
        }, fallback = FALSE)
        if (isTRUE(ok)) {
          d[j, col] <- val
          status_msg("Gemt")
        } else {
          status_msg("Fejl ved gem (se log)")
          revert <- TRUE
        }
      }
      rows(d)
      # Én samlet re-render efter afviste celler: grid'et snapper tilbage til
      # den gemte tilstand (accepterede celler beholdes — de er i rows()).
      if (revert) refresh(refresh() + 1)
    })

    observeEvent(input$add_row, {
      safe_operation("ny række", {
        db$add_row()
        rows(db$list_rows())
        refresh(refresh() + 1)
        status_msg("Ny række tilføjet — udfyld felterne")
      }, fallback = status_msg("Fejl ved oprettelse (se log)"))
    })

    observeEvent(input$delete, {
      sel <- sel_row()
      d <- rows()
      if (is.null(sel) || is.na(sel) || sel < 1 || sel > nrow(d)) {
        status_msg("Vælg en række først")
        return()
      }
      pk_val <- d[[cfg$pk]][sel]
      # App-niveau ref-tjek (kun hvor DB ej enforcer FK)
      if (db$ref_count(pk_val) > 0) {
        status_msg("Kan ikke slettes — posten er i brug")
        return()
      }
      # Ellers forsøg slet; DB-RESTRICT (FK) fanges og rapporteres pænt
      res <- tryCatch({
        db$delete_row(pk_val)
        "ok"
      }, error = function(e) e)
      if (inherits(res, "error")) {
        msg <- conditionMessage(res)
        status_msg(if (grepl("foreign key|23503|violates", msg, ignore.case = TRUE)) {
          "Kan ikke slettes — posten er i brug"
        } else {
          "Fejl ved sletning (se log)"
        })
        return()
      }
      rows(db$list_rows())
      sel_row(NULL) # rækken findes ikke længere — stale selektion må ej genbruges
      refresh(refresh() + 1)
      status_msg("Slettet")
    })

    # eksponér til test
    list(rows = rows, status_msg = status_msg, sel_row = sel_row)
  })
}
