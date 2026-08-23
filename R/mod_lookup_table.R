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
        actionButton(ns("add_row"), "Ny r\u00E6kke", class = "btn-success btn-sm"),
        actionButton(ns("delete"), "Slet valgte r\u00E6kke", class = "btn-outline-danger btn-sm"))),
    p(class = "text-muted small",
      "Dobbeltklik en celle for at redigere. Klik en r\u00E6kke og tryk Slet. Id er l\u00E5st."),
    excelR::excelOutput(ns("tbl"), width = "100%", height = "auto")
  )
}

#' @noRd
mod_lookup_table_server <- function(id, db, cfg) {
  moduleServer(id, function(input, output, session) {
    rows <- reactiveVal(db$list_rows())
    refresh <- reactiveVal(0) # bump → re-render (ny/slet række + revert)
    status_msg <- reactiveVal("")
    sel_pk <- reactiveVal(NULL) # pk (chr) for senest valgte række
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
        columns = lookup_excel_columns(cfg, names(d), .fk_sources(), data = d),
        autoColTypes = FALSE,
        # autoWidth sætter width:auto inline på tabellen — det deaktiverer
        # table-layout:fixed, og så vinder celleindholdet over vores
        # beregnede kolonnebredder. FALSE → colgroup-bredderne styrer, og
        # lange værdier ombrydes i cellen (se .jexcel_theme_css).
        autoWidth = FALSE,
        # Række/kolonne-operationer styres af knapperne + DB — ikke af
        # grid'et. Kolonne-sortering er TILLADT (klik på overskriften):
        # diff og selektion er pk-baserede, så en klient-sorteret rækkefølge
        # er ufarlig. Sorteringen er ren visning og nulstilles ved re-render.
        allowInsertRow = FALSE, allowInsertColumn = FALSE,
        allowDeleteRow = FALSE, allowDeleteColumn = FALSE,
        allowRenameColumn = FALSE, columnSorting = TRUE,
        rowDrag = FALSE, columnDrag = FALSE,
        getSelectedData = TRUE
      )
    })

    # excelR sender BÅDE celle-ændringer og selektioner på input$tbl —
    # forSelectedVals skelner. Selektion: gem pk'en (læses fra payloadens
    # fullData, dvs. grid'ets aktuelle — evt. sorterede — rækkefølge).
    observeEvent(input$tbl, {
      p <- input$tbl
      if (isTRUE(p$forSelectedVals)) {
        sel_pk(excel_selected_pk(p))
      }
      # Diff også på selektions-payloads: markør-flytningen efter en
      # celle-commit overskriver change-eventet i Shinys input-batch
      # (se excel_event_df) — fullData bærer ændringen.
      new_df <- excel_event_df(p)
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
        ok <- safe_operation("opdat\u00E9r celle", {
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
      safe_operation("ny r\u00E6kke", {
        db$add_row()
        rows(db$list_rows())
        refresh(refresh() + 1)
        status_msg("Ny r\u00E6kke tilf\u00F8jet \u2014 udfyld felterne")
      }, fallback = status_msg("Fejl ved oprettelse (se log)"))
    })

    observeEvent(input$delete, {
      sel <- sel_pk()
      d <- rows()
      j <- if (is.null(sel)) NA_integer_ else match(sel, as.character(d[[cfg$pk]]))
      if (is.na(j)) {
        status_msg("V\u00E6lg en r\u00E6kke f\u00F8rst")
        return()
      }
      pk_val <- d[[cfg$pk]][j]
      # App-niveau ref-tjek (kun hvor DB ej enforcer FK)
      if (db$ref_count(pk_val) > 0) {
        status_msg("Kan ikke slettes \u2014 posten er i brug")
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
          "Kan ikke slettes \u2014 posten er i brug"
        } else {
          "Fejl ved sletning (se log)"
        })
        return()
      }
      # Fejl-tolerant genindlæsning: DB-udfald efter en gennemført sletning
      # må ikke vælte sessionen — behold senest hentede rækker
      safe_operation("genindl\u00E6s r\u00E6kker", rows(db$list_rows()),
        fallback = status_msg("Databasen svarer ikke \u2014 viser senest hentede data"))
      sel_pk(NULL) # rækken findes ikke længere — stale selektion må ej genbruges
      refresh(refresh() + 1)
      status_msg("Slettet")
    })

    # eksponér til test
    list(rows = rows, status_msg = status_msg, sel_pk = sel_pk)
  })
}
