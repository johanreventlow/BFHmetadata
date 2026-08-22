# Generisk modul til inline-redigering af én opslags-/stamtabel.
# Drevet af et LOOKUP_TABLES-cfg-element. Redigering i et jspreadsheet-grid
# (excelR) — samme editor som biSPCharts: tekst/tal-celler redigeres direkte,
# FK-kolonner er native dropdowns ({id, name}-source: label vises, id gemmes).
# Legacy-ændringer ankommer som HELE tabellen og diffes via pk-match. Opt-in
# adaptergrids sender i stedet servervaliderede enkeltcelle-events. Begge stier
# skriver med db$update_cell og genbruger safe_operation().

#' @noRd
mod_lookup_table_ui <- function(id, cfg) {
  ns <- NS(id)
  grid <- excelR::excelOutput(ns("tbl"), width = "100%", height = "auto")
  if (excel_adapter_enabled(cfg)) {
    grid <- div(class = "bfh-excel-grid", `data-bfh-adapter` = "true", grid)
  }
  tagList(
    div(class = "d-flex justify-content-between align-items-center mb-2 flex-wrap gap-2",
      h4(cfg$label, class = "m-0"),
      div(class = "d-flex gap-2",
        actionButton(ns("add_row"), "Ny r\u00E6kke", class = "btn-success btn-sm"),
        actionButton(ns("delete"), "Slet valgte r\u00E6kke", class = "btn-outline-danger btn-sm"))),
    p(class = "text-muted small",
      "Dobbeltklik en celle for at redigere. Klik en r\u00E6kke og tryk Slet. Id er l\u00E5st."),
    grid
  )
}

#' @noRd
lookup_excel_adapter_map <- function(cfg, col_names) {
  configured <- stats::setNames(
    vapply(cfg$cols, function(col) col$type %||% "text", ""),
    vapply(cfg$cols, function(col) col$col, "")
  )
  value_type <- unname(configured[col_names])
  value_type[is.na(value_type)] <- "text"
  data.frame(
    column_index = seq_along(col_names) - 1L,
    field = col_names,
    value_type = value_type,
    editable = col_names != cfg$pk & col_names %in% names(configured),
    stringsAsFactors = FALSE
  )
}

.lookup_adapter_canonical_cell <- function(event, rows, pk, column_map) {
  if (!is.list(event) || !is.character(event$row_pk) ||
      length(event$row_pk) != 1L || is.na(event$row_pk) ||
      !is_scalar_intish(event$column_index) || !is.data.frame(column_map)) {
    return(list(known = FALSE, value = NA))
  }
  row_index <- which(as.character(rows[[pk]]) == event$row_pk)
  map_index <- which(column_map$column_index == as.integer(event$column_index))
  if (length(row_index) != 1L || length(map_index) != 1L) {
    return(list(known = FALSE, value = NA))
  }
  field <- column_map$field[map_index]
  if (!field %in% names(rows)) return(list(known = FALSE, value = NA))
  list(known = TRUE, value = canonical_for_browser(rows[[field]][row_index]))
}

.lookup_adapter_db_cell <- function(row, event, pk, rows) {
  if (!is.data.frame(row) || nrow(row) != 1L ||
      !all(c(pk, event$field) %in% names(row)) ||
      !identical(as.character(row[[pk]][1]), as.character(event$pk_value))) {
    return(NULL)
  }
  value <- row[[event$field]][1]
  if (length(value) != 1L || typeof(value) != typeof(rows[[event$field]])) {
    return(NULL)
  }
  value
}

.lookup_adapter_values_equal <- function(left, right) {
  identical(left, right) ||
    (length(left) == 1L && length(right) == 1L && is.na(left) && is.na(right))
}

#' @noRd
mod_lookup_table_server <- function(id, db, cfg,
                                    adapter_reply = send_excel_adapter_result) {
  moduleServer(id, function(input, output, session) {
    rows <- reactiveVal(db$list_rows())
    grid_generation <- reactiveVal(1L)
    render_revision <- reactiveVal(0L)
    force_grid_render <- function() {
      grid_generation(isolate(grid_generation()) + 1L)
      render_revision(isolate(render_revision()) + 1L)
    }
    status_msg <- reactiveVal("")
    sel_pk <- reactiveVal(NULL) # pk (chr) for senest valgte række
    adapter_locked <- reactiveVal(FALSE)
    ambiguity_message <- paste0(
      "Databasestatus kunne ikke bekr\u00E6ftes. ",
      "Genindl\u00E6s siden."
    )
    column_map <- reactiveVal(NULL)
    mapped_generation <- NULL
    fk_cols <- Filter(function(c) identical(c$type, "fk"), cfg$cols)
    col_meta <- stats::setNames(cfg$cols, vapply(cfg$cols, function(c) c$col, ""))

    .ensure_column_map <- function(d, generation) {
      if (!excel_adapter_enabled(cfg) || identical(mapped_generation, generation)) {
        return(invisible(NULL))
      }
      map <- lookup_excel_adapter_map(cfg, names(d))
      validate_excel_adapter_map(map, names(d), cfg$pk)
      column_map(map)
      mapped_generation <<- generation
      invisible(NULL)
    }
    .ensure_column_map(isolate(rows()), isolate(grid_generation()))

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
      render_revision()
      d <- isolate(rows())
      generation <- isolate(grid_generation())
      .ensure_column_map(d, generation)
      adapter_args <- if (excel_adapter_enabled(cfg)) {
        list(
          tableOverflow = TRUE,
          tableHeight = "calc(100vh - 250px)",
          # excelR 0.4.x accepts pagination as a numeric page size only;
          # zero preserves jspreadsheet's disabled-pagination semantics.
          pagination = 0L,
          selectionCopy = TRUE
        )
      } else {
        list()
      }
      do.call(excelR::excelTable, c(list(
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
      ), adapter_args))
    })

    if (excel_adapter_enabled(cfg)) {
      observe({
        render_revision()
        session$onFlushed(function() {
          send_excel_adapter_init(session, "tbl", isolate(grid_generation()))
        }, once = TRUE)
      })
    }

    # excelR sender BÅDE celle-ændringer og selektioner på input$tbl —
    # forSelectedVals skelner. Selektion: gem pk'en (læses fra payloadens
    # fullData, dvs. grid'ets aktuelle — evt. sorterede — rækkefølge).
    observeEvent(input$tbl, {
      if (excel_adapter_enabled(cfg)) return()
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
      if (revert) force_grid_render()
    })

    if (excel_adapter_enabled(cfg)) {
      observeEvent(input$tbl_cell, {
        raw_event <- input$tbl_cell
        if (isTRUE(isolate(adapter_locked()))) {
          adapter_reply(session, "tbl", excel_adapter_result(
            raw_event, "rejected", NA, ambiguity_message,
            lock_grid = TRUE
          ))
          return()
        }
        current_rows <- isolate(rows())
        current_map <- isolate(column_map())
        event <- prepare_excel_cell_update(
          raw_event,
          isolate(grid_generation()),
          current_rows,
          cfg$pk,
          current_map
        )
        if (!isTRUE(event$ok)) {
          canonical <- .lookup_adapter_canonical_cell(
            raw_event, current_rows, cfg$pk, current_map
          )
          adapter_reply(session, "tbl", excel_adapter_result(
            raw_event, "rejected", canonical$value, event$message,
            lock_grid = !canonical$known
          ))
          return()
        }
        ok <- safe_operation("opdat\u00E9r adaptercelle", {
          affected <- db$update_cell(event$pk_value, event$field, event$value)
          if (!is_scalar_intish(affected) || affected != 1) {
            stop("Databaseopdateringen p\u00E5virkede ikke pr\u00E6cis en r\u00E6kke.",
                 call. = FALSE)
          }
          TRUE
        }, fallback = FALSE)
        if (isTRUE(ok)) {
          d <- patch_excel_cell(current_rows, event$row_index, event$field, event$value)
          rows(d)
          adapter_reply(session, "tbl", excel_adapter_result(
            event, "saved", event$canonical_value
          ))
          return()
        }

        fresh_row <- safe_operation(
          "genindl\u00E6s adapterr\u00E6kke efter skrivefejl",
          db$get_row(event$pk_value),
          fallback = NULL
        )
        actual <- .lookup_adapter_db_cell(
          fresh_row, event, cfg$pk, current_rows
        )
        if (is.null(actual)) {
          adapter_locked(TRUE)
          adapter_reply(session, "tbl", excel_adapter_result(
            event, "rejected", NA, ambiguity_message,
            lock_grid = TRUE
          ))
          return()
        }
        d <- patch_excel_cell(current_rows, event$row_index, event$field, actual)
        rows(d)
        saved <- .lookup_adapter_values_equal(actual, event$value)
        adapter_reply(session, "tbl", excel_adapter_result(
          event,
          if (saved) "saved" else "rejected",
          canonical_for_browser(actual),
          if (saved) NULL else "\u00C6ndringen blev ikke gemt.",
          lock_grid = FALSE
        ))
      }, ignoreInit = TRUE, priority = 100)

      observeEvent(input$tbl_selection, {
        selection <- input$tbl_selection
        generation <- if (is.list(selection)) {
          .excel_adapter_generation(selection$grid_generation)
        } else {
          NULL
        }
        if (is.null(generation) || generation != isolate(grid_generation())) {
          sel_pk(NULL)
          return()
        }
        row_pks <- selection$row_pks
        if (is.list(row_pks)) row_pks <- unlist(row_pks, recursive = FALSE,
                                                use.names = FALSE)
        if (length(row_pks) < 1L || !is.atomic(row_pks) || anyNA(row_pks)) {
          sel_pk(NULL)
          return()
        }
        requested <- as.character(row_pks[[1]])
        matches <- which(as.character(isolate(rows())[[cfg$pk]]) == requested)
        if (length(matches) != 1L) {
          sel_pk(NULL)
          return()
        }
        sel_pk(as.character(isolate(rows())[[cfg$pk]][matches]))
      }, ignoreInit = TRUE)

      observeEvent(input$tbl_client_status, {
        safe_messages <- c(
          "Inds\u00E6tning af flere celler underst\u00F8ttes ikke endnu."
        )
        message <- input$tbl_client_status$message
        if (is.character(message) && length(message) == 1L &&
            !is.na(message) && message %in% safe_messages) {
          status_msg(message)
        }
      }, ignoreInit = TRUE)
    }

    observeEvent(input$add_row, {
      safe_operation("ny r\u00E6kke", {
        db$add_row()
        rows(db$list_rows())
        sel_pk(NULL)
        force_grid_render()
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
      force_grid_render()
      status_msg("Slettet")
    })

    # eksponér til test
    list(
      rows = rows, status_msg = status_msg, sel_pk = sel_pk,
      grid_generation = grid_generation, render_revision = render_revision,
      column_map = column_map
    )
  })
}
