# Generisk hierarki-CRUD (traeer med parent-FK): excelR-grid (jspreadsheet,
# samme editor som opslagstabellerne) + delt formular-modal til nye noder.
# Config-drevet (HIERARCHY_TABLES) — ét modul, N instanser. Grid'et viser
# traeet via en readOnly "Struktur"-kolonne (indrykket, depth-first);
# tekstfelter redigeres direkte, Forælder/Niveau via dropdowns. Alle
# ændringer valideres server-side af .prepare_hierarchy_inline_update
# (cyklus-værn, obligatorisk visningsnavn, ukendte id'er).

#' Delt formular-UI for en hierarki-node. cfg = element fra HIERARCHY_TABLES.
#' vals = named list (NULL → ny), parent_choices = named vector (label→id,
#' MINUS egen subtree), niveau_choices = named vector (label→id). Prefix h_.
#' @noRd
.hierarchy_form_ui <- function(ns, cfg, vals = NULL, parent_choices,
                               niveau_choices) {
  is_new <- is.null(vals)
  v <- function(col, default = NULL) if (is_new) default else vals[[col]]

  field_inputs <- lapply(cfg$fields, function(f) {
    fid <- ns(paste0("h_", f$col))
    val <- v(f$col) %||% ""
    if (identical(f$type, "textarea")) {
      textAreaInput(fid, f$label, value = val)
    } else {
      textInput(fid, f$label, value = val)
    }
  })

  parent_val <- v(cfg$parent_col)
  parent_sel <- if (is.null(parent_val) || is.na(parent_val)) "" else
    as.character(parent_val)
  niveau_val <- v(cfg$level$col)
  niveau_sel <- if (is.null(niveau_val) || is.na(niveau_val)) "" else
    as.character(niveau_val)

  aktiv_input <- if (!is.null(cfg$aktiv_col)) {
    checkboxInput(ns("h_aktiv"), "Aktiv", value = isTRUE(v(cfg$aktiv_col, TRUE)))
  } else {
    NULL
  }

  tagList(
    field_inputs,
    selectInput(ns("h_parent"), "Forælder",
      choices = c("(rod)" = "", parent_choices), selected = parent_sel),
    selectInput(ns("h_niveau"), "Niveau",
      choices = c("(vælg)" = "", niveau_choices), selected = niveau_sel),
    aktiv_input)
}

#' Visningsnavn for en node med fallback-kæde: display_col -> teknisk navn ->
#' "(uden navn #id)". Forhindrer NA-navngivne selectInput-choices (fejler
#' hårdt i shiny::selectInput ved reelt DB-data med tomme navnefelter).
#' @noRd
.node_label <- function(cfg, display_val, teknisk_val, id) {
  if (!is.na(display_val) && nzchar(display_val)) return(display_val)
  if (!is.na(teknisk_val) && nzchar(teknisk_val)) return(teknisk_val)
  sprintf("(uden navn #%s)", id)
}

#' Saml formular-inputs → named list i hierarchy_edit_cols(cfg)-orden. Tom
#' forælder → NA (rodnode OK). Tomme tekstfelter → NA.
#' @noRd
.collect_hierarchy_form <- function(input, cfg, prefix = "h_") {
  gv <- function(col) input[[paste0(prefix, col)]]
  chr_or_na <- function(x) {
    if (is.null(x) || identical(x, "")) NA_character_ else as.character(x)
  }
  int_or_na <- function(x) {
    if (is.null(x) || identical(x, "")) NA_integer_ else as.integer(x)
  }
  vals <- stats::setNames(
    lapply(cfg$fields, function(f) chr_or_na(gv(f$col))),
    vapply(cfg$fields, function(f) f$col, ""))
  vals[[cfg$parent_col]] <- int_or_na(gv("parent"))
  vals[[cfg$level$col]] <- int_or_na(gv("niveau"))
  if (!is.null(cfg$aktiv_col)) vals[[cfg$aktiv_col]] <- isTRUE(gv("aktiv"))
  vals
}

#' @noRd
mod_hierarchy_ui <- function(id, cfg) {
  ns <- NS(id)
  # Opt-in kaskade-filtre (cfg$filters): én dropdown pr. filter, renderes
  # server-side (choices afhaenger af trae-data)
  filter_row <- if (!is.null(cfg$filters)) {
    do.call(bslib::layout_columns,
      c(list(col_widths = rep(4L, length(cfg$filters))),
        lapply(cfg$filters, function(f)
          uiOutput(ns(paste0("filter_", f$id, "_ui"))))))
  }
  div(class = "mt-2",
    div(class = "d-flex justify-content-end gap-2 mb-2",
      actionButton(ns("new_node"), "Ny node", class = "btn-success"),
      actionButton(ns("delete_selected"), "Slet valgt",
                   class = "btn-outline-danger")),
    filter_row,
    p(class = "text-muted small",
      "Dobbeltklik en celle for at redigere. Klik en række og tryk Slet valgt."),
    excelR::excelOutput(ns("tbl"), width = "100%", height = "auto"))
}

#' @noRd
mod_hierarchy_server <- function(id, db, cfg) {
  moduleServer(id, function(input, output, session) {
    nodes <- reactiveVal(db$list_nodes())
    niveauer <- reactiveVal(db$niveau_options())
    status_msg <- reactiveVal("")
    warn_msg <- reactiveVal("")
    status_event <- reactiveVal(list(message = "", nonce = 0L))
    warn_event <- reactiveVal(list(message = "", nonce = 0L))
    table_revision <- reactiveVal(0L)
    selected_id <- reactiveVal(NULL)
    delete_id <- reactiveVal(NULL)
    # Fejl-tolerant genindlæsning: DB-udfald må aldrig vælte sessionen —
    # behold senest hentede noder (revision bumpes stadig → grid snapper
    # tilbage til den kendte tilstand).
    reload <- function() {
      safe_operation("genindlæs org-træ", nodes(db$list_nodes()),
        fallback = notify_warning("Databasen svarer ikke — viser senest hentede data"))
      table_revision(isolate(table_revision()) + 1L)
    }
    notify_status <- function(message) {
      status_msg(message)
      event <- isolate(status_event())
      status_event(list(message = message, nonce = event$nonce + 1L))
    }
    notify_warning <- function(message) {
      warn_msg(message)
      event <- isolate(warn_event())
      warn_event(list(message = message, nonce = event$nonce + 1L))
    }

    tree <- reactive(hierarchy_order(nodes(), "id", "parent_id_raw",
                                     sort_col = cfg$display_col))

    # Kaskade-filtre (cfg$filters, fx Datapakke → Datasæt): hvert filter
    # tilbyder noder paa sit navngivne niveau; filter k begraenses til
    # grenen under filter k-1's valg. Eget valg bevares ved re-render.
    if (!is.null(cfg$filters)) {
      for (k in seq_along(cfg$filters)) local({
        kk <- k
        f <- cfg$filters[[kk]]
        output[[paste0("filter_", f$id, "_ui")]] <- renderUI({
          d <- tree()
          if (kk > 1) {
            prev <- input[[paste0("filter_", cfg$filters[[kk - 1]]$id)]]
            if (!is.null(prev) && nzchar(prev)) {
              prev_id <- suppressWarnings(as.integer(prev))
              if (!is.na(prev_id) && prev_id %in% d$id)
                d <- hierarchy_subtree(d, "id", "parent_id_raw", prev_id)
            }
          }
          cand <- d[d$niveau_navn %in% f$niveau_navn, , drop = FALSE]
          choices <- c("Alle" = "", stats::setNames(
            as.character(cand$id), hierarchy_node_labels(cand, cfg)))
          selected <- .preserved_filter_selection(
            isolate(input[[paste0("filter_", f$id)]]), choices)
          selectInput(session$ns(paste0("filter_", f$id)), f$label,
                      choices = choices, selected = selected)
        })
      })
    }

    # De VISTE rækker: mest specifikke valgte filter vinder (bagfra).
    # Deles af render og celle-diff, saa diffen aldrig rammer skjulte rækker.
    shown <- reactive({
      d <- tree()
      for (f in rev(cfg$filters)) {
        sel <- input[[paste0("filter_", f$id)]]
        if (is.null(sel) || !nzchar(sel)) next
        sid <- suppressWarnings(as.integer(sel))
        if (!is.na(sid) && sid %in% d$id)
          return(hierarchy_subtree(d, "id", "parent_id_raw", sid))
      }
      d
    })

    # Flydende notifikationer (synlige over modal, jf. mod_diagram)
    observeEvent(status_event(), {
      event <- status_event()
      if (nzchar(event$message)) showNotification(event$message, duration = 5)
    }, ignoreInit = TRUE)
    observeEvent(warn_event(), {
      event <- warn_event()
      if (nzchar(event$message))
        showNotification(event$message, type = "warning", duration = 8)
    }, ignoreInit = TRUE)

    .labels <- function(d) {
      teknisk <- if ("organisatorisk_navn_teknisk" %in% names(d))
        d$organisatorisk_navn_teknisk else rep(NA_character_, nrow(d))
      vapply(seq_len(nrow(d)), function(i)
        .node_label(cfg, d[[cfg$display_col]][i], teknisk[i], d$id[i]), "")
    }

    output$tbl <- excelR::renderExcel({
      table_revision()
      d <- shown()
      excelR::excelTable(
        data = hierarchy_excel_data(d, cfg),
        # source_df = fuldt trae: Forælder-dropdown skal kunne flytte en
        # node UD af den filtrerede gren
        columns = hierarchy_excel_columns(cfg, d, niveauer(),
                                          source_df = tree()),
        autoColTypes = FALSE,
        # FALSE: ellers deaktiverer width:auto table-layout:fixed, og
        # celleindholdet vinder over de beregnede kolonnebredder
        autoWidth = FALSE,
        # R\u00e6kke/kolonne-operationer styres af knapperne + DB. Sortering er
        # BEVIDST sl\u00e5et fra her (modsat de flade grids): depth-first-ordenen
        # med indrykning ER visningen \u2014 en alfabetisk sortering ville g\u00f8re
        # Struktur-kolonnen meningsl\u00f8s.
        allowInsertRow = FALSE, allowInsertColumn = FALSE,
        allowDeleteRow = FALSE, allowDeleteColumn = FALSE,
        allowRenameColumn = FALSE, columnSorting = FALSE,
        rowDrag = FALSE, columnDrag = FALSE,
        getSelectedData = TRUE
      )
    })

    # F\u00e6lles h\u00e5ndtering af \u00e9t valideret inline-event (id, field, value).
    # Afvisning/u\u00e6ndret \u2192 reload (grid snapper tilbage til DB-tilstanden).
    .handle_inline_event <- function(event) {
      result <- .prepare_hierarchy_inline_update(
        nodes(), niveauer(), cfg, event)
      if (!isTRUE(result$ok)) {
        notify_warning(result$error)
        reload()
        return(invisible())
      }
      if (isTRUE(result$unchanged)) {
        reload()
        return(invisible())
      }
      ok <- safe_operation("hierarki-inline-gem", {
        db$update_node(result$id, result$values)
        TRUE
      }, fallback = FALSE)
      reload()
      if (isTRUE(ok)) {
        notify_status("Gemt")
        if (nzchar(result$warning)) notify_warning(result$warning)
      } else {
        notify_warning("Fejl ved gem; v\u00e6rdien er gendannet")
      }
    }

    # excelR sender B\u00c5DE celle-\u00e6ndringer og selektioner p\u00e5 input$tbl \u2014
    # forSelectedVals skelner. Selektion: gem node-ID (ikke position) med
    # det samme, s\u00e5 et senere trae-reorder ikke flytter valget til en anden
    # node. \u00c6ndringer: diff mod grid-data (pk-match) \u2192 \u00e9t valideret event
    # pr. celle; kolonner uden lagringsfelt (id/Struktur) ignoreres.
    observeEvent(input$tbl, {
      p <- input$tbl
      d <- shown()   # diff mod de VISTE rækker (payload = filtreret grid)
      if (isTRUE(p$forSelectedVals)) {
        # pk fra payloadens fullData (samme mønster som de flade grids)
        pk <- excel_selected_pk(p)
        selected_id(if (is.null(pk)) NULL else as.integer(pk))
      }
      # Diff også på selektions-payloads: markør-flytningen efter en
      # celle-commit overskriver change-eventet i Shinys input-batch
      # (se excel_event_df) — fullData bærer ændringen.
      grid <- hierarchy_excel_data(d, cfg)
      changes <- excel_diff_cells(grid, excel_event_df(p), "id")
      fm <- hierarchy_grid_fields(cfg)
      changes <- changes[changes$col %in% names(fm), , drop = FALSE]
      for (k in seq_len(nrow(changes))) {
        .handle_inline_event(list(
          id = as.integer(changes$pk[k]),
          field = fm[[changes$col[k]]],
          value = changes$value[k]
        ))
      }
    })

    .parent_choices <- function() {
      d <- tree()
      stats::setNames(d$id, .labels(d))
    }

    .niveau_choices <- function() {
      nv <- niveauer()
      stats::setNames(nv$id, nv$label)
    }

    .show_form_modal <- function() {
      ns <- session$ns
      showModal(modalDialog(
        title = "Ny node",
        size = "m", easyClose = FALSE,
        .hierarchy_form_ui(ns, cfg, NULL,
          parent_choices = .parent_choices(),
          niveau_choices = .niveau_choices()),
        footer = div(class = "d-flex justify-content-end gap-2 w-100",
          modalButton("Annullér"),
          actionButton(ns("h_save"), "Gem", class = "btn-primary"))))
    }

    observeEvent(input$new_node, {
      .show_form_modal()
    })

    observeEvent(input$h_save, {
      vals <- .collect_hierarchy_form(input, cfg)
      if (is.na(vals[[cfg$display_col]]) || !nzchar(vals[[cfg$display_col]])) {
        notify_status(sprintf("%s er obligatorisk", cfg$label))
        return()
      }
      new_parent <- vals[[cfg$parent_col]]

      # Blød niveau-konsistens-advarsel (springer NA-niveauer over)
      new_niveau <- vals[[cfg$level$col]]
      if (!is.na(new_parent) && !is.na(new_niveau)) {
        parent_row <- nodes()[nodes()$id == new_parent, , drop = FALSE]
        if (nrow(parent_row) > 0 && !is.na(parent_row$niveau_num[1])) {
          nv <- niveauer()
          own_num <- nv$id[nv$id == new_niveau]
          niveau_num_row <- if (length(own_num) > 0) {
            n <- nodes()
            n$niveau_num[n$niveau_id == new_niveau][1]
          } else NA
          if (!is.na(niveau_num_row) &&
              niveau_num_row <= parent_row$niveau_num[1]) {
            notify_warning("Niveau er ikke dybere end forælderens niveau")
          }
        }
      }

      safe_operation("hierarki-gem", {
        newid <- db$create_node(vals)
        notify_status(paste("Oprettet node", newid))
        removeModal(); reload()
      }, fallback = notify_status("Fejl ved gem (se log)"))
    })

    .clear_delete_selection <- function() {
      selected_id(NULL)
      delete_id(NULL)
    }

    observeEvent(input$delete_selected, {
      rid <- selected_id()
      if (is.null(rid)) {
        notify_warning("V\u00e6lg en node f\u00f8rst")
        return()
      }
      row <- nodes()[nodes()$id == rid, , drop = FALSE]
      if (nrow(row) != 1L) {
        reload()
        notify_status("Node ikke fundet")
        .clear_delete_selection()
        return()
      }
      delete_id(rid)
      showModal(modalDialog(
        title = "Slet node",
        sprintf("Vil du slette %s?", .labels(row)[1]),
        easyClose = FALSE,
        footer = div(class = "d-flex justify-content-end gap-2 w-100",
          modalButton("Annull\u00e9r"),
          actionButton(session$ns("confirm_delete"), "Slet",
                       class = "btn-danger"))))
    })

    observeEvent(input$confirm_delete, {
      rid <- delete_id()
      if (is.null(rid)) return()
      removeModal()
      reload()
      row <- nodes()[nodes()$id == rid, , drop = FALSE]
      if (nrow(row) == 0) {
        notify_status("Node ikke fundet")
        .clear_delete_selection()
        return()
      }
      n <- db$child_count(rid)
      if (n > 0) {
        notify_warning(sprintf("Noden har %d b\u00f8rn \u2014 flyt eller slet dem f\u00f8rst.", n))
        reload()
        .clear_delete_selection()
        return()
      }
      ok <- safe_operation("hierarki-slet", {
        db$delete_node(rid)
        TRUE
      }, fallback = FALSE)
      reload()
      .clear_delete_selection()
      if (isTRUE(ok)) {
        notify_status(paste("Slettet node", rid))
      } else {
        notify_warning("Noden er i brug og kan ikke slettes (referencer findes).")
      }
    })

    # eksponér til test
    list(nodes = nodes, tree = tree, shown = shown, status_msg = status_msg,
         warn_msg = warn_msg, status_event = status_event,
         warn_event = warn_event, table_revision = table_revision,
         selected_id = selected_id, delete_id = delete_id)
  })
}
