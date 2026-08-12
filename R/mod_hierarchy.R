# Generisk hierarki-CRUD (traeer med parent-FK): indrykket DT-tabel + delt
# formular-modal. Config-drevet (HIERARCHY_TABLES) — ét modul, N instanser
# (org-struktur Fase C, indikator-hierarki Fase D). Mønster fra mod_diagram.R.

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

#' Byg de fem permanente inline-editor-kolonner til hierarki-tabellen.
#' @noRd
.hierarchy_editor_data <- function(d, cfg, ns, niveauer) {
  fields <- .hierarchy_inline_fields(cfg)
  text_cols <- unname(fields[c("teknisk", "langt", "kort")])
  teknisk_col <- text_cols[1]
  column_names <- c(vapply(cfg$fields, function(field) field$label, ""),
                    "Forælder", "Niveau")
  teknisk <- d[[teknisk_col]]
  display <- d[[cfg$display_col]]
  node_labels <- vapply(seq_len(nrow(d)), function(i) {
    .node_label(cfg, display[i], teknisk[i], d$id[i])
  }, "")
  padding <- function(depth) {
    value <- htmltools::htmlEscape(as.character(depth * 1.5), attribute = TRUE)
    sprintf('<div style="padding-left:%srem">', value)
  }
  text_value <- function(col, row) {
    value <- d[[col]][row]
    if (is.na(value)) "" else value
  }
  if (nrow(d) == 0) {
    return(as.data.frame(stats::setNames(
      replicate(length(column_names), character(), simplify = FALSE),
      column_names), stringsAsFactors = FALSE, check.names = FALSE))
  }

  out <- lapply(seq_len(nrow(d)), function(i) {
    id <- d$id[i]
    parent_value <- d$parent_id_raw[i]
    parent_match <- match(parent_value, d$id)
    parent_label <- if (is.na(parent_value)) "(rod)" else
      node_labels[parent_match]
    level_value <- d$niveau_id[i]
    level_match <- match(level_value, niveauer$id)
    level_label <- if (is.na(level_value)) "(vælg)" else
      niveauer$label[level_match]
    c(
      .hierarchy_text_editor_html(ns, id, text_cols[1], text_value(text_cols[1], i)),
      paste0(padding(d$depth[i]),
             .hierarchy_text_editor_html(ns, id, text_cols[2],
                                         text_value(text_cols[2], i), d$depth[i]),
             "</div>"),
      .hierarchy_text_editor_html(ns, id, text_cols[3], text_value(text_cols[3], i)),
      .hierarchy_select_editor_html(ns, id, fields[["parent"]],
                                    parent_value, choices = character(),
                                    root = TRUE, lazy = TRUE,
                                    current_label = parent_label),
      .hierarchy_select_editor_html(ns, id, fields[["niveau"]],
                                    level_value, choices = character(),
                                    lazy = TRUE, current_label = level_label)
    )
  })
  out <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE,
                       check.names = FALSE)
  names(out) <- column_names
  out
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
  div(class = "mt-2",
    tags$style(HTML(sprintf(paste0(
      "#%s .hierarchy-editor { min-height:calc(1.5em + .5rem + 2px); ",
      "padding:.25rem .5rem; background-color:#f8f9fa; } ",
      "#%s .hierarchy-editor:focus { background-color:#fff; } ",
      "#%s .hierarchy-editor:disabled, #%s .hierarchy-editor.hierarchy-saving ",
      "{ opacity:.65; background-color:#e9ecef; cursor:wait; }"),
      ns("tbl"), ns("tbl"), ns("tbl"), ns("tbl")))),
    div(class = "d-flex justify-content-end gap-2 mb-2",
      actionButton(ns("new_node"), "Ny node", class = "btn-success"),
      actionButton(ns("delete_selected"), "Slet valgt",
                   class = "btn-outline-danger")),
    DT::DTOutput(ns("tbl")))
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
    reload <- function() {
      nodes(db$list_nodes())
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

    observeEvent(input$selected_node_id, {
      selected_id(.hierarchy_editor_integer(input$selected_node_id))
    }, ignoreNULL = FALSE)

    .labels <- function(d) {
      teknisk <- if ("organisatorisk_navn_teknisk" %in% names(d))
        d$organisatorisk_navn_teknisk else rep(NA_character_, nrow(d))
      vapply(seq_len(nrow(d)), function(i)
        .node_label(cfg, d[[cfg$display_col]][i], teknisk[i], d$id[i]), "")
    }

    output$tbl <- DT::renderDT({
      table_revision()
      d <- tree()
      selected <- isolate(selected_id())
      selected_row <- if (is.null(selected)) integer() else match(selected, d$id)
      if (length(selected_row) == 1L && is.na(selected_row)) {
        selected_row <- integer()
      }
      out <- .hierarchy_editor_data(d, cfg, session$ns, niveauer())
      parent_choices <- data.frame(
        id = d$id,
        label = .labels(d),
        parent_id = d$parent_id_raw,
        stringsAsFactors = FALSE)
      level_choices <- data.frame(
        id = niveauer()$id,
        label = niveauer()$label,
        stringsAsFactors = FALSE)
      editor_value <- htmlwidgets::JS(
        "function(data, type) {\n          if (type === 'sort' || type === 'filter') {\n            return $('<div>').html(data).find('.hierarchy-editor').val() || '';\n          }\n          return data;\n        }")
      DT::datatable(out, escape = FALSE, rownames = FALSE,
        selection = list(mode = "single", selected = selected_row),
        callback = .hierarchy_dt_callback(session$ns, parent_choices,
                                          level_choices),
        options = list(pageLength = 25, columnDefs = list(
          list(targets = 0:4, render = editor_value))))
    }, server = FALSE)

    observeEvent(input$inline_edit, {
      result <- .prepare_hierarchy_inline_update(
        nodes(), niveauer(), cfg, input$inline_edit)
      if (!isTRUE(result$ok)) {
        notify_warning(result$error)
        reload()
        return()
      }
      if (isTRUE(result$unchanged)) {
        reload()
        return()
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
      DT::selectRows(DT::dataTableProxy("tbl", session), NULL)
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
    list(nodes = nodes, tree = tree, status_msg = status_msg,
         warn_msg = warn_msg, status_event = status_event,
         warn_event = warn_event, table_revision = table_revision,
         selected_id = selected_id, delete_id = delete_id)
  })
}
