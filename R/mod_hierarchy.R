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
  parent_labels <- function(choices) {
    teknisk <- d[[teknisk_col]]
    display <- d[[cfg$display_col]]
    labels <- vapply(seq_len(nrow(d)), function(i) {
      .node_label(cfg, display[i], teknisk[i], d$id[i])
    }, "")
    stats::setNames(d$id[choices], labels[choices])
  }
  niveau_choices <- c("(vælg)" = "",
                       stats::setNames(niveauer$id, niveauer$label))
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
    excluded <- hierarchy_descendants(d, "id", "parent_id_raw", id)
    parent_choices <- c("(rod)" = "", parent_labels(!(d$id %in% excluded)))
    c(
      .hierarchy_text_editor_html(ns, id, text_cols[1], text_value(text_cols[1], i)),
      paste0(padding(d$depth[i]),
             .hierarchy_text_editor_html(ns, id, text_cols[2],
                                         text_value(text_cols[2], i), d$depth[i]),
             "</div>"),
      .hierarchy_text_editor_html(ns, id, text_cols[3], text_value(text_cols[3], i)),
      .hierarchy_select_editor_html(ns, id, fields[["parent"]],
                                    d$parent_id_raw[i], parent_choices,
                                    root = TRUE),
      .hierarchy_select_editor_html(ns, id, fields[["niveau"]],
                                    d$niveau_id[i], niveau_choices)
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
    div(class = "d-flex justify-content-end mb-2",
      actionButton(ns("new_node"), "Ny node", class = "btn-success")),
    DT::DTOutput(ns("tbl")))
}

#' @noRd
mod_hierarchy_server <- function(id, db, cfg) {
  moduleServer(id, function(input, output, session) {
    nodes <- reactiveVal(db$list_nodes())
    niveauer <- reactiveVal(db$niveau_options())
    status_msg <- reactiveVal("")
    warn_msg <- reactiveVal("")
    editing_id <- reactiveVal(NULL)
    last_opened_parent <- reactiveVal(NULL)
    reload <- function() nodes(db$list_nodes())

    tree <- reactive(hierarchy_order(nodes(), "id", "parent_id_raw",
                                     sort_col = cfg$display_col))

    # Flydende notifikationer (synlige over modal, jf. mod_diagram)
    observeEvent(status_msg(), {
      if (nzchar(status_msg())) showNotification(status_msg(), duration = 5)
    }, ignoreInit = TRUE)
    observeEvent(warn_msg(), {
      if (nzchar(warn_msg()))
        showNotification(warn_msg(), type = "warning", duration = 8)
    }, ignoreInit = TRUE)

    .labels <- function(d) {
      teknisk <- if ("organisatorisk_navn_teknisk" %in% names(d))
        d$organisatorisk_navn_teknisk else rep(NA_character_, nrow(d))
      vapply(seq_len(nrow(d)), function(i)
        .node_label(cfg, d[[cfg$display_col]][i], teknisk[i], d$id[i]), "")
    }

    output$tbl <- DT::renderDT({
      d <- tree()
      out <- .hierarchy_editor_data(d, cfg, session$ns, niveauer())
      editor_value <- htmlwidgets::JS(
        "function(data, type) {\n          if (type === 'sort' || type === 'filter') {\n            return $('<div>').html(data).find('.hierarchy-editor').val() || '';\n          }\n          return data;\n        }")
      DT::datatable(out, escape = FALSE, rownames = FALSE, selection = "single",
        callback = .hierarchy_dt_callback(session$ns),
        options = list(pageLength = 25, columnDefs = list(
          list(targets = 0:4, render = editor_value))))
    }, server = FALSE)

    observeEvent(input$inline_edit, {
      result <- .prepare_hierarchy_inline_update(
        nodes(), niveauer(), cfg, input$inline_edit)
      if (!isTRUE(result$ok)) {
        warn_msg(result$error)
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
        status_msg("Gemt")
        if (nzchar(result$warning)) warn_msg(result$warning)
      } else {
        warn_msg("Fejl ved gem; v\u00e6rdien er gendannet")
      }
    })

    .parent_choices <- function(exclude_subtree_of = NULL) {
      d <- tree()
      if (!is.null(exclude_subtree_of)) {
        excl <- hierarchy_descendants(nodes(), "id", "parent_id_raw",
                                      exclude_subtree_of)
        d <- d[!(d$id %in% excl), , drop = FALSE]
      }
      stats::setNames(d$id, .labels(d))
    }

    .niveau_choices <- function() {
      nv <- niveauer()
      stats::setNames(nv$id, nv$label)
    }

    .show_form_modal <- function(vals = NULL, exclude_subtree_of = NULL) {
      ns <- session$ns
      is_new <- is.null(vals)
      showModal(modalDialog(
        title = if (is_new) "Ny node" else "Redigér node",
        size = "m", easyClose = FALSE,
        .hierarchy_form_ui(ns, cfg, vals,
          parent_choices = .parent_choices(exclude_subtree_of),
          niveau_choices = .niveau_choices()),
        footer = div(class = "d-flex justify-content-between w-100",
          if (is_new) span() else
            actionButton(ns("h_delete"), "Slet", class = "btn-outline-danger"),
          div(class = "d-flex gap-2",
            modalButton("Annullér"),
            actionButton(ns("h_save"), "Gem", class = "btn-primary")))))
    }

    observeEvent(input$open_id, {
      rid <- as.integer(input$open_id)
      row <- nodes()[nodes()$id == rid, , drop = FALSE]
      if (nrow(row) == 0) { status_msg("Node ikke fundet"); return() }
      editing_id(rid)
      last_opened_parent(row[[cfg$parent_col %||% "parent_id_raw"]][1])
      vals <- as.list(row[1, , drop = FALSE])
      vals[[cfg$parent_col]] <- row$parent_id_raw[1]
      vals[[cfg$level$col]] <- row$niveau_id[1]
      .show_form_modal(vals, exclude_subtree_of = rid)
    })

    observeEvent(input$new_node, {
      editing_id(NULL)
      prefill_parent <- last_opened_parent()
      vals <- NULL
      if (!is.null(prefill_parent) && !is.na(prefill_parent)) {
        vals <- stats::setNames(list(prefill_parent), cfg$parent_col)
      }
      .show_form_modal(vals)
    })

    observeEvent(input$h_save, {
      vals <- .collect_hierarchy_form(input, cfg)
      if (is.na(vals[[cfg$display_col]]) || !nzchar(vals[[cfg$display_col]])) {
        status_msg(sprintf("%s er obligatorisk", cfg$label))
        return()
      }
      rid <- editing_id()
      new_parent <- vals[[cfg$parent_col]]

      # Server-side cyklus-assert: forælder må ikke være i egen subtree
      if (!is.null(rid) && !is.na(new_parent)) {
        subtree <- hierarchy_descendants(nodes(), "id", "parent_id_raw", rid)
        if (new_parent %in% subtree) {
          status_msg("Kan ikke flytte node til dens egen subtree (cyklus)")
          return()
        }
      }

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
            warn_msg("Niveau er ikke dybere end forælderens niveau")
          }
        }
      }

      safe_operation("hierarki-gem", {
        if (is.null(rid)) {
          newid <- db$create_node(vals)
          status_msg(paste("Oprettet node", newid))
        } else {
          db$update_node(rid, vals)
          status_msg(paste("Gemt node", rid))
        }
        removeModal(); reload()
      }, fallback = status_msg("Fejl ved gem (se log)"))
    })

    observeEvent(input$h_delete, {
      rid <- editing_id()
      if (is.null(rid)) return()
      n <- db$child_count(rid)
      if (n > 0) {
        warn_msg(sprintf("Noden har %d børn — flyt eller slet dem først.", n))
        return()
      }
      safe_operation("hierarki-slet", {
        db$delete_node(rid)
        status_msg(paste("Slettet node", rid))
        removeModal(); editing_id(NULL); reload()
      }, fallback = warn_msg(
        "Noden er i brug og kan ikke slettes (referencer findes)."))
    })

    # eksponér til test
    list(nodes = nodes, tree = tree, status_msg = status_msg,
         warn_msg = warn_msg, editing_id = editing_id)
  })
}
