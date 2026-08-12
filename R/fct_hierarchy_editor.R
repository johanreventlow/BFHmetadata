# Rene hjaelpere til inline-redigering af hierarki-rækker. De holder
# validering og HTML-generering fri af Shiny-serveren, saa begge dele kan
# testes uden en browser eller database.

#' Felter vist af organisationens inline-editor, mappet til lagringskolonner.
#' @noRd
.hierarchy_inline_fields <- function(cfg) {
  fields <- vapply(cfg$fields, function(field) field$col, "")
  stats::setNames(
    c(fields, cfg$parent_col, cfg$level$col),
    c("teknisk", "langt", "kort", "parent", "niveau"))
}

#' Udtræk en komplet, normaliseret lagringsrække fra et node-resultat.
#' @noRd
.hierarchy_row_values <- function(row, cfg) {
  cols <- hierarchy_edit_cols(cfg)
  values <- stats::setNames(vector("list", length(cols)), cols)
  field_cols <- vapply(cfg$fields, function(field) field$col, "")

  for (col in field_cols) {
    value <- row[[col]][1]
    values[[col]] <- if (is.na(value) || identical(as.character(value), "")) {
      NA_character_
    } else {
      as.character(value)
    }
  }

  values[[cfg$parent_col]] <- .hierarchy_editor_integer(
    row$parent_id_raw[1], allow_empty = TRUE)
  values[[cfg$level$col]] <- .hierarchy_editor_integer(
    row$niveau_id[1], allow_empty = TRUE)
  if (!is.null(cfg$aktiv_col)) {
    aktiv <- if ("aktiv" %in% names(row)) row[["aktiv"]][1] else
      row[[cfg$aktiv_col]][1]
    values[[cfg$aktiv_col]] <- isTRUE(aktiv)
  }
  values
}

#' Konverter et enkelt heltal uden at afkorte ugyldige vaerdier.
#' @noRd
.hierarchy_editor_integer <- function(value, allow_empty = FALSE) {
  if (is.null(value) || length(value) != 1 || is.na(value) ||
      (is.character(value) && !nzchar(value))) {
    return(if (allow_empty) NA_integer_ else NULL)
  }
  if (is.numeric(value)) {
    parsed <- suppressWarnings(as.integer(value))
    if (!is.finite(value) || is.na(parsed) || value != parsed) return(NULL)
    return(parsed)
  }
  value <- as.character(value)
  if (!grepl("^[+-]?[0-9]+$", value)) return(NULL)
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed)) NULL else parsed
}

#' Ensartet resultatform, ogsaa naar et inline-event afvises.
#' @noRd
.hierarchy_inline_result <- function(ok, unchanged = FALSE, id = NA_integer_,
                                     values = list(), warning = "", error = "") {
  list(ok = ok, unchanged = unchanged, id = id, values = values,
       warning = warning, error = error)
}

#' Forbered og valider en enkelt inline-aendring mod den autoritative raekke.
#' @noRd
.prepare_hierarchy_inline_update <- function(nodes, niveauer, cfg, event) {
  reject <- function(error, id = NA_integer_, values = list()) {
    .hierarchy_inline_result(FALSE, id = id, values = values, error = error)
  }
  text_fields <- vapply(cfg$fields, function(field) field$col, "")
  if (is.null(cfg$display_col) || length(cfg$display_col) != 1 ||
      is.na(cfg$display_col) || !nzchar(cfg$display_col) ||
      !(cfg$display_col %in% text_fields)) {
    return(reject("Hierarkiet mangler et gyldigt visningsfelt"))
  }
  if (!is.list(event)) return(reject("Ugyldigt inline-event"))

  id <- .hierarchy_editor_integer(event$id)
  if (is.null(id)) return(reject("Ugyldigt node-id"))
  row <- nodes[nodes$id == id, , drop = FALSE]
  if (nrow(row) != 1) return(reject("Node ikke fundet", id))
  values <- .hierarchy_row_values(row, cfg)

  field <- event$field
  allowed_fields <- unname(.hierarchy_inline_fields(cfg))
  if (is.null(field) || length(field) != 1 || is.na(field) ||
      !(field %in% allowed_fields)) {
    return(reject("Ukendt felt", id, values))
  }
  if (field %in% text_fields) {
    raw <- event$value
    if (is.null(raw) || length(raw) != 1 || is.na(raw) ||
        identical(as.character(raw), "")) {
      value <- NA_character_
    } else {
      value <- as.character(raw)
    }
  } else {
    value <- .hierarchy_editor_integer(event$value, allow_empty = TRUE)
    if (is.null(value)) return(reject("Ugyldigt heltals-id", id, values))
  }
  values[[field]] <- value

  if (is.na(values[[cfg$display_col]]) || !nzchar(values[[cfg$display_col]])) {
    return(reject(sprintf("%s er obligatorisk", cfg$label), id, values))
  }

  if (identical(field, cfg$parent_col) && !is.na(value)) {
    if (!(value %in% nodes$id)) return(reject("Ukendt foraelder", id, values))
    subtree <- hierarchy_descendants(nodes, "id", "parent_id_raw", id)
    if (value %in% subtree) {
      return(reject("Kan ikke flytte node til egen subtree (cyklus)", id, values))
    }
  }
  if (identical(field, cfg$level$col) && !is.na(value) &&
      !(value %in% niveauer$id)) {
    return(reject("Ukendt niveau", id, values))
  }

  unchanged <- identical(values[[field]], .hierarchy_row_values(row, cfg)[[field]])
  warning <- ""
  parent <- values[[cfg$parent_col]]
  niveau <- values[[cfg$level$col]]
  if (!is.na(parent) && !is.na(niveau)) {
    parent_row <- nodes[nodes$id == parent, , drop = FALSE]
    own_num <- nodes$niveau_num[match(niveau, nodes$niveau_id)]
    if (nrow(parent_row) == 1 && length(own_num) == 1 &&
        !is.na(parent_row$niveau_num[1]) && !is.na(own_num) &&
        own_num <= parent_row$niveau_num[1]) {
      warning <- "Niveau er ikke dybere end for\u00e6lderens niveau"
    }
  }

  .hierarchy_inline_result(TRUE, unchanged = unchanged, id = id,
                           values = values, warning = warning)
}

#' Sikker tekst-editor til en enkelt DT-celle.
#' @noRd
.hierarchy_text_editor_html <- function(ns, id, field, value, depth = 0L) {
  esc_attr <- function(x) htmltools::htmlEscape(as.character(x), attribute = TRUE)
  input_id <- ns(paste0("inline_", id, "_", field))
  sprintf(paste0(
    '<input id="%s" class="form-control form-control-sm hierarchy-editor" ',
    'type="text" value="%s" data-saved="%s" data-node-id="%s" ',
    'data-field="%s" data-depth="%s">'),
    esc_attr(input_id), esc_attr(value), esc_attr(value), esc_attr(id),
    esc_attr(field), esc_attr(depth))
}

#' Sikker select-editor til en enkelt DT-celle.
#' @noRd
.hierarchy_select_editor_html <- function(ns, id, field, current, choices,
                                          root = FALSE) {
  esc_attr <- function(x) htmltools::htmlEscape(as.character(x), attribute = TRUE)
  esc_text <- function(x) htmltools::htmlEscape(as.character(x))
  current <- if (is.null(current) || is.na(current)) "" else as.character(current)
  option_html <- vapply(seq_along(choices), function(i) {
    value <- as.character(choices[[i]])
    selected <- if (identical(value, current)) " selected" else ""
    sprintf('<option value="%s"%s>%s</option>',
      esc_attr(value), selected, esc_text(names(choices)[i]))
  }, "")
  input_id <- ns(paste0("inline_", id, "_", field))
  sprintf(paste0(
    '<select id="%s" class="form-select form-select-sm hierarchy-editor" ',
    'data-saved="%s" data-node-id="%s" data-field="%s" data-root="%s">%s</select>'),
    esc_attr(input_id), esc_attr(current), esc_attr(id), esc_attr(field),
    esc_attr(isTRUE(root)), paste0(option_html, collapse = ""))
}

#' Delegated DT-callback til inline-editorernes change/blur-events.
#' @noRd
.hierarchy_dt_callback <- function(ns) {
  input_name <- jsonlite::toJSON(ns("inline_edit"), auto_unbox = TRUE)
  htmlwidgets::JS(sprintf(
    "function(table) {
      var $table = $(table.table().node());
      var inputName = %s;
      function submit(editor) {
        if (editor.dataset.cancelled === 'true') {
          delete editor.dataset.cancelled;
          return;
        }
        if (editor.classList.contains('hierarchy-saving')) return;
        if (editor.value === editor.dataset.saved) return;
        var oldValue = editor.dataset.saved;
        editor.classList.add('hierarchy-saving');
        Shiny.setInputValue(inputName, {
          id: Number(editor.dataset.nodeId),
          field: editor.dataset.field,
          oldValue: oldValue,
          value: editor.value,
          nonce: Date.now()
        }, {priority: 'event'});
      }
      $table.off('.hierarchy-editor');
      $table.on('keydown.hierarchy-editor', '.hierarchy-editor', function(event) {
        if (event.key === 'Enter' && this.tagName === 'INPUT') {
          event.preventDefault();
          this.blur();
        }
        if (event.key === 'Escape') {
          event.preventDefault();
          this.value = this.dataset.saved;
          this.dataset.cancelled = 'true';
          this.blur();
        }
      });
      $table.on('blur.hierarchy-editor change.hierarchy-editor',
        '.hierarchy-editor', function() { submit(this); });
    }", input_name))
}
