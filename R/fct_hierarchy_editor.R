# Rene hjaelpere til inline-redigering af hierarki-rækker. De holder
# validering og HTML-generering fri af Shiny-serveren, saa begge dele kan
# testes uden en browser eller database.

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

#' Konverter en checkbox-vaerdi til logical. excelR/jspreadsheet kan sende
#' logical eller "TRUE"/"true"-strenge; alt andet er ugyldigt (NULL).
#' @noRd
.hierarchy_editor_logical <- function(value) {
  if (is.null(value) || length(value) != 1 || is.na(value)) return(NULL)
  if (is.logical(value)) return(value)
  lv <- tolower(as.character(value))
  if (lv %in% c("true", "1")) return(TRUE)
  if (lv %in% c("false", "0")) return(FALSE)
  NULL
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
  allowed_fields <- hierarchy_edit_cols(cfg)
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
  } else if (!is.null(cfg$aktiv_col) && identical(field, cfg$aktiv_col)) {
    value <- .hierarchy_editor_logical(event$value)
    if (is.null(value)) return(reject("Ugyldig aktiv-vaerdi", id, values))
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

#' Grid-titler → lagringskolonner for hierarki-grid'et (excelR).
#' "Aktiv"-checkbox medtages kun for cfg'er med aktiv_col.
#' @noRd
hierarchy_grid_fields <- function(cfg) {
  stats::setNames(
    c(vapply(cfg$fields, function(f) f$col, ""), cfg$parent_col, cfg$level$col,
      cfg$aktiv_col),   # NULL falder bort i c()
    c(vapply(cfg$fields, function(f) f$label, ""), "Forælder", "Niveau",
      if (!is.null(cfg$aktiv_col)) "Aktiv")
  )
}

#' Visningslabels for alle noder (display_col → teknisk → "(uden navn #id)").
#' @noRd
hierarchy_node_labels <- function(tree_df, cfg) {
  teknisk_col <- cfg$fields[[1]]$col
  vapply(seq_len(nrow(tree_df)), function(i) {
    .node_label(cfg, tree_df[[cfg$display_col]][i],
                tree_df[[teknisk_col]][i], tree_df$id[i])
  }, "")
}

#' Foraelder-choices (named vector label -> id) fra et hierarchy_order-
#' ordnet trae — depth-first med nbsp-indrykning, saa dropdown'en viser
#' traestrukturen visuelt (samme princip som org_hierarchy_choices).
#' @noRd
hierarchy_parent_choices <- function(tree_df, cfg) {
  depth <- if ("depth" %in% names(tree_df)) tree_df$depth else 0L
  stats::setNames(
    tree_df$id,
    paste0(strrep("  ", depth), hierarchy_node_labels(tree_df, cfg))
  )
}

#' Grid-data til excelR fra et hierarchy_order-ordnet trae.
#'
#' Kolonner: id (pk, readOnly) + "Struktur" (indrykket label, readOnly —
#' traeet SKAL kunne aflæses uden at indrykningen forurener redigerbare
#' værdier) + cfg-felterne + Forælder/Niveau som character-id'er ("" = NA,
#' matcher dropdown-sourcens id-format).
#' @noRd
hierarchy_excel_data <- function(tree_df, cfg) {
  chr_or_empty <- function(x) ifelse(is.na(x), "", as.character(x))
  fm <- hierarchy_grid_fields(cfg)
  field_titles <- setdiff(names(fm), c("Forælder", "Niveau", "Aktiv"))
  out <- data.frame(id = tree_df$id, stringsAsFactors = FALSE,
                    check.names = FALSE)
  #   (non-breaking space): alm. mellemrum kollapses i HTML-celler
  out[["Struktur"]] <- paste0(
    strrep("  ", tree_df$depth), hierarchy_node_labels(tree_df, cfg))
  for (t in field_titles) out[[t]] <- chr_or_empty(tree_df[[fm[[t]]]])
  out[["Forælder"]] <- chr_or_empty(tree_df$parent_id_raw)
  out[["Niveau"]] <- chr_or_empty(tree_df$niveau_id)
  if (!is.null(cfg$aktiv_col)) {
    # list_sql aliaser aktiv_col AS aktiv; NA normaliseres til FALSE
    aktiv <- if ("aktiv" %in% names(tree_df)) tree_df$aktiv else
      tree_df[[cfg$aktiv_col]]
    out[["Aktiv"]] <- aktiv %in% TRUE
  }
  out
}

#' Kolonne-spec til hierarki-grid'et: id + Struktur readOnly, tekstfelter
#' åbne, Forælder/Niveau som dropdowns med {id, name}-source. "(rod)"/
#' "(vælg)" er tom-id-valget; ukendte eksisterende id'er tilføjes sourcen
#' ("Ukendt … #id") så de hverken vises blankt eller tabes ved gem.
#' Cyklus-værn ligger IKKE her (kolonne-dropdowns kan ikke filtreres pr.
#' række) — .prepare_hierarchy_inline_update afviser server-side.
#' source_df: fuldt trae til Forælder-dropdown-sourcen, saa re-parenting
#' UD af en filtreret gren stadig er muligt (default = tree_df, dvs. de
#' viste rækker). Kolonnebredder maales altid paa de VISTE rækker.
#' @noRd
hierarchy_excel_columns <- function(cfg, tree_df, niveauer,
                                    source_df = tree_df) {
  # Indrykkede labels: Foraelder-dropdown'en viser traestrukturen visuelt
  labels <- names(hierarchy_parent_choices(source_df, cfg))
  unknown_parents <- setdiff(stats::na.omit(source_df$parent_id_raw),
                             source_df$id)
  parent_src <- data.frame(
    id = c("", as.character(source_df$id), as.character(unknown_parents)),
    name = c("(rod)", labels,
             sprintf("Ukendt forælder #%s", unknown_parents)),
    stringsAsFactors = FALSE)
  unknown_niv <- setdiff(stats::na.omit(tree_df$niveau_id), niveauer$id)
  niveau_src <- data.frame(
    id = c("", as.character(niveauer$id), as.character(unknown_niv)),
    name = c("(vælg)", as.character(niveauer$label),
             sprintf("Ukendt niveau #%s", unknown_niv)),
    stringsAsFactors = FALSE)
  field_titles <- vapply(cfg$fields, function(f) f$label, "")
  has_aktiv <- !is.null(cfg$aktiv_col)
  out <- data.frame(
    title = c("id", "Struktur", field_titles, "Forælder", "Niveau",
              if (has_aktiv) "Aktiv"),
    # id: "hidden" — med i data/payload (pk-diff) men aldrig synlig
    type = c("hidden", "text", rep("text", length(field_titles)),
             "dropdown", "dropdown", if (has_aktiv) "checkbox"),
    readOnly = c(TRUE, TRUE, rep(FALSE, length(field_titles)), FALSE, FALSE,
                 if (has_aktiv) FALSE),
    align = "left",
    stringsAsFactors = FALSE)
  out$source <- c(rep(list(NA), 2 + length(field_titles)),
                  list(parent_src), list(niveau_src),
                  if (has_aktiv) list(NA))
  # Fraktil-baserede bredder (enkelte lange navne må ikke trække kolonnen
  # bred). Dropdown-kolonner måles på deres viste labels, ikke id'erne.
  disp <- hierarchy_excel_data(tree_df, cfg)
  disp[["Forælder"]] <- .excel_dropdown_display(disp[["Forælder"]], parent_src)
  disp[["Niveau"]] <- .excel_dropdown_display(disp[["Niveau"]], niveau_src)
  out$width <- unname(excel_col_widths(disp)[out$title])
  out
}

