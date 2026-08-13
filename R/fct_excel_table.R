# excelR-hjælpelag til inline-redigering (jspreadsheet CE via excelR).
# Erstatter DT's editable-celler: hele tabellen redigeres i et regnearks-grid,
# FK-kolonner som native dropdowns ({id, name}-source: label vises, id gemmes).
# Rene funktioner — fuldt unit-testbare uden session/JS. Se test-excel-table.R.

#' Excel-lignende CSS-tema til jspreadsheet-grids. Portet fra biSPCharts
#' (utils_ui_app_layout.R) så editorerne ser ens ud på tværs af apps.
#' Inkluderes ÉN gang i app_ui (header) — ikke pr. modul-instans.
#' @noRd
.jexcel_theme_css <- function() {
  tags$style(HTML("
    .jexcel_container {
      width: none !important;
      height: auto !important;
      padding-bottom: 25px !important;
      border: none !important;
    }
    .jexcel thead td {
      background: #f3f3f3;
      border-bottom: 2px solid #bfbfbf;
      font-weight: 600;
      white-space: nowrap;
    }
    .jexcel tbody tr:nth-child(odd) { background-color: #f9f9f9; }
    .jexcel tbody tr:nth-child(even) { background-color: #ffffff; }
    .jexcel tbody tr:hover { background-color: #d8eef9 !important; }
    .jexcel td {
      border: 1px solid #d9d9d9;
      padding: 4px 8px;
    }
    .jexcel .highlight { background-color: #99d8f6 !important; }
    .jexcel_content {
      overflow-y: unset !important;
      max-height: none !important;
      margin-bottom: 25px !important;
    }
    .jexcel > thead > tr > td { position: unset !important; }
  "))
}

#' Kolonne-spec til excelR::excelTable fra et LOOKUP_TABLES-cfg.
#'
#' pk-kolonnen er readOnly (numeric); cfg-type "int" → numeric, "fk" →
#' dropdown med {id, name}-source, alt andet → text. Kolonner uden cfg-meta
#' (og en fk-kolonne hvis source mangler, fx fejlet fk_options-hentning)
#' låses som readOnly text — hellere en låst kolonne end en tom dropdown
#' der kan slette data.
#'
#' @param cfg LOOKUP_TABLES-element (pk + cols-liste)
#' @param col_names kolonnenavne i data-rækkefølge (typisk names(rows))
#' @param fk_sources named list: fk-kolonnenavn → data.frame(id, label)
#' @return data.frame(title, type, readOnly) + list-kolonnen source
#' @noRd
lookup_excel_columns <- function(cfg, col_names, fk_sources = list()) {
  meta <- stats::setNames(cfg$cols, vapply(cfg$cols, function(c) c$col, ""))
  spec <- lapply(col_names, function(nm) {
    m <- meta[[nm]]
    if (identical(nm, cfg$pk)) {
      list(type = "numeric", readOnly = TRUE, source = NA)
    } else if (!is.null(m) && identical(m$type, "fk")) {
      src <- fk_sources[[nm]]
      if (is.null(src) || !is.data.frame(src) || nrow(src) == 0) {
        list(type = "text", readOnly = TRUE, source = NA)
      } else {
        list(type = "dropdown", readOnly = FALSE,
          source = data.frame(id = src$id, name = as.character(src$label),
            stringsAsFactors = FALSE))
      }
    } else if (!is.null(m) && identical(m$type, "int")) {
      list(type = "numeric", readOnly = FALSE, source = NA)
    } else if (!is.null(m)) {
      list(type = "text", readOnly = FALSE, source = NA)
    } else {
      list(type = "text", readOnly = TRUE, source = NA)
    }
  })
  out <- data.frame(
    title = col_names,
    type = vapply(spec, function(s) s$type, ""),
    readOnly = vapply(spec, function(s) s$readOnly, TRUE),
    stringsAsFactors = FALSE
  )
  out$source <- lapply(spec, function(s) s$source)
  out
}

#' Kolonne-spec hvor kun `editable`-kolonner kan redigeres (som text);
#' alle øvrige — inkl. pk og afledte label-kolonner — er readOnly text.
#' Til indikator-oversigtens inline-redigering (INLINE_EDITABLE).
#' @noRd
excel_text_columns <- function(col_names, editable) {
  data.frame(
    title = col_names,
    type = "text",
    readOnly = !(col_names %in% editable),
    stringsAsFactors = FALSE
  )
}

#' Rekonstruér data.frame fra excelR's onChange-payload.
#'
#' payload$data er en liste af rækker (liste af celleværdier), payload$
#' colHeaders er kolonnenavne (titler = df-navne i vores opsætning). Alle
#' værdier returneres som character ("" og NULL → NA) — type-koercion sker
#' hos kalderen sammen med validering. Ugyldig payload → NULL; 0 rækker →
#' tom df med kolonnenavne.
#' @noRd
excel_payload_to_df <- function(payload) {
  if (is.null(payload) || is.null(payload$data) || is.null(payload$colHeaders)) {
    return(NULL)
  }
  cn <- as.character(unlist(payload$colHeaders))
  if (length(cn) == 0) {
    return(NULL)
  }
  cell_chr <- function(v) {
    if (is.null(v) || length(v) == 0 || (length(v) == 1 && is.na(v))) {
      return(NA_character_)
    }
    x <- as.character(v)[1]
    if (identical(x, "")) NA_character_ else x
  }
  rows <- lapply(payload$data, function(r) {
    r <- as.list(r)
    length(r) <- length(cn) # pad korte rækker (manglende celler → NA)
    vapply(r, cell_chr, "")
  })
  if (length(rows) == 0) {
    df <- as.data.frame(matrix(character(0), nrow = 0, ncol = length(cn)),
      stringsAsFactors = FALSE)
  } else {
    df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  }
  names(df) <- cn
  df
}

#' Ændrede celler mellem gammel df (typet) og ny character-df fra payload.
#'
#' Rækker matches på pk (som character) — rækkeorden er ikke garanteret
#' identisk, og pk er readOnly, så et pk-match er stabilt. pk-kolonnen selv
#' sammenlignes ikke; rækker med ukendt pk droppes (kan kun opstå ved
#' klient-manipulation — indsæt/slet er slået fra i tabellen).
#' Sammenligning som character med ""≡NA-normalisering.
#'
#' @return data.frame(pk, col, value) — value er den NYE værdi (character,
#'   NA = tømt celle). 0 rækker hvis intet ændret.
#' @noRd
excel_diff_cells <- function(old_df, new_df, pk) {
  empty <- data.frame(pk = character(0), col = character(0),
    value = character(0), stringsAsFactors = FALSE)
  if (is.null(old_df) || is.null(new_df) ||
    !pk %in% names(old_df) || !pk %in% names(new_df)) {
    return(empty)
  }
  norm <- function(x) {
    x <- as.character(x)
    x[!is.na(x) & x == ""] <- NA_character_
    x
  }
  cols <- setdiff(intersect(names(old_df), names(new_df)), pk)
  old_pk <- norm(old_df[[pk]])
  out <- list()
  for (i in seq_len(nrow(new_df))) {
    j <- match(norm(new_df[[pk]])[i], old_pk)
    if (is.na(j)) next
    for (cl in cols) {
      ov <- norm(old_df[[cl]][j])
      nv <- norm(new_df[[cl]][i])
      changed <- if (is.na(ov) || is.na(nv)) !identical(is.na(ov), is.na(nv)) else ov != nv
      if (changed) {
        out[[length(out) + 1]] <- data.frame(
          pk = old_pk[j], col = cl, value = nv, stringsAsFactors = FALSE)
      }
    }
  }
  if (length(out) == 0) empty else do.call(rbind, out)
}
