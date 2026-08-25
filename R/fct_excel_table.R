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
    /* Faste kolonnebredder (autoWidth=FALSE): lange vaerdier ombrydes i
       cellen i stedet for at traekke kolonnen bred. overflow-wrap fanger
       ogsaa lange tokens uden mellemrum (URL'er, tekniske navne). */
    .jexcel > tbody > tr > td {
      white-space: normal;
      overflow-wrap: anywhere;
      overflow: hidden;
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

#' JS-ekko-værn for excelR-grids (inkluderes ÉN gang i app_ui, header).
#'
#' excelR's widget-JS sender ved hvert re-render selv payloads på grid'ets
#' Shiny-input (data-ekko + selektions-ekko fra updateSelectionFromCoords).
#' Sammen med modulernes diff+reload-mønster kan en vedvarende
#' repræsentationsforskel (checkbox true/TRUE, tal-/datoformatering) blive
#' til et evigt gem→reload→ekko-loop uden fejlmeddelelse. Scriptet dropper
#' payloads affyret synkront under et jexcel-outputs re-render; se også
#' new_excel_echo_guard() (server-side værn, forsvar-i-dybden).
#' @noRd
.excel_echo_guard_dependency <- function() {
  htmltools::htmlDependency(
    name = "bfh-excel-echo-guard",
    version = "0.1.0",
    src = c(file = app_sys("www")),
    script = "bfh-excel-echo-guard.js"
  )
}

#' Signatur for et diff-resultat fra excel_diff_cells — bruges af
#' ekko-værnet til at genkende, at præcis samme diff kommer igen.
#' Separatorer er kontroltegn, der ikke kan optræde i celleværdier fra
#' payloadens JSON — en værdi kan altså ikke forfalske en anden signatur.
#' @noRd
excel_changes_signature <- function(changes) {
  if (is.null(changes) || !is.data.frame(changes) || nrow(changes) == 0) {
    return("")
  }
  paste(changes$pk, changes$col,
        ifelse(is.na(changes$value), "\u0001NA", changes$value),
        sep = "\u0002", collapse = "\u0003")
}

#' Server-side ekko-værn mod gem/reload-loops (forsvar-i-dybden bag
#' .excel_echo_guard_dependency's JS-værn).
#'
#' Brug: kald arm(changes) når en behandlet diff kan udløse et re-render
#' (reload/refresh) — og skip(changes) FØR behandling af næste ikke-tomme
#' diff. Returnerer skip TRUE præcis når diffen er identisk med den armerede
#' (dvs. payloadet er grid'ets re-render-ekko af den diff, vi netop har
#' behandlet — at behandle den igen ville gentage cyklussen i det uendelige).
#' Værnet forbruges ved første ikke-tomme diff (ens eller ej), så en ægte
#' gentaget brugerhandling højst ignoreres én gang lige efter et fejlet gem.
#' @return list(arm = function(changes), skip = function(changes))
#' @noRd
new_excel_echo_guard <- function() {
  armed <- ""
  list(
    arm = function(changes) {
      armed <<- excel_changes_signature(changes)
      invisible(NULL)
    },
    skip = function(changes) {
      sig <- excel_changes_signature(changes)
      hit <- nzchar(sig) && identical(sig, armed)
      armed <<- ""
      hit
    }
  )
}

#' Kolonnebredder fra indholdets længde-fordeling.
#'
#' Bredden dækker q-fraktilen (default 95 %) af værdilængderne — enkelte
#' ekstremt lange værdier trækker altså IKKE hele kolonnen bred; de
#' ombrydes i cellen i stedet (autoWidth sætter white-space: normal).
#' Kolonnetitlen skal altid kunne læses, og alt clampes til [min_px,
#' max_px].
#'
#' @param data df med DISPLAY-værdier — dropdown-kolonner skal være mappet
#'   til labels før kald (id-længder siger intet om visningsbredden)
#' @noRd
excel_col_widths <- function(data, q = 0.95, min_px = 70, max_px = 320,
                             px_per_char = 8, padding_px = 24) {
  vapply(names(data), function(nm) {
    v <- as.character(data[[nm]])
    v <- v[!is.na(v)]
    qlen <- if (length(v) == 0) {
      0
    } else {
      stats::quantile(nchar(v), q, names = FALSE)
    }
    chars <- max(qlen, nchar(nm))
    round(min(max_px, max(min_px, chars * px_per_char + padding_px)))
  }, numeric(1))
}

#' Display-tekst for en dropdown-kolonne: id'er → source-labels (ukendte
#' id'er beholdes rå). Bruges KUN til breddeberegning.
#' @noRd
.excel_dropdown_display <- function(values, source) {
  idx <- match(as.character(values), as.character(source$id))
  out <- as.character(source$name)[idx]
  out[is.na(idx)] <- as.character(values)[is.na(idx)]
  out
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
#' @param data valgfri rækker: sætter fraktil-baserede kolonnebredder
#'   (excel_col_widths; fk-kolonner måles på labels). NULL → jexcel auto.
#' @return data.frame(title, type, readOnly) + list-kolonnen source
#' @noRd
lookup_excel_columns <- function(cfg, col_names, fk_sources = list(),
                                 data = NULL) {
  meta <- stats::setNames(cfg$cols, vapply(cfg$cols, function(c) c$col, ""))
  spec <- lapply(col_names, function(nm) {
    m <- meta[[nm]]
    if (identical(nm, cfg$pk)) {
      # pk skjules helt (type "hidden"): den skal med i data/payload til
      # diff-matching, men brugeren behøver aldrig se den
      list(type = "hidden", readOnly = TRUE, source = NA)
    } else if (!is.null(m) && identical(m$type, "fk")) {
      # parent_id i sourcen → vis som indrykket trae (depth-first);
      # flade fk-lister passerer uaendret igennem
      src <- hierarchy_indent_options(fk_sources[[nm]])
      if (is.null(src) || !is.data.frame(src) || nrow(src) == 0) {
        list(type = "text", readOnly = TRUE, source = NA)
      } else {
        # "autocomplete" er jexcels dropdown MED tekst-filter: brugeren kan
        # skrive en del af navnet i stedet for at scrolle (org-traeet er
        # 500+ enheder). Samme {id, name}-source og samme gemte vaerdi.
        list(type = "autocomplete", readOnly = FALSE,
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
    align = "left", # jexcel centrerer som default — venstrestil alle celler
    stringsAsFactors = FALSE
  )
  out$source <- lapply(spec, function(s) s$source)
  if (!is.null(data)) {
    disp <- data[, intersect(col_names, names(data)), drop = FALSE]
    for (i in seq_along(col_names)) {
      src <- out$source[[i]]
      # Baade "dropdown" og "autocomplete" viser labels frem for id'er —
      # begge maales paa label-laengden (ellers bliver kolonnen id-smal).
      if (out$type[i] %in% c("dropdown", "autocomplete") && is.data.frame(src) &&
        col_names[i] %in% names(disp)) {
        disp[[col_names[i]]] <- .excel_dropdown_display(
          disp[[col_names[i]]], src)
      }
    }
    out$width <- unname(excel_col_widths(disp)[col_names])
  }
  out
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

#' Grid'ets fulde indhold fra ET excelR-event — uanset payload-form.
#'
#' excelR's JS sender baade onchange- og onselection-payloads paa SAMME
#' Shiny-input UDEN priority:"event". Naar en celle-commit straks foelges af
#' en markoer-flytning (Enter/klik — jexcel kalder begge i samme JS-task),
#' OVERSKRIVER selektions-payloaden aendrings-payloaden i Shinys input-batch,
#' og serveren ser aldrig aendringen. Selektions-payloadens fullData baerer
#' dog hele grid'ets aktuelle indhold — saa diff'es der ogsaa paa
#' selektions-events (via denne helper), reddes den klobrede aendring.
#'
#' @return data.frame som excel_payload_to_df, eller NULL (payload uden
#'   colHeaders, fx aeldre/partielle fullData-former → ingen diff)
#' @noRd
excel_event_df <- function(p) {
  if (isTRUE(p$forSelectedVals)) {
    excel_payload_to_df(p$fullData)
  } else {
    excel_payload_to_df(p)
  }
}

#' pk-værdien for den valgte række i et excelR onSelection-payload.
#'
#' Læses fra payloadens fullData — grid'ets AKTUELLE rækkefølge — og IKKE
#' fra række-positionen alene: med kolonne-sortering slået til er den
#' visuelle rækkefølge klient-styret, og et positions-opslag i serverens
#' df ville ramme en anden række. Forudsætter at pk er FØRSTE kolonne i
#' grid-data (konvention i alle vores grids; skjult via type "hidden" —
#' skjulte kolonner er stadig med i payload-data).
#' @return pk som character, eller NULL (ingen/ugyldig selektion)
#' @noRd
excel_selected_pk <- function(p) {
  top <- p$selectedDataBoundary$borderTop
  rows <- p$fullData$data
  if (is.null(top) || is.null(rows)) {
    return(NULL)
  }
  i <- suppressWarnings(as.integer(top)) + 1L
  if (is.na(i) || i < 1 || i > length(rows)) {
    return(NULL)
  }
  v <- rows[[i]][[1]]
  if (is.null(v) || length(v) == 0 || is.na(v)) NULL else as.character(v)
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
