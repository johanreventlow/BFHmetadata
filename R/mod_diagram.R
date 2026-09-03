# Diagram-CRUD (admin): filterbar oversigt over ALLE diagrammer + delt
# formular-modal. Formularen (.diagram_form_ui) genbruges af indikator-modalen
# (lock_indikator = TRUE) — deraf ren funktion frem for modul-intern UI.

#' Delt formular-UI for diagram. vals = named list (NULL → ny), opts =
#' list(indikator=df, org=df, type=df, periode=chr). Returnerer tagList —
#' kalderen wrapper i modalDialog.
#' @noRd
.diagram_indicator_initial_choices <- function(indicators, selected = NULL) {
  selected <- suppressWarnings(as.integer(selected))
  empty <- c("(v\u00E6lg)" = "")
  if (length(selected) != 1L || is.na(selected)) return(empty)

  idx <- match(selected, indicators$id)
  label <- if (is.na(idx)) {
    sprintf("Ukendt indikator #%d", selected)
  } else {
    as.character(indicators$label[[idx]])
  }
  c(empty, stats::setNames(as.character(selected), label))
}

#' @noRd
.diagram_indicator_choices <- function(indicators) {
  stats::setNames(as.character(indicators$id), as.character(indicators$label))
}

#' @noRd
.update_diagram_indicator <- function(session, indicators, selected = "") {
  choices <- .diagram_indicator_choices(indicators)
  selected_choice <- .diagram_indicator_initial_choices(indicators, selected)
  selected_choice <- selected_choice[unname(selected_choice) != ""]
  if (length(selected_choice) > 0L &&
      !unname(selected_choice) %in% unname(choices)) {
    choices <- c(choices, selected_choice)
  }
  updateSelectizeInput(session, "d_indikator",
    choices = choices, selected = selected,
    server = TRUE)
}

#' @noRd
.diagram_form_ui <- function(ns, vals = NULL, opts, lock_indikator = FALSE) {
  is_new <- is.null(vals)
  v <- function(col, default = NULL) if (is_new) default else vals[[col]]
  ch <- function(d) stats::setNames(d$id, d$label)

  ind_value <- v("indikator")
  ind_choices <- .diagram_indicator_initial_choices(opts$indikator, ind_value)
  ind_sel <- selectizeInput(ns("d_indikator"), "Indikator",
    choices = ind_choices, selected = ind_value %||% "")
  if (lock_indikator) {
    # Låst: disabled inputs sender ikke værdi til Shiny → send via skjult
    # select og vis navnet i et disabled tekstfelt i stedet.
    locked_choices <- ind_choices[unname(ind_choices) != ""]
    ind_sel <- selectizeInput(ns("d_indikator"), "Indikator",
      choices = locked_choices, selected = ind_value %||% "")
    lbl <- if (length(locked_choices) == 0L) "" else names(locked_choices)[[1]]
    vis <- textInput(ns("d_indikator_vis"), "Indikator", value = lbl)
    vis <- htmltools::tagQuery(vis)$find("input")$addAttrs(disabled = NA)$allTags()
    ind_sel <- tagList(div(style = "display:none", ind_sel), vis)
  }

  tagList(
    ind_sel,
    selectInput(ns("d_organisatorisk_navn_teknisk"), "Organisatorisk enhed",
      choices = c("(v\u00E6lg)" = "", ch(opts$org)),
      selected = v("organisatorisk_navn_teknisk") %||% ""),
    selectInput(ns("d_diagram_type"), "Diagramtype",
      choices = c("(v\u00E6lg)" = "", ch(opts$type)),
      selected = v("diagram_type") %||% ""),
    selectInput(ns("d_periode_aggregering"), "Periode-aggregering",
      choices = c("(ingen)" = "", opts$periode),
      selected = v("periode_aggregering") %||% ""),
    selectInput(ns("d_maalgruppe"), "Målgruppe",
      choices = c("(ingen)" = "", ch(opts$maalgruppe)),
      selected = v("maalgruppe") %||% ""),
    div(class = "d-flex flex-wrap gap-4 pt-1",
      checkboxInput(ns("d_indgaar_i_aggregering"), "Indg\u00E5r i aggregering",
        value = isTRUE(v("indgaar_i_aggregering"))),
      # Opt-in: enhedens serie = egne r\u00E6kker + oprullede b\u00F8rn (m\u00E5 ALDRIG
      # s\u00E6ttes for kilder der selv leverer flere org-niveauer \u2014 dobbeltt\u00E6ller)
      checkboxInput(ns("d_aggreger_egne_og_boern"),
        "Aggreg\u00E9r egne data + b\u00F8rn",
        value = isTRUE(v("aggreger_egne_og_boern"))),
      checkboxInput(ns("d_diagram_aktivt"), "Diagram aktivt",
        value = isTRUE(v("diagram_aktivt", default = TRUE))),
      checkboxInput(ns("d_direktionens_tavle"), "Direktionens tavle",
        value = isTRUE(v("direktionens_tavle")))))
}

#' Saml formular-inputs → named list i DIAGRAM_COLS-orden. Tomme selects → NA.
#' @noRd
.collect_diagram_form <- function(input, prefix = "d_") {
  gv <- function(col) input[[paste0(prefix, col)]]
  int_or_na <- function(x) {
    if (is.null(x) || identical(x, "")) NA_integer_ else as.integer(x)
  }
  chr_or_na <- function(x) {
    if (is.null(x) || identical(x, "")) NA_character_ else as.character(x)
  }
  list(
    indikator = int_or_na(gv("indikator")),
    organisatorisk_navn_teknisk = int_or_na(gv("organisatorisk_navn_teknisk")),
    diagram_type = int_or_na(gv("diagram_type")),
    periode_aggregering = chr_or_na(gv("periode_aggregering")),
    indgaar_i_aggregering = isTRUE(gv("indgaar_i_aggregering")),
    aggreger_egne_og_boern = isTRUE(gv("aggreger_egne_og_boern")),
    diagram_aktivt = isTRUE(gv("diagram_aktivt")),
    direktionens_tavle = isTRUE(gv("direktionens_tavle")),
    maalgruppe = int_or_na(gv("maalgruppe")))
}

# Grid-titler → tblDiagrammer-kolonner for inline-redigering (excelR)
.DIAGRAM_GRID_FIELDS <- c(
  Indikator = "indikator", Enhed = "organisatorisk_navn_teknisk",
  Type = "diagram_type", Periode = "periode_aggregering",
  Målgruppe = "maalgruppe",
  Aggregering = "indgaar_i_aggregering",
  "Egne+børn" = "aggreger_egne_og_boern",
  Aktiv = "diagram_aktivt",
  Tavle = "direktionens_tavle"
)

#' Fuld DIAGRAM_COLS-værdiliste fra en admin-række. Autoritativ basis for
#' inline-patch: én celle ændres, resten bevares som de ER i DB — så en
#' update aldrig kan nulstille felter brugeren ikke rørte.
#' @noRd
.diagram_row_values <- function(row) {
  int_or_na <- function(x) {
    if (is.null(x) || is.na(x)) NA_integer_ else as.integer(x)
  }
  list(
    indikator = int_or_na(row$indikator),
    organisatorisk_navn_teknisk = int_or_na(row$organisatorisk_navn_teknisk),
    diagram_type = int_or_na(row$diagram_type),
    periode_aggregering = if (is.na(row$periode_aggregering)) {
      NA_character_
    } else {
      as.character(row$periode_aggregering)
    },
    indgaar_i_aggregering = isTRUE(row$indgaar_i_aggregering),
    # NULL-tolerant (isTRUE(NULL) = FALSE): admin-df'er fra før kolonnen
    # fandtes må ikke vælte patch-flowet
    aggreger_egne_og_boern = isTRUE(row$aggreger_egne_og_boern),
    diagram_aktivt = isTRUE(row$diagram_aktivt),
    direktionens_tavle = isTRUE(row$direktionens_tavle),
    maalgruppe = int_or_na(row$maalgruppe)
  )
}

#' Grid-data til excelR: pk (hidden) + kontekst (Datapakke/Datasæt,
#' readOnly) + redigerbare felter. FK-felter som character-id'er (matcher
#' dropdown-sourcens id-format); flag som logicals (checkbox-celler).
#' @noRd
diagram_excel_data <- function(d) {
  chr_or_empty <- function(x) ifelse(is.na(x), "", as.character(x))
  out <- data.frame(diagram_id = d$diagram_id, stringsAsFactors = FALSE,
                    check.names = FALSE)
  out[["Datapakke"]] <- chr_or_empty(d$datapakke)
  out[["Datas\u00E6t"]] <- chr_or_empty(d$datasaet)
  out[["Indikator"]] <- chr_or_empty(d$indikator)
  out[["Enhed"]] <- chr_or_empty(d$organisatorisk_navn_teknisk)
  out[["Type"]] <- chr_or_empty(d$diagram_type)
  out[["Periode"]] <- chr_or_empty(d$periode_aggregering)
  out[["Målgruppe"]] <- chr_or_empty(d$maalgruppe)
  out[["Aggregering"]] <- d$indgaar_i_aggregering %in% TRUE
  # %||%-fallback: ældre admin-df'er uden kolonnen viser blot FALSE frem
  # for at vælte grid-renderingen
  out[["Egne+børn"]] <-
    (d$aggreger_egne_og_boern %||% rep(FALSE, nrow(d))) %in% TRUE
  out[["Aktiv"]] <- d$diagram_aktivt %in% TRUE
  out[["Tavle"]] <- d$direktionens_tavle %in% TRUE
  out
}

#' Kolonne-spec til diagram-grid'et: FK-felter som dropdowns med {id, name}-
#' source (Indikator/Enhed med autocomplete — listerne er lange), Periode
#' med "(ingen)"-tomvalg, flag som checkbokse. Ukendte eksisterende id'er
#' tilføjes sourcen ("Ukendt … #id") så de hverken vises blankt eller tabes.
#' @noRd
diagram_excel_columns <- function(d, opts, periode) {
  src_of <- function(df, used, prefix) {
    s <- data.frame(id = as.character(df$id), name = as.character(df$label),
                    stringsAsFactors = FALSE)
    unknown <- setdiff(stats::na.omit(as.character(used)), s$id)
    if (length(unknown) > 0) {
      s <- rbind(s, data.frame(id = unknown,
        name = sprintf("%s #%s", prefix, unknown), stringsAsFactors = FALSE))
    }
    s
  }
  ind_src <- src_of(opts$indikator, d$indikator, "Ukendt indikator")
  org_src <- src_of(opts$org, d$organisatorisk_navn_teknisk, "Ukendt enhed")
  type_src <- src_of(opts$type, d$diagram_type, "Ukendt type")
  per_vals <- unique(c(as.character(periode),
    stats::na.omit(as.character(d$periode_aggregering))))
  per_src <- data.frame(id = c("", per_vals), name = c("(ingen)", per_vals),
                        stringsAsFactors = FALSE)
  mg_src <- src_of(opts$maalgruppe, d$maalgruppe, "Ukendt m\u00E5lgruppe")
  titles <- c("diagram_id", "Datapakke", "Datas\u00E6t", names(.DIAGRAM_GRID_FIELDS))
  out <- data.frame(
    title = titles,
    type = c("hidden", "text", "text", rep("dropdown", 5),
             rep("checkbox", 4)),
    readOnly = c(TRUE, TRUE, TRUE, rep(FALSE, 9)),
    align = "left",
    autocomplete = titles %in% c("Indikator", "Enhed"),
    stringsAsFactors = FALSE)
  out$source <- c(rep(list(NA), 3),
                  list(ind_src), list(org_src), list(type_src), list(per_src),
                  list(mg_src), rep(list(NA), 4))
  # Fraktil-bredder målt på VISTE labels (ikke id'erne)
  disp <- diagram_excel_data(d)
  disp[["Indikator"]] <- .excel_dropdown_display(disp[["Indikator"]], ind_src)
  disp[["Enhed"]] <- .excel_dropdown_display(disp[["Enhed"]], org_src)
  disp[["Type"]] <- .excel_dropdown_display(disp[["Type"]], type_src)
  disp[["Periode"]] <- .excel_dropdown_display(disp[["Periode"]], per_src)
  disp[["Målgruppe"]] <- .excel_dropdown_display(disp[["Målgruppe"]], mg_src)
  out$width <- unname(excel_col_widths(disp)[titles])
  out
}

#' Per-række-filter på grid'ets Indikator-dropdown: kun indikatorer hvis
#' niveau-udledte datasæt matcher rækkens Datasæt-celle. jexcel kalder
#' column.filter ved editor-åbning; funktionen injiceres efter render via
#' onRender (excelR kan ikke serialisere JS-funktioner i column-spec'en).
#' Rækker uden datasæt får hele listen; rækkens nuværende værdi medtages
#' altid, så visningen aldrig knækker. ind_opts uden datasæt-kolonne (fx
#' ældre fake/DB-form) → uændret widget (intet filter).
#' @param widget excelR-htmlwidget
#' @param ind_opts df(id, label, datasaet) fra db$diagram_form_options()
#' @param grid_names names(diagram_excel_data(d)) — kolonneindeks slås op her
#' @noRd
.diagram_attach_indikator_filter <- function(widget, ind_opts, grid_names) {
  if (is.null(ind_opts) || !"datasaet" %in% names(ind_opts)) {
    return(widget)
  }
  ds <- as.character(ind_opts$datasaet)
  ok <- !is.na(ds) & nzchar(ds)
  map <- stats::setNames(as.list(ds[ok]), as.character(ind_opts$id[ok]))
  if (length(map) == 0) {
    return(widget)
  }
  htmlwidgets::onRender(widget, paste0(
    "function(el, x, data) {",
    " var ex = el.excel;",
    " if (!ex || !data || !data.map) return;",
    " ex.options.columns[data.indikatorCol].filter =",
    "  function(instance, cell, c, r, source) {",
    "   var ds = ex.options.data[r][data.datasaetCol];",
    "   if (!ds) return source;",
    "   var cur = String(ex.options.data[r][c]);",
    "   var out = source.filter(function(s) {",
    "    return data.map[String(s.id)] === ds || String(s.id) === cur;",
    "   });",
    "   return out.length ? out : source;",
    "  };",
    "}"
  ), data = list(
    map = map,
    datasaetCol = match("Datas\u00E6t", grid_names) - 1L,
    indikatorCol = match("Indikator", grid_names) - 1L
  ))
}

#' @noRd
mod_diagram_ui <- function(id) {
  ns <- NS(id)
  div(class = "mt-2",
    div(class = "d-flex justify-content-end gap-2 mb-2",
      actionButton(ns("new_diagram"), "Nyt diagram", class = "btn-success"),
      actionButton(ns("delete_row"), "Slet valgte r\u00E6kke",
                   class = "btn-outline-danger"),
      actionButton(ns("select_all_visible"), "V\u00E6lg alle viste",
                   class = "btn-outline-secondary"),
      uiOutput(ns("bulk_edit_btn"), inline = TRUE),
      uiOutput(ns("bulk_undo_btn"), inline = TRUE)),
    bslib::layout_columns(
      col_widths = c(2, 2, 3, 3, 2),
      uiOutput(ns("filter_datapakke_ui")),
      uiOutput(ns("filter_datasaet_ui")),
      uiOutput(ns("filter_indikator_ui")),
      uiOutput(ns("filter_org_ui")),
      selectInput(ns("filter_status"), "Status",
        choices = c("Kun aktive" = "aktive", "Alle" = "alle",
                    "Kun inaktive" = "inaktive"),
        selected = "aktive")),
    p(class = "text-muted small",
      "Dobbeltklik en celle for at redigere. Klik en r\u00E6kke og tryk Slet."),
    excelR::excelOutput(ns("tbl"), width = "100%", height = "auto"))
}

#' @noRd
mod_diagram_server <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    admin <- reactiveVal(db$list_diagrams_admin())
    opts <- db$diagram_form_options()
    # Kanonisk ordforråd + værdier i brug: DISTINCT-udtrækket alene kunne
    # aldrig tilbyde en NY periode (fx "dag"/"kvartal" før første række har
    # den) — se periode_choices/PERIODE_AGGREGERING_CHOICES.
    opts$periode <- periode_choices(db$diagram_periode_choices())
    status_msg <- reactiveVal("")
    warn_msg <- reactiveVal("")
    grid_sel <- reactiveVal(character(0)) # pk-vektor (chr) for den valgte range

    # Selektionen begrænset til rækker der faktisk er i den VISTE (filtrerede)
    # tabel lige nu — et filterskift efterlader ikke en optælling der peger på
    # skjulte rækker.
    grid_sel_visible <- reactive({
      intersect(grid_sel(), as.character(filtered()$diagram_id))
    })
    grid_refresh <- reactiveVal(0) # bump → snap-back efter afvist inline-edit
    # Ekko-værn: excelR re-sender data+selektion efter hvert re-render — en
    # identisk diff lige efter et reload er grid'ets ekko, ikke brugeren.
    echo_guard <- new_excel_echo_guard()
    # Fejl-tolerant genindlæsning: DB-udfald må aldrig vælte sessionen —
    # behold senest hentede rækker og sig det højt.
    reload <- function() {
      safe_operation("genindl\u00E6s diagrammer", admin(db$list_diagrams_admin()),
        fallback = status_msg("Databasen svarer ikke \u2014 viser senest hentede data"))
    }

    # Flydende notifikationer (synlige over modal, jf. mod_indikator_crud)
    observeEvent(status_msg(), {
      if (nzchar(status_msg())) showNotification(status_msg(), duration = 5)
    }, ignoreInit = TRUE)
    observeEvent(warn_msg(), {
      if (nzchar(warn_msg()))
        showNotification(warn_msg(), type = "warning", duration = 8)
    }, ignoreInit = TRUE)

    # Filter-valg afledt af admin-data (kun værdier der faktisk findes).
    # d kan være pre-filtreret: kaskaden lader dimensionerne OVENFOR
    # (Datapakke → Datasæt → Indikator) begrænse valgene nedenfor.
    # Ugyldiggjorte valg ryddes ved re-render (.preserved_filter_selection).
    .filter_ui <- function(input_id, lab, col, d = admin()) {
      ns <- session$ns
      vals <- sort(unique(stats::na.omit(d[[col]])))
      choices <- c("Alle" = "", stats::setNames(vals, vals))
      selected <- .preserved_filter_selection(
        isolate(input[[input_id]]), choices)
      selectInput(ns(input_id), lab, choices = choices, selected = selected)
    }
    # Kaskade-basis: admin-rækker under valgt datapakke (hhv. + datasæt)
    .under_pakke <- reactive({
      d <- admin()
      fdp <- input$filter_datapakke
      if (!is.null(fdp) && nzchar(fdp)) {
        d <- d[d$datapakke %in% fdp, , drop = FALSE]
      }
      d
    })
    .under_datasaet <- reactive({
      d <- .under_pakke()
      fds <- input$filter_datasaet
      if (!is.null(fds) && nzchar(fds)) {
        d <- d[d$datasaet %in% fds, , drop = FALSE]
      }
      d
    })
    output$filter_indikator_ui <- renderUI(
      .filter_ui("filter_indikator", "Indikator", "indikator_navn",
                 .under_datasaet()))
    output$filter_datapakke_ui <- renderUI(
      .filter_ui("filter_datapakke", "Datapakke", "datapakke"))
    output$filter_datasaet_ui <- renderUI(
      .filter_ui("filter_datasaet", "Datas\u00E6t", "datasaet", .under_pakke()))

    # Org-filter: hierarkisk dropdown over HELE org-træet (id-værdier), så
    # en overordnet enhed kan vælges selv om den ikke selv har diagrammer —
    # filtreringen medtager alle underliggende enheder (se filtered()).
    # NULL ved fejlet trae-hentning → flad liste + kun-enheden-selv-filter.
    org_tree <- safe_operation("hent org-tr\u00E6", db$org_struct(), fallback = NULL)
    output$filter_org_ui <- renderUI({
      ns <- session$ns
      admin() # re-render ved reload som de øvrige filtre (bevar valg eksplicit)
      choices <- c("Alle" = "", org_hierarchy_choices(org_tree, opts$org))
      selected <- .preserved_filter_selection(
        isolate(input$filter_org), choices)
      selectInput(ns("filter_org"), "Organisatorisk enhed",
        choices = choices, selected = selected)
    })

    filtered <- reactive({
      d <- admin()
      status <- input$filter_status %||% "aktive"
      if (identical(status, "aktive"))
        d <- d[d$diagram_aktivt %in% TRUE, , drop = FALSE]
      if (identical(status, "inaktive"))
        d <- d[!(d$diagram_aktivt %in% TRUE), , drop = FALSE]
      fi <- input$filter_indikator
      if (!is.null(fi) && nzchar(fi))
        d <- d[d$indikator_navn %in% fi, , drop = FALSE]
      fo <- input$filter_org
      if (!is.null(fo) && nzchar(fo)) {
        # Valgt enhed + alle underliggende (org-id-match, ej navne-match)
        ids <- org_subtree_ids(org_tree, as.integer(fo))
        d <- d[d$organisatorisk_navn_teknisk %in% ids, , drop = FALSE]
      }
      fdp <- input$filter_datapakke
      if (!is.null(fdp) && nzchar(fdp))
        d <- d[d$datapakke %in% fdp, , drop = FALSE]
      fds <- input$filter_datasaet
      if (!is.null(fds) && nzchar(fds))
        d <- d[d$datasaet %in% fds, , drop = FALSE]
      d
    })

    output$tbl <- excelR::renderExcel({
      grid_refresh()
      d <- filtered()
      grid_data <- diagram_excel_data(d)
      w <- excelR::excelTable(
        data = grid_data,
        columns = diagram_excel_columns(d, opts, opts$periode),
        autoColTypes = FALSE,
        # FALSE: ellers deaktiverer width:auto table-layout:fixed, og
        # celleindholdet vinder over de beregnede kolonnebredder
        autoWidth = FALSE,
        # Kolonne-sortering TILLADT: diff og selektion er pk-baserede, så en
        # klient-sorteret rækkefølge er ufarlig (ren visning, nulstilles ved
        # re-render efter gem/filter).
        allowInsertRow = FALSE, allowInsertColumn = FALSE,
        allowDeleteRow = FALSE, allowDeleteColumn = FALSE,
        allowRenameColumn = FALSE, columnSorting = TRUE,
        rowDrag = FALSE, columnDrag = FALSE,
        getSelectedData = TRUE
      )
      .diagram_attach_indikator_filter(w, opts$indikator, names(grid_data))
    })

    # excelR sender BÅDE celle-ændringer og selektioner på input$tbl.
    # Ændringer diffes mod den VISTE (filtrerede) tabel via pk; hver ændret
    # celle patches ind i den fulde DB-række (.diagram_row_values), valideres
    # og gemmes — så en inline-edit aldrig nulstiller urørte felter.
    observeEvent(input$tbl, {
      p <- input$tbl
      if (isTRUE(p$forSelectedVals)) {
        # pk'er fra payloadens fullData — robust under klient-side sortering.
        # Dækker range-selektion (borderTop..borderBottom), ikke kun ét klik.
        grid_sel(excel_selected_pks(p) %||% character(0))
      }
      # Diff også på selektions-payloads: markør-flytningen efter en
      # celle-commit overskriver change-eventet i Shinys input-batch
      # (se excel_event_df) — fullData bærer ændringen.
      d <- filtered()
      changes <- excel_diff_cells(diagram_excel_data(d),
                                  excel_event_df(p), "diagram_id")
      changes <- changes[changes$col %in% names(.DIAGRAM_GRID_FIELDS), ,
                         drop = FALSE]
      if (nrow(changes) == 0) {
        return()
      }
      if (echo_guard$skip(changes)) {
        return()
      }
      revert <- FALSE
      for (k in seq_len(nrow(changes))) {
        rid <- as.integer(changes$pk[k])
        row <- admin()[admin()$diagram_id == rid, , drop = FALSE]
        if (nrow(row) == 0) next
        vals <- .diagram_row_values(row[1, ])
        field <- .DIAGRAM_GRID_FIELDS[[changes$col[k]]]
        val <- changes$value[k]
        if (field %in% c("indikator", "organisatorisk_navn_teknisk",
                         "diagram_type")) {
          iv <- suppressWarnings(as.integer(val))
          if (is.na(iv)) { # tømt/ugyldig FK-celle → afvis + snap tilbage
            status_msg("V\u00E6lg en v\u00E6rdi fra listen")
            revert <- TRUE
            next
          }
          vals[[field]] <- iv
        } else if (identical(field, "periode_aggregering")) {
          vals[[field]] <- if (is.na(val)) NA_character_ else val
        } else if (identical(field, "maalgruppe")) {
          # Valgfri FK ("(ingen)" er gyldig) — tom celle → NA, ellers heltal
          vals[[field]] <- if (is.na(val)) {
            NA_integer_
          } else {
            suppressWarnings(as.integer(val))
          }
        } else {
          vals[[field]] <- identical(val, "TRUE")
        }
        errs <- validate_diagram(vals)
        if (length(errs) > 0) {
          status_msg(paste(errs, collapse = "; "))
          revert <- TRUE
          next
        }
        # Blød duplikat-guard som i modal-gem: advar men blokér ikke
        dup <- db$diagram_duplicate_count(vals$indikator,
          vals$organisatorisk_navn_teknisk, vals$diagram_type,
          exclude_id = rid)
        if (dup > 0) {
          warn_msg(paste("Findes allerede: samme indikator/enhed/type har",
                         "et diagram i forvejen."))
        }
        ok <- safe_operation("diagram-inline-gem", {
          db$update_diagram(rid, vals)
          TRUE
        }, fallback = FALSE)
        if (isTRUE(ok)) {
          status_msg(paste("Gemt diagram", rid))
        } else {
          status_msg("Fejl ved gem af diagram (se log)")
          revert <- TRUE
        }
      }
      reload() # genindlæs fra DB → grid viser den gemte tilstand
      if (revert) grid_refresh(grid_refresh() + 1)
      # reload() re-renderer grid'et (admin → filtered → renderExcel), og
      # excelR gen-sender payloads efter re-render — en identisk diff derfra
      # er ekkoet og må ikke behandles igen (gem→reload→ekko-loop).
      echo_guard$arm(changes)
    })

    observeEvent(input$delete_row, {
      sel <- grid_sel()
      d <- filtered()
      j <- if (length(sel) == 0) NA_integer_ else match(sel[1], as.character(d$diagram_id))
      if (is.na(j)) {
        status_msg("V\u00E6lg en r\u00E6kke f\u00F8rst")
        return()
      }
      rid <- d$diagram_id[j]
      # Pre-check: median-knæk gør sletning destruktiv → venlig blokering
      n <- db$diagram_median_count(rid)
      if (n > 0) {
        warn_msg(sprintf(paste("Diagrammet har %d median-kn\u00E6k \u2014 deaktiv\u00E9r i",
                               "stedet, eller slet kn\u00E6kkene f\u00F8rst."), n))
        return()
      }
      safe_operation("diagram-slet", {
        db$delete_diagram(rid)
        status_msg(paste("Slettet diagram", rid))
        grid_sel(character(0)) # rækken findes ikke længere — ryd stale selektion
        reload()
      }, fallback = status_msg("Fejl ved sletning (se log)"))
    })

    observeEvent(input$select_all_visible, {
      grid_sel(as.character(filtered()$diagram_id))
    })

    # "Redigér valgte (N)" — knap-tekst reflekterer selektionen reaktivt.
    output$bulk_edit_btn <- renderUI({
      ns <- session$ns
      n <- length(grid_sel_visible())
      actionButton(ns("bulk_edit"),
        sprintf("Redig\u00E9r valgte (%d)", n),
        class = if (n > 0) "btn-outline-primary" else "btn-outline-secondary",
        disabled = if (n == 0) "disabled" else NULL,
        title = if (n == 0) "V\u00E6lg mindst \u00E9n r\u00E6kke" else
          "S\u00E6t \u00E9t felt p\u00E5 alle valgte diagrammer"
      )
    })

    # --- Bulk-redigering: s\u00E6t \u00E9t felt p\u00E5 hele selektionen -------------------
    # Samme kontrakt som indikator-fanen (frosset m\u00E5ls\u00E6t, forh\u00E5ndsvisning,
    # atomisk batch med audit og fortryd), men med to diagram-specifikke
    # guards: et diagram er kun gyldigt som HEL r\u00E6kke, og duplikatn\u00F8glen g\u00E5r
    # p\u00E5 tv\u00E6rs af tre felter.
    bulk_frozen <- reactiveVal(NULL)
    last_batch <- reactiveVal(NULL)

    # col \u2192 dansk label, genbrugt fra grid'ets egne kolonnetitler
    bulk_labels <- stats::setNames(
      names(.DIAGRAM_GRID_FIELDS), unname(.DIAGRAM_GRID_FIELDS)
    )
    # FK-valglister til det typede input (samme kilder som grid-dropdowns)
    bulk_fk_choices <- list(
      maalgruppe = stats::setNames(opts$maalgruppe$id, opts$maalgruppe$label),
      diagram_type = stats::setNames(opts$type$id, opts$type$label)
    )

    bulk_fld <- reactive({
      felt <- input$bulk_felt
      if (is.null(felt) || !nzchar(felt)) {
        return(NULL)
      }
      bulk_field_config("diagram", felt)
    })

    bulk_target <- reactive({
      fld <- bulk_fld()
      if (is.null(fld)) {
        return(NULL)
      }
      raa <- input[[paste0("bulk_v_", fld$col)]]
      tryCatch(bulk_coerce_value(fld, raa), error = function(e) NULL)
    })

    bulk_preview <- reactive({
      fld <- bulk_fld()
      d <- bulk_frozen()
      target <- bulk_target()
      if (is.null(fld) || is.null(d) || nrow(d) == 0 || is.null(target)) {
        return(NULL)
      }
      bulk_preview_df(d, "diagram_id", ".bulk_label", fld, target,
        choices = bulk_fk_choices[[fld$col]]
      )
    })

    # R\u00E6kker der IKKE ville validere efter batchen. Beregnes reaktivt, s\u00E5
    # brugeren ser problemet i dialogen frem for efter et klik.
    bulk_valideringsfejl <- reactive({
      fld <- bulk_fld()
      d <- bulk_frozen()
      target <- bulk_target()
      if (is.null(fld) || is.null(d) || is.null(target)) {
        return(NULL)
      }
      bulk_diagram_validation_errors(d, fld$col, target)
    })

    observeEvent(input$bulk_edit, {
      sel <- grid_sel_visible()
      if (length(sel) == 0) {
        status_msg("V\u00E6lg mindst \u00E9n r\u00E6kke f\u00F8rst")
        return()
      }
      d <- filtered()
      fr <- d[as.character(d$diagram_id) %in% sel, , drop = FALSE]
      # Sigende etiket i forh\u00E5ndsvisningen: diagram-id alene siger intet, s\u00E5
      # r\u00E6kken vises som "indikator \u2014 enhed", ligesom grid'et g\u00F8r.
      fr$.bulk_label <- paste(
        ifelse(is.na(fr$indikator_navn), "?", fr$indikator_navn),
        ifelse(is.na(fr$org_navn), "?", fr$org_navn),
        sep = " \u2014 "
      )
      bulk_frozen(fr)
      ns <- session$ns
      showModal(modalDialog(
        title = sprintf("Redig\u00E9r %d valgte diagrammer", length(sel)),
        size = "l", easyClose = FALSE,
        selectInput(ns("bulk_felt"), "Felt der skal s\u00E6ttes",
          choices = c(
            "(v\u00E6lg felt)" = "",
            bulk_field_choices("diagram", bulk_labels)
          )
        ),
        uiOutput(ns("bulk_value_ui")),
        tags$hr(),
        uiOutput(ns("bulk_preview_ui")),
        footer = tagList(
          modalButton("Annull\u00E9r"),
          uiOutput(ns("bulk_confirm_btn"), inline = TRUE)
        )
      ))
    })

    output$bulk_value_ui <- renderUI({
      fld <- bulk_fld()
      if (is.null(fld)) {
        return(NULL)
      }
      .field_input(session$ns, fld, bulk_fk_choices,
        prefix = "bulk_v_",
        label = bulk_labels[[fld$col]] %||% fld$col
      )
    })

    output$bulk_preview_ui <- renderUI({
      fld <- bulk_fld()
      if (is.null(fld)) {
        return(p(class = "text-muted small",
                 "V\u00E6lg et felt for at se \u00E6ndringerne."))
      }
      pv <- bulk_preview()
      if (is.null(pv)) {
        return(p(class = "text-muted small",
                 "V\u00E6lg en v\u00E6rdi for at se \u00E6ndringerne."))
      }
      fejl <- bulk_valideringsfejl()
      vis <- utils::head(pv, 50)
      tagList(
        p(class = "small", sprintf(
          "%d af %d r\u00E6kker \u00E6ndres. %d har allerede v\u00E6rdien og springes over.",
          sum(!pv$uaendret), nrow(pv), sum(pv$uaendret)
        )),
        # Blokerende: hele batchen afvises, s\u00E5 brugeren skal se det F\u00D8R klik.
        if (!is.null(fejl) && nrow(fejl) > 0) {
          div(class = "alert alert-danger small",
            sprintf(paste("%d r\u00E6kker ville blive ugyldige og blokerer",
                          "hele batchen: %s"),
                    nrow(fejl),
                    paste(utils::head(fejl$fejl, 3), collapse = "; ")))
        },
        if (nrow(pv) > nrow(vis)) {
          p(class = "text-muted small",
            sprintf("Viser de f\u00F8rste %d af %d.", nrow(vis), nrow(pv)))
        },
        tags$table(class = "table table-sm small",
          tags$thead(tags$tr(
            tags$th("Diagram"), tags$th("Nu"), tags$th("Ny")
          )),
          tags$tbody(lapply(seq_len(nrow(vis)), function(i) {
            tags$tr(
              class = if (vis$uaendret[i]) "text-muted" else NULL,
              tags$td(vis$indikator[i]),
              tags$td(vis$nuvaerende[i]),
              tags$td(if (vis$uaendret[i]) "(u\u00E6ndret)" else vis$ny[i])
            )
          }))
        )
      )
    })

    output$bulk_confirm_btn <- renderUI({
      pv <- bulk_preview()
      fejl <- bulk_valideringsfejl()
      n <- if (is.null(pv)) 0L else sum(!pv$uaendret)
      blokeret <- !is.null(fejl) && nrow(fejl) > 0
      actionButton(session$ns("bulk_confirm"),
        sprintf("Skriv %d \u00E6ndringer", n),
        class = "btn-primary",
        disabled = if (n == 0 || blokeret) "disabled" else NULL
      )
    })

    observeEvent(input$bulk_confirm, {
      fld <- bulk_fld()
      d <- bulk_frozen()
      target <- bulk_target()
      if (is.null(fld) || is.null(d) || is.null(target)) {
        return()
      }
      # Sidste kontrol server-side: knappen er disabled ved fejl, men et
      # klient-manipuleret event m\u00E5 ikke kunne omg\u00E5 valideringen.
      fejl <- bulk_diagram_validation_errors(d, fld$col, target)
      if (nrow(fejl) > 0) {
        status_msg(sprintf(
          "Intet skrevet \u2014 %d r\u00E6kker ville blive ugyldige: %s",
          nrow(fejl), paste(utils::head(fejl$fejl, 3), collapse = "; ")
        ))
        return()
      }
      removeModal()
      res <- tryCatch(
        med_ventevisning("Skriver \u00E6ndringer\u2026", {
          db$bulk_update("diagram", as.integer(d$diagram_id), fld$col, target,
            bulk_expected_before(d, "diagram_id", fld)
          )
        }),
        bulk_conflict = function(e) e,
        error = function(e) e
      )
      bulk_frozen(NULL)
      if (inherits(res, "bulk_conflict")) {
        last_batch(NULL)
        status_msg(bulk_conflict_text(res))
        reload()
        return()
      }
      if (inherits(res, "error")) {
        last_batch(NULL)
        status_msg("Fejl ved bulk-redigering (se log)")
        message(sprintf("[ERROR] bulk-update: %s", conditionMessage(res)))
        return()
      }
      sprunget <- if (length(res$skipped) > 0) {
        sprintf(" (%d sprunget over)", length(res$skipped))
      } else {
        ""
      }
      if (res$n == 0 || is.na(res$batch_id)) {
        last_batch(NULL)
        status_msg(sprintf("Ingen \u00E6ndringer at skrive%s", sprunget))
      } else {
        last_batch(res$batch_id)
        status_msg(sprintf(
          "%d r\u00E6kker opdateret%s. Batch %s kan fortrydes.",
          res$n, sprunget, substr(res$batch_id, 1, 8)
        ))
        # Bl\u00F8d duplikat-guard: kun relevant n\u00E5r feltet indg\u00E5r i n\u00F8glen
        # (indikator/enhed/type). Advarer samlet, blokerer ikke \u2014 som i dag.
        if (bulk_diagram_rammer_duplikatnoegle(fld$col)) {
          dubletter <- safe_operation("bulk-duplikat-tjek", {
            sum(vapply(seq_len(nrow(d)), function(i) {
              v <- .diagram_row_values(d[i, ])
              v[[fld$col]] <- target
              db$diagram_duplicate_count(v$indikator,
                v$organisatorisk_navn_teknisk, v$diagram_type,
                exclude_id = as.integer(d$diagram_id[i])
              ) > 0
            }, logical(1)))
          }, fallback = 0L)
          if (dubletter > 0) {
            warn_msg(sprintf(paste("%d af r\u00E6kkerne deler nu indikator, enhed",
                                   "og type med et andet diagram."), dubletter))
          }
        }
      }
      reload()
    })

    output$bulk_undo_btn <- renderUI({
      if (is.null(last_batch())) {
        return(NULL)
      }
      actionButton(session$ns("bulk_undo"), "Fortryd seneste batch",
        class = "btn-outline-warning btn-sm"
      )
    })

    observeEvent(input$bulk_undo, {
      bid <- last_batch()
      if (is.null(bid)) {
        return()
      }
      res <- tryCatch(
        med_ventevisning("Fortryder\u2026", db$bulk_undo(bid)),
        bulk_conflict = function(e) e,
        error = function(e) e
      )
      if (inherits(res, "bulk_conflict")) {
        status_msg(bulk_conflict_text(res))
        reload()
        return()
      }
      if (inherits(res, "error")) {
        status_msg("Kunne ikke fortryde batchen (se log)")
        message(sprintf("[ERROR] bulk-undo: %s", conditionMessage(res)))
        return()
      }
      last_batch(NULL)
      status_msg(sprintf("Fortrudt \u2014 %d r\u00E6kker sat tilbage", res$n))
      reload()
    })

    # "Nyt diagram"-modal: oprettelse kræver mange felter på én gang —
    # formular-modal er bedre end en tom grid-række (redigering ER inline).
    observeEvent(input$new_diagram, {
      ns <- session$ns
      showModal(modalDialog(
        title = "Nyt diagram",
        size = "m", easyClose = FALSE,
        .diagram_form_ui(ns, NULL, opts),
        footer = div(class = "d-flex justify-content-end gap-2 w-100",
          modalButton("Annull\u00E9r"),
          actionButton(ns("d_save"), "Gem", class = "btn-primary"))))
      session$onFlushed(function() {
        .update_diagram_indicator(session, opts$indikator, "")
      }, once = TRUE)
    })

    observeEvent(input$d_save, {
      vals <- .collect_diagram_form(input)
      errs <- validate_diagram(vals)
      if (length(errs) > 0) { status_msg(paste(errs, collapse = "; ")); return() }
      # Blød duplikat-guard: advar men blokér ikke (bevidste dubletter findes)
      dup <- db$diagram_duplicate_count(vals$indikator,
        vals$organisatorisk_navn_teknisk, vals$diagram_type,
        exclude_id = -1L)
      if (dup > 0) {
        warn_msg(paste("Findes allerede: samme indikator/enhed/type har",
                       "et diagram i forvejen."))
      }
      safe_operation("diagram-gem", {
        newid <- db$create_diagram(vals)
        status_msg(paste("Oprettet diagram", newid))
        removeModal(); reload()
      }, fallback = status_msg("Fejl ved gem af diagram (se log)"))
    })

    # eksponér til test
    list(admin = admin, filtered = filtered, status_msg = status_msg,
         warn_msg = warn_msg, grid_sel = grid_sel, last_batch = last_batch)
  })
}
