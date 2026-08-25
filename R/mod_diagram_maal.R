# Mål-styring (admin): filterbar oversigt over ALLE mål (tblDiagrammerMaal)
# på tværs af diagrammer. Et diagram kan have flere mål over tid (retning +
# værdi + gældende-fra-dato) — samme 1-til-mange-form som median-knæk, men
# uden signal-scan-koblingen: ren CRUD på egen fane, modelleret på mod_diagram.R.

#' Formular til "Nyt mål": diagram-selectize starter TOM (kun tom-valg) —
#' fulde choices pushes server-side efter flush (se .update_maal_diagram),
#' så listen (kan være tusindvis af diagrammer) ikke serialiseres i HTML'en.
#' @noRd
.maal_form_ui <- function(ns) {
  tagList(
    selectizeInput(ns("mf_diagram"), "Diagram",
      choices = c("(vælg)" = ""), selected = ""),
    selectInput(ns("mf_maal_retning"), "Retning",
      choices = c("(ingen)" = "", MAAL_RETNING_CHOICES), selected = ""),
    numericInput(ns("mf_maal_vaerdi"), "Målværdi", value = NA),
    dateInput(ns("mf_maal_gaeldende_fra"), "Gældende fra", value = Sys.Date())
  )
}

#' Push fulde diagram-choices til selectize server-side (efter modal-flush).
#' @noRd
.update_maal_diagram <- function(session, diagrams) {
  choices <- stats::setNames(as.character(diagrams$id), diagrams$label)
  updateSelectizeInput(session, "mf_diagram",
    choices = c("(vælg)" = "", choices), selected = "", server = TRUE)
}

#' Saml ny-mål-formularens inputs → named list i MAAL_COLS-orden.
#' @noRd
.collect_maal_form <- function(input) {
  d <- suppressWarnings(as.integer(input$mf_diagram))
  retn <- input$mf_maal_retning
  dato <- input$mf_maal_gaeldende_fra
  list(
    diagram = if (length(d) == 0 || is.na(d)) NA_integer_ else d,
    maal_retning = if (is.null(retn) || identical(retn, "")) {
      NA_character_
    } else {
      as.character(retn)
    },
    maal_vaerdi = input$mf_maal_vaerdi,
    maal_gaeldende_fra = if (is.null(dato) || length(dato) == 0 || is.na(dato)) {
      NA
    } else {
      as.character(dato)
    }
  )
}

# Grid-titler → tblDiagrammerMaal-kolonner for inline-redigering (excelR)
.MAAL_GRID_FIELDS <- c(
  Retning = "maal_retning", Værdi = "maal_vaerdi",
  "Gældende fra" = "maal_gaeldende_fra"
)

#' Fuld MAAL_COLS-værdiliste fra en admin-række. Autoritativ basis for
#' inline-patch: én celle ændres, resten bevares som de ER i DB.
#' @noRd
.maal_row_values <- function(row) {
  list(
    diagram = as.integer(row$diagram),
    maal_retning = if (is.na(row$maal_retning)) {
      NA_character_
    } else {
      as.character(row$maal_retning)
    },
    maal_vaerdi = as.numeric(row$maal_vaerdi),
    maal_gaeldende_fra = if (is.na(row$maal_gaeldende_fra)) {
      NA
    } else {
      as.character(as.Date(row$maal_gaeldende_fra))
    }
  )
}

#' Grid-data til excelR: pk (hidden) + kontekst (Indikator/Enhed/Type,
#' readOnly) + redigerbare felter.
#' @noRd
maal_excel_data <- function(d) {
  chr_or_empty <- function(x) ifelse(is.na(x), "", as.character(x))
  out <- data.frame(maal_id = d$maal_id, stringsAsFactors = FALSE,
                    check.names = FALSE)
  out[["Datapakke"]] <- chr_or_empty(d$datapakke)
  out[["Datasæt"]] <- chr_or_empty(d$datasaet)
  out[["Indikator"]] <- chr_or_empty(d$indikator_navn)
  out[["Enhed"]] <- chr_or_empty(d$org_navn)
  out[["Type"]] <- chr_or_empty(d$type_navn)
  # %||%-fallback: ældre kaldere/caches uden kolonnen viser blot tomt frem
  # for at vælte hele grid-renderingen på en manglende kolonne.
  out[["Målgruppe"]] <- chr_or_empty(
    d$maalgruppe_navn %||% rep(NA_character_, nrow(d))
  )
  out[["Retning"]] <- chr_or_empty(d$maal_retning)
  out[["Værdi"]] <- chr_or_empty(d$maal_vaerdi)
  out[["Gældende fra"]] <- ifelse(
    is.na(d$maal_gaeldende_fra), "",
    as.character(as.Date(d$maal_gaeldende_fra))
  )
  out
}

#' Kolonne-spec til mål-grid'et: Retning som dropdown (fast værdisæt),
#' Værdi/dato som tekst (valideres server-side ved gem). Kontekst-kolonner
#' (Datapakke/Datasæt/Indikator/Enhed/Type/Målgruppe) er readOnly — mål
#' oprettes/flyttes mellem diagrammer via "Nyt mål"-modalen, ikke inline.
#' Målgruppen er diagrammets (tblDiagrammer.maalgruppe) og redigeres på
#' Diagram-fanen; her vises den, så mål på samme indikator/enhed med
#' forskellige målgrupper kan skelnes.
#' @noRd
maal_excel_columns <- function(d) {
  retning_src <- data.frame(
    id = c("", MAAL_RETNING_CHOICES), name = c("(ingen)", MAAL_RETNING_CHOICES),
    stringsAsFactors = FALSE)
  titles <- c("maal_id", "Datapakke", "Datasæt", "Indikator", "Enhed", "Type",
             "Målgruppe", names(.MAAL_GRID_FIELDS))
  out <- data.frame(
    title = titles,
    type = c("hidden", rep("text", 6), "dropdown", "text", "text"),
    readOnly = c(TRUE, rep(TRUE, 6), FALSE, FALSE, FALSE),
    align = "left",
    stringsAsFactors = FALSE)
  out$source <- c(rep(list(NA), 7), list(retning_src), rep(list(NA), 2))
  disp <- maal_excel_data(d)
  disp[["Retning"]] <- .excel_dropdown_display(disp[["Retning"]], retning_src)
  out$width <- unname(excel_col_widths(disp)[titles])
  out
}

#' @noRd
mod_diagram_maal_ui <- function(id) {
  ns <- NS(id)
  div(class = "mt-2",
    div(class = "d-flex justify-content-end gap-2 mb-2",
      actionButton(ns("new_maal"), "Nyt mål", class = "btn-success"),
      actionButton(ns("delete_row"), "Slet valgt mål",
                   class = "btn-outline-danger")),
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      uiOutput(ns("filter_datapakke_ui")),
      uiOutput(ns("filter_datasaet_ui")),
      uiOutput(ns("filter_indikator_ui")),
      uiOutput(ns("filter_org_ui"))),
    p(class = "text-muted small",
      "Dobbeltklik en celle for at redigere. Klik en række og tryk Slet."),
    excelR::excelOutput(ns("tbl"), width = "100%", height = "auto"))
}

#' @noRd
mod_diagram_maal_server <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    admin <- reactiveVal(db$list_maal_admin())
    status_msg <- reactiveVal("")
    warn_msg <- reactiveVal("")
    grid_sel <- reactiveVal(NULL)
    grid_refresh <- reactiveVal(0)
    # Ekko-værn: excelR re-sender data+selektion efter hvert re-render — en
    # identisk diff lige efter et reload er grid'ets ekko, ikke brugeren.
    echo_guard <- new_excel_echo_guard()

    reload <- function() {
      safe_operation("genindlæs mål", admin(db$list_maal_admin()),
        fallback = status_msg("Databasen svarer ikke — viser senest hentede data"))
    }

    observeEvent(status_msg(), {
      if (nzchar(status_msg())) showNotification(status_msg(), duration = 5)
    }, ignoreInit = TRUE)
    observeEvent(warn_msg(), {
      if (nzchar(warn_msg()))
        showNotification(warn_msg(), type = "warning", duration = 8)
    }, ignoreInit = TRUE)

    # Diagram-vælgeren til "Nyt mål": {id, label} bygget af diagram-adminen
    # (samme kilde som Diagrammer-fanen), ikke rå diagram-id'er. Målgruppen
    # medtages i labelen, når diagrammet har en — ellers kan to diagrammer på
    # samme indikator/enhed (forskellige målgrupper) ikke skelnes i listen.
    .diagram_choices <- reactive({
      d <- db$list_diagrams_admin()
      mg <- d$maalgruppe_navn %||% rep(NA_character_, nrow(d))
      lbl <- paste0(
        ifelse(is.na(d$indikator_navn), "?", d$indikator_navn), " – ",
        ifelse(is.na(d$org_navn), "?", d$org_navn),
        ifelse(is.na(mg), "", paste0(" · ", mg)),
        " (#", d$diagram_id, ")"
      )
      data.frame(id = d$diagram_id, label = lbl, stringsAsFactors = FALSE)
    })

    .filter_ui <- function(input_id, lab, col, d = admin()) {
      ns <- session$ns
      vals <- sort(unique(stats::na.omit(d[[col]])))
      choices <- c("Alle" = "", stats::setNames(vals, vals))
      selected <- .preserved_filter_selection(
        isolate(input[[input_id]]), choices)
      selectInput(ns(input_id), lab, choices = choices, selected = selected)
    }
    .under_pakke <- reactive({
      d <- admin()
      fdp <- input$filter_datapakke
      if (!is.null(fdp) && nzchar(fdp)) d <- d[d$datapakke %in% fdp, , drop = FALSE]
      d
    })
    .under_datasaet <- reactive({
      d <- .under_pakke()
      fds <- input$filter_datasaet
      if (!is.null(fds) && nzchar(fds)) d <- d[d$datasaet %in% fds, , drop = FALSE]
      d
    })
    output$filter_datapakke_ui <- renderUI(
      .filter_ui("filter_datapakke", "Datapakke", "datapakke"))
    output$filter_datasaet_ui <- renderUI(
      .filter_ui("filter_datasaet", "Datasæt", "datasaet", .under_pakke()))
    output$filter_indikator_ui <- renderUI(
      .filter_ui("filter_indikator", "Indikator", "indikator_navn",
                 .under_datasaet()))
    output$filter_org_ui <- renderUI(
      .filter_ui("filter_org", "Organisatorisk enhed", "org_navn"))

    filtered <- reactive({
      d <- admin()
      fdp <- input$filter_datapakke
      if (!is.null(fdp) && nzchar(fdp)) d <- d[d$datapakke %in% fdp, , drop = FALSE]
      fds <- input$filter_datasaet
      if (!is.null(fds) && nzchar(fds)) d <- d[d$datasaet %in% fds, , drop = FALSE]
      fi <- input$filter_indikator
      if (!is.null(fi) && nzchar(fi)) d <- d[d$indikator_navn %in% fi, , drop = FALSE]
      fo <- input$filter_org
      if (!is.null(fo) && nzchar(fo)) d <- d[d$org_navn %in% fo, , drop = FALSE]
      d
    })

    output$tbl <- excelR::renderExcel({
      grid_refresh()
      d <- filtered()
      excelR::excelTable(
        data = maal_excel_data(d),
        columns = maal_excel_columns(d),
        autoColTypes = FALSE,
        autoWidth = FALSE,
        allowInsertRow = FALSE, allowInsertColumn = FALSE,
        allowDeleteRow = FALSE, allowDeleteColumn = FALSE,
        allowRenameColumn = FALSE, columnSorting = TRUE,
        rowDrag = FALSE, columnDrag = FALSE,
        getSelectedData = TRUE
      )
    })

    observeEvent(input$tbl, {
      p <- input$tbl
      if (isTRUE(p$forSelectedVals)) {
        grid_sel(excel_selected_pk(p))
      }
      d <- filtered()
      changes <- excel_diff_cells(maal_excel_data(d), excel_event_df(p), "maal_id")
      changes <- changes[changes$col %in% names(.MAAL_GRID_FIELDS), , drop = FALSE]
      if (nrow(changes) == 0) return()
      if (echo_guard$skip(changes)) return()
      revert <- FALSE
      for (k in seq_len(nrow(changes))) {
        rid <- as.integer(changes$pk[k])
        row <- admin()[admin()$maal_id == rid, , drop = FALSE]
        if (nrow(row) == 0) next
        vals <- .maal_row_values(row[1, ])
        field <- .MAAL_GRID_FIELDS[[changes$col[k]]]
        val <- changes$value[k]
        if (identical(field, "maal_vaerdi")) {
          nv <- suppressWarnings(as.numeric(val))
          if (!is.na(val) && is.na(nv)) {
            status_msg("Målværdi skal være et tal")
            revert <- TRUE
            next
          }
          vals[[field]] <- nv
        } else if (identical(field, "maal_gaeldende_fra")) {
          if (is.na(val)) {
            vals[[field]] <- NA
          } else {
            dt <- tryCatch(as.Date(val, tryFormats = "%Y-%m-%d"),
                           warning = function(w) NA, error = function(e) NA)
            if (is.na(dt)) {
              status_msg("Dato skal være på formen ÅÅÅÅ-MM-DD")
              revert <- TRUE
              next
            }
            vals[[field]] <- as.character(dt)
          }
        } else {
          vals[[field]] <- if (is.na(val)) NA_character_ else as.character(val)
        }
        errs <- validate_maal(vals)
        if (length(errs) > 0) {
          status_msg(paste(errs, collapse = "; "))
          revert <- TRUE
          next
        }
        ok <- safe_operation("mål-inline-gem", {
          db$update_maal(rid, vals)
          TRUE
        }, fallback = FALSE)
        if (isTRUE(ok)) {
          status_msg(paste("Gemt mål", rid))
        } else {
          status_msg("Fejl ved gem af mål (se log)")
          revert <- TRUE
        }
      }
      reload()
      if (revert) grid_refresh(grid_refresh() + 1)
      # reload() re-renderer grid'et (admin → filtered → renderExcel), og
      # excelR gen-sender payloads efter re-render — en identisk diff derfra
      # er ekkoet og må ikke behandles igen (gem→reload→ekko-loop).
      echo_guard$arm(changes)
    })

    observeEvent(input$delete_row, {
      sel <- grid_sel()
      d <- filtered()
      j <- if (is.null(sel)) NA_integer_ else match(sel, as.character(d$maal_id))
      if (is.na(j)) {
        status_msg("Vælg en række først")
        return()
      }
      rid <- d$maal_id[j]
      safe_operation("mål-slet", {
        db$delete_maal(rid)
        status_msg(paste("Slettet mål", rid))
        grid_sel(NULL)
        reload()
      }, fallback = status_msg("Fejl ved sletning (se log)"))
    })

    observeEvent(input$new_maal, {
      ns <- session$ns
      showModal(modalDialog(
        title = "Nyt mål",
        size = "m", easyClose = FALSE,
        .maal_form_ui(ns),
        footer = div(class = "d-flex justify-content-end gap-2 w-100",
          modalButton("Annullér"),
          actionButton(ns("mf_save"), "Gem", class = "btn-primary"))))
      session$onFlushed(function() {
        .update_maal_diagram(session, .diagram_choices())
      }, once = TRUE)
    })

    observeEvent(input$mf_save, {
      vals <- .collect_maal_form(input)
      errs <- validate_maal(vals)
      if (length(errs) > 0) {
        status_msg(paste(errs, collapse = "; "))
        return()
      }
      safe_operation("mål-gem", {
        newid <- db$create_maal(vals)
        status_msg(paste("Oprettet mål", newid))
        removeModal()
        reload()
      }, fallback = status_msg("Fejl ved gem af mål (se log)"))
    })

    # eksponér til test
    list(admin = admin, filtered = filtered, status_msg = status_msg,
         warn_msg = warn_msg, grid_sel = grid_sel)
  })
}
