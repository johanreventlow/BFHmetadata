#' Bygger ét form-input baseret på felt-kind. prefix giver distinkt id-rum
#' (modal vs sidebar). values pre-udfylder. label kan overstyre (default = col-navn).
#' @noRd
.field_input <- function(ns, f, fk_choices = list(), values = list(),
                         prefix = "", label = NULL) {
  id <- ns(paste0(prefix, f$col))
  v <- values[[f$col]]
  lab <- label %||% f$col
  switch(f$kind,
    "pk"       = NULL,
    "fk"       = selectInput(id, lab, choices = c("(ingen)" = "", fk_choices[[f$col]]),
                             selected = v %||% ""),
    "bool"     = checkboxInput(id, lab, value = isTRUE(v)),
    "date"     = dateInput(id, lab,
                           value = if (is.null(v) || is.na(v)) NULL else as.Date(v)),
    "int"      = numericInput(id, lab, value = if (is.null(v)) NA else v),
    "textarea" = textAreaInput(id, lab, value = v %||% ""),
    textInput(id, lab, value = v %||% "")  # text (default)
  )
}

# Felter modalen viser (design-retning C). indikator_navn_teknisk + output_enhed
# udelades bevidst → modal-gem rører dem ej (bevares). Danske labels + required/
# rosa-markering styres i .build_modal.
INDIKATOR_MODAL_COLS <- c(
  "indikator_navn", "indikator_hierarki", "datakilde", "kontaktperson",
  "ønsket_tendens", "mål", "sp_rapport_id", "direkte_link",
  "definition_kort", "definition_dataportal", "tæller_beskrivelse",
  "nævner_beskrivelse", "indikator_ukompatibel_med", "antal_observationer",
  "periode_fra", "aktiv_indikator", "nøgleindikator", "tillad_auto_opdatering")

# Danske felt-labels i modalen (col → vist tekst)
INDIKATOR_MODAL_LABELS <- c(
  indikator_navn = "Navn på indikator", indikator_hierarki = "Datasæt",
  datakilde = "Datakilde", kontaktperson = "Kontaktperson",
  ønsket_tendens = "Ønsket retning", mål = "Generelt indikatormål",
  sp_rapport_id = "Evt. SP rapport id", direkte_link = "Evt. direkte link",
  definition_kort = "Kort definition", definition_dataportal = "Definition til dataportal",
  tæller_beskrivelse = "Beskrivelse af tæller", nævner_beskrivelse = "Beskrivelse af nævner",
  indikator_ukompatibel_med = "Kommentarer vedr. anvendelse",
  antal_observationer = "Antal observationer", periode_fra = "Periode fra",
  aktiv_indikator = "Aktiv indikator", nøgleindikator = "Nøgleindikator",
  tillad_auto_opdatering = "Auto-opdatér rosa felter")

#' @noRd
mod_indikator_crud_ui <- function(id) {
  ns <- NS(id)
  bslib::navset_tab(
    bslib::nav_panel("Oversigt",
      tags$style(HTML(paste0(
        "#", ns("oversigt"), " table.dataTable tbody tr{cursor:pointer;}",
        "#", ns("oversigt"), " table.dataTable tbody tr:hover{background-color:#e9f2ff;}"))),
      div(class = "mt-2",
        div(class = "d-flex justify-content-end mb-2",
          actionButton(ns("new_modal"), "Ny indikator", class = "btn-success")),
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          uiOutput(ns("filter_datapakke_ui")),
          uiOutput(ns("filter_datasaet_ui")),
          selectInput(ns("filter_status"), "Status",
            choices = c("Alle" = "alle", "Kun aktive" = "aktiv",
                        "Kun inaktive" = "inaktiv",
                        "Nøgleindikatorer" = "noegle"),
            selected = "alle")
        ),
        DT::DTOutput(ns("oversigt")),
        verbatimTextOutput(ns("status"))
      )
    ),
    bslib::nav_panel("Inline-redigering",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 420, position = "right", open = TRUE,
          h5("Redigér / opret"),
          uiOutput(ns("form")),
          div(class = "d-flex gap-2 mt-2",
            actionButton(ns("new"), "Ny", class = "btn-secondary"),
            actionButton(ns("save"), "Gem", class = "btn-primary"),
            actionButton(ns("soft_delete"), "Deaktivér", class = "btn-warning")
          )
        ),
        div(
          checkboxInput(ns("show_inactive"), "Vis inaktive", value = TRUE),
          DT::DTOutput(ns("tbl"))
        )
      )
    )
  )
}

#' @noRd
.collect_form <- function(input, fields, prefix = "") {
  vals <- list()
  for (f in fields) {
    if (f$kind == "pk") next
    v <- input[[paste0(prefix, f$col)]]
    if (f$kind == "bool") v <- isTRUE(v)
    if (f$kind %in% c("text", "textarea", "fk") && identical(v, "")) v <- NA
    vals[[f$col]] <- v
  }
  vals
}

#' @noRd
mod_indikator_crud_server <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    rows <- reactiveVal(db$list_indikatorer())
    status_msg <- reactiveVal("")
    fk <- db$fk_options()
    fk_choices <- lapply(fk, function(d) stats::setNames(d$id, d$label))

    reload <- function() rows(db$list_indikatorer())

    editing_id <- reactiveVal(NULL)

    # Vis status som flydende notifikation: synlig OVER modal og uafhængigt af
    # aktiv fane (status-tekstboksen sidder kun på Oversigt-fanen). Dækker både
    # modal-valideringsfejl (modal forbliver åben) og fejl/kvittering på begge faner.
    observeEvent(status_msg(), {
      m <- status_msg()
      if (nzchar(m)) showNotification(m, duration = 5)
    }, ignoreInit = TRUE)

    # Bygger modal-indhold (design-retning C: to kolonner 5/7, sektioner, rosa).
    # row = NULL → blank "Ny indikator"-tilstand med fornuftige defaults.
    .build_modal <- function(row = NULL) {
      ns <- session$ns
      is_new <- is.null(row)
      # Defaults for ny indikator (design: aktiv + auto-opdatering tændt)
      vals <- if (is_new) list(aktiv_indikator = TRUE, tillad_auto_opdatering = TRUE)
              else as.list(row)
      req_cols  <- c("indikator_navn", "indikator_hierarki", "definition_kort")
      rosa_cols <- c("definition_dataportal", "tæller_beskrivelse", "nævner_beskrivelse")
      # Skalar/FK-felt med dansk label + evt. required-* + rosa-wrap
      fin <- function(col) {
        f <- Find(function(x) x$col == col, INDIKATOR_FIELDS)
        lab <- INDIKATOR_MODAL_LABELS[[col]] %||% col
        if (col %in% req_cols) lab <- tagList(lab, tags$span(" *", class = "req"))
        w <- .field_input(ns, f, fk_choices, values = vals, prefix = "m_", label = lab)
        # Rosa-klasse direkte på textarea (ej wrapper-div → bevarer fuld bredde
        # i bslib-grid'ets flex-kontekst).
        if (col %in% rosa_cols)
          w <- htmltools::tagQuery(w)$find("textarea")$addClass("rosa")$allTags()
        w
      }
      sect <- function(txt, sub = NULL) div(class = "form-section", txt,
        if (!is.null(sub)) tags$span(class = "sub", sub))
      # m2m-multiselect med dansk label
      mfin <- function(key, lab) {
        opts <- db$junction_options(key)
        sel <- if (is_new) integer(0) else db$get_junction(vals$id, key)
        selectInput(ns(paste0("m_j_", key)), lab,
          choices = stats::setNames(opts$id, opts$label),
          selected = sel, multiple = TRUE)
      }
      # 2-up række med almindelig Bootstrap-grid (g-3) — undgår bslib-grid'ets
      # ekstra margin, så felterne ikke skubber følgende sektion for langt ned.
      two_up <- function(a, b, w = c(6, 6)) div(class = "row gx-3",
        div(class = paste0("col-", w[1]), a), div(class = paste0("col-", w[2]), b))

      left <- tagList(
        sect("Stamdata"),
        fin("indikator_navn"),
        fin("indikator_hierarki"),
        two_up(fin("datakilde"), fin("kontaktperson")),
        two_up(fin("ønsket_tendens"), fin("mål")),
        sect("Relationer"),
        mfin("dataprodukter", "Indgår i dataprodukter"),
        mfin("faggrupper", "Relevant for faggrupper"),
        mfin("organisation", "Relevant for afdelinger"),
        two_up(fin("sp_rapport_id"), fin("direkte_link"), w = c(5, 7)))

      right <- tagList(
        sect("Definitioner & beskrivelser", "rosa felter auto-opdateres"),
        fin("definition_kort"),
        fin("definition_dataportal"),
        fin("tæller_beskrivelse"),
        fin("nævner_beskrivelse"),
        fin("indikator_ukompatibel_med"),
        sect("Datagrundlag & status"),
        two_up(fin("antal_observationer"), fin("periode_fra")),
        div(class = "d-flex flex-wrap gap-4 pt-1",
          fin("aktiv_indikator"), fin("nøgleindikator"), fin("tillad_auto_opdatering")))

      modalDialog(title = if (is_new) "Ny indikator" else "Redigér indikator",
        size = "xl", easyClose = FALSE,
        tags$style(HTML(paste0(
          ".modal-dialog{margin-top:24px;}",
          ".modal-body{max-height:80vh;overflow-y:auto;}",
          # Stram feltafstand i modalen (default shiny-margin er for stor her)
          ".modal-body .shiny-input-container,.modal-body .form-group{margin-bottom:.55rem;}",
          ".modal-body .shiny-input-container>label,.modal-body .control-label{",
          "margin-bottom:.15rem;font-weight:600;font-size:.85rem;color:#343a40;}",
          ".form-section{font-size:.75rem;font-weight:700;letter-spacing:.06em;",
          "text-transform:uppercase;color:#0d6efd;margin:1.25rem 0 .75rem;",
          "padding-bottom:.4rem;border-bottom:1px solid #e7ebf0;}",
          ".form-section:first-child{margin-top:0;}",
          ".form-section .sub{font-weight:500;letter-spacing:0;text-transform:none;",
          "color:#8a9099;font-size:.8rem;margin-left:.5rem;}",
          ".shiny-input-container label .req,.footer-note .req{color:#dc3545;font-weight:700;}",
          "textarea.form-control.rosa{background-color:#fbe4ea;border-color:#e7a9b8;}",
          "textarea.form-control.rosa:focus{",
          "box-shadow:0 0 0 .25rem rgba(231,169,184,.4);border-color:#e7a9b8;}",
          ".footer-note{font-size:.82rem;color:#6c757d;}"))),
        bslib::layout_columns(col_widths = c(5, 7), left, right),
        footer = div(class = "d-flex justify-content-between align-items-center w-100",
          span(class = "footer-note", HTML(
            '<span class="req">*</span> = obligatorisk')),
          div(class = "d-flex gap-2",
            modalButton("Annullér"),
            actionButton(ns("modal_save"), "Gem og luk", class = "btn-primary"))))
    }

    observeEvent(input$open_id, {
      rid <- as.integer(input$open_id)
      editing_id(rid)
      row <- rows()[rows()[["id"]] == rid, , drop = FALSE]
      if (nrow(row) == 0) { status_msg("Indikator ikke fundet"); return() }
      showModal(.build_modal(row[1, , drop = FALSE]))
    })

    # Ny blank indikator → samme modal, oprettes først ved Gem
    observeEvent(input$new_modal, {
      editing_id(NULL)
      showModal(.build_modal(NULL))
    })

    observeEvent(input$modal_save, {
      rid <- editing_id()  # NULL → opret ny
      # Saml KUN de felter modalen viser → udeladte kolonner (teknisk navn,
      # output_enhed) røres ej i UPDATE og bevarer deres værdi.
      modal_fields <- Filter(function(f) f$col %in% INDIKATOR_MODAL_COLS,
                             INDIKATOR_FIELDS)
      vals <- .collect_form(input, modal_fields, prefix = "m_")
      errs <- validate_indikator(vals)
      if (length(errs) > 0) { status_msg(paste(errs, collapse = "; ")); return() }
      # Saml alle valgte m2m-relationer → ét atomisk gem (scalar + junctions)
      picks <- lapply(names(INDIKATOR_JUNCTIONS),
                      function(key) as.integer(input[[paste0("m_j_", key)]]))
      names(picks) <- names(INDIKATOR_JUNCTIONS)
      safe_operation("modal-gem", {
        if (is.null(rid)) {
          newid <- db$create_indikator_full(vals, picks)
          status_msg(paste("Oprettet indikator", newid))
        } else {
          db$save_indikator(rid, vals, picks)
          status_msg(paste("Gemt indikator", rid))
        }
        removeModal(); reload()
      }, fallback = status_msg("Fejl ved modal-gem (se log)"))
    })

    output$form <- renderUI({
      ns <- session$ns
      tagList(lapply(INDIKATOR_FIELDS, function(f) .field_input(ns, f, fk_choices)))
    })

    output$tbl <- DT::renderDT({
      d <- rows()
      if (!isTRUE(input$show_inactive) && "aktiv_indikator" %in% names(d))
        d <- d[d$aktiv_indikator %in% TRUE, , drop = FALSE]
      editable_cols <- which(names(d) %in% INLINE_EDITABLE) - 1
      DT::datatable(d, selection = "single", rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = setdiff(seq_len(ncol(d))-1, editable_cols))))
    })

    # Datapakke-filter: valg afledt af de datapakke-værdier der faktisk findes
    output$filter_datapakke_ui <- renderUI({
      ns <- session$ns
      vals <- sort(unique(stats::na.omit(rows()[["label_datapakke"]])))
      selectInput(ns("filter_datapakke"), "Datapakke",
        choices = c("Alle" = "", stats::setNames(vals, vals)), selected = "")
    })

    # Datasæt-filter: kaskaderer på valgt datapakke (viser kun datasæt derunder)
    output$filter_datasaet_ui <- renderUI({
      ns <- session$ns
      d <- rows()
      fdp <- input$filter_datapakke
      if (!is.null(fdp) && nzchar(fdp))
        d <- d[d$label_datapakke %in% fdp, , drop = FALSE]
      vals <- sort(unique(stats::na.omit(d[["label_indikator_hierarki"]])))
      selectInput(ns("filter_datasaet"), "Datasæt",
        choices = c("Alle" = "", stats::setNames(vals, vals)), selected = "")
    })

    # Filtreret datasæt til oversigten (delt af render + række-klik-observer)
    oversigt_rows <- reactive({
      d <- rows()
      status <- input$filter_status %||% "alle"
      if (identical(status, "aktiv"))
        d <- d[d$aktiv_indikator %in% TRUE, , drop = FALSE]
      if (identical(status, "inaktiv"))
        d <- d[!(d$aktiv_indikator %in% TRUE), , drop = FALSE]
      if (identical(status, "noegle"))
        d <- d[d$nøgleindikator %in% TRUE, , drop = FALSE]
      fdp <- input$filter_datapakke
      if (!is.null(fdp) && nzchar(fdp))
        d <- d[d$label_datapakke %in% fdp, , drop = FALSE]
      fds <- input$filter_datasaet
      if (!is.null(fds) && nzchar(fds))
        d <- d[d$label_indikator_hierarki %in% fds, , drop = FALSE]
      d
    })

    output$oversigt <- DT::renderDT({
      d <- oversigt_rows()
      # Aktiv: grøn ✓ / grå streg
      aktiv <- ifelse(d[["aktiv_indikator"]] %in% TRUE,
        '<span style="color:#198754;font-weight:700;">&#10003;</span>',
        '<span style="color:#adb5bd;">&mdash;</span>')
      # Indikator-id (teknisk navn) + gul "Nøgle"-badge ved nøgleindikator
      idtxt <- d[["indikator_navn_teknisk"]]
      idtxt[is.na(idtxt)] <- ""
      idcol <- mapply(function(txt, key) {
        txt <- htmltools::htmlEscape(txt)
        if (isTRUE(key)) paste0(txt, ' <span class="badge text-bg-warning">Nøgle</span>') else txt
      }, idtxt, d[["nøgleindikator"]] %in% TRUE, USE.NAMES = FALSE)
      btn <- '<span class="btn btn-outline-secondary btn-sm">Åbn &rsaquo;</span>'
      # Kolonner: knap → aktiv → datapakke → datasæt → indikator-id → navn
      out <- data.frame(
        ` ` = btn,
        Aktiv = aktiv,
        Datapakke = d[["label_datapakke"]],
        Datasæt = d[["label_indikator_hierarki"]],
        `Indikator-id` = idcol,
        Navn = d[["indikator_navn"]],
        check.names = FALSE, stringsAsFactors = FALSE)
      # Knap/Aktiv/Id indeholder bevidst HTML; escape kun rene tekstkolonner (XSS)
      esc <- which(names(out) %in% c("Datapakke", "Datasæt", "Navn"))
      DT::datatable(out, escape = esc, rownames = FALSE, selection = "single",
        options = list(pageLength = 10, columnDefs = list(
          list(orderable = FALSE, targets = 0))))
    })

    # Hel-række-klik åbner modal (design: hele rækken er klikbar)
    oversigt_proxy <- DT::dataTableProxy("oversigt", session)
    observeEvent(input$oversigt_rows_selected, {
      idx <- input$oversigt_rows_selected
      d <- oversigt_rows()
      if (idx > nrow(d)) return()
      rid <- as.integer(d[["id"]][idx])
      editing_id(rid)
      row <- rows()[rows()[["id"]] == rid, , drop = FALSE]
      if (nrow(row) == 0) { status_msg("Indikator ikke fundet"); return() }
      showModal(.build_modal(row[1, , drop = FALSE]))
      DT::selectRows(oversigt_proxy, NULL)  # nulstil → samme række kan genåbnes
    })

    selected_id <- reactive({
      sel <- input$tbl_rows_selected
      if (is.null(sel)) return(NULL)
      rows()[["id"]][sel]
    })

    observeEvent(input$save, {
      vals <- .collect_form(input, INDIKATOR_FIELDS)
      errs <- validate_indikator(vals)
      if (length(errs) > 0) { status_msg(paste(errs, collapse = "; ")); return() }
      safe_operation("gem indikator", {
        sid <- selected_id()
        if (is.null(sid)) {
          newid <- db$create_indikator(vals); status_msg(paste("Oprettet id", newid))
        } else {
          db$update_indikator(sid, vals); status_msg("Gemt")
        }
        reload()
      }, fallback = status_msg("Fejl ved gem (se log)"))
    })

    observeEvent(input$soft_delete, {
      sid <- selected_id()
      if (is.null(sid)) { status_msg("Vælg en række først"); return() }
      safe_operation("soft-delete", {
        db$soft_delete(sid, active = FALSE); status_msg("Deaktiveret"); reload()
      }, fallback = status_msg("Fejl ved deaktivering"))
    })

    observeEvent(input$tbl_cell_edit, {
      info <- input$tbl_cell_edit
      d <- rows(); col <- names(d)[info$col + 1]
      if (!col %in% INLINE_EDITABLE) { status_msg("Kolonne ej redigerbar inline"); return() }
      rid <- d[["id"]][info$row]
      safe_operation("inline-update", {
        db$update_indikator(rid, stats::setNames(list(info$value), col))
        status_msg(paste("Opdateret", col)); reload()
      }, fallback = status_msg("Fejl ved inline-update"))
    })

    output$status <- renderText(status_msg())

    # eksponér til test
    list(rows = rows, status_msg = status_msg, editing_id = editing_id)
  })
}

#' Minimal safe_operation (logger + fallback)
#' @noRd
safe_operation <- function(op, code, fallback = NULL) {
  tryCatch(force(code), error = function(e) {
    message(sprintf("[ERROR] %s: %s", op, conditionMessage(e)))
    force(fallback)
  })
}
