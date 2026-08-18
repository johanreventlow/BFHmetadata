#' Bygger ét form-input baseret på felt-kind. prefix giver distinkt id-rum
#' (modal vs sidebar). values pre-udfylder. label kan overstyre (default = col-navn).
#' @noRd
.field_input <- function(ns, f, fk_choices = list(), values = list(),
                         prefix = "", label = NULL) {
  id <- ns(paste0(prefix, f$col))
  v <- values[[f$col]]
  lab <- label %||% f$col
  switch(f$kind,
    "pk" = NULL,
    "fk" = selectInput(id, lab,
      choices = c("(ingen)" = "", fk_choices[[f$col]]),
      selected = v %||% ""
    ),
    "bool" = checkboxInput(id, lab, value = isTRUE(v)),
    # NA (ikke NULL!) ved tom værdi: NULL udelader data-initial-date og
    # browserens datepicker defaulter så til DAGS DATO — et gem ville skrive
    # en dato der aldrig har ligget i databasen. NA sætter attributten tom →
    # tomt felt. suppressWarnings: shiny 1.14's dateYMD advarer kosmetisk om
    # NA-koercion, men NA ER den dokumenterede tomme-felt-værdi.
    "date" = if (is.null(v) || length(v) == 0 || is.na(v)) {
      suppressWarnings(dateInput(id, lab, value = NA))
    } else {
      dateInput(id, lab, value = as.Date(v))
    },
    "int" = numericInput(id, lab, value = if (is.null(v)) NA else v),
    "textarea" = textAreaInput(id, lab, value = v %||% ""),
    # Fast værdisæt (f$choices) + evt. eksisterende legacy-værdi bevaret
    "choice" = {
      ch <- f$choices %||% character(0)
      if (!is.null(v) && length(v) == 1 && !is.na(v) && nzchar(v) &&
        !(v %in% ch)) {
        ch <- c(ch, v)
      }
      selectInput(id, lab, choices = c("(ingen)" = "", ch), selected = v %||% "")
    },
    textInput(id, lab, value = v %||% "") # text (default)
  )
}

# Felter modalen viser (design-retning C). indikator_navn_teknisk udelades
# bevidst fra GEM (parquet-mappings-nøgle — omdøbning bryder datakoblingen);
# den VISES readonly i .build_modal. Danske labels + required/rosa-markering
# styres i .build_modal.
INDIKATOR_MODAL_COLS <- c(
  "indikator_navn", "indikator_hierarki", "datakilde", "kontaktperson",
  "ønsket_tendens", "mål", "output_enhed", "sp_rapport_id", "direkte_link",
  "definition_kort", "definition_dataportal", "tæller_beskrivelse",
  "nævner_beskrivelse", "indikator_ukompatibel_med", "antal_observationer",
  "periode_fra", "aktiv_indikator", "nøgleindikator",
  "nulfyld_tomme_perioder", "tillad_auto_opdatering"
)

# Danske felt-labels i modalen (col → vist tekst)
INDIKATOR_MODAL_LABELS <- c(
  indikator_navn = "Navn på indikator",
  indikator_hierarki = "Hierarki-placering",
  datakilde = "Datakilde", kontaktperson = "Kontaktperson",
  ønsket_tendens = "Ønsket retning", mål = "Generelt indikatormål",
  output_enhed = "Output-enhed",
  sp_rapport_id = "Evt. SP rapport id", direkte_link = "Evt. direkte link",
  definition_kort = "Kort definition", definition_dataportal = "Definition til dataportal",
  tæller_beskrivelse = "Beskrivelse af tæller", nævner_beskrivelse = "Beskrivelse af nævner",
  indikator_ukompatibel_med = "Kommentarer vedr. anvendelse",
  antal_observationer = "Antal observationer", periode_fra = "Periode fra",
  aktiv_indikator = "Aktiv indikator", nøgleindikator = "Nøgleindikator",
  nulfyld_tomme_perioder = "Nulfyld tomme perioder",
  tillad_auto_opdatering = "Auto-opdatér rosa felter"
)

#' @noRd
mod_indikator_crud_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "mt-2",
    div(
      class = "d-flex justify-content-end gap-2 mb-2",
      actionButton(ns("new_modal"), "Ny indikator", class = "btn-success"),
      actionButton(ns("open_selected"), "Åbn valgte",
        class = "btn-outline-primary"
      ),
      actionButton(ns("soft_delete"), "Deaktivér valgte",
        class = "btn-warning"
      )
    ),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      uiOutput(ns("filter_datapakke_ui")),
      uiOutput(ns("filter_datasaet_ui")),
      selectInput(ns("filter_status"), "Status",
        choices = c(
          "Alle" = "alle", "Kun aktive" = "aktiv",
          "Kun inaktive" = "inaktiv",
          "Nøgleindikatorer" = "noegle"
        ),
        selected = "alle"
      )
    ),
    p(class = "text-muted small", paste(
      "Dobbeltklik en celle for at redigere direkte.",
      "Klik en række og brug 'Åbn valgte' for definitioner, relationer",
      "og diagrammer — eller 'Deaktivér valgte'."
    )),
    excelR::excelOutput(ns("tbl"), width = "100%", height = "auto"),
    verbatimTextOutput(ns("status"))
  )
}

#' @noRd
.collect_form <- function(input, fields, prefix = "") {
  vals <- list()
  for (f in fields) {
    if (f$kind == "pk") next
    v <- input[[paste0(prefix, f$col)]]
    if (f$kind == "bool") v <- isTRUE(v)
    if (f$kind %in% c("text", "textarea", "fk", "choice") &&
      identical(v, "")) {
      v <- NA
    }
    # Tom dateInput leverer Date af længde 0 — normalisér til NA, så gem
    # aldrig fejler på et 0-længde parameter (eller opfinder en dato)
    if (f$kind == "date" && !is.null(v) && length(v) == 0) v <- NA
    vals[[f$col]] <- v
  }
  vals
}

#' Dropdown-choices for indikator-hierarki: aktive noder + evt. nuvaerende
#' inaktive node markeret "(inaktiv)" — bevarer eksisterende vaerdi uden
#' stille datamutation. Tolererer opts uden aktiv-kolonne (alle = aktive).
#' @noRd
.hierarki_choices <- function(opts_df, current_id = NULL) {
  akt <- if ("aktiv" %in% names(opts_df)) {
    opts_df$aktiv %in% TRUE
  } else {
    rep(TRUE, nrow(opts_df))
  }
  act <- opts_df[akt, , drop = FALSE]
  ch <- stats::setNames(act$id, act$label)
  if (!is.null(current_id) && length(current_id) == 1 && !is.na(current_id) &&
    !(current_id %in% act$id) && current_id %in% opts_df$id) {
    lbl <- opts_df$label[opts_df$id == current_id][1]
    ch <- c(ch, stats::setNames(current_id, paste0(lbl, " (inaktiv)")))
  }
  ch
}

#' Grid-dropdown-source for hierarki-placerings-kolonnen: aktive noder + BRUGTE
#' inaktive ("(inaktiv)"-suffix) + helt ukendte id'er ("Ukendt node #id") saa
#' eksisterende celle-vaerdier hverken vises blankt eller tabes ved gem.
#' @noRd
.hierarki_src <- function(opts_df, used) {
  akt <- if ("aktiv" %in% names(opts_df)) {
    opts_df$aktiv %in% TRUE
  } else {
    rep(TRUE, nrow(opts_df))
  }
  src <- data.frame(
    id = as.character(opts_df$id[akt]),
    name = as.character(opts_df$label[akt]),
    stringsAsFactors = FALSE
  )
  used <- stats::na.omit(as.character(used))
  inactive_used <- setdiff(intersect(used, as.character(opts_df$id)), src$id)
  if (length(inactive_used) > 0) {
    lbl <- opts_df$label[match(inactive_used, as.character(opts_df$id))]
    src <- rbind(src, data.frame(
      id = inactive_used,
      name = paste0(lbl, " (inaktiv)"), stringsAsFactors = FALSE
    ))
  }
  unknown <- setdiff(used, as.character(opts_df$id))
  if (length(unknown) > 0) {
    src <- rbind(src, data.frame(
      id = unknown,
      name = sprintf("Ukendt node #%s", unknown), stringsAsFactors = FALSE
    ))
  }
  src
}

# Grid-titler → tblIndikatorer-kolonner for inline-redigering (excelR).
# Datapakke + Datasæt + Indikator-id vises readOnly (kontekst — niveau-udledte
# forfader-navne); resten redigeres direkte. "Hierarki-placering" er selve
# FK'en (typisk en indikatorsamling). Lange felter (definitioner m.m.) +
# m2m-relationer + diagrammer redigeres fortsat i modalen ("Åbn valgte").
.INDIKATOR_GRID_FIELDS <- c(
  "Aktiv" = "aktiv_indikator", "Nøgle" = "nøgleindikator",
  "Nulfyld" = "nulfyld_tomme_perioder",
  "Hierarki-placering" = "indikator_hierarki", "Navn" = "indikator_navn",
  "Mål" = "mål", "Output-enhed" = "output_enhed",
  "Ønsket tendens" = "ønsket_tendens", "Direkte link" = "direkte_link",
  "Kontaktperson" = "kontaktperson", "Datakilde" = "datakilde"
)

#' Grid-data: pk (hidden) + kontekst (readOnly) + redigerbare felter.
#' FK-felter som character-id'er; flag som logicals (checkbox-celler).
#' Manglende kolonner i d tolereres (→ tomme celler).
#' @noRd
indikator_excel_data <- function(d) {
  col_of <- function(col) if (col %in% names(d)) d[[col]] else rep(NA, nrow(d))
  chr_or_empty <- function(x) ifelse(is.na(x), "", as.character(x))
  out <- data.frame(id = d$id, stringsAsFactors = FALSE, check.names = FALSE)
  out[["Aktiv"]] <- col_of("aktiv_indikator") %in% TRUE
  out[["Nøgle"]] <- col_of("nøgleindikator") %in% TRUE
  out[["Nulfyld"]] <- col_of("nulfyld_tomme_perioder") %in% TRUE
  out[["Datapakke"]] <- chr_or_empty(col_of("label_datapakke"))
  out[["Datasæt"]] <- chr_or_empty(col_of("label_datasaet"))
  out[["Hierarki-placering"]] <- chr_or_empty(col_of("indikator_hierarki"))
  out[["Indikator-id"]] <- chr_or_empty(col_of("indikator_navn_teknisk"))
  out[["Navn"]] <- chr_or_empty(col_of("indikator_navn"))
  out[["Mål"]] <- chr_or_empty(col_of("mål"))
  out[["Output-enhed"]] <- chr_or_empty(col_of("output_enhed"))
  out[["Ønsket tendens"]] <- chr_or_empty(col_of("ønsket_tendens"))
  out[["Direkte link"]] <- chr_or_empty(col_of("direkte_link"))
  out[["Kontaktperson"]] <- chr_or_empty(col_of("kontaktperson"))
  out[["Datakilde"]] <- chr_or_empty(col_of("datakilde"))
  out
}

#' Kolonne-spec til indikator-grid'et: FK-felter som dropdowns med
#' {id, name}-source (Hierarki-placering/Kontaktperson med autocomplete —
#' listerne er lange), output_enhed med kanonisk værdisæt, flag som checkbokse.
#' Ukendte eksisterende id'er/legacy-værdier bevares i sourcen.
#' @param fk db$fk_options(): named list col → data.frame(id, label)
#' @noRd
indikator_excel_columns <- function(d, fk) {
  col_of <- function(col) if (col %in% names(d)) d[[col]] else NULL
  src_of <- function(df, used, prefix) {
    s <- data.frame(
      id = as.character(df$id), name = as.character(df$label),
      stringsAsFactors = FALSE
    )
    unknown <- setdiff(stats::na.omit(as.character(used)), s$id)
    if (length(unknown) > 0) {
      s <- rbind(s, data.frame(
        id = unknown,
        name = sprintf("%s #%s", prefix, unknown), stringsAsFactors = FALSE
      ))
    }
    s
  }
  ds_src <- .hierarki_src(fk$indikator_hierarki, col_of("indikator_hierarki"))
  kp_src <- src_of(fk$kontaktperson, col_of("kontaktperson"), "Ukendt person")
  dk_src <- src_of(fk$datakilde, col_of("datakilde"), "Ukendt datakilde")
  oe_used <- setdiff(
    stats::na.omit(unique(as.character(col_of("output_enhed")))),
    OUTPUT_ENHED_CHOICES
  )
  oe_src <- data.frame(
    id = c("", OUTPUT_ENHED_CHOICES, oe_used),
    name = c("(ingen)", OUTPUT_ENHED_CHOICES, oe_used),
    stringsAsFactors = FALSE
  )
  titles <- c(
    "id", "Aktiv", "Nøgle", "Nulfyld", "Datapakke", "Datasæt",
    "Hierarki-placering", "Indikator-id", "Navn", "Mål",
    "Output-enhed", "Ønsket tendens", "Direkte link",
    "Kontaktperson", "Datakilde"
  )
  dropdown_titles <- c(
    "Hierarki-placering", "Output-enhed", "Kontaktperson",
    "Datakilde"
  )
  out <- data.frame(
    title = titles,
    type = ifelse(titles == "id", "hidden",
      ifelse(titles %in% c("Aktiv", "Nøgle", "Nulfyld"), "checkbox",
        ifelse(titles %in% dropdown_titles, "dropdown", "text")
      )
    ),
    readOnly = titles %in% c("id", "Datapakke", "Datasæt", "Indikator-id"),
    align = "left",
    autocomplete = titles %in% c("Hierarki-placering", "Kontaktperson"),
    stringsAsFactors = FALSE
  )
  srcs <- list(
    "Hierarki-placering" = ds_src, "Output-enhed" = oe_src,
    "Kontaktperson" = kp_src, "Datakilde" = dk_src
  )
  out$source <- lapply(titles, function(t) srcs[[t]] %||% NA)
  # Fraktil-bredder målt på VISTE labels (ikke id'erne)
  disp <- indikator_excel_data(d)
  for (t in names(srcs)) disp[[t]] <- .excel_dropdown_display(disp[[t]], srcs[[t]])
  out$width <- unname(excel_col_widths(disp)[titles])
  out
}

#' @noRd
mod_indikator_crud_server <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    rows <- reactiveVal(db$list_indikatorer())
    status_msg <- reactiveVal("")
    fk <- db$fk_options()
    fk_choices <- lapply(fk, function(d) stats::setNames(d$id, d$label))

    # Fejl-tolerant genindlæsning: et DB-udfald (Supabase lukker inaktive
    # forbindelser; pool-recovery kan fejle ved netværksbortfald) må ALDRIG
    # vælte sessionen — behold senest hentede rækker og sig det højt.
    reload <- function() {
      safe_operation("genindlæs indikatorer", rows(db$list_indikatorer()),
        fallback = status_msg("Databasen svarer ikke — viser senest hentede data")
      )
    }

    editing_id <- reactiveVal(NULL)
    # Swap-retur: husker hvilken indikator-modal der skal genåbnes efter
    # diagram-formularen (redigér/nyt diagram inde fra indikator-modalen).
    return_ind <- reactiveVal(NULL)
    diagram_editing_id <- reactiveVal(NULL)

    # Vis status som flydende notifikation: synlig OVER modal og uafhængigt af
    # aktiv fane (status-tekstboksen sidder kun på Oversigt-fanen). Dækker både
    # modal-valideringsfejl (modal forbliver åben) og fejl/kvittering på begge faner.
    observeEvent(status_msg(),
      {
        m <- status_msg()
        if (nzchar(m)) showNotification(m, duration = 5)
      },
      ignoreInit = TRUE
    )

    # Bygger modal-indhold (design-retning C: to kolonner 5/7, sektioner, rosa).
    # row = NULL → blank "Ny indikator"-tilstand med fornuftige defaults.
    .build_modal <- function(row = NULL) {
      ns <- session$ns
      is_new <- is.null(row)
      # Defaults for ny indikator (design: aktiv + auto-opdatering tændt)
      vals <- if (is_new) {
        list(aktiv_indikator = TRUE, tillad_auto_opdatering = TRUE)
      } else {
        as.list(row)
      }
      req_cols <- c("indikator_navn", "indikator_hierarki", "definition_kort")
      rosa_cols <- c("definition_dataportal", "tæller_beskrivelse", "nævner_beskrivelse")
      # Skalar/FK-felt med dansk label + evt. required-* + rosa-wrap
      fin <- function(col) {
        f <- Find(function(x) x$col == col, INDIKATOR_FIELDS)
        lab <- INDIKATOR_MODAL_LABELS[[col]] %||% col
        if (col %in% req_cols) lab <- tagList(lab, tags$span(" *", class = "req"))
        fkc <- fk_choices
        # Hierarki-placerings-dropdown: kun aktive hierarki-noder ved nyvalg;
        # en eksisterende inaktiv værdi bevares med "(inaktiv)"-suffix.
        if (identical(col, "indikator_hierarki")) {
          fkc$indikator_hierarki <- .hierarki_choices(
            fk$indikator_hierarki, vals$indikator_hierarki
          )
        }
        w <- .field_input(ns, f, fkc, values = vals, prefix = "m_", label = lab)
        # Rosa-klasse direkte på textarea (ej wrapper-div → bevarer fuld bredde
        # i bslib-grid'ets flex-kontekst).
        if (col %in% rosa_cols) {
          w <- htmltools::tagQuery(w)$find("textarea")$addClass("rosa")$allTags()
        }
        w
      }
      sect <- function(txt, sub = NULL) {
        div(
          class = "form-section", txt,
          if (!is.null(sub)) tags$span(class = "sub", sub)
        )
      }
      # m2m-multiselect med dansk label
      mfin <- function(key, lab) {
        opts <- db$junction_options(key)
        sel <- if (is_new) integer(0) else db$get_junction(vals$id, key)
        selectInput(ns(paste0("m_j_", key)), lab,
          choices = stats::setNames(opts$id, opts$label),
          selected = sel, multiple = TRUE
        )
      }
      # 2-up række med almindelig Bootstrap-grid (g-3) — undgår bslib-grid'ets
      # ekstra margin, så felterne ikke skubber følgende sektion for langt ned.
      two_up <- function(a, b, w = c(6, 6)) {
        div(
          class = "row gx-3",
          div(class = paste0("col-", w[1]), a), div(class = paste0("col-", w[2]), b)
        )
      }

      # Teknisk navn: VISES men kan ikke redigeres (parquet-mappings-nøgle —
      # omdøbning ville bryde koblingen til datafilerne). Disabled input
      # sender ingen værdi, og kolonnen er ikke i INDIKATOR_MODAL_COLS.
      teknisk_vis <- if (!is_new) {
        w <- textInput(ns("m_vis_teknisk"), "Indikator-id (teknisk navn — låst)",
          value = vals$indikator_navn_teknisk %||% ""
        )
        htmltools::tagQuery(w)$find("input")$addAttrs(disabled = NA)$allTags()
      }

      left <- tagList(
        sect("Stamdata"),
        fin("indikator_navn"),
        teknisk_vis,
        fin("indikator_hierarki"),
        two_up(fin("datakilde"), fin("kontaktperson")),
        two_up(fin("ønsket_tendens"), fin("mål")),
        two_up(fin("output_enhed"), fin("sp_rapport_id")),
        sect("Relationer"),
        mfin("dataprodukter", "Indgår i dataprodukter"),
        mfin("faggrupper", "Relevant for faggrupper"),
        mfin("organisation", "Relevant for afdelinger"),
        fin("direkte_link"),
        sect("Diagrammer"),
        if (is_new) {
          p(
            class = "text-muted small",
            "Gem indikatoren først for at tilføje diagrammer."
          )
        } else {
          tagList(
            uiOutput(ns("m_diagram_list")),
            actionButton(ns("m_diagram_new"), "Nyt diagram",
              class = "btn-sm btn-outline-primary"
            )
          )
        }
      )

      right <- tagList(
        sect("Definitioner & beskrivelser", "rosa felter auto-opdateres"),
        fin("definition_kort"),
        fin("definition_dataportal"),
        fin("tæller_beskrivelse"),
        fin("nævner_beskrivelse"),
        fin("indikator_ukompatibel_med"),
        sect("Datagrundlag & status"),
        two_up(fin("antal_observationer"), fin("periode_fra")),
        div(
          class = "d-flex flex-wrap gap-4 pt-1",
          fin("aktiv_indikator"), fin("nøgleindikator"),
          fin("nulfyld_tomme_perioder"), fin("tillad_auto_opdatering")
        )
      )

      modalDialog(
        title = if (is_new) "Ny indikator" else "Redigér indikator",
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
          ".footer-note{font-size:.82rem;color:#6c757d;}"
        ))),
        bslib::layout_columns(col_widths = c(5, 7), left, right),
        footer = div(
          class = "d-flex justify-content-between align-items-center w-100",
          span(class = "footer-note", HTML(
            '<span class="req">*</span> = obligatorisk'
          )),
          div(
            class = "d-flex gap-2",
            modalButton("Annullér"),
            actionButton(ns("modal_save"), "Gem og luk", class = "btn-primary")
          )
        )
      )
    }

    # "Åbn valgte": fuld modal (definitioner, m2m, diagrammer) for den
    # række der senest er klikket i grid'et
    observeEvent(input$open_selected, {
      rid <- selected_id()
      if (is.null(rid)) {
        status_msg("Vælg en række først")
        return()
      }
      row <- rows()[rows()[["id"]] == rid, , drop = FALSE]
      if (nrow(row) == 0) {
        status_msg("Indikator ikke fundet")
        return()
      }
      editing_id(as.integer(rid))
      showModal(.build_modal(row[1, , drop = FALSE]))
    })

    # Ny blank indikator → samme modal, oprettes først ved Gem
    observeEvent(input$new_modal, {
      editing_id(NULL)
      showModal(.build_modal(NULL))
    })

    observeEvent(input$modal_save, {
      rid <- editing_id() # NULL → opret ny
      # Saml KUN de felter modalen viser → udeladte kolonner (teknisk navn,
      # output_enhed) røres ej i UPDATE og bevarer deres værdi.
      modal_fields <- Filter(
        function(f) f$col %in% INDIKATOR_MODAL_COLS,
        INDIKATOR_FIELDS
      )
      vals <- .collect_form(input, modal_fields, prefix = "m_")
      errs <- validate_indikator(vals)
      if (length(errs) > 0) {
        status_msg(paste(errs, collapse = "; "))
        return()
      }
      # Saml alle valgte m2m-relationer → ét atomisk gem (scalar + junctions)
      picks <- lapply(
        names(INDIKATOR_JUNCTIONS),
        function(key) as.integer(input[[paste0("m_j_", key)]])
      )
      names(picks) <- names(INDIKATOR_JUNCTIONS)
      safe_operation("modal-gem",
        {
          if (is.null(rid)) {
            newid <- db$create_indikator_full(vals, picks)
            status_msg(paste("Oprettet indikator", newid))
          } else {
            db$save_indikator(rid, vals, picks)
            status_msg(paste("Gemt indikator", rid))
          }
          removeModal()
          reload()
        },
        fallback = status_msg("Fejl ved modal-gem (se log)")
      )
    })

    # --- Diagram-sektion i modal (swap-retur til/fra diagram-formular) -------

    # Kompakt liste over indikatorens diagrammer med redigér-link pr. række
    output$m_diagram_list <- renderUI({
      rid <- editing_id()
      if (is.null(rid)) {
        return(NULL)
      }
      ns <- session$ns
      d <- db$list_diagrams_admin()
      d <- d[d$indikator %in% rid, , drop = FALSE]
      if (nrow(d) == 0) {
        return(p(class = "text-muted small", "Ingen diagrammer endnu."))
      }
      nz <- function(x) ifelse(is.na(x), "", as.character(x))
      trows <- lapply(seq_len(nrow(d)), function(i) {
        badge <- if (d$diagram_aktivt[i] %in% TRUE) {
          tags$span(class = "badge text-bg-success", "aktiv")
        } else {
          tags$span(class = "badge text-bg-secondary", "inaktiv")
        }
        tags$tr(
          tags$td(tags$a(
            href = "#",
            onclick = sprintf(
              "Shiny.setInputValue('%s', %d, {priority: 'event'}); return false;",
              ns("m_diagram_edit"), d$diagram_id[i]
            ),
            htmltools::htmlEscape(nz(d$org_navn[i]))
          )),
          tags$td(htmltools::htmlEscape(nz(d$type_navn[i]))),
          tags$td(htmltools::htmlEscape(nz(d$periode_aggregering[i]))),
          tags$td(badge)
        )
      })
      tags$table(class = "table table-sm small mb-2", tags$tbody(trows))
    })

    # Vis diagram-formular som erstatning for indikator-modalen (swap).
    # Footer bruger actionButton "Tilbage" (IKKE modalButton) → skal trigge
    # genåbning af indikator-modalen.
    .open_diagram_modal <- function(vals, is_new) {
      ns <- session$ns
      opts <- db$diagram_form_options()
      opts$periode <- db$diagram_periode_choices()
      return_ind(editing_id())
      removeModal()
      showModal(modalDialog(
        title = if (is_new) "Nyt diagram" else "Redigér diagram",
        easyClose = FALSE,
        .diagram_form_ui(ns, vals, opts, lock_indikator = TRUE),
        footer = div(
          class = "d-flex gap-2",
          actionButton(ns("m_diagram_back"), "Tilbage"),
          actionButton(ns("m_diagram_save"), "Gem", class = "btn-primary")
        )
      ))
    }

    # Genåbn indikator-modalen for den huskede indikator
    .reopen_indikator_modal <- function() {
      rid <- return_ind()
      return_ind(NULL)
      removeModal()
      if (is.null(rid)) {
        return()
      }
      editing_id(rid)
      row <- rows()[rows()[["id"]] == rid, , drop = FALSE]
      if (nrow(row) > 0) showModal(.build_modal(row[1, , drop = FALSE]))
    }

    observeEvent(input$m_diagram_edit, {
      did <- as.integer(input$m_diagram_edit)
      d <- db$list_diagrams_admin()
      row <- d[d$diagram_id == did, , drop = FALSE]
      if (nrow(row) == 0) {
        status_msg("Diagram ikke fundet")
        return()
      }
      diagram_editing_id(did)
      .open_diagram_modal(as.list(row[1, , drop = FALSE]), is_new = FALSE)
    })

    observeEvent(input$m_diagram_new, {
      diagram_editing_id(NULL)
      .open_diagram_modal(list(indikator = editing_id(), diagram_aktivt = TRUE),
        is_new = TRUE
      )
    })

    observeEvent(input$m_diagram_save, {
      vals <- .collect_diagram_form(input)
      errs <- validate_diagram(vals)
      if (length(errs) > 0) {
        status_msg(paste(errs, collapse = "; "))
        return()
      }
      did <- diagram_editing_id()
      dup <- db$diagram_duplicate_count(vals$indikator,
        vals$organisatorisk_navn_teknisk, vals$diagram_type,
        exclude_id = did %||% -1L
      )
      if (dup > 0) {
        showNotification(paste(
          "Findes allerede: samme indikator/enhed/type",
          "har et diagram i forvejen."
        ), type = "warning")
      }
      safe_operation("diagram-gem (modal)",
        {
          if (is.null(did)) {
            newid <- db$create_diagram(vals)
            status_msg(paste("Oprettet diagram", newid))
          } else {
            db$update_diagram(did, vals)
            status_msg(paste("Gemt diagram", did))
          }
          .reopen_indikator_modal()
        },
        fallback = status_msg("Fejl ved gem af diagram (se log)")
      )
    })

    observeEvent(input$m_diagram_back, .reopen_indikator_modal())

    # Den VISTE tabel (status/datapakke/datasæt-filtre) — delt af render,
    # celle-diff og række-selektion, så indeks aldrig kan pege på en anden
    # række end den der vises.
    tbl_rows <- reactive({
      d <- rows()
      status <- input$filter_status %||% "alle"
      if (identical(status, "aktiv")) {
        d <- d[d$aktiv_indikator %in% TRUE, , drop = FALSE]
      }
      if (identical(status, "inaktiv")) {
        d <- d[!(d$aktiv_indikator %in% TRUE), , drop = FALSE]
      }
      if (identical(status, "noegle")) {
        d <- d[d$nøgleindikator %in% TRUE, , drop = FALSE]
      }
      fdp <- input$filter_datapakke
      if (!is.null(fdp) && nzchar(fdp)) {
        d <- d[d$label_datapakke %in% fdp, , drop = FALSE]
      }
      fds <- input$filter_datasaet
      if (!is.null(fds) && nzchar(fds)) {
        d <- d[d$label_datasaet %in% fds, , drop = FALSE]
      }
      d
    })
    tbl_refresh <- reactiveVal(0) # bump → snap-back efter fejlet gem
    tbl_sel <- reactiveVal(NULL) # pk (chr) for senest valgte række

    output$tbl <- excelR::renderExcel({
      tbl_refresh()
      d <- tbl_rows()
      excelR::excelTable(
        data = indikator_excel_data(d),
        columns = indikator_excel_columns(d, fk),
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
    })

    # Datapakke-filter: valg afledt af de datapakke-værdier der faktisk findes
    output$filter_datapakke_ui <- renderUI({
      ns <- session$ns
      vals <- sort(unique(stats::na.omit(rows()[["label_datapakke"]])))
      choices <- c("Alle" = "", stats::setNames(vals, vals))
      selected <- .preserved_filter_selection(
        isolate(input$filter_datapakke), choices
      )
      selectInput(ns("filter_datapakke"), "Datapakke",
        choices = choices, selected = selected
      )
    })

    # Datasæt-filter: kaskaderer på valgt datapakke (viser kun datasæt
    # derunder). Bruger det NIVEAU-UDLEDTE datasæt (label_datasaet) — nodens
    # eget navn kan være en indikatorsamling under datasættet.
    output$filter_datasaet_ui <- renderUI({
      ns <- session$ns
      d <- rows()
      fdp <- input$filter_datapakke
      if (!is.null(fdp) && nzchar(fdp)) {
        d <- d[d$label_datapakke %in% fdp, , drop = FALSE]
      }
      vals <- sort(unique(stats::na.omit(d[["label_datasaet"]])))
      choices <- c("Alle" = "", stats::setNames(vals, vals))
      selected <- .preserved_filter_selection(
        isolate(input$filter_datasaet), choices
      )
      selectInput(ns("filter_datasaet"), "Datasæt",
        choices = choices, selected = selected
      )
    })

    selected_id <- reactive({
      sel <- tbl_sel()
      d <- tbl_rows()
      j <- if (is.null(sel)) NA_integer_ else match(sel, as.character(d[["id"]]))
      if (is.na(j)) {
        return(NULL)
      } # pk væk fra den viste tabel → intet valg
      d[["id"]][j]
    })

    observeEvent(input$soft_delete, {
      sid <- selected_id()
      if (is.null(sid)) {
        status_msg("Vælg en række først")
        return()
      }
      safe_operation("soft-delete",
        {
          db$soft_delete(sid, active = FALSE)
          status_msg("Deaktiveret")
          reload()
        },
        fallback = status_msg("Fejl ved deaktivering")
      )
    })

    # excelR sender BÅDE celle-ændringer og selektioner på input$tbl —
    # forSelectedVals skelner. Ændringer diffes mod den VISTE tabel (pk-match)
    # og skrives enkeltvis (update_indikator med KUN det ændrede felt);
    # readOnly-kolonner kan ikke redigeres i grid'et, men diffen guarder
    # alligevel (klient-manipulation).
    .IND_FK_FIELDS <- c("indikator_hierarki", "kontaktperson", "datakilde")
    .IND_BOOL_FIELDS <- c(
      "aktiv_indikator", "nøgleindikator",
      "nulfyld_tomme_perioder"
    )
    observeEvent(input$tbl, {
      p <- input$tbl
      if (isTRUE(p$forSelectedVals)) {
        # pk læses fra payloadens fullData (grid'ets aktuelle — evt.
        # klient-sorterede — rækkefølge), aldrig fra positionen alene
        tbl_sel(excel_selected_pk(p))
        return()
      }
      d <- tbl_rows()
      changes <- excel_diff_cells(
        indikator_excel_data(d),
        excel_payload_to_df(p), "id"
      )
      changes <- changes[changes$col %in% names(.INDIKATOR_GRID_FIELDS), ,
        drop = FALSE
      ]
      if (nrow(changes) == 0) {
        return()
      }
      revert <- FALSE
      for (k in seq_len(nrow(changes))) {
        rid <- d[["id"]][match(changes$pk[k], as.character(d[["id"]]))]
        field <- .INDIKATOR_GRID_FIELDS[[changes$col[k]]]
        val <- changes$value[k]
        if (field %in% .IND_FK_FIELDS) {
          # FK-dropdowns har intet tom-valg: tømt/ugyldig celle → afvis
          iv <- suppressWarnings(as.integer(val))
          if (is.na(iv)) {
            status_msg("Vælg en værdi fra listen")
            revert <- TRUE
            next
          }
          val <- iv
        } else if (field %in% .IND_BOOL_FIELDS) {
          val <- identical(val, "TRUE")
        } else if (identical(field, "indikator_navn") && is.na(val)) {
          status_msg("Navn på indikator er obligatorisk")
          revert <- TRUE
          next
        }
        ok <- safe_operation("inline-update",
          {
            db$update_indikator(rid, stats::setNames(list(val), field))
            TRUE
          },
          fallback = FALSE
        )
        if (isTRUE(ok)) {
          status_msg(paste("Opdateret", field))
        } else {
          status_msg("Fejl ved inline-update")
          revert <- TRUE
        }
      }
      reload() # genindlæs fra DB → grid viser den gemte tilstand
      if (revert) tbl_refresh(tbl_refresh() + 1)
    })

    output$status <- renderText(status_msg())

    # eksponér til test
    list(
      rows = rows, status_msg = status_msg, editing_id = editing_id,
      return_ind = return_ind
    )
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
