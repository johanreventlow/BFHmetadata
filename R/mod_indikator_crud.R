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

# Felter modalen viser (design-retning C). indikator_navn_teknisk ER med:
# feltet er parquet-mappings-nøglen, så omdøbning bryder datakoblingen indtil
# mappen på disken omdøbes tilsvarende — men et forkert id skal kunne rettes.
# Modalen er derfor det ENESTE sted feltet kan ændres, og en ændring på en
# eksisterende indikator kræver bekræftelse (se .teknisk_uaendret + pending_save).
# Grid'et og bulk-redigering holder det fortsat readOnly.
# Danske labels + required/rosa-markering styres i .build_modal.
INDIKATOR_MODAL_COLS <- c(
  "indikator_navn", "indikator_navn_teknisk",
  "indikator_hierarki", "datakilde", "kontaktperson",
  "\u00F8nsket_tendens", "m\u00E5l", "output_enhed", "sp_rapport_id", "direkte_link",
  "definition_kort", "definition_dataportal", "t\u00E6ller_beskrivelse",
  "n\u00E6vner_beskrivelse", "indikator_ukompatibel_med", "antal_observationer",
  "periode_fra", "aktiv_indikator", "n\u00F8gleindikator",
  "nulfyld_tomme_perioder", "tillad_auto_opdatering"
)

# Danske felt-labels i modalen (col → vist tekst)
INDIKATOR_MODAL_LABELS <- c(
  indikator_navn = "Navn p\u00E5 indikator",
  indikator_navn_teknisk = "Indikator-id (teknisk navn)",
  indikator_hierarki = "Hierarki-placering",
  datakilde = "Datakilde", kontaktperson = "Kontaktperson",
  "\u00f8nsket_tendens" = "\u00D8nsket retning", "m\u00e5l" = "Generelt indikatorm\u00E5l",
  output_enhed = "Output-enhed",
  sp_rapport_id = "Evt. SP rapport id", direkte_link = "Evt. direkte link",
  definition_kort = "Kort definition", definition_dataportal = "Definition til dataportal",
  "t\u00e6ller_beskrivelse" = "Beskrivelse af t\u00E6ller", "n\u00e6vner_beskrivelse" = "Beskrivelse af n\u00E6vner",
  indikator_ukompatibel_med = "Kommentarer vedr. anvendelse",
  antal_observationer = "Antal observationer", periode_fra = "Periode fra",
  aktiv_indikator = "Aktiv indikator", "n\u00f8gleindikator" = "N\u00F8gleindikator",
  nulfyld_tomme_perioder = "Nulfyld tomme perioder",
  tillad_auto_opdatering = "Auto-opdat\u00E9r rosa felter"
)

#' @noRd
mod_indikator_crud_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "mt-2",
    div(
      class = "d-flex justify-content-end gap-2 mb-2",
      actionButton(ns("new_modal"), "Ny indikator", class = "btn-success"),
      actionButton(ns("open_selected"), "\u00C5bn valgte",
        class = "btn-outline-primary"
      ),
      actionButton(ns("soft_delete"), "Deaktiv\u00E9r valgte",
        class = "btn-warning"
      ),
      actionButton(ns("select_all_visible"), "V\u00E6lg alle viste",
        class = "btn-outline-secondary"
      ),
      uiOutput(ns("bulk_edit_btn"), inline = TRUE)
    ),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      uiOutput(ns("filter_datapakke_ui")),
      uiOutput(ns("filter_datasaet_ui")),
      selectInput(ns("filter_status"), "Status",
        choices = c(
          "Alle" = "alle", "Kun aktive" = "aktiv",
          "Kun inaktive" = "inaktiv",
          "N\u00F8gleindikatorer" = "noegle"
        ),
        selected = "alle"
      )
    ),
    p(class = "text-muted small", paste(
      "Dobbeltklik en celle for at redigere direkte.",
      "Klik en r\u00E6kke og brug '\u00C5bn valgte' for definitioner, relationer",
      "og diagrammer \u2014 eller 'Deaktiv\u00E9r valgte'."
    )),
    uiOutput(ns("tbl_container")),
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

#' Er to indikator-id'er (indikator_navn_teknisk) reelt den samme vaerdi?
#' Bruges til at afgoere om modal-gem skal kraeve bekraeftelse: kun en FAKTISK
#' omdoebning maa udloese dialogen — at aabne og gemme uden at roere feltet maa
#' ikke. NULL/NA/length-0/blanke kanter normaliseres til samme tomme vaerdi, saa
#' "a" vs " a " ikke taelles som en aendring (browseren kan tilfoeje whitespace),
#' mens forskel paa store/smaa bogstaver TAELLER — parquet-opslaget slaar op i
#' filsystemet, og det er case-sensitivt paa Linux.
#' @noRd
.teknisk_uaendret <- function(a, b) {
  norm <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    x <- as.character(x)[1]
    if (is.na(x)) "" else trimws(x)
  }
  identical(norm(a), norm(b))
}

#' Bekraeftelsesdialog foer en omdoebning af indikator-id.
#' Ren byggefunktion (ingen DB/skrivning) — selve gemmet ligger i observeren
#' bundet til teknisk_confirm. Viser fra/til eksplicit, saa brugeren kan se
#' praecis hvilken streng parquet-opslaget skifter til; tom vaerdi vises som
#' "(tomt)" fremfor NA, da et tomt id betyder ingen datakobling overhovedet.
#' @param ns modulets session$ns
#' @noRd
.byg_teknisk_confirm <- function(gammelt, nyt, ns) {
  vis <- function(x) {
    if (is.null(x) || length(x) == 0) return("(tomt)")
    x <- as.character(x)[1]
    if (is.na(x) || !nzchar(trimws(x))) "(tomt)" else x
  }
  build_confirm_modal(
    title = "\u00C6ndr indikator-id?",
    body = tagList(
      p(paste(
        "Indikator-id er n\u00F8glen til datafilerne: appen finder indikatorens",
        "parquet-data ved at sl\u00E5 id'et op som mappe- og filnavn i",
        "parquet-lageret."
      )),
      tags$ul(
        tags$li(tags$b("Fra: "), tags$code(vis(gammelt))),
        tags$li(tags$b("Til: "), tags$code(vis(nyt)))
      )
    ),
    warning = paste(
      "Indtil mappen/filen i parquet-lageret omd\u00F8bes tilsvarende, kan appen",
      "ikke finde data for indikatoren \u2014 signal-gennemgang og diagrammer",
      "vil fejle for den."
    ),
    confirm_id = ns("teknisk_confirm"),
    confirm_label = "\u00C6ndr indikator-id",
    confirm_class = "btn-warning",
    cancel_id = ns("teknisk_cancel")
  )
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
  "Aktiv" = "aktiv_indikator", "N\u00F8gle" = "n\u00F8gleindikator",
  "Nulfyld" = "nulfyld_tomme_perioder",
  "Hierarki-placering" = "indikator_hierarki", "Navn" = "indikator_navn",
  "M\u00E5l" = "m\u00E5l", "Output-enhed" = "output_enhed",
  "\u00D8nsket tendens" = "\u00F8nsket_tendens", "Direkte link" = "direkte_link",
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
  out[["N\u00F8gle"]] <- col_of("n\u00F8gleindikator") %in% TRUE
  out[["Nulfyld"]] <- col_of("nulfyld_tomme_perioder") %in% TRUE
  out[["Datapakke"]] <- chr_or_empty(col_of("label_datapakke"))
  out[["Datas\u00E6t"]] <- chr_or_empty(col_of("label_datasaet"))
  out[["Hierarki-placering"]] <- chr_or_empty(col_of("indikator_hierarki"))
  out[["Indikator-id"]] <- chr_or_empty(col_of("indikator_navn_teknisk"))
  out[["Navn"]] <- chr_or_empty(col_of("indikator_navn"))
  out[["M\u00E5l"]] <- chr_or_empty(col_of("m\u00E5l"))
  out[["Output-enhed"]] <- chr_or_empty(col_of("output_enhed"))
  out[["\u00D8nsket tendens"]] <- chr_or_empty(col_of("\u00F8nsket_tendens"))
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
    "id", "Aktiv", "N\u00F8gle", "Nulfyld", "Datapakke", "Datas\u00E6t",
    "Hierarki-placering", "Indikator-id", "Navn", "M\u00E5l",
    "Output-enhed", "\u00D8nsket tendens", "Direkte link",
    "Kontaktperson", "Datakilde"
  )
  dropdown_titles <- c(
    "Hierarki-placering", "Output-enhed", "Kontaktperson",
    "Datakilde"
  )
  out <- data.frame(
    title = titles,
    type = ifelse(titles == "id", "hidden",
      ifelse(titles %in% c("Aktiv", "N\u00F8gle", "Nulfyld"), "checkbox",
        ifelse(titles %in% dropdown_titles, "dropdown", "text")
      )
    ),
    readOnly = titles %in% c("id", "Datapakke", "Datas\u00E6t", "Indikator-id"),
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
    # Hierarki-placering vises som indrykket træ (depth-first) i både
    # grid-dropdown og modal — kræver parent_id fra fk_options (metadata.R)
    fk$indikator_hierarki <- hierarchy_indent_options(fk$indikator_hierarki)
    fk_choices <- lapply(fk, function(d) stats::setNames(d$id, d$label))

    # Fejl-tolerant genindlæsning: et DB-udfald (Supabase lukker inaktive
    # forbindelser; pool-recovery kan fejle ved netværksbortfald) må ALDRIG
    # vælte sessionen — behold senest hentede rækker og sig det højt.
    reload <- function() {
      safe_operation("genindl\u00E6s indikatorer", rows(db$list_indikatorer()),
        fallback = status_msg("Databasen svarer ikke \u2014 viser senest hentede data")
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
    # overrides/junctions: pre-udfyld med brugerens UAFSENDTE indtastninger i
    # stedet for DB-tilstanden. Bruges naar bekraeftelsesdialogen for et aendret
    # indikator-id fortrydes — dialogen erstattede modalen, saa formularen skal
    # genopbygges som brugeren forlod den (ellers tabes alt andet arbejde i den).
    .build_modal <- function(row = NULL, overrides = NULL, junctions = NULL) {
      ns <- session$ns
      is_new <- is.null(row)
      # Defaults for ny indikator (design: aktiv + auto-opdatering tændt)
      vals <- if (is_new) {
        list(aktiv_indikator = TRUE, tillad_auto_opdatering = TRUE)
      } else {
        as.list(row)
      }
      if (length(overrides) > 0) vals[names(overrides)] <- overrides
      req_cols <- c("indikator_navn", "indikator_hierarki", "definition_kort")
      rosa_cols <- c("definition_dataportal", "t\u00E6ller_beskrivelse", "n\u00E6vner_beskrivelse")
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
        sel <- if (!is.null(junctions)) {
          junctions[[key]] %||% integer(0)
        } else if (is_new) {
          integer(0)
        } else {
          db$get_junction(vals$id, key)
        }
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

      # Indikator-id (teknisk navn): appen slår datafilerne op på PRÆCIS denne
      # streng (parquet_indicator_path / parquet_compact_file), så en omdøbning
      # her kræver at mappen på disken omdøbes tilsvarende. Feltet ER
      # redigerbart — nogle id'er er forkerte fra start — men gem på en
      # EKSISTERENDE indikator går via bekræftelsesdialogen i input$modal_save.
      # Ved oprettelse bekræftes ikke: ingen data peger på indikatoren endnu,
      # så der er intet at bryde.
      teknisk_vis <- tagList(
        fin("indikator_navn_teknisk"),
        div(
          class = "form-text text-muted small",
          paste0(
            "Kobler indikatoren til parquet-lageret \u2014 skal matche ",
            "mappe-/filnavnet p\u00E5 disken."
          )
        )
      )

      left <- tagList(
        sect("Stamdata"),
        fin("indikator_navn"),
        teknisk_vis,
        fin("indikator_hierarki"),
        two_up(fin("datakilde"), fin("kontaktperson")),
        two_up(fin("\u00F8nsket_tendens"), fin("m\u00E5l")),
        two_up(fin("output_enhed"), fin("sp_rapport_id")),
        sect("Relationer"),
        mfin("dataprodukter", "Indg\u00E5r i dataprodukter"),
        mfin("faggrupper", "Relevant for faggrupper"),
        mfin("organisation", "Relevant for afdelinger"),
        fin("direkte_link"),
        sect("Diagrammer"),
        if (is_new) {
          p(
            class = "text-muted small",
            "Gem indikatoren f\u00F8rst for at tilf\u00F8je diagrammer."
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
        fin("t\u00E6ller_beskrivelse"),
        fin("n\u00E6vner_beskrivelse"),
        fin("indikator_ukompatibel_med"),
        sect("Datagrundlag & status"),
        two_up(fin("antal_observationer"), fin("periode_fra")),
        div(
          class = "d-flex flex-wrap gap-4 pt-1",
          fin("aktiv_indikator"), fin("n\u00F8gleindikator"),
          fin("nulfyld_tomme_perioder"), fin("tillad_auto_opdatering")
        )
      )

      modalDialog(
        title = if (is_new) "Ny indikator" else "Redig\u00E9r indikator",
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
            modalButton("Annull\u00E9r"),
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
        status_msg("V\u00E6lg en r\u00E6kke f\u00F8rst")
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

    # Selve skrivningen fra modalen — delt af det direkte gem og af gem efter
    # bekræftet omdøbning af indikator-id, så begge veje er én transaktion.
    .gem_modal <- function(rid, vals, picks) {
      safe_operation("modal-gem",
        med_ventevisning("Gemmer…", {
          if (is.null(rid)) {
            newid <- db$create_indikator_full(vals, picks)
            status_msg(paste("Oprettet indikator", newid))
          } else {
            db$save_indikator(rid, vals, picks)
            status_msg(paste("Gemt indikator", rid))
          }
          removeModal()
          reload()
          TRUE
        }),
        fallback = {
          status_msg("Fejl ved modal-gem (se log)")
          FALSE
        }
      )
    }

    # Nuværende indikator-id fra den senest hentede DB-runde (ikke fra
    # formularen), så sammenligningen sker mod det der faktisk står i basen.
    .teknisk_i_db <- function(rid) {
      d <- rows()
      if (is.null(rid) || !("indikator_navn_teknisk" %in% names(d))) {
        return(NA_character_)
      }
      j <- match(as.character(rid), as.character(d[["id"]]))
      if (is.na(j)) NA_character_ else as.character(d[["indikator_navn_teknisk"]][j])
    }

    # Gem-payload der venter på bekræftelse af en omdøbning. Holder OGSÅ picks,
    # så en fortrydelse kan genopbygge modalen som brugeren forlod den.
    pending_save <- reactiveVal(NULL)

    observeEvent(input$modal_save, {
      rid <- editing_id() # NULL → opret ny
      # Saml KUN de felter modalen viser → udeladte kolonner røres ej i UPDATE
      # og bevarer deres værdi.
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

      # Omdøbt indikator-id på en EKSISTERENDE indikator → bekræft først. Kun
      # når feltet rent faktisk blev sendt med (.collect_form udelader kolonnen
      # når inputtet ikke findes) OG værdien er en anden end databasens.
      gammelt <- .teknisk_i_db(rid)
      har_felt <- "indikator_navn_teknisk" %in% names(vals)
      if (!is.null(rid) && har_felt &&
        !.teknisk_uaendret(gammelt, vals$indikator_navn_teknisk)) {
        pending_save(list(rid = rid, vals = vals, picks = picks))
        showModal(.byg_teknisk_confirm(
          gammelt, vals$indikator_navn_teknisk, session$ns
        ))
        return()
      }

      .gem_modal(rid, vals, picks)
    })

    observeEvent(input$teknisk_confirm, {
      p <- pending_save()
      if (is.null(p)) {
        removeModal()
        return()
      }
      # .gem_modal lukker selv modalen ved succes. Ved fejl bliver dialogen
      # stående OG payloaden bevares, så et nyt klik prøver igen med de samme
      # værdier i stedet for at tabe brugerens indtastninger.
      if (isTRUE(.gem_modal(p$rid, p$vals, p$picks))) pending_save(NULL)
    })

    # Fortryd: dialogen ERSTATTEDE indikator-modalen, så den skal bygges op
    # igen — med brugerens uafsendte værdier (inkl. det ændrede id, som de selv
    # kan rette tilbage), ikke med DB-tilstanden.
    observeEvent(input$teknisk_cancel, {
      p <- pending_save()
      pending_save(NULL)
      removeModal()
      if (is.null(p)) {
        return()
      }
      d <- rows()
      row <- d[as.character(d[["id"]]) == as.character(p$rid), , drop = FALSE]
      if (nrow(row) == 0) {
        status_msg("Indikator ikke fundet")
        return()
      }
      showModal(.build_modal(row[1, , drop = FALSE],
        overrides = p$vals, junctions = p$picks
      ))
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
        title = if (is_new) "Nyt diagram" else "Redig\u00E9r diagram",
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
        med_ventevisning("Gemmer…", {
          if (is.null(did)) {
            newid <- db$create_diagram(vals)
            status_msg(paste("Oprettet diagram", newid))
          } else {
            db$update_diagram(did, vals)
            status_msg(paste("Gemt diagram", did))
          }
          .reopen_indikator_modal()
        }),
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
        d <- d[d[["n\u00f8gleindikator"]] %in% TRUE, , drop = FALSE]
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
    tbl_sel <- reactiveVal(character(0)) # pk-vektor (chr) for den valgte range

    # Selektionen begrænset til rækker der faktisk er i den VISTE (filtrerede)
    # tabel lige nu — et filterskift efterlader ikke en optælling der peger på
    # skjulte rækker.
    tbl_sel_visible <- reactive({
      intersect(tbl_sel(), as.character(tbl_rows()[["id"]]))
    })

    # Viser en forklarende tom-tilstand i stedet for et tomt grid, når
    # filtrene ikke matcher noget — ellers det redigerbare excelR-grid.
    output$tbl_container <- renderUI({
      tbl_refresh()
      d <- tbl_rows()
      ns <- session$ns
      if (nrow(d) == 0) {
        return(tom_tilstand_ui(ns, has_filters = har_aktive_filtre(
          input$filter_datapakke, input$filter_datasaet, input$filter_status
        )))
      }
      excelR::excelOutput(ns("tbl"), width = "100%", height = "auto")
    })

    observeEvent(input$ryd_filtre, {
      updateSelectInput(session, "filter_datapakke", selected = "")
      updateSelectInput(session, "filter_datasaet", selected = "")
      updateSelectInput(session, "filter_status", selected = "alle")
    })

    output$tbl <- excelR::renderExcel({
      tbl_refresh()
      d <- tbl_rows()
      req(nrow(d) > 0)
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
      selectInput(ns("filter_datasaet"), "Datas\u00E6t",
        choices = choices, selected = selected
      )
    })

    selected_id <- reactive({
      sel <- tbl_sel()
      d <- tbl_rows()
      j <- if (length(sel) == 0) NA_integer_ else match(sel[1], as.character(d[["id"]]))
      if (is.na(j)) {
        return(NULL)
      } # pk væk fra den viste tabel → intet valg
      d[["id"]][j]
    })

    observeEvent(input$select_all_visible, {
      tbl_sel(as.character(tbl_rows()[["id"]]))
    })

    # "Redigér valgte (N)" — knap-tekst reflekterer selektionen reaktivt, men
    # selve bulk-flowet leveres først i en senere leverance (batch-kontrakt +
    # audit i DB-laget) — knappen er derfor disabled her.
    output$bulk_edit_btn <- renderUI({
      ns <- session$ns
      n <- length(tbl_sel_visible())
      actionButton(ns("bulk_edit"),
        sprintf("Redigér valgte (%d)", n),
        class = "btn-outline-secondary", disabled = "disabled",
        title = "Kommer i en senere leverance"
      )
    })

    # Bekr\u00E6ftelse f\u00F8r deaktivering \u2014 skriver intet selv. Selve skrivningen
    # sker i input$soft_delete_confirm. sid fryses i pending_soft_delete_id,
    # s\u00E5 et evt. selektionsskift mens dialogen er \u00E5ben ikke \u00E6ndrer hvad der
    # rent faktisk deaktiveres.
    pending_soft_delete_id <- reactiveVal(NULL)
    observeEvent(input$soft_delete, {
      sid <- selected_id()
      if (is.null(sid)) {
        status_msg("V\u00E6lg en r\u00E6kke f\u00F8rst")
        return()
      }
      pending_soft_delete_id(sid)
      showModal(build_confirm_modal(
        title = "Deaktiv\u00E9r indikator?",
        body = p("Indikatoren skjules fra aktive lister. Data slettes ikke."),
        confirm_id = session$ns("soft_delete_confirm"),
        confirm_label = "Deaktiv\u00E9r",
        confirm_class = "btn-warning"))
    })

    observeEvent(input$soft_delete_confirm, {
      sid <- pending_soft_delete_id()
      removeModal()
      if (is.null(sid)) return()
      safe_operation("soft-delete",
        med_ventevisning("Deaktiverer…", {
          db$soft_delete(sid, active = FALSE)
          status_msg("Deaktiveret")
          reload()
        }),
        fallback = status_msg("Fejl ved deaktivering")
      )
      pending_soft_delete_id(NULL)
    })

    # excelR sender BÅDE celle-ændringer og selektioner på input$tbl —
    # forSelectedVals skelner. Ændringer diffes mod den VISTE tabel (pk-match)
    # og skrives enkeltvis (update_indikator med KUN det ændrede felt);
    # readOnly-kolonner kan ikke redigeres i grid'et, men diffen guarder
    # alligevel (klient-manipulation). Diffen køres OGSÅ på selektions-
    # payloads: markør-flytningen efter en celle-commit overskriver
    # change-eventet i Shinys input-batch (se excel_event_df) — uden diff
    # her ville tekst-redigeringer først gemmes ved næste checkbox-klik.
    .IND_FK_FIELDS <- c("indikator_hierarki", "kontaktperson", "datakilde")
    .IND_BOOL_FIELDS <- c(
      "aktiv_indikator", "n\u00F8gleindikator",
      "nulfyld_tomme_perioder"
    )
    observeEvent(input$tbl, {
      p <- input$tbl
      if (isTRUE(p$forSelectedVals)) {
        # pk'er læses fra payloadens fullData (grid'ets aktuelle — evt.
        # klient-sorterede — rækkefølge), aldrig fra positionen alene.
        # Dækker range-selektion (borderTop..borderBottom), ikke kun ét klik.
        tbl_sel(excel_selected_pks(p) %||% character(0))
      }
      d <- tbl_rows()
      changes <- excel_diff_cells(
        indikator_excel_data(d),
        excel_event_df(p), "id"
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
            status_msg("V\u00E6lg en v\u00E6rdi fra listen")
            revert <- TRUE
            next
          }
          val <- iv
        } else if (field %in% .IND_BOOL_FIELDS) {
          val <- identical(val, "TRUE")
        } else if (identical(field, "indikator_navn") && is.na(val)) {
          status_msg("Navn p\u00E5 indikator er obligatorisk")
          revert <- TRUE
          next
        }
        ok <- safe_operation("inline-update",
          med_ventevisning("Gemmer…", {
            db$update_indikator(rid, stats::setNames(list(val), field))
            TRUE
          }),
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
      return_ind = return_ind, tbl_sel = tbl_sel
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
