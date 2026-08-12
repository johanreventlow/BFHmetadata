# Modul: Signal-gennemgang. Peg på parquet-mappe → scan filtrerede aktive
# Seriediagrammer → vis signal-diagrammer interaktivt → registrér faseskift.

#' @noRd
mod_signal_review_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320, open = TRUE,
      textInput(ns("parquet_dir"), "Parquet-mappe", placeholder = "/sti/til/parquet"),
      div(
        class = "d-flex gap-2 align-items-end",
        radioButtons(ns("window_mode"), "Datavindue",
          c("Alle data" = "all", "Seneste N" = "latest"),
          inline = TRUE
        ),
        # N tæller PERIODER (uger/måneder), ej dage — 36 matcher BFHddl's
        # effektive default (antal_observationer er NULL for ~alle indikatorer).
        numericInput(ns("window_n"), "N", value = 36, min = 6, max = 200, width = "90px")
      ),
      uiOutput(ns("window_hint")),
      hr(),
      selectizeInput(ns("f_overafdeling"), "Overafdeling", NULL, multiple = FALSE),
      selectizeInput(ns("f_afsnit"), "Afsnit", NULL, multiple = FALSE),
      selectizeInput(ns("f_datapakke"), "Datapakke", NULL, multiple = FALSE),
      selectizeInput(ns("f_datasaet"), "Datasæt", NULL, multiple = FALSE),
      selectizeInput(ns("f_indikator_navn"), "Indikator", NULL, multiple = FALSE),
      checkboxInput(ns("force_refresh"), "Ignorér dags-cache (genindlæs data)",
        value = FALSE
      ),
      actionButton(ns("scan"), "Scan", class = "btn-primary w-100"),
      actionButton(ns("stop_scan"), "Stop scan",
        class = "btn-outline-secondary btn-sm w-100 mt-1"
      ),
      uiOutput(ns("scan_summary")),
      hr(),
      checkboxInput(ns("show_no_signal"), "Vis også diagrammer uden signal",
        value = FALSE
      ),
      bslib::accordion(
        id = ns("diagram_acc"), open = FALSE,
        bslib::accordion_panel("Diagramliste", uiOutput(ns("diagram_list")))
      )
    ),
    # Hovedområde: navigation + graf + faseskift
    div(
      class = "d-flex justify-content-between align-items-center mb-2",
      uiOutput(ns("nav_label")),
      div(
        class = "btn-group",
        actionButton(ns("prev"), "‹ Forrige", class = "btn-outline-secondary btn-sm"),
        actionButton(ns("next_"), "Næste ›", class = "btn-outline-secondary btn-sm")
      )
    ),
    uiOutput(ns("agg_badge")),
    uiOutput(ns("break_warning")),
    ggiraph::girafeOutput(ns("chart"), height = "420px"),
    div(class = "small", tableOutput(ns("phase_stats"))),
    hr(),
    div(
      class = "d-flex gap-2 align-items-center flex-wrap",
      uiOutput(ns("selected_label")),
      actionButton(ns("preview"), "Forhåndsvis faseskift", class = "btn-outline-primary btn-sm"),
      actionButton(ns("save_break"), "Gem faseskift", class = "btn-success btn-sm")
    ),
    div(
      class = "mt-3",
      h6("Eksisterende median-knæk"),
      DT::DTOutput(ns("breaks_tbl")),
      actionButton(ns("delete_break"), "Fjern valgt knæk", class = "btn-outline-danger btn-sm mt-1")
    )
  )
}

#' @noRd
mod_signal_review_server <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    index <- reactiveVal(db$list_active_seriediagrammer())
    variants <- reactiveVal(db$org_enhed_variants())
    cache <- reactiveVal(list()) # nøgle "<diagram_id>|<window>" → scan-res
    scanned_list <- reactiveVal(NULL) # df: ALLE ok-scannede (+ signal/status)
    show_all <- reactiveVal(FALSE) # checkbox-tilstand (styret via observer,
    # så cursor kan remappes deterministisk)
    # Visningen der bladres i: checkbox fra -> kun signal; til -> alle ok-scannede
    view_list <- reactive(scan_view_filter(scanned_list(), show_all()))
    # Bagudkompat (tests + evt. eksterne læsere): kun signal-diagrammer
    signal_list <- reactive(scan_view_filter(scanned_list(), FALSE))
    cursor <- reactiveVal(1L)
    preview_parts <- reactiveVal(NULL) # ekstra forhåndsvist knæk (dato)
    selected_cursor <- reactiveVal(NULL) # cursor-stempel da punkt sidst blev klikket
    scanned_n <- reactiveVal(NULL) # vindue-værdi (NULL/int) brugt ved SIDSTE scan
    # org-træ + aggregerings-flag til hierarki-oprulning (Task 4). Hentes én
    # gang pr. scan (se scan-start-observeren) — list(os=..., fl=...); NULL
    # indtil første scan, hvilket blot slår oprulning fra (safe default).
    agg_ctx <- reactiveVal(NULL)

    # ggiraph-input kan ikke nulstilles fra server → stempl med cursor ved klik,
    # så et valg fra ET diagram aldrig læses som gyldigt på et ANDET (Task 7-guard).
    observeEvent(input$chart_selected, selected_cursor(cursor()))

    # Checkbox-toggle: bevar det aktuelle diagram i visningen hvis muligt.
    # show_all opdateres HER (ikke direkte fra input) så gammel/ny visning kan
    # beregnes side om side - en reaktiv afledning ville miste den gamle
    # position før remap.
    observeEvent(input$show_no_signal,
      {
        sl <- scanned_list()
        old_view <- scan_view_filter(sl, show_all())
        old_id <- if (!is.null(old_view) && nrow(old_view) > 0) {
          old_view$diagram_id[min(cursor(), nrow(old_view))]
        } else {
          NULL
        }
        show_all(isTRUE(input$show_no_signal))
        new_view <- scan_view_filter(sl, show_all())
        pos <- if (!is.null(old_id) && !is.null(new_view)) {
          match(old_id, new_view$diagram_id)
        } else {
          NA_integer_
        }
        cursor(if (is.na(pos)) 1L else as.integer(pos))
        # Fallback (diagrammet forsvandt) kan flytte cursoren til et ANDET
        # diagram på SAMME numeriske position (Task 7-guard: valid_selected_date
        # sammenligner kun cursor-tal, ikke diagram-id). Et klik-stempel fra FØR
        # togglet må derfor aldrig overleve - ellers kan "Gem faseskift" skrive
        # på det forkerte diagram.
        selected_cursor(NULL)
        preview_parts(NULL)
      },
      ignoreInit = TRUE
    )

    # Klik i sidebar-listen: spring direkte til diagrammet. Id-baseret (ej
    # position) så et klik på en stale-renderet liste aldrig rammer forkert
    # række — ukendte id'er ignoreres blot.
    observeEvent(input$goto_diagram, {
      vl <- view_list()
      if (is.null(vl) || nrow(vl) == 0) {
        return()
      }
      pos <- match(as.integer(input$goto_diagram), vl$diagram_id)
      if (is.na(pos)) {
        return()
      }
      cursor(as.integer(pos))
      preview_parts(NULL)
    })

    output$diagram_list <- renderUI({
      vl <- view_list()
      if (is.null(vl)) {
        return(div(class = "small text-muted", "Ingen scan endnu"))
      }
      if (nrow(vl) == 0) {
        return(div(class = "small text-muted", "Ingen diagrammer i visningen"))
      }
      cur <- min(cursor(), nrow(vl))
      tags$div(
        class = "list-group list-group-flush small",
        lapply(seq_len(nrow(vl)), function(i) {
          icon <- if (isTRUE(vl$signal[i])) "⚠" else "✓"
          tags$a(
            href = "#",
            class = paste(
              "list-group-item list-group-item-action py-1 px-2",
              if (identical(i, cur)) "active" else ""
            ),
            onclick = sprintf(
              "Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
              session$ns("goto_diagram"), vl$diagram_id[i]
            ),
            sprintf("%s %s · %s", icon, vl$indikator_navn[i], vl$org_navn[i])
          )
        })
      )
    })

    # Populér filter-valg ved start
    observeEvent(index(),
      {
        ch <- index_filter_choices(index())
        for (dim in names(ch)) {
          updateSelectizeInput(session, paste0("f_", dim),
            choices = c("(alle)" = "", ch[[dim]]), server = FALSE
          )
        }
      },
      once = TRUE
    )

    current_filters <- reactive(list(
      overafdeling = input$f_overafdeling, afsnit = input$f_afsnit,
      datapakke = input$f_datapakke, datasaet = input$f_datasaet,
      indikator_navn = input$f_indikator_navn
    ))

    window_n <- reactive(
      if (identical(input$window_mode, "latest")) as.integer(input$window_n) else NULL
    )

    # Cache-nøgle af en vindue-værdi. Bruges med scanned_n() efter scan, så et
    # senere vindue-skifte (uden re-scan) ikke gør cache-opslaget stale (C1).
    .wkey <- function(n) if (is.null(n)) "all" else as.character(n)

    # Ryd forældede dags-cache-filer én gang pr. session (aldrig blokerende)
    safe_operation("cache-prune", slice_cache_prune(), fallback = NULL)

    # Prefill parquet-mappen fra sidste session, så brugeren ikke skal taste
    # stien hver gang (gemmes ved scan; læses også af startup-kompaktering).
    prev_dir <- last_parquet_dir_read()
    if (!is.null(prev_dir)) {
      updateTextInput(session, "parquet_dir", value = prev_dir)
    }

    # --- Progressivt scan ------------------------------------------------
    # Diagrammer grupperes pr. indikator (ét parquet-load pr. indikator via
    # dags-disk-cachen) og processeres én gruppe ad gangen i en later-kæde.
    # Mellem grupperne når Shiny at flushe → signal-listen vokser løbende,
    # og brugeren kan begynde gennemgangen mens resten scanner.
    # scan_gen er en generations-guard: et nyt scan (eller Stop) bumper den,
    # så ventende callbacks fra et gammelt scan opdager de er stale og dør.
    scan_gen <- reactiveVal(0L)
    scan_running <- reactiveVal(FALSE)
    scan_progress <- reactiveVal(NULL) # list(done, total, found, n_cand)
    scan_ctx <- NULL # env: cand, groups, gi, wn, wk, vdf, base, force, sig

    .scan_finish <- function(gen) {
      if (!identical(gen, isolate(scan_gen()))) {
        return(invisible())
      }
      scan_running(FALSE)
      showNotification(
        sprintf(
          "%d af %d diagrammer har signal",
          sum(isolate(scanned_list())$signal, na.rm = TRUE), nrow(scan_ctx$cand)
        ),
        session = session
      )
    }

    .scan_process_group <- function(gen) {
      if (!identical(gen, isolate(scan_gen()))) {
        return(invisible())
      } # stale
      ctx <- scan_ctx
      if (ctx$gi > length(ctx$groups)) {
        return(.scan_finish(gen))
      }
      idxs <- ctx$groups[[ctx$gi]]
      ind <- ctx$cand$indikator_navn_teknisk[idxs[1]]
      cc <- isolate(cache())
      # Slice-loader deles af gruppens diagrammer: loader højst ÉN gang (kun
      # hvis mindst ét diagram er session-cache-miss). Kæden er RDS-cache →
      # _compact-spejl → rå parquet-lager, og friskhed styres hele vejen af
      # kildens fingeraftryk: uændrede indikatorer rammer cache/spejl (også
      # på tværs af dage), intradag-regenererede læses automatisk forfra.
      # force springer begge cache-lag over og læser råt.
      slice_env <- new.env(parent = emptyenv())
      slice_env$loaded <- FALSE
      get_slice <- function() {
        if (!slice_env$loaded) {
          src <- parquet_indicator_path(ctx$base, ind)
          fp <- source_fingerprint(src)
          slice_env$val <- load_indicator_slice_cached(
            ctx$base, ind,
            loader = function() {
              parquet_load_indicator_best(
                ctx$base, ind,
                force = ctx$force,
                manifest = ctx$manifest, src = src, fp = fp
              )
            },
            force = ctx$force, key = fp
          )
          slice_env$loaded <- TRUE
        }
        slice_env$val
      }
      for (i in idxs) {
        row <- as.list(ctx$cand[i, ])
        key <- paste0(row$diagram_id, "|", ctx$wk)
        res <- cc[[key]]
        if (is.null(res)) {
          # Medians er forhåndshentet for HELE scannet i ét kald (ctx$meds).
          # Fallback til per-diagram-opslag hvis batch-hentningen fejlede, så
          # et DB-hikke degraderer i stedet for at tømme alle knæk.
          meds <- ctx$meds[[as.character(row$diagram_id)]]
          if (is.null(meds)) meds <- db$diagram_medians(row$diagram_id)
          res <- scan_diagram(row, ctx$base, meds, ctx$vdf,
            window_n = ctx$wn,
            slice_loader = get_slice,
            period = row$periode_aggregering,
            org_struct = ctx$agg_os, agg_flags = ctx$agg_fl
          )
          res$row <- row
          cc[[key]] <- res
        }
        ctx$sig[i] <- isTRUE(res$signal)
        ctx$status[i] <- res$status %||% NA_character_
      }
      cache(cc)
      ctx$gi <- ctx$gi + 1L
      # Opdater den scannede liste løbende i cand-rækkefølge. Grupper
      # processeres i cand-rækkefølge, så nye rækker tilføjes altid EFTER de
      # viste → cursor peger stadig på samme diagram mens listen vokser.
      # Alle ok-scannede optages (signal-flag som kolonne). ingen_data/fejl
      # holdes ude - de har ingen tegnbar graf (design 2026-08-11).
      ok_idx <- which(ctx$status %in% "ok")
      sl <- ctx$cand[ok_idx, , drop = FALSE]
      sl$signal <- ctx$sig[ok_idx] %in% TRUE
      sl$status <- ctx$status[ok_idx]
      scanned_list(sl)
      scan_progress(list(
        done = ctx$gi - 1L, total = length(ctx$groups),
        found = sum(ctx$sig, na.rm = TRUE),
        n_cand = nrow(ctx$cand),
        ingen_data = sum(ctx$status == "ingen_data", na.rm = TRUE),
        fejl = sum(ctx$status == "fejl", na.rm = TRUE)
      ))
      if (ctx$gi > length(ctx$groups)) {
        .scan_finish(gen)
      } else {
        next_tick_session(session, function() .scan_process_group(gen))
      }
    }

    observeEvent(input$scan, {
      base <- input$parquet_dir
      if (is.null(base) || !nzchar(base) || !dir.exists(base)) {
        showNotification("Angiv en eksisterende parquet-mappe", type = "warning")
        return()
      }
      cand <- apply_index_filters(index(), current_filters())
      if (nrow(cand) == 0) {
        showNotification("Ingen diagrammer matcher filtrene")
        return()
      }
      last_parquet_dir_write(base) # husk stien til næste session/startup-modal
      wn <- window_n()
      # Gruppér pr. indikator i first-appearance-rækkefølge (cand er sorteret
      # efter indikator_navn → grupper er sammenhængende blokke).
      tekn <- cand$indikator_navn_teknisk
      tekn[is.na(tekn)] <- ""
      groups <- unname(split(seq_len(nrow(cand)), factor(tekn, levels = unique(tekn))))
      gen <- scan_gen() + 1L
      scan_gen(gen)
      # Hent ALLE median-knæk i ét kald frem for ét pr. diagram: ved ~600
      # diagrammer sparer det ~600 round-trips til Supabase. NULL ved fejl →
      # scan-loopet falder tilbage til per-diagram-opslag.
      all_meds <- safe_operation("batch-hent medians",
        medians_by_diagram(
          db$diagram_medians_batch(cand$diagram_id),
          cand$diagram_id
        ),
        fallback = NULL
      )
      # Org-træ + aggregerings-flag til hierarki-oprulning: hentes ÉN gang pr.
      # scan (ikke pr. diagram/gruppe). NULL ved fejl slår oprulning fra for
      # scannet (scannet overlever, blot uden fallback for aggregat-diagrammer).
      # Samme snapshot-pr-scan-mønster som meds/manifest ovenfor: flag-
      # ændringer i DB midt i en session opdages først ved NÆSTE scan — et
      # bevidst valg (konsistens inden for ét scan vejer tungere end friskhed;
      # en gruppe diagrammer der deler org-træ skal alle se samme udgave af
      # det, ellers kan to scans af samme session give modstridende resultater).
      agg_os <- safe_operation("hent org-træ", db$org_struct(), fallback = NULL)
      agg_fl <- safe_operation("hent aggregerings-flag",
        db$aggregation_flags(),
        fallback = NULL
      )
      agg_ctx(list(os = agg_os, fl = agg_fl))
      scan_ctx <<- list2env(
        list(
          cand = cand, groups = groups, gi = 1L,
          wn = wn, wk = .wkey(wn), vdf = variants(), base = base,
          force = isTRUE(input$force_refresh), meds = all_meds %||% list(),
          # Manifest læses ÉN gang pr. scan (ellers én JSON-læsning pr. indikator)
          manifest = safe_operation("laes kompakt-manifest",
            compact_manifest_read(base),
            fallback = NULL
          ),
          agg_os = agg_os, agg_fl = agg_fl,
          sig = rep(NA, nrow(cand)),
          status = rep(NA_character_, nrow(cand))
        ),
        envir = new.env(parent = emptyenv())
      )
      empty <- cand[0, , drop = FALSE]
      empty$signal <- logical(0)
      empty$status <- character(0)
      scanned_list(empty)
      scanned_n(wn) # vindue låst til denne scan (bruges af .scan_of_current/Task 7)
      cursor(1L)
      preview_parts(NULL)
      scan_running(TRUE)
      scan_progress(list(
        done = 0L, total = length(groups), found = 0L,
        n_cand = nrow(cand), ingen_data = 0L, fejl = 0L
      ))
      # Første gruppe synkront (øjeblikkelig feedback); resten via later-kæden
      .scan_process_group(gen)
    })

    observeEvent(input$stop_scan, {
      if (!isTRUE(scan_running())) {
        return()
      }
      scan_gen(scan_gen() + 1L) # invalidér ventende callbacks (stale-guard)
      scan_running(FALSE)
      showNotification("Scan stoppet — fundne signaler kan gennemgås")
    })

    current_diagram <- reactive({
      vl <- view_list()
      if (is.null(vl) || nrow(vl) == 0) {
        return(NULL)
      }
      as.list(vl[min(cursor(), nrow(vl)), ])
    })

    .scan_of_current <- reactive({
      cd <- current_diagram()
      if (is.null(cd)) {
        return(NULL)
      }
      # Brug vinduet fra SCAN-tidspunktet (scanned_n), ej live window_n — ellers
      # gør et vindue-skifte efter scan cache-opslaget stale → blank graf (C1).
      cache()[[paste0(cd$diagram_id, "|", .wkey(scanned_n()))]]
    })

    observeEvent(input$next_, {
      sl <- view_list()
      if (is.null(sl) || nrow(sl) == 0) {
        return()
      }
      cursor(min(cursor() + 1L, nrow(sl)))
      preview_parts(NULL)
    })
    observeEvent(input$prev, {
      sl <- view_list()
      if (is.null(sl) || nrow(sl) == 0) {
        return()
      }
      cursor(max(cursor() - 1L, 1L))
      preview_parts(NULL)
    })

    output$nav_label <- renderUI({
      sl <- view_list()
      if (is.null(sl)) {
        return(span("Ingen scan endnu", class = "text-muted"))
      }
      if (nrow(sl) == 0) {
        txt <- if (isTRUE(scan_running())) {
          "Scanner — endnu ingen signaler"
        } else {
          "Scannet — 0 diagrammer i visningen"
        }
        return(span(txt, class = "text-muted"))
      }
      cd <- current_diagram()
      suffix <- if (isTRUE(scan_running())) " (scanner…)" else ""
      strong(sprintf(
        "%d/%d%s — %s · %s", cursor(), nrow(sl), suffix,
        cd$indikator_navn, cd$org_navn
      ))
    })

    # "Seneste N" tæller perioder, ej dage. Vis hvilken enhed N har for det
    # aktuelle udsnit, så "N" ikke er tvetydigt.
    output$window_hint <- renderUI({
      if (!identical(input$window_mode, "latest")) {
        return(NULL)
      }
      cand <- apply_index_filters(index(), current_filters())
      periods <- unique(vapply(
        cand$periode_aggregering %||% character(0),
        period_to_en, ""
      ))
      unit <- if (length(periods) == 1L) {
        switch(periods,
          week = "uger",
          month = "måneder",
          quarter = "kvartaler",
          year = "år",
          day = "dage",
          periods
        )
      } else {
        "perioder"
      }
      div(class = "small text-muted", sprintf("N = antal %s", unit))
    })

    output$scan_summary <- renderUI({
      p <- scan_progress()
      if (is.null(p)) {
        return(NULL)
      }
      txt <- if (isTRUE(scan_running())) {
        sprintf(
          "Scanner… %d/%d indikatorer — %d med signal",
          p$done, p$total, p$found
        )
      } else {
        sprintf("%d med signal", p$found)
      }
      # Oversprungne diagrammer skal være SYNLIGE. "ingen_data" kan nu dække
      # flere årsager: (a) center-org uden nogen flagede bidragydere (ingen
      # gyldig oprulning), (b) værdi-only serie der per design ikke må
      # oprulles (sum-af-medianer er statistisk forkert), eller (c) oprulning
      # slået fra for HELE scannet fordi org-træ/flag ikke kunne hentes (se
      # advarslen nedenfor) — uden denne tæller forsvinder de tavst, og
      # scanningen ser mere komplet ud end den er.
      skipped <- (p$ingen_data %||% 0L) + (p$fejl %||% 0L)
      # Delvis/total fetch-fejl på oprulnings-konteksten er usynlig for
      # brugeren medmindre den eksplicit flages her — uden advarslen ser en
      # aggregat-diagram-flok der stille lander i ingen_data ud som "normal"
      # scan-adfærd i stedet for en degraderet DB-hentning.
      ac <- agg_ctx()
      agg_disabled <- !is.null(p) && !is.null(ac) &&
        (is.null(ac$os) || is.null(ac$fl))
      div(
        class = "small text-muted mt-2", txt,
        if (skipped > 0) {
          tagList(br(), sprintf(
            "%d uden data, %d fejlede (ej vurderet)",
            p$ingen_data %||% 0L, p$fejl %||% 0L
          ))
        },
        if (agg_disabled) {
          tagList(br(), "⚠ Oprulning deaktiveret (org-træ/flag kunne ikke hentes)")
        }
      )
    })

    # Cursor-stemplet valg: returnér KUN den valgte dato hvis klikket skete på
    # det diagram der vises NU. ggiraph-input kan ikke nulstilles fra server, så
    # uden dette stempel ville et valg fra forrige diagram læses som gyldigt på
    # det aktuelle → faseskift på FORKERT diagram (korrekthedsfejl, ikke nit).
    valid_selected_date <- reactive({
      sel <- input$chart_selected
      if (is.null(sel) || !nzchar(sel) || !identical(selected_cursor(), cursor())) {
        return(NULL)
      }
      sel
    })

    # Ryd diagrammets cache-nøgler + re-scan det scannede vindue, så grafen
    # opdateres i stedet for at gå blank efter en knæk-ændring (gem/fjern).
    .refresh_diagram <- function(cd) {
      cc <- cache()
      cc[grepl(paste0("^", cd$diagram_id, "\\|"), names(cc))] <- NULL
      meds <- db$diagram_medians(cd$diagram_id)
      base <- input$parquet_dir
      ind <- cd$indikator_navn_teknisk
      # Knæk-ændringer rører kun medians — indikatorens slice er uændret, så
      # re-scan må gerne gå via disk-cachen (fingeraftryk-nøglet: hurtigt,
      # ingen parquet-læs så længe kilden er uændret).
      loader <- function() {
        src <- parquet_indicator_path(base, ind)
        fp <- source_fingerprint(src)
        load_indicator_slice_cached(
          base, ind,
          loader = function() {
            parquet_load_indicator_best(
              base, ind,
              src = src, fp = fp
            )
          },
          key = fp
        )
      }
      # Perioden SKAL med her også — ellers falder netop dette diagram tavst
      # tilbage til uaggregeret efter et gemt/fjernet knæk (grafen ser fin ud,
      # men er en anden serie end scannet).
      # Samme org-træ/flag som selve scannet — ellers går aggregat-diagrammer
      # blanke (ingen_data) igen efter et gemt/fjernet knæk. Defensivt guardet
      # (agg_ctx() er reelt altid sat her, da cd forudsætter et forudgående
      # scan) med %||%, så en fremtidig kaldevej ikke kan fejle på NULL$os.
      ac <- agg_ctx() %||% list(os = NULL, fl = NULL)
      res <- c(
        scan_diagram(as.list(cd), base, meds, variants(),
          window_n = scanned_n(), slice_loader = loader,
          period = cd$periode_aggregering,
          org_struct = ac$os, agg_flags = ac$fl
        ),
        list(row = as.list(cd))
      )
      cc[[paste0(cd$diagram_id, "|", .wkey(scanned_n()))]] <- res
      cache(cc)
      # Synkronisér listen: gem/fjern af knæk kan ændre diagrammets signal-
      # status — uden denne opdatering viser sidebar-listen et forældet ikon,
      # og et løst diagram bliver hængende i visningen (eller omvendt).
      sl <- scanned_list()
      i <- which(sl$diagram_id == cd$diagram_id)
      if (!is.null(sl) && length(i) == 1L) {
        sl$signal[i] <- isTRUE(res$signal)
        sl$status[i] <- res$status %||% sl$status[i]
        scanned_list(sl)
      }
      # Visningens sammensætning kan være ændret → et klik-stempel fra før
      # må aldrig overleve (jf. toggle-guarden).
      selected_cursor(NULL)
    }

    # --- Graf -------------------------------------------------------------
    # Det qic-resultat der VISES lige nu: scan-resultatet, eller preview-
    # genberegningen når et faseskift forhåndsvises. Delt mellem graf og
    # fase-statistik-tabellen, så de aldrig kan vise hver sin beregning.
    display_qic <- reactive({
      sc <- .scan_of_current()
      if (is.null(sc) || is.null(sc$qic_result)) {
        return(NULL)
      }
      pv <- preview_parts()
      if (is.null(pv) || is.null(sc$slice)) {
        return(sc$qic_result)
      }
      # Date-normaliseret via preview_break_parts (ingen rbind af Date på
      # POSIXct). Fejl -> fald tilbage til det scannede resultat.
      safe_operation("preview-genberegning",
        {
          base_meds <- db$diagram_medians(current_diagram()$diagram_id)
          parts <- preview_break_parts(
            current_diagram()$diagram_id, base_meds,
            pv, sc$slice$dato
          )
          compute_signal(sc$slice, parts = parts)$qic_result
        },
        fallback = sc$qic_result
      )
    })

    # Hele render-kroppen er fejl-værnet: en graf der ikke kan bygges
    # (degenereret qic-data, uventede kolonner, ggplot-range-fejl) må ALDRIG
    # vælte sessionen — i dev med options(error/shiny.error = browser) endte
    # en uncaught render-fejl ellers i Browse[1] og "frøs" appen midt i scan.
    output$chart <- ggiraph::renderGirafe({
      qr <- display_qic()
      if (is.null(qr)) {
        return(NULL)
      }
      g <- safe_operation("tegn diagram",
        interactive_run_chart(qr, selected_date = valid_selected_date()),
        fallback = "fejl"
      )
      if (identical(g, "fejl") || is.null(g)) {
        # validate-tekst vises i grafens plads (grå besked, ingen crash)
        validate(need(
          FALSE,
          "Diagrammet kan ikke tegnes (ingen tegnbare datapunkter) — se log for detaljer."
        ))
      }
      g
    })

    # Fase-statistik under grafen — samme tal som PDF-rapporternes SPC-tabel.
    output$phase_stats <- renderTable(
      {
        qr <- display_qic()
        if (is.null(qr)) {
          return(NULL)
        }
        phase_stats_df(qr$summary)
      },
      striped = FALSE,
      spacing = "xs",
      width = "auto",
      na = ""
    )

    # Transparens: en oprullet serie SKAL kunne kendes fra en direkte målt —
    # tallene er en sum af underliggende enheder, ikke egne målinger.
    output$agg_badge <- renderUI({
      sc <- .scan_of_current()
      if (is.null(sc) || !isTRUE(sc$aggregated)) {
        return(NULL)
      }
      div(
        class = "alert alert-info py-1 px-2 small mb-2",
        sprintf(
          "Aggregeret fra %d underliggende enheder",
          sc$n_agg_units %||% 0L
        )
      )
    })

    # Advarsel ved grafen når knæk er udeladt af beregningen — så en ændret
    # periode_aggregering ikke ændrer faserne uden at brugeren opdager det.
    output$break_warning <- renderUI({
      sc <- .scan_of_current()
      n <- sc$n_ignored_breaks %||% 0L
      if (is.null(sc) || n == 0L) {
        return(NULL)
      }
      div(
        class = "alert alert-warning py-1 px-2 small mb-2",
        sprintf(
          paste(
            "%d median-knæk indgår ikke: de blev sat under en anden",
            "aggregering end diagrammet bruger nu. Se tabellen",
            "nedenfor — fjern og gensæt dem for at bruge dem igen."
          ),
          n
        )
      )
    })

    output$selected_label <- renderUI({
      sel <- valid_selected_date() # stale valg fra andet diagram → "klik en obs"
      if (is.null(sel)) {
        return(span("Klik en observation", class = "text-muted"))
      }
      span(sprintf("Valgt: %s", sel), class = "fw-bold")
    })

    # --- Forhåndsvis ------------------------------------------------------
    observeEvent(input$preview, {
      sel <- valid_selected_date()
      sc <- .scan_of_current()
      if (is.null(sel) || is.null(sc) || is.null(sc$slice)) {
        showNotification("Vælg en observation på dette diagram", type = "warning")
        return()
      }
      parts <- resolve_median_breaks(
        current_diagram()$diagram_id,
        data.frame(
          diagram = current_diagram()$diagram_id,
          laas_median = as.Date(sel)
        ), sc$slice$dato
      )
      if (is.null(parts)) {
        showNotification("Kan ikke lave faseskift her (første/ugyldig observation)",
          type = "warning"
        )
        return()
      }
      preview_parts(sel)
    })

    # --- Gem faseskift ----------------------------------------------------
    observeEvent(input$save_break, {
      sel <- valid_selected_date()
      cd <- current_diagram()
      sc <- .scan_of_current()
      if (is.null(sel) || is.null(cd) || is.null(sc$slice)) {
        showNotification("Vælg en observation på dette diagram", type = "warning")
        return()
      }
      # Valider at det er et lovligt knæk (ikke første obs / uden for data)
      parts <- resolve_median_breaks(
        cd$diagram_id,
        data.frame(diagram = cd$diagram_id, laas_median = as.Date(sel)),
        sc$slice$dato
      )
      if (is.null(parts)) {
        showNotification("Kan ikke lave faseskift her (første/ugyldig observation)",
          type = "warning"
        )
        return()
      }
      # Stempl knækket med den aggregering serien blev beregnet på — ej en
      # frisk DB-læsning, som kunne være ændret siden scannet.
      ok <- safe_operation("gem faseskift",
        {
          db$add_median_break(cd$diagram_id, as.Date(sel),
            aggregering = cd$periode_aggregering
          )
          TRUE
        },
        fallback = FALSE
      )
      if (!isTRUE(ok)) {
        showNotification("Fejl ved gem (se log)", type = "error")
        return()
      }
      .refresh_diagram(cd) # invalidér + re-scan → graf viser ny signal-status
      preview_parts(NULL)
      showNotification("Faseskift gemt")
    })

    # --- Eksisterende median-knæk + fjern ---------------------------------
    output$breaks_tbl <- DT::renderDT({
      cd <- current_diagram()
      if (is.null(cd)) {
        return(DT::datatable(data.frame()))
      }
      m <- safe_operation("hent median-knæk", db$diagram_medians(cd$diagram_id),
        fallback = NULL
      )
      if (is.null(m)) {
        return(DT::datatable(data.frame()))
      }
      # Markér knæk der IKKE indgår i beregningen, fordi de blev sat under en
      # anden aggregering. Uden denne kolonne ville faserne ændre sig usynligt
      # efter en periode-ændring.
      if (is.data.frame(m) && nrow(m) > 0 && "aggregering" %in% names(m)) {
        keep_ids <- filter_medians_by_period(m, cd$periode_aggregering)$id
        m$status <- ifelse(m$id %in% keep_ids, "anvendes",
          sprintf("⚠ sat under %s — ignoreres nu", m$aggregering)
        )
      }
      DT::datatable(
        m[, intersect(c("id", "laas_median", "aggregering", "status"), names(m)),
          drop = FALSE
        ],
        rownames = FALSE, selection = "single",
        options = list(dom = "t", paging = FALSE)
      )
    })

    observeEvent(input$delete_break, {
      cd <- current_diagram()
      if (is.null(cd)) {
        return()
      }
      sel <- input$breaks_tbl_rows_selected
      if (is.null(sel)) {
        showNotification("Vælg et knæk først", type = "warning")
        return()
      }
      m <- safe_operation("hent median-knæk", db$diagram_medians(cd$diagram_id),
        fallback = NULL
      )
      if (is.null(m)) {
        showNotification("Fejl ved hent (se log)", type = "error")
        return()
      }
      mid <- m$id[sel]
      ok <- safe_operation("fjern faseskift",
        {
          db$delete_median_break(mid)
          TRUE
        },
        fallback = FALSE
      )
      if (!isTRUE(ok)) {
        showNotification("Fejl ved fjern (se log)", type = "error")
        return()
      }
      .refresh_diagram(cd) # re-scan så grafen opdateres (ej blank) efter fjern
      preview_parts(NULL)
      showNotification("Knæk fjernet")
    })

    # Eksponér til test
    list(
      signal_list = signal_list, scanned_list = scanned_list,
      view_list = view_list, current_diagram = current_diagram,
      cursor = cursor, cache = cache, preview_parts = preview_parts,
      scan_of_current = .scan_of_current, scan_running = scan_running,
      display_qic = display_qic
    )
  })
}
