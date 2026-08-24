# Kompaktering af parquet-lageret, styret af kilde-fingeraftryk. Driftmiljøet
# er en almindelig laptop uden scheduled tasks, så kompaktering udføres af
# brugeren via appen: ved opstart (og via manuel knap på landingssiden) køres
# en billig baggrunds-sweep der sammenligner hver indikators fingeraftryk med
# manifestet — og KUN hvis noget er nyt/ændret, tilbydes kompaktering af
# netop de indikatorer. Uændrede (fx SundK) røres aldrig, og deres
# spejl-entries forbliver gyldige på tværs af dage.
#
# Kørslen er chunket (én indikator pr. tick, samme mønster som progressivt
# scan), så appen kan bruges imens, og kan afbrydes. Manifestet skrives
# SIDST med merge af uændrede entries — et afbrudt forløb efterlader det
# gamle manifest intakt (gamle entries stadig gyldige, nye tages ikke i brug).

#' Manuel kompaktér-knap (placeres på landingssiden). id skal matche
#' mod_compact_server-instansens id.
#' @noRd
mod_compact_btn_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("open"), "Tjek og kompakt\u00E9r parquet-lager",
               class = "btn-sm btn-outline-secondary")
}

#' Server-modul: al interaktion sker via modal og notifikationer (+ knappen
#' ovenfor). Startup-sweepen er DOVEN via lazy_module (selected_tab) — den
#' ville ellers konkurrere med andre faners lazy-init om samme later-kø, og
#' på et stort lokalt lager (mange hundrede indikator-mapper) kunne sweepen
#' optage køen i minutter og blokere fx Indikatorer/Diagrammer, hvis brugeren
#' navigerede direkte dertil uden at ramme "Start" først.
#' @noRd
mod_compact_server <- function(
    id, selected_tab = reactive(NULL),
    arrow_available = function() requireNamespace("arrow", quietly = TRUE)) {
  moduleServer(id, function(input, output, session) {
    capability <- reactiveVal(signal_data_capability("", arrow_available()))
    asked <- reactiveVal(FALSE)      # blev modalen vist?
    running <- reactiveVal(FALSE)    # kompaktering i gang?
    sweeping <- reactiveVal(FALSE)   # sweep i gang?
    result <- reactiveVal(NULL)
    gen <- reactiveVal(0L)
    ctx <- NULL
    prog_id <- "compact_progress"

    # --- Fase 1: sweep (fingeraftryk pr. indikator, chunket) --------------
    # Kører i bidder af sweep_chunk indikatorer pr. tick (~9 ms/stk målt),
    # så app-start aldrig blokeres. Resultat: ændrede/nye indikatorer.
    sweep_id <- "compact_sweep"

    .sweep_finish <- function(g) {
      if (!identical(g, isolate(gen()))) return(invisible())
      removeNotification(sweep_id, session = session)
      sweeping(FALSE)
      changed <- ctx$items[!is.na(ctx$fps) & ctx$fps != ctx$stored, , drop = FALSE]
      changed$fp <- ctx$fps[!is.na(ctx$fps) & ctx$fps != ctx$stored]
      if (nrow(changed) == 0) {
        if (isTRUE(ctx$manual)) {
          showNotification("Lageret er allerede kompakteret og u\u00E6ndret \u2014 intet at g\u00F8re",
                           session = session)
        }
        return(invisible())
      }
      ctx$todo <- changed
      first_time <- length(compact_manifest_entries(ctx$manifest)) == 0
      showModal(modalDialog(
        title = "Kompakt\u00E9r parquet-lageret?",
        p(sprintf("%d af %d indikatorer har nye eller \u00E6ndrede data siden sidste kompaktering.",
                  nrow(changed), nrow(ctx$items)),
          if (first_time) "(Lageret er aldrig kompakteret f\u00F8r.)" else NULL),
        p("Kompaktering samler hver \u00E6ndret indikators mange dagsfiler i \u00E9n fil",
          "i det delte", tags$code("_compact/"), "-spejl. U\u00E6ndrede indikatorer r\u00F8res ikke."),
        p(class = "text-muted small",
          "K\u00F8rer i baggrunden \u2014 appen kan bruges imens. Kan afbrydes."),
        footer = tagList(
          actionButton(session$ns("skip"), "Ikke nu"),
          actionButton(session$ns("go"),
                       sprintf("Kompakt\u00E9r %d \u00E6ndrede", nrow(changed)),
                       class = "btn-primary")),
        easyClose = TRUE),
        session = session)   # eksplicit: kaldes fra tick uden default-domain
      asked(TRUE)
    }

    .sweep_tick <- function(g) {
      if (!identical(g, isolate(gen()))) return(invisible())
      n <- nrow(ctx$items)
      to <- min(ctx$si + ctx$sweep_chunk - 1L, n)
      for (j in ctx$si:to) {
        ctx$fps[j] <- safe_operation(paste("fingeraftryk", ctx$items$rel[j]),
          source_fingerprint(ctx$items$src[j]), fallback = NA_character_)
      }
      ctx$si <- to + 1L
      if (ctx$si > n) .sweep_finish(g) else {
        next_tick_session(session, function() .sweep_tick(g))
      }
    }

    # --- Fase 0b: enumerér indikator-mapper (chunket) --------------------
    # Samme granularitet som capability-scannet: én topmappe (inkl. dens
    # evt. undermapper) pr. tick. compact_list_indicators' dyre skridt er
    # list.files() PR. kandidat-mappe (is_indicator_dir) — ikke selve
    # list.dirs-opremsningen, som allerede skete billigt i .enum_init.
    .enum_tick <- function(g) {
      if (!identical(g, isolate(gen()))) return(invisible())
      done <- .compact_list_indicators_step(ctx$enum_ctx, 1L)
      if (!done) {
        next_tick_session(session, function() .enum_tick(g))
        return(invisible())
      }
      items <- data.frame(rel = ctx$enum_ctx$rel, src = ctx$enum_ctx$src,
                          stringsAsFactors = FALSE)
      if (nrow(items) == 0) {
        removeNotification(sweep_id, session = session)
        sweeping(FALSE)
        if (isTRUE(ctx$manual)) {
          showNotification("Ingen indikatorer fundet i lageret",
                           type = "warning", session = session)
        }
        return(invisible())
      }
      manifest <- compact_manifest_read(ctx$base)
      entries <- compact_manifest_entries(manifest)
      ctx$items <- items
      ctx$manifest <- manifest
      ctx$entries <- entries
      ctx$stored <- vapply(items$rel, function(r) entries[[r]]$fingerprint %||% "", "")
      ctx$fps <- rep(NA_character_, nrow(items))
      ctx$si <- 1L
      .sweep_tick(g)
    }

    # --- Fase 0a: capability-tjek (chunket) ---------------------------------
    # parquet_indicator_dirs' dyre skridt er parquet_indicator_dir_has_data
    # PR. kandidat-mappe — samme is_indicator_dir-agtige list.files()-kald
    # der gør fase 0b langsom. Chunk = 1 topmappe (inkl. dens ét-niveau-ned
    # undermapper) pr. tick, så en enkelt stor topmappe (fx 822 undermapper)
    # stadig deles over mange ticks i stedet for at blokere i ét hug.
    .cap_tick <- function(g, base, manual) {
      if (!identical(g, isolate(gen()))) return(invisible())
      cctx <- ctx$cap_ctx
      if (is.null(cctx)) {
        # base_path ugyldig/mangler — samme "ingen_data"-kontrakt som før
        capability(list(
          state = "ingen_data",
          message = "Ingen lokal parquet-mappe er valgt. Database-CRUD virker fortsat."
        ))
        removeNotification(sweep_id, session = session)
        sweeping(FALSE)
        return(invisible())
      }
      done <- .parquet_indicator_dirs_step(cctx, 1L)
      if (!done) {
        next_tick_session(session, function() .cap_tick(g, base, manual))
        return(invisible())
      }
      cap <- if (length(cctx$found) == 0L) {
        list(state = "ingen_data",
             message = "Mappen indeholder ingen lokale parquet-data. Database-CRUD virker fortsat.")
      } else if (!isTRUE(arrow_available())) {
        list(state = "arrow_mangler",
             message = paste("Signal-gennemgang kræver R-pakken 'arrow'.",
                             "Database-CRUD virker fortsat."))
      } else {
        list(state = "klar", message = "Lokale signaldata er klar til scanning.")
      }
      capability(cap)
      if (!identical(cap$state, "klar")) {
        removeNotification(sweep_id, session = session)
        sweeping(FALSE)
        if (isTRUE(manual)) {
          showNotification(cap$message, type = "warning", session = session)
        }
        return(invisible())
      }
      ctx$enum_ctx <- .compact_list_indicators_init(base)
      .enum_tick(g)
    }

    .start_sweep <- function(base, manual = FALSE) {
      # isolate: .start_sweep kaldes også fra startup-ticken (uden reaktiv
      # kontekst) — en bar gen()-læsning kastede dér i produktion
      g <- isolate(gen()) + 1L
      gen(g)
      ctx <<- list2env(list(
        base = base, manual = manual, items = NULL, manifest = NULL,
        entries = NULL, stored = NULL, fps = NULL, si = 1L, sweep_chunk = 25L,
        cap_ctx = .parquet_indicator_dirs_init(base), enum_ctx = NULL,
        todo = NULL, i = 1L, n_ok = 0L, n_failed = 0L, n_empty = 0L
      ), envir = new.env(parent = emptyenv()))
      sweeping(TRUE)
      # Synlighed: sweepen (capability-tjek + enumerering + fingeraftryk) kan
      # tage 10-20 s på fuldt lager, og R er optaget i bidder imens — uden
      # denne ÉNE gennemgående besked føles klik "døde" uden forklaring, og
      # flere separate notifikationer for hver fase ville virke usammenhængende.
      showNotification("Tjekker parquet-lager for ændringer…",
                       id = sweep_id, duration = NULL, session = session)
      .cap_tick(g, base, manual)
    }

    # Startup: kendt mappe fra sidste session → sweep i baggrunden, men først
    # når "Start"-fanen faktisk vises (lazy_module) — ikke ved app_server-
    # init. Ellers konkurrerer denne tick om later-køen med lazy-init af
    # hvilken som helst anden fane brugeren navigerer direkte til (se
    # modul-docstring). Første-gangs-brugere spørges ikke (ingen kendt sti).
    base0 <- last_parquet_dir_read()
    if (!is.null(base0) && dir.exists(base0)) {
      lazy_module("start", selected_tab, function() {
        g0 <- isolate(gen())
        # session-bundet: lukkes sessionen inden ticken fyrer (fx hurtig
        # reload), må gen() ikke røres — den reaktive er destrueret
        next_tick_session(session, function() {
          if (identical(g0, isolate(gen()))) .start_sweep(base0)
        })
      }, session = session)
    }

    # Manuel knap (landingssiden): sweep on demand — dækker intradag-
    # workflows ("jeg har lige regenereret data → kompaktér nu").
    observeEvent(input$open, {
      base <- last_parquet_dir_read()
      if (is.null(base) || !dir.exists(base)) {
        showNotification(
          "Ingen kendt parquet-mappe endnu \u2014 k\u00F8r et scan under Signal-gennemgang f\u00F8rst",
          type = "warning", session = session)
        return()
      }
      .start_sweep(base, manual = TRUE)
    })

    # --- Fase 2: kompaktér de ændrede (én indikator pr. tick) -------------
    .finish <- function(g) {
      if (!identical(g, isolate(gen()))) return(invisible())
      running(FALSE)
      removeNotification(prog_id, session = session)
      ok <- safe_operation("skriv kompakt-manifest", {
        compact_manifest_write(ctx$base, ctx$entries,
                               n_ok = ctx$n_ok, n_failed = ctx$n_failed)
        TRUE
      }, fallback = FALSE)
      result(list(n_ok = ctx$n_ok, n_failed = ctx$n_failed,
                  n_empty = ctx$n_empty,
                  n_skipped = nrow(ctx$items) - nrow(ctx$todo)))
      if (!isTRUE(ok)) {
        showNotification(
          "Kompaktering slut, men manifestet kunne ikke skrives (skriveadgang?) \u2014 de nye spejl-filer tages ikke i brug",
          type = "error", session = session)
      } else if (ctx$n_ok == 0L && ctx$n_failed > 0L) {
        showNotification(
          "Kompaktering fejlede for alle indikatorer \u2014 har du skriveadgang til lager-mappen?",
          type = "error", session = session)
      } else {
        showNotification(sprintf(
          "Lager kompakteret: %d \u00E6ndrede indikatorer (%d fejlede, %d tomme, %d u\u00E6ndrede sprunget over)",
          ctx$n_ok, ctx$n_failed, ctx$n_empty,
          nrow(ctx$items) - nrow(ctx$todo)), session = session)
      }
    }

    .tick <- function(g) {
      if (!identical(g, isolate(gen()))) return(invisible())  # afbrudt/nyt run
      if (ctx$i > nrow(ctx$todo)) return(.finish(g))
      it <- ctx$todo[ctx$i, , drop = FALSE]
      res <- safe_operation(paste("kompakt\u00E9r", it$rel),
        compact_indicator(it$src, compact_dest_path(ctx$base, it$rel)),
        fallback = list(status = "fejl"))
      if (res$status == "ok") {
        ctx$n_ok <- ctx$n_ok + 1L
        ctx$entries[[it$rel]] <- list(
          fingerprint = it$fp,
          compacted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      } else if (res$status == "tom") {
        ctx$n_empty <- ctx$n_empty + 1L
        ctx$entries[[it$rel]] <- NULL
      } else {
        ctx$n_failed <- ctx$n_failed + 1L
        ctx$entries[[it$rel]] <- NULL   # fejlet → ingen entry → læses råt
      }
      ctx$i <- ctx$i + 1L
      showNotification(
        sprintf("Kompakterer lager\u2026 %d/%d", ctx$i - 1L, nrow(ctx$todo)),
        id = prog_id, duration = NULL, session = session,
        action = actionLink(session$ns("cancel"), "Afbryd"))
      if (ctx$i > nrow(ctx$todo)) .finish(g) else {
        next_tick_session(session, function() .tick(g))
      }
    }

    observeEvent(input$go, {
      removeModal()
      if (is.null(ctx$todo) || nrow(ctx$todo) == 0) return()
      g <- gen() + 1L
      gen(g)
      running(TRUE)
      .tick(g)   # første indikator synkront; resten via later-kæden
    })

    observeEvent(input$skip, removeModal())

    observeEvent(input$cancel, {
      if (!isTRUE(running())) return()
      gen(gen() + 1L)   # stale-guard: ventende ticks dør
      running(FALSE)
      removeNotification(prog_id, session = session)
      showNotification(
        "Kompaktering afbrudt \u2014 allerede kompakterede indikatorer tages f\u00F8rst i brug ved n\u00E6ste fulde k\u00F8rsel",
        session = session)
    })

    # Eksponér til test
    list(
      asked = asked, running = running, sweeping = sweeping, result = result,
      capability = capability
    )
  })
}
