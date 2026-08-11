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
  actionButton(ns("open"), "Tjek og kompaktér parquet-lager",
               class = "btn-sm btn-outline-secondary")
}

#' Server-modul: al interaktion sker via modal og notifikationer (+ knappen
#' ovenfor). Initialiseres EAGER i app_server (skal kunne spørge på
#' landingssiden, før nogen fane er åbnet); koster ingen DB-kald — kun
#' fil-stats mod lageret, og de køres chunket i baggrunden.
#' @noRd
mod_compact_server <- function(id) {
  moduleServer(id, function(input, output, session) {
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
    .sweep_finish <- function(g) {
      if (!identical(g, isolate(gen()))) return(invisible())
      sweeping(FALSE)
      changed <- ctx$items[!is.na(ctx$fps) & ctx$fps != ctx$stored, , drop = FALSE]
      changed$fp <- ctx$fps[!is.na(ctx$fps) & ctx$fps != ctx$stored]
      if (nrow(changed) == 0) {
        if (isTRUE(ctx$manual)) {
          showNotification("Lageret er allerede kompakteret og uændret — intet at gøre",
                           session = session)
        }
        return(invisible())
      }
      ctx$todo <- changed
      first_time <- length(compact_manifest_entries(ctx$manifest)) == 0
      showModal(modalDialog(
        title = "Kompaktér parquet-lageret?",
        p(sprintf("%d af %d indikatorer har nye eller ændrede data siden sidste kompaktering.",
                  nrow(changed), nrow(ctx$items)),
          if (first_time) "(Lageret er aldrig kompakteret før.)" else NULL),
        p("Kompaktering samler hver ændret indikators mange dagsfiler i én fil",
          "i det delte", tags$code("_compact/"), "-spejl. Uændrede indikatorer røres ikke."),
        p(class = "text-muted small",
          "Kører i baggrunden — appen kan bruges imens. Kan afbrydes."),
        footer = tagList(
          actionButton(session$ns("skip"), "Ikke nu"),
          actionButton(session$ns("go"),
                       sprintf("Kompaktér %d ændrede", nrow(changed)),
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

    .start_sweep <- function(base, manual = FALSE) {
      items <- safe_operation("enumerér lager", compact_list_indicators(base),
                              fallback = NULL)
      if (is.null(items) || nrow(items) == 0) {
        if (manual) showNotification("Ingen indikatorer fundet i lageret",
                                     type = "warning", session = session)
        return(invisible())
      }
      manifest <- compact_manifest_read(base)
      entries <- compact_manifest_entries(manifest)
      # isolate: .start_sweep kaldes også fra startup-ticken (uden reaktiv
      # kontekst) — en bar gen()-læsning kastede dér i produktion
      g <- isolate(gen()) + 1L
      gen(g)
      ctx <<- list2env(list(
        base = base, items = items, manifest = manifest, manual = manual,
        stored = vapply(items$rel, function(r) entries[[r]]$fingerprint %||% "", ""),
        fps = rep(NA_character_, nrow(items)), si = 1L, sweep_chunk = 25L,
        todo = NULL, i = 1L, n_ok = 0L, n_failed = 0L, n_empty = 0L,
        entries = entries), envir = new.env(parent = emptyenv()))
      sweeping(TRUE)
      .sweep_tick(g)
    }

    # Startup: kendt mappe fra sidste session → sweep i baggrunden.
    # Første-gangs-brugere spørges ikke (ingen kendt sti endnu).
    base0 <- last_parquet_dir_read()
    if (!is.null(base0) && dir.exists(base0)) {
      local({
        g0 <- isolate(gen())
        # session-bundet: lukkes sessionen inden ticken fyrer (fx hurtig
        # reload), må gen() ikke røres — den reaktive er destrueret
        next_tick_session(session, function() {
          if (identical(g0, isolate(gen()))) .start_sweep(base0)
        })
      })
    }

    # Manuel knap (landingssiden): sweep on demand — dækker intradag-
    # workflows ("jeg har lige regenereret data → kompaktér nu").
    observeEvent(input$open, {
      base <- last_parquet_dir_read()
      if (is.null(base) || !dir.exists(base)) {
        showNotification(
          "Ingen kendt parquet-mappe endnu — kør et scan under Signal-gennemgang først",
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
          "Kompaktering slut, men manifestet kunne ikke skrives (skriveadgang?) — de nye spejl-filer tages ikke i brug",
          type = "error", session = session)
      } else if (ctx$n_ok == 0L && ctx$n_failed > 0L) {
        showNotification(
          "Kompaktering fejlede for alle indikatorer — har du skriveadgang til lager-mappen?",
          type = "error", session = session)
      } else {
        showNotification(sprintf(
          "Lager kompakteret: %d ændrede indikatorer (%d fejlede, %d tomme, %d uændrede sprunget over)",
          ctx$n_ok, ctx$n_failed, ctx$n_empty,
          nrow(ctx$items) - nrow(ctx$todo)), session = session)
      }
    }

    .tick <- function(g) {
      if (!identical(g, isolate(gen()))) return(invisible())  # afbrudt/nyt run
      if (ctx$i > nrow(ctx$todo)) return(.finish(g))
      it <- ctx$todo[ctx$i, , drop = FALSE]
      res <- safe_operation(paste("kompaktér", it$rel),
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
        sprintf("Kompakterer lager… %d/%d", ctx$i - 1L, nrow(ctx$todo)),
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
        "Kompaktering afbrudt — allerede kompakterede indikatorer tages først i brug ved næste fulde kørsel",
        session = session)
    })

    # Eksponér til test
    list(asked = asked, running = running, sweeping = sweeping, result = result)
  })
}
