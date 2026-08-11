# Startup-kompaktering af parquet-lageret. Driftmiljøet er en almindelig
# laptop uden scheduled tasks, så kompakteringen tilbydes i stedet som et
# spørgsmål når appen åbner: kendt lager-mappe + intet frisk manifest →
# modal med "Kompaktér nu / Ikke nu". Kørslen er chunket (én indikator pr.
# tick, samme mønster som det progressive scan), så appen kan bruges imens,
# og kan afbrydes undervejs. Manifestet skrives SIDST — et afbrudt forløb
# efterlader aldrig et spejl der tages i brug (læsere falder blot tilbage
# til det rå lager).

#' Server-only modul: har ingen fane-UI — al interaktion sker via modal og
#' notifikationer. Initialiseres EAGER i app_server (skal kunne spørge på
#' landingssiden, før nogen fane er åbnet); koster ingen DB-kald.
#' @noRd
mod_compact_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    asked <- reactiveVal(FALSE)
    running <- reactiveVal(FALSE)
    result <- reactiveVal(NULL)
    gen <- reactiveVal(0L)
    ctx <- NULL
    prog_id <- "compact_progress"

    # --- Startup-tjek: spørg kun når det er relevant ----------------------
    # Kendt mappe (gemt ved sidste scan), mappen findes, og spejlet er ikke
    # allerede kompakteret i dag. Første-gangs-brugere spørges ikke (ingen
    # kendt sti) — stien gemmes ved deres første scan, så næste start spørger.
    base0 <- last_parquet_dir_read()
    if (!is.null(base0) && dir.exists(base0) &&
        !compact_manifest_fresh(base0)) {
      m <- compact_manifest_read(base0)
      last_txt <- if (is.null(m)) "aldrig" else as.character(m$date)
      showModal(modalDialog(
        title = "Kompaktér parquet-lageret?",
        p("Lageret består af mange små dagsfiler, som gør scanning langsom.",
          "Kompaktering samler hver indikator i én fil i et delt",
          tags$code("_compact/"), "-spejl, som alle brugere får glæde af."),
        p(strong("Sidst kompakteret: "), last_txt),
        textInput(session$ns("dir"), "Parquet-mappe", value = base0),
        p(class = "text-muted small",
          "Kører i baggrunden — appen kan bruges imens. Kan afbrydes."),
        footer = tagList(
          actionButton(session$ns("skip"), "Ikke nu"),
          actionButton(session$ns("go"), "Kompaktér nu", class = "btn-primary")),
        easyClose = TRUE))
      asked(TRUE)
    }

    # --- Chunket kørsel (én indikator pr. tick) ---------------------------
    .finish <- function(g) {
      if (!identical(g, isolate(gen()))) return(invisible())
      running(FALSE)
      removeNotification(prog_id, session = session)
      ok <- safe_operation("skriv kompakt-manifest", {
        compact_manifest_write(ctx$base, n_ok = ctx$n_ok, n_failed = ctx$n_failed)
        TRUE
      }, fallback = FALSE)
      result(list(n_ok = ctx$n_ok, n_failed = ctx$n_failed,
                  n_empty = ctx$n_empty))
      if (!isTRUE(ok)) {
        showNotification(
          "Kompaktering slut, men manifestet kunne ikke skrives (skriveadgang?) — spejlet tages ikke i brug",
          type = "error", session = session)
      } else if (ctx$n_ok == 0L && ctx$n_failed > 0L) {
        showNotification(
          "Kompaktering fejlede for alle indikatorer — har du skriveadgang til lager-mappen?",
          type = "error", session = session)
      } else {
        showNotification(sprintf(
          "Lager kompakteret: %d indikatorer (%d fejlede, %d tomme)",
          ctx$n_ok, ctx$n_failed, ctx$n_empty), session = session)
      }
    }

    .tick <- function(g) {
      if (!identical(g, isolate(gen()))) return(invisible())  # afbrudt/nyt run
      if (ctx$i > nrow(ctx$items)) return(.finish(g))
      it <- ctx$items[ctx$i, , drop = FALSE]
      res <- safe_operation(paste("kompaktér", it$rel),
        compact_indicator(it$src, compact_dest_path(ctx$base, it$rel)),
        fallback = list(status = "fejl"))
      if (res$status == "ok") ctx$n_ok <- ctx$n_ok + 1L
      else if (res$status == "tom") ctx$n_empty <- ctx$n_empty + 1L
      else ctx$n_failed <- ctx$n_failed + 1L
      ctx$i <- ctx$i + 1L
      showNotification(
        sprintf("Kompakterer lager… %d/%d", ctx$i - 1L, nrow(ctx$items)),
        id = prog_id, duration = NULL, session = session,
        action = actionLink(session$ns("cancel"), "Afbryd"))
      if (ctx$i > nrow(ctx$items)) .finish(g) else {
        next_tick(function() .tick(g))
      }
    }

    observeEvent(input$go, {
      base <- input$dir %||% base0
      if (is.null(base) || !nzchar(base) || !dir.exists(base)) {
        showNotification("Angiv en eksisterende parquet-mappe", type = "warning")
        return()
      }
      removeModal()
      items <- compact_list_indicators(base)
      if (nrow(items) == 0) {
        showNotification("Ingen indikatorer fundet i lageret", type = "warning")
        return()
      }
      g <- gen() + 1L
      gen(g)
      ctx <<- list2env(list(base = base, items = items, i = 1L,
                            n_ok = 0L, n_failed = 0L, n_empty = 0L),
                       envir = new.env(parent = emptyenv()))
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
        "Kompaktering afbrudt — spejlet tages først i brug efter en fuld kompaktering",
        session = session)
    })

    # Eksponér til test
    list(asked = asked, running = running, result = result)
  })
}
