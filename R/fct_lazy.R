# Lazy-init af moduler. Hvert modul henter referencedata når dets server-
# funktion kører; med ivrig init betaler enhver app-start for ALLE modulers
# opstart-queries, selvom brugeren lander på "Start"-fanen og måske kun skal
# bruge én fane. Her udskydes init til fanen faktisk åbnes første gang.

#' Skedulér fn til afvikling efter næste reactive flush (chunket baggrunds-
#' arbejde: progressivt scan, kompaktering). Injicérbar via option, fordi
#' testServer selv afvikler later-køen under flush — tests kan kun observere
#' mellemtilstande (progressivitet/stop/cancel) med en manuel kø.
#' @noRd
next_tick <- function(fn) {
  sched <- getOption("bfhmeta.scan_scheduler", NULL)
  if (is.null(sched)) later::later(fn, 0) else sched(fn)
}

#' Skedulér en SESSION-BUNDET baggrunds-tick. later-callbacks overlever
#' sessionen: lukkes/genindlæses browseren midt i et scan (eller trigges
#' shiny.autoreload), fyrer de ventende ticks stadig — og deres første
#' reaktive læsning kaster så "Can't access reactive ...; its module session
#' has been destroyed" (uopfanget → Browse[1] i dev, set i produktion).
#' Derfor: (1) rør INTET reaktivt hvis sessionen er lukket — dø stille,
#' (2) fejl i en tick må aldrig nå top-level (logges via safe_operation),
#' (3) fn() køres i isolate(): later-callbacks har INGEN reaktiv kontekst,
#' så en bar reaktiv læsning i tick-koden ville kaste "Operation not
#' allowed without an active reactive context" (set i produktion —
#' testServer maskerer det ved selv at wrappe test-udtryk i isolate).
#' @noRd
next_tick_session <- function(session, fn) {
  next_tick(function() {
    closed <- tryCatch(isTRUE(session$isClosed()), error = function(e) TRUE)
    if (closed) return(invisible())
    safe_operation("baggrunds-tick", shiny::isolate(fn()), fallback = NULL)
  })
}

#' Kør init() første gang `selected()` matcher tab_value.
#'
#' Idempotent: init kører præcis én gang, uanset hvor mange gange brugeren
#' skifter til og fra fanen (modul-server må ikke registreres to gange —
#' det ville give dublerede observers).
#'
#' @param tab_value fanens value i navbar'en
#' @param selected reactive der giver den aktuelt valgte fane
#' @param init 0-args funktion der starter modulet
#' @noRd
lazy_module <- function(tab_value, selected, init) {
  started <- FALSE
  shiny::observe({
    if (started) return()
    if (!identical(selected(), tab_value)) return()
    started <<- TRUE
    init()
  })
}
