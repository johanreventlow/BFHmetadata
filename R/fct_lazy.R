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
