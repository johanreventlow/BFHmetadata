# Lazy-init af moduler. Hvert modul henter referencedata når dets server-
# funktion kører; med ivrig init betaler enhver app-start for ALLE modulers
# opstart-queries, selvom brugeren lander på "Start"-fanen og måske kun skal
# bruge én fane. Her udskydes init til fanen faktisk åbnes første gang.

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
