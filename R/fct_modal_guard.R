# Vagt mod laast sidescroll efter en modal. Bootstrap laaser sidescroll ved at
# saette overflow:hidden paa <body> naar en modal aabnes, og ruller foerst
# laasen tilbage naar modalens hidden.bs.modal fyrer efter fade-out. Fyrer det
# event ikke, staar laasen tilbage, og siden kan ikke scrolles selv om ingen
# modal er synlig — typisk sammen med en efterladt backdrop, der ligger som et
# usynligt klik-skjold over siden.
#
# Rapporteret symptom: efter redigering af en indikator i modalen ("Gem og
# luk") kunne siden ikke scrolles. RODAARSAGEN ER IKKE FASTSLAAET — flere veje
# giver samme slutresultat:
#   - Shiny's showModal() erstatter en aaben modal ved at overskrive
#     wrapperens indhold (renderContentAsync), saa den gamle modals
#     hidden.bs.modal aldrig fyrer. Appen erstatter modaler med vilje i
#     mod_indikator_crud.R (bekraeftelse af aendret indikator-id, diagram-swap,
#     fortryd) og i mod_compact.R, hvor en baggrunds-sweep kan vise sin egen
#     modal midt i brugerens redigering.
#   - En modal-lukning der ikke naar at afslutte sin transition, fx fordi
#     grid'et re-renderes tungt i samme runde (.gem_modal kalder removeModal()
#     og reload() i samme reaktive cyklus).
#
# Vagten haenger derfor ikke alene paa hidden.bs.modal, men ser ogsaa direkte
# paa <body> via en MutationObserver. Den er et SIKKERHEDSNET: den fjerner
# symptomet uanset hvilken vej der fejler, men forklarer det ikke. Findes
# rodaarsagen senere, boer den fixes ved kilden — vagten kan blive staaende som
# vaern, den rydder kun op naar ingen modal er synlig.

#' @noRd
.modal_scroll_guard_dependency <- function() {
  htmltools::htmlDependency(
    name = "bfh-modal-scroll-guard",
    version = "0.2.0",
    src = c(file = app_sys("www")),
    script = "bfh-modal-scroll-guard.js"
  )
}
