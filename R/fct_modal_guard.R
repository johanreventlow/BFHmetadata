# Vagt mod laast sidescroll efter modal-skift. Bootstrap laaser sidescroll ved
# at saette overflow:hidden paa <body> naar en modal aabnes, og gendanner ved
# lukning praecis den vaerdi der stod der da modalen aabnede. Shiny's
# showModal() erstatter en allerede aaben modal ved at overskrive modal-
# wrapperens HTML — den gamle modals hidden.bs.modal fyrer derfor aldrig, og
# laasen rulles aldrig tilbage.
#
# Appen erstatter modaler med vilje flere steder i mod_indikator_crud.R:
# bekraeftelsen af et aendret indikator-id vises oven paa den aabne modal,
# diagram-swap bytter formular, og fortryd genopbygger indikator-modalen. Uden
# vagten er symptomet en side der ikke kan scrolles efter at en indikator er
# redigeret — laasen staar tilbage paa <body>, selv om ingen modal er synlig.
#
# bfh-modal-scroll-guard.js rydder body-tilstanden naar sidste modal er lukket.
# Alternativet — at forbyde modal-erstatning — ville koste swap-flowet og
# fortryd-flowet, som begge er bevidst UX.

#' @noRd
.modal_scroll_guard_dependency <- function() {
  htmltools::htmlDependency(
    name = "bfh-modal-scroll-guard",
    version = "0.1.0",
    src = c(file = app_sys("www")),
    script = "bfh-modal-scroll-guard.js"
  )
}
