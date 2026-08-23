# Rene UI-hjælpere uden reaktiv kontekst → testbare uden Shiny.

#' Forklarende tom-tilstand i stedet for en tom tabel.
#' @noRd
tom_tilstand_ui <- function(ns, has_filters) {
  div(class = "text-center text-muted py-5",
    h5("Ingen indikatorer at vise"),
    p(if (has_filters) "Ingen indikatorer matcher de valgte filtre."
      else "Der er ingen indikatorer i databasen endnu."),
    if (has_filters)
      actionButton(ns("ryd_filtre"), "Ryd filtre", class = "btn-outline-secondary btn-sm"))
}

#' Er mindst ét filter aktivt? Ren funktion → testbar uden Shiny.
#' Alle tre filtre bruger "" (hhv. "alle" for status) som "intet valgt" —
#' nzchar() (og identical(..., "alle")) er derfor det, der reelt skiller
#' "intet valgt" fra "noget valgt". NULL (input ikke initialiseret endnu)
#' må ikke fejle og skal tælle som ikke-sat.
#' @noRd
har_aktive_filtre <- function(datapakke, datasaet, status) {
  nzchar(datapakke %||% "") ||
    nzchar(datasaet %||% "") ||
    !identical(status %||% "alle", "alle")
}

#' Generisk bekræftelsesdialog: modalDialog med Annullér + bekræft-knap.
#' Skriver intet selv — kalderen håndterer selve DB-kaldet i en separat
#' observer bundet til confirm_id. confirm_id skal være namespaced
#' (session$ns("...")), så knappen rammer det rigtige modul-input.
#' warning = valgfri konsekvens-tekst, vist i en gul advarselsboks når angivet.
#' @noRd
build_confirm_modal <- function(title, body, confirm_id, confirm_label,
                                confirm_class = "btn-danger", warning = NULL) {
  modalDialog(
    title = title,
    body,
    if (!is.null(warning) && nzchar(warning))
      div(class = "alert alert-warning", warning),
    footer = tagList(
      modalButton("Annullér"),
      actionButton(confirm_id, confirm_label, class = confirm_class)))
}
