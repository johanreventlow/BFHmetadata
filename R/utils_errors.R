# Fælles fejlhåndtering. Bevidst IKKE i et modul — deles af crud- og lookup-modulet
# (og kan genbruges af signal-gennemgang og scan-laget).
#
# NB: safe_operation() (R/mod_indikator_crud.R) definerer allerede den fælles
# fejl-fallback-mekanisme og bruges 40+ steder — den røres IKKE her.

#' Oversæt en kendt Postgres-fejl til en dansk brugerbesked. NULL = ukendt.
#'
#' Det er den engelske fejltekst (fra `conditionMessage()`), der reelt gør
#' arbejdet — verificeret mod produktions-DB'ens `lc_messages = en_US.UTF-8`
#' (server- og klient-encoding UTF8). SQLSTATE-koderne ("23503" m.fl.) er
#' bevidst beholdt som fremadrettet robusthed, men er i praksis UOPNÅELIGE
#' i dag: RPostgres' installerede binær (`RPostgres.so`) indeholder ikke
#' strengen "SQLSTATE" nogen steder, så `conditionMessage()` fra en
#' RPostgres-fejl vil aldrig matche dem. De rammes kun, hvis en fremtidig
#' driver-version eller et andet fejl-lag begynder at inkludere SQLSTATE i
#' meddelelsen. Fjern dem roligt, hvis den antagelse ændrer sig.
#' @noRd
pg_besked <- function(msg) {
  if (grepl("23503", msg, fixed = TRUE) ||
      grepl("foreign key", msg, ignore.case = TRUE)) {
    return("Posten er i brug af andre data og kan ikke ændres eller slettes.")
  }
  if (grepl("23505", msg, fixed = TRUE) ||
      grepl("duplicate key", msg, ignore.case = TRUE)) {
    return("Værdien findes allerede — den skal være unik.")
  }
  if (grepl("23502", msg, fixed = TRUE) ||
      grepl("not-null constraint", msg, ignore.case = TRUE)) {
    return("Feltet må ikke være tomt.")
  }
  # \\b-ankeret 08xxx-mønster: uden anker kunne det falsk-matche inde i en
  # vilkårlig 5-tegns delstreng, den dag SQLSTATE rent faktisk optræder.
  if (grepl("could not connect|server closed|connection|\\b08[0-9A-Z]{3}\\b",
            msg, ignore.case = TRUE)) {
    return("Forbindelsen til databasen blev afbrudt — prøv igen.")
  }
  NULL
}

#' Vis "arbejder…" mens code kører. Virker også uden reaktiv kontekst
#' (så rene tests og scripts kan kalde den).
#' @noRd
med_ventevisning <- function(besked, code) {
  har_session <- !is.null(getDefaultReactiveDomain())
  id <- NULL
  if (har_session) {
    id <- showNotification(besked, duration = NULL, closeButton = FALSE,
                           type = "message")
    on.exit(removeNotification(id), add = TRUE)
  }
  force(code)
}
