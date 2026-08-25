# Flydende reconnect-oplevelse. Naar websocket-forbindelsen til den lokale
# Shiny-proces tabes (dvale, laast maskine, netvaerksskift, timeout), viste
# Shiny et moerkt overlay, og appen var doed indtil manuel genindlaesning.
# bfh-reconnect.js undertrykker overlayet, viser en diskret toast, poller
# serveren og genindlaeser siden naar den svarer igen; sessionStorage husker
# den aktive fane, som serveren genaabner via input$bfh_restore_nav (se
# app_server). Lazy-init af modulerne goer genindlaesningen billig — der
# koeres kun opstart-queries for den fane, brugeren faktisk stod paa.
#
# NB: Supabase-forbindelsen haandteres separat af poolen (fct_db.R,
# validationInterval = 0) — det er websocket'en til Shiny, ikke DB'en, der
# udloeser overlayet.

#' @noRd
.reconnect_dependency <- function() {
  htmltools::htmlDependency(
    name = "bfh-reconnect",
    version = "0.1.0",
    src = c(file = app_sys("www")),
    script = "bfh-reconnect.js",
    stylesheet = "bfh-reconnect.css"
  )
}
