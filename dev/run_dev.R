# Lokal udvikling — starter appen + åbner browser på fast port.
# Kør:  Rscript dev/run_dev.R    (eller source() i en R-session)
options(shiny.autoreload = TRUE)
pkgload::load_all(".", reset = TRUE, helpers = FALSE)

# runApp() med eksplicit port + browser så den åbner pålideligt
# (run_app() alene returnerer kun app-objektet og åbner ikke browser via Rscript).
shiny::runApp(
  run_app(),
  host = "127.0.0.1",
  port = 3838,
  launch.browser = TRUE
)
