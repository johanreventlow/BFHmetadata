# Hot-reload udvikling
options(shiny.autoreload = TRUE)
pkgload::load_all(".", reset = TRUE, helpers = FALSE)
run_app()
