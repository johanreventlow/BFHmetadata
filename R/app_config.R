#' Sti til app-ressource i inst/app
#' @noRd
app_sys <- function(...) system.file(..., package = "BFHmetadata")

#' Læs golem-config (app-niveau indstillinger)
#' @noRd
get_golem_config <- function(value, config = Sys.getenv("GOLEM_CONFIG_ACTIVE", "default"),
                             use_parent = TRUE) {
  config::get(value = value, config = config,
              file = app_sys("golem-config.yml"), use_parent = use_parent)
}
