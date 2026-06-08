#' @noRd
app_server <- function(input, output, session) {
  pool <- db_connect()
  onStop(function() pool::poolClose(pool))
  db <- make_db(pool)
  mod_indikator_crud_server("indik", db)
}
