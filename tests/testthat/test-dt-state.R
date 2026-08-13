test_that("breaks-tabellen gemmer ikke DT-tilstand uden brugerfiltrering", {
  db <- list(
    list_active_seriediagrammer = function() data.frame(
      diagram_id = integer(), indikator_id = integer(),
      stringsAsFactors = FALSE
    ),
    org_enhed_variants = function() data.frame(
      org_id = integer(), teknisk = character(), kort = character(),
      langt = character(), fra_data = character(),
      stringsAsFactors = FALSE
    )
  )

  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    widget <- jsonlite::fromJSON(output$breaks_tbl, simplifyVector = FALSE)

    expect_null(widget$x$options$stateSave)
    expect_null(widget$x$options$stateSaveCallback)
    expect_null(widget$x$options$stateLoadCallback)
  })
})
