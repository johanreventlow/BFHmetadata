library(shiny)

adapter_dependency <- function() {
  www <- normalizePath(file.path("..", "..", "..", "..", "inst", "www"),
                       mustWork = TRUE)
  htmltools::htmlDependency(
    name = "bfh-excel-adapter-fixture",
    version = "0.1.0",
    src = c(file = www),
    script = "bfh-excel-adapter.js",
    stylesheet = "bfh-excel-adapter.css"
  )
}

option_values <- sprintf("Valg %03d", seq_len(650L))
fixture_rows <- data.frame(
  Id = seq_len(45L),
  Tekst = sprintf("R\u00E6kke %02d", seq_len(45L)),
  Tal = seq_len(45L) * 10L,
  Aktiv = rep(c(TRUE, FALSE), length.out = 45L),
  Valg = option_values[seq_len(45L)],
  Bred_tekst_1 = paste("Lang tekst", seq_len(45L), strrep("x", 35L)),
  Bred_tekst_2 = paste("Mere tekst", seq_len(45L), strrep("y", 35L)),
  Bred_tekst_3 = paste("Sidste tekst", seq_len(45L), strrep("z", 35L)),
  stringsAsFactors = FALSE
)

fixture_columns <- data.frame(
  title = c("Id", "Tekst", "Tal", "Aktiv", "Valg",
            "Bred tekst 1", "Bred tekst 2", "Bred tekst 3"),
  width = c(60L, 180L, 90L, 80L, 180L, 280L, 280L, 280L),
  type = c("hidden", "text", "numeric", "checkbox", "autocomplete",
           "text", "text", "text"),
  readOnly = c(TRUE, rep(FALSE, 7L)),
  source = I(list(NA, NA, NA, NA, option_values, NA, NA, NA)),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  adapter_dependency(),
  tags$h3("Excel-adapter browserfixture"),
  tags$div(
    class = "bfh-excel-grid",
    `data-bfh-adapter` = "true",
    excelR::excelOutput("grid", width = "100%", height = "auto")
  ),
  tags$dl(
    tags$dt("Events"), tags$dd(textOutput("event_count", inline = TRUE)),
    tags$dt("Fake writes"), tags$dd(textOutput("write_count", inline = TRUE)),
    tags$dt("Selections"), tags$dd(textOutput("selection_count", inline = TRUE)),
    tags$dt("Latest"), tags$dd(verbatimTextOutput("latest_event")),
    tags$dt("Event log"), tags$dd(verbatimTextOutput("event_log")),
    tags$dt("Latest selection"), tags$dd(verbatimTextOutput("latest_selection")),
    tags$dt("Client status"), tags$dd(textOutput("client_status", inline = TRUE))
  )
)

server <- function(input, output, session) {
  generation <- 17L
  event_count <- reactiveVal(0L)
  write_count <- reactiveVal(0L)
  selection_count <- reactiveVal(0L)
  latest_event <- reactiveVal(NULL)
  event_log <- reactiveVal(list())
  latest_selection <- reactiveVal(NULL)
  client_status <- reactiveVal("")

  output$grid <- excelR::renderExcel({
    excelR::excelTable(
      data = fixture_rows,
      columns = fixture_columns,
      autoColTypes = FALSE,
      autoWidth = FALSE,
      tableOverflow = TRUE,
      tableHeight = "330px",
      tableWidth = "760px",
      pagination = 0L,
      selectionCopy = TRUE,
      getSelectedData = TRUE,
      allowInsertRow = FALSE,
      allowInsertColumn = FALSE,
      allowDeleteRow = FALSE,
      allowDeleteColumn = FALSE,
      allowRenameColumn = FALSE,
      columnSorting = TRUE,
      rowDrag = FALSE,
      columnDrag = FALSE
    )
  })

  session$onFlushed(function() {
    session$sendCustomMessage("bfh-excel-adapter:init",
                              list(id = "grid", grid_generation = generation))
  }, once = TRUE)

  observeEvent(input$grid_cell, {
    event <- input$grid_cell
    event_count(event_count() + 1L)
    write_count(write_count() + 1L)
    latest_event(event)
    event_log(c(event_log(), list(event)))

    delay <- if (identical(event$raw_value, "SLOW")) 0.9 else 0.35
    later::later(function() {
      value <- event$raw_value
      status <- "saved"
      message <- NULL
      lock_grid <- FALSE
      if (identical(event$raw_value, "AFVIS")) {
        status <- "rejected"
        value <- "R\u00E6kke 01"
        message <- "V\u00E6rdien blev afvist af fake-serveren."
      } else if (identical(event$raw_value, "SLOW")) {
        value <- "SERVER-STALE"
      } else if (identical(event$raw_value, "LATEST")) {
        value <- "SERVER-LATEST"
      } else if (identical(event$raw_value, "LAAS")) {
        status <- "rejected"
        value <- "R\u00E6kke 01"
        message <- "Gridet er l\u00E5st af fake-serveren."
        lock_grid <- TRUE
      } else if (is.character(value) && length(value) == 1L) {
        value <- trimws(value)
      }
      session$sendCustomMessage("bfh-excel-adapter:result", list(
        id = "grid",
        event_id = event$event_id,
        grid_generation = event$grid_generation,
        row_pk = event$row_pk,
        column_index = event$column_index,
        status = status,
        value = value,
        message = message,
        lock_grid = lock_grid
      ))
    }, delay)
  }, ignoreInit = TRUE)

  observeEvent(input$grid_selection, {
    selection_count(selection_count() + 1L)
    latest_selection(input$grid_selection)
  }, ignoreInit = TRUE)

  observeEvent(input$grid_client_status, {
    message <- input$grid_client_status$message
    client_status(if (is.null(message)) "" else message)
  }, ignoreInit = TRUE)

  output$event_count <- renderText(event_count())
  output$write_count <- renderText(write_count())
  output$selection_count <- renderText(selection_count())
  output$latest_event <- renderText(jsonlite::toJSON(latest_event(), auto_unbox = TRUE))
  output$event_log <- renderText(jsonlite::toJSON(event_log(), auto_unbox = TRUE))
  output$latest_selection <- renderText(
    jsonlite::toJSON(latest_selection(), auto_unbox = TRUE)
  )
  output$client_status <- renderText(client_status())
}

shinyApp(ui, server)
