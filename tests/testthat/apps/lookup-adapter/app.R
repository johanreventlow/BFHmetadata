library(shiny)

package_root <- normalizePath(file.path("..", "..", "..", ".."),
                              mustWork = TRUE)
pkgload::load_all(package_root, quiet = TRUE)

lookup_tables <- getFromNamespace("LOOKUP_TABLES", "BFHmetadata")
adapter_cfg <- Find(function(cfg) identical(cfg$id, "faggrupper"), lookup_tables)
legacy_cfg <- Find(function(cfg) identical(cfg$id, "datakilder"), lookup_tables)

stopifnot(isTRUE(adapter_cfg$excel_adapter),
          !isTRUE(legacy_cfg$excel_adapter))

memory_lookup_db <- function(initial_rows, rejected_value = NULL) {
  store <- initial_rows
  write_count <- reactiveVal(0L)
  get_row_count <- reactiveVal(0L)

  list(
    list_rows = function() store,
    get_row = function(pk_val) {
      get_row_count(get_row_count() + 1L)
      store[as.character(store$Id) == as.character(pk_val), , drop = FALSE]
    },
    update_cell = function(pk_val, col, value) {
      write_count(write_count() + 1L)
      if (!is.null(rejected_value) && identical(value, rejected_value)) {
        stop("fixture write rejected")
      }
      row <- which(as.character(store$Id) == as.character(pk_val))
      stopifnot(length(row) == 1L, col %in% names(store))
      store[row, col] <<- value
      1L
    },
    add_row = function() {
      next_id <- max(store$Id) + 1L
      blank <- store[1L, , drop = FALSE]
      blank[1L, ] <- NA
      blank$Id <- next_id
      store <<- rbind(store, blank)
      next_id
    },
    delete_row = function(pk_val) {
      store <<- store[as.character(store$Id) != as.character(pk_val), , drop = FALSE]
      1L
    },
    ref_count = function(pk_val) 0L,
    fk_options = function(col) NULL,
    .store = function() store,
    .write_count = write_count,
    .get_row_count = get_row_count
  )
}

ui <- fluidPage(
  getFromNamespace(".jexcel_theme_css", "BFHmetadata")(),
  getFromNamespace(".excel_adapter_dependency", "BFHmetadata")(),
  tags$h3("Lookup-modul browserfixture"),
  fluidRow(
    column(6, getFromNamespace("mod_lookup_table_ui", "BFHmetadata")(
      "adapter", adapter_cfg
    )),
    column(6, getFromNamespace("mod_lookup_table_ui", "BFHmetadata")(
      "legacy", legacy_cfg
    ))
  ),
  tags$dl(
    tags$dt("Adapter writes"),
    tags$dd(textOutput("adapter_write_count", inline = TRUE)),
    tags$dt("Adapter rereads"),
    tags$dd(textOutput("adapter_get_row_count", inline = TRUE)),
    tags$dt("Adapter DB"),
    tags$dd(textOutput("adapter_db_value", inline = TRUE)),
    tags$dt("Adapter local"),
    tags$dd(textOutput("adapter_local_value", inline = TRUE)),
    tags$dt("Legacy writes"),
    tags$dd(textOutput("legacy_write_count", inline = TRUE)),
    tags$dt("Legacy DB"),
    tags$dd(textOutput("legacy_db_value", inline = TRUE)),
    tags$dt("Legacy local"),
    tags$dd(textOutput("legacy_local_value", inline = TRUE)),
    tags$dt("Legacy cell events"),
    tags$dd(textOutput("legacy_cell_count", inline = TRUE))
  )
)

server <- function(input, output, session) {
  adapter_db <- memory_lookup_db(
    data.frame(Id = 1:2, faggruppe = c("Adapter A", "Adapter B"),
               stringsAsFactors = FALSE),
    rejected_value = "AFVIS"
  )
  legacy_db <- memory_lookup_db(
    data.frame(
      Id = 1:2,
      datakilde_navn = c("Legacy A", "Legacy B"),
      datakilde_beskrivelse = c("Beskrivelse A", "Beskrivelse B"),
      stringsAsFactors = FALSE
    )
  )
  legacy_cell_count <- reactiveVal(0L)

  adapter_state <- getFromNamespace(
    "mod_lookup_table_server", "BFHmetadata"
  )("adapter", adapter_db, adapter_cfg)
  legacy_state <- getFromNamespace(
    "mod_lookup_table_server", "BFHmetadata"
  )("legacy", legacy_db, legacy_cfg)

  observeEvent(input[["legacy-tbl_cell"]], {
    legacy_cell_count(legacy_cell_count() + 1L)
  }, ignoreInit = TRUE)

  output$adapter_write_count <- renderText(adapter_db$.write_count())
  output$adapter_get_row_count <- renderText(adapter_db$.get_row_count())
  output$adapter_db_value <- renderText({
    adapter_db$.write_count()
    adapter_db$.get_row_count()
    adapter_db$.store()$faggruppe[[1L]]
  })
  output$adapter_local_value <- renderText(adapter_state$rows()$faggruppe[[1L]])
  output$legacy_write_count <- renderText(legacy_db$.write_count())
  output$legacy_db_value <- renderText({
    legacy_db$.write_count()
    legacy_db$.store()$datakilde_navn[[1L]]
  })
  output$legacy_local_value <- renderText(
    legacy_state$rows()$datakilde_navn[[1L]]
  )
  output$legacy_cell_count <- renderText(legacy_cell_count())
}

shinyApp(ui, server)
