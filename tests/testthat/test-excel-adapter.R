adapter_rows <- data.frame(
  Id = c(11L, 12L),
  navn = c("A", "B"),
  niveau = c(1L, 2L),
  stringsAsFactors = FALSE
)

adapter_map <- data.frame(
  column_index = 0:2,
  field = c("Id", "navn", "niveau"),
  value_type = c("int", "text", "int"),
  editable = c(FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

test_that("adapter map kræver entydige 0-baserede kolonner og skjult pk", {
  expect_silent(validate_excel_adapter_map(adapter_map, names(adapter_rows), "Id"))
  expect_error(validate_excel_adapter_map(transform(adapter_map,
    column_index = c(0L, 1L, 1L)), names(adapter_rows), "Id"), "entydig")
  expect_error(validate_excel_adapter_map(transform(adapter_map,
    editable = c(TRUE, TRUE, TRUE)), names(adapter_rows), "Id"), "PK")
  expect_error(validate_excel_adapter_map(transform(adapter_map,
    field = c("navn", "Id", "niveau"), editable = c(TRUE, FALSE, TRUE)),
    c("navn", "Id", "niveau"), "Id"), "første")
})

cell_event <- function(event_id = "1", generation = 7L, row_pk = "11",
                       column_index = 1L, raw_value = "Nyt") {
  list(event_id = event_id, grid_generation = generation, row_pk = row_pk,
       column_index = column_index, raw_value = raw_value)
}

test_that("prepare_excel_cell_update mapper kun serverens indeks til felt", {
  result <- prepare_excel_cell_update(cell_event(), 7L, adapter_rows, "Id", adapter_map)
  expect_identical(result, list(
    ok = TRUE, event_id = "1", grid_generation = 7L, cell_key = "11:1",
    row_index = 1L, pk_value = 11L, field = "navn", value = "Nyt",
    canonical_value = "Nyt", message = NULL
  ))
  event <- cell_event()
  event$field <- "niveau"
  expect_identical(prepare_excel_cell_update(event, 7L, adapter_rows, "Id", adapter_map)$field,
                   "navn")
})

test_that("prepare_excel_cell_update afviser ugyldige events før write", {
  cases <- list(
    cell_event(event_id = ""), cell_event(generation = 6L),
    cell_event(generation = 8L), cell_event(row_pk = "99"),
    cell_event(column_index = 9L), cell_event(column_index = 0L),
    cell_event(generation = 2147483648)
  )
  for (event in cases) {
    result <- prepare_excel_cell_update(event, 7L, adapter_rows, "Id", adapter_map)
    expect_false(result$ok)
    expect_false("field" %in% names(result))
    expect_false("value" %in% names(result))
  }
  old <- prepare_excel_cell_update(cell_event(generation = 6L), 7L,
    adapter_rows, "Id", adapter_map)
  expect_identical(old$event_id, "1")
  expect_identical(old$grid_generation, 6L)
})

test_that("prepare_excel_cell_update afviser ugyldig server-generation", {
  result <- prepare_excel_cell_update(cell_event(), 2147483648,
    adapter_rows, "Id", adapter_map)
  expect_false(result$ok)
  expect_identical(result$grid_generation, 7L)
})

test_that("prepare_excel_cell_update koerceder int fk tomme og boolean strengt", {
  int_map <- transform(adapter_map, value_type = c("int", "int", "fk"))
  seven <- prepare_excel_cell_update(cell_event(column_index = 2L, raw_value = "7"),
    7L, adapter_rows, "Id", int_map)
  expect_identical(seven$value, 7L)
  expect_identical(seven$canonical_value, 7L)
  for (value in c("7.2", "abc")) {
    expect_false(prepare_excel_cell_update(cell_event(column_index = 2L, raw_value = value),
      7L, adapter_rows, "Id", int_map)$ok)
  }
  empty_text <- prepare_excel_cell_update(cell_event(raw_value = ""), 7L,
    adapter_rows, "Id", adapter_map)
  empty_int <- prepare_excel_cell_update(cell_event(column_index = 2L, raw_value = ""),
    7L, adapter_rows, "Id", int_map)
  expect_identical(empty_text$value, NA_character_)
  expect_identical(empty_int$value, NA_integer_)
  bool_map <- transform(adapter_map, value_type = c("int", "boolean", "int"))
  for (value in c("TRUE", "FALSE", "true", "false", "1", "0")) {
    expect_true(prepare_excel_cell_update(cell_event(raw_value = value), 7L,
      adapter_rows, "Id", bool_map)$ok)
  }
  expect_false(prepare_excel_cell_update(cell_event(raw_value = "yes"), 7L,
    adapter_rows, "Id", bool_map)$ok)
  expect_identical(prepare_excel_cell_update(cell_event(raw_value = ""), 7L,
    adapter_rows, "Id", bool_map)$value, NA)
})

test_that("prepare_excel_cell_update koerceder den direkte int-kolonne strengt", {
  expect_identical(prepare_excel_cell_update(cell_event(column_index = 2L, raw_value = "7"),
    7L, adapter_rows, "Id", adapter_map)$value, 7L)
  for (value in c("7.2", "abc", "2147483648")) {
    expect_false(prepare_excel_cell_update(cell_event(column_index = 2L, raw_value = value),
      7L, adapter_rows, "Id", adapter_map)$ok)
  }
  expect_identical(prepare_excel_cell_update(cell_event(column_index = 2L, raw_value = ""),
    7L, adapter_rows, "Id", adapter_map)$value, NA_integer_)
})

test_that("prepare_excel_cell_update afviser dubleret pk fail-closed", {
  duplicate_rows <- rbind(adapter_rows, adapter_rows[1, , drop = FALSE])
  result <- prepare_excel_cell_update(cell_event(), 7L, duplicate_rows, "Id", adapter_map)
  expect_false(result$ok)
  expect_false("field" %in% names(result))
})

test_that("patch_excel_cell ændrer kun den udpegede celle og bevarer typer", {
  patched <- patch_excel_cell(adapter_rows, 2L, "niveau", 7L)
  expect_identical(patched$Id, c(11L, 12L))
  expect_identical(patched$navn, c("A", "B"))
  expect_identical(patched$niveau, c(1L, 7L))
  expect_type(patched$niveau, "integer")
  expect_error(patch_excel_cell(adapter_rows, 2L, "niveau", "7"), "type")
})

test_that("excel_adapter_result og sendere bruger den faste client-kontrakt", {
  result <- excel_adapter_result(cell_event(), "rejected", NA_character_, "Afvist", TRUE)
  expect_identical(names(result), c("event_id", "grid_generation", "status", "value",
    "message", "lock_grid"))
  expect_error(excel_adapter_result(cell_event(), "other", NULL), "status")
  expect_identical(excel_adapter_result(cell_event(), "saved", "")$value, NA)
  expect_identical(excel_adapter_result(cell_event(), "saved", NA_character_)$value, NA)
  expect_identical(excel_adapter_result(cell_event(), "saved", 7L)$value, 7L)
  expect_identical(excel_adapter_result(cell_event(), "saved", TRUE)$value, TRUE)

  sent <- list()
  session <- list(
    ns = function(id) paste0("mod-", id),
    sendCustomMessage = function(type, message) sent[[length(sent) + 1L]] <<- list(type, message)
  )
  send_excel_adapter_result(session, "grid", result)
  expect_identical(sent[[1]], list("bfh-excel-adapter:result",
    c(list(id = "mod-grid"), result)))
  send_excel_adapter_init(session, "grid", 7L)
  expect_identical(sent[[2]], list("bfh-excel-adapter:init",
    list(id = "mod-grid", grid_generation = 7L)))
  expect_error(send_excel_adapter_init(session, "grid", 2147483648), "generation")
})
