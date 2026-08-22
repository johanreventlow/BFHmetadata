# Serverejet validering og resultatkontrakt for excelR-celleevents.

.excel_adapter_dependency <- function() {
  htmltools::htmlDependency(
    name = "bfh-excel-adapter",
    version = "0.1.0",
    src = c(file = app_sys("www")),
    script = "bfh-excel-adapter.js",
    stylesheet = "bfh-excel-adapter.css"
  )
}

excel_adapter_enabled <- function(cfg) isTRUE(cfg$excel_adapter)

validate_excel_adapter_map <- function(map, col_names, pk) {
  required <- c("column_index", "field", "value_type", "editable")
  if (!is.data.frame(map) || !identical(names(map), required)) {
    stop("Ugyldigt kolonnemap.", call. = FALSE)
  }
  indices <- map$column_index
  if (anyNA(indices) || !is.numeric(indices) || any(!is.finite(indices)) ||
      any(indices != floor(indices)) || any(indices < 0) || anyDuplicated(indices)) {
    stop("Kolonneindeks skal v\u00E6re entydige ikke-negative heltal.", call. = FALSE)
  }
  if (!identical(as.character(map$field), as.character(col_names)) ||
      !identical(as.integer(indices), seq_along(col_names) - 1L)) {
    stop("Kolonnemap matcher ikke data.", call. = FALSE)
  }
  if (!identical(map$field[[1]], pk) || !identical(map$column_index[[1]], 0L)) {
    stop("PK skal v\u00E6re f\u00F8rste kolonne.", call. = FALSE)
  }
  if (!identical(map$editable[match(pk, map$field)], FALSE)) {
    stop("PK skal v\u00E6re ikke-redigerbar.", call. = FALSE)
  }
  if (anyNA(map$value_type) || !all(map$value_type %in% c("text", "int", "fk", "boolean"))) {
    stop("Ugyldig kolonnetype.", call. = FALSE)
  }
  invisible(map)
}

is_scalar_intish <- function(x) {
  length(x) == 1L && !is.na(x) && is.numeric(x) && is.finite(x) && x == floor(x)
}

.excel_adapter_generation <- function(x) {
  if (!is_scalar_intish(x) || x < 0 || x > .Machine$integer.max) return(NULL)
  as.integer(x)
}

canonical_for_browser <- function(x) if (length(x) == 0L || is.na(x)) NA else x

.excel_adapter_rejected <- function(event, message) {
  event_id <- NULL
  generation <- NULL
  if (is.list(event) && is.character(event$event_id) && length(event$event_id) == 1L &&
      !is.na(event$event_id) && nzchar(event$event_id)) {
    event_id <- event$event_id
  }
  if (is.list(event)) {
    generation <- .excel_adapter_generation(event$grid_generation)
  }
  list(ok = FALSE, event_id = event_id, grid_generation = generation, message = message)
}

.excel_adapter_integer <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) return(NULL)
  if (identical(value, "")) return(NA_integer_)
  if (!grepl("^[+-]?(?:0|[1-9][0-9]*)$", value, perl = TRUE)) return(NULL)
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!is.finite(numeric_value) || numeric_value < -.Machine$integer.max ||
      numeric_value > .Machine$integer.max) return(NULL)
  as.integer(numeric_value)
}

.excel_adapter_value <- function(value, value_type) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) return(NULL)
  if (identical(value_type, "text")) {
    return(if (identical(value, "")) NA_character_ else value)
  }
  if (value_type %in% c("int", "fk")) return(.excel_adapter_integer(value))
  if (identical(value_type, "boolean")) {
    if (identical(value, "")) return(NA)
    if (value %in% c("TRUE", "true", "1")) return(TRUE)
    if (value %in% c("FALSE", "false", "0")) return(FALSE)
  }
  NULL
}

prepare_excel_cell_update <- function(event, generation, rows, pk, column_map) {
  validate_excel_adapter_map(column_map, names(rows), pk)
  if (!is.list(event) || !is.character(event$event_id) || length(event$event_id) != 1L ||
      is.na(event$event_id) || !nzchar(event$event_id)) {
    return(.excel_adapter_rejected(event, "Ugyldigt event."))
  }
  event_generation <- .excel_adapter_generation(event$grid_generation)
  server_generation <- .excel_adapter_generation(generation)
  if (is.null(event_generation) || is.null(server_generation) ||
      event_generation != server_generation) {
    return(.excel_adapter_rejected(event, "Ugyldig grid-generation."))
  }
  if (!is.character(event$row_pk) || length(event$row_pk) != 1L || is.na(event$row_pk) ||
      !is_scalar_intish(event$column_index)) {
    return(.excel_adapter_rejected(event, "Ugyldigt celleevent."))
  }
  pk_values <- rows[[pk]]
  if (anyDuplicated(as.character(pk_values))) {
    return(.excel_adapter_rejected(event, "Dubleret PK i data."))
  }
  row_index <- match(event$row_pk, as.character(pk_values))
  map_index <- match(as.integer(event$column_index), column_map$column_index)
  if (is.na(row_index) || is.na(map_index)) {
    return(.excel_adapter_rejected(event, "Ukendt r\u00E6kke eller kolonne."))
  }
  if (!isTRUE(column_map$editable[map_index])) {
    return(.excel_adapter_rejected(event, "Kolonnen kan ikke redigeres."))
  }
  value <- .excel_adapter_value(event$raw_value, column_map$value_type[map_index])
  if (is.null(value)) return(.excel_adapter_rejected(event, "Ugyldig cellev\u00E6rdi."))
  list(
    ok = TRUE,
    event_id = event$event_id,
    grid_generation = event_generation,
    cell_key = paste0(as.character(pk_values[row_index]), ":", as.integer(event$column_index)),
    row_index = as.integer(row_index),
    pk_value = pk_values[row_index],
    field = column_map$field[map_index],
    value = value,
    canonical_value = canonical_for_browser(value),
    message = NULL
  )
}

patch_excel_cell <- function(rows, row_index, field, value) {
  if (!is.data.frame(rows) || !is_scalar_intish(row_index) || row_index < 1L ||
      row_index > nrow(rows) || !is.character(field) || length(field) != 1L ||
      !field %in% names(rows)) {
    stop("Ugyldig celleopdatering.", call. = FALSE)
  }
  column <- rows[[field]]
  if (length(value) != 1L || typeof(value) != typeof(column)) {
    stop("Ugyldig celletype.", call. = FALSE)
  }
  column[as.integer(row_index)] <- value
  if (typeof(column) != typeof(rows[[field]])) stop("Ugyldig celletype.", call. = FALSE)
  rows[[field]] <- column
  rows
}

excel_adapter_result <- function(event, status, value, message = NULL, lock_grid = FALSE) {
  if (!status %in% c("saved", "rejected")) stop("Ugyldig status.", call. = FALSE)
  metadata <- .excel_adapter_rejected(event, "")
  value <- if (identical(value, "")) NA else value
  list(event_id = metadata$event_id, grid_generation = metadata$grid_generation,
       status = status, value = canonical_for_browser(value), message = message,
       lock_grid = isTRUE(lock_grid))
}

send_excel_adapter_result <- function(session, output_id, result) {
  session$sendCustomMessage("bfh-excel-adapter:result",
    c(list(id = session$ns(output_id)), result))
  invisible(NULL)
}

send_excel_adapter_init <- function(session, output_id, generation) {
  generation <- .excel_adapter_generation(generation)
  if (is.null(generation)) stop("Ugyldig generation.", call. = FALSE)
  session$sendCustomMessage("bfh-excel-adapter:init",
    list(id = session$ns(output_id), grid_generation = generation))
  invisible(NULL)
}
