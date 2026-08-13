#' DataTables-options med stabil, faneafgrænset state
#'
#' @param output_id Fuldt namespacet Shiny-output-id.
#' @param options Eksisterende DataTables-options, der skal bevares.
#' @noRd
.dt_session_state_options <- function(output_id, options = list()) {
  stopifnot(
    is.character(output_id), length(output_id) == 1L,
    !is.na(output_id), nzchar(output_id), is.list(options)
  )
  storage_key <- paste0("BFHmetadata:dt-state:", output_id)
  quoted_key <- jsonlite::toJSON(storage_key, auto_unbox = TRUE)
  state_options <- list(
    stateSave = TRUE,
    stateDuration = -1,
    stateSaveCallback = htmlwidgets::JS(sprintf(
      paste0(
        "function(settings, data) {\n",
        "  try {\n",
        "    window.sessionStorage.setItem(%s, JSON.stringify(data));\n",
        "  } catch (error) {}\n",
        "}"
      ),
      quoted_key
    )),
    stateLoadCallback = htmlwidgets::JS(sprintf(
      paste0(
        "function(settings) {\n",
        "  try {\n",
        "    var saved = window.sessionStorage.getItem(%s);\n",
        "    return saved === null ? null : JSON.parse(saved);\n",
        "  } catch (error) {\n",
        "    return null;\n",
        "  }\n",
        "}"
      ),
      quoted_key
    ))
  )

  utils::modifyList(options, state_options)
}
