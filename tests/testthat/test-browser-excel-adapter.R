skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")
skip_on_cran()

wait_for_browser <- function(app, script, timeout = 5000) {
  deadline <- Sys.time() + timeout / 1000
  repeat {
    value <- app$get_js(script)
    if (isTRUE(value)) return(invisible(TRUE))
    if (Sys.time() >= deadline) {
      return(testthat::fail(paste("Browserbetingelsen blev ikke opfyldt:", script)))
    }
    Sys.sleep(0.05)
  }
}

press_browser_key <- function(app, key, code = key, modifiers = 0L) {
  session <- app$get_chromote_session()
  virtual_key <- switch(key, Enter = 13L, Tab = 9L, Escape = 27L,
                        ArrowLeft = 37L, ArrowUp = 38L, ArrowRight = 39L,
                        ArrowDown = 40L,
                        if (nchar(key) == 1L) utf8ToInt(toupper(key)) else 0L)
  session$Input$dispatchKeyEvent(type = "keyDown", key = key, code = code,
                                 windowsVirtualKeyCode = virtual_key,
                                 modifiers = modifiers)
  session$Input$dispatchKeyEvent(type = "keyUp", key = key, code = code,
                                 windowsVirtualKeyCode = virtual_key,
                                 modifiers = modifiers)
  invisible(NULL)
}

start_adapter_app <- function() {
  app <- shinytest2::AppDriver$new("apps/excel-adapter", load_timeout = 15000)
  wait_for_browser(app,
    "!!(document.getElementById('grid') && document.getElementById('grid').excel && document.getElementById('grid').dataset.bfhGeneration === '17')",
    timeout = 15000)
  app
}

browser_output <- function(app, id) {
  app$get_js(sprintf("document.getElementById('%s').textContent.trim()", id))
}

wait_for_event_count <- function(app, count, timeout = 5000) {
  wait_for_browser(app, sprintf(
    "document.getElementById('event_count').textContent.trim() === '%d'", count),
    timeout = timeout)
}

selected_cell <- function(app) {
  app$get_js(
    "JSON.stringify(document.getElementById('grid').excel.selectedCell.map(Number))")
}

expect_no_browser_console_errors <- function(app) {
  logs <- app$get_logs()
  errors <- logs[logs$location == "JS" & logs$level == "error", , drop = FALSE]
  expect_equal(nrow(errors), 0L,
               info = if (nrow(errors)) paste(errors$message, collapse = "\n") else NULL)
}

open_browser_cell_editor <- function(app, x, y, value) {
  app$run_js(sprintf(
    paste0(
      "(() => { const c = document.querySelector('#grid td[data-x=\"%d\"][data-y=\"%d\"]'); ",
      "['mousedown', 'mouseup', 'click', 'mousedown', 'mouseup', 'click', 'dblclick'].",
      "forEach(type => c.dispatchEvent(new MouseEvent(type, { bubbles: true, button: 0 }))); })()"
    ),
    x, y
  ))
  app$run_js("document.activeElement.select()")
  app$get_chromote_session()$Input$insertText(text = value)
  invisible(NULL)
}

edit_browser_cell <- function(app, x, y, value, key = "Enter", code = key,
                              modifiers = 0L) {
  open_browser_cell_editor(app, x, y, value)
  press_browser_key(app, key, code, modifiers)
  invisible(NULL)
}

test_that("adapter sender en celle n\u00F8jagtigt en gang og patcher uden re-render", {
  app <- start_adapter_app()
  withr::defer(app$stop())

  container_identity <- app$get_js(
    "window.fixtureGrid = document.getElementById('grid'); fixtureGrid.id")
  edit_browser_cell(app, 1L, 0L, "Browserv\u00E6rdi")

  expect_identical(app$get_js(
    "document.querySelector('#grid td[data-x=\"1\"][data-y=\"0\"]').textContent"),
    "Browserv\u00E6rdi")
  expect_true(app$get_js(
    "document.querySelector('#grid td[data-x=\"1\"][data-y=\"0\"]').classList.contains('bfh-cell-pending')"))
  wait_for_browser(app, "document.getElementById('event_count').textContent.trim() === '1'")
  expect_identical(app$get_js(
    "document.getElementById('write_count').textContent.trim()"), "1")
  wait_for_browser(app,
    "document.querySelector('#grid td[data-x=\"1\"][data-y=\"0\"]').classList.contains('bfh-cell-saved')")
  expect_true(app$get_js("window.fixtureGrid === document.getElementById('grid')"))
  expect_identical(container_identity, "grid")
  expect_identical(selected_cell(app),
    "[1,1,1,1]")
  wait_for_browser(app,
    "!document.querySelector('#grid td[data-x=\"1\"][data-y=\"0\"]').classList.contains('bfh-cell-saved')",
    timeout = 2000)
  expect_no_browser_console_errors(app)
})

test_that("adapter bevarer tastatur og sender normaliseret selection separat", {
  app <- start_adapter_app()
  withr::defer(app$stop())

  edit_browser_cell(app, 1L, 2L, "TAB", key = "Tab")
  expect_identical(selected_cell(app), "[2,2,2,2]")
  edit_browser_cell(app, 2L, 2L, "321", key = "Tab", modifiers = 8L)
  expect_identical(selected_cell(app), "[1,2,1,2]")
  press_browser_key(app, "ArrowRight")
  expect_identical(selected_cell(app), "[2,2,2,2]")
  press_browser_key(app, "ArrowDown")
  expect_identical(selected_cell(app), "[2,3,2,3]")
  press_browser_key(app, "ArrowLeft")
  press_browser_key(app, "ArrowUp")
  expect_identical(selected_cell(app), "[1,2,1,2]")

  wait_for_event_count(app, 2L)
  events_before_escape <- browser_output(app, "event_count")
  edit_browser_cell(app, 1L, 4L, "SKAL IKKE GEMMES", key = "Escape")
  expect_identical(app$get_js(
    "document.querySelector('#grid td[data-x=\"1\"][data-y=\"4\"]').textContent"),
    "R\u00E6kke 05")
  expect_identical(browser_output(app, "event_count"), events_before_escape)

  app$run_js(paste0(
    "const start = document.querySelector('#grid td[data-x=\"3\"][data-y=\"4\"]');",
    "const end = document.querySelector('#grid td[data-x=\"1\"][data-y=\"2\"]');",
    "start.dispatchEvent(new MouseEvent('mousedown', { bubbles: true, button: 0, buttons: 1 }));",
    "end.dispatchEvent(new MouseEvent('mouseover', { bubbles: true, button: 0, buttons: 1 }));",
    "end.dispatchEvent(new MouseEvent('mouseup', { bubbles: true, button: 0 }));"
  ))
  wait_for_browser(app,
    "document.getElementById('latest_selection').textContent.includes('\\\"top\\\":2')")
  selection <- jsonlite::fromJSON(browser_output(app, "latest_selection"))
  expect_identical(unname(unlist(selection$boundaries)), c(2L, 4L, 1L, 3L))
  expect_identical(as.integer(selection$row_pks), 3:5)
  expect_false("data" %in% names(selection))
  expect_false("fullData" %in% names(selection))
  expect_no_browser_console_errors(app)
})

test_that("adapter holder samtidige celler entydige og bevarer checkbox autocomplete og copy", {
  app <- start_adapter_app()
  withr::defer(app$stop())

  edit_browser_cell(app, 1L, 0L, "En")
  edit_browser_cell(app, 1L, 1L, "To")
  edit_browser_cell(app, 2L, 2L, "333")
  expect_identical(app$get_js(
    "document.querySelectorAll('#grid td.bfh-cell-pending').length"), 3L)
  wait_for_event_count(app, 3L)
  expect_identical(browser_output(app, "write_count"), "3")

  app$run_js(
    "document.querySelector('#grid td[data-x=\"3\"][data-y=\"4\"] input').click()")
  edit_browser_cell(app, 1L, 5L, "Efter checkbox")
  wait_for_event_count(app, 5L)
  event_log <- jsonlite::fromJSON(browser_output(app, "event_log"),
                                  simplifyVector = FALSE)
  last_two <- event_log[4:5]
  expect_false(identical(last_two[[1]]$event_id, last_two[[2]]$event_id))
  expect_identical(names(last_two[[2]]),
                   c("event_id", "grid_generation", "row_pk", "column_index",
                     "raw_value"))
  expect_identical(last_two[[1]]$column_index, 3L)
  expect_type(last_two[[1]]$raw_value, "logical")
  expect_identical(last_two[[2]]$column_index, 1L)

  app$run_js(paste0(
    "const c = document.querySelector('#grid td[data-x=\"4\"][data-y=\"0\"]');",
    "['mousedown','mouseup','click','mousedown','mouseup','click','dblclick'].",
    "forEach(type => c.dispatchEvent(new MouseEvent(type, { bubbles: true, button: 0 })));",
    "c.querySelector('.jdropdown-header').select();"
  ))
  app$get_chromote_session()$Input$insertText(text = "Valg 600")
  press_browser_key(app, "0", "Digit0")
  wait_for_browser(app,
    "Array.from(document.querySelectorAll('#grid .jdropdown-item')).filter(x => x.style.display !== 'none').length === 2",
    timeout = 3000)
  expect_identical(app$get_js(
    "document.querySelectorAll('#grid .jdropdown-item').length"), 650L)
  app$run_js(
    paste0(
      "const item = Array.from(document.querySelectorAll('#grid .jdropdown-item')).",
      "find(x => x.textContent.trim() === 'Valg 600');",
      "item.dispatchEvent(new MouseEvent('mouseup', { bubbles: true, button: 0 }));"
    ))
  wait_for_event_count(app, 6L)
  expect_identical(app$get_js(
    "document.querySelector('#grid td[data-x=\"4\"][data-y=\"0\"]').textContent"),
    "Valg 600")

  app$run_js(
    "document.getElementById('grid').excel.updateSelectionFromCoords(1, 0, 2, 1)")
  writes_before_copy <- browser_output(app, "write_count")
  press_browser_key(app, "c", "KeyC", modifiers = 2L)
  expect_identical(app$get_js(
    "document.getElementById('grid').excel.textarea.value"),
    "En\t10\nTo\t20")
  expect_identical(browser_output(app, "write_count"), writes_before_copy)
  expect_no_browser_console_errors(app)
})

test_that("adapter afviser stale svar og patcher efter sort uden scroll eller DOM-tab", {
  app <- start_adapter_app()
  withr::defer(app$stop())

  edit_browser_cell(app, 1L, 0L, "SLOW")
  edit_browser_cell(app, 1L, 0L, "LATEST")
  wait_for_event_count(app, 2L)
  wait_for_browser(app,
    "document.querySelector('#grid td[data-x=\"1\"][data-y=\"0\"]').textContent === 'SERVER-LATEST'",
    timeout = 2500)
  Sys.sleep(0.7)
  expect_identical(app$get_js(
    "document.querySelector('#grid td[data-x=\"1\"][data-y=\"0\"]').textContent"),
    "SERVER-LATEST")

  app$run_js(paste0(
    "const h = document.querySelector('#grid thead td[data-x=\"2\"]');",
    "h.dispatchEvent(new MouseEvent('mousedown', { bubbles: true, button: 0, buttons: 1 }));",
    "h.dispatchEvent(new MouseEvent('dblclick', { bubbles: true, button: 0 }));",
    "h.dispatchEvent(new MouseEvent('dblclick', { bubbles: true, button: 0 }));"
  ))
  wait_for_browser(app,
    "String(document.getElementById('grid').excel.getValueFromCoords(0, 0)) !== '1'")
  sorted_pk <- app$get_js(
    "String(document.getElementById('grid').excel.getValueFromCoords(0, 0))")
  app$run_js("window.sortedGrid = document.getElementById('grid')")
  edit_browser_cell(app, 1L, 0L, "SORTERET")
  app$run_js(paste0(
    "const s = sortedGrid.querySelector('.jexcel_content'); s.scrollTop = 180; s.scrollLeft = 420;",
    "window.sortedScroll = [s.scrollTop, s.scrollLeft];"
  ))
  wait_for_event_count(app, 3L)
  latest <- jsonlite::fromJSON(browser_output(app, "latest_event"))
  expect_identical(as.character(latest$row_pk), sorted_pk)
  wait_for_browser(app,
    "document.querySelector('#grid td[data-x=\"1\"][data-y=\"0\"]').classList.contains('bfh-cell-saved')")
  expect_true(app$get_js("window.sortedGrid === document.getElementById('grid')"))
  expect_true(app$get_js(paste0(
    "const s = document.querySelector('#grid .jexcel_content');",
    "s.scrollTop === window.sortedScroll[0] && s.scrollLeft === window.sortedScroll[1]"
  )))

  app$run_js(
    "document.querySelector('#grid thead td[data-x=\"2\"]').dispatchEvent(new MouseEvent('dblclick', { bubbles: true, button: 0 }))")
  wait_for_browser(app,
    "String(document.getElementById('grid').excel.getValueFromCoords(0, 0)) === '1'")
  neighbor_before <- app$get_js(
    "document.querySelector('#grid td[data-x=\"2\"][data-y=\"0\"]').textContent")
  edit_browser_cell(app, 1L, 0L, "AFVIS")
  app$run_js(paste0(
    "const s = document.querySelector('#grid .jexcel_content'); s.scrollTop = 150; s.scrollLeft = 360;",
    "window.rejectScroll = [s.scrollTop, s.scrollLeft];"
  ))
  wait_for_event_count(app, 4L)
  wait_for_browser(app,
    "document.querySelector('#grid td[data-x=\"1\"][data-y=\"0\"]').classList.contains('bfh-cell-rejected')")
  expect_identical(app$get_js(
    "document.querySelector('#grid td[data-x=\"1\"][data-y=\"0\"]').textContent"),
    "R\u00E6kke 01")
  expect_match(app$get_js(
    "document.querySelector('#grid td[data-x=\"1\"][data-y=\"0\"]').title"),
    "afvist")
  expect_identical(app$get_js(
    "document.querySelector('#grid td[data-x=\"2\"][data-y=\"0\"]').textContent"),
    neighbor_before)
  expect_true(app$get_js(paste0(
    "const s = document.querySelector('#grid .jexcel_content');",
    "s.scrollTop === window.rejectScroll[0] && s.scrollLeft === window.rejectScroll[1]"
  )))

  edit_browser_cell(app, 1L, 0L, "SLOW")
  edit_browser_cell(app, 1L, 0L, "LAAS")
  wait_for_event_count(app, 6L)
  wait_for_browser(app,
    "document.querySelector('.bfh-excel-grid').classList.contains('bfh-grid-locked')")
  Sys.sleep(0.7)
  expect_true(app$get_js(
    "document.querySelector('.bfh-excel-grid').classList.contains('bfh-grid-locked')"))
  expect_false(app$get_js("document.getElementById('grid').excel.options.editable"))
  expect_no_browser_console_errors(app)
})

test_that("jspreadsheet 3.9.1 paste sender en selvst\u00E6ndig event per \u00E6ndret celle", {
  app <- start_adapter_app()
  withr::defer(app$stop())

  app$run_js(paste0(
    "const c = document.querySelector('#grid td[data-x=\"1\"][data-y=\"10\"]');",
    "c.dispatchEvent(new MouseEvent('mousedown', { bubbles: true, button: 0, buttons: 1 }));",
    "c.dispatchEvent(new MouseEvent('mouseup', { bubbles: true, button: 0 }));",
    "const transfer = new DataTransfer(); transfer.setData('text/plain', 'P11\\t111\\nP21\\t222');",
    "document.dispatchEvent(new ClipboardEvent('paste', { bubbles: true, cancelable: true, clipboardData: transfer }));"
  ))
  wait_for_event_count(app, 4L)
  events <- jsonlite::fromJSON(browser_output(app, "event_log"),
                               simplifyVector = FALSE)
  expect_length(events, 4L)
  expect_length(unique(vapply(events, `[[`, "", "event_id")), 4L)
  expect_identical(vapply(events, function(x) as.integer(x$column_index), 0L),
                   c(1L, 2L, 1L, 2L))
  expect_identical(vapply(events, function(x) as.character(x$row_pk), ""),
                   c("11", "11", "12", "12"))
  expect_identical(browser_output(app, "write_count"), "4")
  expect_identical(app$get_js(
    "document.getElementById('grid').excel.getValueFromCoords(1, 10)"), "P11")
  expect_identical(app$get_js(
    "String(document.getElementById('grid').excel.getValueFromCoords(2, 11))"), "222")
  expect_no_browser_console_errors(app)
})

test_that("adapter l\u00E5ser uden offline-k\u00F8 n\u00E5r Shiny-forbindelsen mangler", {
  app <- start_adapter_app()
  withr::defer(app$stop())

  open_browser_cell_editor(app, 1L, 0L, "OFFLINE")
  app$run_js("Shiny.shinyapp.$socket.close()")
  wait_for_browser(app,
    "!Shiny.shinyapp.$socket || Shiny.shinyapp.$socket.readyState !== 1")
  press_browser_key(app, "Enter")
  expect_true(app$get_js(
    "document.querySelector('.bfh-excel-grid').classList.contains('bfh-grid-locked')"))
  expect_false(app$get_js("document.getElementById('grid').excel.options.editable"))
  expect_match(app$get_js(
    "document.querySelector('#grid td[data-x=\"1\"][data-y=\"0\"]').title"),
    "afbrudt")
  expect_identical(browser_output(app, "event_count"), "0")
  expect_no_browser_console_errors(app)
})
