# testServer-tests for mod_diagram med fake-db (closures der logger kald).

selected_option_value <- function(tag) {
  html <- htmltools::renderTags(tag)$html
  option <- regmatches(html, regexpr(
    '<option[^>]* selected(?:="selected")?[^>]*>', html, perl = TRUE))
  sub('.*value="([^"]*)".*', '\\1', option)
}

fake_diagram_db <- function(dup_count = 0L, median_count = 0L) {
  admin <- data.frame(
    diagram_id = c(1L, 2L),
    indikator = c(10L, 11L),
    organisatorisk_navn_teknisk = c(20L, 21L),
    diagram_type = c(1L, 2L),
    periode_aggregering = c("måned", NA),
    indgaar_i_aggregering = c(TRUE, FALSE),
    diagram_aktivt = c(TRUE, FALSE),
    direktionens_tavle = c(FALSE, FALSE),
    indikator_navn = c("Tryksår", "Fald"),
    org_navn = c("Kirurgi", "Medicin"),
    type_navn = c("Seriediagram", "Søjlediagram"),
    datasaet = c("Tryksår-datasæt", "Fald-datasæt"),
    datapakke = c("Kliniske indikatorer", "Kliniske indikatorer"),
    stringsAsFactors = FALSE)
  calls <- list(created = NULL, updated = NULL, deleted = NULL)
  list(
    list_diagrams_admin = function() admin,
    diagram_form_options = function() list(
      indikator = data.frame(
        id = c(10L, 11L),
        label = c("Tryksår", "Inaktiv indikator"),
        aktiv_indikator = c(TRUE, FALSE)),
      org = data.frame(
        id = c(19L, 20L, 21L, 22L),
        label = c("Hospital", "Kirurgi", "Medicin", "Onkologi")),
      type = data.frame(id = c(1L, 2L),
                        label = c("Seriediagram", "Søjlediagram"))),
    # Org-træ: Hospital(19) → Kirurgi(20) + Medicin(21); Medicin → Onkologi(22)
    org_struct = function() data.frame(
      id = c(19L, 20L, 21L, 22L),
      parent_id = c(NA, 19L, 19L, 21L)),
    diagram_periode_choices = function() c("måned", "uge"),
    diagram_duplicate_count = function(indikator, org, type, exclude_id = -1L) {
      dup_count
    },
    diagram_median_count = function(diagram_id) median_count,
    create_diagram = function(values) { calls$created <<- values; 99L },
    update_diagram = function(id, values) {
      calls$updated <<- list(id = id, values = values); 1L
    },
    delete_diagram = function(id) { calls$deleted <<- id; 1L },
    .calls = function() calls
  )
}

large_diagram_form_options <- function() {
  list(
    indikator = data.frame(
      id = seq_len(2000L),
      label = paste("Indikator", seq_len(2000L)),
      stringsAsFactors = FALSE),
    org = data.frame(id = 20L, label = "Kirurgi"),
    type = data.frame(id = 1L, label = "Seriediagram"),
    periode = "måned")
}

diagram_form_html <- function(vals = NULL, lock_indikator = FALSE) {
  htmltools::renderTags(.diagram_form_ui(
    NS("diagram"), vals, large_diagram_form_options(), lock_indikator))$html
}

diagram_selectize_options <- function(html) {
  select <- regmatches(
    html,
    regexpr(
      '<select[^>]*id="diagram-d_indikator"[^>]*>[\\s\\S]*?</select>',
      html,
      perl = TRUE))
  regmatches(select, gregexpr('<option(?:\\s|>)[^>]*>.*?</option>', select, perl = TRUE))[[1]]
}

test_that("indikator-selectize starter med hoejst noedvendige options uden advarsel", {
  expect_no_warning(new_html <- diagram_form_html())
  new_options <- diagram_selectize_options(new_html)
  expect_length(new_options, 1L)
  expect_match(new_options[[1]], 'value=""', fixed = TRUE)

  edit_options <- diagram_selectize_options(diagram_form_html(list(indikator = 2000L)))
  expect_lte(length(edit_options), 2L)
  expect_true(any(grepl('value="2000"', edit_options, fixed = TRUE)))
})

test_that("redigering bevarer den kendte valgte indikator", {
  html <- diagram_form_html(list(indikator = 2000L))
  expect_match(html, "Indikator 2000", fixed = TRUE)
  expect_match(html, 'value="2000"', fixed = TRUE)
})

test_that("redigering viser fallback for ukendt valgt indikator", {
  html <- diagram_form_html(list(indikator = 9999L))
  expect_match(html, "Ukendt indikator #9999", fixed = TRUE)
  expect_match(html, 'value="9999"', fixed = TRUE)
})

test_that("låst indikator bevarer skjult værdi og deaktiveret visning", {
  html <- diagram_form_html(list(indikator = 9999L), lock_indikator = TRUE)
  expect_match(html, 'value="9999"', fixed = TRUE)
  expect_match(html, "Ukendt indikator #9999", fixed = TRUE)
  expect_match(html, "disabled", fixed = TRUE)
})

test_that("laast indikator bevarer en valgt indikator med label vaelg", {
  opts <- large_diagram_form_options()
  opts$indikator$label[[2000L]] <- "(vælg)"
  html <- htmltools::renderTags(.diagram_form_ui(
    NS("diagram"), list(indikator = 2000L), opts, lock_indikator = TRUE))$html

  expect_match(html, 'value="2000"', fixed = TRUE)
  expect_match(html, 'value="(vælg)"', fixed = TRUE)
  expect_length(diagram_selectize_options(html), 1L)
})

capture_diagram_indicator_updates <- function(code) {
  updates <- list()
  testthat::local_mocked_bindings(
    updateSelectizeInput = function(session, inputId, choices = NULL,
                                    selected = NULL, server = FALSE, ...) {
      updates[[length(updates) + 1L]] <<- list(
        inputId = inputId, choices = choices, selected = selected, server = server)
    },
    .package = "BFHmetadata")
  force(code)
  updates
}

test_that("nyt diagram registrerer alle indikatorer til server-side soegning", {
  updates <- capture_diagram_indicator_updates({
    db <- fake_diagram_db()
    testServer(mod_diagram_server, args = list(db = db), {
      session$setInputs(new_diagram = 1)
      session$flushReact()
    })
  })

  expect_length(updates, 1L)
  expect_identical(updates[[1]]$inputId, "d_indikator")
  expect_identical(updates[[1]]$choices,
                   c("Tryksår" = "10", "Inaktiv indikator" = "11"))
  expect_identical(updates[[1]]$selected, "")
  expect_true(updates[[1]]$server)
})

test_that("redigering registrerer indikatorer med den eksisterende selection", {
  updates <- capture_diagram_indicator_updates({
    db <- fake_diagram_db()
    testServer(mod_diagram_server, args = list(db = db), {
      session$setInputs(open_id = 1)
      session$flushReact()
    })
  })

  expect_length(updates, 1L)
  expect_identical(updates[[1]]$inputId, "d_indikator")
  expect_identical(updates[[1]]$choices,
                   c("Tryksår" = "10", "Inaktiv indikator" = "11"))
  expect_identical(updates[[1]]$selected, 10L)
  expect_true(updates[[1]]$server)
})

test_that("redigering bevarer en ukendt indikator i server-side choices", {
  updates <- capture_diagram_indicator_updates({
    db <- fake_diagram_db()
    admin <- db$list_diagrams_admin()
    admin$indikator[admin$diagram_id == 1L] <- 9999L
    db$list_diagrams_admin <- function() admin
    testServer(mod_diagram_server, args = list(db = db), {
      session$setInputs(open_id = 1)
      session$flushReact()
    })
  })

  expect_length(updates, 1L)
  expect_identical(updates[[1]]$choices,
                   c("Tryksår" = "10", "Inaktiv indikator" = "11",
                     "Ukendt indikator #9999" = "9999"))
  expect_identical(updates[[1]]$selected, 9999L)
})

test_that("admin-liste indlæses og status-filter default 'aktive' reducerer", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    expect_equal(nrow(admin()), 2)
    expect_equal(nrow(filtered()), 1)          # kun diagram_aktivt = TRUE
    expect_equal(filtered()$diagram_id, 1L)
    session$setInputs(filter_status = "alle")
    expect_equal(nrow(filtered()), 2)
    session$setInputs(filter_status = "inaktive")
    expect_equal(filtered()$diagram_id, 2L)
  })
})

test_that("diagramoversigten bevarer DT-tilstand i browser-sessionen", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(id = "diagram", db = db), {
    first_widget <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    first_state <- expect_session_dt_state(first_widget, session$ns("tbl"))

    expect_identical(first_widget$x$options$pageLength, 15L)
    expect_true(any(vapply(first_widget$x$options$columnDefs, function(def) {
      identical(def$orderable, FALSE) && identical(def$targets, 0L)
    }, logical(1))))

    reload()
    session$flushReact()
    rerendered_widget <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    rerendered_state <- expect_session_dt_state(
      rerendered_widget, session$ns("tbl"))

    expect_identical(rerendered_state, first_state)
  })
})

test_that("dynamiske diagramfiltre bevarer gyldige valg efter reload", {
  db <- fake_diagram_db()
  admin_source <- new.env(parent = emptyenv())
  admin_source$rows <- db$list_diagrams_admin()
  db$list_diagrams_admin <- function() admin_source$rows
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(
      filter_indikator = "Tryksår",
      filter_org = "20",
      filter_datapakke = "Kliniske indikatorer",
      filter_datasaet = "Tryksår-datasæt")
    reload()
    session$flushReact()

    expect_identical(selected_option_value(output$filter_indikator_ui), "Tryksår")
    expect_identical(selected_option_value(output$filter_org_ui), "20")
    expect_identical(selected_option_value(output$filter_datapakke_ui), "Kliniske indikatorer")
    expect_identical(selected_option_value(output$filter_datasaet_ui), "Tryksår-datasæt")

    admin_without_tryksaar <- admin_source$rows
    admin_without_tryksaar <- admin_without_tryksaar[admin_without_tryksaar$diagram_id != 1L, , drop = FALSE]
    admin_source$rows <- admin_without_tryksaar
    reload()
    session$flushReact()

    expect_identical(selected_option_value(output$filter_indikator_ui), "")
  })
})

test_that("org-filter medtager valgt enhed plus alle underliggende enheder", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(filter_org = "19")   # Hospital → alt derunder
    expect_setequal(filtered()$diagram_id, c(1L, 2L))
    session$setInputs(filter_org = "19", filter_datapakke = "Kliniske indikatorer")
    expect_setequal(filtered()$diagram_id, c(1L, 2L))  # AND med øvrige filtre
    session$setInputs(filter_org = "21", filter_datapakke = "")
    expect_identical(filtered()$diagram_id, 2L)        # Medicin → kun diagram 2
    session$setInputs(filter_org = "")
    expect_setequal(filtered()$diagram_id, c(1L, 2L))  # ryddet → alle
  })
})

test_that("org-filterets dropdown har id-værdier og hierarkisk indrykning", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$flushReact()
    html <- htmltools::renderTags(output$filter_org_ui)$html
    expect_match(html, 'value="19"', fixed = TRUE)
    expect_match(html, 'value="20"', fixed = TRUE)
    expect_match(html, " Kirurgi")   # barn af Hospital → indrykket label
  })
})

test_that("gem ny kalder create_diagram med koercede values", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(new_diagram = 1)
    session$setInputs(d_indikator = "11", d_organisatorisk_navn_teknisk = "21",
                      d_diagram_type = "2", d_periode_aggregering = "uge",
                      d_indgaar_i_aggregering = FALSE, d_diagram_aktivt = TRUE,
                      d_direktionens_tavle = FALSE, d_save = 1)
    created <- db$.calls()$created
    expect_false(is.null(created))
    expect_identical(created$indikator, 11L)
    expect_identical(created$organisatorisk_navn_teknisk, 21L)
    expect_identical(created$diagram_type, 2L)
    expect_identical(created$periode_aggregering, "uge")
    expect_true(created$diagram_aktivt)
    expect_null(db$.calls()$updated)
    expect_match(status_msg(), "Oprettet")
  })
})

test_that("valideringsfejl stopper gem (intet db-kald)", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(new_diagram = 1)
    session$setInputs(d_indikator = "", d_organisatorisk_navn_teknisk = "",
                      d_diagram_type = "", d_periode_aggregering = "",
                      d_indgaar_i_aggregering = FALSE, d_diagram_aktivt = TRUE,
                      d_direktionens_tavle = FALSE, d_save = 1)
    expect_null(db$.calls()$created)
    expect_match(status_msg(), "obligatorisk")
  })
})

test_that("duplikat giver advarsel men gem gennemføres", {
  db <- fake_diagram_db(dup_count = 1L)
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(new_diagram = 1)
    session$setInputs(d_indikator = "10", d_organisatorisk_navn_teknisk = "20",
                      d_diagram_type = "1", d_periode_aggregering = "",
                      d_indgaar_i_aggregering = FALSE, d_diagram_aktivt = TRUE,
                      d_direktionens_tavle = FALSE, d_save = 1)
    expect_match(warn_msg(), "Findes allerede")
    expect_false(is.null(db$.calls()$created))   # blød guard: gem fortsætter
  })
})

test_that("redigér eksisterende kalder update_diagram med id", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(open_id = 1)
    expect_equal(editing_id(), 1L)
    session$setInputs(d_indikator = "10", d_organisatorisk_navn_teknisk = "20",
                      d_diagram_type = "1", d_periode_aggregering = "måned",
                      d_indgaar_i_aggregering = TRUE, d_diagram_aktivt = TRUE,
                      d_direktionens_tavle = FALSE, d_save = 1)
    upd <- db$.calls()$updated
    expect_false(is.null(upd))
    expect_identical(upd$id, 1L)
    expect_null(db$.calls()$created)
  })
})

test_that("slet med median-knæk blokeres (delete_diagram IKKE kaldt)", {
  db <- fake_diagram_db(median_count = 3L)
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(open_id = 1)
    session$setInputs(d_delete = 1)
    expect_null(db$.calls()$deleted)
    expect_match(warn_msg(), "median-knæk")
  })
})

test_that("slet uden median-knæk sletter og genindlæser", {
  db <- fake_diagram_db(median_count = 0L)
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(open_id = 1)
    session$setInputs(d_delete = 1)
    expect_identical(db$.calls()$deleted, 1L)
    expect_match(status_msg(), "Slettet")
  })
})

test_that("filter på datapakke reducerer til matchende rækker", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    expect_equal(nrow(filtered()), 2)
    session$setInputs(filter_datapakke = "Kliniske indikatorer")
    expect_equal(nrow(filtered()), 2)
    session$setInputs(filter_datapakke = "")
    expect_equal(nrow(filtered()), 2)
  })
})

test_that("filter på datasæt reducerer til matchende rækker", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(filter_datasaet = "Fald-datasæt")
    expect_equal(nrow(filtered()), 1)
    expect_equal(filtered()$diagram_id, 2L)
  })
})

test_that("kombination af datapakke + datasæt + status virker sammen", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle",
                      filter_datapakke = "Kliniske indikatorer",
                      filter_datasaet = "Tryksår-datasæt")
    expect_equal(nrow(filtered()), 1)
    expect_equal(filtered()$diagram_id, 1L)
    # Status-filter lægges oveni: diagram 1 er aktivt, så "inaktive" giver 0
    session$setInputs(filter_status = "inaktive")
    expect_equal(nrow(filtered()), 0)
  })
})

test_that("tomt datapakke/datasæt-filter viser alle rækker", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle", filter_datapakke = "",
                      filter_datasaet = "")
    expect_equal(nrow(filtered()), 2)
  })
})

test_that("sætning af (nu ukendt) filter_type-input påvirker ikke filtered()", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    before <- nrow(filtered())
    session$setInputs(filter_type = "X")
    expect_equal(nrow(filtered()), before)
  })
})

test_that("indikator uden hierarki (NA datasaet/datapakke) vises under 'Alle' men udelades ved konkret filter", {
  # Test-lokal fixture-kopi (tredje række med NA-hierarki) frem for at ændre
  # den delte fake_diagram_db(): flere eksisterende tests har hardkodede
  # rækketal (nrow(admin())==2 osv.), som en delt tredje række ville bryde.
  db <- fake_diagram_db()
  admin_na <- db$list_diagrams_admin()
  admin_na <- rbind(admin_na, data.frame(
    diagram_id = 3L, indikator = 12L, organisatorisk_navn_teknisk = 22L,
    diagram_type = 1L, periode_aggregering = NA_character_,
    indgaar_i_aggregering = FALSE, diagram_aktivt = TRUE,
    direktionens_tavle = FALSE, indikator_navn = "Uden hierarki",
    org_navn = "Onkologi", type_navn = "Seriediagram",
    datasaet = NA_character_, datapakke = NA_character_,
    stringsAsFactors = FALSE))
  db$list_diagrams_admin <- function() admin_na

  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    expect_equal(nrow(filtered()), 3)
    expect_true(3L %in% filtered()$diagram_id)   # NA-rækken er med under "Alle"

    session$setInputs(filter_datapakke = "Kliniske indikatorer")
    expect_false(3L %in% filtered()$diagram_id)  # udelades ved konkret datapakke-filter
    session$setInputs(filter_datapakke = "")

    session$setInputs(filter_datasaet = "Fald-datasæt")
    expect_false(3L %in% filtered()$diagram_id)  # udelades ved konkret datasæt-filter
  })
})

test_that(".collect_diagram_form koercer typer og tomme til NA", {
  input <- list(d_indikator = "5", d_organisatorisk_navn_teknisk = "",
                d_diagram_type = "2", d_periode_aggregering = "",
                d_indgaar_i_aggregering = TRUE, d_diagram_aktivt = FALSE,
                d_direktionens_tavle = NULL)
  vals <- .collect_diagram_form(input)
  expect_identical(vals$indikator, 5L)
  expect_identical(vals$organisatorisk_navn_teknisk, NA_integer_)
  expect_identical(vals$diagram_type, 2L)
  expect_identical(vals$periode_aggregering, NA_character_)
  expect_true(vals$indgaar_i_aggregering)
  expect_false(vals$diagram_aktivt)
  expect_false(vals$direktionens_tavle)
  expect_identical(names(vals), DIAGRAM_COLS)
})
