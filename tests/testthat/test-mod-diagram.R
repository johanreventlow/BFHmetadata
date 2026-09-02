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
    aggreger_egne_og_boern = c(FALSE, FALSE),
    diagram_aktivt = c(TRUE, FALSE),
    direktionens_tavle = c(FALSE, FALSE),
    maalgruppe = c(1L, NA),
    indikator_navn = c("Tryksår", "Fald"),
    org_navn = c("Kirurgi", "Medicin"),
    type_navn = c("Seriediagram", "Søjlediagram"),
    maalgruppe_navn = c("akut indlagte", NA),
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
        aktiv_indikator = c(TRUE, FALSE),
        datasaet = c("Tryksår-datasæt", "Fald-datasæt")),
      org = data.frame(
        id = c(19L, 20L, 21L, 22L),
        label = c("Hospital", "Kirurgi", "Medicin", "Onkologi")),
      type = data.frame(id = c(1L, 2L),
                        label = c("Seriediagram", "Søjlediagram")),
      maalgruppe = data.frame(
        id = c(1L, 2L, 3L, 4L),
        label = c("akut indlagte", "planlagt indlagte",
                  "akut ambulante", "planlagt ambulante"))),
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

test_that("Målgruppe-felt: valgfri dropdown med '(ingen)' + fk-choices", {
  opts <- fake_diagram_db()$diagram_form_options()
  opts$periode <- c("måned", "uge")
  html <- htmltools::renderTags(.diagram_form_ui(
    NS("diagram"), list(maalgruppe = 2L), opts))$html
  expect_match(html, "Målgruppe", fixed = TRUE)
  expect_match(html, "planlagt indlagte", fixed = TRUE)
  expect_match(html, 'value="2"[^>]* selected', perl = TRUE)
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

# excelR-payload-helpers til diagram-grid'et (samme mønster som hierarki)
diagram_grid_edit <- function(d, id, column, value) {
  g <- diagram_excel_data(d)
  g[[column]][g$diagram_id == id] <- if (is.null(value)) NA else value
  list(
    colHeaders = as.list(names(g)),
    data = lapply(seq_len(nrow(g)), function(i) {
      lapply(g[i, ], function(v) {
        if (length(v) == 1 && is.na(v)) NULL else if (is.logical(v)) v else as.character(v)
      })
    }),
    forSelectedVals = FALSE)
}

diagram_grid_select <- function(row0, pks = c("1", "2")) {
  list(forSelectedVals = TRUE,
       selectedDataBoundary = list(borderTop = row0, borderBottom = row0,
                                   borderLeft = 0, borderRight = 0),
       fullData = list(data = lapply(pks, function(p) list(p))))
}

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

test_that("diagram-grid: skjult pk, dropdowns med autocomplete, checkbokse", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(id = "diagram", db = db), {
    w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    cols <- w$x$columns
    titles <- vapply(cols, function(c) c$title, "")
    types <- vapply(cols, function(c) c$type, "")
    expect_identical(types[titles == "diagram_id"], "hidden")
    expect_identical(types[titles == "Indikator"], "dropdown")
    expect_true(isTRUE(cols[[which(titles == "Indikator")]]$autocomplete))
    expect_identical(types[titles == "Aktiv"], "checkbox")
    expect_true(all(vapply(cols[titles %in% c("Datapakke", "Datasæt")],
                           function(c) isTRUE(c$readOnly), logical(1))))
    expect_false(isTRUE(w$x$autoWidth))
    expect_false(isTRUE(w$x$allowInsertRow))
    expect_true(isTRUE(w$x$columnSorting))     # klik-sortering på overskrifter
  })
})

test_that("inline dropdown-ændring patcher fuld række og kalder update_diagram", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(tbl = diagram_grid_edit(
      isolate(filtered()), 1L, "Type", "2"))
    upd <- db$.calls()$updated
    expect_false(is.null(upd))
    expect_identical(upd$id, 1L)
    expect_identical(upd$values$diagram_type, 2L)
    # urørte felter bevaret som i DB-rækken
    expect_identical(upd$values$indikator, 10L)
    expect_identical(upd$values$periode_aggregering, "måned")
    expect_true(upd$values$indgaar_i_aggregering)
    expect_match(status_msg(), "Gemt diagram 1")
  })
})

test_that("inline checkbox-ændring gemmer logical", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(tbl = diagram_grid_edit(
      isolate(filtered()), 2L, "Aktiv", "TRUE"))
    upd <- db$.calls()$updated
    expect_identical(upd$id, 2L)
    expect_true(upd$values$diagram_aktivt)
  })
})

test_that("inline Periode ryddet gemmes som NA", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(tbl = diagram_grid_edit(
      isolate(filtered()), 1L, "Periode", NULL))
    upd <- db$.calls()$updated
    expect_identical(upd$id, 1L)
    expect_true(is.na(upd$values$periode_aggregering))
  })
})

test_that("inline Målgruppe-ændring gemmer heltal-fk", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(tbl = diagram_grid_edit(
      isolate(filtered()), 2L, "Målgruppe", "3"))
    upd <- db$.calls()$updated
    expect_identical(upd$id, 2L)
    expect_identical(upd$values$maalgruppe, 3L)
    # urørte felter bevaret
    expect_identical(upd$values$indikator, 11L)
  })
})

test_that("inline Målgruppe ryddet ('(ingen)') gemmes som NA — valgfri fk", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(tbl = diagram_grid_edit(
      isolate(filtered()), 1L, "Målgruppe", NULL))
    upd <- db$.calls()$updated
    expect_identical(upd$id, 1L)
    expect_true(is.na(upd$values$maalgruppe))
  })
})

test_that("inline duplikat giver blød advarsel men gemmer", {
  db <- fake_diagram_db(dup_count = 1L)
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(tbl = diagram_grid_edit(
      isolate(filtered()), 1L, "Type", "2"))
    expect_match(warn_msg(), "Findes allerede")
    expect_false(is.null(db$.calls()$updated))
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

test_that("range-selektion sætter grid_sel til flere pk'er (i grid-orden)", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(tbl = list(
      forSelectedVals = TRUE,
      selectedDataBoundary = list(borderTop = 0, borderBottom = 1,
                                   borderLeft = 0, borderRight = 0),
      fullData = list(data = list(list("1"), list("2")))))
    expect_identical(grid_sel(), c("1", "2"))
  })
})

test_that("'Vælg alle viste' sætter selektionen til alle rækker i den viste (filtrerede) tabel", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(select_all_visible = 1)
    expect_identical(grid_sel(), "1") # kun diagram_aktivt=TRUE er vist (default-filter)

    session$setInputs(filter_status = "alle", select_all_visible = 2)
    expect_setequal(grid_sel(), c("1", "2"))
  })
})

test_that("'Redigér valgte (N)'-knappen reflekterer selektionen og er disabled i denne leverance", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    expect_match(output$bulk_edit_btn$html, "Redigér valgte \\(0\\)")
    session$setInputs(filter_status = "alle", select_all_visible = 1)
    expect_match(output$bulk_edit_btn$html, "Redigér valgte \\(2\\)")
    expect_match(output$bulk_edit_btn$html, "disabled")
  })
})

test_that("slet med median-knæk blokeres (delete_diagram IKKE kaldt)", {
  db <- fake_diagram_db(median_count = 3L)
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(tbl = diagram_grid_select(0))   # diagram 1
    session$setInputs(delete_row = 1)
    expect_null(db$.calls()$deleted)
    expect_match(warn_msg(), "median-knæk")
  })
})

test_that("slet uden median-knæk sletter via række-selektion", {
  db <- fake_diagram_db(median_count = 0L)
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(tbl = diagram_grid_select(0))   # diagram 1
    expect_identical(grid_sel(), "1")
    session$setInputs(delete_row = 1)
    expect_identical(db$.calls()$deleted, 1L)
    expect_match(status_msg(), "Slettet")
    expect_identical(grid_sel(), character(0))         # stale selektion ryddes
  })
})

test_that("slet uden valgt række beder om valg", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(delete_row = 1)
    expect_match(status_msg(), "Vælg en række")
    expect_null(db$.calls()$deleted)
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
    direktionens_tavle = FALSE, maalgruppe = NA_integer_,
    indikator_navn = "Uden hierarki",
    org_navn = "Onkologi", type_navn = "Seriediagram",
    maalgruppe_navn = NA_character_,
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

test_that("diagram: aendring der kun ankommer via fullData gemmes", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    edit <- diagram_grid_edit(isolate(filtered()), 1L, "Periode", "uge")
    session$setInputs(tbl = list(
      forSelectedVals = TRUE,
      selectedDataBoundary = list(borderTop = 0, borderBottom = 0,
                                  borderLeft = 0, borderRight = 0),
      fullData = list(colHeaders = edit$colHeaders, data = edit$data)))
    u <- db$.calls()$updated
    expect_false(is.null(u))
    expect_identical(u$id, 1L)
    expect_identical(u$values$periode_aggregering, "uge")
  })
})

# Alle option-values i et renderet filter-select (uden "Alle"-tomvalget)
filter_option_values <- function(tag) {
  html <- htmltools::renderTags(tag)$html
  opts <- regmatches(html, gregexpr('<option[^>]*value="[^"]*"', html,
                                    perl = TRUE))[[1]]
  vals <- sub('.*value="([^"]*)".*', "\\1", opts)
  vals[nzchar(vals)]
}

test_that("diagram-filtre kaskaderer datapakke -> datasaet -> indikator", {
  db <- fake_diagram_db()
  adm <- data.frame(
    diagram_id = 1:3, indikator = c(10L, 11L, 12L),
    organisatorisk_navn_teknisk = 20L, diagram_type = 1L,
    periode_aggregering = "måned", indgaar_i_aggregering = TRUE,
    diagram_aktivt = TRUE, direktionens_tavle = FALSE,
    indikator_navn = c("I1", "I2", "I3"), org_navn = "Kirurgi",
    type_navn = "Seriediagram", datasaet = c("D1", "D2", "D3"),
    datapakke = c("P1", "P1", "P2"), stringsAsFactors = FALSE)
  db$list_diagrams_admin <- function() adm
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    expect_setequal(filter_option_values(output$filter_datasaet_ui),
                    c("D1", "D2", "D3"))
    session$setInputs(filter_datapakke = "P1")
    expect_setequal(filter_option_values(output$filter_datasaet_ui),
                    c("D1", "D2"))
    expect_setequal(filter_option_values(output$filter_indikator_ui),
                    c("I1", "I2"))
    session$setInputs(filter_datasaet = "D2")
    expect_identical(filter_option_values(output$filter_indikator_ui), "I2")
    # Skift af datapakke goer gammelt datasaet-valg ugyldigt -> ryddes
    session$setInputs(filter_datapakke = "P2")
    expect_identical(filter_option_values(output$filter_datasaet_ui), "D3")
    expect_identical(selected_option_value(output$filter_datasaet_ui), "")
  })
})

test_that("diagram-grid: indikator-dropdown filtreres per raekke paa datasaet", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    hooks <- w$jsHooks$render
    expect_true(length(hooks) >= 1)
    h <- hooks[[length(hooks)]]
    expect_match(h$code, "filter", fixed = TRUE)
    # map: indikator-id -> niveau-udledt datasaet (fra diagram_form_options)
    expect_identical(h$data$map[["10"]], "Tryksår-datasæt")
    expect_identical(h$data$map[["11"]], "Fald-datasæt")
    # 0-baserede kolonneindeks i grid-data:
    # diagram_id, Datapakke, Datasæt, Indikator
    expect_equal(h$data$datasaetCol, 2)
    expect_equal(h$data$indikatorCol, 3)
  })
})

test_that("aggreger_egne_og_boern: checkbox-kolonne i grid + inline-gem + modal-collect", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(id = "diagram", db = db), {
    w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    cols <- w$x$columns
    titles <- vapply(cols, function(c) c$title, "")
    expect_true("Egne+børn" %in% titles)
    expect_identical(
      vapply(cols, function(c) c$type, "")[titles == "Egne+børn"], "checkbox")
  })
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    session$setInputs(tbl = diagram_grid_edit(
      isolate(filtered()), 1L, "Egne+børn", "TRUE"))
    upd <- db$.calls()$updated
    expect_identical(upd$id, 1L)
    expect_true(upd$values$aggreger_egne_og_boern)
    # urørte felter bevaret
    expect_true(upd$values$indgaar_i_aggregering)
    expect_identical(upd$values$indikator, 10L)
  })
  # Modal-formularen samler flaget med (og udelades → FALSE, aldrig NA)
  vals <- .collect_diagram_form(list(d_indikator = "5",
    d_aggreger_egne_og_boern = TRUE))
  expect_true(vals$aggreger_egne_og_boern)
  expect_identical(names(vals), DIAGRAM_COLS)
})

test_that("diagram_excel_data: manglende aggreger_egne_og_boern-kolonne → FALSE, ej fejl", {
  d <- fake_diagram_db()$list_diagrams_admin()
  d$aggreger_egne_og_boern <- NULL
  g <- diagram_excel_data(d)
  expect_identical(g[["Egne+børn"]], c(FALSE, FALSE))
})

test_that("Periode-valg omfatter dag og kvartal — også før nogen række bruger dem", {
  db <- fake_diagram_db()   # diagram_periode_choices() = c("måned", "uge")
  testServer(mod_diagram_server, args = list(id = "diagram", db = db), {
    # Grid'ets Periode-dropdown-source
    w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    cols <- w$x$columns
    titles <- vapply(cols, function(c) c$title, "")
    per_src <- cols[[which(titles == "Periode")]]$source
    ids <- vapply(per_src, function(s) s$id, "")
    expect_true(all(c("dag", "uge", "maaned", "kvartal", "aar") %in% ids))
    expect_true("måned" %in% ids)   # legacy-værdi i brug bevares
    # Modal-formularens select bygges af samme opts$periode
    expect_true(all(c("dag", "kvartal") %in% opts$periode))
  })
})
