# testServer-tests for mod_diagram med fake-db (closures der logger kald).

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
      indikator = data.frame(id = c(10L, 11L), label = c("Tryksår", "Fald")),
      org = data.frame(id = c(20L, 21L), label = c("Kirurgi", "Medicin")),
      type = data.frame(id = c(1L, 2L),
                        label = c("Seriediagram", "Søjlediagram"))),
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

test_that("filter_type input findes ikke længere og påvirker ikke filtered()", {
  db <- fake_diagram_db()
  testServer(mod_diagram_server, args = list(db = db), {
    session$setInputs(filter_status = "alle")
    before <- nrow(filtered())
    session$setInputs(filter_type = "X")
    expect_equal(nrow(filtered()), before)
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
