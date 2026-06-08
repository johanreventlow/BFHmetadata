fake_db <- function() {
  store <- data.frame(id = 1L, indikator_navn = "A", aktiv_indikator = TRUE,
                      stringsAsFactors = FALSE)
  calls <- list(created = NULL, updated = NULL, deleted = NULL)
  list(
    list_indikatorer = function() store,
    fk_options = function() list(
      indikator_hierarki = data.frame(id = 1L, label = "H1"),
      kontaktperson = data.frame(id = 1L, label = "Per Sen"),
      datakilde = data.frame(id = 1L, label = "SP")),
    create_indikator = function(values) { calls$created <<- values; 99L },
    update_indikator = function(id, values) { calls$updated <<- list(id, values); 1L },
    soft_delete = function(id, active = FALSE) { calls$deleted <<- list(id, active); 1L },
    .calls = function() calls
  )
}

test_that("modul indlæser data ved start", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    expect_equal(nrow(rows()), 1)
  })
})

test_that("Gem med tomt navn giver valideringsfejl, ingen update", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl_rows_selected = 1, indikator_navn = "", save = 1)
    expect_match(status_msg(), "indikator_navn")
  })
})

test_that("soft_delete kalder db.soft_delete med active=FALSE", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl_rows_selected = 1, soft_delete = 1)
    expect_equal(db$.calls()$deleted[[2]], FALSE)
  })
})

test_that("inline-edit på editable felt kalder update", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl_cell_edit = list(row = 1, col = which(names(db$list_indikatorer())=="indikator_navn")-1, value = "Nyt navn"))
    expect_false(is.null(db$.calls()$updated))
  })
})

test_that(".collect_form med prefix læser præfiksede inputs", {
  fields <- list(list(col = "indikator_navn", kind = "text"),
                 list(col = "aktiv_indikator", kind = "bool"))
  input <- list(m_indikator_navn = "Test", m_aktiv_indikator = TRUE)
  vals <- .collect_form(input, fields, prefix = "m_")
  expect_equal(vals$indikator_navn, "Test")
  expect_true(vals$aktiv_indikator)
})
