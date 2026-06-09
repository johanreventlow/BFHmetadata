fake_db <- function() {
  store <- data.frame(id = 1L, indikator_navn = "A", aktiv_indikator = TRUE,
                      indikator_hierarki = 1L, kontaktperson = 1L, datakilde = 1L,
                      label_indikator_hierarki = "Inf.hyg",
                      stringsAsFactors = FALSE)
  calls <- list(created = NULL, updated = NULL, deleted = NULL, junction = list())
  jstore <- list(faggrupper = c(1L, 2L), dataprodukter = integer(0),
                 organisation = integer(0))
  list(
    list_indikatorer = function() store,
    fk_options = function() list(
      indikator_hierarki = data.frame(id = 1L, label = "Inf.hyg"),
      kontaktperson = data.frame(id = 1L, label = "Per Sen"),
      datakilde = data.frame(id = 1L, label = "SP")),
    create_indikator = function(values) { calls$created <<- values; 99L },
    update_indikator = function(id, values) { calls$updated <<- list(id, values); 1L },
    soft_delete = function(id, active = FALSE) { calls$deleted <<- list(id, active); 1L },
    get_junction = function(indikator_id, key) jstore[[key]],
    junction_options = function(key) data.frame(id = c(1L, 2L), label = c("X", "Y")),
    set_junction = function(indikator_id, key, parent_ids) {
      calls$junction[[key]] <<- parent_ids; invisible(TRUE)
    },
    save_indikator = function(id, values, picks) {
      calls$updated <<- list(id, values)
      for (key in names(picks)) calls$junction[[key]] <<- picks[[key]]
      invisible(TRUE)
    },
    create_indikator_full = function(values, picks) {
      calls$created <<- list(values, picks); 99L
    },
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

test_that("åbn-knap (open_id) henter m2m og åbner modal", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(open_id = 1)
    expect_equal(editing_id(), 1L)
  })
})

test_that("modal-gem kalder update + set_junction ×3", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(open_id = 1)
    session$setInputs(m_indikator_navn = "Nyt", m_aktiv_indikator = TRUE,
                      m_j_faggrupper = c("1", "2"),
                      m_j_dataprodukter = character(0),
                      m_j_organisation = character(0),
                      modal_save = 1)
    expect_false(is.null(db$.calls()$updated))
    expect_equal(db$.calls()$junction$faggrupper, c(1L, 2L))
    expect_true("organisation" %in% names(db$.calls()$junction))
  })
})

test_that("modal-gem med tomt navn validerer, ingen update", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(open_id = 1, m_indikator_navn = "",
                      m_j_faggrupper = character(0),
                      m_j_dataprodukter = character(0),
                      m_j_organisation = character(0),
                      modal_save = 1)
    expect_match(status_msg(), "indikator_navn")
    expect_null(db$.calls()$updated)
  })
})

test_that("hel-række-klik (rows_selected) sætter editing_id + åbner", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(filter_status = "alle", filter_datapakke = "",
                      filter_datasaet = "")
    session$setInputs(oversigt_rows_selected = 1)
    expect_equal(editing_id(), 1L)
  })
})

test_that("Ny indikator nulstiller editing_id (opret-tilstand)", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(open_id = 1)          # vælg eksisterende
    expect_equal(editing_id(), 1L)
    session$setInputs(new_modal = 1)         # skift til ny
    expect_null(editing_id())
  })
})

test_that("Ny + Gem kalder create_indikator_full, ikke update", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(new_modal = 1)
    session$setInputs(m_indikator_navn = "Helt ny", m_aktiv_indikator = TRUE,
                      m_j_faggrupper = c("1"),
                      m_j_dataprodukter = character(0),
                      m_j_organisation = character(0),
                      modal_save = 1)
    expect_false(is.null(db$.calls()$created))   # create-stien ramt
    expect_null(db$.calls()$updated)             # ikke update
    expect_match(status_msg(), "Oprettet")
  })
})
