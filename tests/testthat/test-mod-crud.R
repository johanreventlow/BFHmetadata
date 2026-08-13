selected_option_value <- function(tag) {
  html <- htmltools::renderTags(tag)$html
  option <- regmatches(html, regexpr(
    '<option[^>]* selected(?:="selected")?[^>]*>', html, perl = TRUE))
  sub('.*value="([^"]*)".*', '\\1', option)
}

fake_db <- function() {
  store <- data.frame(id = 1L, indikator_navn = "A", aktiv_indikator = TRUE,
                      indikator_hierarki = 1L, kontaktperson = 1L, datakilde = 1L,
                      label_indikator_hierarki = "Inf.hyg",
                      stringsAsFactors = FALSE)
  calls <- list(created = NULL, updated = NULL, deleted = NULL, junction = list(),
                diagram_created = NULL, diagram_updated = NULL)
  diagrams <- data.frame(
    diagram_id = 7L, indikator = 1L, organisatorisk_navn_teknisk = 20L,
    diagram_type = 1L, periode_aggregering = "måned",
    indgaar_i_aggregering = TRUE, diagram_aktivt = TRUE,
    direktionens_tavle = FALSE, indikator_navn = "A", org_navn = "Kirurgi",
    type_navn = "Seriediagram", stringsAsFactors = FALSE)
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
    # Diagram-accessors (bruges af Diagrammer-sektionen i modalen)
    list_diagrams_admin = function() diagrams,
    diagram_form_options = function() list(
      indikator = data.frame(id = 1L, label = "A"),
      org = data.frame(id = 20L, label = "Kirurgi"),
      type = data.frame(id = 1L, label = "Seriediagram")),
    diagram_periode_choices = function() c("måned", "uge"),
    diagram_duplicate_count = function(indikator, org, type, exclude_id = -1L) 0L,
    create_diagram = function(values) { calls$diagram_created <<- values; 88L },
    update_diagram = function(id, values) {
      calls$diagram_updated <<- list(id = id, values = values); 1L
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

test_that("oversigt bevarer DT-tilstand; inline-tabel er excelR-grid med låste kolonner", {
  db <- fake_db()
  overview_rows <- data.frame(
    id = 1L, indikator_navn = "A", indikator_navn_teknisk = "a",
    aktiv_indikator = TRUE, nøgleindikator = FALSE,
    indikator_hierarki = 1L, kontaktperson = 1L, datakilde = 1L,
    label_datapakke = "Pakke A", label_indikator_hierarki = "Datasæt A",
    stringsAsFactors = FALSE)
  db$list_indikatorer <- function() overview_rows
  testServer(mod_indikator_crud_server,
    args = list(id = "indik", db = db), {
    inline_widget <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    overview_widget <- jsonlite::fromJSON(output$oversigt, simplifyVector = FALSE)

    expect_session_dt_state(overview_widget, session$ns("oversigt"))
    expect_identical(overview_widget$x$options$pageLength, 10L)
    expect_true(any(vapply(overview_widget$x$options$columnDefs, function(def) {
      identical(def$orderable, FALSE) && identical(def$targets, 0L)
    }, logical(1))))

    # Inline-tabellen: excelR-grid — kun INLINE_EDITABLE-kolonner er åbne
    cols <- inline_widget$x$columns
    titles <- vapply(cols, function(c) c$title, "")
    ro <- vapply(cols, function(c) isTRUE(c$readOnly), logical(1))
    types <- vapply(cols, function(c) c$type, "")
    expect_false(ro[titles == "indikator_navn"])
    expect_equal(types[titles == "id"], "hidden")   # pk aldrig synlig
    expect_true(ro[titles == "aktiv_indikator"])
    expect_false(isTRUE(inline_widget$x$autoWidth))
    expect_false(isTRUE(inline_widget$x$allowInsertRow))
  })
})

test_that("dynamiske oversigtsfiltre bevarer kun gyldige valg", {
  db <- fake_db()
  initial_rows <- data.frame(
    id = c(1L, 2L), indikator_navn = c("A", "B"),
    indikator_navn_teknisk = c("a", "b"), aktiv_indikator = c(TRUE, TRUE),
    nøgleindikator = c(FALSE, FALSE), indikator_hierarki = c(1L, 2L),
    kontaktperson = c(1L, 1L), datakilde = c(1L, 1L),
    label_datapakke = c("Pakke A", "Pakke B"),
    label_indikator_hierarki = c("Datasæt A", "Datasæt B"),
    stringsAsFactors = FALSE)
  db$list_indikatorer <- function() initial_rows

  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(filter_datapakke = "Pakke A", filter_datasaet = "Datasæt A")
    reload()
    session$flushReact()

    expect_identical(selected_option_value(output$filter_datapakke_ui), "Pakke A")
    expect_identical(selected_option_value(output$filter_datasaet_ui), "Datasæt A")

    session$setInputs(filter_datapakke = "Pakke B")
    session$flushReact()

    expect_identical(input$filter_datapakke, "Pakke B")
    expect_identical(selected_option_value(output$filter_datasaet_ui), "")
  })
})

# excelR-selektion: borderTop er 0-baseret række i den viste tabel
.tbl_select <- function(row0) {
  list(forSelectedVals = TRUE,
       selectedDataBoundary = list(borderTop = row0, borderBottom = row0,
                                   borderLeft = 0, borderRight = 0))
}

test_that("Gem med tomt navn giver valideringsfejl, ingen update", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0), indikator_navn = "", save = 1)
    expect_match(status_msg(), "indikator_navn")
  })
})

test_that("soft_delete kalder db.soft_delete med active=FALSE", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))   # selektion FØR knap (egen flush)
    session$setInputs(soft_delete = 1)
    expect_equal(db$.calls()$deleted[[2]], FALSE)
  })
})

test_that("inline-edit på editable felt diffes og kalder update med korrekt id", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    d <- rows()
    pay <- lapply(seq_len(nrow(d)), function(i) {
      lapply(d[i, ], function(v) if (is.na(v)) NULL else as.character(v))
    })
    pay[[1]][[which(names(d) == "indikator_navn")]] <- "Nyt navn"
    session$setInputs(tbl = list(colHeaders = as.list(names(d)), data = pay,
                                 forSelectedVals = FALSE))
    u <- db$.calls()$updated
    expect_false(is.null(u))
    expect_equal(u[[1]], 1L)                       # rid fra pk-match
    expect_equal(u[[2]], list(indikator_navn = "Nyt navn"))
  })
})

test_that("inline-edit på readOnly kolonne ignoreres (klient-manipulation)", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    d <- rows()
    pay <- lapply(seq_len(nrow(d)), function(i) {
      lapply(d[i, ], function(v) if (is.na(v)) NULL else as.character(v))
    })
    pay[[1]][[which(names(d) == "aktiv_indikator")]] <- "FALSE"
    session$setInputs(tbl = list(colHeaders = as.list(names(d)), data = pay,
                                 forSelectedVals = FALSE))
    expect_null(db$.calls()$updated)
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

# --- Diagram-sektion i modal (swap-retur, Fase B) ----------------------------

test_that("m_diagram_edit gemmer retur-id og aabner diagram-formular", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(open_id = 1)
    session$setInputs(m_diagram_edit = 7)
    expect_equal(return_ind(), 1L)          # husker indikator til genaabning
  })
})

test_that("diagram-gem fra modal kalder update_diagram og vender tilbage", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(open_id = 1)
    session$setInputs(m_diagram_edit = 7)
    session$setInputs(d_indikator = "1", d_organisatorisk_navn_teknisk = "20",
                      d_diagram_type = "1", d_periode_aggregering = "uge",
                      d_indgaar_i_aggregering = TRUE, d_diagram_aktivt = TRUE,
                      d_direktionens_tavle = FALSE, m_diagram_save = 1)
    upd <- db$.calls()$diagram_updated
    expect_false(is.null(upd))
    expect_identical(upd$id, 7L)
    expect_identical(upd$values$periode_aggregering, "uge")
    expect_null(return_ind())               # retur gennemfoert + nulstillet
    expect_equal(editing_id(), 1L)          # indikator-modal genaabnet
  })
})

test_that("m_diagram_new opretter med laast indikator og vender tilbage", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(open_id = 1)
    session$setInputs(m_diagram_new = 1)
    expect_equal(return_ind(), 1L)
    session$setInputs(d_indikator = "1", d_organisatorisk_navn_teknisk = "20",
                      d_diagram_type = "1", d_periode_aggregering = "",
                      d_indgaar_i_aggregering = FALSE, d_diagram_aktivt = TRUE,
                      d_direktionens_tavle = FALSE, m_diagram_save = 1)
    created <- db$.calls()$diagram_created
    expect_false(is.null(created))
    expect_identical(created$indikator, 1L)
    expect_null(db$.calls()$diagram_updated)
    expect_null(return_ind())
  })
})

test_that("Tilbage-knap genaabner indikator-modal uden db-kald", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(open_id = 1)
    session$setInputs(m_diagram_edit = 7)
    session$setInputs(m_diagram_back = 1)
    expect_null(db$.calls()$diagram_updated)
    expect_null(db$.calls()$diagram_created)
    expect_null(return_ind())
    expect_equal(editing_id(), 1L)
  })
})
