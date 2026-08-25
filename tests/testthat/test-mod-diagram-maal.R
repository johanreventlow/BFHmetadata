# testServer-tests for mod_diagram_maal med fake-db (samme mønster som
# test-mod-diagram.R).

fake_maal_db <- function() {
  admin <- data.frame(
    maal_id = c(1L, 2L),
    diagram = c(1L, 2L),
    maal_retning = c(">=", NA),
    maal_vaerdi = c(80, 50),
    maal_gaeldende_fra = as.Date(c("2026-01-01", NA)),
    indikator_navn = c("Tryksår", "Fald"),
    org_navn = c("Kirurgi", "Medicin"),
    type_navn = c("Seriediagram", "Søjlediagram"),
    maalgruppe_navn = c("Klinikere", NA),
    datasaet = c("Tryksår-datasæt", "Fald-datasæt"),
    datapakke = c("Kliniske indikatorer", "Kliniske indikatorer"),
    stringsAsFactors = FALSE)
  diagrams <- data.frame(
    diagram_id = c(1L, 2L),
    indikator_navn = c("Tryksår", "Fald"),
    org_navn = c("Kirurgi", "Medicin"),
    maalgruppe_navn = c("Klinikere", NA),
    stringsAsFactors = FALSE)
  calls <- list(created = NULL, updated = NULL, updates = list(), deleted = NULL)
  list(
    list_maal_admin = function() admin,
    list_diagrams_admin = function() diagrams,
    create_maal = function(values) { calls$created <<- values; 99L },
    update_maal = function(id, values) {
      calls$updated <<- list(id = id, values = values)
      calls$updates[[length(calls$updates) + 1L]] <<- calls$updated
      1L
    },
    delete_maal = function(id) { calls$deleted <<- id; 1L },
    .calls = function() calls
  )
}

# excelR-payload-helpers (samme mønster som test-mod-diagram.R)
maal_grid_edit <- function(d, id, column, value) {
  g <- maal_excel_data(d)
  g[[column]][g$maal_id == id] <- if (is.null(value)) NA else value
  list(
    colHeaders = as.list(names(g)),
    data = lapply(seq_len(nrow(g)), function(i) {
      lapply(g[i, ], function(v) {
        if (length(v) == 1 && is.na(v)) NULL else if (is.logical(v)) v else as.character(v)
      })
    }),
    forSelectedVals = FALSE)
}

maal_grid_select <- function(row0, pks = c("1", "2")) {
  list(forSelectedVals = TRUE,
       selectedDataBoundary = list(borderTop = row0, borderBottom = row0,
                                   borderLeft = 0, borderRight = 0),
       fullData = list(data = lapply(pks, function(p) list(p))))
}

test_that("admin-liste indlæses uden filtre", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    expect_equal(nrow(admin()), 2)
    expect_equal(nrow(filtered()), 2)
  })
})

test_that("mål-grid: skjult pk, Retning som dropdown, resten readOnly-kontekst", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(id = "maal", db = db), {
    w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    cols <- w$x$columns
    titles <- vapply(cols, function(c) c$title, "")
    types <- vapply(cols, function(c) c$type, "")
    expect_identical(types[titles == "maal_id"], "hidden")
    expect_identical(types[titles == "Retning"], "dropdown")
    expect_true(all(vapply(
      cols[titles %in% c("Indikator", "Enhed", "Type", "Målgruppe")],
      function(c) isTRUE(c$readOnly), logical(1))))
    expect_false(isTRUE(cols[[which(titles == "Værdi")]]$readOnly)) # editable
  })
})

test_that("mål-grid viser diagrammets målgruppe (tom ved NA)", {
  g <- maal_excel_data(data.frame(
    maal_id = 1:2, datapakke = "p", datasaet = "d",
    indikator_navn = c("A", "B"), org_navn = "E", type_navn = "Serie",
    maalgruppe_navn = c("Klinikere", NA),
    maal_retning = ">=", maal_vaerdi = 1,
    maal_gaeldende_fra = as.Date(NA), stringsAsFactors = FALSE))
  expect_identical(g[["Målgruppe"]], c("Klinikere", ""))
  # Ældre kaldere uden kolonnen (fx cachede admin-df'er): tom, aldrig fejl
  g2 <- maal_excel_data(data.frame(
    maal_id = 1L, datapakke = "p", datasaet = "d", indikator_navn = "A",
    org_navn = "E", type_navn = "Serie", maal_retning = ">=",
    maal_vaerdi = 1, maal_gaeldende_fra = as.Date(NA),
    stringsAsFactors = FALSE))
  expect_identical(g2[["Målgruppe"]], "")
})

test_that("Nyt mål-vælgeren medtager målgruppe i diagram-labelen", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    ch <- .diagram_choices()
    expect_identical(ch$label[ch$id == 1L], "Tryksår – Kirurgi · Klinikere (#1)")
    expect_identical(ch$label[ch$id == 2L], "Fald – Medicin (#2)") # NA → udelades
  })
})

test_that("inline Retning-ændring patcher fuld række og kalder update_maal", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    session$setInputs(tbl = maal_grid_edit(
      isolate(filtered()), 1L, "Retning", "<="))
    upd <- db$.calls()$updated
    expect_false(is.null(upd))
    expect_identical(upd$id, 1L)
    expect_identical(upd$values$maal_retning, "<=")
    # urørte felter bevaret som i DB-rækken
    expect_identical(upd$values$diagram, 1L)
    expect_identical(upd$values$maal_vaerdi, 80)
  })
})

test_that("inline Værdi-ændring gemmer numerisk", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    session$setInputs(tbl = maal_grid_edit(
      isolate(filtered()), 2L, "Værdi", "65.5"))
    upd <- db$.calls()$updated
    expect_identical(upd$id, 2L)
    expect_identical(upd$values$maal_vaerdi, 65.5)
  })
})

test_that("inline ugyldig Værdi afvises og gemmer intet", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    session$setInputs(tbl = maal_grid_edit(
      isolate(filtered()), 1L, "Værdi", "ikke-et-tal"))
    expect_null(db$.calls()$updated)
    expect_match(status_msg(), "tal")
  })
})

test_that("inline Gældende fra-ændring gemmer gyldig dato", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    session$setInputs(tbl = maal_grid_edit(
      isolate(filtered()), 2L, "Gældende fra", "2026-03-15"))
    upd <- db$.calls()$updated
    expect_identical(upd$id, 2L)
    expect_identical(upd$values$maal_gaeldende_fra, "2026-03-15")
  })
})

test_that("inline ugyldig dato afvises med dansk fejlbesked", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    session$setInputs(tbl = maal_grid_edit(
      isolate(filtered()), 2L, "Gældende fra", "15/03/2026"))
    expect_null(db$.calls()$updated)
    expect_match(status_msg(), "ÅÅÅÅ-MM-DD")
  })
})

test_that("inline Gældende fra ryddet gemmes som NA", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    session$setInputs(tbl = maal_grid_edit(
      isolate(filtered()), 1L, "Gældende fra", NULL))
    upd <- db$.calls()$updated
    expect_identical(upd$id, 1L)
    expect_true(is.na(upd$values$maal_gaeldende_fra))
  })
})

test_that("delete_row uden valgt række giver besked, ingen sletning", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    session$setInputs(delete_row = 1)
    expect_match(status_msg(), "Vælg en række")
    expect_null(db$.calls()$deleted)
  })
})

test_that("delete_row med valgt række sletter og genindlæser", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    session$setInputs(tbl = maal_grid_select(0))
    session$setInputs(delete_row = 1)
    expect_identical(db$.calls()$deleted, 1L)
    expect_match(status_msg(), "Slettet mål 1")
  })
})

test_that("Nyt mål: gyldig formular opretter og lukker modal", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    session$setInputs(mf_diagram = "2", mf_maal_retning = ">=",
                      mf_maal_vaerdi = 42, mf_maal_gaeldende_fra = "2026-06-01")
    session$setInputs(mf_save = 1)
    created <- db$.calls()$created
    expect_false(is.null(created))
    expect_identical(created$diagram, 2L)
    expect_identical(created$maal_retning, ">=")
    expect_identical(created$maal_vaerdi, 42)
    expect_match(status_msg(), "Oprettet mål 99")
  })
})

test_that("Nyt mål: manglende diagram/værdi blokerer og opretter intet", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    session$setInputs(mf_diagram = "", mf_maal_vaerdi = NA)
    session$setInputs(mf_save = 1)
    expect_null(db$.calls()$created)
    expect_match(status_msg(), "obligatorisk")
  })
})

test_that("filtrering på indikator/enhed/datapakke/datasæt reducerer listen", {
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    session$setInputs(filter_indikator = "Tryksår")
    expect_equal(nrow(filtered()), 1)
    expect_identical(filtered()$maal_id, 1L)

    session$setInputs(filter_indikator = "", filter_org = "Medicin")
    expect_equal(nrow(filtered()), 1)
    expect_identical(filtered()$maal_id, 2L)

    session$setInputs(filter_org = "", filter_datapakke = "Kliniske indikatorer")
    expect_equal(nrow(filtered()), 2)

    session$setInputs(filter_datasaet = "Fald-datasæt")
    expect_equal(nrow(filtered()), 1)
    expect_identical(filtered()$maal_id, 2L)
  })
})

test_that("identisk payload efter gem skippes som re-render-ekko (loop-værn)", {
  # excelR gen-sender payloads efter hvert re-render, og reload() efter et gem
  # re-renderer grid'et. Fake-db'ens admin ændres ikke af update_maal, så
  # diffen er identisk anden gang — præcis som et ekko med en vedvarende
  # repræsentationsforskel. Uden værn ville hver gentagelse gemme + reloade
  # igen (gem→reload→ekko-loop); med værn behandles ekkoet ikke.
  db <- fake_maal_db()
  testServer(mod_diagram_maal_server, args = list(db = db), {
    p <- maal_grid_edit(isolate(filtered()), 1L, "Retning", "<=")
    session$setInputs(tbl = p)
    expect_length(db$.calls()$updates, 1)
    session$setInputs(tbl = p)               # ekkoet
    expect_length(db$.calls()$updates, 1)    # ingen ny skrivning
    session$setInputs(tbl = p)               # ægte gentagelse (værn forbrugt)
    expect_length(db$.calls()$updates, 2)
  })
})
