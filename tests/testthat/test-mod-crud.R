selected_option_value <- function(tag) {
  html <- htmltools::renderTags(tag)$html
  option <- regmatches(html, regexpr(
    '<option[^>]* selected(?:="selected")?[^>]*>', html,
    perl = TRUE
  ))
  sub('.*value="([^"]*)".*', "\\1", option)
}

fake_db <- function() {
  store <- data.frame(
    id = 1L, indikator_navn = "A",
    indikator_navn_teknisk = "a",
    aktiv_indikator = TRUE, nøgleindikator = FALSE,
    nulfyld_tomme_perioder = FALSE,
    indikator_hierarki = 1L, kontaktperson = 1L, datakilde = 1L,
    mål = NA_character_, ønsket_tendens = NA_character_,
    direkte_link = NA_character_,
    label_datapakke = "Pakke A",
    label_datasaet = "Datasæt A",
    label_indikator_hierarki = "Inf.hyg",
    output_enhed = "pct", # legacy-fritekst (ej kanonisk)
    stringsAsFactors = FALSE
  )
  calls <- list(
    created = NULL, updated = NULL, deleted = NULL, junction = list(),
    diagram_created = NULL, diagram_updated = NULL
  )
  diagrams <- data.frame(
    diagram_id = 7L, indikator = 1L, organisatorisk_navn_teknisk = 20L,
    diagram_type = 1L, periode_aggregering = "måned",
    indgaar_i_aggregering = TRUE, diagram_aktivt = TRUE,
    direktionens_tavle = FALSE, indikator_navn = "A", org_navn = "Kirurgi",
    type_navn = "Seriediagram", stringsAsFactors = FALSE
  )
  jstore <- list(
    faggrupper = c(1L, 2L), dataprodukter = integer(0),
    organisation = integer(0)
  )
  list(
    list_indikatorer = function() store,
    fk_options = function() {
      list(
        # parent_id: node 2 er rod (datasaet), node 1 barn (samling) —
        # dropdown'en skal vise trae-orden med indrykning
        indikator_hierarki = data.frame(
          id = c(1L, 2L), label = c("Inf.hyg", "Datasaet X"),
          parent_id = c(2L, NA), stringsAsFactors = FALSE),
        kontaktperson = data.frame(id = 1L, label = "Per Sen"),
        datakilde = data.frame(id = 1L, label = "SP")
      )
    },
    create_indikator = function(values) {
      calls$created <<- values
      99L
    },
    update_indikator = function(id, values) {
      calls$updated <<- list(id, values)
      1L
    },
    soft_delete = function(id, active = FALSE) {
      calls$deleted <<- list(id, active)
      1L
    },
    get_junction = function(indikator_id, key) jstore[[key]],
    junction_options = function(key) data.frame(id = c(1L, 2L), label = c("X", "Y")),
    set_junction = function(indikator_id, key, parent_ids) {
      calls$junction[[key]] <<- parent_ids
      invisible(TRUE)
    },
    save_indikator = function(id, values, picks) {
      calls$updated <<- list(id, values)
      for (key in names(picks)) calls$junction[[key]] <<- picks[[key]]
      invisible(TRUE)
    },
    create_indikator_full = function(values, picks) {
      calls$created <<- list(values, picks)
      99L
    },
    # Diagram-accessors (bruges af Diagrammer-sektionen i modalen)
    list_diagrams_admin = function() diagrams,
    diagram_form_options = function() {
      list(
        indikator = data.frame(id = 1L, label = "A"),
        org = data.frame(id = 20L, label = "Kirurgi"),
        type = data.frame(id = 1L, label = "Seriediagram")
      )
    },
    diagram_periode_choices = function() c("måned", "uge"),
    diagram_duplicate_count = function(indikator, org, type, exclude_id = -1L) 0L,
    create_diagram = function(values) {
      calls$diagram_created <<- values
      88L
    },
    update_diagram = function(id, values) {
      calls$diagram_updated <<- list(id = id, values = values)
      1L
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

test_that("indikator-grid: skjult pk, checkbokse, FK-dropdowns, låst kontekst", {
  db <- fake_db()
  testServer(mod_indikator_crud_server,
    args = list(id = "indik", db = db),
    {
      w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
      cols <- w$x$columns
      titles <- vapply(cols, function(c) c$title, "")
      ro <- vapply(cols, function(c) isTRUE(c$readOnly), logical(1))
      types <- vapply(cols, function(c) c$type, "")
      expect_equal(types[titles == "id"], "hidden") # pk aldrig synlig
      expect_equal(types[titles == "Aktiv"], "checkbox")
      expect_equal(types[titles == "Nøgle"], "checkbox")
      expect_equal(types[titles == "Nulfyld"], "checkbox") # BFHddl-opt-in-flag
      expect_equal(types[titles == "Hierarki-placering"], "dropdown")
      hp <- cols[[which(titles == "Hierarki-placering")]]
      expect_true(isTRUE(hp$autocomplete))
      expect_equal(types[titles == "Kontaktperson"], "dropdown")
      expect_equal(types[titles == "Datakilde"], "dropdown")
      expect_true(ro[titles == "Datapakke"]) # kontekst låst
      expect_true(ro[titles == "Datasæt"]) # niveau-udledt kontekst
      expect_true(ro[titles == "Indikator-id"])
      expect_false(ro[titles == "Navn"])
      expect_false(isTRUE(w$x$autoWidth))
      expect_false(isTRUE(w$x$allowInsertRow))
    }
  )
})

test_that("dynamiske oversigtsfiltre bevarer kun gyldige valg", {
  db <- fake_db()
  initial_rows <- data.frame(
    id = c(1L, 2L), indikator_navn = c("A", "B"),
    indikator_navn_teknisk = c("a", "b"), aktiv_indikator = c(TRUE, TRUE),
    nøgleindikator = c(FALSE, FALSE), indikator_hierarki = c(1L, 2L),
    kontaktperson = c(1L, 1L), datakilde = c(1L, 1L),
    label_datapakke = c("Pakke A", "Pakke B"),
    # Datasæt-filteret læser det NIVEAU-UDLEDTE datasæt — ikke nodens eget
    # navn (der kan være en indikatorsamling under datasættet)
    label_datasaet = c("Datasæt A", "Datasæt B"),
    label_indikator_hierarki = c("Samling A", "Samling B"),
    stringsAsFactors = FALSE
  )
  db$list_indikatorer <- function() initial_rows

  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(filter_datapakke = "Pakke A", filter_datasaet = "Datasæt A")
    reload()
    session$flushReact()

    expect_identical(selected_option_value(output$filter_datapakke_ui), "Pakke A")
    expect_identical(selected_option_value(output$filter_datasaet_ui), "Datasæt A")
    expect_identical(tbl_rows()$id, 1L) # filtreret på label_datasaet

    session$setInputs(filter_datapakke = "Pakke B")
    session$flushReact()

    expect_identical(input$filter_datapakke, "Pakke B")
    expect_identical(selected_option_value(output$filter_datasaet_ui), "")
  })
})

test_that("tom-tilstand vises når filtre ikke matcher nogen rækker, og Ryd filtre er wired", {
  db <- fake_db()
  initial_rows <- data.frame(
    id = c(1L, 2L), indikator_navn = c("A", "B"),
    indikator_navn_teknisk = c("a", "b"), aktiv_indikator = c(TRUE, TRUE),
    nøgleindikator = c(FALSE, FALSE), indikator_hierarki = c(1L, 2L),
    kontaktperson = c(1L, 1L), datakilde = c(1L, 1L),
    label_datapakke = c("Pakke A", "Pakke B"),
    label_datasaet = c("Datasæt A", "Datasæt B"),
    label_indikator_hierarki = c("Samling A", "Samling B"),
    stringsAsFactors = FALSE
  )
  db$list_indikatorer <- function() initial_rows

  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(filter_datapakke = "Pakke A", filter_datasaet = "Datasæt B")
    reload()
    session$flushReact()

    expect_identical(nrow(tbl_rows()), 0L)
    html <- output$tbl_container$html
    expect_match(html, "Ingen indikatorer")
    expect_match(html, "Ryd filtre")

    # updateSelectInput() er en no-op i testServer (MockShinySession ekkoer
    # ikke klientens svar tilbage til input$...) — den faktiske nulstilling
    # kan kun verificeres i en browser-test. Her tjekkes kun at knappen er
    # wired og ikke fejler.
    expect_no_error({
      session$setInputs(ryd_filtre = 1)
      session$flushReact()
    })
  })
})

# excelR-selektion: borderTop er 0-baseret række; fullData bærer grid'ets
# aktuelle rækkefølge (pk i første kolonne)
.tbl_select <- function(row0, pks = "1") {
  list(
    forSelectedVals = TRUE,
    selectedDataBoundary = list(
      borderTop = row0, borderBottom = row0,
      borderLeft = 0, borderRight = 0
    ),
    fullData = list(data = lapply(pks, function(p) list(p)))
  )
}

# Range-selektion (klik + shift-klik / træk): borderTop..borderBottom er
# begge inklusive og kan spænde over flere rækker.
.tbl_select_range <- function(top, bottom, pks) {
  list(
    forSelectedVals = TRUE,
    selectedDataBoundary = list(
      borderTop = top, borderBottom = bottom,
      borderLeft = 0, borderRight = 0
    ),
    fullData = list(data = lapply(pks, function(p) list(p)))
  )
}

# 3-rækkers db til multi-selektion-tests
.fake_db_3rows <- function() {
  db <- fake_db()
  db$list_indikatorer <- function() {
    data.frame(
      id = c(1L, 2L, 3L), indikator_navn = c("A", "B", "C"),
      indikator_navn_teknisk = c("a", "b", "c"),
      aktiv_indikator = c(TRUE, TRUE, TRUE),
      nøgleindikator = c(FALSE, FALSE, FALSE),
      nulfyld_tomme_perioder = c(FALSE, FALSE, FALSE),
      indikator_hierarki = c(1L, 1L, 1L), kontaktperson = c(1L, 1L, 1L),
      datakilde = c(1L, 1L, 1L),
      mål = NA_character_, ønsket_tendens = NA_character_,
      direkte_link = NA_character_,
      label_datapakke = "Pakke A", label_datasaet = "Datasæt A",
      label_indikator_hierarki = "Inf.hyg", output_enhed = "pct",
      stringsAsFactors = FALSE
    )
  }
  db
}

# excelR onChange-payload: bygget fra den ægte grid-data-helper med én
# celle overskrevet (value = NULL → tømt celle)
.ind_grid_edit <- function(d, id, column, value) {
  g <- indikator_excel_data(d)
  g[[column]][g$id == id] <- if (is.null(value)) NA else value
  list(
    colHeaders = as.list(names(g)),
    data = lapply(seq_len(nrow(g)), function(i) {
      lapply(g[i, ], function(v) {
        if (length(v) == 1 && is.na(v)) NULL else if (is.logical(v)) v else as.character(v)
      })
    }),
    forSelectedVals = FALSE
  )
}

test_that("range-selektion sætter tbl_sel til flere pk'er (i grid-orden)", {
  testServer(mod_indikator_crud_server, args = list(db = .fake_db_3rows()), {
    session$setInputs(tbl = .tbl_select_range(0, 1, pks = c("1", "2", "3")))
    expect_identical(tbl_sel(), c("1", "2"))
  })
})

test_that("'Vælg alle viste' sætter selektionen til alle rækker i den viste (filtrerede) tabel", {
  testServer(mod_indikator_crud_server, args = list(db = .fake_db_3rows()), {
    session$setInputs(select_all_visible = 1)
    expect_setequal(tbl_sel(), c("1", "2", "3"))

    # Filterskift indsnævrer hvad "Vælg alle viste" rammer
    session$setInputs(filter_datapakke = "Pakke B", select_all_visible = 2)
    expect_identical(tbl_sel(), character(0)) # ingen rækker matcher filteret
  })
})

test_that("'Redigér valgte (N)'-knappen reflekterer selektionen og er disabled i denne leverance", {
  testServer(mod_indikator_crud_server, args = list(db = .fake_db_3rows()), {
    expect_match(output$bulk_edit_btn$html, "Redigér valgte \\(0\\)")
    session$setInputs(tbl = .tbl_select_range(0, 1, pks = c("1", "2", "3")))
    expect_match(output$bulk_edit_btn$html, "Redigér valgte \\(2\\)")
    expect_match(output$bulk_edit_btn$html, "disabled")
  })
})

test_that("inline-tømt Navn afvises uden update (obligatorisk)", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .ind_grid_edit(isolate(rows()), 1L, "Navn", NULL))
    expect_match(status_msg(), "obligatorisk")
    expect_null(db$.calls()$updated)
  })
})

test_that("soft_delete viser bekræftelsesdialog og skriver først ved bekræftelse", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0)) # selektion FØR knap (egen flush)
    session$setInputs(soft_delete = 1)
    expect_null(db$.calls()$deleted) # kun dialog vist endnu
    session$setInputs(soft_delete_confirm = 1)
    expect_equal(db$.calls()$deleted[[2]], FALSE)
  })
})

test_that("inline-edit på tekstfelt diffes og kalder update med korrekt id", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .ind_grid_edit(isolate(rows()), 1L, "Navn", "Nyt navn"))
    u <- db$.calls()$updated
    expect_false(is.null(u))
    expect_equal(u[[1]], 1L) # rid fra pk-match
    expect_equal(u[[2]], list(indikator_navn = "Nyt navn"))
  })
})

test_that("inline checkbox-ændring gemmer logical", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .ind_grid_edit(isolate(rows()), 1L, "Nøgle", "TRUE"))
    u <- db$.calls()$updated
    expect_equal(u[[2]], list(nøgleindikator = TRUE))
    session$setInputs(tbl = .ind_grid_edit(isolate(rows()), 1L, "Nulfyld", "TRUE"))
    expect_equal(db$.calls()$updated[[2]], list(nulfyld_tomme_perioder = TRUE))
  })
})

test_that("inline FK-dropdown gemmer integer parent-id; tømt afvises", {
  db <- fake_db()
  db$fk_options <- function() {
    list(
      indikator_hierarki = data.frame(id = c(1L, 2L), label = c("Inf.hyg", "Andet")),
      kontaktperson = data.frame(id = 1L, label = "Per Sen"),
      datakilde = data.frame(id = 1L, label = "SP")
    )
  }
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .ind_grid_edit(
      isolate(rows()), 1L,
      "Hierarki-placering", "2"
    ))
    u <- db$.calls()$updated
    expect_equal(u[[2]], list(indikator_hierarki = 2L))
    session$setInputs(tbl = .ind_grid_edit(
      isolate(rows()), 1L,
      "Hierarki-placering", NULL
    ))
    expect_match(status_msg(), "Vælg en værdi")
    expect_equal(
      db$.calls()$updated[[2]],
      list(indikator_hierarki = 2L)
    ) # ingen NY update
  })
})

test_that("output_enhed er dropdown med kanoniske værdier + bevaret legacy", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    cols <- w$x$columns
    titles <- vapply(cols, function(c) c$title, "")
    oe <- cols[[which(titles == "Output-enhed")]]
    expect_equal(oe$type, "dropdown")
    names_in_src <- vapply(oe$source, function(s) s$name, "")
    expect_true(all(OUTPUT_ENHED_CHOICES %in% names_in_src))
    expect_true("(ingen)" %in% names_in_src)
    expect_true("pct" %in% names_in_src) # legacy-værdi tabes ikke
  })
})

test_that("inline-valg af output_enhed kalder update med kanonisk værdi", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .ind_grid_edit(
      isolate(rows()), 1L,
      "Output-enhed", "Procent"
    ))
    u <- db$.calls()$updated
    expect_false(is.null(u))
    expect_equal(u[[1]], 1L)
    expect_equal(u[[2]], list(output_enhed = "Procent"))
  })
})

test_that("inline-edit på readOnly kolonne ignoreres (klient-manipulation)", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .ind_grid_edit(
      isolate(rows()), 1L,
      "Datapakke", "Hacket"
    ))
    expect_null(db$.calls()$updated)
    session$setInputs(tbl = .ind_grid_edit(
      isolate(rows()), 1L,
      "Datasæt", "Hacket"
    ))
    expect_null(db$.calls()$updated)
  })
})

test_that(".collect_form med prefix læser præfiksede inputs", {
  fields <- list(
    list(col = "indikator_navn", kind = "text"),
    list(col = "aktiv_indikator", kind = "bool")
  )
  input <- list(m_indikator_navn = "Test", m_aktiv_indikator = TRUE)
  vals <- .collect_form(input, fields, prefix = "m_")
  expect_equal(vals$indikator_navn, "Test")
  expect_true(vals$aktiv_indikator)
})

test_that(".collect_form: tom dato og tomt choice-valg → NA (aldrig dags dato)", {
  fields <- list(
    list(col = "periode_fra", kind = "date"),
    list(
      col = "output_enhed", kind = "choice",
      choices = OUTPUT_ENHED_CHOICES
    )
  )
  # Tom dateInput leverer en Date af længde 0 — må ikke ende som dags dato
  input <- list(m_periode_fra = as.Date(character(0)), m_output_enhed = "")
  vals <- .collect_form(input, fields, prefix = "m_")
  expect_true(is.na(vals$periode_fra))
  expect_true(is.na(vals$output_enhed))
})

test_that(".field_input: tom dato renderes TOM (ikke dags dato)", {
  f <- list(col = "periode_fra", kind = "date")
  html <- as.character(htmltools::renderTags(
    .field_input(NS("x"), f, values = list(periode_fra = NA))
  )$html)
  # dateInput(value = NULL) ville skrive dags dato i data-initial-date
  expect_false(grepl(format(Sys.Date(), "%Y-%m-%d"), html, fixed = TRUE))
})

test_that(".field_input: choice-felt er dropdown med kanoniske + legacy-værdi", {
  f <- list(
    col = "output_enhed", kind = "choice",
    choices = OUTPUT_ENHED_CHOICES
  )
  html <- as.character(htmltools::renderTags(
    .field_input(NS("x"), f, values = list(output_enhed = "pct"))
  )$html)
  expect_match(html, "<select", fixed = TRUE)
  expect_match(html, ">Procent<", fixed = TRUE)
  expect_match(html, 'value="pct"', fixed = TRUE) # legacy bevares + valgt
  expect_match(html, "(ingen)", fixed = TRUE)
})

test_that("modal-gem inkluderer output_enhed", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))
    session$setInputs(open_selected = 1)
    session$setInputs(
      m_indikator_navn = "Nyt", m_output_enhed = "Procent",
      m_j_faggrupper = character(0),
      m_j_dataprodukter = character(0),
      m_j_organisation = character(0),
      modal_save = 1
    )
    u <- db$.calls()$updated
    expect_false(is.null(u))
    expect_identical(u[[2]]$output_enhed, "Procent")
  })
})

test_that("Åbn valgte henter m2m og åbner modal", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))

    session$setInputs(open_selected = 1)
    expect_equal(editing_id(), 1L)
  })
})

test_that("modal-gem kalder update + set_junction ×3", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))

    session$setInputs(open_selected = 1)
    session$setInputs(
      m_indikator_navn = "Nyt", m_aktiv_indikator = TRUE,
      m_j_faggrupper = c("1", "2"),
      m_j_dataprodukter = character(0),
      m_j_organisation = character(0),
      modal_save = 1
    )
    expect_false(is.null(db$.calls()$updated))
    expect_equal(db$.calls()$junction$faggrupper, c(1L, 2L))
    expect_true("organisation" %in% names(db$.calls()$junction))
  })
})

test_that("modal-gem med tomt navn validerer, ingen update", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))
    session$setInputs(open_selected = 1)
    session$setInputs(
      m_indikator_navn = "",
      m_j_faggrupper = character(0),
      m_j_dataprodukter = character(0),
      m_j_organisation = character(0),
      modal_save = 1
    )
    expect_match(status_msg(), "indikator_navn")
    expect_null(db$.calls()$updated)
  })
})

test_that("Åbn valgte uden selektion beder om valg", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(open_selected = 1)
    expect_match(status_msg(), "Vælg en række")
    expect_null(editing_id())
  })
})

test_that("Ny indikator nulstiller editing_id (opret-tilstand)", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0)) # vælg eksisterende
    session$setInputs(open_selected = 1)
    expect_equal(editing_id(), 1L)
    session$setInputs(new_modal = 1) # skift til ny
    expect_null(editing_id())
  })
})

test_that("Ny + Gem kalder create_indikator_full, ikke update", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(new_modal = 1)
    session$setInputs(
      m_indikator_navn = "Helt ny", m_aktiv_indikator = TRUE,
      m_j_faggrupper = c("1"),
      m_j_dataprodukter = character(0),
      m_j_organisation = character(0),
      modal_save = 1
    )
    expect_false(is.null(db$.calls()$created)) # create-stien ramt
    expect_null(db$.calls()$updated) # ikke update
    expect_match(status_msg(), "Oprettet")
  })
})

# --- Diagram-sektion i modal (swap-retur, Fase B) ----------------------------

test_that("m_diagram_edit gemmer retur-id og aabner diagram-formular", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))

    session$setInputs(open_selected = 1)
    session$setInputs(m_diagram_edit = 7)
    expect_equal(return_ind(), 1L) # husker indikator til genaabning
  })
})

test_that("diagram-gem fra modal kalder update_diagram og vender tilbage", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))

    session$setInputs(open_selected = 1)
    session$setInputs(m_diagram_edit = 7)
    session$setInputs(
      d_indikator = "1", d_organisatorisk_navn_teknisk = "20",
      d_diagram_type = "1", d_periode_aggregering = "uge",
      d_indgaar_i_aggregering = TRUE, d_diagram_aktivt = TRUE,
      d_direktionens_tavle = FALSE, m_diagram_save = 1
    )
    upd <- db$.calls()$diagram_updated
    expect_false(is.null(upd))
    expect_identical(upd$id, 7L)
    expect_identical(upd$values$periode_aggregering, "uge")
    expect_null(return_ind()) # retur gennemfoert + nulstillet
    expect_equal(editing_id(), 1L) # indikator-modal genaabnet
  })
})

test_that("m_diagram_new opretter med laast indikator og vender tilbage", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))

    session$setInputs(open_selected = 1)
    session$setInputs(m_diagram_new = 1)
    expect_equal(return_ind(), 1L)
    session$setInputs(
      d_indikator = "1", d_organisatorisk_navn_teknisk = "20",
      d_diagram_type = "1", d_periode_aggregering = "",
      d_indgaar_i_aggregering = FALSE, d_diagram_aktivt = TRUE,
      d_direktionens_tavle = FALSE, m_diagram_save = 1
    )
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
    session$setInputs(tbl = .tbl_select(0))

    session$setInputs(open_selected = 1)
    session$setInputs(m_diagram_edit = 7)
    session$setInputs(m_diagram_back = 1)
    expect_null(db$.calls()$diagram_updated)
    expect_null(db$.calls()$diagram_created)
    expect_null(return_ind())
    expect_equal(editing_id(), 1L)
  })
})

# --- Aktiv-filtrering af datasaet-dropdown (Fase D) --------------------------

.ih_opts <- function() {
  data.frame(
    id = c(1L, 2L, 3L),
    label = c("Aktiv A", "Aktiv B", "Inaktiv C"),
    aktiv = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

test_that(".hierarki_choices filtrerer inaktive ved nyvalg", {
  ch <- .hierarki_choices(.ih_opts(), current_id = NULL)
  expect_setequal(unname(ch), c(1L, 2L))
  expect_false("Inaktiv C" %in% names(ch))
})

test_that(".hierarki_choices bevarer nuvaerende inaktiv vaerdi med suffix", {
  ch <- .hierarki_choices(.ih_opts(), current_id = 3L)
  expect_true(3L %in% ch)
  expect_identical(names(ch)[ch == 3L], "Inaktiv C (inaktiv)")
})

test_that(".hierarki_choices duplikerer ikke aktivt valg og taaler manglende aktiv-kolonne", {
  ch <- .hierarki_choices(.ih_opts(), current_id = 1L)
  expect_identical(sum(ch == 1L), 1L)
  # fk_options uden aktiv-kolonne (aeldre fakes/DB): alle behandles som aktive
  legacy <- data.frame(id = 1:2, label = c("A", "B"), stringsAsFactors = FALSE)
  expect_length(.hierarki_choices(legacy, current_id = NULL), 2)
})

test_that(".hierarki_src til grid: aktive + brugte inaktive + ukendte id'er", {
  src <- .hierarki_src(.ih_opts(), used = c(1L, 3L, 99L))
  expect_setequal(src$id, c("1", "2", "3", "99"))
  expect_identical(src$name[src$id == "3"], "Inaktiv C (inaktiv)")
  expect_match(src$name[src$id == "99"], "Ukendt")
  # ubrugte inaktive noder tilbydes ikke
  src2 <- .hierarki_src(.ih_opts(), used = c(1L))
  expect_false("3" %in% src2$id)
})

# Regression: jexcel flytter markøren umiddelbart efter en celle-commit, og
# selektions-eventet OVERSKRIVER change-eventet i Shinys input-batch (samme
# input-id, ingen priority:event i excelR's JS). Ændringen ankommer derfor
# ofte KUN via selektions-payloadens fullData — den skal stadig gemmes.
.ind_grid_select_full <- function(d, id, column, value, row0 = 0L) {
  edit <- .ind_grid_edit(d, id, column, value)
  list(
    forSelectedVals = TRUE,
    selectedDataBoundary = list(
      borderTop = row0, borderBottom = row0,
      borderLeft = 0, borderRight = 0
    ),
    fullData = list(colHeaders = edit$colHeaders, data = edit$data)
  )
}

test_that("tekst-aendring der kun ankommer via selektionens fullData gemmes", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .ind_grid_select_full(
      isolate(rows()), 1L, "Navn", "Nyt navn"
    ))
    u <- db$.calls()$updated
    expect_false(is.null(u))
    expect_equal(u[[1]], 1L)
    expect_equal(u[[2]], list(indikator_navn = "Nyt navn"))
    expect_identical(tbl_sel(), "1") # selektionen registreres stadig
  })
})

test_that("Hierarki-placering-dropdown viser trae (depth-first + indrykning)", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    w <- jsonlite::fromJSON(output$tbl, simplifyVector = FALSE)
    cols <- w$x$columns
    titles <- vapply(cols, function(c) c$title, "")
    src <- cols[[which(titles == "Hierarki-placering")]]$source
    expect_identical(vapply(src, function(s) s$id, ""), c("2", "1"))
    expect_identical(vapply(src, function(s) s$name, ""),
                     c("Datasaet X", paste0(strrep(" ", 2), "Inf.hyg")))
  })
})

# --- Indikator-id (indikator_navn_teknisk): redigerbart bag bekraeftelse -----
# Feltet er parquet-noeglen — appen slaar datafilerne op paa praecis denne
# streng. Det maa kunne rettes (id'er kan vaere forkerte fra start), men
# aldrig ved et uheld: modalen er eneste indgang, og en aendring paa en
# eksisterende indikator skal bekraeftes foer den skrives.

test_that(".teknisk_uaendret: kun en reel omdoebning taeller som aendring", {
  # tomme repraesentationer er samme vaerdi
  expect_true(.teknisk_uaendret(NULL, NA))
  expect_true(.teknisk_uaendret(NA_character_, ""))
  expect_true(.teknisk_uaendret(character(0), NULL))
  # whitespace i kanten er ikke en aendring (browseren kan tilfoeje den)
  expect_true(.teknisk_uaendret("a", " a "))
  expect_true(.teknisk_uaendret("a", "a"))
  # reelle aendringer — inkl. case, da parquet-opslaget er case-sensitivt
  expect_false(.teknisk_uaendret("a", "b"))
  expect_false(.teknisk_uaendret("a", "A"))
  expect_false(.teknisk_uaendret("a", NA)) # toemning bryder ogsaa koblingen
  expect_false(.teknisk_uaendret(NA, "a")) # foerste udfyldning
})

test_that("indikator-id er med i modalens felter, men ikke i grid/bulk", {
  expect_true("indikator_navn_teknisk" %in% INDIKATOR_MODAL_COLS)
  # grid'et redigerer kun felter herfra — id'et maa ikke kunne rammes der
  expect_false("indikator_navn_teknisk" %in% .INDIKATOR_GRID_FIELDS)
  bulk <- vapply(BULK_INDIKATOR_FIELDS, function(f) f$col, "")
  expect_false("indikator_navn_teknisk" %in% bulk)
})

test_that("gem uden aendret indikator-id skriver direkte (ingen dialog)", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))
    session$setInputs(open_selected = 1)
    session$setInputs(
      m_indikator_navn = "Nyt",
      m_indikator_navn_teknisk = "a", # uaendret ift. fake_db's store
      m_j_faggrupper = character(0),
      m_j_dataprodukter = character(0),
      m_j_organisation = character(0),
      modal_save = 1
    )
    expect_null(pending_save()) # intet i vente → ingen bekraeftelse kraevet
    u <- db$.calls()$updated
    expect_false(is.null(u))
    expect_identical(u[[2]]$indikator_navn_teknisk, "a")
  })
})

test_that("aendret indikator-id skriver IKKE foer bekraeftelse", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))
    session$setInputs(open_selected = 1)
    session$setInputs(
      m_indikator_navn = "Nyt",
      m_indikator_navn_teknisk = "a_rettet",
      m_j_faggrupper = c("1", "2"),
      m_j_dataprodukter = character(0),
      m_j_organisation = character(0),
      modal_save = 1
    )
    expect_null(db$.calls()$updated) # intet skrevet endnu
    p <- pending_save()
    expect_false(is.null(p))
    expect_identical(p$vals$indikator_navn_teknisk, "a_rettet")
    expect_equal(p$picks$faggrupper, c(1L, 2L)) # m2m-valg holdes i vente
  })
})

test_that("bekraeftelse skriver det nye indikator-id og rydder ventende gem", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))
    session$setInputs(open_selected = 1)
    session$setInputs(
      m_indikator_navn = "Nyt",
      m_indikator_navn_teknisk = "a_rettet",
      m_j_faggrupper = character(0),
      m_j_dataprodukter = character(0),
      m_j_organisation = character(0),
      modal_save = 1
    )
    session$setInputs(teknisk_confirm = 1)
    u <- db$.calls()$updated
    expect_false(is.null(u))
    expect_equal(u[[1]], 1L)
    expect_identical(u[[2]]$indikator_navn_teknisk, "a_rettet")
    expect_null(pending_save()) # ryddet efter vellykket gem
  })
})

test_that("fortryd skriver intet og rydder ventende gem", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl = .tbl_select(0))
    session$setInputs(open_selected = 1)
    session$setInputs(
      m_indikator_navn = "Nyt",
      m_indikator_navn_teknisk = "a_rettet",
      m_j_faggrupper = character(0),
      m_j_dataprodukter = character(0),
      m_j_organisation = character(0),
      modal_save = 1
    )
    session$setInputs(teknisk_cancel = 1)
    expect_null(db$.calls()$updated)
    expect_null(pending_save())
  })
})

test_that("ny indikator faar sit indikator-id uden bekraeftelse", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(new_modal = 1)
    session$setInputs(
      m_indikator_navn = "Ny",
      m_indikator_navn_teknisk = "ny_indikator",
      m_j_faggrupper = character(0),
      m_j_dataprodukter = character(0),
      m_j_organisation = character(0),
      modal_save = 1
    )
    # Ingen data peger paa indikatoren endnu → intet at bryde, ingen dialog
    expect_null(pending_save())
    cr <- db$.calls()$created
    expect_false(is.null(cr))
    expect_identical(cr[[1]]$indikator_navn_teknisk, "ny_indikator")
  })
})

test_that("bekraeftelsesdialogen viser fra/til og advarer om datakoblingen", {
  html <- as.character(htmltools::renderTags(
    .byg_teknisk_confirm("gammelt_id", "nyt_id", NS("x"))
  )$html)
  expect_match(html, "gammelt_id", fixed = TRUE)
  expect_match(html, "nyt_id", fixed = TRUE)
  expect_match(html, "alert-warning", fixed = TRUE)
  expect_match(html, "parquet", fixed = TRUE)
  # baade bekraeft og fortryd skal vaere rigtige inputs (fortryd genaabner
  # formularen — en ren modalButton ville tabe brugerens indtastninger)
  expect_match(html, 'id="x-teknisk_confirm"', fixed = TRUE)
  expect_match(html, 'id="x-teknisk_cancel"', fixed = TRUE)
})

test_that("bekraeftelsesdialogen viser tom vaerdi som (tomt), ikke NA", {
  html <- as.character(htmltools::renderTags(
    .byg_teknisk_confirm(NA_character_, "", NS("x"))
  )$html)
  expect_match(html, "(tomt)", fixed = TRUE)
  expect_false(grepl(">NA<", html, fixed = TRUE))
})
