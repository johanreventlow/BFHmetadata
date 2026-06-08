# Oversigtstabel + Modal-redigering med m2m-relationer — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Tilføj en kompakt oversigtstabel med per-række åbn-knap der åbner en modal til fuld redigering af indikatorens felter, direkte FK-relationer og m2m-junction-relationer (vist med tekst-værdier).

**Architecture:** `mod_indikator_crud` ombygges til `bslib::navset_tab` med fane "Oversigt" (ny kompakt DT + [Åbn]-knap → modal) og fane "Inline-redigering" (eksisterende UI uændret). Modal genbruger `.field_input()`/`.collect_form()` (udvidet med `prefix` + `values`). M2m-relationer skrives via replace-strategi i `pool::poolWithTransaction` (atomisk). Begge faner deler `rows()` reactiveVal.

**Tech Stack:** R, Shiny, bslib, DT, DBI, RPostgres, pool, testthat (edition 3), withr.

**Spec:** `docs/superpowers/specs/2026-06-08-oversigt-modal-m2m-design.md`

---

## File Structure

| Fil | Ansvar | Ændring |
|-----|--------|---------|
| `R/metadata.R` | `INDIKATOR_JUNCTIONS`-konstant | Modify (append) |
| `R/fct_sql.R` | 4 rene junction-SQL-byggere | Modify (append) |
| `R/fct_db.R` | `get_junction`/`junction_options`/`set_junction` accessors | Modify (`make_db`) |
| `R/mod_indikator_crud.R` | navset_tab UI, oversigt-DT + knap, modal-flow, `.field_input`/`.collect_form` udvidet | Modify |
| `tests/testthat/test-sql.R` | junction-builder unit-tests | Modify (append) |
| `tests/testthat/test-mod-crud.R` | fake_db udvidet + modal-flow-tests | Modify |
| `tests/testthat/test-db-junction.R` | gated integration: replace-roundtrip + rollback | Create |
| `DESCRIPTION`, `NEWS.md` | version-bump 0.2.0 | Modify |

---

## Task 1: Junction-metadata

**Files:**
- Modify: `R/metadata.R` (append efter `INLINE_EDITABLE`)
- Test: `tests/testthat/test-sql.R`

- [ ] **Step 1: Write the failing test**

Append til `tests/testthat/test-sql.R`:

```r
test_that("INDIKATOR_JUNCTIONS har 3 relationer med påkrævede felter", {
  expect_named(INDIKATOR_JUNCTIONS, c("faggrupper", "dataprodukter", "organisation"))
  for (j in INDIKATOR_JUNCTIONS) {
    expect_true(all(c("table", "fk", "parent", "parent_pk", "label") %in% names(j)))
  }
  expect_equal(INDIKATOR_JUNCTIONS$faggrupper$table, "tblForbindIndikatorerFaggrupper")
  expect_equal(INDIKATOR_JUNCTIONS$dataprodukter$fk, "dataprodukt_id")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-sql.R')"`
Expected: FAIL — `object 'INDIKATOR_JUNCTIONS' not found`

- [ ] **Step 3: Write minimal implementation**

Append til `R/metadata.R`:

```r
# --- m2m junction-metadata for tblIndikatorer --------------------------------
# Junctions har ingen PK (kun (indikator_id, parent_id)) → skrives via replace.
# label: SQL-udtryk for parent-tekstværdi (double-quoted idents / COALESCE).
INDIKATOR_JUNCTIONS <- list(
  faggrupper    = list(table = "tblForbindIndikatorerFaggrupper",
                       fk = "faggruppe_id",   parent = "tblFaggrupper",
                       parent_pk = "Id", label = '"faggruppe"'),
  dataprodukter = list(table = "tblForbindIndikatorerDataprodukter",
                       fk = "dataprodukt_id", parent = "tblDataprodukter",
                       parent_pk = "Id", label = '"dataprodukt_navn"'),
  organisation  = list(table = "tblForbindIndikatorerOrganisation",
                       fk = "organisations_id", parent = "tblOrganisationStruktur",
                       parent_pk = "Id",
                       label = 'COALESCE("organisatorisk_navn_langt","organisatorisk_navn_teknisk")')
)
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-sql.R')"`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/metadata.R tests/testthat/test-sql.R
git commit -m "feat(meta): INDIKATOR_JUNCTIONS m2m-metadata"
```

---

## Task 2: Junction SQL-byggere

**Files:**
- Modify: `R/fct_sql.R` (append)
- Test: `tests/testthat/test-sql.R`

- [ ] **Step 1: Write the failing test**

Append til `tests/testthat/test-sql.R`:

```r
test_that("junction-byggere bygger parametriseret SQL", {
  j <- INDIKATOR_JUNCTIONS$faggrupper
  expect_match(build_junction_select_sql(j),
    'SELECT "faggruppe_id" FROM "tblForbindIndikatorerFaggrupper" WHERE "indikator_id" = \\$1')
  expect_match(build_junction_delete_sql(j),
    'DELETE FROM "tblForbindIndikatorerFaggrupper" WHERE "indikator_id" = \\$1')
  # 2 parent-ids → $1 (indikator) genbrugt, $2+$3 = parents
  ins <- build_junction_insert_sql(j, 2)
  expect_match(ins, 'INSERT INTO "tblForbindIndikatorerFaggrupper" \\("indikator_id", "faggruppe_id"\\)')
  expect_match(ins, 'VALUES \\(\\$1, \\$2\\), \\(\\$1, \\$3\\)')
  opt <- build_junction_options_sql(j)
  expect_match(opt, '"Id" AS id')
  expect_match(opt, 'FROM "tblFaggrupper"')
})

test_that("organisation-options bruger COALESCE-label", {
  expect_match(build_junction_options_sql(INDIKATOR_JUNCTIONS$organisation),
    "COALESCE")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-sql.R')"`
Expected: FAIL — `could not find function "build_junction_select_sql"`

- [ ] **Step 3: Write minimal implementation**

Append til `R/fct_sql.R`:

```r
#' SELECT parent-ids for én indikators junction-rækker
#' @noRd
build_junction_select_sql <- function(j) {
  sprintf('SELECT "%s" FROM "%s" WHERE "indikator_id" = $1', j$fk, j$table)
}

#' DELETE alle junction-rækker for én indikator
#' @noRd
build_junction_delete_sql <- function(j) {
  sprintf('DELETE FROM "%s" WHERE "indikator_id" = $1', j$table)
}

#' Multi-row INSERT: $1 = indikator_id (genbrugt), $2..$(n+1) = parent-ids
#' @noRd
build_junction_insert_sql <- function(j, n) {
  vals <- vapply(seq_len(n), function(i) sprintf("($1, $%d)", i + 1), "")
  sprintf('INSERT INTO "%s" ("indikator_id", "%s") VALUES %s',
          j$table, j$fk, paste(vals, collapse = ", "))
}

#' id + tekst-label for m2m-multiselect
#' @noRd
build_junction_options_sql <- function(j) {
  sprintf('SELECT "%s" AS id, (%s) AS label FROM "%s" ORDER BY 2',
          j$parent_pk, j$label, j$parent)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-sql.R')"`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/fct_sql.R tests/testthat/test-sql.R
git commit -m "feat(sql): junction select/delete/insert/options-byggere"
```

---

## Task 3: DB-accessors (get/options/set_junction)

**Files:**
- Modify: `R/fct_db.R` (`make_db()` list)
- Create: `tests/testthat/test-db-junction.R` (gated integration)

- [ ] **Step 1: Write the failing test (gated integration)**

Create `tests/testthat/test-db-junction.R`:

```r
# Integration: kræver rigtig Supabase + skrivning aktiveret. Skippes ellers.
skip_if_no_db <- function() {
  testthat::skip_if_not(identical(Sys.getenv("BFHMETA_WRITE"), "1"),
                        "BFHMETA_WRITE!=1 — springer DB-integration over")
}

test_that("set_junction replace-roundtrip + tom selektion + rollback", {
  skip_if_no_db()
  pool <- db_connect(); on.exit(pool::poolClose(pool))
  db <- make_db(pool)
  # Vælg en eksisterende indikator-id (mindste aktive)
  id <- DBI::dbGetQuery(pool, 'SELECT MIN("id") AS id FROM "tblIndikatorer"')$id[1]
  before <- db$get_junction(id, "faggrupper")
  on.exit(db$set_junction(id, "faggrupper", before), add = TRUE)  # gendan

  opts <- db$junction_options("faggrupper")$id
  pick <- head(opts, 2)
  db$set_junction(id, "faggrupper", pick)
  expect_setequal(db$get_junction(id, "faggrupper"), pick)

  # Tom selektion → kun delete
  db$set_junction(id, "faggrupper", integer(0))
  expect_length(db$get_junction(id, "faggrupper"), 0)

  # Rollback: ugyldig parent-id (FK-violation) → ingen ændring
  db$set_junction(id, "faggrupper", pick)            # sæt kendt udgangspunkt
  expect_error(db$set_junction(id, "faggrupper", c(pick[1], -999999L)))
  expect_setequal(db$get_junction(id, "faggrupper"), pick)  # uændret efter rollback
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `BFHMETA_WRITE=1 Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-db-junction.R')"`
Expected: FAIL — `attempt to apply non-function` / `$ operator` (accessors findes ikke endnu). (Uden `BFHMETA_WRITE=1`: SKIP.)

- [ ] **Step 3: Write minimal implementation**

I `R/fct_db.R`, tilføj til listen i `make_db(pool)` (efter `soft_delete`):

```r
    get_junction = function(indikator_id, key) {
      j <- INDIKATOR_JUNCTIONS[[key]]
      res <- DBI::dbGetQuery(pool, build_junction_select_sql(j),
                             params = list(indikator_id))
      res[[j$fk]]
    },
    junction_options = function(key) {
      j <- INDIKATOR_JUNCTIONS[[key]]
      DBI::dbGetQuery(pool, build_junction_options_sql(j))
    },
    set_junction = function(indikator_id, key, parent_ids) {
      assert_write_enabled()
      j <- INDIKATOR_JUNCTIONS[[key]]
      parent_ids <- parent_ids[!is.na(parent_ids)]
      pool::poolWithTransaction(pool, function(conn) {
        DBI::dbExecute(conn, build_junction_delete_sql(j),
                       params = list(indikator_id))
        if (length(parent_ids)) {
          DBI::dbExecute(conn, build_junction_insert_sql(j, length(parent_ids)),
                         params = c(list(indikator_id), as.list(parent_ids)))
        }
      })
    }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `BFHMETA_WRITE=1 Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-db-junction.R')"`
Expected: PASS (eller SKIP uden env). Bekræft at den **kører** (ikke kun skipper) ved at sætte env.

- [ ] **Step 5: Commit**

```bash
git add R/fct_db.R tests/testthat/test-db-junction.R
git commit -m "feat(db): get/options/set_junction accessors med atomisk replace"
```

---

## Task 4: Udvid `.field_input`/`.collect_form` med prefix + values

**Files:**
- Modify: `R/mod_indikator_crud.R:1-49`
- Test: `tests/testthat/test-mod-crud.R`

> Modal-inputs skal have distinkt id-prefix (`m_`) for ikke at kollidere med
> sidebar-formens `ns(f$col)`-inputs. `values` pre-udfylder felter ved åbning.

- [ ] **Step 1: Write the failing test**

Append til `tests/testthat/test-mod-crud.R`:

```r
test_that(".collect_form med prefix læser præfiksede inputs", {
  fields <- list(list(col = "indikator_navn", kind = "text"),
                 list(col = "aktiv_indikator", kind = "bool"))
  input <- list(m_indikator_navn = "Test", m_aktiv_indikator = TRUE)
  vals <- .collect_form(input, fields, prefix = "m_")
  expect_equal(vals$indikator_navn, "Test")
  expect_true(vals$aktiv_indikator)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-mod-crud.R')"`
Expected: FAIL — `unused argument (prefix = "m_")`

- [ ] **Step 3: Write minimal implementation**

Erstat `.field_input` (linje 1-14) og `.collect_form` (linje 38-49) i `R/mod_indikator_crud.R`:

```r
#' Bygger ét form-input baseret på felt-kind. prefix giver distinkt id-rum
#' (modal vs sidebar). values pre-udfylder.
#' @noRd
.field_input <- function(ns, f, fk_choices = list(), values = list(), prefix = "") {
  id <- ns(paste0(prefix, f$col))
  v <- values[[f$col]]
  switch(f$kind,
    "pk"       = NULL,
    "fk"       = selectInput(id, f$col, choices = c("(ingen)" = "", fk_choices[[f$col]]),
                             selected = v %||% ""),
    "bool"     = checkboxInput(id, f$col, value = isTRUE(v)),
    "date"     = dateInput(id, f$col,
                           value = if (is.null(v) || is.na(v)) NULL else as.Date(v)),
    "int"      = numericInput(id, f$col, value = if (is.null(v)) NA else v),
    "textarea" = textAreaInput(id, f$col, value = v %||% ""),
    textInput(id, f$col, value = v %||% "")  # text (default)
  )
}
```

```r
#' @noRd
.collect_form <- function(input, fields, prefix = "") {
  vals <- list()
  for (f in fields) {
    if (f$kind == "pk") next
    v <- input[[paste0(prefix, f$col)]]
    if (f$kind == "bool") v <- isTRUE(v)
    if (f$kind %in% c("text", "textarea", "fk") && identical(v, "")) v <- NA
    vals[[f$col]] <- v
  }
  vals
}
```

> Bemærk: eksisterende sidebar-kald bruger default `prefix = ""` → uændret adfærd.
> `%||%` er allerede defineret i `R/utils_validation.R`.

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-mod-crud.R')"`
Expected: PASS (inkl. alle eksisterende mod-crud-tests)

- [ ] **Step 5: Commit**

```bash
git add R/mod_indikator_crud.R tests/testthat/test-mod-crud.R
git commit -m "feat(mod): .field_input/.collect_form med prefix + pre-udfyldning"
```

---

## Task 5: Modal-flow (åbn-observer + modal + gem med m2m)

**Files:**
- Modify: `R/mod_indikator_crud.R` (`mod_indikator_crud_server`)
- Test: `tests/testthat/test-mod-crud.R`

- [ ] **Step 1: Write the failing test**

Udvid `fake_db()` i `tests/testthat/test-mod-crud.R` (tilføj felter til `store` + junction-stubs). Erstat `store`-linjen og tilføj junction-accessors:

```r
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
    .calls = function() calls
  )
}
```

Tilføj nye tests:

```r
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-mod-crud.R')"`
Expected: FAIL — `editing_id` not found / modal-observer mangler

- [ ] **Step 3: Write minimal implementation**

I `mod_indikator_crud_server` (`R/mod_indikator_crud.R`), tilføj inde i `moduleServer`-funktionen, efter `reload <- function() ...`:

```r
    editing_id <- reactiveVal(NULL)

    # Bygger modal-indhold for én indikator (pre-udfyldt form + m2m-multiselect)
    .build_modal <- function(row) {
      ns <- session$ns
      vals <- as.list(row)
      scalar_fk <- tagList(lapply(INDIKATOR_FIELDS, function(f)
        .field_input(ns, f, fk_choices, values = vals, prefix = "m_")))
      m2m <- lapply(names(INDIKATOR_JUNCTIONS), function(key) {
        opts <- db$junction_options(key)
        sel <- db$get_junction(vals$id, key)
        selectInput(ns(paste0("m_j_", key)), key,
          choices = stats::setNames(opts$id, opts$label),
          selected = sel, multiple = TRUE)
      })
      modalDialog(title = paste("Redigér indikator", vals$id), size = "l",
        easyClose = FALSE,
        scalar_fk, hr(), h5("Relationer"), tagList(m2m),
        footer = tagList(
          actionButton(ns("modal_save"), "Gem", class = "btn-primary"),
          modalButton("Annullér")))
    }

    observeEvent(input$open_id, {
      rid <- as.integer(input$open_id)
      editing_id(rid)
      row <- rows()[rows()[["id"]] == rid, , drop = FALSE]
      if (nrow(row) == 0) { status_msg("Indikator ikke fundet"); return() }
      showModal(.build_modal(row[1, , drop = FALSE]))
    })

    observeEvent(input$modal_save, {
      rid <- editing_id()
      vals <- .collect_form(input, INDIKATOR_FIELDS, prefix = "m_")
      errs <- validate_indikator(vals)
      if (length(errs) > 0) { status_msg(paste(errs, collapse = "; ")); return() }
      safe_operation("modal-gem", {
        db$update_indikator(rid, vals)
        for (key in names(INDIKATOR_JUNCTIONS)) {
          picked <- as.integer(input[[paste0("m_j_", key)]])
          db$set_junction(rid, key, picked)
        }
        removeModal(); status_msg(paste("Gemt indikator", rid)); reload()
      }, fallback = status_msg("Fejl ved modal-gem (se log)"))
    })
```

Tilføj `editing_id` til retur-listen (nederst i serveren):

```r
    list(rows = rows, status_msg = status_msg, editing_id = editing_id)
```

> `as.integer(NULL)` → `integer(0)`, og `as.integer(character(0))` → `integer(0)`,
> så tom multiselect giver korrekt tom selektion til `set_junction`.

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-mod-crud.R')"`
Expected: PASS (alle, inkl. eksisterende)

- [ ] **Step 5: Commit**

```bash
git add R/mod_indikator_crud.R tests/testthat/test-mod-crud.R
git commit -m "feat(mod): modal-redigering med m2m-relationer (åbn + gem)"
```

---

## Task 6: UI — navset_tab + oversigtstabel med [Åbn]-knap

**Files:**
- Modify: `R/mod_indikator_crud.R` (`mod_indikator_crud_ui` + `output$oversigt`-render)
- Test: manuel (UI — ingen testServer for renderDT-HTML)

- [ ] **Step 1: Erstat `mod_indikator_crud_ui`**

Erstat hele `mod_indikator_crud_ui`-funktionen i `R/mod_indikator_crud.R`:

```r
#' @noRd
mod_indikator_crud_ui <- function(id) {
  ns <- NS(id)
  bslib::navset_tab(
    bslib::nav_panel("Oversigt",
      div(class = "mt-2",
        checkboxInput(ns("show_inactive_ov"), "Vis inaktive", value = TRUE),
        DT::DTOutput(ns("oversigt")),
        verbatimTextOutput(ns("status"))
      )
    ),
    bslib::nav_panel("Inline-redigering",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 420, position = "right", open = TRUE,
          h5("Redigér / opret"),
          uiOutput(ns("form")),
          div(class = "d-flex gap-2 mt-2",
            actionButton(ns("new"), "Ny", class = "btn-secondary"),
            actionButton(ns("save"), "Gem", class = "btn-primary"),
            actionButton(ns("soft_delete"), "Deaktivér", class = "btn-warning")
          )
        ),
        div(
          checkboxInput(ns("show_inactive"), "Vis inaktive", value = TRUE),
          DT::DTOutput(ns("tbl"))
        )
      )
    )
  )
}
```

> `status` flyttes til Oversigt-fanen (modal-flow rapporterer der). Den eksisterende
> `verbatimTextOutput(ns("status"))` i sidebaren fjernes (kun ét output-id tilladt).

- [ ] **Step 2: Tilføj oversigt-render i serveren**

I `mod_indikator_crud_server`, tilføj (fx efter `output$tbl`-render):

```r
    output$oversigt <- DT::renderDT({
      ns <- session$ns
      d <- rows()
      if (!isTRUE(input$show_inactive_ov) && "aktiv_indikator" %in% names(d))
        d <- d[d$aktiv_indikator %in% TRUE, , drop = FALSE]
      btn <- vapply(d[["id"]], function(i) sprintf(
        '<button class="btn btn-sm btn-primary" onclick="Shiny.setInputValue(\'%s\', %d, {priority:\'event\'})">Åbn</button>',
        ns("open_id"), i), "")
      out <- data.frame(
        Aktiv = ifelse(d[["aktiv_indikator"]] %in% TRUE, "✓", "✗"),
        Datasæt = d[["label_indikator_hierarki"]],
        Id = d[["id"]],
        Navn = d[["indikator_navn"]],
        Handling = btn,
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(out, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(pageLength = 25))
    })
```

- [ ] **Step 3: Verificér tests stadig grønne**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_dir('tests/testthat')"`
Expected: PASS (alle). UI-ændring bryder ikke testServer.

- [ ] **Step 4: Manuel smoke**

Run: `Rscript dev/run_dev.R` → browser på http://127.0.0.1:3838
Verificér:
- Fane "Oversigt" viser kolonner Aktiv / Datasæt / Id / Navn / Handling
- [Åbn]-knap åbner modal med pre-udfyldte felter + 3 relations-multiselect (tekst-labels)
- Fane "Inline-redigering" uændret

For skrive-test: `BFHMETA_WRITE=1 Rscript dev/run_dev.R`, redigér i modal → Gem → verificér opdatering i begge faner. Ctrl+C for at stoppe.

- [ ] **Step 5: Commit**

```bash
git add R/mod_indikator_crud.R
git commit -m "feat(mod): navset_tab + kompakt oversigt med åbn-knap"
```

---

## Task 7: Version-bump + NEWS

**Files:**
- Modify: `DESCRIPTION`, `NEWS.md`

- [ ] **Step 1: Bump DESCRIPTION**

Sæt `Version: 0.2.0` i `DESCRIPTION`.

- [ ] **Step 2: NEWS-entry**

Prepend til `NEWS.md`:

```markdown
# BFHmetadata 0.2.0

## Nye features
* Kompakt oversigtstabel over indikatorer (aktiv-status, hierarki-placering,
  id, navn) med per-række åbn-knap.
* Modal-redigering: fuld adgang til alle felter, direkte FK-relationer og
  many-to-many-relationer (faggrupper, dataprodukter, organisation) vist med
  tekst-værdier i stedet for rå id'er.
* Two-fane-layout adskiller kompakt oversigt fra inline-redigering.

## Interne ændringer
* M2m-relationer skrives atomisk via replace-strategi i poolWithTransaction.
* Nye rene SQL-byggere for junction-tabeller (unit-testet).
```

- [ ] **Step 3: Fuld test-suite**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_dir('tests/testthat')"`
Expected: PASS (DB-integration skipper uden BFHMETA_WRITE=1).

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION NEWS.md
git commit -m "chore(release): bump til 0.2.0 + NEWS"
```

---

## Self-Review-noter (udført ved plan-skrivning)

- **Spec-dækning:** Oversigtskolonner (Task 6), åbn-knap (Task 6), modal fuld
  redigering (Task 5), m2m tekst-labels (Task 5/6), replace-atomicitet (Task 3),
  tests (Task 1-5) — alle dækket.
- **Type-konsistens:** `set_junction(indikator_id, key, parent_ids)`,
  `get_junction(indikator_id, key)`, `junction_options(key)` ens i db (Task 3),
  fake_db (Task 5) og kald (Task 5). Modal-input-ids: `m_<col>` (skalar/FK),
  `m_j_<key>` (m2m) — konsistente på tværs af Task 4/5.
- **Placeholders:** ingen — alle steps har komplet kode/kommando.
- **Rollback-test** kræver rigtig DB → gated (skip uden BFHMETA_WRITE=1); skal
  køres mindst én gang manuelt med env sat for at validere replace-atomicitet.
```
