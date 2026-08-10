# Fuld CRUD — Fase A (oversættelse) + Fase B (diagrammer): Implementeringsplan

> **For Claude:** REQUIRED SUB-SKILL: Brug superpowers:executing-plans til at
> implementere denne plan task-for-task.

**Mål:** Gør `tblOrganisationOversaettelse` redigerbar via eksisterende
lookup-mønster (Fase A, →0.6.0) og byg diagram-CRUD med filterbar oversigt +
delt formular kaldt fra både oversigt og indikator-modal (Fase B, →0.7.0).

**Arkitektur:** Fase A er ren config (`LOOKUP_TABLES`-entry — nul ny
modulkode). Fase B: nye rene SQL-byggere i `fct_sql.R`, nye accessors i
`make_db()` (`fct_db.R`), nyt `mod_diagram.R` med delt formular-UI-funktion,
integration i `mod_indikator_crud.R` via modal-swap-med-retur.

**Tech stack:** R/Shiny (Golem-stil), bslib, DT, pool+RPostgres mod Supabase,
testthat 3. Design: `docs/superpowers/specs/2026-08-10-fuld-crud-design.md`.

**Miljø (Windows):**
- Rscript: `"C:\Program Files\R\R-4.6.0\bin\Rscript.exe"` (fuld sti — ej PATH)
- Alle test-kommandoer køres fra projektroden
- DB-writes i tests er gated: kræver `BFHMETA_WRITE=1` + `SUPABASE_DB_PASSWORD`
  i `.Renviron` (findes lokalt). Uden env: testene skipper — det er OK i CI.
- Git Bash kan tabe V:-drevet (set 2026-08-10) → kør git-kommandoer i
  PowerShell hvis Bash fejler uden output.

**Kommando-alias brugt nedenfor:**
```
RS = & "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" --encoding=UTF-8
```

---

## Fase A — tblOrganisationOversaettelse via LOOKUP_TABLES (→ 0.6.0)

Branch: `git checkout -b feat/crud-fase-a docs/fuld-crud-design`

### Task A1: Config-entry + config-validerende test

**Files:**
- Modify: `R/metadata.R` (LOOKUP_TABLES, efter `personer`-entry ~linje 163)
- Test: `tests/testthat/test-metadata.R` (ny fil)

**Step 1: Skriv fejlende test**

`tests/testthat/test-metadata.R`:
```r
# Validerer LOOKUP_TABLES-config mod forventet skema (fanger tastefejl i
# tabel-/kolonnenavne før de rammer DB).

test_that("org_oversaettelse-entry findes med korrekte kolonner", {
  ids <- vapply(LOOKUP_TABLES, function(x) x$id, "")
  expect_true("org_oversaettelse" %in% ids)
  cfg <- LOOKUP_TABLES[[which(ids == "org_oversaettelse")]]
  expect_identical(cfg$table, "tblOrganisationOversaettelse")
  expect_identical(cfg$pk, "Id")
  cols <- vapply(cfg$cols, function(c) c$col, "")
  expect_setequal(cols, c("organisatorisk_navn_fra_data",
                          "organisatorisk_navn_teknisk"))
  fk <- Filter(function(c) identical(c$type, "fk"), cfg$cols)[[1]]
  expect_identical(fk$parent, "tblOrganisationStruktur")
  expect_identical(fk$parent_pk, "Id")
  expect_true(grepl("organisatorisk_navn_langt", fk$label_expr))
})

test_that("alle LOOKUP_TABLES-entries har paakraevede felter", {
  for (cfg in LOOKUP_TABLES) {
    expect_true(all(c("id", "table", "pk", "label", "cols") %in% names(cfg)),
                info = cfg$id)
    for (c in cfg$cols) {
      expect_true(all(c("col", "type", "label") %in% names(c)),
                  info = paste(cfg$id, c$col))
      if (identical(c$type, "fk")) {
        expect_true(all(c("parent", "parent_pk", "label_expr") %in% names(c)),
                    info = paste(cfg$id, c$col))
      }
    }
  }
})
```

**Step 2: Kør — forvent FAIL**

Run: `RS -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-metadata.R')"`
Expected: FAIL — `"org_oversaettelse" %in% ids is not TRUE`

**Step 3: Minimal implementering**

I `R/metadata.R`, tilføj som SIDSTE element i `LOOKUP_TABLES` (efter
`personer`-entry, husk komma efter den foregående):
```r
  ,
  # Oversættelse: navn-fra-data → organisatorisk enhed. Bruges af signal-scan
  # (enhed-varianter); sletning ufarlig (intet refererer til rækkerne).
  list(id = "org_oversaettelse", table = "tblOrganisationOversaettelse",
       pk = "Id", label = "Organisations-oversættelse",
       cols = list(
         list(col = "organisatorisk_navn_fra_data", type = "text",
              label = "Navn fra data"),
         list(col = "organisatorisk_navn_teknisk", type = "fk",
              label = "Organisatorisk enhed",
              parent = "tblOrganisationStruktur", parent_pk = "Id",
              label_expr = 'COALESCE("organisatorisk_navn_langt","organisatorisk_navn_teknisk")')))
```

**Step 4: Kør — forvent PASS**

Run: samme som Step 2. Expected: PASS (2 tests).

**Step 5: Fuldt testsuite + commit**

Run: `RS -e "pkgload::load_all('.'); testthat::test_dir('tests/testthat')"`
Expected: alle grønne (202+ PASS, gated skips OK).

```bash
git add R/metadata.R tests/testthat/test-metadata.R
git commit -m "feat(lookup): org-oversaettelse som opslagstabel (config-drevet)"
```

### Task A2: Manuel røgtest + version-bump 0.6.0

**Files:**
- Modify: `DESCRIPTION` (Version: 0.5.0 → 0.6.0)
- Modify: `NEWS.md` (ny sektion øverst)

**Step 1: Manuel røgtest** — start appen:
`RS dev/run_dev.R` → landing viser ny flise "Organisations-oversættelse" under
Opslagstabeller → åbn → 443 rækker med FK-dropdown (org-navne) → redigér én
celle (kræver `BFHMETA_WRITE=1`), verificér notifikation + persistens ved
refresh. (Flisen flyttes til "Organisation"-sektion i Fase C — bevidst udskudt.)

**Step 2: Bump + NEWS**

`DESCRIPTION`: `Version: 0.6.0`

`NEWS.md`, øverst:
```markdown
# BFHmetadata 0.6.0

## Nye features
* Organisations-oversættelse (tblOrganisationOversaettelse) kan nu redigeres
  i appen som opslagstabel: navn-fra-data + organisatorisk enhed (dropdown).
  Fuld CRUD-plan: se docs/superpowers/specs/2026-08-10-fuld-crud-design.md.
```

**Step 3: Commit**

```bash
git add DESCRIPTION NEWS.md
git commit -m "chore(release): bump 0.6.0 (Fase A komplet)"
```

**Step 4:** Draft-PR mod `main` (browser — ingen gh CLI). STOP: afvent bruger-
merge før Fase B-branch.

---

## Fase B — Diagram-CRUD (→ 0.7.0)

Branch (efter A merged): `git pull` på main → `git checkout -b feat/crud-fase-b main`

### Task B1: SQL-byggere (rene funktioner)

**Files:**
- Modify: `R/fct_sql.R` (tilføj sektion nederst)
- Test: `tests/testthat/test-sql.R` (tilføj tests nederst)

**Step 1: Skriv fejlende tests** (mønster: eksisterende test-sql.R — assertions
på SQL-streng-indhold):

```r
# --- Diagram-CRUD (Fase B) ---------------------------------------------------

test_that("build_diagram_admin_sql joiner labels og har intet aktiv-filter", {
  sql <- build_diagram_admin_sql()
  expect_match(sql, 'FROM "tblDiagrammer" d', fixed = TRUE)
  expect_match(sql, '"tblIndikatorer"', fixed = TRUE)
  expect_match(sql, '"tblOrganisationStruktur"', fixed = TRUE)
  expect_match(sql, '"tblDiagramTyper"', fixed = TRUE)
  expect_match(sql, '"periode_aggregering"', fixed = TRUE)
  expect_no_match(sql, "diagram_aktivt\\s*(=|AND)")  # admin ser ALT
  expect_no_match(sql, 'WHERE d\\."diagram_type"')
})

test_that("build_diagram_insert_sql parametriserer alle kolonner + RETURNING", {
  sql <- build_diagram_insert_sql()
  for (col in DIAGRAM_COLS) expect_match(sql, sprintf('"%s"', col), fixed = TRUE)
  expect_match(sql, "\\$6")           # 6 kolonner → $1..$6
  expect_match(sql, 'RETURNING "id"', fixed = TRUE)
})

test_that("build_diagram_update_sql saetter alle kolonner, id sidst", {
  sql <- build_diagram_update_sql()
  expect_match(sql, 'UPDATE "tblDiagrammer" SET', fixed = TRUE)
  expect_match(sql, '"id" = \\$7')    # 6 kolonner + id
})

test_that("build_diagram_delete_sql og duplicate/periode-byggere", {
  expect_identical(build_diagram_delete_sql(),
                   'DELETE FROM "tblDiagrammer" WHERE "id" = $1')
  dup <- build_diagram_duplicate_sql()
  expect_match(dup, '"indikator" = \\$1')
  expect_match(dup, '"organisatorisk_navn_teknisk" = \\$2')
  expect_match(dup, '"diagram_type" = \\$3')
  expect_match(dup, '"id" <> \\$4')   # ekskludér egen række ved update
  per <- build_diagram_periode_sql()
  expect_match(per, "DISTINCT", fixed = TRUE)
  expect_match(per, "IS NOT NULL", fixed = TRUE)
  cnt <- build_median_count_sql()
  expect_match(cnt, 'FROM "tblDiagrammerMedian" WHERE "diagram" = \\$1')
})
```

**Step 2: Kør — forvent FAIL** (`could not find function build_diagram_admin_sql`)

Run: `RS -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-sql.R')"`

**Step 3: Implementér** i `R/fct_sql.R`, nederst:

```r
# --- Diagram-CRUD (admin) ----------------------------------------------------
# Kolonner der redigeres i diagram-formularen (rækkefølge = parameter-orden).
DIAGRAM_COLS <- c("indikator", "organisatorisk_navn_teknisk", "diagram_type",
                  "periode_aggregering", "indgaar_i_aggregering",
                  "diagram_aktivt", "direktionens_tavle")
# OBS: 7 kolonner → insert $1..$7, update-id $8. Justér tests tilsvarende.

#' Alle diagrammer med resolvede labels — INGEN aktiv/type-filtre (admin).
#' @noRd
build_diagram_admin_sql <- function() {
  paste0(
    'SELECT d."id" AS diagram_id, d."indikator", ',
    'd."organisatorisk_navn_teknisk", d."diagram_type", ',
    'd."periode_aggregering", d."indgaar_i_aggregering", ',
    'd."diagram_aktivt", d."direktionens_tavle", ',
    'i."indikator_navn", ',
    'COALESCE(o."organisatorisk_navn_langt", o."organisatorisk_navn_teknisk") ',
    'AS org_navn, ',
    't."diagram_type" AS type_navn ',
    'FROM "tblDiagrammer" d ',
    'LEFT JOIN "tblIndikatorer" i ON i."id" = d."indikator" ',
    'LEFT JOIN "tblOrganisationStruktur" o ',
    'ON o."Id" = d."organisatorisk_navn_teknisk" ',
    'LEFT JOIN "tblDiagramTyper" t ON t."Id" = d."diagram_type" ',
    'ORDER BY i."indikator_navn", org_navn')
}

#' @noRd
build_diagram_insert_sql <- function() {
  ph <- paste(sprintf("$%d", seq_along(DIAGRAM_COLS)), collapse = ", ")
  qcols <- paste(sprintf('"%s"', DIAGRAM_COLS), collapse = ", ")
  sprintf('INSERT INTO "tblDiagrammer" (%s) VALUES (%s) RETURNING "id"',
          qcols, ph)
}

#' @noRd
build_diagram_update_sql <- function() {
  sets <- vapply(seq_along(DIAGRAM_COLS),
                 function(i) sprintf('"%s" = $%d', DIAGRAM_COLS[i], i), "")
  sprintf('UPDATE "tblDiagrammer" SET %s WHERE "id" = $%d',
          paste(sets, collapse = ", "), length(DIAGRAM_COLS) + 1)
}

#' @noRd
build_diagram_delete_sql <- function() {
  'DELETE FROM "tblDiagrammer" WHERE "id" = $1'
}

#' Blød duplikat-guard: findes (indikator, org, type) allerede (undtagen id)?
#' @noRd
build_diagram_duplicate_sql <- function() {
  paste0('SELECT count(*) AS n FROM "tblDiagrammer" WHERE "indikator" = $1 ',
         'AND "organisatorisk_navn_teknisk" = $2 AND "diagram_type" = $3 ',
         'AND "id" <> $4')
}

#' Distinkte periode-værdier (choices til select — robust ved nye værdier)
#' @noRd
build_diagram_periode_sql <- function() {
  paste0('SELECT DISTINCT "periode_aggregering" FROM "tblDiagrammer" ',
         'WHERE "periode_aggregering" IS NOT NULL ORDER BY 1')
}

#' Antal median-knæk for ét diagram (pre-check før slet → venlig besked)
#' @noRd
build_median_count_sql <- function() {
  'SELECT count(*) AS n FROM "tblDiagrammerMedian" WHERE "diagram" = $1'
}
```

**VIGTIGT:** `DIAGRAM_COLS` har 7 kolonner — ret test-forventningerne fra
Step 1 til `$7`/`$8` (testen ovenfor skrev 6; koden er sandheden, opdatér
testen ved Step 2-kørslen når den præcise fejl ses).

**Step 4: Kør — forvent PASS.** **Step 5: Commit**
`git commit -m "feat(diagram): rene SQL-byggere for diagram-CRUD (admin-liste, insert/update/delete, guards)"`

### Task B2: db-accessors i make_db()

**Files:**
- Modify: `R/fct_db.R` (i `make_db()`-listen, efter `org_enhed_variants`)
- Test: `tests/testthat/test-db-diagram.R` (ny, gated — mønster: test-db-junction.R)

**Step 1: Skriv gated test** (kopier skip-mønster fra `test-db-junction.R`:
skip hvis `Sys.getenv("BFHMETA_WRITE") != "1"` eller intet password; opret
pool i `withr::defer(poolClose)`). Testen: opret diagram mod KENDT indikator/
org/type (læs første id'er fra DB), verificér RETURNING-id, opdatér
`periode_aggregering`, læs tilbage, tæl medianer (0), slet, verificér væk.

**Step 2: FAIL** (`db$create_diagram` findes ikke) → **Step 3: Implementér**:

```r
    # --- Diagram-CRUD (admin) --------------------------------------------
    list_diagrams_admin = function() {
      DBI::dbGetQuery(pool, build_diagram_admin_sql())
    },
    diagram_periode_choices = function() {
      DBI::dbGetQuery(pool, build_diagram_periode_sql())[[1]]
    },
    diagram_duplicate_count = function(indikator, org, type, exclude_id = -1L) {
      as.integer(DBI::dbGetQuery(pool, build_diagram_duplicate_sql(),
        params = list(indikator, org, type, exclude_id))$n[1])
    },
    diagram_median_count = function(diagram_id) {
      as.integer(DBI::dbGetQuery(pool, build_median_count_sql(),
        params = list(diagram_id))$n[1])
    },
    create_diagram = function(values) {   # values: named list i DIAGRAM_COLS-orden
      assert_write_enabled()
      DBI::dbGetQuery(pool, build_diagram_insert_sql(),
        params = unname(values[DIAGRAM_COLS]))$id[1]
    },
    update_diagram = function(id, values) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_diagram_update_sql(),
        params = c(unname(values[DIAGRAM_COLS]), list(id)))
    },
    delete_diagram = function(id) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_diagram_delete_sql(), params = list(id))
    },
```

**Step 4:** `BFHMETA_WRITE=1 RS -e "...test_file('tests/testthat/test-db-diagram.R')"` → PASS.
Kør også fuld suite uden env → de nye tests SKIPPER (verificér).
**Step 5: Commit** `feat(diagram): db-accessors (list/create/update/delete + guards)`

### Task B3: Validering

**Files:**
- Modify: `R/utils_validation.R`
- Test: `tests/testthat/test-validation.R` (tilføj)

**Step 1: Test:** `validate_diagram(vals)` → character(0) ved gyldigt input;
fejl ved manglende indikator/org/type; accepterer NA-periode.
**Step 3: Implementér:**
```r
#' Valider diagram-form-værdier. Returnerer character() hvis OK.
#' @noRd
validate_diagram <- function(vals) {
  errs <- character(0)
  if (is.na(vals$indikator)) errs <- c(errs, "Indikator er obligatorisk")
  if (is.na(vals$organisatorisk_navn_teknisk))
    errs <- c(errs, "Organisatorisk enhed er obligatorisk")
  if (is.na(vals$diagram_type)) errs <- c(errs, "Diagramtype er obligatorisk")
  errs
}
```
**Step 5: Commit** `feat(diagram): validate_diagram (obligatoriske FK-felter)`

### Task B4: mod_diagram — oversigt + formular

**Files:**
- Create: `R/mod_diagram.R`
- Test: `tests/testthat/test-mod-diagram.R` (testServer, fake-db-mønster fra
  test-mod-crud.R: db = list af closures der logger kald)

**Indhold (skitse — følg mod_indikator_crud.R-idiomer):**

1. `.diagram_form_ui(ns, vals, opts, lock_indikator = FALSE)` — DELT ren
   UI-funktion (bruges også af indikator-modalen i B5). `vals` = named list
   (NULL → ny), `opts` = list(indikator=df, org=df, type=df, periode=chr).
   Felter: selectize indikator (disabled hvis `lock_indikator`), selectize
   org, select type, select periode (choices = opts$periode, tom tilladt),
   3 × checkboxInput. Returnerer tagList — IKKE modalDialog (kalderen wrapper).
2. `.collect_diagram_form(input, prefix = "d_")` — named list i
   DIAGRAM_COLS-orden, `as.integer`/`as.logical`-coercion, tomme → NA.
3. `mod_diagram_ui(id)`: 4 filtre (selectize indikator/org via choices fra
   admin-df; select status alle/aktive/inaktive DEFAULT "aktive"; select type)
   + "Nyt diagram"-knap + `DT::DTOutput`.
4. `mod_diagram_server(id, db)`:
   - `admin <- reactiveVal(db$list_diagrams_admin())`; filtrering i R
     (dplyr på 4.126 rækker er øjeblikkelig).
   - DT: kolonner id, indikator_navn, org_navn, type_navn, periode + 3 flag
     (vis som ✓/✗), Åbn-knap pr. række (samme onclick-setInputValue-mønster
     som mod_indikator_crud `input$open_id`).
   - Åbn/Ny → `showModal(modalDialog(.diagram_form_ui(...), footer = Annullér
     (modalButton) + Gem (actionButton d_save)))`.
   - Gem: `.collect_diagram_form` → `validate_diagram` → duplikat-tjek
     (`diagram_duplicate_count > 0` → `showNotification("Findes allerede …",
     type = "warning")` men fortsæt) → `safe_operation` create/update →
     `removeModal(); admin(db$list_diagrams_admin())`.
   - Slet-knap i modal (kun ved eksisterende): pre-check
     `diagram_median_count(id)` → hvis >0: notifikation *"Diagrammet har N
     median-knæk — deaktivér i stedet, eller slet knækkene først."* og INGEN
     delete. Ellers `safe_operation` delete + reload.

**testServer-tests (min. 4):** (1) admin-liste renderes + status-filter
default "aktive" reducerer rækker; (2) gem ny → fake-db `create_diagram`
kaldt med korrekte values; (3) duplikat → advarsel men gem gennemføres;
(4) slet med medianer>0 → `delete_diagram` IKKE kaldt.

**Commit pr. grønt delstep;** afsluttende:
`feat(diagram): mod_diagram (filterbar oversigt + formular-modal + slet-guard)`

### Task B5: Wiring (nav + landing) + indikator-modal-integration

**Files:**
- Modify: `R/app_ui.R` (nav_panel "diagrammer" + landing-sektion "Diagrammer")
- Modify: `R/app_server.R` (mod_diagram_server + go_diagrammer-observer)
- Modify: `R/mod_indikator_crud.R` (Diagrammer-sektion i modal + swap-retur)
- Test: udvid `tests/testthat/test-mod-crud.R` (swap-retur-flow)

**app_ui/app_server:** følg det eksisterende signal-mønster 1:1
(nav_panel value "diagrammer", landing-tile + `nav_select`-observer).

**Indikator-modal (mod_indikator_crud.R):**
1. I `.build_modal` `left`-tagList, EFTER relationer-blokken:
   `sect("Diagrammer")` + hvis `is_new`: muted tekst "Gem indikatoren først
   for at tilføje diagrammer"; ellers `uiOutput(ns("m_diagram_list"))` +
   `actionButton(ns("m_diagram_new"), "Nyt diagram", class = "btn-sm")`.
2. `output$m_diagram_list <- renderUI(...)`: kompakt tabel over
   `db$list_diagrams_admin()` filtreret på `editing_id()` (org_navn,
   type_navn, periode, aktiv-badge) med actionLink pr. række →
   `setInputValue("m_diagram_edit", diagram_id)`.
3. Swap-retur: `observeEvent(input$m_diagram_edit / m_diagram_new)`:
   gem `editing_id()` i `return_ind <- reactiveVal()`; `removeModal()`;
   vis diagram-formularmodal (`.diagram_form_ui(..., lock_indikator = TRUE)`,
   indikator forudfyldt). Footer: actionButton "Tilbage" (IKKE modalButton —
   skal trigge genåbning) + Gem.
4. Ved Gem/Tilbage: udfør evt. gem (som B4) → genåbn indikator-modal:
   `row <- rows()[rows()$id == return_ind(), , drop=FALSE];
   showModal(.build_modal(row))` + `return_ind(NULL)`.

**testServer:** simulér `m_diagram_edit` → verificér fake-db-kald + at
modal-state vender tilbage (via `session$returned`/reactive-inspektion som i
eksisterende modal-tests).

**Commit:** `feat(diagram): wiring + diagram-sektion i indikator-modal (swap-retur)`

### Task B6: Røgtest + release 0.7.0

1. Fuld suite: `RS -e "pkgload::load_all('.'); testthat::test_dir('tests/testthat')"`
   → alle grønne.
2. Manuel røgtest (`RS dev/run_dev.R` + `BFHMETA_WRITE=1`):
   - Oversigt: default viser ~614 aktive; filtre virker; åbn/redigér/gem.
   - Opret nyt diagram → vises i listen; slet det igen (0 medianer → OK).
   - Diagram MED medianer: slet-forsøg → venlig besked, intet slettet.
   - Indikator-modal → Diagrammer-sektion → redigér → retur til modal.
3. `DESCRIPTION` 0.7.0 + NEWS-entry (Nye features: diagram-CRUD, begge veje;
   slet-guard mod median-knæk).
4. Commit `chore(release): bump 0.7.0 (Fase B komplet)` → draft-PR → STOP.

---

## Udskudt til Fase C/D-plan (skrives efter B er merged)
- `mod_hierarchy` + `HIERARCHY_TABLES` (org-struktur, indikator-hierarki)
- Landing-sektion "Organisation" (flyt oversættelses-flisen dertil)
- `hierarchy_order`/`hierarchy_descendants` rene funktioner
- aktiv/kilde_id-håndtering i indikator-modalens hierarki-dropdown
