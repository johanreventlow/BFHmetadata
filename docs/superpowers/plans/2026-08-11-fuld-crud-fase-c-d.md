# Fuld CRUD — Fase C (org-struktur) + Fase D (indikator-hierarki): Implementeringsplan

> **For Claude:** REQUIRED SUB-SKILL: Brug superpowers:executing-plans til at
> implementere denne plan task-for-task.

**Mål:** Generisk, config-drevet hierarki-modul (`mod_hierarchy`) der gør
`tblOrganisationStruktur` (Fase C, →0.8.0) og `tblIndikatorHierarki` (Fase D,
→0.9.0) redigerbare: felter, opret/slet og flyt (re-parenting med
cyklus-værn). Plus landing-reorganisering ("Organisation"-sektion) og synlig
write-guard-indikator i navbaren.

**Arkitektur:** `HIERARCHY_TABLES`-config i `metadata.R` (mønster fra
`LOOKUP_TABLES`). Rene funktioner `hierarchy_order`/`hierarchy_descendants` i
ny `R/fct_hierarchy.R` (unit-testbare uden DB). Config-drevne SQL-byggere i
`fct_sql.R`, `make_hierarchy_db(pool, cfg)` i `fct_db.R` (mønster fra
`make_lookup_db`). Ét `mod_hierarchy` instantieret pr. config-entry.

**Tech stack:** R/Shiny (Golem-stil), bslib, DT, pool+RPostgres mod Supabase,
testthat 3. Design: `docs/superpowers/specs/2026-08-10-fuld-crud-design.md`.

**Miljø (Windows):**
- Rscript: `"C:\Program Files\R\R-4.6.0\bin\Rscript.exe"` (fuld sti — ej PATH)
- DB-write-tests gated: kræver `BFHMETA_WRITE=1` + `SUPABASE_DB_PASSWORD` i
  `.Renviron`. Uden env: skip — OK.
- Git Bash kan tabe V:-drevet → kør git i PowerShell hvis Bash fejler stumt.
- OBS: multi-line strings med `\\` via bash-heredoc kan miste backslashes —
  skriv testfiler med Write/Edit-tool, ikke `cat <<EOF` (set 2026-08-11).

**Kommando-alias:**
```
RS = & "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" --encoding=UTF-8
```

**Skema-fakta (verificeret mod migration/access_schema.yaml 2026-08-10):**

| | tblOrganisationStruktur | tblIndikatorHierarki |
|---|---|---|
| PK | `Id` | `Id` |
| Parent-kolonne | `parent_Id` (bemærk casing) | `parent_id` |
| Navnefelter | teknisk / langt / kort | `hierarki_navn`, `hierarki_navn_kort` |
| Øvrige felter | — | `beskrivelse_kort`, `beskrivelse_lang`, `kilde_id` |
| Niveau-FK | `organisatorisk_niveau` → `tblOrganisationNiveauer.Id` | `indikator_niveau` → `tblIndikatorNiveauer.Id` |
| Aktiv-kolonne | (ingen) | `aktiv` (7 FALSE — i brug) |
| Rækker / rødder | 362 / **2 rødder** | 165 / 1 rod |

Indgående FK'er mod org-noder: tblDiagrammer, tblPersoner,
tblOrganisationOversaettelse, tblForbindIndikatorerOrganisation, self-join.
Mod hierarki-noder: tblIndikatorer.indikator_hierarki, self-join. Ingen ON
DELETE-regler → slet af node i brug fejler loud (fanges → venlig besked).

---

## Fase C — mod_hierarchy + org-struktur (→ 0.8.0)

Branch: `git checkout -b feat/crud-fase-c main` (efter B er merged — done).

### Task C1: HIERARCHY_TABLES-config + config-test

**Files:**
- Modify: `R/metadata.R` (nederst, efter LOOKUP_TABLES)
- Test: `tests/testthat/test-metadata.R` (tilføj nederst)

**Step 1: Skriv fejlende test** (tilføj i test-metadata.R):

```r
# --- HIERARCHY_TABLES (Fase C/D) ---------------------------------------------

test_that("org_struktur-entry har korrekte kolonner og parent-casing", {
  cfg <- HIERARCHY_TABLES$org_struktur
  expect_identical(cfg$table, "tblOrganisationStruktur")
  expect_identical(cfg$pk, "Id")
  expect_identical(cfg$parent_col, "parent_Id")   # stort I — casing kritisk
  expect_identical(cfg$display_col, "organisatorisk_navn_langt")
  expect_null(cfg$aktiv_col)
  expect_identical(cfg$level$parent, "tblOrganisationNiveauer")
  cols <- vapply(cfg$fields, function(f) f$col, "")
  expect_setequal(cols, c("organisatorisk_navn_teknisk",
                          "organisatorisk_navn_langt",
                          "organisatorisk_navn_kort"))
})

test_that("alle HIERARCHY_TABLES-entries har paakraevede felter", {
  for (cfg in HIERARCHY_TABLES) {
    expect_true(all(c("id", "table", "pk", "parent_col", "display_col",
                      "label", "fields", "level") %in% names(cfg)),
                info = cfg$id)
    expect_true(all(c("col", "parent", "parent_pk", "num_col", "name_col",
                      "label_expr") %in% names(cfg$level)), info = cfg$id)
  }
})
```

**Step 2: Kør — forvent FAIL** (`object 'HIERARCHY_TABLES' not found`)

Run: `RS -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-metadata.R')"`

**Step 3: Implementér** i `R/metadata.R`, nederst:

```r
# --- Hierarki-tabeller (traeer med parent-FK) til generisk mod_hierarchy ------
# level: FK-kolonne paa noden + parent-tabel med numerisk niveau (num_col, til
# bloed niveau-konsistens-advarsel) og visningsnavn (name_col/label_expr).
# aktiv_col: NULL hvis tabellen ingen aktiv-kolonne har.
# Fase D tilfoejer indikator_hierarki-entry.
HIERARCHY_TABLES <- list(
  org_struktur = list(
    id = "org_struktur", table = "tblOrganisationStruktur", pk = "Id",
    parent_col = "parent_Id", display_col = "organisatorisk_navn_langt",
    label = "Organisations-struktur", aktiv_col = NULL,
    fields = list(
      list(col = "organisatorisk_navn_teknisk", type = "text",
           label = "Teknisk navn"),
      list(col = "organisatorisk_navn_langt", type = "text",
           label = "Langt navn"),
      list(col = "organisatorisk_navn_kort", type = "text",
           label = "Kort navn")),
    level = list(col = "organisatorisk_niveau",
                 parent = "tblOrganisationNiveauer", parent_pk = "Id",
                 num_col = "organisatorisk_niveau",
                 name_col = "organisatorisk_niveau_navn",
                 label_expr = '"organisatorisk_niveau_navn"'))
)
```

**Step 4: Kør — forvent PASS. Step 5: Fuld suite + commit**

```bash
git add R/metadata.R tests/testthat/test-metadata.R
git commit -m "feat(hierarki): HIERARCHY_TABLES-config (org-struktur-entry)"
```

### Task C2: Rene træ-funktioner (hierarchy_order + hierarchy_descendants)

**Files:**
- Create: `R/fct_hierarchy.R`
- Test: `tests/testthat/test-hierarchy.R` (ny)

**Step 1: Skriv fejlende tests** (brug Write-tool, IKKE heredoc):

```r
# Rene trae-funktioner: depth-first orden + subtree. Kritiske paths: cyklus,
# multi-rod, orphans — fuld unit-daekning uden DB.

.tree_df <- function() data.frame(
  id = c(1L, 2L, 3L, 4L, 5L, 6L),
  parent = c(NA, 1L, 1L, 3L, NA, 999L),   # 2 roedder + orphan (999 findes ej)
  navn = c("RodA", "B", "A-child", "D", "RodB", "Orphan"),
  stringsAsFactors = FALSE)

test_that("hierarchy_order giver depth-first orden med dybder", {
  out <- hierarchy_order(.tree_df(), "id", "parent", sort_col = "navn")
  # RodA(0) -> A-child(1) -> D(2) -> B(1), saa RodB(0), orphan behandles som rod
  expect_identical(out$id, c(1L, 3L, 4L, 2L, 5L, 6L))
  expect_identical(out$depth, c(0L, 1L, 2L, 1L, 0L, 0L))
})

test_that("hierarchy_order overlever cyklus uden at haenge", {
  df <- data.frame(id = 1:3, parent = c(2L, 1L, NA))  # 1<->2 cyklus, 3 rod
  out <- hierarchy_order(df, "id", "parent")
  expect_setequal(out$id, 1:3)                        # alle noder med, praecis en gang
  expect_identical(nrow(out), 3L)
})

test_that("hierarchy_order haandterer tom df", {
  df <- data.frame(id = integer(0), parent = integer(0))
  out <- hierarchy_order(df, "id", "parent")
  expect_identical(nrow(out), 0L)
  expect_true("depth" %in% names(out))
})

test_that("hierarchy_descendants returnerer subtree inkl. noden selv", {
  df <- .tree_df()
  expect_setequal(hierarchy_descendants(df, "id", "parent", 1L),
                  c(1L, 2L, 3L, 4L))
  expect_setequal(hierarchy_descendants(df, "id", "parent", 3L), c(3L, 4L))
  expect_identical(hierarchy_descendants(df, "id", "parent", 5L), 5L)
})

test_that("hierarchy_descendants overlever cyklus", {
  df <- data.frame(id = 1:2, parent = c(2L, 1L))
  expect_setequal(hierarchy_descendants(df, "id", "parent", 1L), c(1L, 2L))
})
```

**Step 2: FAIL. Step 3: Implementér** `R/fct_hierarchy.R`:

```r
# Rene trae-funktioner for hierarki-tabeller (org-struktur, indikator-
# hierarki). Ingen DB-afhaengighed — fuldt unit-testbare.

#' Depth-first trae-orden med dybde-kolonne. Multi-rod + orphan-tolerant +
#' cyklus-sikker. Orphans (parent findes ej i df) behandles som roedder;
#' noder i rene cykler appendes fladt sidst (depth 0) frem for at tabes.
#' @noRd
hierarchy_order <- function(df, pk, parent_col, sort_col = NULL) {
  df$depth <- integer(nrow(df))
  if (nrow(df) == 0) return(df)
  ids <- df[[pk]]
  parents <- df[[parent_col]]
  is_root <- is.na(parents) | !(parents %in% ids)
  kids_of <- split(which(!is_root), as.character(parents[!is_root]))
  order_idx <- integer(0); depths <- integer(0)
  visited <- rep(FALSE, nrow(df))
  visit <- function(i, depth) {
    if (visited[i]) return()               # cyklus-vaern
    visited[i] <<- TRUE
    order_idx <<- c(order_idx, i); depths <<- c(depths, depth)
    kids <- kids_of[[as.character(ids[i])]]
    if (!is.null(sort_col) && length(kids) > 1)
      kids <- kids[order(df[[sort_col]][kids])]
    for (k in kids) visit(k, depth + 1L)
  }
  roots <- which(is_root)
  if (!is.null(sort_col) && length(roots) > 1)
    roots <- roots[order(df[[sort_col]][roots])]
  for (r in roots) visit(r, 0L)
  leftover <- which(!visited)              # rene cykler — tab dem ikke
  out <- df[c(order_idx, leftover), , drop = FALSE]
  out$depth <- c(depths, integer(length(leftover)))
  out
}

#' Alle ids i subtree under id — INKLUSIV id selv. Bruges til at ekskludere
#' egen subtree fra foraelder-dropdown (cyklus-lag 1) og som server-side
#' assert foer flyt (cyklus-lag 2).
#' @noRd
hierarchy_descendants <- function(df, pk, parent_col, id) {
  ids <- df[[pk]]; parents <- df[[parent_col]]
  res <- id; frontier <- id
  repeat {
    kids <- ids[!is.na(parents) & parents %in% frontier & !(ids %in% res)]
    if (length(kids) == 0) break
    res <- c(res, kids); frontier <- kids
  }
  res
}
```

**Step 4: PASS. Step 5: Fuld suite + commit**
`git commit -m "feat(hierarki): hierarchy_order + hierarchy_descendants (rene trae-funktioner)"`

### Task C3: SQL-byggere + make_hierarchy_db

**Files:**
- Modify: `R/fct_sql.R` (ny sektion nederst)
- Modify: `R/fct_db.R` (make_hierarchy_db efter make_lookup_db)
- Test: `tests/testthat/test-sql.R` (tilføj) + `tests/testthat/test-db-hierarchy.R` (ny, gated)

**Step 1: Fejlende SQL-tests** (test-sql.R, nederst — husk `\\$` dobbelt-escapes):

```r
# --- Hierarki-CRUD (Fase C) --------------------------------------------------

test_that("build_hierarchy_list_sql normaliserer aliaser og joiner niveau", {
  cfg <- HIERARCHY_TABLES$org_struktur
  sql <- build_hierarchy_list_sql(cfg)
  expect_match(sql, 'h."Id" AS id', fixed = TRUE)
  expect_match(sql, 'h."parent_Id" AS parent_id_raw', fixed = TRUE)
  expect_match(sql, '"tblOrganisationNiveauer"', fixed = TRUE)
  expect_match(sql, "AS niveau_num")
  expect_match(sql, "AS niveau_navn")
  expect_match(sql, "LEFT JOIN")     # noder uden niveau bevares
})

test_that("hierarchy insert/update/delete parametriserer alle edit-kolonner", {
  cfg <- HIERARCHY_TABLES$org_struktur
  cols <- hierarchy_edit_cols(cfg)   # 3 felter + parent + niveau = 5
  expect_length(cols, 5)
  ins <- build_hierarchy_insert_sql(cfg)
  for (col in cols) expect_match(ins, sprintf('"%s"', col), fixed = TRUE)
  expect_match(ins, 'RETURNING "Id"', fixed = TRUE)
  upd <- build_hierarchy_update_sql(cfg)
  expect_match(upd, '"Id" = \\$6')   # 5 kolonner + id
  expect_identical(build_hierarchy_delete_sql(cfg),
    'DELETE FROM "tblOrganisationStruktur" WHERE "Id" = $1')
  expect_identical(build_hierarchy_child_count_sql(cfg),
    'SELECT count(*) AS n FROM "tblOrganisationStruktur" WHERE "parent_Id" = $1')
})
```

**Step 2: FAIL. Step 3: Implementér** i `fct_sql.R`:

```r
# --- Hierarki-CRUD (config-drevet, HIERARCHY_TABLES) --------------------------

#' Kolonner formularen redigerer (raekkefoelge = parameter-orden).
#' @noRd
hierarchy_edit_cols <- function(cfg) {
  c(vapply(cfg$fields, function(f) f$col, ""), cfg$parent_col, cfg$level$col,
    cfg$aktiv_col)   # aktiv_col er NULL for org → falder bort i c()
}

#' Alle noder med normaliserede aliaser (id/parent_id_raw) + niveau-join.
#' Aliaser goer mod_hierarchy uafhaengig af casing-forskelle (Id/parent_Id).
#' @noRd
build_hierarchy_list_sql <- function(cfg) {
  fcols <- vapply(cfg$fields, function(f) sprintf('h."%s"', f$col), "")
  aktiv <- if (is.null(cfg$aktiv_col)) "" else
    sprintf('h."%s" AS aktiv, ', cfg$aktiv_col)
  paste0(
    'SELECT h."', cfg$pk, '" AS id, h."', cfg$parent_col,
    '" AS parent_id_raw, ', paste(fcols, collapse = ", "), ", ", aktiv,
    'h."', cfg$level$col, '" AS niveau_id, ',
    'n."', cfg$level$num_col, '" AS niveau_num, ',
    'n."', cfg$level$name_col, '" AS niveau_navn ',
    'FROM "', cfg$table, '" h ',
    'LEFT JOIN "', cfg$level$parent, '" n ON n."', cfg$level$parent_pk,
    '" = h."', cfg$level$col, '"')
}

#' @noRd
build_hierarchy_insert_sql <- function(cfg) {
  cols <- hierarchy_edit_cols(cfg)
  ph <- paste(sprintf("$%d", seq_along(cols)), collapse = ", ")
  qcols <- paste(sprintf('"%s"', cols), collapse = ", ")
  sprintf('INSERT INTO "%s" (%s) VALUES (%s) RETURNING "%s"',
          cfg$table, qcols, ph, cfg$pk)
}

#' @noRd
build_hierarchy_update_sql <- function(cfg) {
  cols <- hierarchy_edit_cols(cfg)
  sets <- vapply(seq_along(cols),
                 function(i) sprintf('"%s" = $%d', cols[i], i), "")
  sprintf('UPDATE "%s" SET %s WHERE "%s" = $%d',
          cfg$table, paste(sets, collapse = ", "), cfg$pk, length(cols) + 1)
}

#' @noRd
build_hierarchy_delete_sql <- function(cfg) {
  sprintf('DELETE FROM "%s" WHERE "%s" = $1', cfg$table, cfg$pk)
}

#' Antal boern (slet-guard: noder med boern kan ikke slettes)
#' @noRd
build_hierarchy_child_count_sql <- function(cfg) {
  sprintf('SELECT count(*) AS n FROM "%s" WHERE "%s" = $1',
          cfg$table, cfg$parent_col)
}
```

**Step 4: SQL-tests PASS. Step 5: make_hierarchy_db** i `fct_db.R` (efter
`make_lookup_db`):

```r
#' Byg db-accessors for én hierarki-tabel. cfg = element fra HIERARCHY_TABLES.
#' values = named list i hierarchy_edit_cols(cfg)-orden.
#' @noRd
make_hierarchy_db <- function(pool, cfg) {
  cols <- hierarchy_edit_cols(cfg)
  list(
    list_nodes = function() {
      DBI::dbGetQuery(pool, build_hierarchy_list_sql(cfg))
    },
    niveau_options = function() {
      DBI::dbGetQuery(pool, build_fk_options_sql(cfg$level$parent,
                                                 cfg$level$label_expr))
    },
    create_node = function(values) {
      assert_write_enabled()
      DBI::dbGetQuery(pool, build_hierarchy_insert_sql(cfg),
                      params = unname(values[cols]))[[1]][1]
    },
    update_node = function(id, values) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_hierarchy_update_sql(cfg),
                     params = c(unname(values[cols]), list(id)))
    },
    delete_node = function(id) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_hierarchy_delete_sql(cfg), params = list(id))
    },
    child_count = function(id) {
      as.integer(DBI::dbGetQuery(pool, build_hierarchy_child_count_sql(cfg),
                                 params = list(id))$n[1])
    }
  )
}
```

**Step 6: Gated DB-test** `tests/testthat/test-db-hierarchy.R` (mønster fra
test-db-diagram.R: samme skip_if_no_db; `withr::defer`-oprydning):
roundtrip på org_struktur — læs kendt rod-id fra DB, opret node under den
(kendt niveau-id fra `niveau_options()`), verificér RETURNING-id +
`child_count(rod) >= 1`, opdatér navn + flyt til anden kendt node, læs
tilbage via `list_nodes()`, slet, verificér væk. FK-fejl-testen: forsøg
`delete_node` på en node med børn → `expect_error` (DB-FK self-join).

**Step 7:** Gated PASS med env; skip uden. Fuld suite. **Commit:**
`feat(hierarki): SQL-byggere + make_hierarchy_db (config-drevet CRUD)`

### Task C4: mod_hierarchy (træ-tabel + modal)

**Files:**
- Create: `R/mod_hierarchy.R`
- Test: `tests/testthat/test-mod-hierarchy.R` (testServer, fake-db-mønster fra
  test-mod-diagram.R)

**Indhold (følg mod_diagram.R-idiomer — notifikationer via status_msg/warn_msg
reactiveVals, safe_operation ved db-kald, eksponér reactives til test):**

1. `mod_hierarchy_ui(id, cfg)`: "Ny node"-knap + `DT::DTOutput`. Ingen filtre
   (165-362 rækker — DT-søgefeltet rækker).
2. `mod_hierarchy_server(id, db, cfg)`:
   - `nodes <- reactiveVal(db$list_nodes())`; `tree <- reactive(
     hierarchy_order(nodes(), "id", "parent_id_raw", sort_col = cfg$display_col))`
   - DT: navn indrykket efter depth (`paste0(strrep("&nbsp;&nbsp;&nbsp;", depth), navn)`,
     escape = FALSE kun på navnekolonnen — HTML-escape navnet FØRST), niveau-navn,
     evt. aktiv-flag (✓/—), Åbn-knap pr. række (onclick-setInputValue-mønster
     fra mod_diagram `input$open_id`).
   - Modal (`.hierarchy_form_ui` — ren funktion): tekstfelter fra cfg$fields
     (textarea hvor `type = "textarea"`), Forælder-select (choices = "(rod)" +
     alle noder MINUS egen subtree via `hierarchy_descendants` — display_col
     som label, træ-orden), Niveau-select (fra `db$niveau_options()`),
     aktiv-checkbox hvis `cfg$aktiv_col`. Prefix `h_`.
   - `.collect_hierarchy_form(input, cfg, prefix = "h_")`: named list i
     `hierarchy_edit_cols(cfg)`-orden; tom forælder → NA (rodnode OK); tomme
     tekstfelter → NA.
   - Gem: validér display_col ikke-tom → **server-side cyklus-assert**
     (`new_parent %in% hierarchy_descendants(nodes(), "id", "parent_id_raw",
     editing_id())` → fejl-notifikation, INTET gem — værn mod stale UI) →
     **blød niveau-advarsel** (nyt niveau_num ≤ forælders niveau_num →
     warn_msg, gem fortsætter; NA-niveauer springer tjekket over) →
     safe_operation create/update → removeModal + reload.
   - Slet (kun eksisterende): `child_count(id) > 0` → warn *"Noden har N
     børn — flyt eller slet dem først."* og INTET delete. Ellers
     safe_operation delete; DB-FK-fejl (node i brug af diagrammer/personer/…)
     fanges af safe_operation → fallback-besked *"Noden er i brug og kan ikke
     slettes (referencer findes)."*
   - "Ny node": forælder forudfyldt fra senest åbnede række (eller tom).

**testServer-tests (min. 6, fake-db med 4-node træ):** (1) tree() i korrekt
depth-first-orden; (2) gem eksisterende → update_node med korrekte values;
(3) flyt til egen subtree → update_node IKKE kaldt + fejlbesked; (4) niveau-
spring op → warn_msg sat MEN update_node kaldt; (5) slet med børn → blokeret;
(6) opret ny med tom forælder → create_node med NA-parent.

**Commit pr. grønt delstep;** afsluttende:
`feat(hierarki): mod_hierarchy (indrykket trae + flyt med cyklus-vaern + slet-guards)`

### Task C5: Wiring + landing-reorganisering ("Organisation"-sektion)

**Files:**
- Modify: `R/app_ui.R` (nav_panel + landing-sektioner)
- Modify: `R/app_server.R` (mod_hierarchy_server + observer)

**app_ui:**
1. Ny nav_panel efter "Diagrammer": `bslib::nav_panel("Organisation",
   value = "org_struktur", mod_hierarchy_ui("org_struktur",
   HIERARCHY_TABLES$org_struktur))`.
2. Landing: ny `sect("Organisation")` med to fliser — `tile("org_struktur",
   "Organisations-struktur", "Trae-redigering: felter, flyt og opret/slet.")`
   + oversættelses-flisen FLYTTES hertil: i Opslagstabeller-loopet filtreres
   `org_oversaettelse` fra (`Filter(function(cfg) cfg$id != "org_oversaettelse",
   LOOKUP_TABLES)`), og flisen genskabes i Organisation-sektionen med samme
   `go_org_oversaettelse`-value (nav-menuen under "Opslagstabeller" beholder
   alle entries — kun landing-fliserne omorganiseres).

**app_server:** `mod_hierarchy_server("org_struktur",
make_hierarchy_db(pool, HIERARCHY_TABLES$org_struktur),
HIERARCHY_TABLES$org_struktur)` + `observeEvent(input$go_org_struktur,
bslib::nav_select("nav", "org_struktur"))`.

**Verifikation:** `RS -e "pkgload::load_all('.'); ui <- app_ui(NULL); cat('OK')"`
+ fuld suite. **Commit:** `feat(hierarki): wiring + Organisation-sektion paa landing`

### Task C6: Write-guard-indikator i navbar

**Files:**
- Modify: `R/app_ui.R` (ren badge-funktion + nav_item)
- Modify: `R/app_server.R` (renderUI)
- Test: `tests/testthat/test-app-ui.R` (ny)

**Step 1: Fejlende test:**

```r
test_that(".write_badge_ui viser status for skrive-guard", {
  on <- as.character(.write_badge_ui(TRUE))
  off <- as.character(.write_badge_ui(FALSE))
  expect_match(on, "Skrivning aktiv")
  expect_match(on, "text-bg-danger")        # roed: writes rammer prod-DB
  expect_match(off, "Skrivebeskyttet")
  expect_match(off, "text-bg-secondary")
})

test_that("app_ui konstruerer uden fejl", {
  expect_no_error(app_ui(NULL))
})
```

**Step 3: Implementér** i app_ui.R:

```r
#' Badge der viser om DB-skrivning er aktiv (roed = writes rammer prod).
#' Ren funktion — unit-testbar uden session.
#' @noRd
.write_badge_ui <- function(enabled) {
  if (enabled) {
    tags$span(class = "badge text-bg-danger align-self-center",
              title = "BFHMETA_WRITE=1 — aendringer skrives til Supabase",
              "Skrivning aktiv")
  } else {
    tags$span(class = "badge text-bg-secondary align-self-center",
              title = "Saet BFHMETA_WRITE=1 for at aktivere skrivning",
              "Skrivebeskyttet")
  }
}
```

I `app_ui()` page_navbar, sidst: `bslib::nav_spacer(),
bslib::nav_item(uiOutput("write_badge"))`. I `app_server`:
`output$write_badge <- renderUI(.write_badge_ui(write_enabled()))`.

**Step 5: Commit** `feat(ui): write-guard-badge i navbar (Skrivning aktiv/Skrivebeskyttet)`

### Task C7: Røgtest + release 0.8.0

1. Fuld suite → alle grønne.
2. **[MANUELT TRIN]** Røgtest (`BFHMETA_WRITE=1` + `RS dev/run_dev.R`):
   - Navbar viser rød "Skrivning aktiv"-badge (start også uden env →
     grå "Skrivebeskyttet").
   - Organisation-sektion på landing: Struktur + Oversættelse-fliser.
   - Træet viser 362 noder, 2 rødder, korrekt indrykning.
   - Redigér et navn → gem → persistens. Flyt en løvnode → advarsel hvis
     niveau-spring → gem OK. Forsøg flyt af node til egen subtree →
     dropdown tilbyder det ikke.
   - Opret testnode under løvnode → slet den igen. Slet-forsøg på node med
     børn → venlig besked. Slet-forsøg på node i brug (fx med diagrammer) →
     "i brug"-besked.
3. `DESCRIPTION` 0.8.0 + NEWS-entry (org-struktur-redigering; write-badge;
   Organisation-sektion).
4. Commit `chore(release): bump 0.8.0 (Fase C komplet)` → push → draft-PR →
   **STOP: afvent bruger-merge før Fase D.**

---

## Fase D — indikator-hierarki-instans (→ 0.9.0)

Branch (efter C merged): `git checkout -b feat/crud-fase-d main`

### Task D1: Config-entry + tests

**Files:** `R/metadata.R`, `tests/testthat/test-metadata.R`

Tilføj `indikator_hierarki`-entry i HIERARCHY_TABLES (TDD som C1):

```r
  ,
  indikator_hierarki = list(
    id = "indikator_hierarki", table = "tblIndikatorHierarki", pk = "Id",
    parent_col = "parent_id", display_col = "hierarki_navn",
    label = "Indikator-hierarki", aktiv_col = "aktiv",
    fields = list(
      list(col = "hierarki_navn", type = "text", label = "Navn"),
      list(col = "hierarki_navn_kort", type = "text", label = "Kort navn"),
      list(col = "beskrivelse_kort", type = "textarea",
           label = "Kort beskrivelse"),
      list(col = "beskrivelse_lang", type = "textarea",
           label = "Lang beskrivelse"),
      list(col = "kilde_id", type = "text", label = "Kilde-id (import)")),
    level = list(col = "indikator_niveau", parent = "tblIndikatorNiveauer",
                 parent_pk = "Id", num_col = "indikator_niveau",
                 name_col = "indikator_niveau_navn",
                 label_expr = '"indikator_niveau_navn"'))
```

Test: entry-assertions (parent_col = `"parent_id"` småt, aktiv_col = "aktiv",
7 edit-cols inkl. aktiv) + genkør config-loop-testen. SQL-testen for
update: `"Id" = \\$8` (5 felter + parent + niveau + aktiv = 7 → id $8).
Commit: `feat(hierarki): indikator-hierarki-entry i HIERARCHY_TABLES`

### Task D2: Wiring (nav + landing under "Indikatorer"-sektionen)

`app_ui`: nav_panel "Indikator-hierarki" value `indikator_hierarki` +
landing-flise i den EKSISTERENDE "Indikatorer"-sektion (ved siden af
Indikatorer-flisen). `app_server`: instans + `go_indikator_hierarki`-observer.
Aktiv-kolonnen håndteres allerede generisk af mod_hierarchy (C4).
Verifikation: app_ui-konstruktion + fuld suite.
Commit: `feat(hierarki): indikator-hierarki-instans (nav + landing)`

### Task D3: aktiv/kilde_id i indikator-modalens hierarki-dropdown

**Files:**
- Modify: `R/fct_db.R` (fk_options: aktiv-flag for indikator_hierarki)
- Modify: `R/mod_indikator_crud.R` (.field_input/fk_choices for hierarki)
- Test: `tests/testthat/test-mod-crud.R` (tilføj)

**Adfærd (fra spec):** dropdown for `indikator_hierarki` filtreres til AKTIVE
noder ved nyvalg; en eksisterende værdi der peger på inaktiv node BEVARES i
choices og vises med suffix " (inaktiv)" — ingen stille datamutation.

**Implementering:**
1. `fct_db.R` fk_options: for feltet `indikator_hierarki` udvid query'en med
   `"aktiv"`-kolonnen (ny bygger `build_fk_options_aktiv_sql(parent,
   label_expr, aktiv_col)` — TDD i test-sql.R).
2. `mod_indikator_crud.R`: i `.build_modal`s `fin("indikator_hierarki")`-sti
   byg choices = aktive noder + (hvis rækkens nuværende værdi er inaktiv)
   den nuværende node med " (inaktiv)"-suffix. Ren hjælpefunktion
   `.hierarki_choices(opts_df, current_id)` (unit-testbar uden session):

```r
#' Dropdown-choices for indikator-hierarki: aktive + evt. nuvaerende inaktive
#' node markeret "(inaktiv)" — bevarer eksisterende vaerdi uden stille mutation.
#' @noRd
.hierarki_choices <- function(opts_df, current_id = NULL) {
  act <- opts_df[opts_df$aktiv %in% TRUE, , drop = FALSE]
  ch <- stats::setNames(act$id, act$label)
  if (!is.null(current_id) && !is.na(current_id) &&
      !(current_id %in% act$id) && current_id %in% opts_df$id) {
    lbl <- opts_df$label[opts_df$id == current_id][1]
    ch <- c(ch, stats::setNames(current_id, paste0(lbl, " (inaktiv)")))
  }
  ch
}
```

Tests: (a) inaktive noder udeladt ved nyvalg (current_id = NULL); (b) nuværende
inaktiv værdi bevaret med suffix; (c) nuværende aktiv værdi ikke duplikeret.
`kilde_id` kræver intet her — redigeres i mod_hierarchy (D1-felterne).

Commit: `feat(indikator): hierarki-dropdown filtrerer inaktive (bevarer eksisterende valg)`

### Task D4: Røgtest + release 0.9.0

1. Fuld suite → grøn.
2. **[MANUELT TRIN]** Røgtest: Indikator-hierarki-træ (165 noder, 1 rod,
   aktiv-flag synligt); redigér navn + beskrivelser + kilde_id; deaktivér en
   node → indikator-modalens datasæt-dropdown tilbyder den ikke ved nyvalg,
   men eksisterende indikatorer på noden viser "(inaktiv)"; genaktivér.
3. `DESCRIPTION` 0.9.0 + NEWS (indikator-hierarki-CRUD; aktiv-filtrering i
   modal-dropdown; kilde_id-felt).
4. Commit `chore(release): bump 0.9.0 (Fase D komplet)` → push → draft-PR →
   **STOP.** Fuld-CRUD-designet er dermed komplet (alle 19 tabeller
   redigerbare fra appen).

---

## Bevidst udeladt (jf. spec + YAGNI)
- Drag-and-drop-træ; hård diagram-unikhed; PBI-import-omlægning (separat repo)
- `tblDiagramIndstillinger`/`tblDiagrammerMaal`/`tblDiagrammerKommentar`
  (kan tilføjes via LOOKUP_TABLES ved behov)
- Genbrug af `mod_lookup_table` til hierarkier (fravalgt: træ-orden,
  cyklus-værn og modal-form passer ikke inline-cell-mønstret)
