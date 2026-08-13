# Bevar tabeltilstand efter redigering Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Bevar gyldige dropdownfiltre samt DataTables-søgning, sortering og side, når appens data genindlæses efter redigering.

**Architecture:** En ren hjælper afgør, om et eksisterende filtervalg fortsat er gyldigt, og de dynamiske filter-UI'er bruger den ved genrendering. DT-tabeller med søgning eller paging bruger DataTables' sessionsbundne state-lagring (`stateSave = TRUE`, `stateDuration = -1`); tabeller med `dom = "t"` forbliver uændrede.

**Tech Stack:** R, Shiny, DT/DataTables, testthat, htmltools.

## Global Constraints

- Tilstanden lever kun i den aktuelle browserfane/session og må ikke genopstå ved en senere appstart.
- Dropdownvalg bevares kun, mens de fortsat findes; ellers bruges `""` ("Alle").
- Datasætfilteret bevares kun, mens det er gyldigt under den valgte datapakke.
- Database-, validerings- og gemmeadfærd må ikke ændres.
- Tabeller uden søgning, sortering eller paging får ingen state-håndtering.

---

### Task 1: Bevar dynamiske dropdownfiltre

**Files:**
- Modify: `R/utils_validation.R`
- Modify: `R/mod_diagram.R:143-157`
- Modify: `R/mod_indikator_crud.R:383-401`
- Test: `tests/testthat/test-validation.R`
- Test: `tests/testthat/test-mod-diagram.R`
- Test: `tests/testthat/test-mod-crud.R`

**Interfaces:**
- Produces: `.preserved_filter_selection(current, choices, fallback = "") -> character(1)`; returnerer `current` som tekst, hvis præcis én ikke-manglende værdi findes i `unname(choices)`, ellers `fallback`.
- Consumes: eksisterende Shiny-inputværdier og allerede afledte valgvektorer.

- [ ] **Step 1: Skriv den fejlende hjælpertest**

Tilføj i `tests/testthat/test-validation.R`:

```r
test_that("filtervalg bevares kun mens det fortsat er gyldigt", {
  choices <- c("Alle" = "", "Pakke A" = "A", "Pakke B" = "B")
  expect_identical(.preserved_filter_selection("B", choices), "B")
  expect_identical(.preserved_filter_selection("Slettet", choices), "")
  expect_identical(.preserved_filter_selection(NULL, choices), "")
  expect_identical(.preserved_filter_selection(NA_character_, choices), "")
})
```

Mutation den beskytter imod: en hjælper, der altid returnerer fallback eller accepterer et bortfaldet valg.

- [ ] **Step 2: Kør hjælpertesten og bekræft RED**

```powershell
Rscript -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-validation.R', reporter = 'summary')"
```

Expected: FAIL fordi `.preserved_filter_selection` ikke findes.

- [ ] **Step 3: Implementér den minimale hjælper**

Tilføj i `R/utils_validation.R`:

```r
#' @noRd
.preserved_filter_selection <- function(current, choices, fallback = "") {
  if (length(current) != 1L || is.na(current)) return(fallback)
  current <- as.character(current)
  if (current %in% as.character(unname(choices))) current else fallback
}
```

- [ ] **Step 4: Kør hjælpertesten og bekræft GREEN**

Kør Step 2. Expected: PASS.

- [ ] **Step 5: Skriv fejlende integrationstests for dynamiske filtre**

Tilføj en testhjælper, der udtrækker valgt option fra renderet `selectInput`-HTML:

```r
selected_option_value <- function(tag) {
  html <- htmltools::renderTags(tag)$html
  option <- regmatches(html, regexpr(
    '<option[^>]* selected(?:="selected")?[^>]*>', html, perl = TRUE))
  sub('.*value="([^"]*)".*', '\\1', option)
}
```

I `test-mod-diagram.R`: sæt hvert af `filter_indikator`, `filter_org`, `filter_datapakke` og `filter_datasaet` til fixturets gyldige, bogstavelige værdier, kald `reload()`, og verificér valget i hvert `output$*_ui`. Erstat derefter admin-fixturen, så et valgt filter bortfalder, kald `reload()`, og forvent `""`.

I `test-mod-crud.R`: vælg datapakke og datasæt, kald `reload()`, og verificér begge. Skift så til en datapakke uden det valgte datasæt og verificér, at datapakken bevares, mens datasættet bliver `""`.

Forventninger skal være håndskrevne fixtureværdier, ikke beregnet med produktionshjælperen.

- [ ] **Step 6: Kør modultestene og bekræft RED**

```powershell
Rscript -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-mod-diagram.R', reporter = 'summary'); testthat::test_file('tests/testthat/test-mod-crud.R', reporter = 'summary')"
```

Expected: nye assertions fejler med `""` i stedet for aktuelt valg.

- [ ] **Step 7: Brug hjælperen i de dynamiske filter-UI'er**

I `mod_diagram.R` bygger `.filter_ui()` `choices` og anvender:

```r
selected <- .preserved_filter_selection(
  isolate(input[[input_id]]), choices)
selectInput(ns(input_id), lab, choices = choices, selected = selected)
```

I `mod_indikator_crud.R` bygger begge blokke `choices` og anvender isoleret `input$filter_datapakke` henholdsvis `input$filter_datasaet`. Datasætblokken bevarer sin reaktive afhængighed til datapakkefilteret; kun dens eget aktuelle valg isoleres.

- [ ] **Step 8: Kør Step 2 og Step 6 igen og bekræft GREEN**

Expected: PASS.

- [ ] **Step 9: Commit Task 1**

```powershell
git add R/utils_validation.R R/mod_diagram.R R/mod_indikator_crud.R tests/testthat/test-validation.R tests/testthat/test-mod-diagram.R tests/testthat/test-mod-crud.R
git commit -m "fix(filters): preserve valid selections after reload"
```

### Task 2: Bevar DT-søgning, sortering og side i sessionen

**Files:**
- Modify: `R/mod_diagram.R:180-205`
- Modify: `R/mod_hierarchy.R:216-242`
- Modify: `R/mod_indikator_crud.R:374-380`
- Modify: `R/mod_indikator_crud.R:422-449`
- Test: `tests/testthat/test-mod-diagram.R`
- Test: `tests/testthat/test-hierarchy-editor.R`
- Test: `tests/testthat/test-mod-crud.R`

**Interfaces:**
- Consumes: DataTables-optionerne `stateSave` og `stateDuration`.
- Produces: widgetkonfiguration med `stateSave = TRUE` og `stateDuration = -1` på fire filtrerbare eller sideinddelte tabeller.

- [ ] **Step 1: Skriv fejlende widget-adfærdstests**

Kald de faktiske renderDT-udtryk i `testServer()` og verificér på widgets:

```r
expect_true(widget$x$options$stateSave)
expect_identical(widget$x$options$stateDuration, -1)
```

Dæk diagramoversigtens `tbl`, organisationshierarkiets `tbl`, indikatoroversigtens `oversigt` og indikatorens inline-`tbl`. Tilføj en assertion på opslagstabellens widget om, at `stateSave` fortsat er `NULL`, fordi den har `dom = "t"`, `paging = FALSE` og ingen brugerfiltrering. Test widgets, ikke kildekodetekst.

- [ ] **Step 2: Kør fokustestene og bekræft RED**

```powershell
Rscript -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-mod-diagram.R', reporter = 'summary'); testthat::test_file('tests/testthat/test-hierarchy-editor.R', reporter = 'summary'); testthat::test_file('tests/testthat/test-mod-crud.R', reporter = 'summary')"
```

Expected: state-assertions fejler, fordi options mangler.

- [ ] **Step 3: Aktivér sessionsbundet state på fire tabeller**

Flet følgende ind i hver eksisterende `options = list(...)` uden at ændre `pageLength`, `columnDefs` eller redigeringsopsætning:

```r
stateSave = TRUE,
stateDuration = -1
```

Inline-tabellen får `options = list(stateSave = TRUE, stateDuration = -1)`. `mod_lookup_table.R` og signalreviewets `breaks_tbl` forbliver uændrede.

- [ ] **Step 4: Kør Step 2 igen og bekræft GREEN**

Expected: PASS.

- [ ] **Step 5: Kør modulregressionerne**

```powershell
Rscript -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-mod-diagram.R', reporter = 'summary'); testthat::test_file('tests/testthat/test-mod-crud.R', reporter = 'summary'); testthat::test_file('tests/testthat/test-hierarchy-editor.R', reporter = 'summary'); testthat::test_file('tests/testthat/test-mod-hierarchy.R', reporter = 'summary'); testthat::test_file('tests/testthat/test-app-ui.R', reporter = 'summary')"
```

Expected: PASS; kendt Shiny/R-versionsadvarsel er acceptabel.

- [ ] **Step 6: Commit Task 2**

```powershell
git add R/mod_diagram.R R/mod_hierarchy.R R/mod_indikator_crud.R tests/testthat/test-mod-diagram.R tests/testthat/test-hierarchy-editor.R tests/testthat/test-mod-crud.R
git commit -m "fix(tables): retain session state across redraws"
```

### Task 3: Slutverifikation og integration

**Files:**
- Verify only; ingen planlagte produktionsændringer.

**Interfaces:**
- Consumes: Task 1 og Task 2's commits.
- Produces: test-, review- og Git-evidens til integration.

- [ ] **Step 1: Kør den fulde testsuite**

```powershell
Rscript -e "devtools::test(reporter = 'summary')"
```

Expected: exit code 0. Dokumentér kendte miljøadvarsler og write-gated skips.

- [ ] **Step 2: Kontrollér diff og arbejdstræ**

```powershell
git diff --check main...HEAD
git status --short
git log --oneline main..HEAD
```

Expected: ingen diff-fejl og rent arbejdstræ.

- [ ] **Step 3: Gennemfør uafhængigt code review**

Kontrollér specifikationsdækning, browser-sessionens levetid, kaskadefilterets ugyldighedsgren, state på præcis de relevante tabeller samt fravær af database-/gemmeændringer.

- [ ] **Step 4: Integrér efter godkendt review**

Efter frisk slutverifikation: fast-forward-merge til `main`, push `main` til `origin`, fjern worktree og lokal featurebranch. Kontrollér slutteligt:

```powershell
git status --short --branch
git rev-parse main
git rev-parse origin/main
git worktree list
```

Expected: ren `main`, samme commit for `main` og `origin/main`, og intet feature-worktree.

