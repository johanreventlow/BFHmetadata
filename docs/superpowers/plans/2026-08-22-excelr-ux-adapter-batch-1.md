# excelR UX-adapter, batch 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Indfør den fælles excelR UX-adapter og migrér kun den enkle `faggrupper`-opslagstabel, så enkeltceller gemmes entydigt i baggrunden uden fuld grid-render, mens fokus, scroll og sortering bevares.

**Architecture:** En opt-in JavaScript-adapter erstatter excelR's fælles change/selection-input med to prioriterede Shiny-kanaler på migrerede grids. Et rent R-lag validerer generation, PK, kolonneallowlist og type, mens `mod_lookup_table` ejer write, kanonisk lokal tilstand og målrettet DB-reconciliation. Ikke-migrerede grids beholder den nuværende fuldtabel-diff uændret.

**Tech Stack:** R 4.1+, Shiny/Golem, excelR 0.4.0, jspreadsheet CE 3.9.1, htmltools, JavaScript, testthat, shinytest2/Chromote.

**Spec:** `docs/superpowers/specs/2026-08-22-excelr-ux-adapter-design.md`

## Global Constraints

- Arbejd på `feat/origin-main-hardening` oven på den godkendte designspec.
- Bevar brugerens eksisterende `.Rbuildignore`-ændring og `Renviron-kopi.txt`; stage aldrig disse filer.
- Brug TDD: skriv hver fokuseret test først, se den fejle af den forventede grund, implementér mindst muligt, og kør testen igen.
- `faggrupper` er eneste opt-in i batch 1. Alle andre opslagstabeller skal fortsat bruge `input$tbl` og `excel_diff_cells()`.
- Et adapterstyret grid må aldrig bruge legacy-`input$tbl` som write-kilde.
- Skjult PK er første datakolonne, `columnDrag = FALSE`, og serveren stoler aldrig på et feltnavn fra browseren.
- En normal succes må ikke invalidere `renderExcel()` eller ændre `grid_generation`.
- En DB-exception er et tvetydigt write-resultat. Genlæs DB-tilstanden før `saved`/`rejected`; ved mislykket genlæsning låses grid'et fail-closed.
- Ingen schemaændring, bulk-write, audit/undo, concurrency/CAS eller atomisk paste i denne batch.
- Stop efter batchens automatiske tests og den manuelle `faggrupper`-røgtest. Fortsæt ikke til indikator-grid'et uden ny godkendelse.

---

## Task 1: Fastlæg og test den rene R-eventkontrakt

**Files:**

- Create: `R/fct_excel_adapter.R`
- Create: `tests/testthat/test-excel-adapter.R`

- [ ] **Step 1: Skriv tests for kolonneallowlisten**

Tilføj en lille testfixture og krav til en 0-baseret, serverejet mapping:

```r
adapter_rows <- data.frame(
  Id = c(11L, 12L),
  navn = c("A", "B"),
  niveau = c(1L, 2L),
  stringsAsFactors = FALSE
)

adapter_map <- data.frame(
  column_index = 0:2,
  field = c("Id", "navn", "niveau"),
  value_type = c("int", "text", "int"),
  editable = c(FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

test_that("adapter map kræver entydige 0-baserede kolonner og skjult pk", {
  expect_silent(validate_excel_adapter_map(adapter_map, names(adapter_rows), "Id"))
  expect_error(validate_excel_adapter_map(transform(adapter_map,
    column_index = c(0L, 1L, 1L)), names(adapter_rows), "Id"), "entydig")
  expect_error(validate_excel_adapter_map(transform(adapter_map,
    editable = c(TRUE, TRUE, TRUE)), names(adapter_rows), "Id"), "PK")
})
```

- [ ] **Step 2: Kør testen og bekræft RED**

Run: `Rscript -e 'devtools::test(filter = "excel-adapter", stop_on_failure = TRUE)'`

Expected: FAIL med `could not find function "validate_excel_adapter_map"`.

- [ ] **Step 3: Implementér map-validering og den offentlige interne kontrakt**

Implementér disse ikke-eksporterede funktioner i `R/fct_excel_adapter.R`:

```r
# validate_excel_adapter_map(map, col_names, pk) -> invisible(map) eller error
# prepare_excel_cell_update(event, generation, rows, pk, column_map) -> list
# patch_excel_cell(rows, row_index, field, value) -> data.frame
# excel_adapter_result(event, status, value, message = NULL, lock_grid = FALSE)
# send_excel_adapter_result(session, output_id, result) -> invisible(NULL)
# send_excel_adapter_init(session, output_id, generation) -> invisible(NULL)
```

`validate_excel_adapter_map()` skal kræve præcis kolonnerne
`column_index`, `field`, `value_type`, `editable`; entydige non-negative integer-
indekser; samme rækkefølge/felter som `col_names`; en eksisterende, ikke-
redigerbar PK; og typer i `c("text", "int", "fk", "boolean")`.

- [ ] **Step 4: Skriv tests for eventvalidering, mapping og koercion**

Brug denne eventfactory:

```r
cell_event <- function(event_id = "1", generation = 7L, row_pk = "11",
                       column_index = 1L, raw_value = "Nyt") {
  list(event_id = event_id, grid_generation = generation, row_pk = row_pk,
       column_index = column_index, raw_value = raw_value)
}
```

Test mindst:

- gyldig tekst event mapper indeks 1 til `navn`, række 1 og PK `11L`;
- browserens event kan ikke vælge et felt ved navn;
- manglende/blankt `event_id`, gammel/fremtidig generation, ukendt PK,
  ukendt indeks og read-only PK afvises før write;
- `"7"` bliver `7L` for `int`/`fk`, mens `"7.2"` og `"abc"` afvises;
- `""` bliver `NA_character_` for tekst og `NA_integer_` for int/fk;
- boolean accepterer kun entydige `TRUE/FALSE`, `true/false` og `1/0`;
- dubleret PK i `rows` afvises fail-closed;
- `patch_excel_cell()` ændrer kun den ene udpegede celle og bevarer typer.

Det gyldige resultat skal have den stabile form:

```r
list(
  ok = TRUE,
  event_id = "1",
  grid_generation = 7L,
  cell_key = "11:1",
  row_index = 1L,
  pk_value = 11L,
  field = "navn",
  value = "Nyt",
  canonical_value = "Nyt",
  message = NULL
)
```

Et afvist resultat skal stadig gentage event/generation, når de kan parses,
men må ikke indeholde et DB-felt eller en writebar værdi.

- [ ] **Step 5: Kør de nye tests og bekræft RED**

Run: `Rscript -e 'devtools::test(filter = "excel-adapter", stop_on_failure = TRUE)'`

Expected: FAIL på manglende `prepare_excel_cell_update()`.

- [ ] **Step 6: Implementér streng parsing og kanonisk serialisering**

Vigtige regler:

```r
# Indeks kommer fra JSON og skal være et helt, endeligt scalar-tal.
is_scalar_intish <- function(x) {
  length(x) == 1L && !is.na(x) && is.numeric(x) && is.finite(x) && x == floor(x)
}

# Tom brugerinput er database-NA; svarværdien sendes som NA, som Shiny
# serialiserer til JSON null. Browseren viser null som tom celle.
canonical_for_browser <- function(x) if (length(x) == 0L || is.na(x)) NA else x
```

Brug eksakt integer-regex før `as.integer()` og kontrollér overflow. Match PK
som character, men tag den typede PK fra `rows[[pk]]`. Brug korte, sikre danske
fejlbeskeder; eventindhold må ikke interpoleres i beskeden.

- [ ] **Step 7: Test svar- og sendekontrakten**

Test at `excel_adapter_result()` kun tillader `saved`/`rejected`, altid
returnerer `event_id`, `grid_generation`, `status`, `value`, `message` og
`lock_grid`, og at `send_excel_adapter_result()` kalder:

```r
session$sendCustomMessage(
  "bfh-excel-adapter:result",
  c(list(id = session$ns(output_id)), result)
)
```

Brug en lille fake session med `ns()` og `sendCustomMessage()` i unit-testen;
ingen rigtig Shiny-session er nødvendig.

Test også, at `send_excel_adapter_init()` sender outputtets fulde namespacede
DOM-id og generation på `bfh-excel-adapter:init`. Init-kontrakten er separat
fra excelR-parametrene, fordi jspreadsheet 3.9.1 bortfiltrerer ukendte options.

- [ ] **Step 8: Kør fokuserede tests og commit**

Run: `Rscript -e 'devtools::test(filter = "excel-adapter", stop_on_failure = TRUE)'`

Expected: PASS.

```bash
git add R/fct_excel_adapter.R tests/testthat/test-excel-adapter.R
git commit -m "test(adapter): define cell event contract"
```

---

## Task 2: Indlæs opt-in browseradapteren uden at påvirke legacy-grids

**Files:**

- Create: `inst/www/bfh-excel-adapter.js`
- Create: `inst/www/bfh-excel-adapter.css`
- Modify: `R/fct_excel_adapter.R`
- Modify: `R/app_ui.R`
- Modify: `R/mod_lookup_table.R`
- Modify: `tests/testthat/test-mod-lookup.R`

- [ ] **Step 1: Skriv UI- og widgettests for eksplicit opt-in**

Tilføj `excel_adapter = TRUE` til en særskilt testcfg, ikke til den eksisterende
`cfg_test`. Test:

```r
expect_s3_class(.excel_adapter_dependency(), "html_dependency")
expect_equal(.excel_adapter_dependency()$script, "bfh-excel-adapter.js")
expect_match(as.character(mod_lookup_table_ui("x", cfg_adapter)),
             "bfh-excel-grid")
expect_false(grepl("bfh-excel-grid",
                   as.character(mod_lookup_table_ui("x", cfg_test))))
```

I `testServer` skal adaptercfg'ens widget-JSON indeholde:

```r
expect_true(isTRUE(w$x$tableOverflow))
expect_false(isTRUE(w$x$pagination))
expect_false(isTRUE(w$x$columnDrag))
```

Adapteridentitet og generation sendes via wrapper/custom message og må netop
ikke afhænge af ukendte `w$x`-options.

- [ ] **Step 2: Kør modultesten og bekræft RED**

Run: `Rscript -e 'devtools::test(filter = "mod-lookup", stop_on_failure = TRUE)'`

Expected: FAIL fordi dependency, wrapper og widgetparametre ikke findes.

- [ ] **Step 3: Implementér dependency og opt-in wrapper**

Tilføj i `R/fct_excel_adapter.R`:

```r
.excel_adapter_dependency <- function() {
  htmltools::htmlDependency(
    name = "bfh-excel-adapter",
    version = "0.1.0",
    src = c(file = app_sys("www")),
    script = "bfh-excel-adapter.js",
    stylesheet = "bfh-excel-adapter.css"
  )
}

excel_adapter_enabled <- function(cfg) isTRUE(cfg$excel_adapter)
```

Tilføj dependency efter `.jexcel_theme_css()` i `app_ui()`; rækkefølgen er
vigtig, så adapterspecifik CSS kan overskrive legacy-temaet.

Wrap kun opt-in-outputtet:

```r
grid <- excelR::excelOutput(ns("tbl"), width = "100%", height = "auto")
if (excel_adapter_enabled(cfg)) {
  div(class = "bfh-excel-grid", `data-bfh-adapter` = "true", grid)
} else {
  grid
}
```

- [ ] **Step 4: Tilføj generation uden at koble den til kanoniske rows**

I servermodulet oprettes:

```r
grid_generation <- reactiveVal(1L)
render_revision <- reactiveVal(0L)
force_grid_render <- function() {
  grid_generation(isolate(grid_generation()) + 1L)
  render_revision(isolate(render_revision()) + 1L)
}
```

`renderExcel()` læser kun `render_revision()` reaktivt; både `rows()` og
`grid_generation()` læses med `isolate()`. Adapterparametrene sendes kun ved
opt-in:

```r
adapter_args <- if (excel_adapter_enabled(cfg)) {
  list(tableOverflow = TRUE,
       tableHeight = "calc(100vh - 250px)",
       pagination = FALSE,
       selectionCopy = TRUE)
} else list()
```

Kald `excelR::excelTable` med `do.call()` så legacy-widgetens JSON forbliver
uændret. Erstat eksisterende `refresh()`-bumps med `force_grid_render()` ved
opret, slet og legacy-revert. En normal adapter-save må ikke kalde funktionen.

For adaptercfg observeres kun `render_revision()`. Registrér i denne observer
en `session$onFlushed(..., once = TRUE)`, som kalder
`send_excel_adapter_init(session, "tbl", isolate(grid_generation()))`. Dermed
modtager browseren generationen efter hver bevidst fuld render, men ikke når
kun `rows()` patches lokalt.

- [ ] **Step 5: Opret JS-adapterens init-skal**

`inst/www/bfh-excel-adapter.js` skal være en IIFE med strict mode og én global
Shiny custom-message-handler. Initialisering sker idempotent pr. DOM-container
efter excelR har sat `container.excel`:

```js
(function () {
  "use strict";
  const states = new WeakMap();

  function requestedGeneration(container) {
    const value = Number(container.dataset.bfhGeneration);
    return Number.isInteger(value) ? value : null;
  }

  function attach(container) {
    const grid = container.excel;
    const generation = requestedGeneration(container);
    if (!grid || generation === null) return;
    const old = states.get(container);
    if (old && old.grid === grid && old.generation === generation) return;
    // Opret state og komponér callbacks i Task 3.
  }

  function scan() {
    document.querySelectorAll(".bfh-excel-grid .jexcel_container").forEach(attach);
  }

  new MutationObserver(scan).observe(document.documentElement,
    { childList: true, subtree: true });
  document.addEventListener("shiny:connected", scan);
  document.addEventListener("DOMContentLoaded", scan);
})();
```

Handleren for `bfh-excel-adapter:init` finder containeren ved payloadens fulde
`id`, sætter `container.dataset.bfhGeneration` og kalder `attach(container)`.
Hvis init kommer før excelR er konstrueret, vil MutationObserverens næste scan
færdiggøre attach. En ny generation erstatter state og callbacks idempotent.

Mutationsobserveren må kun scanne opt-in-wrapperen. Den må ikke ændre callbacks
eller DOM på legacy-grids.

- [ ] **Step 6: Tilføj tæt, adapterafgrænset CSS**

CSS-selectors skal alle begynde med `.bfh-excel-grid`. Implementér:

```css
.bfh-excel-grid .jexcel_content {
  max-height: calc(100vh - 250px) !important;
  overflow: auto !important;
  margin-bottom: 0 !important;
}
.bfh-excel-grid .jexcel > thead > tr > td {
  position: sticky !important;
  top: 0;
  z-index: 3;
}
.bfh-excel-grid .jexcel > tbody > tr { height: 28px; }
.bfh-excel-grid .jexcel > tbody > tr > td {
  padding: 2px 6px;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.bfh-excel-grid .jexcel td.readonly { background: #f1f3f5; }
.bfh-excel-grid td.bfh-cell-pending { background: #fff3bf !important; }
.bfh-excel-grid td.bfh-cell-saved { background: #d3f9d8 !important; }
.bfh-excel-grid td.bfh-cell-rejected { background: #ffe3e3 !important; }
.bfh-excel-grid.bfh-grid-locked { opacity: .72; pointer-events: none; }
```

Bevar excelR's aktive ramme og dropdown-editor; sæt ikke `outline`, `display`
eller `position` på body-celler.

- [ ] **Step 7: Kør fokuserede tests og commit**

Run: `Rscript -e 'devtools::test(filter = "excel-adapter|mod-lookup", stop_on_failure = TRUE)'`

Expected: PASS for dependency, opt-in og alle eksisterende legacy-tests.

```bash
git add R/fct_excel_adapter.R R/app_ui.R R/mod_lookup_table.R \
  inst/www/bfh-excel-adapter.js inst/www/bfh-excel-adapter.css \
  tests/testthat/test-mod-lookup.R
git commit -m "feat(adapter): add opt-in excel grid shell"
```

---

## Task 3: Implementér klientens entydige change-, selection- og result-flow

**Files:**

- Modify: `inst/www/bfh-excel-adapter.js`
- Create: `tests/testthat/apps/excel-adapter/app.R`
- Create: `tests/testthat/test-browser-excel-adapter.R`
- Modify: `DESCRIPTION`

- [ ] **Step 1: Opret en rigtig browserfixture med forsinket fake server**

Fixture-appen skal bruge den rigtige dependency og et rigtigt
`excelR::excelTable`, men et lille selvstændigt Shiny-serverflow, så
browserkontrakten kan TDD-implementeres før opslagmodulet migreres. Den viser én
adapter-wrapper med mindst 40 rækker, tekst-, numeric-, checkbox- og
autocomplete-kolonner, mindst 600 dropdown-options og nok brede tekstkolonner
til vandret scroll. Den eksponerer antal events samt seneste event i
tekstoutputs. Fake serveren skal:

- vente ca. 350 ms før svar for at gøre flere samtidige pending-celler
  observerbare;
- svare `saved` med kanonisk værdi på almindelige events;
- svare `rejected` med gammel værdi ved `AFVIS`;
- kunne sende et kunstigt forsinket, ældre svar til samme celle;
- kunne sende `lock_grid = TRUE` ved `LAAS`.

Serveren sender først `bfh-excel-adapter:init` efter widget-flush. Der må ikke
bruges database eller netværk.

- [ ] **Step 2: Skriv den første browsertest og bekræft RED**

Tilføj `shinytest2` til `Suggests` og guard testen med:

```r
skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")
skip_on_cran()
```

Start `AppDriver$new("apps/excel-adapter", load_timeout = 15000)` og brug
`run_js()` til at dobbeltklikke den første redigerbare celle, skrive en værdi og
trykke Enter. Assertér:

- cellen flyttes lokalt med det samme;
- `.bfh-cell-pending` ses før den forsinkede ack;
- write count bliver præcis 1;
- `.bfh-cell-saved` ses uden at grid-containerens DOM-identitet ændres;
- den aktive celle er næste række i samme kolonne.

Run: `Rscript -e 'devtools::test(filter = "browser-excel-adapter", stop_on_failure = TRUE)'`

Expected: FAIL fordi JS endnu ikke sender celleevents eller håndterer svar.

- [ ] **Step 3: Implementér change-eventet uden legacy-callback**

Ved `attach()` gemmes state:

```js
{
  grid,
  generation,
  sequence: 0,
  latestByCell: new Map(),
  suppressChange: false,
  savedTimers: new Map()
}
```

Erstat `grid.options.onchange`; kald ikke excelR's oprindelige handler på et
adaptergrid. Callbacken skal:

1. ignorere `suppressChange`;
2. læse PK med `grid.getValueFromCoords(0, y)`;
3. danne `event_id` som `generation + ":" + (++sequence)`;
4. registrere seneste event under `String(rowPk) + ":" + x`;
5. markere `grid.records[y][x]` pending og sætte en kort `title`;
6. sende følgende med event-prioritet:

```js
Shiny.setInputValue(container.id + "_cell", {
  event_id: eventId,
  grid_generation: generation,
  row_pk: rowPk,
  column_index: x,
  raw_value: value
}, { priority: "event" });
```

Hvis Shiny ikke er connected, låses grid'et og cellen får en tekstlig fejl via
`title`; der må ikke oprettes en lokal offline-kø.

- [ ] **Step 4: Implementér separat selection-event**

Erstat `grid.options.onselection` på adaptergrids og send:

```js
Shiny.setInputValue(container.id + "_selection", {
  grid_generation: generation,
  boundaries: { top, bottom, left, right },
  row_pks: uniquePksFromCurrentGridOrder
}, { priority: "event" });
```

Normalisér omvendt trækselektion med `Math.min/Math.max`. PK'er læses fra
`grid.getValueFromCoords(0, y)`, ikke fra serverrækkefølgen. Callbacken må ikke
sende eller diff'e fulde tabeldata.

- [ ] **Step 5: Implementér ack/reject med stale-response guard**

Registrér én handler for `bfh-excel-adapter:result`. Find containeren ved
payloadens fulde `id`; ignorer svar hvis container/state mangler, generationen
afviger, eller `latestByCell.get(cellKey) !== event_id`.

Ved `saved` og `rejected` sættes serverens kanoniske værdi sådan:

```js
state.suppressChange = true;
try {
  state.grid.setValueFromCoords(x, y, message.value == null ? "" : message.value);
} finally {
  state.suppressChange = false;
}
```

Lokalisér den aktuelle række igen via PK før patch; sortering kan have flyttet
den siden write. `saved` får grøn klasse i ca. 900 ms. `rejected` får rød klasse
og sikker tekst i `title`. `lock_grid = TRUE` tilføjer `.bfh-grid-locked` og
må ikke fjernes af et senere stale svar.

- [ ] **Step 6: Udvid browsertesten til tastatur, samtidige events og stale svar**

Test i rigtig browser:

- Enter, Tab, Shift+Tab, piletaster og Escape bevarer excelR-adfærden;
- tre hurtige ændringer i forskellige celler bliver tre pending writes og
  præcis tre DB-kald;
- ændring efterfulgt straks af markørflytning tabes ikke;
- checkboxændring efterfulgt straks af tekstændring giver to entydige events;
- autocomplete kan filtrere og vælge blandt mindst 600 muligheder;
- Ctrl+C kopierer den markerede celle/region uden at skabe et write-event;
- en ældre kunstigt forsinket ack på samme celle kan ikke overskrive en nyere;
- sortér på kolonneoverskriften før edit og kontrollér at PK, ikke position,
  bestemmer write og svarcelle;
- scroll både lodret og vandret; `scrollTop`, `scrollLeft` og container-DOM-
  identitet er uændret gennem ack/reject;
- `AFVIS` gendanner kun den ene celle, viser tekst og lader nabocellen stå;
- fixturekommandoen `LAAS` låser grid'et fail-closed.

- [ ] **Step 7: Verificér pastekontrakten eksplorativt og kod resultatet som test**

Indsæt et 2x2 TSV-område i browserfixturen og tæl events. Hvis jspreadsheet
3.9.1 udsender præcis én entydig `onchange` pr. celle, forvent fire skriverier
og dokumentér at de er uafhængige. Hvis den ikke gør, tilføj `onbeforepaste`
som returnerer `false` på adaptergrids. Den sender i så fald en prioriteret
`container.id + "_client_status"`-event med den sikre tekst
`"Indsætning af flere celler understøttes ikke endnu."`; fixture og migreret
modul viser teksten i statusområdet. Test både afvisningen og teksten. Forsøg
ikke at rekonstruere en batch ud fra fuldtabeldata.

- [ ] **Step 8: Kør browsertesten og commit**

Run:

```bash
node --check inst/www/bfh-excel-adapter.js
Rscript -e 'devtools::test(filter = "browser-excel-adapter", stop_on_failure = TRUE)'
```

Expected: PASS uden browser-consolefejl.

```bash
git add DESCRIPTION inst/www/bfh-excel-adapter.js \
  tests/testthat/apps/excel-adapter/app.R \
  tests/testthat/test-browser-excel-adapter.R
git commit -m "feat(adapter): send reliable cell events"
```

---

## Task 4: Migrér serverflowet for opslagstabellen med tvetydig-fejl reconciliation

**Files:**

- Modify: `R/mod_lookup_table.R`
- Modify: `tests/testthat/test-mod-lookup.R`

- [ ] **Step 1: Gør fake DB'en til en reel mutérbar store**

Udvid `fake_lookup_db()` med `fail = c("none", "before", "after", "reload")`.
`update_cell()` skal ændre store på succes, `after` skal ændre store og derefter
kaste, og `.store()` skal gøre kanonisk DB-tilstand observerbar i testen.

Tilføj en injicerbar svarfunktion til servermodulet:

```r
mod_lookup_table_server <- function(id, db, cfg,
                                    adapter_reply = send_excel_adapter_result)
```

Produktions-defaulten sender custom message; tests kan opsamle svar uden at
afhænge af MockShinySession-internals.

- [ ] **Step 2: Skriv modultests for adapterens succesvej og legacy-isolation**

Med adaptercfg og et payload på `input$tbl_cell` skal testen kræve:

- præcis ét `db$update_cell(1L, "navn", "Nyt")`;
- `rows()$navn[1] == "Nyt"`;
- svar `status == "saved"` med samme event/generation;
- `grid_generation()` og `render_revision()` er uændrede;
- et efterfølgende legacy `input$tbl` med ændret fuldtabeldata giver nul ekstra
  writes;
- den eksisterende legacycfg gemmer fortsat via `input$tbl`.

- [ ] **Step 3: Kør modultesten og bekræft RED**

Run: `Rscript -e 'devtools::test(filter = "mod-lookup", stop_on_failure = TRUE)'`

Expected: FAIL fordi `input$tbl_cell` endnu ikke observeres.

- [ ] **Step 4: Byg lookup-kolonnemappingen på serveren**

Lav en modul-lokal pure helper eller fælles helper, der udleder map fra den
faktisk renderede rækkefølge og `cfg$cols`:

```r
lookup_excel_adapter_map <- function(cfg, col_names) {
  # Én række pr. col_names; PK read-only; ukendt felt read-only.
  # value_type er cfg type, ellers "text".
}
```

Kald `validate_excel_adapter_map()` én gang pr. render-generation. Map og data
skal komme fra samme `names(d)` som `excelTable()`; genbrug ikke browserheaders.

- [ ] **Step 5: Implementér det nye celleobserver-flow**

Kun når `excel_adapter_enabled(cfg)`:

```r
observeEvent(input$tbl_cell, {
  event <- prepare_excel_cell_update(
    input$tbl_cell,
    isolate(grid_generation()),
    isolate(rows()),
    cfg$pk,
    isolate(column_map())
  )
  # Reject før DB hvis !event$ok.
  # Ellers kald db$update_cell præcis én gang.
}, ignoreInit = TRUE, priority = 100)
```

På valideringsfejl returneres kendt kanonisk celleværdi, hvis PK/indeks sikkert
kan identificeres; ellers `rejected` med `lock_grid = TRUE`. På succes patches
`rows` med `patch_excel_cell()` og der sendes `saved`. Kald ikke
`force_grid_render()`.

Legacy-observeren skal starte med en eksplicit gren:

```r
if (excel_adapter_enabled(cfg)) return()
```

så adaptergrids aldrig diffes eller skrives via `input$tbl`.

- [ ] **Step 6: Implementér DB-reconciliation efter exception**

Efter en fanget `update_cell()`-exception:

1. log via eksisterende `safe_operation()`-mønster uden at vise exceptiontekst;
2. kald `db$list_rows()` én gang;
3. match præcis én PK og den servermappede kolonne;
4. er DB-værdien lig den ønskede typede værdi, patch lokal data og send
   `saved` (post-commit exception);
5. ellers erstat kun den kanoniske række/celle lokalt og send `rejected` med
   faktisk DB-værdi;
6. fejler genlæsningen eller er PK tvetydig, send `rejected`,
   `lock_grid = TRUE` og beskeden
   `"Databasestatus kunne ikke bekræftes. Genindlæs siden."`.

Grid'et må ikke låses op automatisk på grundlag af lokal state. En fuld
sidegenindlæsning eller en senere eksplicit, verificeret reload er nødvendig.

- [ ] **Step 7: Test alle fejlgrene og selektion efter sortering**

Tilføj tests for:

- before-write exception + vellykket reread -> `rejected`, gammel værdi;
- after-write exception + vellykket reread -> `saved`, ny værdi;
- write og reread fejler -> `rejected`, `lock_grid = TRUE`;
- fejl på én celle ændrer ikke naboer;
- ukendt/read-only kolonne kalder aldrig DB;
- stale generation kalder aldrig DB;
- `input$tbl_selection$row_pks` bestemmer `sel_pk`, også efter klient-sortering;
- `input$tbl_client_status$message` kan kun vælge mellem en serverdefineret
  allowlist af sikre beskeder og kan ikke injicere vilkårlig notifikationstekst;
- add/delete bumper både generation og render revision præcis én gang.

- [ ] **Step 8: Kør lookup- og adaptertests og commit**

Run: `Rscript -e 'devtools::test(filter = "excel-adapter|mod-lookup", stop_on_failure = TRUE)'`

Expected: PASS for både adapter- og legacycfg.

```bash
git add R/mod_lookup_table.R tests/testthat/test-mod-lookup.R
git commit -m "feat(lookup): persist adapter cell events"
```

---

## Task 5: Aktivér kun `faggrupper` og bevis regressionsgrænsen

**Files:**

- Modify: `R/metadata.R`
- Modify: `tests/testthat/test-metadata.R`
- Modify: `tests/testthat/test-sql.R`
- Modify: `tests/testthat/test-browser-excel-adapter.R`
- Create: `tests/testthat/apps/lookup-adapter/app.R`

- [ ] **Step 1: Skriv metadataregressionen først**

```r
test_that("kun faggrupper er optet ind i excel-adapter batch 1", {
  enabled <- Filter(function(cfg) isTRUE(cfg$excel_adapter), LOOKUP_TABLES)
  expect_equal(vapply(enabled, `[[`, "", "id"), "faggrupper")
})
```

Run: `Rscript -e 'devtools::test(filter = "metadata", stop_on_failure = TRUE)'`

Expected: FAIL, fordi ingen tabel endnu er optet ind.

- [ ] **Step 2: Opt `faggrupper` ind**

Tilføj kun:

```r
list(id = "faggrupper", ..., excel_adapter = TRUE, ...)
```

Ingen anden cfg ændres. Ingen SQL eller DB-kontrakt ændres.

- [ ] **Step 3: Tilføj en rigtig modul-integrationfixture**

Opret `apps/lookup-adapter/app.R` med det rigtige
`mod_lookup_table_ui/server`, en mutérbar in-memory DB og to cfg'er: én
adaptercfg og én legacycfg. Tilføj browserassertioner for at adaptergrid'et
sender præcis ét write, patches uden re-render og gendanner en afvist celle via
DB-reread.

Assertér desuden at legacygrid'et:

- ikke har `.bfh-excel-grid` wrapper;
- stadig har excelR's originale `onchange`;
- kan gemme én ændring via fuldtabelpayload;
- ikke sender et `_cell`-input.

Det er regressionsbeviset for, at batch 1 ikke halv-migrerer de øvrige tabeller.

- [ ] **Step 4: Kør metadata-, SQL-, lookup- og browsertests**

Run:

```bash
Rscript -e 'devtools::test(filter = "metadata|sql|excel-adapter|mod-lookup|browser-excel-adapter", stop_on_failure = TRUE)'
```

Expected: PASS.

- [ ] **Step 5: Commit pilotopt-in**

```bash
git add R/metadata.R tests/testthat/test-metadata.R tests/testthat/test-sql.R \
  tests/testthat/test-browser-excel-adapter.R
git commit -m "feat(lookup): enable adapter for faggrupper"
```

---

## Task 6: Verificér pakken og stop ved manuel batch-1-røgtest

**Files:**

- Modify if generated: `NAMESPACE`, `man/*`
- Do not modify: `.Rbuildignore`, `Renviron-kopi.txt`

- [ ] **Step 1: Kør formattering/lint kun hvis projektet allerede har en kommando**

Undersøg `Makefile`, CI og eksisterende scripts. Introducér ikke et nyt
formatteringsværktøj i denne batch. Hvis ingen kommando findes, spring dette
trin over og dokumentér det.

- [ ] **Step 2: Kør hele testpakken fra ren R-proces**

Run: `Rscript -e 'devtools::test(stop_on_failure = TRUE)'`

Expected: alle tests PASS. Rapportér browser- og ikke-browserresultater særskilt
og angiv eksplicit, hvis en browsertest blev skipped.

- [ ] **Step 3: Kør package check**

Run: `Rscript -e 'devtools::check(error_on = "warning", document = FALSE)'`

Expected: 0 errors, 0 warnings. Eksisterende kendte NOTEs skal opregnes; nye
NOTEs skal undersøges før fortsættelse.

- [ ] **Step 4: Inspicér diff og arbejdsområde**

Run:

```bash
git diff --check
git status --short --branch
git diff origin/main...HEAD --stat
```

Expected: ingen whitespacefejl; kun planlagte adapterfiler plus brugerens
allerede eksisterende `.Rbuildignore` og `Renviron-kopi.txt`. De to sidstnævnte
må ikke være staged eller indgå i commits.

- [ ] **Step 5: Udfør manuel røgtest på `faggrupper`**

Start den installerede app med write aktiveret mod den godkendte Supabase-
konfiguration. Brug en eksisterende ufarlig faggruppeværdi og notér dens PK og
originalværdi før ændringen.

Kontrollér:

1. `faggrupper` viser kompakte ca. 28 px rækker, intern scroll og sticky header.
2. Redigér cellen, tryk Enter og fortsæt straks til næste celle.
3. Pending og kort saved-status ses; fokus, scroll og sortering flytter sig ikke
   på grund af serverens svar.
4. Fuld browserreload viser den gemte værdi fra Supabase.
5. Gendan originalværdien, reload igen og verificér gendannelsen.
6. Åbn mindst én ikke-migreret opslagstabel og verificér, at dens eksisterende
   redigering stadig virker.

- [ ] **Step 6: STOP og rapportér batchresultatet**

Rapportér commit-ID'er, fokuserede tests, browsertest, fuld suite, check og den
manuelle røgtests gendannelsesbevis. Marker batch 1 som bestået eller ikke
bestået. Opret ikke indikatorplan og begynd ikke indikator-grid'et uden brugerens
næste udtrykkelige godkendelse.
