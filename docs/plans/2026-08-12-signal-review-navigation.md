# Signal-gennemgang: Navigation + Fase-statistik — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Klikbar diagram-liste, checkbox "vis også uden signal", og fase-statistik-tabel (serielængde/kryds, observeret + forventet) på signal-gennemgang-siden.

**Architecture:** Én sandhedskilde `scanned_list` (alle ok-scannede diagrammer + `signal`/`status`-kolonner) med afledt `view_list` (checkbox-filter). `cursor` peger ind i `view_list`; cursor bevares ved filter-toggle via diagram_id-remap. Fase-statistik læses fra `display_qic()$summary` (BFHcharts `format_qic_summary`) — delt reactive mellem graf og tabel sikrer preview-konsistens. Design: `docs/plans/2026-08-11-signal-review-navigation-design.md`.

**Tech Stack:** R, Shiny (moduleServer), bslib accordion, shiny::testServer, BFHcharts bfh_qic.

**Branch:** `feat/signal-review-navigation` (allerede oprettet, indeholder DB-resilience-fix).

**Vigtige forudsætninger for implementøren:**
- R er IKKE i PATH. Kør altid: `'/c/Program Files/R/R-4.6.0/bin/Rscript.exe'` (Git Bash).
- Testene i `tests/testthat/test-mod-signal-review.R` evalueres INDE i modulets environment (shiny::testServer), så `scanned_list`, `view_list`, `cursor` osv. tilgås direkte ved navn — de behøver IKKE være i modulets retur-liste for at kunne testes, men eksponér dem alligevel (konvention i filen).
- Eksisterende tests kalder `signal_list()` — det navn SKAL fortsat findes internt i modulet (bagudkompat-reactive, se Task 2).
- Brug `–` (–), `⚠` (⚠), `✓` (✓) escapes i R-kode/tests — ingen rå specialtegn (Windows-encoding).
- `drain_scan()` er en eksisterende test-helper der afvikler det progressive scans later-kø.
- Ingen Claude-attribution i commit-beskeder (GIT_WORKFLOW.md).

---

### Task 1: Rene helpers `scan_view_filter` + `phase_stats_df` i fct_scan.R

**Files:**
- Modify: `R/fct_scan.R` (append i bunden)
- Test: `tests/testthat/test-scan.R` (append i bunden)

**Step 1: Skriv fejlende tests**

Append i `tests/testthat/test-scan.R`:

```r
test_that("scan_view_filter: NULL forbliver NULL; show_all styrer signal-filter", {
  expect_null(scan_view_filter(NULL, FALSE))
  expect_null(scan_view_filter(NULL, TRUE))
  sl <- data.frame(diagram_id = 1:3, signal = c(TRUE, FALSE, NA))
  expect_equal(scan_view_filter(sl, FALSE)$diagram_id, 1L)   # NA taeller ikke som signal
  expect_equal(scan_view_filter(sl, TRUE)$diagram_id, 1:3)
  # 0-raekke ind -> 0-raekke ud (ej NULL): nav-laget skelner "ej scannet" fra "tom"
  expect_equal(nrow(scan_view_filter(sl[0, , drop = FALSE], FALSE)), 0L)
})

test_that("phase_stats_df formaterer observeret/forventet pr. fase som PDF'erne", {
  s <- data.frame(fase = 1:2, antal_observationer = c(12L, 12L),
    anvendelige_observationer = c(12L, 11L),
    laengste_loeb = c(5L, 8L), laengste_loeb_max = c(7L, 7L),
    antal_kryds = c(6L, 2L), antal_kryds_min = c(4L, 4L),
    anhoej_signal = c(FALSE, TRUE))
  out <- phase_stats_df(s)
  expect_equal(nrow(out), 2L)
  expect_equal(out$Fase, 1:2)
  expect_equal(out[["Serielængde"]][1], "5 / maks. 7")
  expect_equal(out[["Antal kryds"]][2], "2 / min. 4")
  expect_equal(out[["Obs."]][2], "12 (11 anv.)")
  expect_equal(out$Signal, c("–", "⚠ signal"))
})

test_that("phase_stats_df taaler NULL, tom df og NA-vaerdier", {
  expect_null(phase_stats_df(NULL))
  expect_null(phase_stats_df(data.frame()))
  s <- data.frame(fase = 1L, antal_observationer = NA_integer_,
    anvendelige_observationer = NA_integer_,
    laengste_loeb = NA_integer_, laengste_loeb_max = NA_integer_,
    antal_kryds = NA_integer_, antal_kryds_min = NA_integer_,
    anhoej_signal = NA)
  out <- phase_stats_df(s)
  expect_equal(out[["Serielængde"]], "– / maks. –")
  expect_equal(out[["Antal kryds"]], "– / min. –")
  expect_equal(out$Signal, "–")
})
```

**Step 2: Kør tests — verificér FAIL**

Run: `'/c/Program Files/R/R-4.6.0/bin/Rscript.exe' -e "devtools::load_all('.', quiet=TRUE); testthat::test_file('tests/testthat/test-scan.R', reporter='summary')"`
Expected: FAIL med "could not find function \"scan_view_filter\"" / "phase_stats_df".

**Step 3: Implementér helpers**

Append i `R/fct_scan.R`:

```r
#' Filtrér den scannede liste til visning. show_all = FALSE → kun diagrammer
#' med signal; TRUE → alle rækker (scanned indeholder kun ok-scannede —
#' ingen_data/fejl optages aldrig, de har ingen tegnbar graf).
#' NULL ind → NULL ud (skelner "ej scannet endnu" fra "tom visning").
#' @noRd
scan_view_filter <- function(scanned, show_all = FALSE) {
  if (is.null(scanned)) return(NULL)
  if (isTRUE(show_all)) return(scanned)
  scanned[scanned$signal %in% TRUE, , drop = FALSE]
}

#' Fase-statistik-df til visning under grafen. Spejler PDF-rapporternes
#' SPC-tabel: observeret serielængde mod forventet maks., observeret antal
#' kryds mod forventet min. — pr. fase, fra BFHcharts format_qic_summary
#' (bfh_qic-resultatets $summary).
#' @param summary df med fase, antal_observationer, anvendelige_observationer,
#'   laengste_loeb(_max), antal_kryds(_min), anhoej_signal — el. NULL
#' @return df med danske visningskolonner el. NULL (intet at vise)
#' @noRd
phase_stats_df <- function(summary) {
  if (is.null(summary) || !is.data.frame(summary) || nrow(summary) == 0) {
    return(NULL)
  }
  fmt <- function(x) ifelse(is.na(x), "–", as.character(x))
  out <- data.frame(
    Fase = as.integer(summary$fase),
    check.names = FALSE, stringsAsFactors = FALSE)
  out[["Obs."]] <- sprintf("%s (%s anv.)", fmt(summary$antal_observationer),
                           fmt(summary$anvendelige_observationer))
  out[["Serielængde"]] <- sprintf("%s / maks. %s",
    fmt(summary$laengste_loeb), fmt(summary$laengste_loeb_max))
  out[["Antal kryds"]] <- sprintf("%s / min. %s",
    fmt(summary$antal_kryds), fmt(summary$antal_kryds_min))
  out[["Signal"]] <- ifelse(summary$anhoej_signal %in% TRUE,
                            "⚠ signal", "–")
  out
}
```

**Step 4: Kør tests — verificér PASS**

Run: samme kommando som Step 2.
Expected: PASS (alle scan-tests grønne, ingen failures).

**Step 5: Commit**

```bash
git add R/fct_scan.R tests/testthat/test-scan.R
git commit -m "feat(signal): helpers til visnings-filter + fase-statistik-df

scan_view_filter (checkbox-filter over scannet liste) og phase_stats_df
(PDF-spejlende SPC-tabel: serielaengde obs/maks, kryds obs/min pr. fase).
Rene funktioner i headless scan-laget - unit-testet uden Shiny."
```

---

### Task 2: State-model — `scanned_list` + `view_list` + cursor-bevarelse

**Files:**
- Modify: `R/mod_signal_review.R` (server-delen)
- Test: `tests/testthat/test-mod-signal-review.R` (append)

**Step 1: Skriv fejlende tests**

Append i `tests/testthat/test-mod-signal-review.R`. Genbrug fixture-mønstret fra filens første test (`build_fixture()` giver `ind_sig` med signal + `ind_flat` uden):

```r
test_that("checkbox 'vis ogsaa uden signal' udvider visningen til alle ok-scannede", {
  skip_if_not_installed("arrow")
  base <- build_fixture()
  idx <- data.frame(diagram_id = c(1L, 2L), indikator_id = c(1L, 2L),
    indikator_navn = c("Sig", "Flad"),
    indikator_navn_teknisk = c("ind_sig", "ind_flat"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    expect_equal(view_list()$diagram_id, 1L)              # default: kun signal
    expect_setequal(scanned_list()$diagram_id, c(1L, 2L)) # begge er ok-scannede
    session$setInputs(show_no_signal = TRUE)
    expect_setequal(view_list()$diagram_id, c(1L, 2L))    # + ok uden signal
    expect_equal(signal_list()$diagram_id, 1L)            # kompat-reaktiv uaendret
    session$setInputs(show_no_signal = FALSE)
    expect_equal(view_list()$diagram_id, 1L)
  })
})

test_that("cursor bevares ved checkbox-toggle (samme diagram forbliver aktivt)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  # a + c har signal, b er flad -> view uden checkbox: 10,30; med: 10,20,30
  for (ind in c("a", "c")) {
    dir.create(file.path(base, ind))
    arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
      vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
      naevner = NA_real_, enhed = "e"), file.path(base, ind, "p.parquet"))
  }
  dir.create(file.path(base, "b"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = rep(c(4, 6), 12), taeller = NA_real_, naevner = NA_real_,
    enhed = "e"), file.path(base, "b", "p.parquet"))
  idx <- data.frame(diagram_id = c(10L, 20L, 30L), indikator_id = 1:3,
    indikator_navn = c("A", "B", "C"), indikator_navn_teknisk = c("a", "b", "c"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    session$setInputs(show_no_signal = TRUE)     # view: 10,20,30
    session$setInputs(next_ = 1)
    session$setInputs(next_ = 2)
    expect_equal(current_diagram()$diagram_id, 30L)
    session$setInputs(show_no_signal = FALSE)    # view: 10,30 -> 30 er pos 2
    expect_equal(current_diagram()$diagram_id, 30L)
    expect_equal(cursor(), 2L)
    session$setInputs(show_no_signal = TRUE)     # view: 10,20,30 -> 30 er pos 3
    expect_equal(current_diagram()$diagram_id, 30L)
    expect_equal(cursor(), 3L)
  })
})

test_that("diagram der forsvinder fra visningen ved toggle -> cursor = 1", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "a"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
    naevner = NA_real_, enhed = "e"), file.path(base, "a", "p.parquet"))
  dir.create(file.path(base, "b"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = rep(c(4, 6), 12), taeller = NA_real_, naevner = NA_real_,
    enhed = "e"), file.path(base, "b", "p.parquet"))
  idx <- data.frame(diagram_id = c(10L, 20L), indikator_id = 1:2,
    indikator_navn = c("A", "B"), indikator_navn_teknisk = c("a", "b"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    session$setInputs(show_no_signal = TRUE)   # view: 10,20
    session$setInputs(next_ = 1)               # staa paa 20 (flad)
    expect_equal(current_diagram()$diagram_id, 20L)
    session$setInputs(show_no_signal = FALSE)  # 20 forsvinder -> cursor 1 (10)
    expect_equal(current_diagram()$diagram_id, 10L)
    expect_equal(cursor(), 1L)
  })
})

test_that("ingen_data-diagrammer optages ALDRIG i visningen (heller ej med checkbox)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "a"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
    naevner = NA_real_, enhed = "e"), file.path(base, "a", "p.parquet"))
  dir.create(file.path(base, "b"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = rep(c(4, 6), 12), taeller = NA_real_, naevner = NA_real_,
    enhed = "e"), file.path(base, "b", "p.parquet"))
  # org_id 99 har ingen enhed-varianter -> scan_diagram giver "ingen_data"
  idx <- data.frame(diagram_id = c(10L, 20L), indikator_id = 1:2,
    indikator_navn = c("A", "B"), indikator_navn_teknisk = c("a", "b"),
    datasaet = "d", datapakke = "p", org_id = c(5L, 99L), org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    session$setInputs(show_no_signal = TRUE)
    expect_equal(view_list()$diagram_id, 10L)          # 20 (ingen_data) er ude
    expect_equal(scanned_list()$diagram_id, 10L)
  })
})
```

**Step 2: Kør tests — verificér FAIL**

Run: `'/c/Program Files/R/R-4.6.0/bin/Rscript.exe' -e "devtools::load_all('.', quiet=TRUE); testthat::test_file('tests/testthat/test-mod-signal-review.R', reporter='summary')"`
Expected: FAIL med "object 'view_list' not found" / "object 'scanned_list' not found".

**Step 3: Implementér state-modellen i `R/mod_signal_review.R`**

3a. Erstat state-deklarationen (linjen `signal_list <- reactiveVal(NULL)  # df: diagrammer med signal`) med:

```r
    scanned_list <- reactiveVal(NULL)     # df: ALLE ok-scannede (+ signal/status)
    show_all <- reactiveVal(FALSE)        # checkbox-tilstand (styret via observer,
                                          # saa cursor kan remappes deterministisk)
    # Visningen der bladres i: checkbox fra -> kun signal; til -> alle ok-scannede
    view_list <- reactive(scan_view_filter(scanned_list(), show_all()))
    # Bagudkompat (tests + evt. eksterne laesere): kun signal-diagrammer
    signal_list <- reactive(scan_view_filter(scanned_list(), FALSE))
```

3b. Tilføj toggle-observeren lige efter (cursor-bevarelse — remap via diagram_id, deterministisk fordi gammel OG ny visning beregnes eksplicit i samme handler):

```r
    # Checkbox-toggle: bevar det aktuelle diagram i visningen hvis muligt.
    # show_all opdateres HER (ikke direkte fra input) saa gammel/ny visning kan
    # beregnes side om side - en reaktiv afledning ville miste den gamle
    # position foer remap.
    observeEvent(input$show_no_signal, {
      sl <- scanned_list()
      old_view <- scan_view_filter(sl, show_all())
      old_id <- if (!is.null(old_view) && nrow(old_view) > 0) {
        old_view$diagram_id[min(cursor(), nrow(old_view))]
      } else {
        NULL
      }
      show_all(isTRUE(input$show_no_signal))
      new_view <- scan_view_filter(sl, show_all())
      pos <- if (!is.null(old_id) && !is.null(new_view)) {
        match(old_id, new_view$diagram_id)
      } else {
        NA_integer_
      }
      cursor(if (is.na(pos)) 1L else as.integer(pos))
      preview_parts(NULL)
    }, ignoreInit = TRUE)
```

3c. I `.scan_process_group`: erstat linjen `signal_list(ctx$cand[which(ctx$sig %in% TRUE), , drop = FALSE])` med:

```r
      # Alle ok-scannede optages (signal-flag som kolonne). ingen_data/fejl
      # holdes ude - de har ingen tegnbar graf (design 2026-08-11).
      ok_idx <- which(ctx$status %in% "ok")
      sl <- ctx$cand[ok_idx, , drop = FALSE]
      sl$signal <- ctx$sig[ok_idx] %in% TRUE
      sl$status <- ctx$status[ok_idx]
      scanned_list(sl)
```

3d. I `.scan_finish`: erstat `nrow(isolate(signal_list()))` med `sum(isolate(scanned_list())$signal, na.rm = TRUE)`.

3e. I scan-start-observeren (`observeEvent(input$scan, ...)`): erstat `signal_list(cand[0, , drop = FALSE])` med:

```r
      empty <- cand[0, , drop = FALSE]
      empty$signal <- logical(0)
      empty$status <- character(0)
      scanned_list(empty)
```

3f. `current_diagram`: skift fra `signal_list()` til `view_list()` + defensiv cursor-clamp:

```r
    current_diagram <- reactive({
      vl <- view_list()
      if (is.null(vl) || nrow(vl) == 0) return(NULL)
      as.list(vl[min(cursor(), nrow(vl)), ])
    })
```

3g. `observeEvent(input$next_)` og `observeEvent(input$prev)`: skift `sl <- signal_list()` til `sl <- view_list()` (resten uændret).

3h. `output$nav_label`: skift `sl <- signal_list()` til `sl <- view_list()`, og ret teksten `"Scannet — 0 diagrammer med signal"` til `"Scannet — 0 diagrammer i visningen"`.

3i. Modulets retur-liste (nederst): tilføj `scanned_list = scanned_list, view_list = view_list` (behold `signal_list = signal_list`).

**Step 4: Kør HELE modulets testfil — verificér PASS inkl. alle eksisterende tests**

Run: samme kommando som Step 2.
Expected: PASS. Ingen failures — de eksisterende tests (der læser `signal_list()`) skal være grønne via kompat-reaktiven.

**Step 5: Commit**

```bash
git add R/mod_signal_review.R tests/testthat/test-mod-signal-review.R
git commit -m "feat(signal): scanned_list/view_list-state med checkbox-filter

Alle ok-scannede diagrammer beholdes (signal som kolonne); visningen
afledes af checkbox 'vis ogsaa uden signal'. Cursor remappes via
diagram_id ved toggle, saa brugeren ikke mister sin plads. signal_list
bevaret som bagudkompatibel reaktiv (kun signal-raekker)."
```

---

### Task 3: Klik-navigation (`goto_diagram`) + sidebar-liste (accordion)

**Files:**
- Modify: `R/mod_signal_review.R` (UI + server)
- Test: `tests/testthat/test-mod-signal-review.R` (append)

**Step 1: Skriv fejlende tests**

```r
test_that("goto_diagram springer direkte til valgt diagram; ukendt id ignoreres", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  for (ind in c("a", "b")) {
    dir.create(file.path(base, ind))
    arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
      vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
      naevner = NA_real_, enhed = "e"), file.path(base, ind, "p.parquet"))
  }
  idx <- data.frame(diagram_id = c(10L, 20L), indikator_id = 1:2,
    indikator_navn = c("A", "B"), indikator_navn_teknisk = c("a", "b"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    session$setInputs(goto_diagram = 20)
    expect_equal(current_diagram()$diagram_id, 20L)
    expect_equal(cursor(), 2L)
    session$setInputs(goto_diagram = 999)      # ukendt/stale id -> ingen aendring
    expect_equal(current_diagram()$diagram_id, 20L)
    expect_equal(cursor(), 2L)
  })
})

test_that("diagram_list renderer raekker for visningen", {
  skip_if_not_installed("arrow")
  base <- build_fixture()
  idx <- data.frame(diagram_id = c(1L, 2L), indikator_id = c(1L, 2L),
    indikator_navn = c("SigNavn", "FladNavn"),
    indikator_navn_teknisk = c("ind_sig", "ind_flat"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    html <- as.character(output$diagram_list$html)
    expect_match(html, "SigNavn")
    expect_no_match(html, "FladNavn")          # checkbox fra -> kun signal
    session$setInputs(show_no_signal = TRUE)
    html2 <- as.character(output$diagram_list$html)
    expect_match(html2, "FladNavn")
  })
})
```

**Step 2: Kør tests — verificér FAIL**

Run: samme testfil-kommando som Task 2.
Expected: FAIL — `goto_diagram` uden effekt (cursor forbliver 1) og `output$diagram_list` findes ikke.

**Step 3: Implementér**

3a. UI — i `mod_signal_review_ui`, efter `uiOutput(ns("scan_summary"))` i sidebar (tilføj komma efter scan_summary-linjen):

```r
      hr(),
      checkboxInput(ns("show_no_signal"), "Vis også diagrammer uden signal",
                    value = FALSE),
      bslib::accordion(id = ns("diagram_acc"), open = FALSE,
        bslib::accordion_panel("Diagramliste", uiOutput(ns("diagram_list"))))
```

3b. Server — tilføj efter toggle-observeren fra Task 2:

```r
    # Klik i sidebar-listen: spring direkte til diagrammet. Id-baseret (ej
    # position) saa et klik paa en stale-renderet liste aldrig rammer forkert
    # raekke - ukendte id'er ignoreres blot.
    observeEvent(input$goto_diagram, {
      vl <- view_list()
      if (is.null(vl) || nrow(vl) == 0) return()
      pos <- match(as.integer(input$goto_diagram), vl$diagram_id)
      if (is.na(pos)) return()
      cursor(as.integer(pos))
      preview_parts(NULL)
    })

    output$diagram_list <- renderUI({
      vl <- view_list()
      if (is.null(vl)) {
        return(div(class = "small text-muted", "Ingen scan endnu"))
      }
      if (nrow(vl) == 0) {
        return(div(class = "small text-muted", "Ingen diagrammer i visningen"))
      }
      cur <- min(cursor(), nrow(vl))
      tags$div(class = "list-group list-group-flush small",
        lapply(seq_len(nrow(vl)), function(i) {
          icon <- if (isTRUE(vl$signal[i])) "⚠" else "✓"
          tags$a(href = "#",
            class = paste("list-group-item list-group-item-action py-1 px-2",
                          if (identical(i, cur)) "active" else ""),
            onclick = sprintf(
              "Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
              session$ns("goto_diagram"), vl$diagram_id[i]),
            sprintf("%s %s · %s", icon, vl$indikator_navn[i], vl$org_navn[i]))
        }))
    })
```

**Step 4: Kør tests — verificér PASS**

Run: samme kommando.
Expected: PASS, hele filen grøn.

**Step 5: Commit**

```bash
git add R/mod_signal_review.R tests/testthat/test-mod-signal-review.R
git commit -m "feat(signal): klikbar diagramliste i sidebar-accordion

Id-baseret goto (stale klik ignoreres), statusikon pr. raekke, aktuel
raekke markeret. Checkbox 'vis ogsaa uden signal' placeret over listen."
```

---

### Task 4: `display_qic` + fase-statistik-tabel med preview-konsistens

**Files:**
- Modify: `R/mod_signal_review.R` (UI + server)
- Test: `tests/testthat/test-mod-signal-review.R` (append)

**Step 1: Skriv fejlende tests**

```r
test_that("fase-statistik vises fra summary og foelger preview-genberegning", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "a"))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
    naevner = NA_real_, enhed = "e"), file.path(base, "a", "p.parquet"))
  idx <- data.frame(diagram_id = 7L, indikator_id = 1L, indikator_navn = "A",
    indikator_navn_teknisk = "a", datasaet = "d", datapakke = "p", org_id = 5L,
    org_teknisk = "E", org_navn = "E", org_niveau = 5L, overafdeling = "OA",
    afdeling = NA, afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    # Foer preview: 1 fase, og tabellen matcher scan-resultatets summary
    expect_equal(nrow(display_qic()$summary), 1L)
    expect_equal(display_qic()$summary$laengste_loeb,
                 .scan_of_current()$summary$laengste_loeb)
    tbl <- paste(as.character(output$phase_stats), collapse = "")
    expect_match(tbl, "maks\\.")
    expect_match(tbl, "min\\.")
    # Preview af faseskift -> genberegning med 2 faser, tabellen foelger med
    session$setInputs(chart_selected = "2020-07-28")
    session$setInputs(preview = 1)
    expect_equal(nrow(display_qic()$summary), 2L)
  })
})

test_that("fase-statistik taaler summary = NULL (fejl-scannet diagram)", {
  skip_if_not_installed("arrow")
  base <- build_fixture()
  idx <- data.frame(diagram_id = c(1L, 2L), indikator_id = c(1L, 2L),
    indikator_navn = c("Sig", "Flad"),
    indikator_navn_teknisk = c("ind_sig", "ind_flat"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    # Forgift cachen: summary + qic_result vaek -> tabel-render maa ikke fejle
    cc <- cache()
    cc[["1|all"]]$qic_result <- NULL
    cc[["1|all"]]$summary <- NULL
    cache(cc)
    expect_no_error(output$phase_stats)
  })
})
```

**Step 2: Kør tests — verificér FAIL**

Run: samme testfil-kommando.
Expected: FAIL med "object 'display_qic' not found".

**Step 3: Implementér**

3a. UI — i `mod_signal_review_ui`, umiddelbart efter `ggiraph::girafeOutput(ns("chart"), height = "420px"),`:

```r
    div(class = "small", tableOutput(ns("phase_stats"))),
```

3b. Server — erstat HELE `output$chart <- ggiraph::renderGirafe({...})`-blokken med en delt reactive + slankere chart-render (preview-genberegningen flytter fra chart-kroppen ind i `display_qic`, så graf OG tabel altid viser samme beregning):

```r
    # Det qic-resultat der VISES lige nu: scan-resultatet, eller preview-
    # genberegningen naar et faseskift forhaandsvises. Delt mellem graf og
    # fase-statistik-tabellen, saa de aldrig kan vise hver sin beregning.
    display_qic <- reactive({
      sc <- .scan_of_current()
      if (is.null(sc) || is.null(sc$qic_result)) return(NULL)
      pv <- preview_parts()
      if (is.null(pv) || is.null(sc$slice)) return(sc$qic_result)
      # Date-normaliseret via preview_break_parts (ingen rbind af Date paa
      # POSIXct). Fejl -> fald tilbage til det scannede resultat.
      safe_operation("preview-genberegning", {
        base_meds <- db$diagram_medians(current_diagram()$diagram_id)
        parts <- preview_break_parts(current_diagram()$diagram_id, base_meds,
                                     pv, sc$slice$dato)
        compute_signal(sc$slice, parts = parts)$qic_result
      }, fallback = sc$qic_result)
    })

    # Hele render-kroppen er fejl-vaernet: en graf der ikke kan bygges
    # (degenereret qic-data, uventede kolonner, ggplot-range-fejl) maa ALDRIG
    # vaelte sessionen — i dev med options(error/shiny.error = browser) endte
    # en uncaught render-fejl ellers i Browse[1] og "froes" appen midt i scan.
    output$chart <- ggiraph::renderGirafe({
      qr <- display_qic()
      if (is.null(qr)) return(NULL)
      g <- safe_operation("tegn diagram",
        interactive_run_chart(qr, selected_date = valid_selected_date()),
        fallback = "fejl")
      if (identical(g, "fejl") || is.null(g)) {
        # validate-tekst vises i grafens plads (graa besked, ingen crash)
        validate(need(FALSE,
          "Diagrammet kan ikke tegnes (ingen tegnbare datapunkter) — se log for detaljer."))
      }
      g
    })

    # Fase-statistik under grafen — samme tal som PDF-rapporternes SPC-tabel.
    output$phase_stats <- renderTable({
      qr <- display_qic()
      if (is.null(qr)) return(NULL)
      phase_stats_df(qr$summary)
    }, striped = FALSE, spacing = "xs", width = "auto", na = "")
```

**OBS:** `qr$summary` er bfh_qic-resultatets summary — identisk med det `compute_signal` returnerer som `summary_all` (og som `scan_diagram` gemmer som `sc$summary`). Preview-genberegningen giver automatisk et nyt `$summary` med de ekstra faser.

3c. Modulets retur-liste: tilføj `display_qic = display_qic`.

**Step 4: Kør tests — verificér PASS (specielt den eksisterende degenereret-qic-test)**

Run: samme testfil-kommando.
Expected: PASS — inkl. den eksisterende test "degenereret qic-data i cache → chart-render giver venlig besked" (beskeden "kan ikke tegnes" er bevaret ordret).

**Step 5: Commit**

```bash
git add R/mod_signal_review.R tests/testthat/test-mod-signal-review.R
git commit -m "feat(signal): fase-statistik-tabel med preview-konsistens

display_qic-reactive deles mellem graf og tabel: serielaengde (obs/maks)
og antal kryds (obs/min) pr. fase - samme tal som PDF-rapporternes
SPC-tabel (BFHcharts format_qic_summary). Preview af faseskift
genberegner begge, saa graf og tal aldrig divergerer."
```

---

### Task 5: Fuld verifikation + oprydning

**Step 1: Kør HELE testsuiten**

Run: `'/c/Program Files/R/R-4.6.0/bin/Rscript.exe' -e "devtools::test(reporter='summary')"`
Expected: 0 failures. Kendte skips (BFHMETA_WRITE!=1) og kendte [ERROR]-log-linjer fra fixtures er OK — se efter `Failed`-sektionen.

**Step 2: Styler + lintr på ændrede filer**

Run:
```bash
'/c/Program Files/R/R-4.6.0/bin/Rscript.exe' -e "styler::style_file(c('R/fct_scan.R','R/mod_signal_review.R'))"
'/c/Program Files/R/R-4.6.0/bin/Rscript.exe' -e "print(lintr::lint('R/fct_scan.R')); print(lintr::lint('R/mod_signal_review.R'))"
```
Expected: styler ændrer intet væsentligt (eller kun whitespace — kør testene igen hvis filer ændres); lintr uden nye fejl.

**Step 3: Manuel røgtest [MANUELT TRIN for bruger]**

`source('dev/run_dev.R')` → Scan → verificér: (1) accordion-listen viser rækker og klik springer, (2) checkbox udvider listen øjeblikkeligt uden re-scan og bevarer aktuelt diagram, (3) fase-tabellen viser obs/forventet og opdaterer ved "Forhåndsvis faseskift".

**Step 4: Commit evt. styler-rettelser**

```bash
git add -u && git commit -m "style: styler paa aendrede signal-filer" || echo "intet at committe"
```

**Step 5: Afslut**

Brug superpowers:finishing-a-development-branch. NEWS.md-entry (MINOR, `feat`) + versionbump håndteres ved release jf. VERSIONING_POLICY.md — ikke i denne branch medmindre brugeren beder om det.
