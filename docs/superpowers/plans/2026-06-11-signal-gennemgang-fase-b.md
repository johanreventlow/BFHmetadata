# Signal-gennemgang Fase B (review-UI) — Implementeringsplan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (anbefalet) eller superpowers:executing-plans til at implementere task-for-task. Steps bruger checkbox (`- [ ]`).

**Goal:** Bygge review-UI'et oven på Fase A's signal-motor: peg app'en på en parquet-mappe, scan filtrerede aktive Seriediagrammer, vis dem med signal i en interaktiv ggiraph-graf, og registrér/fjern faseskift i `tblDiagrammerMedian` ved at klikke en observation.

**Architecture:** Headless scan-lag (`fct_scan.R`) sidder oven på Fase A's `parquet_load_slice`/`resolve_median_breaks`/`compute_signal` + nye DB-accessors for enhed-varianter. Et interaktivt chart-lag (`fct_chart_interactive.R`) bygger ggiraph fra `bfh_qic()$qic_data`. Ét Shiny-modul (`mod_signal_review.R`) orkestrerer sti-input → filtre → scan (cache) → bladr → klik-punkt → forhåndsvis → gem.

**Tech Stack:** R, Shiny, Golem, bslib, ggiraph 0.9.6, BFHcharts 0.25.0, BFHtheme 0.5.2, arrow, dplyr, pool/DBI/RPostgres, testthat ed. 3.

---

## Verificeret kontekst (pin'et før planen — ikke gæt)

**Fase A motor-API (eksisterer, testet):**
- `parquet_indicator_path(base_path, indikator_navn_teknisk)` → mappe-sti (1-niveau discovery).
- `parquet_load_slice(path, enhed = NULL, from = NULL, to = NULL)` → df (`dato, vaerdi, taeller, naevner, enhed`) eller `NULL` hvis enhed angivet men 0 match.
- `parquet_limit_observations(data, max_obs = 36L, date_col = "dato")` → behold seneste N unikke datoer.
- `resolve_median_breaks(diagram_id, all_medians, x_dates)` → integer part-positioner (`min(which(x >= b))`, dropper rp==1/uden-for) eller `NULL`. `all_medians` skal have kolonner `diagram` + `laas_median`.
- `compute_signal(slice, parts = NULL)` → `list(signal, latest, summary_all, qic_result)`. Vælger proportion-gren hvis `"naevner" %in% names && any(!is.na(naevner))` (y=taeller, n=naevner, multiply=100), ellers run på `vaerdi`.
- `db$list_active_seriediagrammer()` → df med `diagram_id, indikator_id, indikator_navn, indikator_navn_teknisk, datasaet, datapakke, org_id, org_teknisk, org_navn, org_niveau, overafdeling, afdeling, afsnit`.
- `db$diagram_medians(diagram_id)` → df med `id, diagram, laas_median`.
- `db$add_median_break(diagram_id, dato)` → ny id (write-guard). `db$delete_median_break(median_id)` (write-guard).

**enhed-mapping (autoritativt fra BFHddl `pipeline.R:825-834` + live schema):**
For et diagram med `org_id` er enhed-varianterne = unikke, ikke-tomme, **lowercase** af:
- `tblOrganisationOversaettelse.organisatorisk_navn_fra_data` WHERE `organisatorisk_navn_teknisk = org_id` (FK'en er **integer** = org_id, ikke en streng), PLUS
- `tblOrganisationStruktur` for org_id: `organisatorisk_navn_teknisk`, `organisatorisk_navn_kort`, `organisatorisk_navn_langt`.

Parquet filtreres `tolower(enhed) %in% variants`. `tblOrganisationOversaettelse` har 404 rækker, kolonner `organisatorisk_navn_fra_data` (text), `organisatorisk_navn_teknisk` (int), `Id` (int).

**`qic_result$qic_data` kolonner (verificeret mod BFHcharts):** `x` (**POSIXct**), `y`, `cl` (centerlinje pr. række = median-trin pr. fase), `n`, `part` (fase-nr), `target`, `anhoej.signal`, `runs.signal`, `sigma.signal`, `longest.run`, `n.crossings`, m.fl. `summary_all` har danske navne: `fase, anhoej_signal, længste_løb, antal_kryds, centerlinje, ...`.

**ggiraph selected-input (verificeret fra `girafe.js`):** `shinyInputId = containerid + "_selected"`. Med `girafeOutput(ns("chart"))` læses valget inde i modulet som **`input$chart_selected`** (Shiny-modulets input-proxy fjerner ns-præfikset). Tom/`NULL` når intet er valgt; ellers `data_id`-strengen.

**POSIXct→Date round-trip (advisor):** `x` er POSIXct → brug `format(x, "%Y-%m-%d")` som `data_id`, og `as.Date(selected_str)` tilbage. ALDRIG `as.Date(posixct)` direkte (TZ kan flytte en dag → forkert knæk-dato i DB).

**Cache-nøgle (advisor):** scan-resultater caches pr. **`(diagram_id, window)`** (ikke pr. filter-sæt). Filterændring → re-subset af allerede-scannede; post-skriv → invalidér netop dét diagram.

---

## Filstruktur

| Fil | Ansvar |
|-----|--------|
| `R/fct_scan.R` (ny) | Headless glue: `enhed_variants_for`, `scan_diagram`, `index_filter_choices`, `apply_index_filters`. Ren/injicerbar — ingen Shiny. |
| `R/fct_chart_interactive.R` (ny) | `interactive_run_chart(qic_result, selected_date)` → ggiraph girafe. Ingen Shiny-state. |
| `R/mod_signal_review.R` (ny) | UI + server. Orkestrerer sti/filtre/scan/bladr/klik/forhåndsvis/gem. |
| `R/fct_sql.R` (udvid) | `build_org_enhed_variants_sql()`. |
| `R/fct_db.R` (udvid) | `db$org_enhed_variants()` accessor. |
| `R/app_ui.R` / `R/app_server.R` (udvid) | `nav_panel("Signal-gennemgang")` + server-kald + landing-flise. |
| `DESCRIPTION` (udvid) | `Imports: ggiraph, BFHtheme`. |
| `tests/testthat/test-scan.R` (ny) | enhed_variants_for, scan_diagram (fixture-parquet), filter-helpers. |
| `tests/testthat/test-chart-interactive.R` (ny) | girafe-struktur. |
| `tests/testthat/test-mod-signal-review.R` (ny) | testServer: scan/list/nav + faseskift. |
| `tests/testthat/test-sql.R` / `test-db-signal.R` (udvid) | SQL-streng + gated accessor. |

---

## Task 1: enhed-variant-resolution (SQL + ren helper + accessor)

**Files:**
- Modify: `R/fct_sql.R` (tilføj `build_org_enhed_variants_sql`)
- Create: `R/fct_scan.R` (tilføj `enhed_variants_for`)
- Modify: `R/fct_db.R:121-137` (tilføj accessor i `make_db`)
- Test: `tests/testthat/test-sql.R`, `tests/testthat/test-scan.R`, `tests/testthat/test-db-signal.R`

- [ ] **Step 1: Skriv fejlende SQL-test** — tilføj i `tests/testthat/test-sql.R`:

```r
test_that("build_org_enhed_variants_sql joiner org + oversaettelse på int-FK", {
  sql <- build_org_enhed_variants_sql()
  expect_match(sql, '"tblOrganisationStruktur"')
  expect_match(sql, '"tblOrganisationOversaettelse"')
  # FK er integer org_id = o."Id" (ikke et strengnavn)
  expect_match(sql, 'ov\\."organisatorisk_navn_teknisk" = o\\."Id"')
  expect_match(sql, 'organisatorisk_navn_fra_data')
  expect_match(sql, 'organisatorisk_navn_kort')
  expect_match(sql, 'LEFT JOIN')   # org uden oversaettelse bevares
})
```

- [ ] **Step 2: Kør → FAIL** `Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-sql.R")'` → "could not find function build_org_enhed_variants_sql".

- [ ] **Step 3: Implementér** — tilføj sidst i `R/fct_sql.R`:

```r
#' Én række pr. (org, enhed-fra-data-variant). LEFT JOIN bevarer organisationer
#' uden oversættelse. Bruges til at bygge parquet-enhed-filter pr. diagram.
#' @noRd
build_org_enhed_variants_sql <- function() {
  paste0(
    'SELECT o."Id" AS org_id, ',
    'o."organisatorisk_navn_teknisk" AS teknisk, ',
    'o."organisatorisk_navn_kort" AS kort, ',
    'o."organisatorisk_navn_langt" AS langt, ',
    'ov."organisatorisk_navn_fra_data" AS fra_data ',
    'FROM "tblOrganisationStruktur" o ',
    'LEFT JOIN "tblOrganisationOversaettelse" ov ',
    'ON ov."organisatorisk_navn_teknisk" = o."Id"')
}
```

- [ ] **Step 4: Kør → PASS.**

- [ ] **Step 5: Skriv fejlende test for ren helper** — opret `tests/testthat/test-scan.R`:

```r
test_that("enhed_variants_for: dedup + lowercase + drop tomme/NA", {
  vdf <- data.frame(
    org_id  = c(12L, 12L, 12L, 99L),
    teknisk = c("Y Hjerte", "Y Hjerte", "Y Hjerte", "Z"),
    kort    = c("YHJ", "YHJ", "YHJ", NA),
    langt   = c("Y Hjerteafdeling", "Y Hjerteafdeling", "Y Hjerteafdeling", ""),
    fra_data = c("HJERTE", "Y HJ", NA, NA),
    stringsAsFactors = FALSE)
  v <- enhed_variants_for(vdf, 12L)
  expect_true(all(v == tolower(v)))                    # alt lowercase
  expect_true(all(c("hjerte", "y hj", "y hjerte", "yhj",
                    "y hjerteafdeling") %in% v))
  expect_false(any(is.na(v) | v == ""))                # ingen tomme/NA
  expect_equal(length(v), length(unique(v)))           # dedup
})

test_that("enhed_variants_for: ukendt org_id → character(0)", {
  vdf <- data.frame(org_id = 1L, teknisk = "a", kort = "b", langt = "c",
                    fra_data = NA, stringsAsFactors = FALSE)
  expect_equal(enhed_variants_for(vdf, 777L), character(0))
})
```

- [ ] **Step 6: Kør → FAIL** (`enhed_variants_for` findes ej).

- [ ] **Step 7: Implementér** — opret `R/fct_scan.R` med header + funktion:

```r
# Headless scan-lag for signal-gennemgang. Sidder oven på Fase A-motoren
# (parquet/​signal) + DB-accessors. Ingen Shiny-state → ren + testbar.

#' Byg parquet-enhed-filter (lowercase varianter) for ét org_id ud fra
#' org_enhed_variants()-df (org-navne + tblOrganisationOversaettelse-fra-data).
#' @noRd
enhed_variants_for <- function(variants_df, org_id) {
  if (is.null(variants_df) || nrow(variants_df) == 0) return(character(0))
  rows <- variants_df[variants_df$org_id == org_id, , drop = FALSE]
  if (nrow(rows) == 0) return(character(0))
  v <- c(rows$fra_data, rows$teknisk[1], rows$kort[1], rows$langt[1])
  v <- tolower(v[!is.na(v) & nzchar(v)])
  unique(v)
}
```

- [ ] **Step 8: Kør → PASS.**

- [ ] **Step 9: Tilføj accessor** — i `R/fct_db.R`, inde i `make_db()`-listen efter `delete_median_break` (ca. linje 136), tilføj komma + :

```r
    org_enhed_variants = function() {
      DBI::dbGetQuery(pool, build_org_enhed_variants_sql())
    }
```

- [ ] **Step 10: Gated accessor-test** — i `tests/testthat/test-db-signal.R` tilføj:

```r
test_that("org_enhed_variants returnerer org-navne + fra-data-varianter", {
  skip_if_no_db()
  pool <- db_connect(); on.exit(pool::poolClose(pool))
  db <- make_db(pool)
  vdf <- db$org_enhed_variants()
  expect_true(all(c("org_id", "teknisk", "kort", "langt", "fra_data") %in% names(vdf)))
  expect_gt(nrow(vdf), 100)
  # Mindst én org har en fra-data-oversættelse
  expect_gt(sum(!is.na(vdf$fra_data)), 0)
})
```

- [ ] **Step 11: Kør hele suiten → grøn** (gated DB skippes uden `BFHMETA_WRITE=1`).
`Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'`

- [ ] **Step 12: Commit**

```bash
git add R/fct_sql.R R/fct_scan.R R/fct_db.R tests/testthat/test-sql.R tests/testthat/test-scan.R tests/testthat/test-db-signal.R
git commit -m "feat(signal): enhed-variant-resolution (org + oversaettelse → parquet-filter)"
```

---

## Task 2: scan_diagram-glue + fixture-test + real-data smoke

**Files:**
- Modify: `R/fct_scan.R` (tilføj `scan_diagram`)
- Test: `tests/testthat/test-scan.R`

`scan_diagram` binder lagene: byg enhed-filter → load slice → (evt. vindue-begræns) → resolve median-knæk → compute_signal. Alt i `safe_operation` så ét dårligt diagram ikke vælter en scan.

- [ ] **Step 1: Skriv fejlende fixture-test** — tilføj i `tests/testthat/test-scan.R`. Bygger en lille parquet-mappe i tempdir:

```r
test_that("scan_diagram: fixture-parquet med langt løb → signal=TRUE", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "test_ind"
  dir.create(file.path(base, ind))
  df <- data.frame(
    dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)),
    taeller = NA_real_, naevner = NA_real_,
    enhed = "afd x", stringsAsFactors = FALSE)
  arrow::write_parquet(df, file.path(base, ind, "part-0.parquet"))

  row <- list(diagram_id = 1L, indikator_navn_teknisk = ind, org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "Afd X", kort = NA,
                    langt = NA, fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf)

  expect_equal(res$status, "ok")
  expect_true(res$signal)
  expect_equal(res$n_obs, 24L)
  expect_s3_class(res$qic_result, "bfh_qic_result")
})

test_that("scan_diagram: manglende mappe → status 'ingen_data', intet hårdt fald", {
  base <- withr::local_tempdir()
  row <- list(diagram_id = 2L, indikator_navn_teknisk = "findes_ikke", org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "x", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf)
  expect_equal(res$status, "ingen_data")
  expect_false(res$signal)
})

test_that("scan_diagram: window_n begrænser til seneste N observationer", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "win_ind"; dir.create(file.path(base, ind))
  df <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                   vaerdi = c(rep(10, 12), rep(2, 12)),
                   taeller = NA_real_, naevner = NA_real_, enhed = "e")
  arrow::write_parquet(df, file.path(base, ind, "p.parquet"))
  row <- list(diagram_id = 3L, indikator_navn_teknisk = ind, org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "E", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf, window_n = 6L)
  expect_equal(res$n_obs, 6L)
})
```

- [ ] **Step 2: Kør → FAIL** (`scan_diagram` findes ej).

- [ ] **Step 3: Implementér** — tilføj i `R/fct_scan.R`:

```r
#' Scan ét diagram: byg enhed-filter → load parquet-slice → (vindue) → resolve
#' median-knæk → compute_signal. Fanger fejl pr. diagram (safe_operation).
#' @param row liste/df-række med indikator_navn_teknisk, org_id, diagram_id
#' @param base_path bruger-valgt parquet-rodmappe
#' @param medians_df alle median-rækker for diagrammet (kolonner diagram, laas_median) el. NULL
#' @param variants_df org_enhed_variants()-output
#' @param window_n behold seneste N observationer (NULL = alle)
#' @return list(diagram_id, status, signal, n_obs, slice, qic_result, summary)
#' @noRd
scan_diagram <- function(row, base_path, medians_df, variants_df, window_n = NULL) {
  empty <- function(status) list(diagram_id = row$diagram_id, status = status,
    signal = FALSE, n_obs = 0L, slice = NULL, qic_result = NULL, summary = NULL)
  out <- safe_operation(sprintf("scan diagram %s", row$diagram_id), {
    path <- parquet_indicator_path(base_path, row$indikator_navn_teknisk)
    variants <- enhed_variants_for(variants_df, row$org_id)
    enhed <- if (length(variants)) variants else NULL
    slice <- parquet_load_slice(path, enhed = enhed)
    if (is.null(slice) || nrow(slice) == 0) return(empty("ingen_data"))
    if (!is.null(window_n)) slice <- parquet_limit_observations(slice, window_n)
    slice <- slice[order(slice$dato), , drop = FALSE]
    parts <- resolve_median_breaks(row$diagram_id, medians_df, slice$dato)
    sig <- compute_signal(slice, parts = parts)
    list(diagram_id = row$diagram_id, status = "ok", signal = isTRUE(sig$signal),
         n_obs = length(unique(slice$dato)), slice = slice,
         qic_result = sig$qic_result, summary = sig$summary_all)
  }, fallback = empty("fejl"))
  out
}
```

- [ ] **Step 4: Kør → PASS.**

- [ ] **Step 5: Real-data smoke (MANUELT, gated på bruger-sti).** Kør mod en RIGTIG parquet-mappe for at fange skemaforskelle fixturet ikke kan reproducere (`dato` som partition-nøgle vs kolonne; blandet NA-`naevner`; dublerede enhed-varianter → dobbelt-tælling). **[MANUELT TRIN]** — kræver `PARQUET_DIR` fra bruger:

```bash
PARQUET_DIR=/sti/til/parquet BFHMETA_WRITE=0 Rscript -e '
  pkgload::load_all(".")
  pool <- db_connect(); on.exit(pool::poolClose(pool))
  db <- make_db(pool)
  idx <- db$list_active_seriediagrammer()
  vdf <- db$org_enhed_variants()
  base <- Sys.getenv("PARQUET_DIR")
  # Tag de første 5 diagrammer hvis parquet-mappe findes
  hits <- 0
  for (i in seq_len(nrow(idx))) {
    row <- as.list(idx[i, ])
    meds <- db$diagram_medians(row$diagram_id)
    r <- scan_diagram(row, base, meds, vdf)
    if (r$status == "ok") {
      hits <- hits + 1
      cat(sprintf("OK  diagram %s (%s): n=%d signal=%s kolonner=%s\n",
        r$diagram_id, row$indikator_navn_teknisk, r$n_obs, r$signal,
        paste(names(r$slice), collapse=",")))
      if (hits >= 5) break
    }
  }
  cat("Diagrammer med data:", hits, "\n")
'
```
Forventet: ≥1 `OK`-linje med `dato` blandt kolonnerne og fornuftigt `n`. Hvis `dato` mangler eller `n` er absurd → STOP, undersøg `parquet_load_slice` mod rigtig partitionering før Task 3+. Notér resultatet i commit-message.

- [ ] **Step 6: Commit**

```bash
git add R/fct_scan.R tests/testthat/test-scan.R
git commit -m "feat(signal): scan_diagram-glue (parquet→signal pr. diagram) + fixture-tests"
```

---

## Task 3: filter-helpers (rene funktioner)

**Files:**
- Modify: `R/fct_scan.R`
- Test: `tests/testthat/test-scan.R`

De 5 filtre (Overafdeling, Afsnit, Datapakke, Datasæt, Indikator) er rene subset-operationer på diagram-indekset.

- [ ] **Step 1: Skriv fejlende test** — tilføj i `tests/testthat/test-scan.R`:

```r
test_that("index_filter_choices: sorterede unikke valg pr. dimension (drop NA)", {
  idx <- data.frame(
    overafdeling = c("B", "A", "A", NA),
    afsnit = NA_character_,
    datapakke = c("P", "P", "Q", "P"),
    datasaet = c("d1", "d2", "d1", "d3"),
    indikator_navn = c("i2", "i1", "i1", "i3"),
    stringsAsFactors = FALSE)
  ch <- index_filter_choices(idx)
  expect_equal(ch$overafdeling, c("A", "B"))     # sorteret, NA væk
  expect_equal(ch$afsnit, character(0))           # helt NA → tom
  expect_equal(ch$indikator_navn, c("i1", "i2", "i3"))
})

test_that("apply_index_filters: AND på tværs af dimensioner; tom filter = alt", {
  idx <- data.frame(
    diagram_id = 1:4,
    overafdeling = c("A", "A", "B", "A"),
    afsnit = NA_character_,
    datapakke = c("P", "Q", "P", "P"),
    datasaet = c("d1", "d1", "d1", "d2"),
    indikator_navn = c("i1", "i1", "i1", "i1"),
    stringsAsFactors = FALSE)
  expect_equal(nrow(apply_index_filters(idx, list())), 4)
  r <- apply_index_filters(idx, list(overafdeling = "A", datapakke = "P"))
  expect_equal(r$diagram_id, c(1L, 4L))
  # tom streng/NULL pr. dimension ignoreres
  expect_equal(nrow(apply_index_filters(idx, list(overafdeling = ""))), 4)
})
```

- [ ] **Step 2: Kør → FAIL.**

- [ ] **Step 3: Implementér** — tilføj i `R/fct_scan.R`:

```r
# De 5 filter-dimensioner (kolonnenavne i diagram-indekset).
.SIGNAL_FILTER_DIMS <- c("overafdeling", "afsnit", "datapakke",
                         "datasaet", "indikator_navn")

#' Sorterede unikke valg pr. filter-dimension (NA/tomme droppes).
#' @noRd
index_filter_choices <- function(index) {
  stats::setNames(lapply(.SIGNAL_FILTER_DIMS, function(col) {
    v <- index[[col]]
    v <- v[!is.na(v) & nzchar(v)]
    sort(unique(v))
  }), .SIGNAL_FILTER_DIMS)
}

#' Subset diagram-indeks på et named filter (AND). Tomme/NULL-værdier ignoreres.
#' @noRd
apply_index_filters <- function(index, filters) {
  keep <- rep(TRUE, nrow(index))
  for (col in names(filters)) {
    val <- filters[[col]]
    if (is.null(val) || !nzchar(val) || !col %in% names(index)) next
    keep <- keep & !is.na(index[[col]]) & index[[col]] == val
  }
  index[keep, , drop = FALSE]
}
```

- [ ] **Step 4: Kør → PASS.**

- [ ] **Step 5: Commit**

```bash
git add R/fct_scan.R tests/testthat/test-scan.R
git commit -m "feat(signal): rene filter-helpers (choices + AND-subset af indeks)"
```

---

## Task 4: interaktiv run chart (ggiraph)

**Files:**
- Create: `R/fct_chart_interactive.R`
- Test: `tests/testthat/test-chart-interactive.R`

Bygger ggplot fra `qic_result$qic_data` (datapunkter + median-trin pr. fase + signal-fremhævning + interaktive punkter med tooltip/`data_id`) → `ggiraph::girafe` med single-selection. `data_id = format(x, "%Y-%m-%d")` (POSIXct→streng, TZ-sikkert).

- [ ] **Step 1: Skriv fejlende test** — opret `tests/testthat/test-chart-interactive.R`:

```r
test_that("interactive_run_chart returnerer girafe-htmlwidget med dato-data_id", {
  d <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                  vaerdi = c(rep(10, 12), rep(2, 12)), naevner = NA_real_)
  sig <- compute_signal(d, parts = 13L)
  g <- interactive_run_chart(sig$qic_result)
  expect_s3_class(g, "girafe")
  # data_id-strenge (ISO-datoer) skal optræde i den genererede SVG
  svg <- as.character(g$x$html)
  expect_match(svg, "2020-01-01")
})

test_that("interactive_run_chart: valgt dato fremhæves uden fejl", {
  d <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                  vaerdi = c(rep(10, 12), rep(2, 12)), naevner = NA_real_)
  sig <- compute_signal(d, parts = 13L)
  expect_s3_class(
    interactive_run_chart(sig$qic_result, selected_date = "2020-07-28"),
    "girafe")
})
```

- [ ] **Step 2: Kør → FAIL.**

- [ ] **Step 3: Implementér** — opret `R/fct_chart_interactive.R`:

```r
# Interaktiv run chart bygget fra bfh_qic()$qic_data via ggiraph. Punkter er
# klikbare (data_id = ISO-dato) → input$<id>_selected. Median-trin følger 'part'
# (fase), signal-punkter fremhæves. BFHtheme-styling hvor muligt.

#' @param qic_result bfh_qic_result (fra compute_signal()$qic_result)
#' @param selected_date valgt ISO-dato ("YYYY-MM-DD") der fremhæves, el. NULL
#' @param height_svg girafe-højde i tommer
#' @return ggiraph::girafe
#' @noRd
interactive_run_chart <- function(qic_result, selected_date = NULL, height_svg = 4) {
  qd <- qic_result$qic_data
  # POSIXct → Date-streng (TZ-sikkert) som stabilt punkt-id
  qd$.id <- format(qd$x, "%Y-%m-%d")
  qd$.tooltip <- sprintf("%s: %s", qd$.id, round(qd$y, 2))
  qd$.signal <- isTRUE_vec(qd$anhoej.signal)
  qd$.selected <- !is.null(selected_date) & qd$.id == (selected_date %||% "")

  p <- ggplot2::ggplot(qd, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_line(color = "grey40", linewidth = 0.4) +
    # Median-trin pr. fase (cl er konstant inden for hver 'part')
    ggplot2::geom_line(ggplot2::aes(y = .data$cl, group = .data$part),
                       color = "steelblue", linewidth = 0.6) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = .data$.tooltip, data_id = .data$.id,
                   color = .data$.signal),
      size = 2) +
    ggplot2::scale_color_manual(values = c(`FALSE` = "grey30", `TRUE` = "firebrick"),
                                guide = "none") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal()

  # Fremhæv valgt punkt (ring udenom)
  if (!is.null(selected_date) && any(qd$.selected)) {
    p <- p + ggplot2::geom_point(
      data = qd[qd$.selected, , drop = FALSE],
      shape = 21, size = 4, stroke = 1.1, color = "black", fill = NA)
  }

  ggiraph::girafe(ggobj = p, height_svg = height_svg,
    options = list(
      ggiraph::opts_selection(type = "single", only_shiny = TRUE),
      ggiraph::opts_hover(css = "cursor:pointer;")))
}

#' Robust TRUE-vektor (NA/NULL → FALSE) — qic-signalkolonner kan have NA.
#' @noRd
isTRUE_vec <- function(x) !is.na(x) & x

#' NULL-coalesce (lokal, undgår rlang-import)
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
```

- [ ] **Step 4: Kør → PASS.** Hvis `%||%` allerede er defineret i pakken (tjek `grep -rn "%||%" R/`), drop den lokale definition for at undgå dublet.

- [ ] **Step 5: Commit**

```bash
git add R/fct_chart_interactive.R tests/testthat/test-chart-interactive.R
git commit -m "feat(signal): interaktiv run chart via ggiraph (klikbare punkter, median-trin)"
```

---

## Task 5: mod_signal_review UI

**Files:**
- Create: `R/mod_signal_review.R` (kun UI i denne task)
- Modify: `DESCRIPTION` (Imports: ggiraph, BFHtheme)

- [ ] **Step 1: Tilføj Imports** — i `DESCRIPTION` under `Imports:` tilføj `ggiraph,` og `BFHtheme,` (alfabetisk efter eksisterende). Verificér med `grep -n "ggiraph\|BFHtheme" DESCRIPTION`.

- [ ] **Step 2: Implementér UI** — opret `R/mod_signal_review.R`:

```r
# Modul: Signal-gennemgang. Peg på parquet-mappe → scan filtrerede aktive
# Seriediagrammer → vis signal-diagrammer interaktivt → registrér faseskift.

#' @noRd
mod_signal_review_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(width = 320, open = TRUE,
      textInput(ns("parquet_dir"), "Parquet-mappe", placeholder = "/sti/til/parquet"),
      div(class = "d-flex gap-2 align-items-end",
        radioButtons(ns("window_mode"), "Datavindue",
          c("Alle data" = "all", "Seneste N" = "latest"), inline = TRUE),
        numericInput(ns("window_n"), "N", value = 24, min = 6, max = 200, width = "90px")),
      hr(),
      selectizeInput(ns("f_overafdeling"), "Overafdeling", NULL, multiple = FALSE),
      selectizeInput(ns("f_afsnit"), "Afsnit", NULL, multiple = FALSE),
      selectizeInput(ns("f_datapakke"), "Datapakke", NULL, multiple = FALSE),
      selectizeInput(ns("f_datasaet"), "Datasæt", NULL, multiple = FALSE),
      selectizeInput(ns("f_indikator_navn"), "Indikator", NULL, multiple = FALSE),
      actionButton(ns("scan"), "Scan", class = "btn-primary w-100"),
      uiOutput(ns("scan_summary"))),
    # Hovedområde: navigation + graf + faseskift
    div(class = "d-flex justify-content-between align-items-center mb-2",
      uiOutput(ns("nav_label")),
      div(class = "btn-group",
        actionButton(ns("prev"), "‹ Forrige", class = "btn-outline-secondary btn-sm"),
        actionButton(ns("next_"), "Næste ›", class = "btn-outline-secondary btn-sm"))),
    ggiraph::girafeOutput(ns("chart"), height = "420px"),
    hr(),
    div(class = "d-flex gap-2 align-items-center flex-wrap",
      uiOutput(ns("selected_label")),
      actionButton(ns("preview"), "Forhåndsvis faseskift", class = "btn-outline-primary btn-sm"),
      actionButton(ns("save_break"), "Gem faseskift", class = "btn-success btn-sm")),
    div(class = "mt-3",
      h6("Eksisterende median-knæk"),
      DT::DTOutput(ns("breaks_tbl")),
      actionButton(ns("delete_break"), "Fjern valgt knæk", class = "btn-outline-danger btn-sm mt-1"))
  )
}
```

- [ ] **Step 3: Verificér load** `Rscript -e 'pkgload::load_all("."); cat("ok\n")'` → "ok" (ingen parse-fejl).

- [ ] **Step 4: GATE — verificér ggiraph-input-wiring end-to-end [MANUELT TRIN].**
testServer-tests i Task 6-7 *injicerer* `input$chart_selected` direkte og beviser
derfor IKKE at det rigtige input-navn virker. Den ene integrationsdetalje der ikke
kan verificeres headless — at `girafeOutput(ns("chart"))` faktisk dukker op som
`input$chart_selected` inde i modulet — skal bekræftes HER, før Task 6-7 bygger
oven på navnet. Opret `dev/signal_chart_probe.R`:

```r
# Minimal probe: monter modul-UI'ets graf-output i dets ns + en triviel server
# der printer input$chart_selected. Klik et punkt → ISO-dato skal printes.
pkgload::load_all(".")
library(shiny)
ui <- fluidPage(ggiraph::girafeOutput("signal-chart"))
server <- function(input, output, session) {
  output[["signal-chart"]] <- ggiraph::renderGirafe({
    d <- data.frame(dato = as.Date("2020-01-01") + 0:9 * 30, vaerdi = 1:10,
                    naevner = NA_real_)
    interactive_run_chart(compute_signal(d)$qic_result)
  })
  # Modul-namespace 'signal' → fuldt input-id er 'signal-chart_selected'
  observe(message("chart_selected = ", input[["signal-chart_selected"]]))
}
shiny::runApp(shinyApp(ui, server), port = 3902, launch.browser = TRUE)
```
Kør `Rscript dev/signal_chart_probe.R`, klik et punkt → konsollen SKAL printe
`chart_selected = 2020-…`. **Hvis input-navnet afviger** (intet printes ved klik):
ret det rigtige navn nu og opdatér Task 6-7's `input$chart_selected`-referencer
før du fortsætter. Cheap insurance — byg ikke Task 6-7 på et uverificeret navn.

- [ ] **Step 5: Commit**

```bash
git add R/mod_signal_review.R DESCRIPTION dev/signal_chart_probe.R
git commit -m "feat(signal): mod_signal_review UI + ggiraph-input-wiring-probe (gate)"
```

---

## Task 6: mod_signal_review server — scan, liste, navigation

**Files:**
- Modify: `R/mod_signal_review.R` (tilføj server)
- Test: `tests/testthat/test-mod-signal-review.R`

Server-state: `index` (fra `db$list_active_seriediagrammer()` ved start), filtre populerer selectize, Scan kører over filtreret subset med `withProgress`, cacher pr. `(diagram_id, window)` i en `reactiveVal`-liste, bygger signal-liste (`signal == TRUE`), `cursor` peger ind i listen. Prev/Next flytter cursor.

- [ ] **Step 1: Skriv fejlende testServer-test** — opret `tests/testthat/test-mod-signal-review.R`. Bruger fixture-parquet + fake db:

```r
make_fake_signal_db <- function(base, idx) {
  list(
    list_active_seriediagrammer = function() idx,
    org_enhed_variants = function() data.frame(org_id = 5L, teknisk = "E",
      kort = NA, langt = NA, fra_data = NA, stringsAsFactors = FALSE),
    diagram_medians = function(diagram_id) data.frame(
      id = integer(0), diagram = integer(0), laas_median = as.Date(character(0))),
    add_median_break = function(diagram_id, dato) 999L,
    delete_median_break = function(median_id) 1L)
}

build_fixture <- function() {
  base <- withr::local_tempdir(.local_envir = parent.frame())
  for (ind in c("ind_sig", "ind_flat")) dir.create(file.path(base, ind))
  arrow::write_parquet(data.frame(
    dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)),
    taeller = NA_real_, naevner = NA_real_, enhed = "e"),
    file.path(base, "ind_sig", "p.parquet"))
  arrow::write_parquet(data.frame(
    dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = rep(c(4, 6), 12),
    taeller = NA_real_, naevner = NA_real_, enhed = "e"),
    file.path(base, "ind_flat", "p.parquet"))
  base
}

test_that("scan finder kun diagrammer med signal", {
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
    expect_equal(signal_list()$diagram_id, 1L)        # kun "Sig"
    expect_equal(current_diagram()$diagram_id, 1L)
  })
})

test_that("næste/forrige bladrer i signal-listen", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  for (ind in c("a", "b")) {
    dir.create(file.path(base, ind))
    arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
      vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
      naevner = NA_real_, enhed = "e"), file.path(base, ind, "p.parquet"))
  }
  idx <- data.frame(diagram_id = c(10L, 20L), indikator_id = c(1L, 2L),
    indikator_navn = c("A", "B"), indikator_navn_teknisk = c("a", "b"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    expect_equal(current_diagram()$diagram_id, 10L)
    session$setInputs(next_ = 1)
    expect_equal(current_diagram()$diagram_id, 20L)
    session$setInputs(next_ = 2)          # ud over slut → bliver på sidste
    expect_equal(current_diagram()$diagram_id, 20L)
    session$setInputs(prev = 1)
    expect_equal(current_diagram()$diagram_id, 10L)
  })
})
```

- [ ] **Step 2: Kør → FAIL** (`mod_signal_review_server` findes ej).

- [ ] **Step 3: Implementér server** — tilføj i `R/mod_signal_review.R`:

```r
#' @noRd
mod_signal_review_server <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    index <- reactiveVal(db$list_active_seriediagrammer())
    variants <- reactiveVal(db$org_enhed_variants())
    cache <- reactiveVal(list())          # nøgle "<diagram_id>|<window>" → scan-res
    signal_list <- reactiveVal(NULL)      # df: diagrammer med signal
    cursor <- reactiveVal(1L)
    preview_parts <- reactiveVal(NULL)    # ekstra forhåndsvist knæk (dato)
    selected_cursor <- reactiveVal(NULL)  # cursor-stempel da punkt sidst blev klikket

    # ggiraph-input kan ikke nulstilles fra server → stempl med cursor ved klik,
    # så et valg fra ET diagram aldrig læses som gyldigt på et ANDET (Task 7-guard).
    observeEvent(input$chart_selected, selected_cursor(cursor()))

    # Populér filter-valg ved start
    observeEvent(index(), {
      ch <- index_filter_choices(index())
      for (dim in names(ch)) {
        updateSelectizeInput(session, paste0("f_", dim),
          choices = c("(alle)" = "", ch[[dim]]), server = FALSE)
      }
    }, once = TRUE)

    current_filters <- reactive(list(
      overafdeling = input$f_overafdeling, afsnit = input$f_afsnit,
      datapakke = input$f_datapakke, datasaet = input$f_datasaet,
      indikator_navn = input$f_indikator_navn))

    window_n <- reactive(
      if (identical(input$window_mode, "latest")) as.integer(input$window_n) else NULL)

    .window_key <- reactive(if (is.null(window_n())) "all" else as.character(window_n()))

    observeEvent(input$scan, {
      base <- input$parquet_dir
      if (is.null(base) || !nzchar(base) || !dir.exists(base)) {
        showNotification("Angiv en eksisterende parquet-mappe", type = "warning")
        return()
      }
      cand <- apply_index_filters(index(), current_filters())
      if (nrow(cand) == 0) { showNotification("Ingen diagrammer matcher filtrene"); return() }
      wk <- .window_key(); vdf <- variants(); cc <- cache()
      results <- list()
      withProgress(message = "Scanner diagrammer", value = 0, {
        n <- nrow(cand)
        for (i in seq_len(n)) {
          row <- as.list(cand[i, ])
          key <- paste0(row$diagram_id, "|", wk)
          res <- cc[[key]]
          if (is.null(res)) {
            meds <- db$diagram_medians(row$diagram_id)
            res <- scan_diagram(row, base, meds, vdf, window_n = window_n())
            res$row <- row
            cc[[key]] <- res
          }
          results[[length(results) + 1]] <- res
          incProgress(1 / n, detail = sprintf("%d/%d", i, n))
        }
      })
      cache(cc)
      sig_ids <- vapply(results, function(r) isTRUE(r$signal), logical(1))
      sl <- cand[cand$diagram_id %in%
                   vapply(results[sig_ids], function(r) r$diagram_id, integer(1)), ,
                 drop = FALSE]
      signal_list(sl)
      cursor(1L)
      preview_parts(NULL)
      showNotification(sprintf("%d af %d diagrammer har signal", nrow(sl), nrow(cand)))
    })

    current_diagram <- reactive({
      sl <- signal_list()
      if (is.null(sl) || nrow(sl) == 0) return(NULL)
      as.list(sl[cursor(), ])
    })

    .scan_of_current <- reactive({
      cd <- current_diagram(); if (is.null(cd)) return(NULL)
      cache()[[paste0(cd$diagram_id, "|", .window_key())]]
    })

    observeEvent(input$next_, {
      sl <- signal_list(); if (is.null(sl) || nrow(sl) == 0) return()
      cursor(min(cursor() + 1L, nrow(sl))); preview_parts(NULL)
    })
    observeEvent(input$prev, {
      if (is.null(signal_list())) return()
      cursor(max(cursor() - 1L, 1L)); preview_parts(NULL)
    })

    output$nav_label <- renderUI({
      sl <- signal_list()
      if (is.null(sl) || nrow(sl) == 0) return(span("Ingen scan endnu", class = "text-muted"))
      cd <- current_diagram()
      strong(sprintf("%d/%d — %s · %s", cursor(), nrow(sl),
                     cd$indikator_navn, cd$org_navn))
    })

    output$scan_summary <- renderUI({
      sl <- signal_list(); if (is.null(sl)) return(NULL)
      div(class = "small text-muted mt-2", sprintf("%d med signal", nrow(sl)))
    })

    # Eksponér til test
    list(signal_list = signal_list, current_diagram = current_diagram,
         cursor = cursor, cache = cache, preview_parts = preview_parts,
         scan_of_current = .scan_of_current)
  })
}
```

- [ ] **Step 4: Kør → PASS.** Justér til tests grønne (især `current_diagram()` på tom liste).

- [ ] **Step 5: Commit**

```bash
git add R/mod_signal_review.R tests/testthat/test-mod-signal-review.R
git commit -m "feat(signal): review-server scan+cache+navigation (testServer-dækket)"
```

---

## Task 7: server — render graf + faseskift (tilføj/fjern/forhåndsvis)

**Files:**
- Modify: `R/mod_signal_review.R` (tilføj render + faseskift-observers)
- Test: `tests/testthat/test-mod-signal-review.R`

Graf renderes fra `scan_of_current()$qic_result`. `input$chart_selected` (verificeret ggiraph-input under ns) = valgt ISO-dato, men læses ALTID via `valid_selected_date()` der gater på cursor-stemplet fra Task 6 — så et valg fra ét diagram aldrig skrives på et andet efter navigation. Forhåndsvis re-kører `compute_signal` med ekstra knæk. Gem → `db$add_median_break(diagram_id, as.Date(valgt))` → invalidér diagram-cache + re-scan → typisk forsvinder signalet → Næste. Fjern → `delete_median_break`. Klik på første observation kan ikke splitte (`resolve_median_breaks` dropper rp==1) → brugerbesked, intet skrives.

- [ ] **Step 1: Skriv fejlende test** — tilføj i `tests/testthat/test-mod-signal-review.R`:

```r
test_that("Gem faseskift kalder add_median_break med valgt dato + invaliderer cache", {
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
  saved <- new.env(); saved$args <- NULL
  db <- make_fake_signal_db(base, idx)
  db$add_median_break <- function(diagram_id, dato) {
    saved$args <- list(diagram_id = diagram_id, dato = dato); 555L }

  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    # Simulér klik på en gyldig (ikke-første) observation
    session$setInputs(chart_selected = "2020-07-28")
    session$setInputs(save_break = 1)
    expect_equal(saved$args$diagram_id, 7L)
    expect_equal(as.Date(saved$args$dato), as.Date("2020-07-28"))
  })
})

test_that("klik på første observation → ingen skrivning (kan ikke splitte)", {
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
  called <- new.env(); called$n <- 0
  db <- make_fake_signal_db(base, idx)
  db$add_median_break <- function(diagram_id, dato) { called$n <- called$n + 1; 1L }
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    session$setInputs(chart_selected = "2020-01-01")   # første obs
    session$setInputs(save_break = 1)
    expect_equal(called$n, 0)
  })
})

test_that("valg fra ét diagram skrives ALDRIG på et andet efter navigation", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  for (ind in c("a", "b")) {
    dir.create(file.path(base, ind))
    arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
      vaerdi = c(rep(10, 12), rep(2, 12)), taeller = NA_real_,
      naevner = NA_real_, enhed = "e"), file.path(base, ind, "p.parquet"))
  }
  idx <- data.frame(diagram_id = c(10L, 20L), indikator_id = c(1L, 2L),
    indikator_navn = c("A", "B"), indikator_navn_teknisk = c("a", "b"),
    datasaet = "d", datapakke = "p", org_id = 5L, org_teknisk = "E",
    org_navn = "E", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  called <- new.env(); called$n <- 0
  db <- make_fake_signal_db(base, idx)
  db$add_median_break <- function(diagram_id, dato) { called$n <- called$n + 1; 1L }
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    session$setInputs(chart_selected = "2020-07-28")  # valgt på diagram 10
    session$setInputs(next_ = 1)                      # naviger til diagram 20
    session$setInputs(save_break = 1)                 # stale valg → ingen skrivning
    expect_equal(called$n, 0)
  })
})
```

- [ ] **Step 2: Kør → FAIL.**

- [ ] **Step 3: Implementér render + faseskift** — tilføj inde i `mod_signal_review_server` (før `list(...)`-returneringen), og tilføj de nye felter til den eksponerede liste:

```r
    # Cursor-stemplet valg: returnér KUN den valgte dato hvis klikket skete på
    # det diagram der vises NU. ggiraph-input kan ikke nulstilles fra server, så
    # uden dette stempel ville et valg fra forrige diagram læses som gyldigt på
    # det aktuelle → faseskift på FORKERT diagram (korrekthedsfejl, ikke nit).
    valid_selected_date <- reactive({
      sel <- input$chart_selected
      if (is.null(sel) || !nzchar(sel) || !identical(selected_cursor(), cursor()))
        return(NULL)
      sel
    })

    # --- Graf -------------------------------------------------------------
    output$chart <- ggiraph::renderGirafe({
      sc <- .scan_of_current(); if (is.null(sc) || is.null(sc$qic_result)) return(NULL)
      qr <- sc$qic_result
      # Forhåndsvis: re-beregn med ekstra knæk hvis valgt + gyldigt
      pv <- preview_parts()
      if (!is.null(pv) && !is.null(sc$slice)) {
        meds_extra <- data.frame(diagram = current_diagram()$diagram_id,
                                 laas_median = as.Date(pv))
        base_meds <- db$diagram_medians(current_diagram()$diagram_id)
        all_meds <- rbind(
          base_meds[, c("diagram", "laas_median")],
          meds_extra)
        parts <- resolve_median_breaks(current_diagram()$diagram_id,
                                       all_meds, sc$slice$dato)
        qr <- compute_signal(sc$slice, parts = parts)$qic_result
      }
      interactive_run_chart(qr, selected_date = valid_selected_date())
    })

    output$selected_label <- renderUI({
      sel <- valid_selected_date()   # stale valg fra andet diagram → "klik en obs"
      if (is.null(sel)) return(span("Klik en observation", class = "text-muted"))
      span(sprintf("Valgt: %s", sel), class = "fw-bold")
    })

    # --- Forhåndsvis ------------------------------------------------------
    observeEvent(input$preview, {
      sel <- valid_selected_date()
      sc <- .scan_of_current()
      if (is.null(sel) || is.null(sc) || is.null(sc$slice)) {
        showNotification("Vælg en observation på dette diagram", type = "warning"); return()
      }
      parts <- resolve_median_breaks(current_diagram()$diagram_id,
        data.frame(diagram = current_diagram()$diagram_id,
                   laas_median = as.Date(sel)), sc$slice$dato)
      if (is.null(parts)) {
        showNotification("Kan ikke lave faseskift her (første/ugyldig observation)",
                         type = "warning"); return()
      }
      preview_parts(sel)
    })

    # --- Gem faseskift ----------------------------------------------------
    observeEvent(input$save_break, {
      sel <- valid_selected_date()
      cd <- current_diagram(); sc <- .scan_of_current()
      if (is.null(sel) || is.null(cd) || is.null(sc$slice)) {
        showNotification("Vælg en observation på dette diagram", type = "warning"); return()
      }
      # Valider at det er et lovligt knæk (ikke første obs / uden for data)
      parts <- resolve_median_breaks(cd$diagram_id,
        data.frame(diagram = cd$diagram_id, laas_median = as.Date(sel)),
        sc$slice$dato)
      if (is.null(parts)) {
        showNotification("Kan ikke lave faseskift her (første/ugyldig observation)",
                         type = "warning"); return()
      }
      ok <- safe_operation("gem faseskift", {
        db$add_median_break(cd$diagram_id, as.Date(sel)); TRUE
      }, fallback = FALSE)
      if (!isTRUE(ok)) { showNotification("Fejl ved gem (se log)", type = "error"); return() }
      # Invalidér netop dette diagram (alle vinduer) + re-scan aktuelt vindue
      cc <- cache()
      cc[grepl(paste0("^", cd$diagram_id, "\\|"), names(cc))] <- NULL
      meds <- db$diagram_medians(cd$diagram_id)
      cc[[paste0(cd$diagram_id, "|", .window_key())]] <-
        c(scan_diagram(as.list(cd), input$parquet_dir, meds, variants(),
                       window_n = window_n()), list(row = as.list(cd)))
      cache(cc)
      preview_parts(NULL)
      showNotification("Faseskift gemt")
    })

    # --- Eksisterende median-knæk + fjern ---------------------------------
    output$breaks_tbl <- DT::renderDT({
      cd <- current_diagram(); if (is.null(cd)) return(DT::datatable(data.frame()))
      m <- db$diagram_medians(cd$diagram_id)
      DT::datatable(m[, intersect(c("id", "laas_median"), names(m)), drop = FALSE],
        rownames = FALSE, selection = "single",
        options = list(dom = "t", paging = FALSE))
    })

    observeEvent(input$delete_break, {
      cd <- current_diagram(); if (is.null(cd)) return()
      sel <- input$breaks_tbl_rows_selected
      if (is.null(sel)) { showNotification("Vælg et knæk først", type = "warning"); return() }
      m <- db$diagram_medians(cd$diagram_id)
      mid <- m$id[sel]
      ok <- safe_operation("fjern faseskift", {
        db$delete_median_break(mid); TRUE
      }, fallback = FALSE)
      if (!isTRUE(ok)) { showNotification("Fejl ved fjern (se log)", type = "error"); return() }
      cc <- cache(); cc[grepl(paste0("^", cd$diagram_id, "\\|"), names(cc))] <- NULL
      cache(cc); preview_parts(NULL)
      showNotification("Knæk fjernet")
    })
```

Bemærk: tilføj ikke nye felter til den eksponerede `list(...)` medmindre testen kræver det — `chart_selected`/`save_break`-stierne testes via `db`-spies.

- [ ] **Step 4: Kør → PASS.**

- [ ] **Step 5: Commit**

```bash
git add R/mod_signal_review.R tests/testthat/test-mod-signal-review.R
git commit -m "feat(signal): faseskift add/remove/preview + graf-render (klik→knæk, cursor-stemplet valg-guard)"
```

---

## Task 8: wiring (nav + landing) + NEWS + version-bump

**Files:**
- Modify: `R/app_ui.R`, `R/app_server.R`
- Modify: `DESCRIPTION` (Version 0.5.0), `NEWS.md`
- Test: manuel smoke

- [ ] **Step 1: nav_panel** — i `R/app_ui.R`, tilføj som `nav_panel` efter "Indikatorer" (før `nav_menu`):

```r
    bslib::nav_panel("Signal-gennemgang", value = "signal",
      mod_signal_review_ui("signal")),
```

- [ ] **Step 2: landing-flise** — i `R/app_ui.R` `.landing_ui()`, tilføj en sektion + flise efter "Indikatorer"-sektionen:

```r
    sect("Signal-gennemgang"),
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      tile("signal", "Signal-gennemgang",
        "Scan parquet for Anhøj-signaler og registrér faseskift.")),
```

- [ ] **Step 3: server-kald + landing-nav** — i `R/app_server.R`, efter `mod_indikator_crud_server("indik", db)`:

```r
  mod_signal_review_server("signal", db)
```
og tilføj ved landing-observers:
```r
  observeEvent(input$go_signal, bslib::nav_select("nav", "signal"))
```

- [ ] **Step 4: Verificér load + fuld suite**

```bash
Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'
```
Forventet: alle grønne (gated DB skippes uden `BFHMETA_WRITE=1`).

- [ ] **Step 5: Manuel app-smoke [MANUELT TRIN]** — kræver bruger-parquet + bekræftet write-target:

```bash
BFHMETA_WRITE=1 Rscript dev/run_dev.R
```
Tjek: Start → Signal-gennemgang → indtast parquet-sti → vælg filtre → Scan (progressbar) → bladr Næste/Forrige → klik et punkt (label opdateres) → Forhåndsvis (median-trin flytter) → Gem (notifikation, signal forsvinder typisk) → genstart bekræfter knæk i `tblDiagrammerMedian`. Test også Fjern-knæk.

- [ ] **Step 6: Version + NEWS** — `DESCRIPTION` `Version: 0.5.0`. Tilføj øverst i `NEWS.md`:

```markdown
# BFHmetadata 0.5.0

## Nye features
* Signal-gennemgang (Fase B — review-UI): peg app'en på en parquet-mappe, scan
  filtrerede aktive Seriediagrammer for Anhøj-signal, og gennemgå dem i en
  interaktiv ggiraph-graf. Klik en observation for at registrere et faseskift
  direkte i tblDiagrammerMedian (tilføj/forhåndsvis/fjern), og bladr hurtigt
  mellem diagrammer. Fem filtre: Overafdeling, Afsnit, Datapakke, Datasæt,
  Indikator. Datavindue kan veksle mellem alle data og seneste N observationer.

## Interne ændringer
* Nyt headless scan-lag (fct_scan.R) + interaktivt chart-lag
  (fct_chart_interactive.R). Nye Imports: ggiraph, BFHtheme.
```

- [ ] **Step 7: Commit**

```bash
git add R/app_ui.R R/app_server.R DESCRIPTION NEWS.md
git commit -m "feat(signal): wiring (nav + landing) + bump 0.5.0 (Fase B komplet)"
```

---

## Faldgruber (bak ind i implementeringen)

1. **POSIXct→Date:** `qic_data$x` er POSIXct. `data_id = format(x, "%Y-%m-%d")`, retur via `as.Date(sel)`. ALDRIG `as.Date(posixct)` direkte (TZ-dag-skift → forkert knæk i DB).
2. **ggiraph-input under ns:** `girafeOutput(ns("chart"))` → læs `input$chart_selected` i modulet (verificeret fra girafe.js: id = containerid + "_selected").
3. **Cache-nøgle = `(diagram_id, window)`** — ikke filter. Filterændring re-subsetter; post-skriv dropper kun det ene diagrams nøgler.
4. **Ugyldigt knæk:** klik på første obs → `resolve_median_breaks` returnerer `NULL` → brugerbesked "kan ikke lave faseskift her", ingen skrivning. Tjek FØR `add_median_break`.
4b. **Stale ggiraph-valg på tværs af navigation (KORREKTHEDSFEJL):** `input$chart_selected` kan ikke nulstilles fra server og bevarer forrige diagrams dato efter Næste/Forrige. Uden guard ville Gem skrive knækket på det FORKERTE diagram (gyldigheds-tjekket mod ny slice fanger det ikke, da datointervaller ofte overlapper). Løst med `selected_cursor`-stempel (Task 6) + `valid_selected_date()` (Task 7): valg er kun gyldigt hvis stemplet == aktuel cursor.
5. **Blandet/manglende `naevner`:** `compute_signal` vælger proportion-gren på `any(!is.na(naevner))`. Hvis ægte parquet har delvist udfyldt naevner kan `bfh_qic` fejle på NA-rækker → fanges af Task 2-smoke + `safe_operation` (diagram markeres "fejl", væltes ikke scan). Hvis udbredt: filtrér NA-naevner-rækker i `scan_diagram` før compute (notér som opfølgning, ikke i denne plan).
6. **`dato` som partition-nøgle:** bekræftes i Task 2-smoke (kolonne i `names(slice)`?). `parquet_load_slice` filtrerer `.data$dato`; hvis arrow eksponerer det som partition virker filteret stadig, men n/datointerval skal verificeres på rigtig data.
7. **`%||%`-dublet:** tjek `grep -rn "%||%" R/` før Task 4 — drop lokal definition hvis pakken allerede har den.
8. **Write-guard:** `add/delete_median_break` er bag `assert_write_enabled()` (Fase A). UI'et kører read-only uden `BFHMETA_WRITE=1` → Gem fejler pænt via `safe_operation` + notifikation.

## Verifikation
- `Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'` — alle grønne (gated DB + real-parquet skippes uden env/sti). **Status: 202 PASS / 10 SKIP.**

### Udskudt manuel smoke (KRÆVER bruger-parquet + browser) — checkliste

Begge gates blev udskudt (ingen parquet lokalt på implementeringstidspunktet).
Kør FØR feature stoles på i produktion. Rækkefølge = make-or-break først:

1. **enhed-variant-matching (DEN kritiske antagelse).** Et scan returnerer kun
   data hvis parquet-`enhed`-værdier matcher (case-insensitivt) én af org'ens
   `teknisk/kort/langt/fra_data`-varianter. Matcher de ikke → ALLE scans giver
   stille `status="ingen_data"` (tom review-flade, ingen fejl). Smoke: vælg et
   kendt diagram, bekræft `scan_diagram` → `status="ok", n_obs>0`; spot-tjek at
   distinkte parquet-`enhed`-værdier faktisk skærer variant-sættet.
2. **parquet-sti-dybde.** `parquet_indicator_path` søger direkte + præcis 1 niveau
   ned. Bekræft at den rigtige mappestruktur resolver (dybere nesting → stille tom).
3. **NULL `afsnit`-filter degraderer pænt** (afsnit er all-NULL i DB nu) → selectize
   viser kun `(alle)`, ingen fejl.
4. **selectize `server=FALSE` med rigtige valg-antal** (hundredvis af indikator/
   overafdeling-værdier client-side) → acceptabel load, ingen trunkering.
5. **`breaks_tbl` slet row→id-mapping** mod diagram med ≥2 knæk (stabil
   `ORDER BY laas_median`; ties → nondeterministisk valg).
6. **POSIXct `laas_median` round-trip på rigtig data** — bekræft at gemt knæk =
   klikket observations-dato (ingen ±1 dag). (DB returnerer UTC-tagget POSIXct →
   `as.Date` er shift-fri i dette miljø; bekræft én gang på rigtig data.)
7. **ggiraph-wiring** (`dev/signal_chart_probe.R` ELLER i app): klik et punkt →
   `input$chart_selected` fyrer med ISO-dato. (JS-bevis stærkt; bekræft her.)
8. **`dato` som kolonne (ej kun partition-nøgle)** i `names(slice)` ved real-load.
9. **Fuld klik-til-gem-løkke + genstart-persistens** mod Supabase (`BFHMETA_WRITE=1`).

### Kendte ikke-blokerende noter (fra final review)
- NA `y` i proportion-charts (mid-serie NA-`naevner`) → punkt droppes med ggplot-
  warning, ingen fejl; hul i linjen hvor naevner mangler. Kosmetisk.
- Ingen "scanner…"-disabled-state på Scan-knap → dobbeltklik harmløst (idempotent,
  cache-backed). Valgfri polish.

## Bevidst UDE af scope
P-diagrammer (type 10), punkt-eksklusion, mål-redigering/`resolve_target`, kommentarer, baggrunds-scan af alle 553, master-detail, Access-skrivning. (Jf. spec §"Ikke i scope".)
