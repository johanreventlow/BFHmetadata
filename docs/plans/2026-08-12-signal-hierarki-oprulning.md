# Hierarki-oprulning i signal-gennemgang — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Diagrammer på overafdelings-/hospitalsniveau kan gennemgås i signal-gennemgangen via in-memory hierarki-oprulning, der spejler BFHddl's semantik.

**Architecture:** Fallback i `scan_diagram`: direkte enhed-match = 0 rækker → find bidragende enheder via vendored `find_aggregation_children` (org-træ + `indgaar_i_aggregering`-flag, gennemfald, gren-eksklusion) → filtrér det allerede indlæste fulde indikator-slice pr. enhed → summér taeller/naevner pr. dato (`na.rm = FALSE`) via vendored `aggregate_child_data`. Oprulning FØR periode-aggregering. Design: `docs/plans/2026-08-12-signal-hierarki-oprulning-design.md`.

**Tech Stack:** R, Shiny testServer, DBI/pool (Supabase), arrow-parquet in-memory slices.

**Branch:** `feat/signal-hierarki-oprulning` (oprettet).

**Forudsætninger for implementøren:**
- R IKKE i PATH: `'/c/Program Files/R/R-4.6.0/bin/Rscript.exe'` (Git Bash).
- BFHddl-kilden er læsbar på `c:\Users\jrev0004\OneDrive - Region Hovedstaden\4_R\BFHddl` — vendoring SKAL ske ved at læse den faktiske kilde (`R/db_organisations.R` ~linje 143–271, `R/data_loader.R` ~linje 651–714) og portere trofast. Kontrakt-testene i denne plan er acceptkriterier.
- Kendt flake: sjælden exit-139 segfault ved teardown — kør igen; kun gennemførte kørsler tæller.
- `dev/run_dev.R` har en urelateret lokal ændring — må ALDRIG stages.
- Danske kommentarer (rigtige diakritiske tegn), engelske identifikatorer, ingen Claude-attribution i commits.
- testServer-tests evalueres i modul-env (interne objekter tilgås ved navn).

**Datashapes (fælles kontrakt for alle tasks):**
- `org_struct`: df med `id` (int), `parent_id` (int) — hele org-træet.
- `agg_flags`: df med `org_id` (int), `indikator_id` (int), `indgaar` (logical) — én række pr. diagram-række i `tblDiagrammer`, UDEN aktiv-filter.
- Bidragydere: `find_aggregation_children(center_org_id, indikator_id, org_struct, agg_flags, max_depth = 5L)` → integer-vektor af org-id'er.

---

### Task 1: Vendored kerne — `find_aggregation_children` + `aggregate_child_data`

**Files:**
- Create: `R/fct_aggregate.R`
- Test: `tests/testthat/test-aggregate.R` (ny)

**Step 1: Skriv fejlende kontrakt-tests** (`tests/testthat/test-aggregate.R`):

```r
# Kontrakt-tests: pinner BFHddl's oprulnings-semantik (DATA_CONVENTIONS §4-6,
# db_organisations.R). Fejler disse efter en fremtidig ændring, er vendored
# kode drevet fra BFHddl — synkronisér med kilden.

.os <- function(...) {
  # org_struct-helper: byg df af (id, parent_id)-par
  m <- matrix(c(...), ncol = 2, byrow = TRUE)
  data.frame(id = as.integer(m[, 1]), parent_id = as.integer(m[, 2]))
}
.fl <- function(...) {
  # agg_flags-helper: byg df af (org_id, indikator_id, indgaar)-tripler
  m <- matrix(c(...), ncol = 3, byrow = TRUE)
  data.frame(org_id = as.integer(m[, 1]), indikator_id = as.integer(m[, 2]),
             indgaar = as.logical(m[, 3]))
}

test_that("find_aggregation_children: flagede boern bidrager", {
  os <- .os(2, 1,  3, 1)                       # 1 har boern 2 og 3
  fl <- .fl(2, 9, TRUE,  3, 9, TRUE)
  expect_setequal(find_aggregation_children(1L, 9L, os, fl), c(2L, 3L))
})

test_that("find_aggregation_children: FALSE-flag ekskluderer hele grenen", {
  os <- .os(2, 1,  4, 2)                       # 1 -> 2 -> 4
  fl <- .fl(2, 9, FALSE,  4, 9, TRUE)          # 2 er FALSE, 4 er TRUE
  expect_length(find_aggregation_children(1L, 9L, os, fl), 0L)
})

test_that("find_aggregation_children: gennemfald traverserer uregistreret mellemniveau", {
  os <- .os(2, 1,  4, 2)                       # 1 -> 2 -> 4; 2 har INGEN raekke
  fl <- .fl(4, 9, TRUE)
  expect_equal(find_aggregation_children(1L, 9L, os, fl), 4L)
})

test_that("find_aggregation_children: TRUE-barn terminerer grenen (ingen dobbelttaelling)", {
  os <- .os(2, 1,  4, 2)                       # 1 -> 2 -> 4; baade 2 og 4 TRUE
  fl <- .fl(2, 9, TRUE,  4, 9, TRUE)
  expect_equal(find_aggregation_children(1L, 9L, os, fl), 2L)   # KUN 2
})

test_that("find_aggregation_children: andre indikatorers flag ignoreres; ingen boern -> tom", {
  os <- .os(2, 1)
  fl <- .fl(2, 8, TRUE)                        # flag paa ANDEN indikator
  expect_length(find_aggregation_children(1L, 9L, os, fl), 0L)
  expect_length(find_aggregation_children(99L, 9L, os, fl), 0L)  # ingen boern
})

test_that("find_aggregation_children: max_depth begraenser gennemfald", {
  os <- .os(2, 1,  3, 2,  4, 3)                # 1 -> 2 -> 3 -> 4 (kun 4 flaget)
  fl <- .fl(4, 9, TRUE)
  expect_equal(find_aggregation_children(1L, 9L, os, fl), 4L)
  expect_length(find_aggregation_children(1L, 9L, os, fl, max_depth = 1L), 0L)
})

test_that("aggregate_child_data: summerer pr. dato med na.rm=FALSE", {
  d <- data.frame(
    dato = as.Date(c("2024-01-01", "2024-01-01", "2024-02-01", "2024-02-01")),
    taeller = c(2, 3, 4, NA), naevner = c(10, 10, 10, 10),
    enhed = c("a", "b", "a", "b"))
  out <- aggregate_child_data(d, center_enhed = "center")
  out <- out[order(out$dato), ]
  expect_equal(out$taeller, c(5, NA))          # NA smitter (na.rm = FALSE)
  expect_equal(out$naevner, c(20, 20))
  expect_true(all(out$enhed == "center"))
})

test_that("aggregate_child_data: ikke-overlappende datoer = det ene barns vaerdi", {
  d <- data.frame(dato = as.Date(c("2024-01-01", "2024-02-01")),
                  taeller = c(2, 7), enhed = c("a", "b"))
  out <- aggregate_child_data(d, center_enhed = "c")
  expect_equal(out$taeller[order(out$dato)], c(2, 7))
  expect_false("naevner" %in% names(out))      # ingen naevner-kolonne ind -> ingen ud
})
```

**Step 2:** Kør: `'/c/Program Files/R/R-4.6.0/bin/Rscript.exe' -e "devtools::load_all('.', quiet=TRUE); testthat::test_file('tests/testthat/test-aggregate.R', reporter='summary')"`
Expected: FAIL ("could not find function").

**Step 3: Vendor implementationen** i ny `R/fct_aggregate.R`. LÆS FØRST BFHddl-kilden (`../BFHddl/R/db_organisations.R` og `../BFHddl/R/data_loader.R`) og portér semantikken trofast til BFHmetadatas datashapes (se kontrakten øverst). Fil-header SKAL indeholde:

```r
# Vendored fra BFHddl (R/db_organisations.R + R/data_loader.R, v0.6.0):
# hierarki-oprulning til signal-gennemgang. Semantik pinnet af
# tests/testthat/test-aggregate.R — aendres oprulningen i BFHddl, skal
# denne fil OG testene synkroniseres med kilden.
```

Funktioner (alle `@noRd`, danske kommentarer):
- `find_aggregation_children(center_org_id, indikator_id, org_struct, agg_flags, max_depth = 5L)`: strukturelle børn af center; pr. barn: flag-række for (barn, indikator) → TRUE = bidrager (grenen terminerer dér, ingen dobbelttælling); FALSE/tom-men-eksisterende = hele grenen ekskluderes; INGEN række = gennemfald (rekursion, dybde-begrænset). Returnér integer-vektor.
- `aggregate_child_data(child_data, center_enhed, date_col = "dato")`: gruppér pr. dato; `taeller = sum(taeller, na.rm = FALSE)`; `naevner` summeres kun hvis kolonnen findes; `enhed = center_enhed`; behold kun dato/enhed/taeller(/naevner)-kolonner.

**Step 4:** Kør testfilen — alle PASS. **Step 5: Commit:**

```bash
git add R/fct_aggregate.R tests/testthat/test-aggregate.R
git commit -m "feat(aggregering): vendored oprulnings-kerne fra BFHddl

find_aggregation_children (gennemfald, gren-eksklusion, dobbelttaellings-
vaern) + aggregate_child_data (sum pr. dato, na.rm=FALSE). Semantik
pinnet af kontrakt-tests — synkroniseres med BFHddl ved aendringer."
```

---

### Task 2: Adapter `aggregate_slice_for_center`

**Files:**
- Modify: `R/fct_aggregate.R` (append)
- Test: `tests/testthat/test-aggregate.R` (append)

**Step 1: Fejlende tests:**

```r
.variants <- function(...) {
  # org_enhed_variants-facon: org_id, teknisk, kort, langt, fra_data
  m <- list(...)
  do.call(rbind, lapply(m, function(x) data.frame(
    org_id = x[[1]], teknisk = x[[2]], kort = NA, langt = NA,
    fra_data = if (length(x) > 2) x[[3]] else NA, stringsAsFactors = FALSE)))
}

test_that("aggregate_slice_for_center: summerer boerns raekker fra slicet", {
  os <- .os(2, 1,  3, 1)
  fl <- .fl(2, 9, TRUE,  3, 9, TRUE)
  vdf <- .variants(list(1L, "center"), list(2L, "afsnit_a"), list(3L, "afsnit_b"))
  slice <- data.frame(
    dato = as.Date(rep(c("2024-01-01", "2024-02-01"), each = 2)),
    taeller = c(1, 2, 3, 4), naevner = c(10, 10, 10, 10),
    enhed = rep(c("afsnit_a", "afsnit_b"), 2))
  out <- aggregate_slice_for_center(slice, center_org_id = 1L,
    indikator_id = 9L, center_enhed = "center",
    org_struct = os, agg_flags = fl, variants_df = vdf)
  out <- out[order(out$dato), ]
  expect_equal(out$taeller, c(3, 7))
  expect_equal(out$naevner, c(20, 20))
  expect_true(all(out$enhed == "center"))
})

test_that("aggregate_slice_for_center: NULL naar ingen bidragydere eller ingen data", {
  os <- .os(2, 1); fl <- .fl(2, 9, FALSE)
  vdf <- .variants(list(1L, "center"), list(2L, "afsnit_a"))
  slice <- data.frame(dato = as.Date("2024-01-01"), taeller = 1,
                      enhed = "afsnit_a")
  # FALSE-flag -> ingen bidragydere
  expect_null(aggregate_slice_for_center(slice, 1L, 9L, "center", os, fl, vdf))
  # Bidragyder findes, men slicet mangler dens enhed
  fl2 <- .fl(2, 9, TRUE)
  slice2 <- data.frame(dato = as.Date("2024-01-01"), taeller = 1,
                       enhed = "andet_afsnit")
  expect_null(aggregate_slice_for_center(slice2, 1L, 9L, "center", os, fl2, vdf))
})

test_that("aggregate_slice_for_center: vaerdi-only kan IKKE oprulles -> NULL", {
  os <- .os(2, 1); fl <- .fl(2, 9, TRUE)
  vdf <- .variants(list(1L, "center"), list(2L, "afsnit_a"))
  slice <- data.frame(dato = as.Date("2024-01-01"), vaerdi = 42,
                      taeller = NA_real_, enhed = "afsnit_a")
  expect_null(aggregate_slice_for_center(slice, 1L, 9L, "center", os, fl, vdf))
})

test_that("aggregate_slice_for_center: NULL-input degraderer til NULL", {
  expect_null(aggregate_slice_for_center(NULL, 1L, 9L, "c",
    .os(2, 1), .fl(2, 9, TRUE), .variants(list(2L, "a"))))
  slice <- data.frame(dato = as.Date("2024-01-01"), taeller = 1, enhed = "a")
  expect_null(aggregate_slice_for_center(slice, 1L, 9L, "c",
    NULL, NULL, .variants(list(2L, "a"))))   # manglende org/flags -> ingen oprulning
})
```

**Step 2:** Kør — FAIL. **Step 3: Implementér** (append i `R/fct_aggregate.R`):

```r
#' Oprul et fuldt indikator-slice til ét center: find bidragende enheder,
#' filtrér slicet pr. enhed (samme variant-matching som direkte match) og
#' summér. Adapteren erstatter BFHddl's fil-loader — slicet er allerede i
#' hukommelsen, så oprulningen koster ingen ekstra parquet-læs.
#' Returnerer NULL når oprulning ikke er mulig (ingen bidragydere, ingen
#' data, vaerdi-only indikator, manglende org/flag-input) — kalderen
#' falder tilbage til "ingen_data".
#' @noRd
aggregate_slice_for_center <- function(full_slice, center_org_id, indikator_id,
                                       center_enhed, org_struct, agg_flags,
                                       variants_df) {
  if (is.null(full_slice) || nrow(full_slice) == 0) return(NULL)
  if (is.null(org_struct) || is.null(agg_flags)) return(NULL)
  # vaerdi-only indikatorer kan ikke oprulles (sum af medianer/andele er
  # statistisk forkert) — spejler DATA_CONVENTIONS §5.
  if (!"taeller" %in% names(full_slice) || all(is.na(full_slice$taeller))) {
    return(NULL)
  }
  kids <- find_aggregation_children(center_org_id, indikator_id,
                                    org_struct, agg_flags)
  if (length(kids) == 0) return(NULL)
  parts <- lapply(kids, function(k) {
    slice_filter_enhed(full_slice, enhed_variants_for(variants_df, k))
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  aggregate_child_data(do.call(rbind, parts), center_enhed = center_enhed)
}
```

(Justér til den faktiske vendored `aggregate_child_data`-signatur; behold rbind-af-child-rows-tilgangen — den matcher BFHddl's sum-over-alle-børn pr. dato.)

**Step 4:** Kør — PASS. **Step 5: Commit:** `feat(aggregering): slice-adapter til center-oprulning` (+ kort dansk body).

---

### Task 3: DB-accessors + SQL

**Files:**
- Modify: `R/fct_sql.R`, `R/fct_db.R`
- Test: `tests/testthat/test-sql.R` (append), `tests/testthat/test-db-signal.R` (append)

**Step 1: Fejlende tests.** `test-sql.R` (følg filens mønster for streng-assertions):

```r
test_that("build_org_struct_sql henter id + parent fra org-tabellen", {
  s <- build_org_struct_sql()
  expect_match(s, "tblOrganisationStruktur")
  expect_match(s, '"Id" AS id')
  expect_match(s, '"parent_Id" AS parent_id')
})

test_that("build_aggregation_flags_sql henter flag pr. diagram-raekke UDEN aktiv-filter", {
  s <- build_aggregation_flags_sql()
  expect_match(s, "tblDiagrammer")
  expect_match(s, '"organisatorisk_navn_teknisk" AS org_id')
  expect_match(s, '"indikator" AS indikator_id')
  expect_match(s, '"indgaar_i_aggregering" AS indgaar')
  expect_no_match(s, "diagram_aktivt")   # BFHddl laeser flag med active_only=FALSE
})
```

`test-db-signal.R` (append, samme `skip_if_no_db`-gate som filens øvrige tests):

```r
test_that("org_struct + aggregation_flags returnerer forventede kolonner", {
  skip_if_no_db()
  pool <- db_connect(); on.exit(pool::poolClose(pool))
  db <- make_db(pool)
  os <- db$org_struct()
  expect_true(all(c("id", "parent_id") %in% names(os)))
  expect_gt(nrow(os), 100)
  fl <- db$aggregation_flags()
  expect_true(all(c("org_id", "indikator_id", "indgaar") %in% names(fl)))
  expect_gt(nrow(fl), 100)
})
```

**Step 2:** Kør begge filer — nye tests FAIL. **Step 3: Implementér.**

`R/fct_sql.R` (i signal-gennemgang-sektionen):

```r
#' Hele org-traeet (id, parent) til hierarki-oprulning. Hentes én gang pr. scan.
#' @noRd
build_org_struct_sql <- function() {
  'SELECT "Id" AS id, "parent_Id" AS parent_id FROM "tblOrganisationStruktur"'
}

#' Aggregerings-flag pr. diagram-raekke. BEVIDST uden aktiv-filter: BFHddl
#' laeser flagene med active_only = FALSE — et inaktivt diagram kan stadig
#' bidrage opad.
#' @noRd
build_aggregation_flags_sql <- function() {
  paste0('SELECT "organisatorisk_navn_teknisk" AS org_id, ',
         '"indikator" AS indikator_id, ',
         '"indgaar_i_aggregering" AS indgaar FROM "tblDiagrammer"')
}
```

`R/fct_db.R` i `make_db` (efter `org_enhed_variants`):

```r
    org_struct = function() {
      DBI::dbGetQuery(pool, build_org_struct_sql())
    },
    aggregation_flags = function() {
      DBI::dbGetQuery(pool, build_aggregation_flags_sql())
    },
```

**Step 4:** Kør — PASS (db-integration skipper uden BFHMETA_WRITE). **Step 5: Commit:** `feat(db): org-trae + aggregerings-flag-accessors til oprulning`.

---

### Task 4: Scan-integration (fallback i `scan_diagram` + modul)

**Files:**
- Modify: `R/fct_scan.R` (scan_diagram), `R/mod_signal_review.R` (scan-start + .refresh_diagram), test-helper `make_fake_signal_db`
- Test: `tests/testthat/test-mod-signal-review.R` (append), evt. `test-scan.R`

**Step 1: Fejlende testServer-tests.** Udvid FØRST `make_fake_signal_db` med tomme defaults (bagudkompatibelt — eksisterende tests uændrede):

```r
    org_struct = function() data.frame(id = integer(0), parent_id = integer(0)),
    aggregation_flags = function() data.frame(
      org_id = integer(0), indikator_id = integer(0), indgaar = logical(0)),
```

Append tests (fixture-idé: parquet med enheder "afsnit_a"/"afsnit_b" med signal-serie når de summeres; center-org 1 uden egne rækker; org-træ 2→1, 3→1; flag TRUE for org 2+3 på indikatoren; `org_enhed_variants` udvidet med org 1/2/3):

```r
test_that("aggregat-diagram oprulles og scannes (signal fra summeret serie)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "ind_agg"))
  # To afsnit: summen 10+10 -> 2+2 giver tydeligt signal; hver for sig ens serie
  arrow::write_parquet(data.frame(
    dato = rep(as.Date("2020-01-01") + 0:23 * 30, 2),
    taeller = c(rep(10, 12), rep(2, 12), rep(10, 12), rep(2, 12)),
    naevner = 100,
    enhed = rep(c("afsnit_a", "afsnit_b"), each = 24)),
    file.path(base, "ind_agg", "p.parquet"))
  idx <- data.frame(diagram_id = 70L, indikator_id = 9L,
    indikator_navn = "Agg", indikator_navn_teknisk = "ind_agg",
    datasaet = "d", datapakke = "p", org_id = 1L, org_teknisk = "center",
    org_navn = "Center", org_niveau = 5L, overafdeling = "OA", afdeling = NA,
    afsnit = NA, stringsAsFactors = FALSE)
  db <- make_fake_signal_db(base, idx)
  db$org_enhed_variants <- function() data.frame(
    org_id = c(1L, 2L, 3L), teknisk = c("center", "afsnit_a", "afsnit_b"),
    kort = NA, langt = NA, fra_data = NA, stringsAsFactors = FALSE)
  db$org_struct <- function() data.frame(id = c(2L, 3L), parent_id = c(1L, 1L))
  db$aggregation_flags <- function() data.frame(
    org_id = c(2L, 3L), indikator_id = 9L, indgaar = TRUE)
  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 24,
      f_overafdeling = "", f_afsnit = "", f_datapakke = "", f_datasaet = "",
      f_indikator_navn = "", scan = 1)
    drain_scan()
    expect_equal(scanned_list()$diagram_id, 70L)          # scannet OK, ej ingen_data
    sc <- .scan_of_current()
    expect_equal(sc$status, "ok")
    expect_true(isTRUE(sc$aggregated))
    expect_equal(sc$n_agg_units, 2L)
    expect_true(isTRUE(sc$signal))                        # summen har signal
  })
})

test_that("center uden flagede boern forbliver ingen_data", {
  # Samme fixture, men aggregation_flags tom -> ingen oprulning
  ...(kopiér fixture, sæt db$aggregation_flags til 0-række-df)...
  # assert: scanned_list() tom (status ingen_data optages ikke), og
  # scan_progress' ingen_data-tæller er 1 (læs scan_progress())
})

test_that("direkte enhed-match vinder over oprulning", {
  # Fixture hvor parquet OGSÅ har enhed "center" med FLAD serie, mens
  # boernenes sum ville have signal. Efter scan: sc$aggregated er ikke TRUE,
  # og sc$signal er FALSE (den flade direkte serie blev brugt).
})
```

(Udfyld de to sidste tests komplet efter mønstret i den første — de er acceptkriterier, ikke pseudokode.)

**Step 2:** Kør — FAIL. **Step 3: Implementér.**

3a. `scan_diagram` får nye valgfrie parametre `org_struct = NULL, agg_flags = NULL` (+ roxygen). I grenen hvor `slice_filter_enhed` returnerede NULL/tom (nu "ingen_data"): prøv først oprulning:

```r
      slice <- slice_filter_enhed(full, variants)
      aggregated <- FALSE
      n_agg_units <- 0L
      if (is.null(slice) || nrow(slice) == 0) {
        kids <- if (!is.null(org_struct) && !is.null(agg_flags)) {
          find_aggregation_children(row$org_id, row$indikator_id,
                                    org_struct, agg_flags)
        } else {
          integer(0)
        }
        slice <- aggregate_slice_for_center(full, row$org_id,
          row$indikator_id, row$org_teknisk, org_struct, agg_flags,
          variants_df)
        if (!is.null(slice)) { aggregated <- TRUE; n_agg_units <- length(kids) }
      }
```

(Behold værdi-givende if/else-stil uden non-local returns — se filens eksisterende kommentarer. `aggregated`/`n_agg_units` føjes til resultat-listen for "ok"-grenen; `empty()`-hjælperen behøver ikke felterne.)

3b. `mod_signal_review.R` scan-start: hent én gang pr. scan med safe_operation (fallback NULL → oprulning slås fra, scan overlever):

```r
      agg_os <- safe_operation("hent org-trae", db$org_struct(), fallback = NULL)
      agg_fl <- safe_operation("hent aggregerings-flag",
                               db$aggregation_flags(), fallback = NULL)
```

Læg begge i `scan_ctx` OG i en modul-reactiveVal `agg_ctx` (list(os, fl)) så `.refresh_diagram` kan genbruge dem. `.scan_process_group` + `.refresh_diagram` sender dem videre til `scan_diagram(...)` — VIGTIGT: `.refresh_diagram` re-scanner ét diagram efter gem/fjern af knæk; uden parametrene dér ville et aggregat-diagram gå blank efter et gemt knæk.

**Step 4:** Kør HELE test-mod-signal-review.R + test-scan.R — alt grønt, eksisterende tests uændrede. **Step 5: Commit:** `feat(signal): hierarki-oprulning som scan-fallback` (+ dansk body om spejlet BFHddl-semantik).

---

### Task 5: UI-badge "Aggregeret fra N enheder"

**Files:**
- Modify: `R/mod_signal_review.R` (UI + server)
- Test: `tests/testthat/test-mod-signal-review.R` (append)

**Step 1: Fejlende test** (genbrug Task 4's aggregat-fixture):

```r
test_that("aggregat-badge vises for oprullede diagrammer og kun dér", {
  ...(aggregat-fixture fra Task 4)...
  # efter drain_scan():
  html <- paste(as.character(output$agg_badge), collapse = "")
  expect_match(html, "Aggregeret fra 2 enheder")
})
```

Plus assert i et eksisterende-stil fixture (direkte match) at `output$agg_badge` er tom/NULL.

**Step 2:** FAIL. **Step 3: Implementér.** UI: `uiOutput(ns("agg_badge"))` lige over `break_warning`. Server:

```r
    # Transparens: en oprullet serie SKAL kunne kendes fra en direkte målt.
    output$agg_badge <- renderUI({
      sc <- .scan_of_current()
      if (is.null(sc) || !isTRUE(sc$aggregated)) return(NULL)
      div(class = "alert alert-info py-1 px-2 small mb-2",
        sprintf("Aggregeret fra %d enheder (hierarki-oprulning som i BFHddl)",
                sc$n_agg_units %||% 0L))
    })
```

**Step 4:** PASS. **Step 5: Commit:** `feat(ux): badge for hierarki-oprullede diagrammer`.

---

### Task 6: Fuld verifikation

1. Fuld suite: `'/c/Program Files/R/R-4.6.0/bin/Rscript.exe' -e "devtools::test(reporter='summary')"` — 0 failures (kendte skips/[ERROR]-fixtures/1 warning accepteret).
2. Styler på ændrede filer (`R/fct_aggregate.R`, `R/fct_scan.R`, `R/fct_sql.R`, `R/fct_db.R`, `R/mod_signal_review.R`) → re-test hvis ændret.
3. Lintr på samme — nye fund fikses hvis trivielle.
4. Commit evt. style-fix: `style(aggregering): styler/lintr på ændrede filer`.
5. `git log --oneline main..HEAD` + `git status --short` rapporteres.
6. **[MANUELT TRIN, bruger]** Røgtest mod rigtige data: scan med filter på en overafdeling med kendte aggregat-diagrammer; verificér badge + at serien matcher PDF'ens.
