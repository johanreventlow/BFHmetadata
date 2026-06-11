# Signal-gennemgang — Fase A (headless engine) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Byg den testbare, UI-løse motor til signal-gennemgang: indlæs parquet-slices, byg diagram-indeks fra Supabase, og beregn Anhøj-signal pr. diagram via BFHcharts — plus DB-accessors til at læse/skrive median-knæk.

**Architecture:** Vendored parquet-/median-logik (fra BFHddl, men Supabase-fodret), et rent SQL-bygget diagram-indeks, og en `compute_signal()` der kører `bfh_qic(part=knæk)` og aflæser seneste fase i `$summary`. Alt headless og testbart uden Shiny.

**Tech Stack:** R, arrow (parquet), BFHcharts 0.25.0 (`bfh_qic`/qicharts2), DBI/RPostgres/pool, testthat (edition 3), withr.

**Spec:** `docs/superpowers/specs/2026-06-11-signal-gennemgang-design.md` (Fase A-delen).

**Org-niveau-filter (afklaret):** Filtrér på org-NIVEAU via ancestry (selv + forældre
op ad træet). Niveauer: Overafdeling=5, Afdeling=6, Afsnit=7. Diagrammernes org-noder
ligger pt. på Hospital(2, 285 diag, ingen overafdeling) og Overafdeling(5, 268 diag,
= sig selv). `afsnit` er tom nu, men populerer automatisk når diagrammer/data opstår på
niveau 7 (fremtidssikret). Indekset (Task 4) resolver `overafdeling/afdeling/afsnit`
via rekursiv CTE — verificeret mod DB: 268 overafdeling, 0 afsnit, 285 NULL (Hospital).

---

## File Structure

| Fil | Ansvar | Ændring |
|-----|--------|---------|
| `R/fct_parquet.R` | Vendored parquet: folder-discovery + slice-load + obs-limit | Create |
| `R/fct_signal.R` | `resolve_median_breaks` (vendored) + `compute_signal` | Create |
| `R/fct_sql.R` | `build_diagram_index_sql` + median SQL-byggere | Modify (append) |
| `R/fct_db.R` | `make_db`: list_active_seriediagrammer, diagram_medians, add/delete_median_break | Modify (`make_db`) |
| `tests/testthat/test-parquet.R` | Fixture-parquet → load/discovery/limit | Create |
| `tests/testthat/test-signal.R` | Kendt serie → signal-flag + median-resolve | Create |
| `tests/testthat/test-sql.R` | diagram-index + median SQL-byggere | Modify (append) |
| `tests/testthat/test-db-signal.R` | Gated: index-query + median round-trip mod Supabase | Create |
| `DESCRIPTION` | Imports: arrow, BFHcharts | Modify |

---

## Task 1: Vendored parquet-lag (`fct_parquet.R`)

**Files:**
- Create: `R/fct_parquet.R`
- Test: `tests/testthat/test-parquet.R`

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-parquet.R`:

```r
# Bygger en fixture-parquet med folder-pr-indikator-struktur
make_parquet_fixture <- function(env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = env)
  ind <- file.path(base, "test_ind")
  dir.create(ind, recursive = TRUE)
  d <- data.frame(
    dato = as.Date("2020-01-01") + 0:5 * 30,
    vaerdi = c(1, 2, 3, 4, 5, 6),
    taeller = NA_real_, naevner = NA_real_,
    enhed = rep("Afd X", 6), stringsAsFactors = FALSE)
  arrow::write_parquet(d, file.path(ind, "part-0.parquet"))
  base
}

test_that("parquet_indicator_path finder direkte + 1-niveau", {
  base <- make_parquet_fixture()
  expect_equal(parquet_indicator_path(base, "test_ind"), file.path(base, "test_ind"))
  # 1-niveau ned
  sub <- file.path(base, "gruppe"); dir.create(file.path(sub, "ind2"), recursive = TRUE)
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01"), vaerdi = 1),
                       file.path(sub, "ind2", "p.parquet"))
  expect_equal(parquet_indicator_path(base, "ind2"), file.path(sub, "ind2"))
})

test_that("parquet_load_slice filtrerer på enhed + dato", {
  base <- make_parquet_fixture()
  p <- parquet_indicator_path(base, "test_ind")
  all <- parquet_load_slice(p)
  expect_equal(nrow(all), 6)
  # enhed-match (case-insensitive)
  expect_equal(nrow(parquet_load_slice(p, enhed = "afd x")), 6)
  expect_null(parquet_load_slice(p, enhed = "Ukendt"))
  # dato-filter
  expect_equal(nrow(parquet_load_slice(p, from = "2020-03-01")), 4)
})

test_that("parquet_limit_observations beholder seneste N unikke datoer", {
  d <- data.frame(dato = as.Date("2020-01-01") + 0:9 * 30, vaerdi = 1:10)
  expect_equal(nrow(parquet_limit_observations(d, 3)), 3)
  expect_equal(max(parquet_limit_observations(d, 3)$dato), max(d$dato))
  expect_equal(nrow(parquet_limit_observations(d, NULL)), 10)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-parquet.R')"`
Expected: FAIL — `could not find function "parquet_indicator_path"`

- [ ] **Step 3: Write minimal implementation**

Create `R/fct_parquet.R`:

```r
# Vendored fra BFHddl (data_loader.R), Supabase-projekt: ingen config/cli-afhængighed.
# Parquet er folder-pr-indikator; arrow håndterer dato-partitionering automatisk.

#' Find mappen for én indikators parquet (direkte, ellers 1 niveau ned).
#' 1-niveau-grænsen undgår scan af ~67k dato-partition-mapper.
#' @noRd
parquet_indicator_path <- function(base_path, indikator_navn_teknisk) {
  direct <- file.path(base_path, indikator_navn_teknisk)
  if (dir.exists(direct)) return(direct)
  for (sub in list.dirs(base_path, recursive = FALSE, full.names = TRUE)) {
    cand <- file.path(sub, indikator_navn_teknisk)
    if (dir.exists(cand)) return(cand)
  }
  direct  # fejler downstream med klar besked
}

#' Indlæs én indikators parquet-slice, filtreret på enhed + dato.
#' Returnerer NULL hvis enhed angivet men intet matcher (eller tom).
#' @noRd
parquet_load_slice <- function(path, enhed = NULL, from = NULL, to = NULL) {
  if (!dir.exists(path)) return(NULL)
  ds <- arrow::open_dataset(path)
  if (!is.null(from)) ds <- dplyr::filter(ds, .data$dato >= as.Date(from))
  if (!is.null(to))   ds <- dplyr::filter(ds, .data$dato <= as.Date(to))
  if (!is.null(enhed)) {
    vars <- unique(tolower(enhed))
    ds <- dplyr::filter(ds, tolower(.data$enhed) %in% vars)
  }
  res <- dplyr::collect(ds)
  if (!is.null(enhed) && nrow(res) == 0) return(NULL)
  res
}

#' Behold de seneste max_obs unikke datoer (en observation = unik dato).
#' @noRd
parquet_limit_observations <- function(data, max_obs = 36L, date_col = "dato") {
  if (is.null(max_obs) || is.na(max_obs)) return(data)
  max_obs <- as.integer(max_obs)
  if (!date_col %in% names(data)) {
    if (nrow(data) <= max_obs) return(data)
    return(dplyr::slice_tail(data, n = max_obs))
  }
  ud <- sort(unique(data[[date_col]]))
  if (length(ud) <= max_obs) return(data)
  cutoff <- min(utils::tail(ud, max_obs))
  dplyr::filter(data, .data[[date_col]] >= cutoff)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-parquet.R')"`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/fct_parquet.R tests/testthat/test-parquet.R
git commit -m "feat(signal): vendored parquet-lag (path/slice/limit)"
```

---

## Task 2: `resolve_median_breaks` (`fct_signal.R`)

**Files:**
- Create: `R/fct_signal.R`
- Test: `tests/testthat/test-signal.R`

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-signal.R`:

```r
test_that("resolve_median_breaks: datoer → række-index (drop første/sidste/uden-for)", {
  meds <- data.frame(diagram = c(7, 7, 9),
    laas_median = as.Date(c("2020-03-01", "2020-05-01", "2020-01-01")))
  x <- as.Date("2020-01-01") + 0:5 * 30  # 6 unikke datoer
  pos <- resolve_median_breaks(7, meds, x)
  # 2020-03-01 → første dato >= = index 3; 2020-05-01 → index 5
  expect_equal(pos, c(3L, 5L))
})

test_that("resolve_median_breaks returnerer NULL uden data/match", {
  expect_null(resolve_median_breaks(7, NULL, as.Date("2020-01-01")))
  expect_null(resolve_median_breaks(99, data.frame(diagram = 7,
    laas_median = as.Date("2020-03-01")), as.Date("2020-01-01") + 0:5 * 30))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-signal.R')"`
Expected: FAIL — `could not find function "resolve_median_breaks"`

- [ ] **Step 3: Write minimal implementation**

Create `R/fct_signal.R`:

```r
# Vendored fra BFHddl (target_parsing.R): median-knæk-datoer → bfh_qic part-positioner.

#' Konverter tblDiagrammerMedian-datoer for ét diagram til række-positioner
#' (bfh_qic part=...). Knæk på første/sidste række eller uden for data droppes.
#' @noRd
resolve_median_breaks <- function(diagram_id, all_medians, x_dates) {
  if (is.null(all_medians) || !is.data.frame(all_medians) ||
      nrow(all_medians) == 0 || !"diagram" %in% names(all_medians)) return(NULL)
  rows <- all_medians[all_medians$diagram == diagram_id, , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)
  date_col <- intersect(names(rows),
    c("laas_median", "median_dato", "dato", "knaek_dato", "break_date"))
  if (length(date_col) == 0) return(NULL)
  bd <- sort(as.Date(rows[[date_col[1]]]))
  bd <- bd[!is.na(bd)]
  if (length(bd) == 0) return(NULL)
  x <- sort(unique(as.Date(x_dates)))
  pos <- integer(0)
  for (b in bd) {
    p <- which(x >= b)
    if (length(p) > 0) {
      rp <- min(p)
      if (rp > 1 && rp <= length(x)) pos <- c(pos, rp)
    }
  }
  pos <- sort(unique(pos))
  if (length(pos) == 0) NULL else pos
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-signal.R')"`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/fct_signal.R tests/testthat/test-signal.R
git commit -m "feat(signal): vendored resolve_median_breaks"
```

---

## Task 3: `compute_signal` (`fct_signal.R`)

**Files:**
- Modify: `R/fct_signal.R` (append)
- Test: `tests/testthat/test-signal.R` (append)

> `bfh_qic()$summary` har Anhøj-stats PR. FASE (kolonne `fase`, `anhoej_signal`).
> Signal afgøres af seneste fase (max `fase`). y/n: hvis `naevner` har værdier →
> proportion (y=taeller, n=naevner, multiply=100); ellers run på `vaerdi`.

- [ ] **Step 1: Write the failing test**

Append til `tests/testthat/test-signal.R`:

```r
test_that("compute_signal flagger langt løb (seneste fase ustabil)", {
  # 12 høje + 12 lave → langt løb, signal i (eneste) fase
  d <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                  vaerdi = c(rep(10, 12), rep(2, 12)), naevner = NA_real_)
  r <- compute_signal(d)
  expect_true(r$signal)
  expect_equal(max(r$summary_all$fase), 1)
})

test_that("compute_signal: stabil serie giver intet signal", {
  set.seed(1)
  d <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                  vaerdi = rep(c(4, 6), 12), naevner = NA_real_)  # krydser median tit
  expect_false(compute_signal(d)$signal)
})

test_that("compute_signal: kun seneste fase afgør (tidligt løb ignoreres)", {
  # Fase 1 (1:12) langt løb; fase 2 (13:24) stabil krydsende → seneste = stabil
  d <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                  vaerdi = c(rep(10, 12), rep(c(4, 6), 6)), naevner = NA_real_)
  r <- compute_signal(d, parts = 13L)
  expect_equal(max(r$summary_all$fase), 2)
  expect_false(r$signal)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-signal.R')"`
Expected: FAIL — `could not find function "compute_signal"`

- [ ] **Step 3: Write minimal implementation**

Append til `R/fct_signal.R`:

```r
#' Beregn run chart + Anhøj-signal for ét diagram-slice.
#' Alle faser beregnes (historik); signal-flag = seneste fase (max fase).
#' @param slice data.frame med dato/vaerdi (+ evt. taeller/naevner)
#' @param parts integer-vektor af part-positioner (fra resolve_median_breaks) el. NULL
#' @return list(signal, latest, summary_all, qic_result)
#' @noRd
compute_signal <- function(slice, parts = NULL) {
  has_n <- "naevner" %in% names(slice) && any(!is.na(slice$naevner))
  res <- if (has_n)
    BFHcharts::bfh_qic(slice, x = dato, y = taeller, n = naevner,
                       chart_type = "run", part = parts, multiply = 100)
  else
    BFHcharts::bfh_qic(slice, x = dato, y = vaerdi, chart_type = "run", part = parts)
  s <- res$summary
  latest <- s[s$fase == max(s$fase), , drop = FALSE]
  list(
    signal = isTRUE(latest$anhoej_signal[1]),
    latest = latest,
    summary_all = s,
    qic_result = res
  )
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-signal.R')"`
Expected: PASS (alle 5)

- [ ] **Step 5: Commit**

```bash
git add R/fct_signal.R tests/testthat/test-signal.R
git commit -m "feat(signal): compute_signal (Anhøj pr. seneste fase via bfh_qic)"
```

---

## Task 4: Diagram-indeks SQL-bygger (`fct_sql.R`)

**Files:**
- Modify: `R/fct_sql.R` (append)
- Test: `tests/testthat/test-sql.R` (append)

- [ ] **Step 1: Write the failing test**

Append til `tests/testthat/test-sql.R`:

```r
test_that("build_diagram_index_sql joiner indikator/hierarki/datapakke/org + org-niveauer", {
  sql <- build_diagram_index_sql()
  expect_match(sql, 'FROM "tblDiagrammer"')
  expect_match(sql, '"diagram_type" = 1')
  expect_match(sql, '"diagram_aktivt"')
  expect_match(sql, '"tblIndikatorer"')
  expect_match(sql, '"tblIndikatorHierarki"')
  expect_match(sql, '"tblOrganisationStruktur"')
  expect_match(sql, "datapakke")       # forælder-hierarki
  expect_match(sql, "datasaet")
  expect_match(sql, "indikator_navn_teknisk")
  # Org-niveau-ancestry (rekursiv CTE)
  expect_match(sql, "WITH RECURSIVE")
  expect_match(sql, "overafdeling")
  expect_match(sql, "afdeling")
  expect_match(sql, "afsnit")
})

test_that("median SQL-byggere er parametriserede", {
  expect_match(build_median_list_sql(),
    'FROM "tblDiagrammerMedian" WHERE "diagram" = \\$1')
  expect_match(build_median_insert_sql(),
    'INSERT INTO "tblDiagrammerMedian" \\("diagram", "laas_median"\\) VALUES \\(\\$1, \\$2\\) RETURNING "id"')
  expect_match(build_median_delete_sql(),
    'DELETE FROM "tblDiagrammerMedian" WHERE "id" = \\$1')
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-sql.R')"`
Expected: FAIL — `could not find function "build_diagram_index_sql"`

- [ ] **Step 3: Write minimal implementation**

Append til `R/fct_sql.R`:

```r
# --- Signal-gennemgang: diagram-indeks + median-knæk ------------------------

#' Ét row pr. aktivt Seriediagram med resolvede labels til filtrering/visning.
#' datapakke = forælder-hierarki (h.parent_id → dp). Org-niveauer (overafdeling=5/
#' afdeling=6/afsnit=7) resolves via rekursiv ancestry (selv + forældre op ad
#' parent_Id-træet) → fremtidssikret når diagrammer opstår på dybere niveauer.
#' diagram_type=1 (Seriediagram) + diagram_aktivt.
#' @noRd
build_diagram_index_sql <- function() {
  paste0(
    'WITH RECURSIVE anc AS (',
    ' SELECT "Id" AS start_id, "parent_Id", "organisatorisk_niveau", "organisatorisk_navn_langt"',
    ' FROM "tblOrganisationStruktur"',
    ' UNION ALL',
    ' SELECT a.start_id, p."parent_Id", p."organisatorisk_niveau", p."organisatorisk_navn_langt"',
    ' FROM anc a JOIN "tblOrganisationStruktur" p ON p."Id" = a."parent_Id"',
    '), lvl AS (',
    ' SELECT start_id,',
    ' max("organisatorisk_navn_langt") FILTER (WHERE "organisatorisk_niveau" = 5) AS overafdeling,',
    ' max("organisatorisk_navn_langt") FILTER (WHERE "organisatorisk_niveau" = 6) AS afdeling,',
    ' max("organisatorisk_navn_langt") FILTER (WHERE "organisatorisk_niveau" = 7) AS afsnit',
    ' FROM anc GROUP BY start_id',
    ') ',
    'SELECT d."id" AS diagram_id, ',
    'i."id" AS indikator_id, i."indikator_navn", i."indikator_navn_teknisk", ',
    'h."hierarki_navn" AS datasaet, dp."hierarki_navn" AS datapakke, ',
    'o."Id" AS org_id, o."organisatorisk_navn_teknisk" AS org_teknisk, ',
    'o."organisatorisk_navn_langt" AS org_navn, o."organisatorisk_niveau" AS org_niveau, ',
    'lvl.overafdeling, lvl.afdeling, lvl.afsnit ',
    'FROM "tblDiagrammer" d ',
    'JOIN "tblIndikatorer" i ON i."id" = d."indikator" ',
    'LEFT JOIN "tblIndikatorHierarki" h ON h."Id" = i."indikator_hierarki" ',
    'LEFT JOIN "tblIndikatorHierarki" dp ON dp."Id" = h."parent_id" ',
    'LEFT JOIN "tblOrganisationStruktur" o ON o."Id" = d."organisatorisk_navn_teknisk" ',
    'LEFT JOIN lvl ON lvl.start_id = o."Id" ',
    'WHERE d."diagram_type" = 1 AND d."diagram_aktivt" ',
    'ORDER BY i."indikator_navn", o."organisatorisk_navn_langt"')
}

#' @noRd
build_median_list_sql <- function() {
  'SELECT * FROM "tblDiagrammerMedian" WHERE "diagram" = $1 ORDER BY "laas_median"'
}

#' @noRd
build_median_insert_sql <- function() {
  'INSERT INTO "tblDiagrammerMedian" ("diagram", "laas_median") VALUES ($1, $2) RETURNING "id"'
}

#' @noRd
build_median_delete_sql <- function() {
  'DELETE FROM "tblDiagrammerMedian" WHERE "id" = $1'
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-sql.R')"`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/fct_sql.R tests/testthat/test-sql.R
git commit -m "feat(signal): diagram-indeks + median SQL-byggere"
```

---

## Task 5: DB-accessors (`fct_db.R`)

**Files:**
- Modify: `R/fct_db.R` (`make_db()` list)
- Test: `tests/testthat/test-mod-crud.R` (append — bruger eksisterende fake-mønster ej nødvendigt; ren accessor-form verificeres i Task 6 gated)

> Accessors tilføjes til `make_db()`. Skrivninger guardes med `assert_write_enabled()`.
> Funktionel verifikation sker i Task 6 (gated integration mod Supabase), da disse
> kræver rigtig pool. Her udvides kun listen.

- [ ] **Step 1: Tilføj accessors**

I `R/fct_db.R`, tilføj til listen i `make_db(pool)` (efter `create_indikator_full`):

```r
    list_active_seriediagrammer = function() {
      DBI::dbGetQuery(pool, build_diagram_index_sql())
    },
    diagram_medians = function(diagram_id) {
      DBI::dbGetQuery(pool, build_median_list_sql(), params = list(diagram_id))
    },
    add_median_break = function(diagram_id, dato) {
      assert_write_enabled()
      DBI::dbGetQuery(pool, build_median_insert_sql(),
                      params = list(diagram_id, as.character(as.Date(dato))))[[1]][1]
    },
    delete_median_break = function(median_id) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_median_delete_sql(), params = list(median_id))
    }
```

> `as.character(as.Date(dato))` sender en ISO-datostreng → Postgres caster til
> `laas_median`-kolonnens type (timestamp/date) uden tidszone-overraskelser.

- [ ] **Step 2: Verificér load + eksisterende tests stadig grønne**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_dir('tests/testthat')"`
Expected: PASS (ingen regression; gated signal-test skipper indtil Task 6).

- [ ] **Step 3: Commit**

```bash
git add R/fct_db.R
git commit -m "feat(signal): db-accessors (diagram-indeks + median læs/skriv)"
```

---

## Task 6: Gated integration + DESCRIPTION Imports

**Files:**
- Create: `tests/testthat/test-db-signal.R`
- Modify: `DESCRIPTION`

- [ ] **Step 1: Tilføj Imports**

I `DESCRIPTION` under `Imports:`, tilføj `arrow` og `BFHcharts` (alfabetisk hvor det passer):

```
Imports:
    shiny,
    golem,
    bslib,
    DT,
    DBI,
    RPostgres,
    pool,
    config,
    yaml,
    arrow,
    BFHcharts,
    dplyr
```

> `dplyr` bruges af parquet-laget. `BFHcharts` trækker qicharts2. Verificér at alle
> tre er installeret: `Rscript -e 'for(p in c("arrow","BFHcharts","dplyr")) cat(p, requireNamespace(p, quietly=TRUE), "\n")'`.

- [ ] **Step 2: Write the gated integration test**

Create `tests/testthat/test-db-signal.R`:

```r
skip_if_no_db <- function() {
  testthat::skip_if_not(identical(Sys.getenv("BFHMETA_WRITE"), "1"),
                        "BFHMETA_WRITE!=1 — springer DB-integration over")
}

test_that("diagram-indeks returnerer aktive Seriediagrammer med labels", {
  skip_if_no_db()
  pool <- db_connect(); on.exit(pool::poolClose(pool))
  db <- make_db(pool)
  idx <- db$list_active_seriediagrammer()
  expect_gt(nrow(idx), 100)
  expect_true(all(c("diagram_id", "indikator_navn_teknisk", "datasaet",
                    "datapakke", "org_teknisk", "overafdeling", "afdeling",
                    "afsnit") %in% names(idx)))
  # Org-niveau-ancestry: ~268 diagrammer på Overafdeling-niveau har overafdeling
  expect_gt(sum(!is.na(idx$overafdeling)), 100)
})

test_that("median-knæk INSERT → læs → DELETE round-trip", {
  skip_if_no_db()
  pool <- db_connect()
  db <- make_db(pool)
  did <- db$list_active_seriediagrammer()$diagram_id[1]
  newid <- db$add_median_break(did, as.Date("2099-01-01"))  # sikker test-dato
  on.exit(try(db$delete_median_break(newid), silent = TRUE), add = TRUE)
  on.exit(pool::poolClose(pool), add = TRUE)
  meds <- db$diagram_medians(did)
  expect_true(newid %in% meds$id)
  expect_true(as.Date("2099-01-01") %in% as.Date(meds$laas_median))
  db$delete_median_break(newid)
  expect_false(newid %in% db$diagram_medians(did)$id)
})
```

- [ ] **Step 3: Run gated test (med env sat)**

Run: `BFHMETA_WRITE=1 Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-db-signal.R')"`
Expected: PASS (kører faktisk — bekræft det ej kun skipper). Verificér også at
INSERT virker (tblDiagrammerMedian.id er identity); hvis ej, RETTES via at gøre
kolonnen identity i en separat migration — men forventning er at den ER identity
(PK fra PK_MAP).

- [ ] **Step 4: Full suite (uden env → gated skipper)**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_dir('tests/testthat')"`
Expected: PASS, signal-gated tests SKIP.

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/test-db-signal.R DESCRIPTION
git commit -m "test(signal): gated integration (indeks + median round-trip) + Imports"
```

---

## Task 7: NEWS + version

**Files:**
- Modify: `NEWS.md`, `DESCRIPTION`

- [ ] **Step 1: Bump + NEWS**

`DESCRIPTION`: `Version: 0.4.0`. Prepend til `NEWS.md`:

```markdown
# BFHmetadata 0.4.0

## Nye features
* Signal-gennemgang (Fase A — motor): indlæser lokale parquet-slices, bygger
  diagram-indeks fra Supabase og beregner Anhøj-signal pr. aktivt Seriediagram
  via BFHcharts (signal vurderet på seneste fase efter median-knæk). DB-accessors
  til at læse og skrive median-knæk (tblDiagrammerMedian).

## Interne ændringer
* Vendored parquet-/median-logik fra BFHddl (Supabase-fodret, ingen Access-kobling).
```

- [ ] **Step 2: Full suite**

Run: `Rscript -e "pkgload::load_all('.'); testthat::test_dir('tests/testthat')"`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add NEWS.md DESCRIPTION
git commit -m "chore(release): bump 0.4.0 + NEWS (signal-gennemgang fase A)"
```

---

## Self-Review-noter (udført ved plan-skrivning)

- **Spec-dækning (Fase A):** parquet-lag (Task 1), median-resolve (Task 2),
  compute_signal m. seneste-fase + alle-faser (Task 3), diagram-indeks (Task 4),
  DB-accessors inkl. median skriv/læs (Task 5), gated DB-verifikation (Task 6).
  Target-resolve bevidst UDE (påvirker ej Anhøj — Fase B/chart). Afdeling/Afsnit-
  filter bevidst UDE (org-model-uklarhed — Fase B).
- **Placeholders:** ingen — al kode + kommandoer konkrete.
- **Type-konsistens:** `compute_signal` returnerer `list(signal, latest, summary_all,
  qic_result)` — `summary_all` brugt konsistent. SQL-byggere navngivet
  `build_diagram_index_sql`/`build_median_{list,insert,delete}_sql`, kaldt ens i
  accessors (Task 5) + tests (Task 4/6). `resolve_median_breaks(diagram_id,
  all_medians, x_dates)` ens i Task 2 + fremtidig brug.
- **Verificér ved impl:** tblDiagrammerMedian.id er identity (Task 6 INSERT beviser);
  arrow/BFHcharts/dplyr installeret (Task 6 Step 1).
```
