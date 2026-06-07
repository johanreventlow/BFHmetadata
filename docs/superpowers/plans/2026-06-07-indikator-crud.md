# tblIndikatorer CRUD-app Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Byg en lokal Golem CRUD-app for `tblIndikatorer` mod Supabase, så backend-write-back er valideret før consumers skiftes fra Access.

**Architecture:** Golem-pakke i BFHmetadata-roden. Migration flyttes til `migration/`. DB-lag via `pool`+`RPostgres` som `postgres`-rolle (bypasser RLS, bevidst — admin-tooling). Rene query-byggere + validering er unit-testede; CRUD-modulet testes via `testServer()` med injiceret fake-db (dependency injection). Skrivninger gated bag en write-guard.

**Tech Stack:** R, Shiny, golem, bslib, DT, DBI, RPostgres, pool, config, yaml, testthat.

**Reference:** Spec i `docs/superpowers/specs/2026-06-07-indikator-crud-design.md`.

---

## Filstruktur (mål)

```
BFHmetadata/                         # Golem-pakke "BFHmetadata"
├── DESCRIPTION, NAMESPACE
├── R/
│   ├── run_app.R, app_ui.R, app_server.R, app_config.R   # Golem-skelet
│   ├── metadata.R              # FK_MAP/PK_MAP + INDIKATOR_FIELDS (flyttet fra migration)
│   ├── fct_db.R                # db_config(), write_enabled(), make_db(pool)
│   ├── fct_sql.R               # rene SQL-byggere (testbare)
│   ├── utils_validation.R      # validate_indikator()
│   └── mod_indikator_crud.R    # CRUD-modul (ui+server)
├── dev/run_dev.R
├── inst/golem-config.yml, inst/app/www/
├── tests/testthat/             # test-sql, test-validation, test-mod-crud
├── config.yml, .Renviron(.example)   # delt (rod)
├── docs/superpowers/{specs,plans}/
└── migration/                  # flyttede migrations-filer
```

**tblIndikatorer kolonner (autoritativt fra access_schema.yaml):**

| kolonne | type | rolle i form |
|---|---|---|
| id | int | PK (readonly) |
| indikator_hierarki | int FK→tblIndikatorHierarki.Id (label `hierarki_navn`) | dropdown |
| indikator_navn | text | text-input |
| indikator_navn_teknisk | text | text-input |
| kontaktperson | int FK→tblPersoner.Id (label `fornavn ' ' efternavn`) | dropdown |
| sp_rapport_id | text | text-input |
| tillad_auto_opdatering | bool | checkbox |
| aktiv_indikator | bool | checkbox |
| nøgleindikator | bool | checkbox |
| definition_kort | memo | textarea |
| definition_dataportal | memo | textarea |
| tæller_beskrivelse | memo | textarea |
| nævner_beskrivelse | memo | textarea |
| indikator_ukompatibel_med | memo | textarea |
| mål | text | text-input |
| datakilde | int FK→tblDatakilder.Id (label `datakilde_navn`) | dropdown |
| direkte_link | text | text-input |
| ønsket_tendens | text | text-input |
| antal_observationer | int | numeric-input |
| periode_fra | timestamp | date-input |
| output_enhed | text | text-input |

---

## Task 1: Flyt migration til `migration/` + fix stier

**Files:**
- Move: `00_introspect_access.R`, `01_generate_ddl.R`, `02_migrate_data.R`, `migration_metadata.R`, `99_pak_zip.sh`, `PLAN.md`, `PLAN_TRIN1_DDL.md`, `access_schema.yaml`, `access_data_dump/`, `access_database_documenter.pdf`, `access_relationships.png`, `introspection_log.txt`, `01a_create_tables.sql`, `01b_foreign_keys.sql` → `migration/`
- Modify: `migration/01_generate_ddl.R`, `migration/02_migrate_data.R` (config-sti)
- Modify: `.gitignore` (sti-prefiks)

- [ ] **Step 1: Opret migration/ + flyt filer med git**

```bash
cd /Users/johanreventlow/R/BFHmetadata
mkdir -p migration
git mv 00_introspect_access.R 01_generate_ddl.R 02_migrate_data.R migration_metadata.R \
       99_pak_zip.sh PLAN.md PLAN_TRIN1_DDL.md migration/
# ej-trackede artefakter (gitignored) flyttes med almindelig mv
mv access_schema.yaml access_data_dump access_database_documenter.pdf \
   access_relationships.png introspection_log.txt migration/ 2>/dev/null
mv 01a_create_tables.sql 01b_foreign_keys.sql migration/ 2>/dev/null
```

- [ ] **Step 2: Ret config-sti i migration-scripts**

I `migration/01_generate_ddl.R` og `migration/02_migrate_data.R`, ændr læsning så de virker fra `migration/` ELLER repo-rod. Erstat `yaml::read_yaml("config.yml")` med en sti-robust opslag:

```r
# Find config.yml: lokal (rod) eller ../config.yml (fra migration/)
.cfg_path <- if (file.exists("config.yml")) "config.yml" else "../config.yml"
cfg <- yaml::read_yaml(.cfg_path)$default
```

I `migration/01_generate_ddl.R` ændr `source("migration_metadata.R", ...)` → uændret (filen ligger nu også i migration/). I `migration/02_migrate_data.R` samme. `access_schema.yaml`-stien (`cfg$paths$schema_yaml` = "access_schema.yaml") virker fra `migration/` da filen er flyttet dertil; intet at ændre der.

- [ ] **Step 3: Opdater .gitignore med migration/-prefiks**

Erstat de relevante linjer i `.gitignore`:

```
# Data-dumps (Parquet)
migration/access_data_dump/
*.parquet
# Store binære artefakter
migration/*.pdf
migration/*.png
# Genereret skema-output
migration/01_schema.sql
migration/01a_create_tables.sql
migration/01b_foreign_keys.sql
# Logs
migration/introspection_log.txt
*.log
```

- [ ] **Step 4: Verificér migration-scripts stadig kører fra migration/**

Run:
```bash
cd /Users/johanreventlow/R/BFHmetadata/migration && Rscript 01_generate_ddl.R 2>&1 | tail -2
```
Expected: `19 CREATE TABLE | 16 aktive FK, 1 kommenteret` + `Færdig`.

- [ ] **Step 5: Commit**

```bash
cd /Users/johanreventlow/R/BFHmetadata
git add -A
git commit -m "refactor(migration): flyt migrations-filer til migration/ undermappe

Frigør roden til Golem CRUD-pakke. config.yml + .Renviron bliver i roden
(delt). Scripts læser config sti-robust (rod eller ../). Verificeret: DDL-gen
kører fra migration/."
```

---

## Task 2: Golem-skelet i roden

**Files:**
- Create: `DESCRIPTION`, `NAMESPACE`, `R/run_app.R`, `R/app_ui.R`, `R/app_server.R`, `R/app_config.R`, `dev/run_dev.R`, `inst/golem-config.yml`, `inst/app/www/.gitkeep`

- [ ] **Step 1: DESCRIPTION**

Create `DESCRIPTION`:
```
Package: BFHmetadata
Title: BFH Metadata Admin (Indikator CRUD)
Version: 0.1.0
Authors@R: person("Johan", "Reventlow", email = "johan@reventlow.dk", role = c("aut", "cre"))
Description: Lokal admin-app til CRUD på BFH-metadata i Supabase. v0: tblIndikatorer.
License: file LICENSE
Encoding: UTF-8
Imports:
    shiny,
    golem,
    bslib,
    DT,
    DBI,
    RPostgres,
    pool,
    config,
    yaml
Suggests:
    testthat (>= 3.0.0)
Config/testthat/edition: 3
RoxygenNote: 7.3.1
```

- [ ] **Step 2: NAMESPACE**

Create `NAMESPACE`:
```
export(run_app)
import(shiny)
```

- [ ] **Step 3: app_config.R (sti-helpers)**

Create `R/app_config.R`:
```r
#' Sti til app-ressource i inst/app
#' @noRd
app_sys <- function(...) system.file(..., package = "BFHmetadata")

#' Læs golem-config (app-niveau indstillinger)
#' @noRd
get_golem_config <- function(value, config = Sys.getenv("GOLEM_CONFIG_ACTIVE", "default"),
                             use_parent = TRUE) {
  config::get(value = value, config = config,
              file = app_sys("golem-config.yml"), use_parent = use_parent)
}
```

- [ ] **Step 4: run_app.R**

Create `R/run_app.R`:
```r
#' Kør CRUD-appen (kun lokalt — host 127.0.0.1)
#' @export
run_app <- function(...) {
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    options = list(host = "127.0.0.1", ...)
  )
}
```

- [ ] **Step 5: app_ui.R + app_server.R (skelet, modul wires i Task 8/10)**

Create `R/app_ui.R`:
```r
#' @import shiny
#' @noRd
app_ui <- function(request) {
  bslib::page_navbar(
    title = "BFH Metadata — Indikatorer",
    bslib::nav_panel("Indikatorer", mod_indikator_crud_ui("indik"))
  )
}
```

Create `R/app_server.R`:
```r
#' @noRd
app_server <- function(input, output, session) {
  pool <- db_connect()
  onStop(function() pool::poolClose(pool))
  db <- make_db(pool)
  mod_indikator_crud_server("indik", db)
}
```

- [ ] **Step 6: inst/golem-config.yml + dev/run_dev.R**

Create `inst/golem-config.yml`:
```yaml
default:
  golem_name: BFHmetadata
  golem_version: 0.1.0
  app_prod: no
```

Create `dev/run_dev.R`:
```r
# Hot-reload udvikling
options(shiny.autoreload = TRUE)
pkgload::load_all(".", reset = TRUE, helpers = FALSE)
run_app()
```

Create empty `inst/app/www/.gitkeep` (`touch inst/app/www/.gitkeep`).

- [ ] **Step 7: Verificér pakke loader**

Run:
```bash
cd /Users/johanreventlow/R/BFHmetadata
Rscript -e 'pkgload::load_all(".", helpers=FALSE); cat("load_all OK\n")' 2>&1 | tail -3
```
Expected: `load_all OK` (advarsler om manglende mod_/db-funktioner er OK indtil senere tasks; hvis load_all fejler pga. manglende funktioner, fortsæt — de defineres i Task 3-8. Kør denne verifikation igen efter Task 8.)

- [ ] **Step 8: Commit**

```bash
git add DESCRIPTION NAMESPACE R/ dev/ inst/
git commit -m "feat(app): Golem-skelet for BFHmetadata CRUD-pakke"
```

---

## Task 3: metadata.R + indikator-felt-definition

**Files:**
- Create: `R/metadata.R`
- Modify: `migration/01_generate_ddl.R`, `migration/02_migrate_data.R` (source fra pakke)

- [ ] **Step 1: Flyt FK/PK-metadata ind i pakken**

Kopiér indholdet af `migration/migration_metadata.R` til `R/metadata.R` (samme `map_odbc_type`, `PK_MAP`, `FK_MAP`, `mk_fk_stmt`). Tilføj derefter felt-definitionen for tblIndikatorer nederst i `R/metadata.R`:

```r
# --- tblIndikatorer felt-metadata til CRUD-form ------------------------------
# kind: pk | fk | text | textarea | bool | int | date
INDIKATOR_FIELDS <- list(
  list(col="id",                       kind="pk"),
  list(col="indikator_hierarki",       kind="fk", parent="tblIndikatorHierarki", label="hierarki_navn"),
  list(col="indikator_navn",           kind="text"),
  list(col="indikator_navn_teknisk",   kind="text"),
  list(col="kontaktperson",            kind="fk", parent="tblPersoner",          label="fornavn||' '||efternavn"),
  list(col="sp_rapport_id",            kind="text"),
  list(col="tillad_auto_opdatering",   kind="bool"),
  list(col="aktiv_indikator",          kind="bool"),
  list(col="nøgleindikator",           kind="bool"),
  list(col="definition_kort",          kind="textarea"),
  list(col="definition_dataportal",    kind="textarea"),
  list(col="tæller_beskrivelse",       kind="textarea"),
  list(col="nævner_beskrivelse",       kind="textarea"),
  list(col="indikator_ukompatibel_med",kind="textarea"),
  list(col="mål",                      kind="text"),
  list(col="datakilde",                kind="fk", parent="tblDatakilder",        label="datakilde_navn"),
  list(col="direkte_link",             kind="text"),
  list(col="ønsket_tendens",           kind="text"),
  list(col="antal_observationer",      kind="int"),
  list(col="periode_fra",              kind="date"),
  list(col="output_enhed",             kind="text")
)

# Felter sikre til inline DT-redigering (simple tekst/tal — ej FK/bool/date)
INLINE_EDITABLE <- c("indikator_navn", "mål", "output_enhed",
                     "direkte_link", "ønsket_tendens")
```

- [ ] **Step 2: Migration-scripts sourcer fra pakke (undgå dublet)**

Slet `migration/migration_metadata.R`:
```bash
git rm migration/migration_metadata.R
```
I `migration/01_generate_ddl.R` og `migration/02_migrate_data.R`, ændr:
```r
source("migration_metadata.R", encoding = "UTF-8")
```
til:
```r
# Metadata bor nu i pakkens R/metadata.R (én sandhedskilde)
source(if (file.exists("../R/metadata.R")) "../R/metadata.R" else "R/metadata.R", encoding = "UTF-8")
```

- [ ] **Step 3: Verificér migration stadig kører + pakke loader**

Run:
```bash
cd /Users/johanreventlow/R/BFHmetadata/migration && Rscript 01_generate_ddl.R 2>&1 | tail -1
cd /Users/johanreventlow/R/BFHmetadata && Rscript -e 'source("R/metadata.R"); cat(length(FK_MAP),"FK,",length(INDIKATOR_FIELDS),"felter\n")' 2>&1 | tail -1
```
Expected: `Færdig...` og `17 FK, 21 felter`.

- [ ] **Step 4: Commit**

```bash
git add R/metadata.R migration/
git commit -m "feat(metadata): flyt FK/PK-metadata til pakke + INDIKATOR_FIELDS

Én sandhedskilde for skema-metadata; migration-scripts sourcer fra R/metadata.R.
Tilføjer felt-definition + inline-editable-liste for CRUD-form."
```

---

## Task 4: SQL-byggere (rene funktioner, TDD)

**Files:**
- Create: `R/fct_sql.R`
- Test: `tests/testthat/test-sql.R`

- [ ] **Step 1: Skriv failing tests**

Create `tests/testthat/test-sql.R`:
```r
test_that("build_list_sql joiner alle 3 FK-parents med labels", {
  sql <- build_list_sql()
  expect_match(sql, 'FROM "tblIndikatorer"')
  expect_match(sql, '"tblIndikatorHierarki"')
  expect_match(sql, '"tblPersoner"')
  expect_match(sql, '"tblDatakilder"')
  expect_match(sql, "hierarki_navn")
  expect_match(sql, "datakilde_navn")
})

test_that("build_fk_options_sql bygger id+label select for parent", {
  sql <- build_fk_options_sql("tblDatakilder", "datakilde_navn")
  expect_match(sql, '"Id"')
  expect_match(sql, "datakilde_navn")
  expect_match(sql, 'FROM "tblDatakilder"')
})

test_that("build_update_sql bruger parametriserede placeholders", {
  res <- build_update_sql(c("indikator_navn", "mål"))
  expect_match(res, 'UPDATE "tblIndikatorer" SET')
  expect_match(res, '"indikator_navn" = \\$1')
  expect_match(res, '"mål" = \\$2')
  expect_match(res, 'WHERE "id" = \\$3')
})

test_that("build_insert_sql returnerer RETURNING id", {
  res <- build_insert_sql(c("indikator_navn", "datakilde"))
  expect_match(res, 'INSERT INTO "tblIndikatorer"')
  expect_match(res, "RETURNING \"id\"")
  expect_match(res, "\\$1, \\$2")
})
```

- [ ] **Step 2: Kør tests — verificér de fejler**

Run:
```bash
cd /Users/johanreventlow/R/BFHmetadata
Rscript -e 'pkgload::load_all(".",helpers=FALSE); testthat::test_file("tests/testthat/test-sql.R")' 2>&1 | tail -8
```
Expected: FAIL ("could not find function build_list_sql").

- [ ] **Step 3: Implementér SQL-byggere**

Create `R/fct_sql.R`:
```r
# Rene SQL-byggere for tblIndikatorer. Identifiers double-quotes (bevar casing+æøå).
# Bruger $n-placeholders (RPostgres parametrisering) → ingen SQL-injection.

#' @noRd
.fk_fields <- function() Filter(function(f) f$kind == "fk", INDIKATOR_FIELDS)

#' SELECT med FK-labels (LEFT JOIN så NULL-FK bevares)
#' @noRd
build_list_sql <- function() {
  base_cols <- vapply(INDIKATOR_FIELDS, function(f) sprintf('i."%s"', f$col), "")
  joins <- character(0); labels <- character(0)
  for (f in .fk_fields()) {
    al <- paste0("p_", f$col)
    labels <- c(labels, sprintf('(%s) AS "label_%s"',
                  gsub("([a-zæøå_]+)", sprintf('%s.\\1', al), f$label, perl = TRUE), f$col))
    joins  <- c(joins, sprintf('LEFT JOIN "%s" %s ON %s."Id" = i."%s"',
                  f$parent, al, al, f$col))
  }
  sprintf('SELECT %s, %s FROM "tblIndikatorer" i %s ORDER BY i."id"',
          paste(base_cols, collapse = ", "), paste(labels, collapse = ", "),
          paste(joins, collapse = " "))
}

#' id + label for FK-dropdown
#' @noRd
build_fk_options_sql <- function(parent, label_expr) {
  sprintf('SELECT "Id" AS id, (%s) AS label FROM "%s" ORDER BY 2', label_expr, parent)
}

#' Parametriseret UPDATE; cols → $1..$n, id → $(n+1)
#' @noRd
build_update_sql <- function(cols) {
  sets <- vapply(seq_along(cols), function(i) sprintf('"%s" = $%d', cols[i], i), "")
  sprintf('UPDATE "tblIndikatorer" SET %s WHERE "id" = $%d',
          paste(sets, collapse = ", "), length(cols) + 1)
}

#' Parametriseret INSERT med RETURNING id
#' @noRd
build_insert_sql <- function(cols) {
  ph <- paste(sprintf("$%d", seq_along(cols)), collapse = ", ")
  qcols <- paste(sprintf('"%s"', cols), collapse = ", ")
  sprintf('INSERT INTO "tblIndikatorer" (%s) VALUES (%s) RETURNING "id"', qcols, ph)
}

#' Soft-delete / gendan
#' @noRd
build_soft_delete_sql <- function() {
  'UPDATE "tblIndikatorer" SET "aktiv_indikator" = $1 WHERE "id" = $2'
}
```

- [ ] **Step 4: Kør tests — verificér de består**

Run:
```bash
Rscript -e 'pkgload::load_all(".",helpers=FALSE); testthat::test_file("tests/testthat/test-sql.R")' 2>&1 | tail -6
```
Expected: PASS (4 tests).

- [ ] **Step 5: Commit**

```bash
git add R/fct_sql.R tests/testthat/test-sql.R
git commit -m "feat(sql): parametriserede SQL-byggere for tblIndikatorer (TDD)"
```

---

## Task 5: Validering (TDD)

**Files:**
- Create: `R/utils_validation.R`
- Test: `tests/testthat/test-validation.R`

- [ ] **Step 1: Skriv failing tests**

Create `tests/testthat/test-validation.R`:
```r
test_that("validate_indikator kræver ikke-tomt indikator_navn", {
  errs <- validate_indikator(list(indikator_navn = ""))
  expect_true(any(grepl("indikator_navn", errs)))
})

test_that("validate_indikator accepterer gyldig række", {
  errs <- validate_indikator(list(indikator_navn = "Genindlæggelser",
                                  antal_observationer = 30))
  expect_length(errs, 0)
})

test_that("validate_indikator afviser ikke-numerisk antal_observationer", {
  errs <- validate_indikator(list(indikator_navn = "X",
                                  antal_observationer = "abc"))
  expect_true(any(grepl("antal_observationer", errs)))
})

test_that("validate_indikator tillader NA/NULL antal_observationer", {
  errs <- validate_indikator(list(indikator_navn = "X", antal_observationer = NA))
  expect_length(errs, 0)
})
```

- [ ] **Step 2: Kør tests — verificér de fejler**

Run:
```bash
Rscript -e 'pkgload::load_all(".",helpers=FALSE); testthat::test_file("tests/testthat/test-validation.R")' 2>&1 | tail -6
```
Expected: FAIL ("could not find function validate_indikator").

- [ ] **Step 3: Implementér validering**

Create `R/utils_validation.R`:
```r
#' Validér indikator-værdier før gem. Returnerer char-vektor af fejl (tom = OK).
#' Konservativ: kun praktiske krav (skema tillader NULL på det meste).
#' @noRd
validate_indikator <- function(values) {
  errs <- character(0)
  nm <- values[["indikator_navn"]]
  if (is.null(nm) || !nzchar(trimws(as.character(nm %||% "")))) {
    errs <- c(errs, "indikator_navn må ikke være tom")
  }
  ao <- values[["antal_observationer"]]
  if (!is.null(ao) && !is.na(ao) && nzchar(as.character(ao))) {
    if (is.na(suppressWarnings(as.numeric(ao)))) {
      errs <- c(errs, "antal_observationer skal være et tal")
    }
  }
  errs
}

#' NULL-coalesce
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
```

- [ ] **Step 4: Kør tests — verificér de består**

Run:
```bash
Rscript -e 'pkgload::load_all(".",helpers=FALSE); testthat::test_file("tests/testthat/test-validation.R")' 2>&1 | tail -6
```
Expected: PASS (4 tests).

- [ ] **Step 5: Commit**

```bash
git add R/utils_validation.R tests/testthat/test-validation.R
git commit -m "feat(validation): validate_indikator (TDD)"
```

---

## Task 6: DB-lag (pool, config, write-guard, accessors)

**Files:**
- Create: `R/fct_db.R`
- Test: `tests/testthat/test-db-guard.R`

- [ ] **Step 1: Skriv failing tests for write-guard + config**

Create `tests/testthat/test-db-guard.R`:
```r
test_that("write_enabled er FALSE som default", {
  withr::with_envvar(c(BFHMETA_WRITE = ""), {
    withr::with_options(list(bfhmeta.write_enabled = NULL), {
      expect_false(write_enabled())
    })
  })
})

test_that("write_enabled TRUE via env eller option", {
  withr::with_envvar(c(BFHMETA_WRITE = "1"), expect_true(write_enabled()))
  withr::with_envvar(c(BFHMETA_WRITE = ""), {
    withr::with_options(list(bfhmeta.write_enabled = TRUE),
                        expect_true(write_enabled()))
  })
})

test_that("assert_write_enabled fejler når disabled", {
  withr::with_envvar(c(BFHMETA_WRITE = ""), {
    withr::with_options(list(bfhmeta.write_enabled = NULL), {
      expect_error(assert_write_enabled(), "skrivning")
    })
  })
})
```

(Tilføj `withr` til DESCRIPTION Suggests hvis ej til stede.)

- [ ] **Step 2: Kør — verificér fejl**

Run:
```bash
Rscript -e 'pkgload::load_all(".",helpers=FALSE); testthat::test_file("tests/testthat/test-db-guard.R")' 2>&1 | tail -6
```
Expected: FAIL ("could not find function write_enabled").

- [ ] **Step 3: Implementér db-lag**

Create `R/fct_db.R`:
```r
#' Læs supabase-DB-config fra rod-config.yml
#' @noRd
db_config <- function() {
  path <- if (file.exists("config.yml")) "config.yml" else app_sys("../config.yml")
  yaml::read_yaml(path)$default$supabase
}

#' Er skrivning aktiveret? (write-guard — bevidst friktion mod forkert target)
#' @noRd
write_enabled <- function() {
  isTRUE(getOption("bfhmeta.write_enabled")) ||
    identical(Sys.getenv("BFHMETA_WRITE"), "1")
}

#' Stop hvis skrivning ej aktiveret
#' @noRd
assert_write_enabled <- function() {
  if (!write_enabled()) {
    stop("Skrivning er deaktiveret. Sæt BFHMETA_WRITE=1 eller ",
         "options(bfhmeta.write_enabled=TRUE) efter at have bekræftet target.",
         call. = FALSE)
  }
}

#' Opret pool mod Supabase (postgres-rolle, bypasser RLS — admin-tooling)
#' @noRd
db_connect <- function() {
  cfg <- db_config()
  pw <- Sys.getenv("SUPABASE_DB_PASSWORD")
  if (!nzchar(pw)) stop("SUPABASE_DB_PASSWORD mangler i .Renviron", call. = FALSE)
  pool::dbPool(RPostgres::Postgres(), host = cfg$host, port = cfg$port,
    dbname = cfg$dbname, user = cfg$user, password = pw, sslmode = cfg$sslmode)
}

#' Byg db-accessor-liste bundet til pool (dependency injection til modul/test)
#' @noRd
make_db <- function(pool) {
  list(
    list_indikatorer = function() DBI::dbGetQuery(pool, build_list_sql()),
    fk_options = function() {
      stats::setNames(lapply(.fk_fields(), function(f)
        DBI::dbGetQuery(pool, build_fk_options_sql(f$parent, f$label))),
        vapply(.fk_fields(), function(f) f$col, ""))
    },
    create_indikator = function(values) {
      assert_write_enabled()
      cols <- names(values)
      DBI::dbGetQuery(pool, build_insert_sql(cols), params = unname(values))$id[1]
    },
    update_indikator = function(id, values) {
      assert_write_enabled()
      cols <- names(values)
      DBI::dbExecute(pool, build_update_sql(cols), params = c(unname(values), list(id)))
    },
    soft_delete = function(id, active = FALSE) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_soft_delete_sql(), params = list(active, id))
    }
  )
}
```

- [ ] **Step 4: Kør tests — verificér de består**

Run:
```bash
Rscript -e 'pkgload::load_all(".",helpers=FALSE); testthat::test_file("tests/testthat/test-db-guard.R")' 2>&1 | tail -6
```
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add R/fct_db.R tests/testthat/test-db-guard.R DESCRIPTION
git commit -m "feat(db): pool-forbindelse, write-guard, db-accessors (DI)"
```

---

## Task 7: CRUD-modul UI

**Files:**
- Create: `R/mod_indikator_crud.R` (UI-del)

- [ ] **Step 1: Implementér modul-UI**

Create `R/mod_indikator_crud.R`:
```r
#' Bygger ét form-input baseret på felt-kind
#' @noRd
.field_input <- function(ns, f, fk_choices = list()) {
  id <- ns(f$col)
  switch(f$kind,
    "pk"       = NULL,
    "fk"       = selectInput(id, f$col, choices = c("(ingen)" = "", fk_choices[[f$col]])),
    "bool"     = checkboxInput(id, f$col, value = FALSE),
    "date"     = dateInput(id, f$col, value = NA),
    "int"      = numericInput(id, f$col, value = NA),
    "textarea" = textAreaInput(id, f$col, value = ""),
    textInput(id, f$col, value = "")  # text (default)
  )
}

#' @noRd
mod_indikator_crud_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 420, position = "right", open = TRUE,
      h5("Redigér / opret"),
      uiOutput(ns("form")),
      div(class = "d-flex gap-2 mt-2",
        actionButton(ns("new"), "Ny", class = "btn-secondary"),
        actionButton(ns("save"), "Gem", class = "btn-primary"),
        actionButton(ns("soft_delete"), "Deaktivér", class = "btn-warning")
      ),
      verbatimTextOutput(ns("status"))
    ),
    div(
      checkboxInput(ns("show_inactive"), "Vis inaktive", value = TRUE),
      DT::DTOutput(ns("tbl"))
    )
  )
}
```

- [ ] **Step 2: Verificér UI-funktion parser**

Run:
```bash
Rscript -e 'pkgload::load_all(".",helpers=FALSE); ui <- mod_indikator_crud_ui("x"); cat("UI OK:", class(ui)[1], "\n")' 2>&1 | tail -2
```
Expected: `UI OK: shiny.tag.list` eller lignende tag-klasse.

- [ ] **Step 3: Commit**

```bash
git add R/mod_indikator_crud.R
git commit -m "feat(mod): indikator CRUD-modul UI (liste + form + knapper)"
```

---

## Task 8: CRUD-modul server + testServer (TDD)

**Files:**
- Modify: `R/mod_indikator_crud.R` (server-del)
- Test: `tests/testthat/test-mod-crud.R`

- [ ] **Step 1: Skriv failing testServer-tests med fake db**

Create `tests/testthat/test-mod-crud.R`:
```r
fake_db <- function() {
  store <- data.frame(id = 1L, indikator_navn = "A", aktiv_indikator = TRUE,
                      stringsAsFactors = FALSE)
  calls <- list(created = NULL, updated = NULL, deleted = NULL)
  list(
    list_indikatorer = function() store,
    fk_options = function() list(
      indikator_hierarki = data.frame(id = 1L, label = "H1"),
      kontaktperson = data.frame(id = 1L, label = "Per Sen"),
      datakilde = data.frame(id = 1L, label = "SP")),
    create_indikator = function(values) { calls$created <<- values; 99L },
    update_indikator = function(id, values) { calls$updated <<- list(id, values); 1L },
    soft_delete = function(id, active = FALSE) { calls$deleted <<- list(id, active); 1L },
    .calls = function() calls
  )
}

test_that("modul indlæser data ved start", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    expect_equal(nrow(rows()), 1)
  })
})

test_that("Gem med tomt navn giver valideringsfejl, ingen update", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl_rows_selected = 1, indikator_navn = "", save = 1)
    expect_match(status_msg(), "indikator_navn")
  })
})

test_that("soft_delete kalder db.soft_delete med active=FALSE", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl_rows_selected = 1, soft_delete = 1)
    expect_equal(db$.calls()$deleted[[2]], FALSE)
  })
})
```

- [ ] **Step 2: Kør — verificér fejl**

Run:
```bash
Rscript -e 'pkgload::load_all(".",helpers=FALSE); testthat::test_file("tests/testthat/test-mod-crud.R")' 2>&1 | tail -8
```
Expected: FAIL ("could not find function mod_indikator_crud_server").

- [ ] **Step 3: Implementér modul-server**

Tilføj til `R/mod_indikator_crud.R`:
```r
#' @noRd
.collect_form <- function(input, fields) {
  vals <- list()
  for (f in fields) {
    if (f$kind == "pk") next
    v <- input[[f$col]]
    if (f$kind == "bool") v <- isTRUE(v)
    if (f$kind %in% c("text","textarea","fk") && identical(v, "")) v <- NA
    vals[[f$col]] <- v
  }
  vals
}

#' @noRd
mod_indikator_crud_server <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    rows <- reactiveVal(db$list_indikatorer())
    status_msg <- reactiveVal("")
    fk <- db$fk_options()
    fk_choices <- lapply(fk, function(d) stats::setNames(d$id, d$label))

    reload <- function() rows(db$list_indikatorer())

    output$form <- renderUI({
      ns <- session$ns
      tagList(lapply(INDIKATOR_FIELDS, function(f) .field_input(ns, f, fk_choices)))
    })

    output$tbl <- DT::renderDT({
      d <- rows()
      if (!isTRUE(input$show_inactive) && "aktiv_indikator" %in% names(d))
        d <- d[isTRUE(d$aktiv_indikator) | d$aktiv_indikator, , drop = FALSE]
      DT::datatable(d, selection = "single", rownames = FALSE)
    })

    selected_id <- reactive({
      sel <- input$tbl_rows_selected
      if (is.null(sel)) return(NULL)
      rows()[["id"]][sel]
    })

    observeEvent(input$save, {
      vals <- .collect_form(input, INDIKATOR_FIELDS)
      errs <- validate_indikator(vals)
      if (length(errs) > 0) { status_msg(paste(errs, collapse = "; ")); return() }
      res <- safe_operation("gem indikator", {
        sid <- selected_id()
        if (is.null(sid)) {
          newid <- db$create_indikator(vals); status_msg(paste("Oprettet id", newid))
        } else {
          db$update_indikator(sid, vals); status_msg("Gemt")
        }
        reload()
      }, fallback = status_msg("Fejl ved gem (se log)"))
    })

    observeEvent(input$soft_delete, {
      sid <- selected_id()
      if (is.null(sid)) { status_msg("Vælg en række først"); return() }
      safe_operation("soft-delete", {
        db$soft_delete(sid, active = FALSE); status_msg("Deaktiveret"); reload()
      }, fallback = status_msg("Fejl ved deaktivering"))
    })

    output$status <- renderText(status_msg())

    # eksponér til test
    list(rows = rows, status_msg = status_msg)
  })
}

#' Minimal safe_operation (logger + fallback)
#' @noRd
safe_operation <- function(op, code, fallback = NULL) {
  tryCatch(force(code), error = function(e) {
    message(sprintf("[ERROR] %s: %s", op, conditionMessage(e)))
    force(fallback)
  })
}
```

- [ ] **Step 4: Kør tests — verificér de består**

Run:
```bash
Rscript -e 'pkgload::load_all(".",helpers=FALSE); testthat::test_file("tests/testthat/test-mod-crud.R")' 2>&1 | tail -8
```
Expected: PASS (3 tests). Hvis `status_msg()`/`rows()` ej tilgængelig i testServer-scope, sikr at server returnerer listen (sidste linje) — testServer eksponerer returnerede reactives.

- [ ] **Step 5: Commit**

```bash
git add R/mod_indikator_crud.R tests/testthat/test-mod-crud.R
git commit -m "feat(mod): indikator CRUD-server (load/create/update/soft-delete) + testServer"
```

---

## Task 9: Inline DT-redigering (sekundær)

**Files:**
- Modify: `R/mod_indikator_crud.R` (tilføj editable + observeEvent for celle-edit)
- Test: `tests/testthat/test-mod-crud.R` (tilføj inline-test)

- [ ] **Step 1: Tilføj failing inline-test**

Tilføj til `tests/testthat/test-mod-crud.R`:
```r
test_that("inline-edit på editable felt kalder update", {
  db <- fake_db()
  testServer(mod_indikator_crud_server, args = list(db = db), {
    session$setInputs(tbl_cell_edit = list(row = 1, col = which(names(db$list_indikatorer())=="indikator_navn")-1, value = "Nyt navn"))
    expect_false(is.null(db$.calls()$updated))
  })
})
```

- [ ] **Step 2: Kør — verificér fejl**

Run:
```bash
Rscript -e 'pkgload::load_all(".",helpers=FALSE); testthat::test_file("tests/testthat/test-mod-crud.R")' 2>&1 | tail -6
```
Expected: FAIL (ingen tbl_cell_edit-håndtering → update ikke kaldt).

- [ ] **Step 3: Gør DT editable for sikre kolonner + håndtér celle-edit**

I `output$tbl`-renderDT, gør editable kun for `INLINE_EDITABLE`-kolonner:
```r
    output$tbl <- DT::renderDT({
      d <- rows()
      if (!isTRUE(input$show_inactive) && "aktiv_indikator" %in% names(d))
        d <- d[d$aktiv_indikator %in% TRUE, , drop = FALSE]
      editable_cols <- which(names(d) %in% INLINE_EDITABLE) - 1
      DT::datatable(d, selection = "single", rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = setdiff(seq_len(ncol(d))-1, editable_cols))))
    })
```
Tilføj observer (efter eksisterende observere):
```r
    observeEvent(input$tbl_cell_edit, {
      info <- input$tbl_cell_edit
      d <- rows(); col <- names(d)[info$col + 1]
      if (!col %in% INLINE_EDITABLE) { status_msg("Kolonne ej redigerbar inline"); return() }
      rid <- d[["id"]][info$row]
      safe_operation("inline-update", {
        db$update_indikator(rid, stats::setNames(list(info$value), col))
        status_msg(paste("Opdateret", col)); reload()
      }, fallback = status_msg("Fejl ved inline-update"))
    })
```

- [ ] **Step 4: Kør tests — verificér de består**

Run:
```bash
Rscript -e 'pkgload::load_all(".",helpers=FALSE); testthat::test_file("tests/testthat/test-mod-crud.R")' 2>&1 | tail -6
```
Expected: PASS (4 tests).

- [ ] **Step 5: Commit**

```bash
git add R/mod_indikator_crud.R tests/testthat/test-mod-crud.R
git commit -m "feat(mod): inline DT-redigering for sikre tekstfelter"
```

---

## Task 10: App-samling + manuel smoke-test

**Files:**
- Verify: hele pakken loader + kører
- Create: `dev/smoke_supabase.R` (manuel integration)

- [ ] **Step 1: Kør hele test-suiten**

Run:
```bash
cd /Users/johanreventlow/R/BFHmetadata
Rscript -e 'pkgload::load_all(".",helpers=FALSE); testthat::test_dir("tests/testthat")' 2>&1 | tail -10
```
Expected: alle tests PASS (sql 4, validation 4, db-guard 3, mod-crud 4).

- [ ] **Step 2: Verificér app starter (uden at blokere)**

Run:
```bash
Rscript -e 'pkgload::load_all(".",helpers=FALSE); app <- run_app(); cat("app-objekt:", class(app)[1], "\n")' 2>&1 | tail -3
```
Expected: `app-objekt: shiny.appobj`.

- [ ] **Step 3: Manuel smoke-test-script (integration mod Supabase)**

Create `dev/smoke_supabase.R`:
```r
# Manuel: verificér read + write-round-trip mod Supabase. KRÆVER .Renviron + BFHMETA_WRITE=1.
pkgload::load_all(".", helpers = FALSE)
pool <- db_connect(); on.exit(pool::poolClose(pool))
db <- make_db(pool)
cat("Antal indikatorer:", nrow(db$list_indikatorer()), "\n")
cat("FK-options datakilde:\n"); print(utils::head(db$fk_options()$datakilde, 3))
# Write-round-trip (kræver BFHMETA_WRITE=1):
if (write_enabled()) {
  id <- db$create_indikator(list(indikator_navn = "__smoke__", aktiv_indikator = TRUE))
  cat("Oprettet id:", id, "\n")
  db$soft_delete(id, FALSE); cat("Soft-deleted\n")
  DBI::dbExecute(pool, 'DELETE FROM "tblIndikatorer" WHERE "id"=$1', params = list(id))
  cat("Oprydning: hard-deleted smoke-række\n")
} else cat("BFHMETA_WRITE ej sat — springer write-test over\n")
```

- [ ] **Step 4: Kør manuel smoke (bruger udfører — kræver bekræftelse)**

**[MANUELT TRIN]** Bruger kører:
```bash
BFHMETA_WRITE=1 Rscript dev/smoke_supabase.R
```
Expected: antal indikatorer (836), FK-options, oprettet+soft-deleted+oprydning. Bekræfter end-to-end write-back mod Supabase.

- [ ] **Step 5: Opdater README + commit**

Tilføj kort afsnit til `README.md` (app-start: `Rscript -e 'pkgload::load_all(".");run_app()'`, write-guard, lokal-only). Commit:
```bash
git add dev/smoke_supabase.R README.md
git commit -m "feat(app): smoke-test-script + README; v0 CRUD komplet"
```

---

## Self-Review-noter

- **Spec-dækning:** scope (tblIndikatorer ✓ Task 3-8), fuld CRUD+soft-delete (✓ Task 8), ingen auth + write-guard (✓ Task 6), master-detail + inline (✓ Task 7-9), Golem (✓ Task 2), restructure (✓ Task 1), FK_MAP-genbrug (✓ Task 3), hard-delete manuel (dokumenteret i spec, ej app — bevidst).
- **Åbne punkter fra spec løst:** label-kolonner pinnet (Task 3), inline-scope pinnet til `INLINE_EDITABLE` (Task 3/9), metadata-flytning + sti-fix (Task 3), migration-sti-robusthed (Task 1).
- **Type-konsistens:** `db`-accessor-navne (`list_indikatorer`, `fk_options`, `create_indikator`, `update_indikator`, `soft_delete`) ens i fct_db, fake_db, modul. `build_*_sql`-navne ens i fct_sql + tests + accessors.
- **Risici:** `build_list_sql`'s label-regex (`gsub`) på `fornavn||' '||efternavn` skal verificeres ved kørsel (Task 4 test dækker join+kolonne, men den sammensatte person-label bør smoke-testes i Task 10 step 3).
