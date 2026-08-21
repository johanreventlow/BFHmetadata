# Signal/CRUD Isolation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make BFHmetadata installable and usable for Supabase CRUD without Arrow or local parquet data, while preserving full signal scanning and compaction where those capabilities exist.

**Architecture:** Keep the existing lazy signal module on `origin/main`, but introduce one typed capability boundary in `R/fct_parquet.R`. Signal review and eager startup compaction consult that boundary before any Arrow call; low-level parquet reads retain per-indicator error isolation and surface read failures through the existing progressive scan state.

**Tech Stack:** R 4.1+, Shiny/Golem, testthat 3, Arrow as an optional suggested package, yaml/config, DBI/RPostgres/pool, later, devtools, R CMD build/check.

**Spec:** `docs/superpowers/specs/2026-08-21-origin-main-hardening-design.md`

## Global Constraints

- Start from `origin/main` commit `40a1716ff9a59b2e8897b4919bb6ddc0a9a80eeb`; do not merge or cherry-pick UI patches from `feat/crud-fundament`.
- Keep excelR as the table engine; this delivery must not modify the excelR CRUD flow.
- Missing Arrow, a missing directory, an empty directory, or corrupt parquet data must never stop ordinary database CRUD.
- Keep the existing lazy signal-module initialization in `R/app_server.R`; do not add a second `active` flag.
- Treat DB passwords and all other secrets as environment variables; never place them in package resources, tests, logs, or commits.
- Report focused and full-suite test results separately.
- Baseline at the branch point is 1,292 PASS, 0 FAIL, 0 WARN, and 15 explicitly skipped DB integration tests.

---

## File Map

- `R/fct_parquet.R`: owns Arrow availability checks, shallow storage discovery, and the typed signal capability result.
- `R/fct_compact.R`: keeps compact-file reads behind the same Arrow boundary.
- `R/mod_signal_review.R`: renders capability status and prevents unavailable scans before DB scan queries begin.
- `R/mod_compact.R`: suppresses startup/manual compaction when Arrow is unavailable.
- `R/fct_db.R`: resolves and validates explicit, development, or packaged DB configuration.
- `DESCRIPTION`: moves Arrow from `Imports` to `Suggests`.
- `.Rbuildignore`: excludes the development-only root `config.yml` from source packages.
- `inst/db-config.yml`: contains only the tracked non-secret Supabase connection fields needed by an installed app.
- `Renviron.example`: documents the optional explicit config-path override; no real values.
- `tests/testthat/test-parquet.R`: low-level optional-Arrow and capability tests.
- `tests/testthat/test-mod-signal-review.R`: signal-tab state, scan guard, mixed-result, and read-error tests.
- `tests/testthat/test-mod-compact.R`: eager compaction behavior without Arrow.
- `tests/testthat/test-db-guard.R`: packaged/explicit config resolution and validation.
- `NEWS.md`: documents the behavior and installation change.

---

### Task 1: Make Arrow optional at the low-level parquet boundary

**Files:**
- Modify: `DESCRIPTION:8-31`
- Modify: `R/fct_parquet.R:17-37`
- Modify: `R/fct_compact.R:239-263`
- Modify: `tests/testthat/test-parquet.R:1-48`

**Interfaces:**
- Produces: `.require_arrow(available)`; invisibly returns `TRUE` or raises class `bfhmeta_arrow_unavailable`.
- Produces: `parquet_files_present(path)`; returns one logical value without loading Arrow.
- Changes: `parquet_load_slice(path, enhed = NULL, from = NULL, to = NULL, arrow_available = requireNamespace("arrow", quietly = TRUE))`; returns `NULL` before checking Arrow when no parquet file exists.
- Consumes: all existing callers continue using the first four arguments unchanged.

- [ ] **Step 1: Add failing dependency and empty-directory tests**

Move fixture skipping into the fixture itself and append these tests to `tests/testthat/test-parquet.R`:

```r
make_parquet_fixture <- function(env = parent.frame()) {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir(.local_envir = env)
  ind <- file.path(base, "test_ind")
  dir.create(ind, recursive = TRUE)
  d <- data.frame(
    dato = as.Date("2020-01-01") + 0:5 * 30,
    vaerdi = c(1, 2, 3, 4, 5, 6),
    taeller = NA_real_, naevner = NA_real_,
    enhed = rep("Afd X", 6), stringsAsFactors = FALSE
  )
  arrow::write_parquet(d, file.path(ind, "part-0.parquet"))
  base
}

test_that("Arrow er Suggests, ikke Imports", {
  dcf <- read.dcf(
    testthat::test_path("..", "..", "DESCRIPTION"),
    fields = c("Imports", "Suggests")
  )
  expect_false(grepl("arrow", dcf[1, "Imports"], fixed = TRUE))
  expect_match(dcf[1, "Suggests"], "arrow", fixed = TRUE)
})

test_that("manglende og tom indikatormappe kræver ikke Arrow", {
  base <- withr::local_tempdir()
  empty <- file.path(base, "tom")
  dir.create(empty)

  expect_null(parquet_load_slice(file.path(base, "findes-ikke"),
                                 arrow_available = FALSE))
  expect_null(parquet_load_slice(empty, arrow_available = FALSE))
})

test_that("parquet-filer uden Arrow giver typed handlingsanvisende fejl", {
  ind <- withr::local_tempdir()
  writeBin(charToRaw("ikke vigtig for availability-testen"),
           file.path(ind, "part-0.parquet"))

  expect_error(
    parquet_load_slice(ind, arrow_available = FALSE),
    "Signal-gennemgang kræver R-pakken 'arrow'",
    class = "bfhmeta_arrow_unavailable"
  )
})
```

- [ ] **Step 2: Run the focused tests and confirm the intended failures**

Run:

```bash
Rscript -e 'devtools::test(filter = "parquet")'
```

Expected: the dependency test fails because Arrow is still in `Imports`; the empty directory attempts `arrow::open_dataset()`; the typed class does not yet exist.

- [ ] **Step 3: Implement the optional dependency boundary**

Move `arrow` from `Imports` to `Suggests` in `DESCRIPTION`. Add this code above `parquet_load_slice()` in `R/fct_parquet.R`:

```r
#' Stop med en typed, handlingsanvisende fejl når Arrow mangler.
#' @noRd
.require_arrow <- function(available = requireNamespace("arrow", quietly = TRUE)) {
  if (!isTRUE(available)) {
    cond <- simpleError(
      "Signal-gennemgang kræver R-pakken 'arrow'. Database-CRUD kan bruges uden."
    )
    class(cond) <- c("bfhmeta_arrow_unavailable", class(cond))
    stop(cond)
  }
  invisible(TRUE)
}

#' Har mappen mindst én parquet-fil? Kalder aldrig Arrow.
#' @noRd
parquet_files_present <- function(path) {
  if (length(path) != 1L || is.na(path) || !dir.exists(path)) return(FALSE)
  length(list.files(
    path, pattern = "\\.parquet$", recursive = TRUE,
    full.names = FALSE, ignore.case = TRUE
  )) > 0L
}
```

Change the beginning of `parquet_load_slice()` to:

```r
parquet_load_slice <- function(path, enhed = NULL, from = NULL, to = NULL,
                               arrow_available = requireNamespace(
                                 "arrow", quietly = TRUE
                               )) {
  if (!parquet_files_present(path)) return(NULL)
  .require_arrow(arrow_available)
  ds <- arrow::open_dataset(path)
  # existing filtering, collection, date coercion, and return contract follow
}
```

In `parquet_load_indicator_best()` call `.require_arrow()` immediately before `arrow::read_parquet(f)`. Keep its `tryCatch` fallback so a corrupt compact mirror still falls back to raw storage.

- [ ] **Step 4: Run low-level and scan regression tests**

Run:

```bash
Rscript -e 'devtools::test(filter = "parquet|scan|compact")'
```

Expected: PASS. Tests requiring real Arrow may skip when it is not installed; the three new boundary tests must run regardless.

- [ ] **Step 5: Commit the optional dependency boundary**

```bash
git add DESCRIPTION R/fct_parquet.R R/fct_compact.R tests/testthat/test-parquet.R
git commit -m "fix(signal): make Arrow optional for CRUD installs"
```

---

### Task 2: Add a typed capability model to signal review

**Files:**
- Modify: `R/fct_parquet.R`
- Modify: `R/mod_signal_review.R:5-96,292-486,579-619,950-957`
- Modify: `tests/testthat/test-parquet.R`
- Modify: `tests/testthat/test-mod-signal-review.R`

**Interfaces:**
- Produces: `signal_data_capability(base_path, arrow_available)` returning exactly `list(state, message)` where state is one of `klar`, `ingen_data`, `arrow_mangler`.
- Changes: `mod_signal_review_server(id, db, arrow_available = function() requireNamespace("arrow", quietly = TRUE))`.
- Produces to tests: module return value `availability`, a `reactiveVal` holding `list(state, message)`; existing returned reactives remain unchanged.

- [ ] **Step 1: Add failing pure capability tests**

Append to `tests/testthat/test-parquet.R`:

```r
test_that("signal_data_capability skelner manglende, tom, Arrow og klar", {
  expect_equal(signal_data_capability("", FALSE)$state, "ingen_data")

  base <- withr::local_tempdir()
  expect_equal(signal_data_capability(base, FALSE)$state, "ingen_data")

  part <- file.path(base, "ind", "dato=2026-01-01")
  dir.create(part, recursive = TRUE)
  writeBin(charToRaw("availability ser kun filnavnet"),
           file.path(part, "part-0.parquet"))

  expect_equal(signal_data_capability(base, FALSE)$state, "arrow_mangler")
  expect_equal(signal_data_capability(base, TRUE)$state, "klar")
})

test_that("capability-scan er begrænset til understøttet mappedybde", {
  base <- withr::local_tempdir()
  too_deep <- file.path(base, "gruppe", "ekstra", "ind", "dato=2026-01-01")
  dir.create(too_deep, recursive = TRUE)
  writeBin(charToRaw("x"), file.path(too_deep, "part-0.parquet"))

  expect_equal(signal_data_capability(base, TRUE)$state, "ingen_data")
})
```

- [ ] **Step 2: Add a failing module guard test**

Append to `tests/testthat/test-mod-signal-review.R`:

```r
test_that("scan uden lokale data stopper før scan-DB-kald", {
  base <- withr::local_tempdir()
  idx <- data.frame(
    diagram_id = 1L, indikator_id = 1L, indikator_navn = "A",
    indikator_navn_teknisk = "a", datasaet = "d", datapakke = "p",
    org_id = 5L, org_teknisk = "E", org_navn = "E", org_niveau = 5L,
    overafdeling = "OA", afdeling = NA, afsnit = NA,
    stringsAsFactors = FALSE
  )
  calls <- new.env(parent = emptyenv())
  calls$medians <- 0L
  db <- make_fake_signal_db(base, idx)
  db$diagram_medians_batch <- function(ids) {
    calls$medians <- calls$medians + 1L
    data.frame(id = integer(), diagram = integer(),
               laas_median = as.Date(character()))
  }

  shiny::testServer(
    mod_signal_review_server,
    args = list(db = db, arrow_available = function() FALSE),
    {
      session$setInputs(parquet_dir = base, window_mode = "all", window_n = 36,
        f_overafdeling = character(), f_afsnit = character(),
        f_datapakke = character(), f_datasaet = character(),
        f_indikator_navn = character(), scan = 1)
      expect_equal(availability()$state, "ingen_data")
      expect_false(scan_running())
      expect_equal(calls$medians, 0L)
      expect_null(scanned_list())
    }
  )
})
```

- [ ] **Step 3: Run the focused tests and verify the missing interfaces**

Run:

```bash
Rscript -e 'devtools::test(filter = "parquet|mod-signal-review")'
```

Expected: FAIL because `signal_data_capability`, the `arrow_available` server argument, and `availability()` do not exist.

- [ ] **Step 4: Implement shallow capability discovery**

Add to `R/fct_parquet.R`:

```r
#' Find kandidat-indikatormapper direkte eller ét gruppeniveau nede.
#' @noRd
parquet_indicator_dirs <- function(base_path) {
  if (length(base_path) != 1L || is.na(base_path) || !dir.exists(base_path)) {
    return(character())
  }
  first <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)
  first <- first[!startsWith(basename(first), "_")]
  second <- unlist(lapply(first, function(path) {
    dirs <- list.dirs(path, recursive = FALSE, full.names = TRUE)
    dirs[!startsWith(basename(dirs), "_")]
  }), use.names = FALSE)
  candidates <- unique(c(first, second))
  candidates[vapply(candidates, parquet_indicator_dir_has_data, logical(1))]
}

#' Har en kandidat data direkte eller i en dato-partition?
#' Scanner højst ét niveau under kandidaten og kalder aldrig Arrow.
#' @noRd
parquet_indicator_dir_has_data <- function(path) {
  if (length(path) != 1L || is.na(path) || !dir.exists(path)) return(FALSE)
  has_parquet <- function(dir) {
    length(list.files(
      dir, pattern = "\\.parquet$", recursive = FALSE,
      full.names = FALSE, ignore.case = TRUE
    )) > 0L
  }
  if (has_parquet(path)) return(TRUE)
  partitions <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  any(vapply(partitions, has_parquet, logical(1)))
}

#' Beskriv om signaldata kan bruges på denne computer.
#' @noRd
signal_data_capability <- function(
    base_path,
    arrow_available = requireNamespace("arrow", quietly = TRUE)) {
  if (length(base_path) != 1L || is.na(base_path) || !nzchar(base_path) ||
      !dir.exists(base_path)) {
    return(list(
      state = "ingen_data",
      message = "Ingen lokal parquet-mappe er valgt. Database-CRUD virker fortsat."
    ))
  }
  dirs <- parquet_indicator_dirs(base_path)
  if (length(dirs) == 0L) {
    return(list(
      state = "ingen_data",
      message = "Mappen indeholder ingen lokale parquet-data. Database-CRUD virker fortsat."
    ))
  }
  if (!isTRUE(arrow_available)) {
    return(list(
      state = "arrow_mangler",
      message = paste(
        "Signal-gennemgang kræver R-pakken 'arrow'.",
        "Database-CRUD virker fortsat."
      )
    ))
  }
  list(state = "klar", message = "Lokale signaldata er klar til scanning.")
}
```

The shallow helper must not recursively enumerate the full parquet store; recursion remains limited to individual indicator folders in `parquet_files_present()` immediately before a read.

- [ ] **Step 5: Wire the capability into the signal module**

In `mod_signal_review_ui()`, add `uiOutput(ns("availability"))` directly below the parquet path input.

Change the server signature and initialize state:

```r
mod_signal_review_server <- function(
    id, db,
    arrow_available = function() requireNamespace("arrow", quietly = TRUE)) {
  moduleServer(id, function(input, output, session) {
    availability <- reactiveVal(signal_data_capability("", arrow_available()))
    # existing initialization follows
```

Add:

```r
observeEvent(input$parquet_dir, {
  availability(signal_data_capability(input$parquet_dir, arrow_available()))
}, ignoreInit = FALSE)

output$availability <- renderUI({
  cap <- availability()
  if (identical(cap$state, "klar")) return(NULL)
  css <- if (identical(cap$state, "arrow_mangler")) {
    "alert alert-warning py-2 small"
  } else {
    "alert alert-info py-2 small"
  }
  div(class = css, cap$message)
})
```

At the top of the scan observer, replace the current path-only guard with:

```r
base <- input$parquet_dir
cap <- signal_data_capability(base, arrow_available())
availability(cap)
if (!identical(cap$state, "klar")) {
  showNotification(cap$message, type = "warning", session = session)
  return()
}
```

Expose `availability = availability` and `scan_progress = scan_progress` in the module's returned test list. Do not change `R/app_server.R`; its existing lazy call remains valid because the new argument has a default.

- [ ] **Step 6: Run the capability and module tests**

Run:

```bash
Rscript -e 'devtools::test(filter = "parquet|mod-signal-review|lazy-init")'
```

Expected: PASS, including the pre-DB scan guard and all existing progressive scan tests.

- [ ] **Step 7: Commit the signal capability model**

```bash
git add R/fct_parquet.R R/mod_signal_review.R tests/testthat/test-parquet.R tests/testthat/test-mod-signal-review.R
git commit -m "feat(signal): expose local data capability without blocking CRUD"
```

---

### Task 3: Isolate corrupt indicators and disable impossible compaction

**Files:**
- Modify: `R/mod_signal_review.R:318-405,579-619`
- Modify: `R/mod_compact.R:28-143,225-226`
- Modify: `tests/testthat/test-mod-signal-review.R`
- Modify: `tests/testthat/test-mod-compact.R`

**Interfaces:**
- Changes: each progressive scan group reads its indicator at most once, including when the read raises an error.
- Extends: signal availability with runtime state `laesefejl` and message containing the number of failed diagrams.
- Changes: `mod_compact_server(id, arrow_available = function() requireNamespace("arrow", quietly = TRUE))`.
- Produces to tests: compact module return value `capability`, a `reactiveVal` holding the last capability result.

- [ ] **Step 1: Add a failing mixed valid/corrupt scan test**

Append a test to `tests/testthat/test-mod-signal-review.R` that creates one valid indicator and one corrupt `.parquet` file:

```r
test_that("korrupt indikator isoleres og rapporteres uden at skjule gyldigt signal", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  dir.create(file.path(base, "god"))
  dir.create(file.path(base, "korrupt"))
  arrow::write_parquet(data.frame(
    dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)),
    taeller = NA_real_, naevner = NA_real_, enhed = "e"
  ), file.path(base, "god", "p.parquet"))
  writeBin(charToRaw("ikke parquet"),
           file.path(base, "korrupt", "p.parquet"))

  idx <- data.frame(
    diagram_id = c(1L, 2L), indikator_id = c(1L, 2L),
    indikator_navn = c("God", "Korrupt"),
    indikator_navn_teknisk = c("god", "korrupt"),
    datasaet = "d", datapakke = "p", org_id = 5L,
    org_teknisk = "E", org_navn = "E", org_niveau = 5L,
    overafdeling = "OA", afdeling = NA, afsnit = NA,
    stringsAsFactors = FALSE
  )
  db <- make_fake_signal_db(base, idx)

  shiny::testServer(mod_signal_review_server, args = list(db = db), {
    session$setInputs(parquet_dir = base, window_mode = "all", window_n = 36,
      f_overafdeling = character(), f_afsnit = character(),
      f_datapakke = character(), f_datasaet = character(),
      f_indikator_navn = character(), scan = 1)
    drain_scan()

    expect_equal(signal_list()$diagram_id, 1L)
    expect_equal(scan_progress()$fejl, 1L)
    expect_equal(availability()$state, "laesefejl")
    expect_match(availability()$message, "1 diagram")
    expect_match(as.character(output$scan_summary$html), "læsefejl")
  })
})
```

- [ ] **Step 2: Add a failing no-Arrow startup-compaction test**

Append to `tests/testthat/test-mod-compact.R`:

```r
test_that("startup med kendt lager men uden Arrow tilbyder ikke kompaktering", {
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  base <- withr::local_tempdir()
  part <- file.path(base, "ind", "dato=2026-01-01")
  dir.create(part, recursive = TRUE)
  writeBin(charToRaw("x"), file.path(part, "part-0.parquet"))
  last_parquet_dir_write(base)
  q <- .with_queue()

  shiny::testServer(
    mod_compact_server,
    args = list(arrow_available = function() FALSE),
    {
      .run_queued(q)
      expect_false(asked())
      expect_false(sweeping())
      expect_equal(capability()$state, "arrow_mangler")
    }
  )
})
```

- [ ] **Step 3: Run focused tests and verify the runtime-state failures**

Run:

```bash
Rscript -e 'devtools::test(filter = "mod-signal-review|mod-compact")'
```

Expected: FAIL because read failures do not yet update `availability`, scan summary still says generic `fejlede`, and compact server lacks injected availability state.

- [ ] **Step 4: Memoize per-indicator read errors and report them**

In `.scan_process_group()`, initialize `slice_env$error <- NULL`. Replace the first-load body of `get_slice()` with:

```r
if (!slice_env$loaded) {
  src <- parquet_indicator_path(ctx$base, ind)
  fp <- source_fingerprint(src)
  slice_env$val <- tryCatch(
    load_indicator_slice_cached(
      ctx$base, ind,
      loader = function() parquet_load_indicator_best(
        ctx$base, ind,
        force = ctx$force,
        manifest = ctx$manifest, src = src, fp = fp
      ),
      force = ctx$force, key = fp
    ),
    error = function(e) {
      slice_env$error <- e
      NULL
    }
  )
  slice_env$loaded <- TRUE
}
if (!is.null(slice_env$error)) stop(slice_env$error)
slice_env$val
```

After updating `scan_progress`, derive runtime availability:

```r
p <- scan_progress()
if ((p$fejl %||% 0L) > 0L) {
  availability(list(
    state = "laesefejl",
    message = sprintf(
      "%d diagram%s kunne ikke læses; øvrige resultater kan stadig gennemgås.",
      p$fejl, if (identical(p$fejl, 1L)) "" else "mer"
    )
  ))
}
```

Change scan-summary wording from `fejlede` to `med læsefejl`. At the beginning of every new scan, reset availability to the fresh `klar` preflight result so an old error does not persist into a successful rescan.

- [ ] **Step 5: Guard eager compaction with the same capability**

Change the compact module signature and state:

```r
mod_compact_server <- function(
    id,
    arrow_available = function() requireNamespace("arrow", quietly = TRUE)) {
  moduleServer(id, function(input, output, session) {
    capability <- reactiveVal(signal_data_capability("", arrow_available()))
    # existing state follows
```

At the start of `.start_sweep(base, manual = FALSE)`:

```r
cap <- signal_data_capability(base, arrow_available())
capability(cap)
if (!identical(cap$state, "klar")) {
  if (isTRUE(manual)) {
    showNotification(cap$message, type = "warning", session = session)
  }
  return(invisible())
}
```

Expose `capability = capability` in the returned test list. Keep startup silent when Arrow is missing; the signal tab supplies the persistent explanatory UI.

- [ ] **Step 6: Run focused scan, compact, and lazy-start tests**

Run:

```bash
Rscript -e 'devtools::test(filter = "mod-signal-review|mod-compact|compact|lazy-init")'
```

Expected: PASS. A corrupt indicator produces `laesefejl`, valid indicators remain usable, and startup compaction remains idle without Arrow.

- [ ] **Step 7: Commit runtime isolation**

```bash
git add R/mod_signal_review.R R/mod_compact.R tests/testthat/test-mod-signal-review.R tests/testthat/test-mod-compact.R
git commit -m "fix(signal): isolate parquet read failures per indicator"
```

---

### Task 4: Make installed DB configuration explicit and secret-free

**Files:**
- Modify: `R/fct_db.R:1-6`
- Modify: `tests/testthat/test-db-guard.R`
- Create: `inst/db-config.yml`
- Modify: `.Rbuildignore`
- Modify: `Renviron.example`

**Interfaces:**
- Produces: `db_config_path(path = NULL)` with precedence: explicit argument, `BFHMETA_DB_CONFIG`, development-root `config.yml`, packaged `app_sys("db-config.yml")`.
- Changes: `db_config(path = NULL)` validates `host`, `port`, `dbname`, `user`, and `sslmode`; no caller change required.
- Security contract: packaged config has no `password`, API key, service-role key, or anon key.

- [ ] **Step 1: Add failing path, package, and validation tests**

Append to `tests/testthat/test-db-guard.R`:

```r
test_that("pakket DB-konfiguration virker uden udviklingsrod", {
  cfg <- withr::with_dir(withr::local_tempdir(), {
    db_config(app_sys("db-config.yml"))
  })
  expect_named(cfg, c("host", "port", "dbname", "user", "sslmode"),
               ignore.order = TRUE)
  expect_false("password" %in% names(cfg))
  expect_true(nzchar(cfg$host))
  expect_true(nzchar(cfg$user))
})

test_that("BFHMETA_DB_CONFIG vælger eksplicit fil", {
  p <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(
    "default:", "  supabase:", "    host: localhost",
    "    port: 5432", "    dbname: postgres", "    user: tester",
    "    sslmode: require"
  ), p)
  withr::with_envvar(c(BFHMETA_DB_CONFIG = p), {
    expect_identical(db_config()$user, "tester")
  })
})

test_that("manglende eller ufuldstændig DB-konfiguration fejler lukket", {
  expect_error(db_config(file.path(tempdir(), "findes-ikke.yml")),
               "DB-konfigurationen mangler")
  p <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("default:", "  supabase:", "    host: localhost"), p)
  expect_error(db_config(p), "DB-konfigurationen er ufuldstændig")
})
```

- [ ] **Step 2: Run the config tests and verify the packaged-path failure**

Run:

```bash
Rscript -e 'devtools::test(filter = "db-guard")'
```

Expected: FAIL because `db_config()` has no path argument or validation and `inst/db-config.yml` does not exist.

- [ ] **Step 3: Implement deterministic config resolution and validation**

Replace the current `db_config()` prelude in `R/fct_db.R` with:

```r
#' Find Supabase-DB-konfiguration uden at pakke hemmeligheder.
#' @noRd
db_config_path <- function(path = NULL) {
  if (!is.null(path)) return(path)
  explicit <- Sys.getenv("BFHMETA_DB_CONFIG")
  if (nzchar(explicit)) return(explicit)
  if (file.exists("config.yml")) return("config.yml")
  app_sys("db-config.yml")
}

#' Læs og validér Supabase-DB-konfiguration.
#' @noRd
db_config <- function(path = NULL) {
  path <- db_config_path(path)
  if (length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("DB-konfigurationen mangler i installationen", call. = FALSE)
  }
  cfg <- yaml::read_yaml(path)$default$supabase
  required <- c("host", "port", "dbname", "user", "sslmode")
  if (!is.list(cfg) || !all(required %in% names(cfg)) ||
      any(vapply(cfg[required], function(x) length(x) != 1L || is.na(x),
                 logical(1)))) {
    stop("DB-konfigurationen er ufuldstændig", call. = FALSE)
  }
  cfg[required]
}
```

Keep `db_connect()` unchanged; it still reads only `SUPABASE_DB_PASSWORD` for the secret.

- [ ] **Step 4: Add the packaged non-secret configuration**

Create `inst/db-config.yml` by copying only `default.supabase` from the tracked root `config.yml`. The file must have this exact shape and no additional keys:

```yaml
default:
  supabase:
    host: "aws-0-eu-west-1.pooler.supabase.com"
    port: 5432
    dbname: "postgres"
    user: "postgres.ijgwlqpbjcfffdmxeahh"
    sslmode: "require"
```

These are the existing tracked non-secret values from `config.yml`. Before staging, run this assertion so any secret-bearing key fails the task:

```bash
Rscript -e 'x <- yaml::read_yaml("inst/db-config.yml")$default$supabase; stopifnot(identical(sort(names(x)), sort(c("host","port","dbname","user","sslmode"))))'
```

Add `^config\.yml$` to `.Rbuildignore`. Add this commented option to `Renviron.example` without a real path:

```text
# Valgfri: brug en anden ikke-hemmelig DB-config end den pakkede standard.
BFHMETA_DB_CONFIG=
```

- [ ] **Step 5: Run config and package-resource tests**

Run:

```bash
Rscript -e 'devtools::test(filter = "db-guard")'
Rscript -e 'x <- yaml::read_yaml("inst/db-config.yml")$default$supabase; stopifnot(identical(sort(names(x)), sort(c("host","port","dbname","user","sslmode"))))'
```

Expected: PASS; packaged configuration contains exactly five non-secret fields.

- [ ] **Step 6: Commit installed configuration support**

```bash
git add R/fct_db.R tests/testthat/test-db-guard.R inst/db-config.yml .Rbuildignore Renviron.example
git commit -m "fix(config): package non-secret Supabase connection settings"
```

---

### Task 5: Verify the complete delivery and document the behavior

**Files:**
- Modify: `NEWS.md`
- Verify: all files changed in Tasks 1-4

**Interfaces:**
- Consumes: all Task 1-4 contracts.
- Produces: a release-note entry and recorded focused/full/package verification evidence.

- [ ] **Step 1: Add the release note**

Add under `## Nye features` in `NEWS.md`:

```markdown
* Appen kan nu installeres og bruges til database-CRUD uden R-pakken Arrow og
  uden lokale parquet-data. Signal-gennemgangen viser særskilt, om data mangler,
  Arrow mangler, eller enkelte diagrammer har læsefejl; disse tilstande påvirker
  ikke excelR-redigering eller andre faner. Startup-kompaktering tilbyder ikke
  en operation, som maskinen mangler Arrow til.
```

- [ ] **Step 2: Run the focused delivery suite**

Run:

```bash
Rscript -e 'devtools::test(filter = "parquet|scan|mod-signal-review|compact|mod-compact|db-guard|lazy-init")'
```

Expected: 0 FAIL and 0 WARN. Record PASS and SKIP counts in the implementation handoff.

- [ ] **Step 3: Run the full regression suite**

Run:

```bash
Rscript -e 'devtools::test()'
```

Expected: 0 FAIL and 0 WARN. DB tests may remain skipped when `BFHMETA_WRITE != 1`; report them separately rather than calling them passed.

- [ ] **Step 4: Build the source package and verify package contents**

Run:

```bash
R CMD build .
tar -tf BFHmetadata_0.7.0.9000.tar.gz | rg '(^|/)config\.yml$|db-config\.yml$'
```

Expected: the archive contains `BFHmetadata/inst/db-config.yml` and does not contain root `BFHmetadata/config.yml`.

- [ ] **Step 5: Run R CMD check**

Run:

```bash
R CMD check --no-manual BFHmetadata_0.7.0.9000.tar.gz
```

Expected: Status OK. If a test skips because Arrow is intentionally absent, verify that all non-Arrow CRUD and capability tests still execute.

- [ ] **Step 6: Perform the manual Shiny smoke test**

On a machine with valid `SUPABASE_DB_PASSWORD` but no configured parquet root:

1. Start the packaged app and open Indikatorer.
2. Confirm the excelR grid loads and an allowed cell can be edited and reloaded.
3. Open Signal-gennemgang and confirm the persistent "Database-CRUD virker fortsat" message.
4. Enter an empty existing directory and confirm scan stays stopped with no session error.
5. Return to Indikatorer and confirm the grid and DB write path still work in the same session.

Expected: all five checks pass; no Arrow namespace error appears before a real parquet read is requested.

- [ ] **Step 7: Commit release documentation**

```bash
git add NEWS.md
git commit -m "docs(news): document CRUD without local signal data"
```

- [ ] **Step 8: Record final branch state**

Run:

```bash
git status --short --branch
git log --oneline origin/main..HEAD
```

Expected: clean worktree on `feat/origin-main-hardening`; the branch contains the approved spec, this plan, and the five delivery commits from Tasks 1-5.
