# Server-side Indicator Search Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove Shiny's large-options warning by making the unlocked diagram indicator field server-side searchable across all indicators, including inactive ones.

**Architecture:** Keep the shared diagram form responsible only for rendering a small initial indicator choice set. The diagram server initializes the unlocked field after the modal is mounted with `updateSelectizeInput(..., server = TRUE)` and the complete cached option set; the indicator module's locked diagram flow retains only its current hidden value and display label.

**Tech Stack:** R, Shiny, Selectize, testthat

## Global Constraints

- All indicators remain searchable, including inactive indicators.
- Editing preserves the current indicator; creation starts at `(vælg)`.
- The locked indicator flow remains locked and submits its current id.
- Database accessors, validation, duplicate checks, and persisted value formats do not change.
- Existing unknown indicator ids are preserved rather than cleared when a form opens.

---

### Task 1: Small initial indicator control

**Files:**
- Modify: `R/mod_diagram.R:9-43`
- Test: `tests/testthat/test-mod-diagram.R`

**Interfaces:**
- Consumes: `opts$indikator`, a data frame with integer `id` and character `label`; `vals$indikator`, an optional current id.
- Produces: `.diagram_indicator_initial_choices(indicators, selected)`, a named character vector containing `(vælg) = ""` and at most the selected indicator; `.diagram_form_ui()` output whose indicator HTML remains bounded independently of option count.

- [x] **Step 1: Write failing tests for bounded HTML and value preservation**

Add tests that construct 2,000 indicators, capture warnings from `.diagram_form_ui()`, and assert that `Indikator 2000` is not embedded for a new form. Add edit cases asserting that a known selected id/label and an unknown selected id with fallback label `Ukendt indikator #<id>` remain present. Assert that `lock_indikator = TRUE` still contains the selected hidden value and disabled display input.

- [x] **Step 2: Run the focused UI tests and verify RED**

Run:

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "devtools::load_all(quiet=TRUE); testthat::test_file('tests/testthat/test-mod-diagram.R', reporter='summary')"
```

Expected: FAIL because the large list is embedded, the warning is emitted, and no unknown-id fallback exists.

- [x] **Step 3: Implement the bounded initial choices**

Add `.diagram_indicator_initial_choices()` to normalize `selected`, locate its label, and return only the empty and current options. Change the indicator control to `selectizeInput()` using that result. For the locked flow, render only the selected hidden option plus the existing disabled text display; do not pass the full list into either control.

- [x] **Step 4: Run the focused tests and verify GREEN**

Run the Step 2 command. Expected: all `test-mod-diagram.R` assertions pass and the large fixture emits no large-options warning.

- [x] **Step 5: Commit Task 1**

```powershell
git add R/mod_diagram.R tests/testthat/test-mod-diagram.R
git commit -m "fix(diagram): bound initial indicator choices"
```

### Task 2: Server-side option registration

**Files:**
- Modify: `R/mod_diagram.R:86-194`
- Test: `tests/testthat/test-mod-diagram.R`

**Interfaces:**
- Consumes: `.diagram_indicator_choices(opts$indikator)`, returning named values for every row without active-status filtering.
- Produces: `.update_diagram_indicator(session, indicators, selected)`, which calls `updateSelectizeInput(session, "d_indikator", choices = ..., selected = ..., server = TRUE)` after the modal has been sent.

- [x] **Step 1: Write failing server-initialization tests**

Use a test-local binding around `updateSelectizeInput` to record `inputId`, `choices`, `selected`, and `server`. Open a new diagram and an existing diagram through `testServer()`. Assert `server` is `TRUE`, `inputId` is `d_indikator`, all fixture choices including a label explicitly named `Inaktiv indikator` are supplied, new selection is `""`, and edit selection is the existing indicator id.

- [x] **Step 2: Run the focused server tests and verify RED**

Run the same focused command from Task 1. Expected: FAIL because no server-side update is currently issued.

- [x] **Step 3: Implement post-modal server initialization**

Add `.update_diagram_indicator()` and call it from `.show_form_modal()` via `session$onFlushed(..., once = TRUE)` after `showModal()`. Pass every row returned by `diagram_form_options()` without examining or filtering active status. Keep `.open_diagram_modal()` in `mod_indikator_crud.R` unchanged because it calls `.diagram_form_ui(..., lock_indikator = TRUE)`.

- [x] **Step 4: Run diagram and indicator-modal regressions**

Run:

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "devtools::load_all(quiet=TRUE); testthat::test_file('tests/testthat/test-mod-diagram.R', reporter='summary'); testthat::test_file('tests/testthat/test-mod-crud.R', reporter='summary')"
```

Expected: both files pass, including locked indicator create/update tests.

- [x] **Step 5: Commit Task 2**

```powershell
git add R/mod_diagram.R tests/testthat/test-mod-diagram.R
git commit -m "fix(diagram): search indicators server-side"
```

### Task 3: Browser and package verification

**Files:**
- Modify only if a verified regression is found: `R/mod_diagram.R`, `tests/testthat/test-mod-diagram.R`, or `tests/testthat/test-mod-crud.R`

**Interfaces:**
- Consumes: completed Tasks 1 and 2.
- Produces: browser evidence and a clean branch ready for integration.

- [x] **Step 1: Reproduce the original large-list scenario in a real browser**

Run the app with the worktree code and main checkout's `.Renviron`, open the Diagram tab and both new/edit modals through Chromote, and capture R warnings plus browser console errors. Confirm the `diagram-d_indikator` element exists, its initial DOM contains at most the empty/current choice, typing finds a late-list indicator, and no large-options warning occurs.

- [x] **Step 2: Run the relevant regression suite**

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "devtools::load_all(quiet=TRUE); testthat::test_dir('tests/testthat', filter='diagram|mod-crud|app-ui', reporter='summary')"
git diff --check
```

Expected: zero failures; only documented environment warnings/skips are acceptable.

- [x] **Step 3: Run the full package suite**

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "devtools::test(reporter='summary')"
```

Expected: exit code 0, with write-gated DB integration tests skipped when `BFHMETA_WRITE != 1`.

- [x] **Step 4: Commit any verification-driven adjustment**

If Step 1 exposed a real regression, first add a failing automated test, implement the minimal correction, rerun Steps 1-3, and commit only those files. If no adjustment was needed, do not create an empty commit.
