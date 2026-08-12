# Organisation Inline Editing Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace modal-based editing of existing organisation nodes with permanently visible DT text inputs and dropdowns that validate and save each changed field immediately.

**Architecture:** Keep `mod_hierarchy` as the Shiny orchestration boundary and keep the existing complete-row `db$update_node(id, values)` interface. Add pure helpers in a focused file for editor markup, event normalization, field mapping, and validation; the server turns each browser event into a validated complete-row update, reloads authoritative database state after both success and failure, and retains dialogs for create and delete.

**Tech Stack:** R 4.6, Shiny, DT/DataTables, htmltools, testthat, existing DB/cache closures

## Global Constraints

- Continue using `DT`; do not add `rhandsontable` or another table dependency.
- All five fields are permanently visible editors: technical name, long name, short name, parent, and level.
- Text saves on Enter or blur; Escape restores the saved value without writing; dropdowns save on change.
- Every accepted change is persisted immediately through `db$update_node(id, values)`.
- Database state is authoritative; reload on success and on validation/database failure.
- Parent choices and server validation must prevent self-parenting and descendant-parenting cycles.
- Keep the existing create dialog and use a separate confirmation dialog for deleting the selected row.
- HTML-escape all user/database values inserted into editor markup and whitelist incoming field names server-side.
- Preserve the existing soft warning when a node's level is not deeper than its parent's level.

---

## File Structure

- Create `R/fct_hierarchy_editor.R`: pure field mapping, value normalization, update preparation, escaped HTML editor builders, and the delegated DT JavaScript callback.
- Create `tests/testthat/helper-hierarchy.R`: shared mutable hierarchy fake DB used by pure and server tests.
- Create `tests/testthat/test-hierarchy-editor.R`: focused unit tests for the new pure helpers and generated markup.
- Modify `R/mod_hierarchy.R`: render the five editor columns, handle `inline_edit` events, retain create flow, and replace per-row open/edit with selected-row delete confirmation.
- Modify `tests/testthat/test-mod-hierarchy.R`: use the shared fake DB and add Shiny server tests for save, rollback, warnings, create, and delete.
- Modify `tests/testthat/test-app-ui.R`: assert the organisation UI exposes the two required action buttons and no longer describes modal editing.

### Task 1: Pure inline-editor model and safe HTML

**Files:**
- Create: `R/fct_hierarchy_editor.R`
- Create: `tests/testthat/helper-hierarchy.R`
- Create: `tests/testthat/test-hierarchy-editor.R`
- Modify: `tests/testthat/test-mod-hierarchy.R`

**Interfaces:**
- Consumes: `hierarchy_edit_cols(cfg)` and `hierarchy_descendants(df, pk, parent_col, id)`.
- Produces: `.hierarchy_inline_fields(cfg) -> named character`, `.hierarchy_row_values(row, cfg) -> named list`, `.prepare_hierarchy_inline_update(nodes, niveauer, cfg, event) -> list`, `.hierarchy_text_editor_html(ns, id, field, value, depth = 0L) -> character`, `.hierarchy_select_editor_html(ns, id, field, current, choices, root = FALSE) -> character`, and `.hierarchy_dt_callback(ns) -> htmlwidgets::JS`.

- [ ] **Step 1: Move the shared hierarchy fixture into a helper file**

Move `.hierarchy_cfg()` and `fake_hierarchy_db()` unchanged from
`test-mod-hierarchy.R` into `helper-hierarchy.R`. This makes the fixture
available before both test files are evaluated. Run the existing module tests
once and confirm the move alone does not change behavior.

- [ ] **Step 2: Write failing field-mapping and normalization tests**

```r
test_that("inline-felter mapper de fem viste felter til lagringsfelter", {
  cfg <- HIERARCHY_TABLES$org_struktur
  expect_identical(.hierarchy_inline_fields(cfg), c(
    teknisk = "organisatorisk_navn_teknisk",
    langt = "organisatorisk_navn_langt",
    kort = "organisatorisk_navn_kort",
    parent = "parent_Id",
    niveau = "organisatorisk_niveau"))
})

test_that("inline-opdatering bygger en komplet vaerdiliste", {
  db <- fake_hierarchy_db()
  result <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "organisatorisk_navn_kort", value = "Nyt"))
  expect_true(result$ok)
  expect_false(result$unchanged)
  expect_identical(result$id, 2L)
  expect_identical(result$values$organisatorisk_navn_kort, "Nyt")
  expect_identical(names(result$values), hierarchy_edit_cols(.hierarchy_cfg()))
})

test_that("tom tekst og tom foraelder normaliseres til korrekt NA-type", {
  db <- fake_hierarchy_db()
  text <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "organisatorisk_navn_kort", value = ""))
  root <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "parent_Id", value = ""))
  expect_true(is.na(text$values$organisatorisk_navn_kort))
  expect_type(text$values$organisatorisk_navn_kort, "character")
  expect_true(is.na(root$values$parent_Id))
  expect_type(root$values$parent_Id, "integer")
})
```

- [ ] **Step 3: Run the focused tests and verify they fail**

Run:

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "devtools::test(filter='hierarchy-editor')"
```

Expected: FAIL because the helper functions do not exist.

- [ ] **Step 4: Implement field mapping, complete-row extraction, coercion, and validation**

Implement `.prepare_hierarchy_inline_update()` to return exactly:

```r
list(
  ok = TRUE,             # FALSE for rejected events
  unchanged = FALSE,     # TRUE when normalized value equals server state
  id = 2L,
  values = values,       # names exactly hierarchy_edit_cols(cfg)
  warning = "",          # soft level warning, otherwise empty
  error = ""             # rejection message, otherwise empty
)
```

Reject unknown fields, unknown node ids, invalid integer ids, an empty
`cfg$display_col`, unknown parent/level ids, and parent ids in
`hierarchy_descendants(nodes, "id", "parent_id_raw", id)`. Compare normalized
values with the authoritative row and return `unchanged = TRUE` without an
update. Build `values` from the row's configured text fields plus
`parent_id_raw`, `niveau_id`, and `aktiv` when configured.

- [ ] **Step 5: Add failing validation tests**

```r
test_that("inline-opdatering afviser ukendt felt og tomt langt navn", {
  db <- fake_hierarchy_db()
  unknown <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "id", value = "99"))
  empty <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "organisatorisk_navn_langt", value = ""))
  expect_false(unknown$ok)
  expect_match(unknown$error, "felt", ignore.case = TRUE)
  expect_false(empty$ok)
  expect_match(empty$error, "obligatorisk", ignore.case = TRUE)
})

test_that("inline-foraelder afviser egen subtree", {
  db <- fake_hierarchy_db()
  self <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 1, field = "parent_Id", value = "1"))
  child <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 1, field = "parent_Id", value = "3"))
  expect_false(self$ok)
  expect_false(child$ok)
  expect_match(child$error, "cyklus|subtree|efterkommer", ignore.case = TRUE)
})
```

- [ ] **Step 6: Run focused tests and make validation pass**

Run the Task 1 command again. Expected: mapping, normalization, unchanged, and validation tests PASS.

- [ ] **Step 7: Add failing escaped-markup and callback tests**

```r
test_that("teksteditor escaper attributter og indeholder stabil routing", {
  html <- .hierarchy_text_editor_html(
    function(x) paste0("org-", x), 7L, "organisatorisk_navn_langt",
    '\"><script>alert(1)</script>', depth = 2L)
  expect_match(html, "data-node-id=\"7\"")
  expect_match(html, "data-field=\"organisatorisk_navn_langt\"")
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
})

test_that("select-editor escaper labels og udelader subtree-valg", {
  choices <- c("Rod <A>" = "", "Barn & B" = "2")
  html <- .hierarchy_select_editor_html(
    function(x) paste0("org-", x), 7L, "parent_Id", "", choices,
    root = TRUE)
  expect_false(grepl("Rod <A>", html, fixed = TRUE))
  expect_match(html, "Rod &lt;A&gt;", fixed = TRUE)
  expect_match(html, "data-saved=\"\"")
})

test_that("DT callback sender change og implementerer Enter Escape blur", {
  js <- as.character(.hierarchy_dt_callback(function(x) paste0("org-", x)))
  expect_match(js, "Shiny.setInputValue", fixed = TRUE)
  expect_match(js, "org-inline_edit", fixed = TRUE)
  expect_match(js, "keydown", fixed = TRUE)
  expect_match(js, "Escape", fixed = TRUE)
  expect_match(js, "blur", fixed = TRUE)
})
```

- [ ] **Step 8: Implement escaped editor builders and delegated JavaScript**

The callback must bind handlers under the table node using an event namespace,
remove old namespaced handlers before rebinding, and route `.hierarchy-editor`
events. On text Enter call `blur()`; on Escape restore `data-saved`, mark a
one-shot cancellation flag, and blur; on blur/change compare current value to
`data-saved`, add class `hierarchy-saving`, and call:

```javascript
Shiny.setInputValue(ns + 'inline_edit', {
  id: Number(editor.dataset.nodeId),
  field: editor.dataset.field,
  oldValue: editor.dataset.saved,
  value: editor.value,
  nonce: Date.now()
}, {priority: 'event'});
```

Use `htmltools::htmlEscape(x, attribute = TRUE)` for attribute values and
`htmltools::htmlEscape(x)` for visible option labels.

- [ ] **Step 9: Run Task 1 tests and commit**

Run the focused test command. Expected: all `test-hierarchy-editor.R` tests PASS.

```powershell
git add R/fct_hierarchy_editor.R tests/testthat/helper-hierarchy.R tests/testthat/test-hierarchy-editor.R tests/testthat/test-mod-hierarchy.R
git commit -m "feat(hierarchy): add safe inline editor helpers"
```

### Task 2: Render permanently visible DT editors

**Files:**
- Modify: `R/mod_hierarchy.R`
- Modify: `tests/testthat/test-mod-hierarchy.R`

**Interfaces:**
- Consumes: all Task 1 helpers, existing `tree()`, `.labels()`, `.parent_choices()`, and `.niveau_choices()`.
- Produces: `output$tbl` with five editor columns, stable row selection, and `input$inline_edit` browser events.

- [ ] **Step 1: Write failing render-helper tests**

Extract table construction into `.hierarchy_editor_data(d, cfg, ns, niveauer)`
and test its contract:

```r
test_that("hierarki-tabel viser fem permanente editor-kolonner", {
  db <- fake_hierarchy_db()
  d <- hierarchy_order(db$list_nodes(), "id", "parent_id_raw",
                       .hierarchy_cfg()$display_col)
  out <- .hierarchy_editor_data(d, .hierarchy_cfg(), identity,
                                db$niveau_options())
  expect_named(out, c("Teknisk navn", "Langt navn", "Kort navn",
                      "Forælder", "Niveau"))
  expect_true(all(grepl("hierarchy-editor", out[["Langt navn"]], fixed = TRUE)))
  expect_match(out[["Langt navn"]][3], "padding-left:3rem", fixed = TRUE)
  expect_false(grepl('value="3"', out[["Forælder"]][1], fixed = TRUE))
})
```

- [ ] **Step 2: Run the focused module tests and verify failure**

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "devtools::test(filter='(hierarchy-editor|mod-hierarchy)')"
```

Expected: FAIL because `.hierarchy_editor_data()` is not defined.

- [ ] **Step 3: Implement editor table construction and DT rendering**

Replace the `Navn`, `Niveau`, and `Åbn` columns with the five editor columns.
For every parent cell, derive choices from the depth-first tree after excluding
the row's own subtree; prepend `"(rod)" = ""`. Derive level choices from
`niveauer`. Render with `selection = "single"`, `escape = FALSE`,
`callback = .hierarchy_dt_callback(session$ns)`, and client-side processing.

Add Bootstrap-compatible scoped CSS in `mod_hierarchy_ui()` for compact editor
height, a subtle editable background, and a disabled/saving state. Apply the
long-name hierarchy indentation as an escaped inline `padding-left` value of
`depth * 1.5rem` on a wrapper around the input. Add a JavaScript
`columnDefs.render` function for all
five columns that returns the embedded input/select value for `sort` and
`filter`, and the original HTML for `display`.

- [ ] **Step 4: Run focused tests and commit**

Run the Task 2 test command. Expected: helper and module tests PASS through table rendering.

```powershell
git add R/mod_hierarchy.R tests/testthat/test-mod-hierarchy.R
git commit -m "feat(hierarchy): render permanent DT editors"
```

### Task 3: Immediate persistence, validation feedback, and rollback

**Files:**
- Modify: `R/mod_hierarchy.R`
- Modify: `tests/testthat/test-mod-hierarchy.R`

**Interfaces:**
- Consumes: `.prepare_hierarchy_inline_update()` and `db$update_node(id, values)`.
- Produces: `observeEvent(input$inline_edit, ...)`, authoritative `nodes()` reloads, `status_msg()`, and `warn_msg()`.

- [ ] **Step 1: Make the fake DB mutable and error-capable**

Change the shared `fake_hierarchy_db()` so `update_node()` records every call in
`calls$updates`, mutates its private `nodes`, and optionally fails when
`.set_update_error(TRUE)` is called. Expose `.nodes()` and `.set_update_error()`
for assertions. Keep existing create/delete call accessors compatible until
their tests are migrated in Task 4.

- [ ] **Step 2: Write failing immediate-save tests**

```r
test_that("inline tekstændring gemmer straks hele noden og genindlaeser", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(inline_edit = list(
        id = 2, field = "organisatorisk_navn_langt",
        oldValue = "Barn B", value = "Barn B ny", nonce = 1))
      upd <- tail(db$.calls()$updates, 1)[[1]]
      expect_identical(upd$id, 2L)
      expect_identical(upd$values$organisatorisk_navn_langt, "Barn B ny")
      expect_identical(nodes()$organisatorisk_navn_langt[nodes()$id == 2L],
                       "Barn B ny")
      expect_match(status_msg(), "Gemt")
    })
})

test_that("inline dropdowns gemmer integer-id og flytning genordner traeet", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(inline_edit = list(
        id = 3, field = "parent_Id", oldValue = "2", value = "4", nonce = 1))
      expect_identical(db$.nodes()$parent_id_raw[db$.nodes()$id == 3L], 4L)
      expect_identical(tree()$id, c(1L, 2L, 4L, 3L))
      session$setInputs(inline_edit = list(
        id = 3, field = "organisatorisk_niveau",
        oldValue = "10", value = "20", nonce = 2))
      expect_identical(db$.nodes()$niveau_id[db$.nodes()$id == 3L], 20L)
    })
})
```

- [ ] **Step 3: Run focused tests and verify failure**

Run the Task 2 command. Expected: FAIL because `input$inline_edit` has no observer.

- [ ] **Step 4: Implement immediate save and authoritative reload**

For every event, call `.prepare_hierarchy_inline_update(nodes(), niveauer(),
cfg, input$inline_edit)`. On rejection, set `warn_msg(result$error)` and call
`reload()`. On `unchanged`, call `reload()` without `db$update_node()`. On an
accepted change, execute:

```r
ok <- safe_operation("hierarki-inline-gem", {
  db$update_node(result$id, result$values)
  TRUE
}, fallback = FALSE)
reload()
if (isTRUE(ok)) {
  status_msg("Gemt")
  if (nzchar(result$warning)) warn_msg(result$warning)
} else {
  warn_msg("Fejl ved gem; værdien er gendannet")
}
```

- [ ] **Step 5: Add failure, unchanged, cycle, required-name, and soft-warning tests**

Use `session$setInputs(inline_edit = ...)` to assert: unknown field and cycle do
not append to `calls$updates`; unchanged values do not write; an empty long name
sets an obligatory-field warning; a level not deeper than its parent writes and
sets `warn_msg`; forced DB failure leaves `nodes()` equal to `db$.nodes()` and
reports that the browser value was restored.

- [ ] **Step 6: Run focused tests and commit**

Run the Task 2 command. Expected: all inline server tests PASS.

```powershell
git add R/mod_hierarchy.R tests/testthat/helper-hierarchy.R tests/testthat/test-mod-hierarchy.R
git commit -m "feat(hierarchy): save inline edits immediately"
```

### Task 4: Preserve create dialog and add confirmed selected-row deletion

**Files:**
- Modify: `R/mod_hierarchy.R`
- Modify: `tests/testthat/test-mod-hierarchy.R`
- Modify: `tests/testthat/test-app-ui.R`

**Interfaces:**
- Consumes: `input$tbl_rows_selected`, current `tree()`, `db$child_count(id)`, and `db$delete_node(id)`.
- Produces: buttons `new_node` and `delete_selected`, confirmation event `confirm_delete`, and reactive `delete_id`.

- [ ] **Step 1: Extend the shared fake DB for deletion outcomes**

Make `delete_node(id)` remove the matching row from its private `nodes` after
recording `calls$deleted`. Add `.set_delete_error(TRUE/FALSE)`; when enabled,
`delete_node()` throws `stop("foreign key constraint")` before mutating rows.
This lets success and rollback tests observe the same authoritative reload used
in production.

- [ ] **Step 2: Write failing UI and deletion-flow tests**

```r
test_that("hierarki-UI har opret og slet men ingen aabn-knap", {
  html <- as.character(mod_hierarchy_ui(
    "org", HIERARCHY_TABLES$org_struktur))
  expect_match(html, "org-new_node", fixed = TRUE)
  expect_match(html, "org-delete_selected", fixed = TRUE)
  expect_false(grepl("open_id", html, fixed = TRUE))
})

test_that("slet valgt kraever valg og bekraeftelse", {
  db <- fake_hierarchy_db()
  testServer(mod_hierarchy_server,
    args = list(db = db, cfg = .hierarchy_cfg()), {
      session$setInputs(delete_selected = 1)
      expect_match(warn_msg(), "Vælg", fixed = TRUE)
      session$setInputs(tbl_rows_selected = 4, delete_selected = 2)
      expect_identical(delete_id(), 4L)
      expect_null(db$.calls()$deleted)
      session$setInputs(confirm_delete = 1)
      expect_identical(db$.calls()$deleted, 4L)
    })
})
```

- [ ] **Step 3: Run focused tests and verify failure**

Run the Task 2 command. Expected: FAIL because the delete-selection controls and state do not exist.

- [ ] **Step 4: Replace existing-node modal editing with selected-row deletion**

Render `Ny node` and `Slet valgt` buttons above the table. Remove the `Åbn`
button, `open_id` observer, existing-node save path, and delete button from the
form modal. Keep `.hierarchy_form_ui()`, `.collect_hierarchy_form()`, and the
`new_node`/`h_save` create path.

On `delete_selected`, resolve the selected row against current `tree()` and set
`delete_id`; show a modal naming the node with `Annullér` and `Slet` buttons.
On `confirm_delete`, re-check the node exists, reject nodes with children,
attempt `db$delete_node(delete_id())`, reload after success or failure, clear
selection state, and preserve the current friendly reference-error message.

- [ ] **Step 5: Update legacy module tests**

Remove tests coupled to `open_id`, `editing_id`, and editing existing nodes via
`h_save`. Retain the create test and convert delete tests to row selection plus
confirmation. Add a test that deletion of a parent is blocked and one that
calls `.set_delete_error(TRUE)`, reports "i brug", and leaves authoritative
rows unchanged.

- [ ] **Step 6: Run hierarchy and UI tests, then commit**

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "devtools::test(filter='(hierarchy|app-ui)')"
```

Expected: all hierarchy helper, module, SQL/DB unit tests that do not require credentials, and app UI tests PASS; credentialed DB tests SKIP unless explicitly enabled.

```powershell
git add R/mod_hierarchy.R tests/testthat/helper-hierarchy.R tests/testthat/test-mod-hierarchy.R tests/testthat/test-app-ui.R
git commit -m "feat(hierarchy): add confirmed selected-row deletion"
```

### Task 5: Full regression verification

**Files:**
- Modify only if a regression reveals a defect in files already listed above.

**Interfaces:**
- Consumes: completed Tasks 1-4.
- Produces: a verified implementation with no new package dependency and a clean worktree.

- [ ] **Step 1: Run static checks**

```powershell
git diff --check main...HEAD
rg -n "rhandsontable|open_id|Åbn &rsaquo;" R tests DESCRIPTION
```

Expected: `git diff --check` exits 0; no `rhandsontable`; no hierarchy `open_id` or open-button remnants.

- [ ] **Step 2: Run the complete automated test suite**

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "devtools::test()"
```

Expected: all non-credentialed tests PASS and write-enabled Supabase integration tests SKIP unless `BFHMETA_WRITE=1` and credentials are intentionally provided.

- [ ] **Step 3: Run package check**

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "devtools::check(error_on='warning')"
```

Expected: 0 errors and 0 warnings. Existing notes may be documented if they are unchanged from `main`.

- [ ] **Step 4: Inspect final scope and commit any verification-only fix**

```powershell
git status --short
git diff --stat main...HEAD
git log --oneline main..HEAD
```

Expected: only the planned helper/module/test files plus the approved design and plan commits; worktree clean. If verification reveals a defect, return to the relevant earlier task, add a failing regression test, implement the smallest correction, commit it with a message naming the observed defect, and rerun Steps 1-3.
