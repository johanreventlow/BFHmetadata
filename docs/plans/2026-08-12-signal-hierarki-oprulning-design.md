# Design: Hierarki-oprulning i signal-gennemgang

**Dato:** 2026-08-12
**Status:** Godkendt (tilgang A)
**Berører:** `R/fct_scan.R`, `R/fct_sql.R`, `R/fct_db.R`, `R/mod_signal_review.R`, ny `R/fct_aggregate.R`, tests

## Baggrund

Signal-gennemgangen kan i praksis kun vurdere diagrammer hvis org-enhed
findes direkte i parquet-data (typisk afsnit). Diagrammer på
overafdelings-/hospitalsniveau får deres data i BFHddl via
**hierarki-oprulning** (aggregering af underliggende enheder) — den findes
ikke i appen, så disse diagrammer rammer "ingen_data" og kan ikke
gennemgås.

## Fakta fra udforskning (BFHddl)

- Oprulning er et **læse-tids-fallback** i BFHddl (`pipeline.R` trin
  5.1b): aktiveres KUN når direkte enhed-match giver 0 rækker OG org'en
  har kvalificerende børn. Direkte match vinder altid.
- Barn-udvælgelse: `find_aggregation_children()`
  (`BFHddl/R/db_organisations.R:220`) — strukturelle børn via
  `tblOrganisationStruktur.parent_Id`, hvor barnets diagram-række for
  samme indikator har `indgaar_i_aggregering = TRUE`. Mellemniveauer uden
  diagram-række traverseres ("gennemfald"); en række med FALSE/tomt flag
  ekskluderer hele grenen. `db_get_diagrams(active_only = FALSE)` —
  flagene læses UDEN aktiv-filter.
- Summering: `aggregate_child_data()` (`BFHddl/R/data_loader.R:651`) —
  pr. dato `sum(taeller)`, `sum(naevner)` med **`na.rm = FALSE`**
  (manglende barn-værdi må ikke tavst blive 0). Rate beregnes downstream
  som sum/sum (korrekt vægtning).
- **`vaerdi`-only indikatorer kan ikke oprulles** (sum-af-medianer er
  statistisk forkert) — DATA_CONVENTIONS §5 forbyder flag på disse.
- **Parquet må ALDRIG indeholde aggregerede niveauer** (Model B,
  DATA_CONVENTIONS §6 "never mix") — det udelukker præ-aggregering i
  kompaktering/BFHddl-output.
- Rækkefølge: oprulning FØR periode-aggregering (§5.1 → §5.2).
- Footgun: `tblDiagrammer."organisatorisk_navn_teknisk"` og `."indikator"`
  er **heltals-FK'er** trods navnene.

## Beslutninger (afklaret med bruger)

1. **Placering: tilgang A** — oprulning som fallback i BFHmetadatas
   scan-sti, in-memory på det allerede indlæste indikator-slice (som
   indeholder ALLE enheder). Ingen ændringer i BFHddl eller kompaktering.
   - Fravalgt B (præ-aggregering i parquet/kompaktering): forbudt af
     DATA_CONVENTIONS §6 — blandede niveauer gør direkte match
     tavst-forkert.
   - Fravalgt C (DB-side): måledata ligger i parquet, ikke i Supabase.
2. **Genbrug: vendoring** — de rene BFHddl-funktioner
   (`find_aggregation_children` + hjælper, `aggregate_child_data`)
   kopieres til ny `R/fct_aggregate.R` med "Vendored fra BFHddl"-header
   (samme mønster som `resolve_median_breaks` i `fct_signal.R`).
   Drift-risiko håndteres med kontrakt-tests der pinner den dokumenterede
   semantik. Fravalgt `Imports: BFHddl`: pakken er ikke installeret og
   trækker tunge deps (dm, blastula); kobler admin-app til
   pipeline-versioner.

## Design

### 1. Trigger & dataflow (scan_diagram)

Nuværende: `slice_filter_enhed(full, variants)` → NULL → "ingen_data".
Nyt fallback (spejler BFHddl D3):

1. Direkte match = 0 rækker → find bidragende enheder via vendored
   `find_aggregation_children(center_org_id, indikator, org_struct,
   agg_flags)` (gennemfald + gren-eksklusion som BFHddl).
2. Ingen bidragende enheder → "ingen_data" (som i dag).
3. Ellers: adapter `aggregate_slice_for_center()` filtrerer det
   in-memory slice pr. bidragende enhed (via `org_enhed_variants`) og
   summerer med vendored `aggregate_child_data` (na.rm = FALSE).
4. `vaerdi`-only slices (uden taeller/naevner) oprulles IKKE →
   "ingen_data".
5. Oprulning sker FØR periode-aggregering og vindues-begrænsning
   (BFHddl-rækkefølgen §5.1 → §5.2a → §5.2b).
6. Scan-resultatet stemples `aggregated = TRUE` + `n_agg_units`.

### 2. Nye DB-accessors (én hentning pr. scan)

- `org_struct()`: `SELECT "Id", "parent_Id" FROM "tblOrganisationStruktur"`
- `aggregation_flags()`: pr. diagram-række (org-id, indikator-id,
  `indgaar_i_aggregering`) fra `tblDiagrammer` — UDEN aktiv-filter
  (spejler BFHddl's `active_only = FALSE`).

Hentes i scan-start (`scan_ctx`) med `safe_operation`-fallback NULL →
oprulning degraderer til "ingen_data" (aldrig scan-crash).

### 3. Vendored funktioner (`R/fct_aggregate.R`)

- `find_aggregation_children()` + `.has_flagged_descendant()` — port fra
  `BFHddl/R/db_organisations.R` tilpasset df-input fra accessors ovenfor.
- `aggregate_child_data()` — port fra `BFHddl/R/data_loader.R:651`.
- `aggregate_slice_for_center()` — BFHmetadata-specifik adapter:
  erstatter BFHddl's fil-loader med slice-filtrering
  (`slice_filter_enhed` pr. bidragende enhed), samler og summerer.
- Kontrakt-tests pinner: na.rm=FALSE-semantik, gennemfald,
  gren-eksklusion ved FALSE, ikke-overlappende datoer (sum over
  tilstedeværende børn), vaerdi-only → NULL, 0 børn → NULL.

### 4. UI-transparens

- Badge ved grafen: "Aggregeret fra N enheder" når `aggregated`.
- Signal-beregning, faseskift, median-knæk uændrede (diagram_id-baserede).
- BFHddl-caveat "aggregeringsbærer uden flaggede efterkommere → tavst 0"
  bliver hos os til "ingen_data", som allerede tælles synligt i
  scan-opsummeringen.

### 5. Bagudkompatibilitet

- `scan_diagram()` får org_struct/flags som valgfrie parametre
  (NULL-default = ingen oprulning) → eksisterende tests/kald uændrede.
- Test-helperen `make_fake_signal_db` udvides med de to nye accessors
  (tomme defaults).

### 6. Tests

- Unit: vendored semantik (se §3), adapter-kanttilfælde.
- SQL: nye builder-funktioner (test-sql.R-mønster).
- testServer: aggregat-diagram scannes og kan få signal; ingen flagede
  børn → "ingen_data"; direkte match forbigår oprulning; vaerdi-only →
  "ingen_data"; badge renderes.
- DB-integration (skippes uden BFHMETA_WRITE): accessors returnerer
  forventede kolonner.

## Konsekvenser

- Appen vurderer samme serie som BFHddl tegner i PDF'erne for
  aggregat-diagrammer.
- Vendoring kræver disciplin: ændres oprulningen i BFHddl, skal
  `fct_aggregate.R` følge med (note tilføjes i header + BFHddl-relaterede
  kontrakt-tests fejler ved semantik-drift i egne funktioner, ikke i
  BFHddl's).
- Ingen breaking changes; ingen NAMESPACE-ændringer (@noRd).
