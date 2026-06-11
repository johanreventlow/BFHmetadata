# Design: Signal-gennemgang (faseskift-workflow)

**Dato:** 2026-06-11
**Status:** Godkendt (afventer spec-review)
**App:** BFHmetadata (Shiny, Golem) — kører lokalt + cloud, mod Supabase

## Formål
Effektiv gennemgang af seriediagrammer (run charts) for procesændrings-signaler
(Anhøj-reglerne). Brugeren peger app'en på en lokal parquet-mappe; app'en bygger
hvert aktivt Seriediagram efter `tblDiagrammer` + eksisterende median-knæk,
vurderer Anhøj-signal på seneste fase, viser diagrammer med signal til
gennemgang, og lader brugeren registrere et faseskift direkte i
`tblDiagrammerMedian` ved at klikke en observation i en interaktiv graf.

## Kontekst & beslutninger (afklaret med bruger)
| Emne | Valg |
|------|------|
| DB | **Supabase** (dette projekt; resten af økosystemet er stadig MS Access midt i transition) |
| Parquet/chart-arg-logik | **Vendored** minimal logik i BFHmetadata (ej `Imports: BFHddl` — Access-koblet, i transition) |
| Interaktiv graf | **Byg ggiraph fra `bfh_qic()$qic_data`** (BFHcharts laver SPC-matematik; vi tegner) |
| Scan-strategi | **Scan filtreret undersæt + progress + session-cache** |
| Faseskift | **Tilføj + fjern + forhåndsvis** (INSERT/DELETE `tblDiagrammerMedian`) |
| Scope | **Kun aktive Seriediagrammer**: `diagram_type = 1 AND diagram_aktivt` (553 stk) |
| Signal-fase | **Seneste fase** (efter sidste eksisterende median-knæk) |
| Dato-vindue | **UI-toggle**: Alle data ↔ Seneste N punkter (N konfigurerbar) |
| Levering | **To faser**: A (headless engine) → B (review-UI) |

## Nøglefakta fra økosystem-udforskning
- **BFHddl** (`~/R/BFHddl`): `data_load_indicator(indicator_name, organisation, from_date,
  to_date)` loader parquet pr. indikator (arrow, 1-niveau folder-discovery der undgår
  ~67k dato-partition-mapper), filtrerer på `enhed`-kolonne. `resolve_median_breaks()`
  → tblDiagrammerMedian-datoer til `part`-række-index. `resolve_target()` → mål-args.
  DB-lag er **kun ODBC/Access** → genimplementeres ikke; data-/arg-logik vendores.
- **Parquet-kolonner**: `dato`, `vaerdi`, `taeller`, `naevner`, `enhed`. Diagram-identitet
  = `(indikator_navn_teknisk, organisatorisk_navn_teknisk)` (ingen diagram_id i parquet).
- **BFHcharts 0.25.0**: `bfh_qic(data, x, y, n, chart_type="run", part=, freeze=, cl=,
  target_value=, target_text=, multiply=, ...)` → `bfh_qic_result` med `$plot` (ggplot2),
  `$qic_data` (x/y/cl/signal pr. række), `$summary` (pr.-fase SPC-stats), `$config`.
  `bfh_extract_spc_stats()` → `runs_actual/expected`, `crossings_actual/expected`,
  `is_run_chart` m.fl. Signal: `runs_actual>runs_expected` ELLER `crossings_actual<crossings_expected`.
  Ingen ggiraph i pakken endnu.
- **Ingen eksisterende app** skriver til `tblDiagrammerMedian` (kun læser) eller har
  review-loop / click-to-mark. `bfh_seriediagrammer_2024` enumererer via `pmap` over
  tblDiagrammer (reference). `periode_fra` er næsten tom (2/836) → kan ikke styre vindue.
- **`tblDiagrammerMedian`**: kolonner `diagram` (FK→tblDiagrammer.id), `laas_median`
  (dato), `id` (pk, identity). Faseskift = INSERT (diagram, laas_median); fjern = DELETE pk.

## Arkitektur — komponenter (ét ansvar hver)

### Fase A — headless engine (testbar uden UI)
- **`R/fct_parquet.R`** (vendored): `parquet_indicator_path(base, indikator_navn_teknisk)`
  (1-niveau discovery), `parquet_load_slice(path, enhed_variants, from, to)` (arrow
  open_dataset + filter på `dato`/`enhed`). `enhed_variants` afledes af org-navne +
  `tblOrganisationOversaettelse`.
- **`R/fct_diagram_index.R`**: `build_diagram_index(db)` → én række pr. aktivt
  Seriediagram med kolonner: `diagram_id, indikator_id, indikator_navn,
  indikator_navn_teknisk, datasaet (=hierarki_navn), datapakke (=forælder-hierarki),
  org_id, org_navn_teknisk, afdeling, afsnit`. Supabase-query joiner tblDiagrammer +
  tblIndikatorer + tblIndikatorHierarki(+self-join forælder) + tblOrganisationStruktur.
  **Afdeling/Afsnit** løses via rekursiv CTE op ad org-træet til de relevante
  `organisatorisk_niveau`-niveauer (defineres ud fra tblOrganisationNiveauer).
- **`R/fct_signal.R`**: `resolve_median_breaks(median_rows, x_dates)` (vendored),
  `resolve_target(target_rows)` (vendored), `compute_signal(slice, parts, target,
  window)` → kører `bfh_qic(chart_type="run", part=parts, ...)`, læser sidste række af
  `$summary`/`bfh_extract_spc_stats` for seneste fase → `list(signal=TRUE/FALSE,
  runs=…, crossings=…, qic_result=…)`. y/n-mapping: hvis `naevner` findes → proportion
  (y=taeller, n=naevner, multiply=100); ellers run på `vaerdi`.
- **`R/fct_db.R`** (udvid `make_db`): `list_active_seriediagrammer()`,
  `diagram_medians(diagram_id)`, `diagram_targets(diagram_id)`,
  `add_median_break(diagram_id, dato)` (INSERT, write-guard, RETURNING id),
  `delete_median_break(median_id)` (DELETE, write-guard).

### Fase B — review-UI
- **`R/fct_chart_interactive.R`**: `interactive_run_chart(qic_data, target, breaks)` →
  ggplot fra `qic_data` (linje + `geom_point_interactive` med tooltip=dato+værdi,
  `data_id`=dato; median-trin pr. fase; mål-linje; signal-punkter fremhævet) →
  `ggiraph::girafe(ggobj=…, options=opts_selection(type="single"))`. BFHtheme-styling.
- **`R/mod_signal_review.R`**: UI (parquet-sti + vindue-toggle + 5 filtre + Scan-knap +
  signal-liste + graf + Forrige/Næste + faseskift-knapper + eksisterende-knæk-liste) og
  server (filtrér index → scan med `withProgress` + cache i `reactiveVal` nøglet på
  (filter, vindue) → vælg/bladr → render girafe → `input$<id>_selected` = valgt dato →
  forhåndsvis (re-kør bfh_qic med ekstra break) → Gem (add_median_break) → re-scan diagram /
  næste). Fjern-knæk via DELETE.
- **`R/app_ui.R`/`app_server.R`**: ny `nav_panel("Signal-gennemgang", …)` +
  `mod_signal_review_server` + landing-flise.

## Dataflow
```
parquet-mappe + 5 filtre + vindue-valg
  → build_diagram_index(db) filtreres → kandidat-diagrammer
  → SCAN (withProgress, cache): pr. diagram
       parquet_load_slice → resolve parts/target → compute_signal
  → signal-liste (kun signal=TRUE)
  → vælg/bladr → interactive_run_chart (girafe)
  → klik punkt (input$..._selected = dato)
  → forhåndsvis split → Gem → add_median_break(diagram_id, dato)
  → re-scan diagram (signal forsvinder typisk) → Næste
```

## Fejlhåndtering
- Manglende parquet-folder/indikator eller tom slice → diagram markeres "ingen data",
  springes over i scan, logges (struktureret). Ingen hård fejl.
- `bfh_qic`-fejl på et diagram fanges (`safe_operation`) → diagram udelades + log.
- DB-skriv (`add/delete_median_break`) bag `assert_write_enabled()` + `tryCatch`;
  fejl → notifikation, ingen delvis tilstand.
- Scan kan afbrydes; cache bevarer færdige resultater.

## Test
| Hvad | Hvordan | Fase |
|------|---------|------|
| Signal-motor | Fixtures parquet/df med kendt løb/kryds → `compute_signal` flagger korrekt; seneste-fase-logik med parts | A |
| Median-resolve / target | Pure-funktion unit-tests (datoer → række-index; mål-parse) | A |
| Diagram-index | Mod fake_db / gated Supabase: korrekte labels + Afdeling/Afsnit-ancestor | A |
| parquet_load_slice | Lille fixture-parquet → korrekt filter på enhed/dato | A |
| Faseskift-skriv | Gated integration: INSERT→læs→DELETE round-trip mod Supabase (`BFHMETA_WRITE=1`), oprydning via on.exit | A/B |
| Chart + interaktion | testServer: `input$..._selected` → forhåndsvis-state; Gem kalder add_median_break | B |

## Afhængigheder
Nye `Imports`: `BFHcharts`, `BFHtheme`, `ggiraph`, `arrow` (qicharts2 trækkes via
BFHcharts). Verificér installerede versioner ved opstart.

## Sikkerhed
- Write-guard (`BFHMETA_WRITE=1` / option) på alle median-skrivninger.
- RLS aktiv på Supabase; postgres-rollen (vores pool) bypasser → admin-tooling.
- Parquet-sti er bruger-angivet lokal mappe; valider at den findes + er læsbar (ingen
  path-traversal-risiko da kun læsning af bruger-valgt mappe).

## Ikke i scope (YAGNI / senere)
- P-diagrammer (type 10) + andre korttyper.
- Punkt-eksklusion, mål-redigering, kommentarer (tblDiagrammerKommentar) i denne runde.
- Master-detail for diagram-børn (separat runde).
- Baggrunds-scan af alle 553 (vi scanner filtreret undersæt).
- Skrivning til Access (kun Supabase).
