# Design: Fuld CRUD (organisation, hierarki, diagrammer)

**Dato:** 2026-08-10
**Status:** Godkendt
**App:** BFHmetadata (Shiny, Golem) — lokalt, mod Supabase

## Formål
Gøre metadata-databasen fuldt redigerbar fra appen. Fire tabeller mangler
skrive-interface: `tblOrganisationOversaettelse`, `tblDiagrammer`,
`tblOrganisationStruktur`, `tblIndikatorHierarki`. Efter dette design kan alle
19 tabeller vedligeholdes uden Access.

## Kontekst & beslutninger (afklaret med bruger)
| Emne | Valg |
|------|------|
| Sandhedskilde | **Supabase er master fra nu.** Access fryses; ingen flere drop+reload. PBI-import (bfh_dataportal) skal omlægges til Supabase — separat opgave uden for dette design |
| Arkitektur | **Tilgang 1 — genbrug maksimalt**: oversættelse via `LOOKUP_TABLES`; ét generisk `mod_hierarchy` til begge hierarkier; bespoke `mod_diagram` + delt formular |
| Diagram-redigering | **Begge veje**: selvstændig filterbar oversigt + redigering fra indikator-modalen (delt formular-funktion) |
| Hierarki-scope | **Felter + opret/slet + flyt** (re-parenting via dropdown, cyklus-validering). Ingen drag-and-drop |
| Levering | **Fire faser** A→D, hver selvstændigt releasebar med MINOR-bump (0.6.0→0.9.0) |

## Nøglefakta fra data (reload 2026-08-10, 12.392 rækker)
- **`tblDiagrammer`** (4.126; 614 aktive): FK'er → indikator, org, diagramtype.
  Ny kolonne `periode_aggregering`: to-værdi-enum `maaned` (3.513) / `uge` (613),
  ingen NA. `diagram_type` reelt kun 1 (4.109) og 10 (17).
- **`tblIndikatorHierarki`** (165 noder, 5 niveauer): ny `aktiv` (7 FALSE — i
  brug) + ny `kilde_id` (100 % tom — forberedt til import-sporbarhed).
  Parent-kolonne hedder `parent_id`.
- **`tblOrganisationStruktur`** (362 noder, niveauer 2/3/5/6/7/8, **2 rødder**):
  ingen aktiv-kolonne. Parent-kolonne hedder `parent_Id` (bemærk casing).
  Niveau-spring er legitime i data (2→3→5).
- **`tblOrganisationOversaettelse`** (443): 2 datakolonner — `…navn_fra_data`
  (tekst) + `…navn_teknisk` (FK→org). Intet refererer til rækkerne → sletning
  ufarlig.
- FK `tblDiagrammerMedian.diagram → tblDiagrammer.id` er nu **enforced** i
  Supabase (aktiveret ved reload; orphan-tjek rent). Ingen FK'er har ON
  DELETE-regler → sletning af rækker i brug fejler loud.
- Signal-indekset resolver overafdeling/afdeling/afsnit via org-niveau 5/6/7
  (rekursiv ancestry) — niveau-semantik må ikke ødelægges ved flyt.

## Arkitektur — komponenter (ét ansvar hver)

### Fælles principper
- Al skrivning via `make_db`-accessors + `assert_write_enabled()` (eksisterende
  write-guard dækker automatisk alt nyt). `safe_operation()` ved DB-kald.
- SQL-byggere = rene funktioner i `fct_sql.R` (unit-testbare uden DB).
- Navigation: landing-siden får sektioner **"Diagrammer"** (1 flise) og
  **"Organisation"** (Struktur + Oversættelse); Indikator-hierarki-flise under
  eksisterende "Indikatorer"-sektion. `nav_panel`/`nav_select`-mønster uændret.

### Fase A — Oversættelse (ren config, ingen ny modulkode)
Ny entry i `LOOKUP_TABLES` (metadata.R):
- `organisatorisk_navn_fra_data` → `type = "text"`
- `organisatorisk_navn_teknisk` → `type = "fk"`, parent
  `tblOrganisationStruktur`, `label_expr =
  COALESCE("organisatorisk_navn_langt","organisatorisk_navn_teknisk")` —
  identisk med Personer-mønstret.
- 443 rækker × 362-option-selects OK: DT DOM-renderer kun synlig side.
- Ingen `ref_check` (intet peger på oversættelses-rækker).

### Fase B — Diagram-modul
- **`mod_diagram`** (oversigt): DT over alle diagrammer med resolvede labels
  (indikator-navn, org-navn, type-navn, periode, 3 bool-flag) + Åbn-knap.
  Ny SQL-bygger `build_diagram_admin_sql()` — join-mønster som
  `build_diagram_index_sql()` men uden type/aktiv-filter.
  Fire filtre over tabellen: indikator (selectize, 1.282), organisation
  (selectize, 362), status (alle/aktive/inaktive — **default aktive**), type.
- **`diagram_form_modal()`** (delt funktion, ej modul): indikator + org
  (selectize-FK, påkrævede), diagramtype (select), `periode_aggregering`
  (select, choices fra `SELECT DISTINCT` i DB — robust ved nye værdier),
  3 checkboxes (aktivt, indgår i aggregering, direktionens tavle).
- **Duplikat-guard (blød)**: advarsel hvis (indikator, org, type) findes
  allerede — blokerer ikke (Access håndhævede aldrig unikhed).
- **Sletning**: FK-fejl fra median-knæk fanges → *"Diagrammet har N
  median-knæk — deaktivér i stedet, eller slet knækkene først."*
  Deaktivering er anbefalet vej (fjerner diagram fra signal-scan).
- **Indikator-modal-integration**: sektion "Diagrammer" nederst — kompakt
  liste + "Nyt diagram" (indikator forudvalgt + låst). Shiny-modal-swap:
  diagram-formular erstatter indikator-modalen; ved gem/annullér genåbnes
  indikator-modalen på samme indikator.

### Fase C+D — Generisk hierarki-modul (`mod_hierarchy`)
Config-drevet via `HIERARCHY_TABLES` (metadata.R), to instanser:

| | `org_struktur` (C) | `indikator_hierarki` (D) |
|---|---|---|
| Parent-kolonne | `parent_Id` | `parent_id` |
| Visningsnavn | `organisatorisk_navn_langt` | `hierarki_navn` |
| Felter | teknisk/langt/kort navn | navn, kort navn, 2 beskrivelser, `kilde_id` |
| Niveau-FK | → `tblOrganisationNiveauer` | → `tblIndikatorNiveauer` |
| Aktiv-kolonne | (ingen) | `aktiv` |

- **Visning**: indrykket træ-tabel i DT — én række pr. node, navn indrykket
  efter dybde, depth-first træ-orden. Sortering beregnes i R af ren funktion
  **`hierarchy_order(df, pk, parent_col)`** (unit-testbar, genbruges).
- **Redigering**: klik række → modal med felter + Forælder-dropdown +
  aktiv-checkbox (hvor findes). Rodnoder tilladt (org har 2).
- **Cyklus-forhindring i to lag**: (1) Forælder-dropdown ekskluderer nodens
  egen subtree via ren funktion **`hierarchy_descendants(df, id)`**;
  (2) server-side assert før UPDATE (værn mod stale UI). Samme rene funktion
  bærer begge lag.
- **Niveau-konsistens (blød)**: advarsel hvis nodens niveau ikke er dybere end
  forælderens — ikke blokering (data har legitime spring). Beskytter
  signal-indeksets niveau-5/6/7-resolution.
- **Opret/slet**: "Ny node" med forælder forudfyldt fra valgt række. Sletning
  kræver ingen børn (server-tjek) + ingen indgående FK'er (DB-fejl fanges →
  *"Noden er i brug af N …"*). Indikator-hierarki: deaktivering anbefales som
  alternativ.
- **Fase D-specifikt**: indikator-modalens hierarki-dropdown filtreres til
  aktive noder ved *nyvalg*; eksisterende værdi på inaktiv node bevares og
  markeres "(inaktiv)" — ingen stille datamutation. `kilde_id` = valgfrit
  tekstfelt.

## Fejlhåndtering
- Write-guard: alle nye writes bag `assert_write_enabled()`.
- FK-/DB-fejl fanges via `safe_operation()` og oversættes til brugervenlige,
  handlingsanvisende beskeder (deaktivér frem for slet, osv.).
- Ingen ON DELETE-kaskader — bevidst: fejl-loud frem for stille tab.

## Teststrategi (TDD, risk-based)
| Lag | Dækning |
|-----|---------|
| Rene funktioner | `hierarchy_order`, `hierarchy_descendants`, alle nye SQL-byggere — fuld unit-dækning (kritiske paths: cyklus, træ-orden) |
| DB-writes | Gated tests (`BFHMETA_WRITE=1`) som `test-db-junction.R` — create/update/delete + FK-fejl-oversættelse |
| Moduler | `testServer` for kritiske flows: diagram-formular gem/annullér, modal-swap-retur, hierarki flyt+cyklus-afvisning, slet-guards |
| Config | `LOOKUP_TABLES`/`HIERARCHY_TABLES`-validering (kolonner findes i skema) |

## Levering & versionering
| Fase | Indhold | Version |
|------|---------|---------|
| A | Oversættelse via `LOOKUP_TABLES` | 0.6.0 |
| B | `mod_diagram` + delt formular + indikator-modal-integration | 0.7.0 |
| C | `mod_hierarchy` + org-struktur-instans | 0.8.0 |
| D | Indikator-hierarki-instans + aktiv/kilde_id-håndtering | 0.9.0 |

Hver fase: egen feature-branch, TDD, NEWS-entry, MINOR-bump jf.
`VERSIONING_POLICY.md`.

## Uden for scope (bevidst)
- Omlægning af bfh_dataportal PBI-import til Supabase (forudsætning for
  "Supabase som master" — separat projekt/repo).
- Drag-and-drop-træ-UI (YAGNI ved 165-362 noder).
- Hård unikheds-constraint på diagrammer (data har legitime dubletter).
- Redigering af `tblDiagramIndstillinger`, `tblDiagrammerMaal`,
  `tblDiagrammerKommentar` (småtabeller; kan senere tilføjes via
  `LOOKUP_TABLES` hvis behov).
