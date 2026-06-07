# Design: tblIndikatorer CRUD-app (v0)

**Dato:** 2026-06-07
**Status:** Til review
**Formål:** Validere Supabase-backenden med en fungerende CRUD-app FØR consumers
(bfh_dataportal/BFHddl) skiftes fra Access (Trin 3). Giver "noget der virker"
som tryghed inden produktions-pipelines røres.

---

## Kontekst

Access→Supabase-migrationen er gennemført (19 tabeller, 8720 rækker, FK +
sequences verificeret, RLS aktiveret). Denne app er Fase 2 fremrykket, afgrænset
til ÉN tabel for at bevise write-back-round-trip mod Supabase.

---

## Beslutninger (truffet i brainstorm)

| Område | Valg |
|--------|------|
| **Scope** | Kun `tblIndikatorer` (21 kolonner, 3 FK) |
| **Operationer** | Create + Read + Update + Soft-delete |
| **Sletning** | Soft-delete (`aktiv_indikator=FALSE`) i appen; hard-delete er manuel escape hatch i Supabase |
| **Auth** | Ingen (lokal kørsel). Forbinder som `postgres`-rolle → bypasser RLS |
| **Redigerings-UX** | Master-detail formular (primær) + inline DT-redigering (sekundær) |
| **Struktur** | Fuld Golem-pakke |
| **Placering** | Golem-pakke i BFHmetadata-roden; migration flyttes til `migration/` |

---

## Repo-omstrukturering

BFHmetadata-roden bliver Golem-pakken `BFHmetadata`. Migrations-artefakter
flyttes til `migration/`. Delt config bliver i roden.

```
BFHmetadata/
├── DESCRIPTION, NAMESPACE          # Golem-pakke
├── R/                              # app-kode
├── inst/                           # golem-config + app-ressourcer
├── tests/testthat/                 # unit-tests
├── dev/                            # golem dev-scripts (run_dev.R, 01_start.R...)
├── config.yml                      # BLIVER i roden (delt: app + migration)
├── .Renviron / .Renviron.example   # BLIVER (delt secrets)
├── docs/                           # specs, ADR
└── migration/                      # FLYTTET hertil
    ├── 00_introspect_access.R, 01_generate_ddl.R, 02_migrate_data.R
    ├── migration_metadata.R        # genbruges af app (se DB-lag)
    ├── access_schema.yaml, access_data_dump/, PLAN*.md, ...
```

**Sti-konsekvens:** migrations-scripts læser i dag `config.yml` +
`access_schema.yaml` relativt til CWD. Efter flytning skal de læse `../config.yml`
og lokal `access_*`. Migrationen er færdig (scripts er reference), men jeg
verificerer at de stadig kører fra `migration/` efter sti-justering.

**`migration_metadata.R`:** genbruges af både migration og app som FK-sandhedskilde.
Placering: enten kopi i pakkens `R/` eller sourced fra `migration/`. Beslutning:
flyt FK/PK-metadata ind i pakken (`R/`) som autoritativ kilde; migration-scripts
sourcer fra pakken via relativ sti. Undgår to kopier der kan drifte.

---

## Pakke-arkitektur (Golem)

- `R/run_app.R`, `R/app_ui.R`, `R/app_server.R`, `R/app_config.R` — Golem-skelet
- `R/mod_indikator_crud.R` — CRUD-modul (UI + server for tblIndikatorer)
- `R/fct_db.R` — pool-livscyklus + queries (load, insert, update, soft-delete, FK-lookups)
- `R/utils_validation.R` — input-validering før gem
- `R/metadata.R` — FK_MAP/PK_MAP (flyttet fra migration_metadata.R)

**DB-forbindelse:** `pool::dbPool(RPostgres::Postgres(), ...)` oprettet ved
app-start, lukket ved `onStop()`. Læser `config.yml$default$supabase` +
`Sys.getenv("SUPABASE_DB_PASSWORD")`. Rolle = `postgres` → bypasser RLS.

### Sikkerheds-posture (v0) — bevidst trade-off

v0 er et **lokalt single-operator admin-værktøj**, ikke deployet. `postgres`-rollen
bypasser RLS bevidst — privilegeret adgang er normalt for admin-tooling (Supabases
egen Table Editor bruger `service_role` med samme bypass). RLS beskytter den
offentlige anon-API, ikke dette betroede værktøj.

**Guards i v0 (billige hærdninger ift. Codex adversarial review):**
- `run_app()` kører kun lokalt (host `127.0.0.1`); ikke eksponeret på netværk.
- **Skrive-guard:** før første skrivning kræves eksplicit bekræftelse af target
  (vis host + dbname; `options(bfhmeta.write_enabled = TRUE)` eller env-flag
  `BFHMETA_WRITE=1` skal være sat). Forhindrer utilsigtet skrivning mod forkert DB.
- Læsning er fri; skrivning gated bag guarden.

**Udskudt til Fase 2 (ved deployment / fler-bruger):**
- Rigtig auth (shinymanager el. Supabase Auth) + bruger-attribution
- Least-privilege DB-rolle (table-grants, ej `postgres`) + RLS-policies
- Audit-log (hvem ændrede hvad, hvornår)

---

## CRUD-modul: `mod_indikator_crud`

### UI (bslib)
- **DT-liste** (read): søgbar, FK'er opløst til navne (ikke id'er), `aktiv`-status
  synlig (fx farve/badge). Inaktive (soft-deleted) kan filtreres til/fra.
- **Redigerings-formular** (detail): FK-dropdowns (læsbare navne), 3 checkboxes
  (booleans), date-picker (`periode_fra`), textareas (definitioner/beskrivelser),
  tekstfelter (øvrige). Vælg række i listen → fyld formular.
- **Inline DT-redigering**: hurtige rettelser af simple felter (sekundært).
- Knapper: Ny, Gem, Soft-delete, Gendan (aktiv=TRUE).

### Server-flow
- **Load:** query tblIndikatorer + JOIN FK-parents for labels → `reactiveVal`.
  Genindlæses efter hver skrivning.
- **Create:** INSERT (id auto via IDENTITY — sequence resat, sikkert) `RETURNING id`.
- **Update:** UPDATE efter validering.
- **Soft-delete:** UPDATE `aktiv_indikator=FALSE`. Gendan = TRUE.
- Alle skrivninger via `safe_operation()` + struktureret logging + reaktiv reload.

---

## DB-lag + FK-dropdowns

`FK_MAP` (fra metadata) filtreret til tblIndikatorers 3 FK'er som sandhedskilde:

| FK-kolonne | Parent-tabel | Parent-PK | Vist label (verificeres mod skema) |
|---|---|---|---|
| `indikator_hierarki` | tblIndikatorHierarki | Id | hierarki-navn |
| `kontaktperson` | tblPersoner | Id | fornavn + efternavn |
| `datakilde` | tblDatakilder | Id | datakilde-navn |

Dropdowns fyldes fra parent-tabeller (id → label) ved app-start (cached) eller
ved reload. Label-kolonner verificeres mod `access_schema.yaml` ved implementering.

---

## Validering + fejlhåndtering

- `indikator_navn` ikke-tom (praktisk krav; skema tillader NULL).
- FK-valg gyldige (dropdowns garanterer; tom = NULL tilladt for nullable FK).
- DB-fejl fanges i `safe_operation()` → brugervenlig besked (notification), ingen crash.
- Skrivninger er enkelt-række → ingen transaktions-kompleksitet.

---

## Test (risk-baseret)

- **Unit:** `utils_validation.R` + query-byggere (rene funktioner) via testthat.
- **Modul:** `testServer()` for create/update/soft-delete-logik med mocket/injiceret db-lag.
- **Integration:** manuel smoke mod Supabase (write→read round-trip). Dokumenteres
  som manuelt trin (kræver target + credentials).

---

## Uden for scope (YAGNI for v0)

- Auth (shinymanager/Supabase Auth) — Fase 2
- Audit-log (hvem/hvornår) — Fase 2
- Øvrige 18 tabeller — Fase 2 (CRUD-modul genbruges)
- Hard-delete i appen — manuel i Supabase nu; evt. beskyttet knap senere
- pbi-import-ejerskabskoordinering — afklares før Create/Delete i produktion

### Hard-delete (manuel escape hatch — dokumenteret, ej i app)
Fejl-importerede indikatorer slettes i Supabase. FK'er er RESTRICT → ryd børn først.
`FK_MAP` bekræfter at PRÆCIS 4 tabeller refererer `tblIndikatorer.id` (de 3 forbind-
tabeller + tblDiagrammer) — listen nedenfor er komplet, ikke et udsnit.

**SKAL køres i transaktion** (partiel fejl ruller tilbage; ingen halvt-slettede børn):
```sql
BEGIN;
DELETE FROM "tblForbindIndikatorerFaggrupper"    WHERE indikator_id = :id;
DELETE FROM "tblForbindIndikatorerOrganisation"  WHERE indikator_id = :id;
DELETE FROM "tblForbindIndikatorerDataprodukter" WHERE indikator_id = :id;
DELETE FROM "tblDiagrammer"                        WHERE indikator     = :id;
DELETE FROM "tblIndikatorer"                        WHERE id            = :id;
COMMIT;   -- ROLLBACK hvis nogen sætning fejler
```
Supabase SQL Editor kører hver kørsel som én transaktion, men skriv BEGIN/COMMIT
eksplicit så intentionen er klar + kopierbar til psql.

---

## Åbne punkter (afklares ved implementering)

1. Label-kolonne-navne i de 3 parent-tabeller (verificeres mod skema).
2. Inline-redigerings scope: hvilke felter er sikre til inline (ej FK/dato/bool)?
3. `migration_metadata.R`-flytning til `R/` — sti-opdatering i 3 migration-scripts.
4. pbi-import vs manuel Create: hvilke felter ejer hvem (ikke blokerende for v0,
   men noteres så manuel + script ikke overskriver hinanden utilsigtet).
