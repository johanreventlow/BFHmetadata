# BFH-database-migration

Arbejdsmappe til migration af BFH-metadata-database fra Access til Supabase
(EU-region, Frankfurt).

**Denne mappe ligger BEVIDST uden for bfh_dataportal-repoet** — den indeholder
data-dumps og work-in-progress der ikke hører hjemme i git.

Se [PLAN.md](PLAN.md) for fuld plan og kontekst.

---

## Hvor du er nu

Migrationen følger en multi-trins struktur. Du er sandsynligvis her:

| Trin | Hvor | Status |
|------|------|--------|
| 0a Backend-afklaring (Supabase) | Manuel (email Region) | ⏳ |
| **0b Skema-introspection + data-dump** | **Windows (her)** | 👉 START HER |
| ZIP-transport til Mac | Windows → OneDrive → Mac | ⏭ |
| 1+ DDL-gen, data-import, consumer-updates | Mac | ⏭ |

---

## Windows-flow (Trin 0b)

### Forudsætninger

- ODBC DSN `dataportal_2024` konfigureret (peger på Access-filen)
- R ≥ 4.5 installeret
- R-pakker:

```r
install.packages(c("DBI", "odbc", "yaml", "arrow", "tibble", "dplyr"))
```

### Trin 1 — Kør introspection-scriptet

```r
setwd("~/bfh_db_migration")  # eller cd dertil i terminal og start R
source("00_introspect_access.R", encoding = "UTF-8")
```

Det producerer:

- `access_schema.yaml` — komplet skema-metadata
- `access_data_dump/<tbl>.parquet` — data pr. tabel (~22 filer)
- `introspection_log.txt` — log

Forventet runtime: 1-5 minutter afhængigt af data-størrelse.

### Trin 2 — Suppler manuelt via Access GUI

To ting kan ikke scriptes pålideligt og kræver manuel eksport fra Access:

#### A) Relations-diagram (PNG)

1. Åbn `dataportal_2024.accdb` i MS Access
2. Database Tools → Relationships
3. Tag screenshot (Win+Shift+S eller Snipping Tool) af hele diagrammet
4. Gem som `~/bfh_db_migration/access_relationships.png`

#### B) Database Documenter-rapport (PDF)

1. I Access: Database Tools → Database Documenter
2. Vælg "Tables" → "Select All"
3. Klik "Options..." — minimum: Include for Table = Properties + Relationships;
   Include for Fields = Names, Data Types, Sizes, Properties; Include for
   Indexes = Names, Fields, Properties
4. OK → Print Preview åbnes
5. Print til PDF → gem som `~/bfh_db_migration/access_database_documenter.pdf`

### Trin 3 — Pak til ZIP

```bash
# Fra git-bash i ~/bfh_db_migration/:
bash 99_pak_zip.sh
```

Output: `~/bfh_db_migration_YYYY-MM-DD.zip` i parent-mappen.

Upload ZIP til OneDrive eller anden delt lokation Mac kan tilgå.

---

## Mac-flow (Trin 1+)

På Mac:

1. Download ZIP fra OneDrive
2. Unpack til `~/bfh_db_migration/` (eller andet sted — opdater stier hvis nødvendigt)
3. `cd ~/bfh_db_migration/`
4. Kopier `.Renviron.example` til `.Renviron` + udfyld Supabase credentials
5. Installer R-pakker:
   ```r
   install.packages(c("DBI", "RPostgres", "pool", "yaml", "arrow",
                       "tidyverse", "here"))
   # Hvis renv.lock findes: renv::restore()
   ```
6. Kør Trin 1: `Rscript 01_generate_ddl.R` (skrives på Mac)
7. Kør Trin 2: `Rscript 02_migrate_data.R` (skrives på Mac)

Se PLAN.md for detaljer på resterende trin.

---

## Fil-struktur

```
~/bfh_db_migration/
├── README.md                          ← du er her
├── PLAN.md                            ← fuld plan (kopi af approved plan)
├── config.yml                         ← connection-config
├── .Renviron.example                  ← skabelon (kopier til .Renviron)
├── .Renviron                          ← KUN LOKALT — aldrig commit/zip
├── 00_introspect_access.R             ← Windows-script (Trin 0b)
├── 99_pak_zip.sh                      ← ZIP-pakning til Mac-transport
│
├── access_schema.yaml                 ← OUTPUT fra 00_introspect_access.R
├── access_data_dump/                  ← OUTPUT: Parquet-filer pr. tabel
│   ├── tblIndikatorer.parquet
│   └── ...
├── access_relationships.png           ← MANUELT fra Access GUI
├── access_database_documenter.pdf     ← MANUELT fra Access GUI
├── introspection_log.txt              ← LOG fra introspection-kørsel
│
├── 01_generate_ddl.R                  ← TILFØJES PÅ MAC (Trin 1)
├── 01_schema.sql                      ← OUTPUT fra 01_generate_ddl.R
├── 02_migrate_data.R                  ← TILFØJES PÅ MAC (Trin 2)
└── 03_verify.R                        ← TILFØJES PÅ MAC (Trin 3)
```

---

## Hjælp og fejlfinding

**MSysRelationships ikke tilgængelig:**
Aktivér system-tabeller i Access: Filer → Indstillinger → Gennemse-database →
Navigations-indstillinger → "Vis systemobjekter" = JA. Genstart Access.
Test derefter med Trin 0b igen.

**Encoding-problemer med æ/ø/å:**
Sample-rækkerne i `access_schema.yaml` skal indeholde læselige danske tegn.
Hvis de viser mojibake (`Ã¦`, `Ã¸`), tjek at DSN er sat med `encoding=UTF-8`.

**Parquet-fil er tom (0 bytes):**
`dbReadTable()` fejlede på den specifikke tabel. Tjek `introspection_log.txt`
for fejlmeddelelse — sandsynligvis kolonne-typer (fx memo-felter) der
arrow-pakken ikke kan håndtere uden konvertering.

---

## CRUD-app (Indikator-admin, v0)

Lokal Golem Shiny-app til redigering af `tblIndikatorer` i Supabase.

### Start app

```bash
Rscript -e 'pkgload::load_all("."); run_app()'
```

Åbner på http://127.0.0.1 (kun lokal). Kræver `.Renviron` med `SUPABASE_DB_PASSWORD`.

### Skrive-guard

Skrivninger (create/update/soft-delete) er deaktiveret som standard. Aktivér bevidst:

```r
options(bfhmeta.write_enabled = TRUE)   # i R-session før run_app()
# eller
BFHMETA_WRITE=1 Rscript -e 'pkgload::load_all("."); run_app()'
```

### Smoke-test (read + write round-trip)

```bash
BFHMETA_WRITE=1 Rscript dev/smoke_supabase.R
```

Migrations-filer ligger i `migration/`.
