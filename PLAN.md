# Plan: Migration af BFH-metadata-database fra Access til moderne backend

## Context

`dataportal_2024.accdb` (MS Access) er den fælles metadata-database for **mindst to R-projekter** i BFH-økosystemet:

- **bfh_dataportal** — primær consumer + skriver (PBI-import-script INSERT/UPDATE'er hierarki + indikatorer)
- **BFHddl** — read-only consumer (læser metadata til at producere SPC-seriediagrammer)

Access via ODBC har vist begrænsninger:
- File-locking-issues i OneDrive-mappe (har forårsaget render-fejl tidligere)
- Ingen indbygget multi-user CRUD med audit/versionering
- Begrænset til Windows + ODBC-driver
- Vanskelig at backe op atomisk mens scripts kører
- Vanskelig at exponere via web (Shiny CRUD-app)

**Mål for migration:** Flytte til en moderne relationel backend der:
1. Understøtter samtidig læsning + få samtidige skrivere uden file-locks
2. Har god R-integration via DBI
3. Kan eksponeres bag en fremtidig Shiny CRUD-app
4. Bevarer 1:1 skema-kompatibilitet med eksisterende Access-DB (konservativ migration)
5. Lever op til Region Hovedstadens data-håndteringskrav (metadata, ingen patientdata)

**Brugerens beslutninger:**
- **Skema:** Konservativ 1:1-oversættelse (bevarer kolonne-navne inkl. danske tegn). Modernisering udskydes til separat sprint.
- **Prioritering:** Migration først (Fase 1), Shiny CRUD-app senere (Fase 2).
- **Backend:** **Supabase EU-region (Frankfurt)** — managed PostgreSQL.
- **Migration-projekt isoleres:** alle migrations-artefakter ligger UDEN FOR bfh_dataportal-repoet i en separat arbejdsmappe (`~/bfh_db_migration/` eller lignende). Transport mellem Windows og Mac sker via **ZIP-fil** (ikke git-commit) for at undgå at fylde repoet med data-dumps.

---

## Current state: Database-mapping

### Tabel-inventar (22 tabeller, alle med `tbl`-prefix)

| Kategori | Tabeller | Estimeret rækker | Skrivere |
|----------|----------|------------------|----------|
| **Indikator-kerne** | tblIndikatorer | ~600 (~350 aktive) | bfh_dataportal/pbi-import |
| **Hierarki** | tblIndikatorHierarki (5 niveauer self-join), tblIndikatorNiveauer | ~157 | bfh_dataportal/pbi-import |
| **Organisation** | tblOrganisationStruktur (self-join), tblOrganisationOversaettelse, tblOrganisationNiveauer | ~30 | (manuel) |
| **Kontaktpersoner** | tblPersoner | ~10-30 | (manuel) |
| **Faggrupper** | tblFaggrupper | ~5-8 | (manuel) |
| **Datakilder** | tblDatakilder | ~5 (SP, FLIS, PBI, SundK, m.fl.) | bfh_dataportal (kan oprette) |
| **Many-to-many relationer** | tblForbindIndikatorerOrganisation, tblForbindIndikatorerFaggrupper, tblForbindIndikatorerDataprodukter | ~100-1000+ | bfh_dataportal/pbi-import |
| **Diagram-config** | tblDiagrammer, tblDiagramTyper, tblDiagramIndstillinger, tblDiagrammerMaal, tblDiagrammerMedian, tblDiagrammerKommentar | varies | (manuel/BFHddl?) |
| **Dataprodukter** | tblDataprodukter | ~? | (manuel) |

### Access-specifikke mønstre der skal håndteres

- **`@@IDENTITY`** efter INSERT (auto-increment ID) → erstattes med `RETURNING id` (Postgres) eller `lastval()`
- **`[bracket]`-syntax** for danske tegn-kolonner (`[nøgleindikator]`) → forsvinder; Postgres kræver kun anførselstegn ved reserverede ord
- **`True`/`False`** (Access bool-konvention) → `TRUE`/`FALSE` (Postgres standard)
- **ODBC DSN-baseret forbindelse** → `RPostgres::Postgres()` + host/db/user

### Danske tegn-kolonner (skal bevares)

`nøgleindikator`, `tæller_beskrivelse`, `nævner_beskrivelse`, `mål`, `ønsket_tendens`, `organisatorisk_navn_*` — Postgres understøtter dem fuldt i UTF-8, men kræver double-quote-citater i SQL (`"nøgleindikator"`).

---

## Backend-valg: Supabase (EU-region, Frankfurt)

Brugeren har valgt Supabase som backend. Resten af planen er skrevet med Supabase som default.
Hvis Region's cloud-DB-politik senere skulle blokere Supabase (se Trin 0a), kan MySQL på egen cloud-server bruges som fallback — ændringerne er marginale (driver-skifte + lidt SQL-dialekt).

### Hvorfor Supabase

Supabase er managed PostgreSQL + en række batteries-included features der direkte adresserer dette projekts behov:

| Behov | Hvad Supabase tilbyder |
|-------|------------------------|
| Multi-writer relationel DB | Postgres med MVCC, intet file-lock som Access |
| UTF-8 + danske tegn | Native Postgres-håndtering |
| R-integration | `DBI` + `RPostgres` + `pool` — standard pattern |
| Backups | Automatiske daglige + point-in-time recovery (på Pro-tier) |
| Auth (til fremtidig Shiny CRUD) | Indbygget GoTrue-auth med JWT, email/password, SSO |
| Direkte DB-redigering (interim) | Indbygget table editor i Supabase dashboard — kan delvist erstatte Shiny CRUD i opstartsfase |
| Row-Level Security | Postgres RLS — fine-grained adgangskontrol pr. tabel/række |
| REST API auto-genereret | PostgREST — gør Shiny CRUD-byg endnu lettere (kan bruge `httr2` i stedet for direkte DBI) |
| Lavt operationelt overhead | Ingen OS-updates, security patches, monitoring du skal håndtere |
| Skalering | Free → Pro ($25/mo) → større tier ved behov |
| Vendor lock-in | Begrænset: ren Postgres under, kan eksporteres som `pg_dump` |

**Free tier dækker udviklingsfasen** (500 MB DB, pauset efter 7 dages inaktivitet — ikke et issue under aktivt arbejde). Til produktion: Pro-tier $25/mo.

### Fallback: MySQL hvis Supabase blokeres

Hvis Region's cloud-DB-policy senere udelukker Supabase, har vi en backup-vej:

| Aspekt | MySQL på cloud-server (fallback) |
|--------|----------------------------------|
| R-driver | `RMariaDB` (modent) — minimal kode-ændring fra `RPostgres` |
| UTF-8 | Skal sættes eksplicit til `utf8mb4` |
| Ops-arbejde | Du installerer, patcher, backupper, overvåger |
| Auth til CRUD | Bygges fra bunden (shinymanager eller lignende) |
| Backup-strategi | `mysqldump`-cron til ekstern lokation |

Skemaet er kompatibelt med begge — kun DDL-genereringen og connection-driveren differerer.

### GDPR / data-sovereignty-vurdering

Metadata-databasens indhold:
- Indikator-definitioner, beskrivelser, mål (ikke person-data)
- Hierarki-strukturer (ikke person-data)
- Organisations-navne (offentligt tilgængelige)
- Power BI-URLs (links)
- `tblPersoner`: fornavn, efternavn, titel, organisatorisk_enhed (faglig kontaktperson) → **dette er PII af medarbejdere**

Supabase EU-region (Frankfurt) er omfattet af GDPR og har Standard Contractual Clauses. For at hoste BFH-metadata med medarbejder-PII der bør du:

1. Tjekke Region Hovedstadens politik for cloud-DB-tjenester
2. Hvis Supabase ikke er på regionens godkendte liste: anmode om vurdering
3. Alternativt minimere PII i `tblPersoner` (kun navn + titel, ikke fx telefonnumre)
4. Hvis afvist → fald tilbage til MySQL på din cloud-server (forhåbentlig på en server med eksisterende godkendelse)

### Andre overvejede + afviste

| Backend | Hvorfor afvist |
|---------|----------------|
| **Firebase** | NoSQL → kræver komplet skema-redesign for 22 relationelle tabeller. Dårlig fit |
| **SQLite på shared drive** | OneDrive + SQLite har dokumenterede file-lock-issues. Ikke pålideligt for selv 2-3 samtidige skrivere |
| **DuckDB** | Stærk til analytics, umoden til transaktionel CRUD med flere samtidige writers |

### Eksekverings-sekvens

1. **Trin 0a:** Tjek Region's cloud-DB-politik for Supabase (~1 uge wall-time)
2. Hvis OK → Supabase-spor (resten af planen)
3. Hvis blokeret → skift til MySQL-fallback (samme plan, andre driver-pakker)

Region-check skal ske som ALLERFØRSTE skridt før migration-arbejdet starter — så undgår vi at lave arbejdet to gange.

---

## Cross-platform eksekvering (Windows + Mac via ZIP-transport)

Migration-projektet ligger UDEN FOR bfh_dataportal-repoet (fx `~/bfh_db_migration/` eller `~/Documents/bfh_db_migration/`). Artefakter overføres mellem Windows og Mac via **ZIP-fil** — ingen git-tracking for migration-data og data-dumps.

| Trin | Platform | Hvorfor |
|------|----------|---------|
| 0a Backend-afklaring | Hvor som helst | Email/forespørgsel |
| **0b Skema-introspection + data-dump** | **Windows (kun)** | MS Access ODBC-driver findes ikke officielt til Mac |
| **ZIP-overførsel #1** | Windows → Mac | Pak `~/bfh_db_migration/` → upload til OneDrive/USB → unpack på Mac |
| 1 DDL-generering | **Mac (anbefalet) eller Windows** | Læser portable YAML/Parquet-artefakter fra Trin 0b |
| 2 Data-import til Supabase | **Mac (anbefalet) eller Windows** | Læser Parquet, skriver til Supabase via cross-platform R-drivere |
| 3 Opdater consumers (R-kode) | Hvor som helst | Ren tekst-redigering — sker direkte i bfh_dataportal-/BFHddl-repo'er (de er git-trackede normalt) |
| 4 Parallel-drift (re-sync) | Windows | Genkør Trin 0b's data-dump periodisk; re-pakker ZIP til Mac hvis nødvendigt |
| 5 Cutover | Hvor som helst | Configuration-ændringer |

### Portabilitets-principper

- **Migration-arbejdsmappe** (`~/bfh_db_migration/`) er IKKE git-tracked og IKKE en del af bfh_dataportal-repo. Den indeholder R-scripts, schema-dumps, data-dumps (Parquet) og dokumentation.
- **ZIP-baseret transport:** Når Windows har produceret artefakter, zippes hele `~/bfh_db_migration/` (excl. evt. `.venv`/`renv/library`) og overføres via OneDrive (eller USB hvis OneDrive ikke synkroniseres på Mac). Mac unzipper til samme placering og fortsætter.
- **Connection-info** (Supabase URL, anon-key, db-password) i `~/bfh_db_migration/config.yml` med ENV-variabel-overrides; secrets (db-password, service-role-key) i `.Renviron` eller `.env` (aldrig i ZIP'en — Mac sætter sit eget)
- **Stier i R-scripts** er relative til arbejdsmappen via `here::here()` eller `getwd()` — ingen hardkodede `C:\Users\...` eller `/Users/...`
- **Line endings:** scripts gemmes som UTF-8 med LF (Unix-style) for cross-platform-kompatibilitet. Windows R håndterer LF fint
- **R-pakker:** dokumentér i `~/bfh_db_migration/renv.lock` så Mac kan installere samme versioner via `renv::restore()`
- **Plan-filen:** denne plan kopieres som `~/bfh_db_migration/PLAN.md` og inkluderes i ZIP'en

### Tjekliste: ZIP-pakning (Windows-side)

Når Trin 0b er færdig:

```bash
# I git-bash på Windows:
cd ~
zip -r bfh_db_migration_<dato>.zip bfh_db_migration/ \
  -x "bfh_db_migration/.Renviron" \
  -x "bfh_db_migration/renv/library/*" \
  -x "bfh_db_migration/.Rhistory"
# Upload bfh_db_migration_<dato>.zip til OneDrive
```

### Mac-setup-tjekliste (engangs-opsætning)

Inden Mac kan eksekvere Trin 1+:
1. R ≥ 4.5 installeret (`brew install r` eller fra CRAN)
2. R-pakker: `install.packages(c("DBI", "RPostgres", "pool", "yaml", "arrow", "tidyverse", "here", "renv"))`
3. Hent ZIP fra OneDrive, unpack til `~/bfh_db_migration/`
4. `cd ~/bfh_db_migration && Rscript -e 'renv::restore()'`
5. Opret `~/bfh_db_migration/.Renviron` med Supabase credentials (db-password, anon-key, service-role-key)
6. Installer Supabase CLI: `brew install supabase/tap/supabase` (valgfri, til lokal udvikling)
7. Klone bfh_dataportal og BFHddl repo'er via git (separat fra migration-mappen) — disse trin 3-ændringer går via normal git-flow

---

## Migration-approach (Fase 1)

**Bemærk:** Nedenstående trin er beskrevet med Supabase/Postgres som default. Hvis MySQL vælges, er ændringerne marginale — primært `RPostgres` → `RMariaDB`, `SERIAL` → `AUTO_INCREMENT`, `RETURNING id` → `LAST_INSERT_ID()`. Begge spor er low-risk fra et migration-perspektiv.

### Trin 0a: Backend-afklaring (1 uge wall-time, før migration-arbejde)

Inden DDL-skrivning skal det afklares om Supabase EU-region er tilladt jvf. Region's cloud-DB-politik (se GDPR-vurderingen ovenfor). Konkret:
1. Send forespørgsel til Region's databehandler-funktion med beskrivelse af data-indhold
2. Hvis OK → Supabase-spor; ellers MySQL-spor på din cloud-server

Denne afklaring koster intet at starte og frigør resten af planen til at vælge optimal teknisk vej.

### Trin 0b: Skema-introspection (KUN PÅ WINDOWS, 1 dag)

**Hvorfor:** Hverken bfh_dataportal eller BFHddl har komplet skema-indblik — begge bruger Access som blackbox via ODBC og henter kun de tabeller/kolonner de skal bruge. dm-modellen i [01_import_indhold_dm.R:143-147](01_import_indhold_dm.R) eksponerer kun 7 FK-relationer, men Access kan have flere relations, indexes, defaults og validation-rules vi ikke kender. Migration uden komplet introspection risikerer at miste data-integritets-regler.

**Skal ske på Windows** fordi MS Access ODBC-driveren kun findes til Windows. Outputtet (YAML + SQL + screenshots) er portabelt og bruges senere på Mac.

**Konkrete leverancer fra Windows:**

1. **`migration/access_schema.yaml`** — genereret af et R-script (`migration/00_introspect_access.R`) der via ODBC catalog-funktioner trækker:
   - Alle tabeller (inkl. system-tabeller hvis tilladt)
   - Kolonner pr. tabel: navn, type, størrelse, nullable, default value
   - Primary keys
   - Foreign keys (via `MSysRelationships` hvis muligt)
   - Indexes
   - Row counts pr. tabel
   - Sample-rækker (5 pr. tabel) til UTF-8-validering
2. **`migration/access_relationships.png`** — screenshot af Access' "Relationships"-diagram (Database Tools → Relationships)
3. **`migration/access_database_documenter.pdf`** — komplet skemarapport via Access' Database Documenter (Database Tools → Database Documenter, vælg alle tabeller, eksportér som PDF)
4. **`migration/access_data_dump/<tabel>.parquet`** — alle data fra alle tabeller eksporteret som Parquet (portabelt format, læses cross-platform med `arrow`)

**Hvorfor disse formater:**
- YAML/JSON for skema-metadata: human-readable + parseable på enhver platform
- PNG/PDF for Access-specifikke visninger der ikke har struktureret alternativ
- Parquet for data: stort kompakt, behold typer korrekt (inkl. UTF-8), læses uden Access-driver

**Commit eller upload til OneDrive** — disse artefakter er kilden til sandhed for resten af migrationen og skal være tilgængelige fra Mac.

### Trin 1: Skemaspecifikation (1-2 dage) — KAN KØRE PÅ MAC

Generér DDL-script fra Access-skemaet ved at:

1. Læse alle 22 tabeller fra Access via `DBI::dbListTables()` + `dbListFields()` + `dbColumnInfo()`
2. Mappe Access-datatyper → target-typer:
   - **Postgres/Supabase:** `COUNTER` → `SERIAL`/`IDENTITY`; `BIT` → `BOOLEAN`; `MEMO` → `TEXT`; `DATETIME` → `TIMESTAMP`
   - **MySQL:** `COUNTER` → `INT AUTO_INCREMENT`; `BIT` → `TINYINT(1)`; `MEMO` → `TEXT`; `DATETIME` → `DATETIME`
3. Beholde alle kolonne-navne (også med æ/ø/å):
   - **Postgres:** double-quote-citater (`"nøgleindikator"`)
   - **MySQL:** backticks (`` `nøgleindikator` ``) + `CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci`
4. Tilføje PRIMARY KEY constraints (Access-PKs)
5. Tilføje FOREIGN KEY constraints baseret på `dm_add_fk`-relationerne i [01_import_indhold_dm.R:143-147](01_import_indhold_dm.R)

Output: `migration/01_schema.sql` til valgt target. R-script kan generere begge varianter parallelt under afklaring.

### Trin 2: Data-import til target (1 dag) — KAN KØRE PÅ MAC

Et R-script (`migration/02_migrate_data.R`) der:

1. Læser Parquet-dumps fra Trin 0b via `arrow::read_parquet("migration/access_data_dump/tblXxx.parquet")`
   — IKKE direkte fra Access (det er allerede sket på Windows-trinnet)
2. Forbinder til target (`RPostgres::Postgres()` for Supabase, `RMariaDB::MariaDB()` for MySQL)
3. For hver tabel:
   - Læs Parquet → tibble
   - `DBI::dbWriteTable(target_con, "tblXxx", df, append = TRUE)`
4. Deaktiverer FKs midlertidigt under load:
   - **Postgres:** `SET session_replication_role = 'replica';`
   - **MySQL:** `SET FOREIGN_KEY_CHECKS = 0;`
5. Genaktiverer FKs og kører integrity-tjek
6. Sammenligner row counts pr. tabel mellem Parquet-dumps og target

**Fordel ved Parquet-mellemtrin:**
- Mac behøver ikke Access ODBC-driver
- Re-runs er hurtige (Parquet er kompakt og hurtigt at læse)
- Type-bevarelse er bedre end CSV (UTF-8 strenge, datetime, bool — alt korrekt)
- Idempotent: dump fra Trin 0b kan genbruges flere gange ved fejl

**UTF-8-tjek særligt vigtigt på MySQL:** kør sample-query med `SELECT * FROM tblPersoner WHERE efternavn LIKE '%æ%' OR efternavn LIKE '%ø%'` for at validere encoding.

### Trin 3: Opdater consumers (2-3 dage)

#### `bfh_dataportal`

Påvirkede filer:
- [01_import_indhold_dm.R:14-17](01_import_indhold_dm.R) — `pool::dbPool(odbc::odbc(), dsn = "dataportal_2024", encoding = "UTF8")` → ny driver:
  - Supabase: `pool::dbPool(RPostgres::Postgres(), host=<supabase-host>, dbname="postgres", user=..., password=..., sslmode="require")`
  - MySQL: `pool::dbPool(RMariaDB::MariaDB(), host=..., dbname=..., user=..., password=...)`
- [pbi/03_importer_pbi_dashboards.R](pbi/03_importer_pbi_dashboards.R) — flere `dbConnect(odbc::odbc(), dsn = ...)`-kald skiftes tilsvarende
- Fjern `@@IDENTITY`-pattern → brug `RETURNING id` (Postgres) eller `LAST_INSERT_ID()` (MySQL)
- Fjern `[bracket]`-syntax → erstattes med target-specifik citation
- Skift `True`/`False` → `TRUE`/`FALSE` (begge targets understøtter dette)

Connection-strings (host, user, password) flyttes til `config.yml` (eksisterende pattern) under nye nøgler, så Access-config kan beholdes som fallback i overgangsperioden. **Adgangskoder håndteres via `.Renviron`**, ikke i `config.yml` (følg `SECURITY_BEST_PRACTICES.md`).

#### `BFHddl`

- Tilsvarende driver-skifte i config; READ-only operationer kræver ingen anden tilpasning.

### Trin 4: Parallel-drift (1 uge)

Behold Access-DB intact. Læg en manuel re-sync-procedure der dagligt:
- Eksporterer Access-tabeller
- Genimporterer til target (med `truncate + insert` eller `upsert` afhængigt af tabel)
- Logger forskelle

Dette giver sikkerhedsnet under tilvænning. CRUD-skrivninger via bfh_dataportal går til **kun target** i denne periode (ikke dual-write — det bliver hurtigt komplekst); periodisk eksport fra target til Access-format som backup.

### Trin 5: Cutover (1 dag)

- Stop sync fra target → Access
- Arkivér Access-DB (read-only kopi i SharePoint/backup)
- Dokumentér ny connection-info i `CLAUDE.md` for begge projekter
- (Supabase) Tag baseline-snapshot via dashboard; aktivér PITR hvis Pro-tier

---

## Shiny CRUD-app (Fase 2)

Separat projekt, ikke del af Fase 1. Skitse:

- **Nyt repo:** `bfh_dataportal_admin/` (eller lignende)
- **Stack:** `shiny` + `bslib` + `DBI` + (driver: `RPostgres` eller `RMariaDB`) + `pool` + `DT` (editable tables)
- **Auth:**
  - **Supabase-spor:** brug Supabase Auth (GoTrue + JWT) — Shiny app validerer JWT via `httr2`; konsistent identity-model mellem direkte dashboard-edit og Shiny CRUD
  - **MySQL-spor:** `shinymanager` med Region-mailadresser eller SSO-integration hvis muligt
- **Funktioner:**
  - List/filter/søg på alle 22 tabeller
  - Inline-edit på de fleste felter (`DT::editable`)
  - Audit-log (hvem ændrede hvad, hvornår — kan implementeres som trigger på alle tabeller)
  - Validering før commit (fx FK-tjek inden DELETE)
  - Eksport/import af YAML for PBI-dashboards
- **Hosting:**
  - **Supabase-spor:** Edge functions kan håndtere noget logik; Shiny app på RConnect/ShinyProxy/Hugging Face Spaces
  - **MySQL-spor:** Shiny på samme cloud-server som DB (minimerer latency)

**Bonus ved Supabase-spor:** Supabase' indbyggede Table Editor i dashboardet kan bruges interim som "v0 CRUD" mens Shiny-appen bygges — det reducerer Fase 2's afhængighed.

Fase 2-arbejdet kan delvist erstatte den nuværende manuelle Access-redigering, hvilket reducerer afhængigheden af Windows + Access-klienten.

---

## Filer berørt (Fase 1)

**Arbejdsmappe (UDEN FOR repo'er):** `~/bfh_db_migration/` — overføres mellem Windows og Mac via ZIP, IKKE git.

**Windows-genereret (Trin 0b):**

| Fil | Type | Beskrivelse |
|-----|------|-------------|
| `~/bfh_db_migration/00_introspect_access.R` | Ny | R-script der trækker fuld skema-info via ODBC catalog |
| `~/bfh_db_migration/access_schema.yaml` | Ny (output) | Komplet skema: tabeller, kolonner, types, PKs, FKs, indexes |
| `~/bfh_db_migration/access_relationships.png` | Ny (output) | Screenshot af Access' Relationships-view |
| `~/bfh_db_migration/access_database_documenter.pdf` | Ny (output) | Database Documenter-rapport |
| `~/bfh_db_migration/access_data_dump/<tabel>.parquet` | Ny (output) | Alle data fra hver tabel som Parquet |

**Mac-genereret/redigeret (Trin 1+):**

| Fil | Type | Ændring |
|-----|------|---------|
| `~/bfh_db_migration/01_generate_ddl.R` | Ny | Genererer 01_schema.sql fra access_schema.yaml |
| `~/bfh_db_migration/01_schema.sql` | Ny (output) | Postgres DDL — alle 22 tabeller + constraints |
| `~/bfh_db_migration/02_migrate_data.R` | Ny | Læser Parquet-dumps → Supabase via RPostgres |
| `~/bfh_db_migration/03_verify.R` | Ny | Sammenlign row counts + sample data + FK-integritet |
| `~/bfh_db_migration/config.yml` | Ny | Connection-info (host, dbname, ssl); credentials i `.Renviron` |
| `~/bfh_db_migration/.Renviron.example` | Ny | Skabelon for credentials (gitignored på Mac, ikke en del af ZIP) |
| `~/bfh_db_migration/renv.lock` | Ny | R-pakke-versioner for repro |
| `~/bfh_db_migration/PLAN.md` | Ny | Kopi af denne plan-fil for portabel reference |
| `~/bfh_db_migration/README.md` | Ny | Quick-start til Mac-bruger (efter unpack af ZIP) |

**Consumer-projekter (modificeres i deres egne git-repo'er — IKKE i migration-arbejdsmappen):**

| Fil | Type | Ændring |
|-----|------|---------|
| `bfh_dataportal/01_import_indhold_dm.R` | Modificér | Skift `pool::dbPool` til Supabase via `RPostgres::Postgres()` |
| `bfh_dataportal/pbi/03_importer_pbi_dashboards.R` | Modificér | Skift `dbConnect`, fjern `@@IDENTITY`, `[bracket]` |
| `bfh_dataportal/config.yml` | Modificér | Tilføj Supabase connection-info |
| `BFHddl/R/<connection-fil>` | Modificér | Skift driver til `RPostgres::Postgres()` |
| `BFHddl/config.yml` | Modificér | Tilføj Supabase connection-info |
| `bfh_dataportal/CLAUDE.md` + `BFHddl/CLAUDE.md` | Modificér | Dokumentér Supabase som backend |

## Eksisterende kode der genbruges

- **`pool::dbPool`** — eksisterende pattern bevares, kun driver+args ændres
- **`dm`-pakken** + dm_add_fk-relations ([01_import_indhold_dm.R:107-147](01_import_indhold_dm.R)) — fungerer identisk mod Postgres
- **`config::get()` + `config.yml`** — eksisterende pattern; nye config-nøgler tilføjes
- **`janitor::clean_names()`** — bevarer kolonne-navne ens på tværs af backends
- **Access-skemaet** — ingen logisk redesign, kun teknisk oversættelse

---

## Verifikation

End-to-end test efter migration:

1. **Skema-tjek:** `psql -c "\dt"` + `\d tblIndikatorer` viser samme struktur som Access
2. **Row count match:** For hver af de 22 tabeller, sammenlign `SELECT COUNT(*)` mellem Access og Postgres
3. **FK-integritet:** Postgres vil afvise ugyldige FKs ved enable; ingen rejected rows tilladt
4. **bfh_dataportal pipeline:** Kør `source("00_generer_alt.R")` på Postgres — alle 3 trin skal færdiggøre uden fejl og producere identisk `.qmd`-output som før migration (sammenlign et sample fil)
5. **BFHddl pipeline:** Kør én SPC-batch-produktion — output PNG'er skal være identiske med pre-migration
6. **CRUD-test:** Manuelt INSERT/UPDATE/DELETE på `tblIndikatorer` via `psql` eller `DBeaver` — tjek at PBI-import respekterer manuelle ændringer (samme idempotency-egenskaber som før)
7. **Performance:** Tid `00_generer_alt.R` end-to-end før/efter migration — Postgres bør være ≥ Access (sandsynligvis hurtigere)

---

## Risici + mitigationer

| Risiko | Sandsynlighed | Mitigering |
|--------|---------------|------------|
| Access-data har "skjulte" relationer/constraints ikke afspejlet i R-kode | Medium | Eksportér Access-relations via Access' interface inden migration; sammenlign med dm-relations |
| Danske tegn-kolonner kræver special-håndtering | Lav | Postgres: `"nøgleindikator"`; MySQL: `` `nøgleindikator` `` + utf8mb4 |
| `@@IDENTITY`-brug findes flere steder end forventet | Medium | Grep efter `@@IDENTITY` på tværs af projekter; refaktor til `RETURNING id` / `LAST_INSERT_ID()` |
| Region afviser Supabase pga. cloud-policy | Medium-Høj | Fald tilbage til MySQL-spor; ingen omkostning bortset fra ekstra ops-arbejde |
| Supabase Free tier paused ved inaktivitet (7 dage) | Lav (kun ved langvarig pause) | Skift til Pro-tier ($25/mo) før produktion, eller hold månedlig aktivitet |
| MySQL-cloud-server går ned uden backup | Medium (hvis MySQL-spor) | Opsæt `mysqldump`-cron til ekstern lokation FØR migration. Test restore. |
| Data-souverænitet ved Supabase (medarbejder-PII i tblPersoner) | Medium | EU-region + databehandleraftale; minimér PII; afklar med Region |
| BFHddl-pipeline går i stykker pga. subtile data-type-ændringer | Lav | Trin 4 (parallel-drift) fanger dette inden cutover |
| Encoding-issues ved data-overførsel | Lav (Postgres) / Medium (MySQL) | Eksplicit UTF-8/utf8mb4 i begge ender; tjek æ/ø/å i sample-data efter migration |
| Vendor lock-in ved Supabase | Lav | Ren Postgres under — kan altid eksporteres via `pg_dump` og flyttes til self-hosted eller MySQL senere |

## Open questions (skal afklares før eller under Fase 1)

1. **Region's cloud-DB-politik** (KRITISK før Trin 1): Er Supabase EU-region tilladt jvf. Region's databehandleraftaler? Dette afgør Supabase- vs MySQL-spor og bør afklares FØRST.
2. **Backup-strategi:**
   - Supabase: indbygget — afklar om Free tier-backup er tilstrækkeligt eller Pro-tier nødvendig
   - MySQL: opsætning af automatisk `mysqldump` til SharePoint eller anden backup-lokation
3. **Auth-model for fremtidig CRUD:**
   - Supabase: GoTrue-auth, evt. SSO mod Region-AD
   - MySQL: shinymanager med separat brugerliste, eller SSO
4. **Diagram-tabellerne** (tblDiagrammer*, tblDataprodukter) — hvilken kode skriver/læser disse? Måske BFHddl eller andre projekter har specifik logik der skal verificeres efter migration.
5. **MySQL-cloud-server specs** (kun hvis MySQL-spor): hvilken version (8.0+ anbefales), `utf8mb4`-default, SSL/TLS påkrævet for connections.
