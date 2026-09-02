# Bulk-redigering af flere poster i indikator- og diagramtabellen

**Dato:** 2026-08-30
**Status:** Plan/design — ikke implementeret
**Bygger på:** `docs/superpowers/specs/2026-08-21-origin-main-hardening-design.md`
(afsnittet "Bulk-redigering og fortryd" / Leverance 4) og Fase 0-proben
`dev/bulk_probe.R`.

## Formål

Brugeren skal kunne ændre det samme felt på mange poster på én gang i både
indikatortabellen (`mod_indikator_crud`) og diagramtabellen (`mod_diagram`) —
fx "sæt Kontaktperson = X på disse 40 indikatorer" eller "deaktivér disse 25
diagrammer" — uden at klikke sig igennem rækkerne én ad gangen.

## Udgangspunkt (hvad findes i dag)

- Begge tabeller er excelR-grids med inline-redigering pr. celle. Hver ændret
  celle gemmes som en **selvstændig** UPDATE (`db$update_indikator` med ét
  felt; `db$update_diagram` med fuld-række-patch via `.diagram_row_values`).
- Grid-diffen (`excel_diff_cells`) kan allerede levere **flere** ændrede
  celler i ét payload (fx ved paste af en kolonne værdier), men de skrives
  i dag som N uafhængige commits — ingen atomicitet, ingen fortryd.
- Selektion er én række: `excel_selected_pk` læser kun
  `selectedDataBoundary$borderTop`. Payloadet indeholder dog også
  `borderBottom` (jexcel range-selektion), så multi-række-selektion er
  tilgængelig uden widget-ændringer.
- Hardening-designet har allerede **låst kravene** til bulk:
  - En batch er alt-eller-intet og skrives sammen med sin auditpost i samme
    PostgreSQL-transaktion (krav 6).
  - Fortryd må ikke overskrive nyere ændringer tavst (krav 7).
  - Browser-events må ikke direkte bestemme SQL-kolonner/rækker/typer (krav 5)
    → serverside bulk-allowlist + typekonvertering.
  - Audit i et ikke-eksponeret `audit`-schema; ingen grants til
    `anon`/`authenticated`.
- Fase 0 er kørt (`dev/bulk_probe.R`): RPostgres-bindingen af id-vektorer er
  verificeret (mønster B: dynamiske placeholders `id IN ($2,$3,…)`; alternativ
  array-literal `= ANY($1::int[])` virker også — sidstnævnte bruges allerede i
  `diagram_medians_batch`/`pg_int_array`), og `SELECT … FOR UPDATE` låser
  beviseligt mod en konkurrerende forbindelse.

## UX-design

To komplementære mekanismer, leveret i den rækkefølge:

### A. "Redigér valgte" — sæt ét felt på N rækker (primær leverance)

1. Brugeren markerer flere rækker i grid'et: klik + shift-klik / træk
   (sammenhængende range — det jexcel giver gratis). Supplerende knap
   **"Vælg alle viste"** vælger alle rækker i det aktuelle filterresultat,
   så filtrene bliver den naturlige måde at udpege store mængder på.
2. Toolbaren viser **"Redigér valgte (N)"**. Klik åbner en modal:
   - Dropdown "Felt" — kun felter fra den serverside bulk-allowlist.
   - Ét typet input for det valgte felt (genbrug af feltmetadata:
     checkbokse for bool, FK-dropdown med samme choices som grid'et,
     `OUTPUT_ENHED_CHOICES` for choice, tekstfelt for text).
   - **Forhåndsvisning**: kompakt tabel over de ramte rækker med
     `nuværende værdi → ny værdi` (rækker hvor værdien allerede er den
     ønskede markeres "uændret" og kan udelades af batchen).
   - Knappen "Skriv N ændringer" + Annullér.
3. Efter succes: statusbesked "Batch <kort-id>: N rækker opdateret" med en
   **"Fortryd"**-knap (notifikation/toolbar) der ruller netop denne batch
   tilbage.
4. "Deaktivér valgte" i indikatortabellen opgraderes fra én række til at
   virke på hele selektionen (det er reelt bulk-sæt af
   `aktiv_indikator = FALSE` og genbruger samme batch-flow inkl. fortryd).

### B. Atomisk multi-celle-paste (senere, valgfri leverance)

Diffen fra ét grid-payload med >1 ændret celle grupperes som **én batch**
(samme `batch_id`, én transaktion, per-celle-værdier) i stedet for dagens
løkke af enkelt-commits. Ingen ny UI — kun at paste/fill bliver atomisk og
fortrydbar. Kræver at ekko-værnet (`new_excel_echo_guard`) armeres med hele
batch-diffen som i dag.

### Fravalg i UX

- Ikke-sammenhængende selektion (cherry-picking på tværs af rækker) løses i
  første omgang med filtre + range. Hvis det viser sig utilstrækkeligt,
  tilføjes en dedikeret "Vælg"-checkbox-kolonne i grid'et — den skal i så
  fald ekskluderes fra persistens-diffen (ændringer i den kolonne er
  selektion, ikke data) og fra `excel_col_widths`-beregningen. Beslutning
  udskydes til efter brugerafprøvning af A.
- Bulk rammer kun **skalarfelter** på hovedtabellen. m2m-junctions
  (faggrupper/dataprodukter/organisation) og median-knæk er ikke omfattet
  (jf. hardening-designet).
- `indikator_navn_teknisk` kan aldrig bulk-ændres (parquet-nøgle, readonly
  overalt i dag).

## Arkitektur

Lagdelingen følger de eksisterende komponentgrænser (grid-adapter uden
DB-kendskab → modul → db-accessor → SQL-builder).

### 1. Grid-adapter (`R/fct_excel_table.R`)

Ny ren funktion:

```
excel_selected_pks(p)  # character-vektor af pk'er for rækkerne
                       # borderTop..borderBottom, læst fra payloadens
                       # fullData (robust under klient-sortering);
                       # NULL ved ugyldigt payload
```

`excel_selected_pk` beholdes (single-valg til "Åbn valgte"/slet) og kan
implementeres som `excel_selected_pks(p)[1]`. Modulernes selektions-state
(`tbl_sel`/`grid_sel`) udvides fra én pk til en vektor.

### 2. Bulk-allowlists (`R/metadata.R` eller `R/fct_sql.R`)

Statisk serverside-konfiguration pr. tabel — feltnavn → kolonne + kind:

- `BULK_INDIKATOR_FIELDS`: delmængde af `.INDIKATOR_GRID_FIELDS` +
  `tillad_auto_opdatering`. Typisk: de tre bool-flag, `indikator_hierarki`,
  `kontaktperson`, `datakilde`, `output_enhed`, `ønsket_tendens`, `mål`.
  (`indikator_navn` udelades — et fælles navn på N rækker giver ikke mening.)
- `BULK_DIAGRAM_FIELDS`: de fire bool-flag, `periode_aggregering`,
  `maalgruppe`, `diagram_type`. (`indikator` og
  `organisatorisk_navn_teknisk` udelades som bulk-mål i første omgang —
  at flytte N diagrammer til samme indikator/enhed kolliderer med
  duplikat-reglen og er sjældent intentionen.)

Feltet fra UI'et slås op i allowlisten; ukendt felt → afvis uden DB-kald.
Typekonvertering sker mod feltets deklarerede kind (genbrug af mønstrene i
`.IND_FK_FIELDS`/`.IND_BOOL_FIELDS` og diagram-modulets koercion).

### 3. SQL-buildere (`R/fct_sql.R`)

Rene funktioner + tests som de eksisterende buildere:

- `build_bulk_lock_sql(tabel, pk, felt)` —
  `SELECT pk, felt FROM tabel WHERE pk = ANY($1::int[]) ORDER BY pk FOR UPDATE`
  (array-literal-mønstret: én parameter uanset N; `pg_int_array` findes).
  Stabil ORDER BY-låserækkefølge forebygger deadlocks mellem to samtidige
  batches.
- `build_bulk_update_sql(tabel, pk, felt)` —
  `UPDATE tabel SET felt = $1 WHERE pk = ANY($2::int[])`.
- `build_audit_insert_sql(n)` — N audit-rækker i ét statement.

Tabel-/kolonnenavne kommer **kun** fra de statiske konfigurationer og
double-quotes som i de øvrige buildere.

### 4. Audit-schema (migration — gated)

Ny idempotent migration (køres efter migrationsporten i hardening-designet:
read-only introspektion, lokal succes- + rollback-rehearsal, særskilt
godkendelse før Supabase):

```
CREATE SCHEMA audit;                      -- ingen grants til anon/authenticated
CREATE TABLE audit.tbl_batch (
  batch_id      uuid PRIMARY KEY,
  tabel         text NOT NULL,
  felt          text NOT NULL,
  udfoert_ts    timestamptz NOT NULL DEFAULT now(),
  fortrudt_ts   timestamptz               -- NULL = ikke fortrudt
);
CREATE TABLE audit.tbl_batch_raekke (
  batch_id      uuid NOT NULL REFERENCES audit.tbl_batch,
  row_id        int  NOT NULL,
  vaerdi_foer   text,                     -- typed-as-text + felt-kind
  vaerdi_efter  text,
  PRIMARY KEY (batch_id, row_id)
);
```

Værdier gemmes som text sammen med feltets kind (kendt fra allowlisten), så
fortryd kan re-type deterministisk. Appen forbinder som `postgres`-rollen
(admin-tooling, jf. `db_connect`), så ingen nye grants er nødvendige.

### 5. DB-accessorer (`R/fct_db.R`)

```
bulk_update(tabel_key, ids, felt, vaerdi)   # → list(batch_id, n, skipped)
bulk_undo(batch_id)                         # → list(ok, konflikter)
list_recent_batches(tabel_key, n = 5)       # til evt. "seneste batches"-UI
```

`bulk_update`-flow i **én** `poolWithTransaction`:

1. `assert_write_enabled()`.
2. Slå `tabel_key`+`felt` op i bulk-allowlisten; konvertér værdien til
   feltets type (fejl → abort før transaktion).
3. Lås målrækkerne med `build_bulk_lock_sql` (ORDER BY pk). Afvis hele
   batchen hvis nogen id'er mangler eller er dubletter — med liste over
   hvilke ("rækken er slettet/uden for tabellen").
4. Sammenlign de låste førværdier med de førværdier UI'et viste
   (forhåndsvisningen sender dem med): afviger nogen, er grid'et stale →
   abort med konfliktrapport (krav 7-ånden også for selve batchen).
   Rækker hvor værdien allerede er målværdien udelades (rapporteres som
   "uændret").
5. Én UPDATE for alle resterende rækker + audit-header + audit-rækker med
   samme `batch_id`.
6. Commit kun hvis alt lykkes. Fejl hvor som helst → rollback af det hele.
7. Efter commit: cache-invalidering + `reload()` i modulet.

`bulk_undo`-flow (ny transaktion):

1. Læs batchens rækker fra audit; lås de samme målrækker (ORDER BY pk).
2. Kontrollér for **hver** række at nuværende værdi = `vaerdi_efter`.
   Én afvigelse → hele fortrydelsen afvises med konfliktrapport
   (række-id, forventet, faktisk). Ingen delvis fortryd.
3. Skriv `vaerdi_foer` tilbage (re-typet efter feltets kind), sæt
   `fortrudt_ts`, commit.

### 6. Moduler (`mod_indikator_crud`, `mod_diagram`)

Fælles mønster (kandidat til en delt hjælper, fx `R/fct_bulk.R` +
`mod_bulk_modal`-agtige rene funktioner):

- Selektions-state: `reactiveVal(character())` med pk-vektor; sæt fra
  `excel_selected_pks` på selektions-payloads. Knaptekst opdateres
  reaktivt ("Redigér valgte (3)").
- "Vælg alle viste": sætter selektionen til `tbl_rows()`/`filtered()`s
  pk'er (server-side — uafhængig af grid-scroll).
- Bulk-modal: felt-dropdown → `renderUI` af det typede input →
  forhåndsvisningstabel (gammel → ny, med "uændret"-markering) →
  bekræft. Pk-sættet og førværdierne **fryses ved modal-åbning**
  (samme princip som `pending_soft_delete_id`), så filterskift bag
  modalen ikke ændrer målsættet.
- Diagram-specifikt: før batchen valideres hver ramt række som patched
  fuld række (`.diagram_row_values` + `validate_diagram`) — fx må bulk-sæt
  af `maalgruppe` ikke kunne "reparere" en række med manglende FK'er
  tavst; valideringsfejl lister rækkerne og stopper hele batchen.
  Duplikat-guarden (`diagram_duplicate_count`) køres kun hvis feltet
  indgår i dubletnøglen og rapporteres samlet som advarsel (blokerer ikke,
  som i dag).
- Indikator-specifikt: "Deaktivér valgte" flyttes over på batch-flowet
  (bulk-sæt `aktiv_indikator = FALSE`) — bekræftelsesmodalen viser N.
- Fortryd: efter succes vises notifikation med handlings-knap; seneste
  `batch_id` holdes i session-state. Konflikt ved fortryd → modal med
  rapporten.
- Fejl-/statusbeskeder: korte danske beskeder via eksisterende
  `status_msg`/`warn_msg` + `safe_operation`; DB-udfald må aldrig vælte
  sessionen (eksisterende mønster).

## Leverancer og rækkefølge

Hver leverance er selvstændigt releasebar og TDD-drevet (fokuserede tests →
fuld `devtools::test()`).

1. **Multi-selektion i grid-adapteren** (ingen DB-ændring):
   `excel_selected_pks` + vektor-selektions-state + "Redigér valgte (N)"-
   knap (disabled/skjult funktionalitet indtil leverance 3) + "Vælg alle
   viste". Unit-tests på payload-former (range, sorteret grid, stale pk'er).
2. **Batch-kontrakt i DB-laget**: allowlists, SQL-buildere,
   `bulk_update`/`bulk_undo` med transaktions- og konfliktlogik.
   Integrationstests (BFHMETA_WRITE-gated som de eksisterende) mod en
   engangstabel efter `bulk_probe`-mønstret, inkl. **tvungen fejl midt i
   batchen** med bevis for fuld rollback, og fortryd-konflikt-casen.
3. **Audit-migration**: idempotent migration + migrationsport-dokumentation
   (introspektion, rehearsal, godkendelse). Leverance 2's accessorer peger
   først på audit-tabellerne her; indtil da kører integrationstests mod
   test-udgaver af tabellerne.
4. **Indikator-UI**: bulk-modal, forhåndsvisning, frosset målsæt, fortryd,
   "Deaktivér valgte (N)". `shiny::testServer`-tests + manuel browserprøve.
5. **Diagram-UI**: samme flow + per-række fuldvalidering og samlet
   duplikat-advarsel. Overvej her at udtrække den delte modal-hjælper.
6. **(Valgfri) Atomisk paste**: multi-celle-diff som batch, samspil med
   ekko-værn. Kun hvis A-flowet viser behovet.

## Risici og åbne beslutninger

| Emne | Vurdering |
|---|---|
| Kun sammenhængende range-selektion | Accepteret i v1; filtre + "Vælg alle viste" dækker de store cases. Checkbox-kolonne som fallback-beslutning efter brugerafprøvning. |
| Ingen versionskolonne i tabellerne | Konfliktdetektion sker via førværdi-sammenligning under `FOR UPDATE` (batch) og `vaerdi_efter`-sammenligning (fortryd) — dækker kravet uden schemaændring på domænetabellerne. |
| To brugere bulk'er samtidigt | Stabil ORDER BY-låserækkefølge forebygger deadlock; taberen får konfliktrapport i stedet for tavs overskrivning. |
| Store batches (alle ~600+ rækker) | Én UPDATE + N audit-rækker er billigt; forhåndsvisningen kan paginere/opsummere ved N > ~50 ("viser de første 50 af 320"). |
| Fortryd af gamle batches | v1 tilbyder kun "seneste batch" i UI; audit gemmer alt, så en "seneste batches"-liste kan tilføjes senere uden datamodel-ændring. |

## Manuel browser-verifikation (minimum)

1. Range-selektion + "Redigér valgte" på tværs af klient-sorteret grid
   (pk-korrekthed, ikke position).
2. Bulk-sæt af bool, FK og choice i begge tabeller; forhåndsvisningens
   gammel→ny stemmer med databasen efter reload.
3. Batch med én ugyldig række (fx slettet i anden session) → intet skrevet,
   forståelig konfliktrapport.
4. Fortryd umiddelbart efter batch; fortryd efter at én række er ændret
   manuelt → afvisning med rapport.
5. Filterskift mens bulk-modalen er åben ændrer ikke målsættet.
