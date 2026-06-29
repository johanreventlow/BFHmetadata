# Design: Oversigtstabel + modal-redigering med m2m-relationer

**Dato:** 2026-06-08
**Status:** Godkendt (afventer spec-review)
**Modul:** `mod_indikator_crud`
**Bygger videre på:** `2026-06-07-indikator-crud-design.md` (v0 CRUD)

## Formål

Tilføj en kompakt, læsevenlig oversigt over indikatorer plus en modal til
fuld redigering — inkl. many-to-many-relationer vist med tekst-værdier i
stedet for rå id'er. Validerer backend videre før consumer-skifte (Trin 3).

## Krav (fra bruger)

1. Ny kompakt oversigtstabel **ud over** den eksisterende inline-redigerbare tabel.
2. Oversigtskolonner: aktiv-status, "tilhørende datasæt" (= indikatorhierarki-
   placering), indikator-id, indikator-navn.
3. Per-række knap der åbner indikator til redigering.
4. Redigering foregår i **modal dialog** med fuld adgang til alle felter + relationer.
5. Relationer vises med tilhørende **tekst-værdi**, ikke kun unikt id.
6. Redigerbare relationer inkluderer m2m-junctions (faggrupper, dataprodukter,
   organisation).

## Beslutninger

| Spørgsmål | Valg |
|-----------|------|
| "Datasæt"-kolonne | `indikator_hierarki` FK → `hierarki_navn` (allerede resolvet som `label_indikator_hierarki`) |
| Layout | To faner (`bslib::navset_tab`): "Oversigt" + "Inline-redigering" |
| Modal-scope | Alle 21 felter + 3 direkte FK + 3 m2m-junctions |
| Organisation-label | `COALESCE(organisatorisk_navn_langt, organisatorisk_navn_teknisk)` |
| M2m-skrivestrategi | Replace (slet alle for indikator → genindsæt valgte) i transaktion |

## Arkitektur

### UI-struktur (`R/mod_indikator_crud.R`)

`mod_indikator_crud_ui` ombygges til `bslib::navset_tab`:

- **Fane "Oversigt"**: kompakt `DT::DTOutput` — kolonner `aktiv_indikator`,
  `label_indikator_hierarki` (vist-header "Datasæt"), `id`, `indikator_navn`,
  samt per-række **[Åbn]**-knap. `show_inactive`-checkbox bevares.
- **Fane "Inline-redigering"**: eksisterende inline-DT + sidebar-form, uændret.

Begge faner læser **samme** `rows()` reactiveVal. Modal-gem kalder eksisterende
`reload()` → begge faner opdateres.

### [Åbn]-knap

HTML-knap renderet i DT-kolonne (`escape = FALSE`). onclick sætter Shiny-input:

```r
sprintf(
  '<button class="btn btn-sm btn-primary" onclick="Shiny.setInputValue(\'%s\', %d, {priority:\'event\'})">Åbn</button>',
  session$ns("open_id"), id
)
```

Namespace-prefix bygges med `session$ns()` (kritisk — typisk fejlkilde).
`observeEvent(input$open_id, ...)` åbner modal for den valgte id.

### Modal (`showModal` + `modalDialog`)

Indhold:

- **21 skalarfelter + 3 direkte FK**: genbruger `.field_input()` + `fk_choices`
  (FK-dropdowns med tekst-labels — allerede implementeret i v0).
- **3 m2m-multiselect** (`selectInput(multiple = TRUE)`) med tekst-labels:
  - Faggrupper → `tblFaggrupper.faggruppe`
  - Dataprodukter → `tblDataprodukter.dataprodukt_navn`
  - Organisation → `COALESCE(organisatorisk_navn_langt, organisatorisk_navn_teknisk)`

Pre-udfyldning ved åbning:
- Skalar/FK-felter fra den valgte `rows()`-række.
- M2m fra DB: `get_junction(indikator_id, junction_key)`.

Knapper: **Gem** (validér → `update_indikator` + 3× `set_junction`),
**Annullér** (`removeModal`).

### Datalag

**Metadata (`R/metadata.R`)** — ny konstant:

```r
INDIKATOR_JUNCTIONS <- list(
  faggrupper    = list(table = "tblForbindIndikatorerFaggrupper",
                       fk = "faggruppe_id",   parent = "tblFaggrupper",
                       parent_pk = "Id", label = "\"faggruppe\""),
  dataprodukter = list(table = "tblForbindIndikatorerDataprodukter",
                       fk = "dataprodukt_id", parent = "tblDataprodukter",
                       parent_pk = "Id", label = "\"dataprodukt_navn\""),
  organisation  = list(table = "tblForbindIndikatorerOrganisation",
                       fk = "organisations_id", parent = "tblOrganisationStruktur",
                       parent_pk = "Id",
                       label = "COALESCE(\"organisatorisk_navn_langt\",\"organisatorisk_navn_teknisk\")")
)
```

**Rene SQL-byggere (`R/fct_sql.R`)** — testbare uden DB:

- `build_junction_select_sql(j)` → `SELECT "<fk>" FROM "<table>" WHERE "indikator_id" = $1`
- `build_junction_delete_sql(j)` → `DELETE FROM "<table>" WHERE "indikator_id" = $1`
- `build_junction_insert_sql(j, n)` → multi-row `INSERT INTO "<table>"
  ("indikator_id","<fk>") VALUES ($1,$2),($1,$3),...` for `n` parent-ids
  (`$1` = indikator_id genbrugt, `$2..$(n+1)` = parent-ids)
- `build_junction_options_sql(j)` → `SELECT "<parent_pk>" AS id, (<label>) AS label
  FROM "<parent>" ORDER BY 2`

**Accessors (`R/fct_db.R`)** — tilføjes til `make_db()`:

```r
get_junction = function(indikator_id, key) {
  j <- INDIKATOR_JUNCTIONS[[key]]
  DBI::dbGetQuery(pool, build_junction_select_sql(j), params = list(indikator_id))[[j$fk]]
},
junction_options = function(key) {
  j <- INDIKATOR_JUNCTIONS[[key]]
  DBI::dbGetQuery(pool, build_junction_options_sql(j))
},
set_junction = function(indikator_id, key, parent_ids) {
  assert_write_enabled()
  j <- INDIKATOR_JUNCTIONS[[key]]
  pool::poolWithTransaction(pool, function(conn) {
    DBI::dbExecute(conn, build_junction_delete_sql(j), params = list(indikator_id))
    if (length(parent_ids)) {
      DBI::dbExecute(conn, build_junction_insert_sql(j, length(parent_ids)),
                     params = c(list(indikator_id), as.list(parent_ids)))
    }
  })
}
```

## Dataflow

```
[Åbn]-klik → input$open_id (id)
  → observeEvent: hent række fra rows(), hent m2m via get_junction() ×3
  → showModal med pre-udfyldt form + multiselects
  → Gem-klik:
       validate_indikator(skalar-værdier)  [fejl → vis i modal, behold åben]
       update_indikator(id, skalar+FK)
       set_junction(id, key, valgte) ×3
       removeModal(); reload()  → begge faner opdateres
```

## M2m replace-semantik — tabsanalyse

Junction-tabellerne indeholder **kun** parret (`indikator_id`, `parent_id`) —
ingen payload-kolonner (verificeret mod `access_schema.yaml` 2026-06-08), ingen
PK/identity. Rækken **er** relationen. Slet+genindsæt af samme par er
bit-identisk → intet data-/informationstab. Replace og diff giver identisk
slut-tilstand.

**Edge cases:**
- *Concurrency:* samtidig redigering af samme indikators relationer →
  last-write-wins. Lav risiko (lokalt single-user admin-værktøj uden auth).
  Diff løser ikke uden låsning.
- *Atomicitet:* hele replace kører i `poolWithTransaction` på én connection →
  rollback ved fejl, ingen halvt-slettet tilstand.

> **Forudsætning:** Hvis junction-tabeller senere får payload-kolonner
> (timestamp, note, mv.), skal replace-strategien genovervejes til diff.

## Fejlhåndtering

- Skrivninger guardes af eksisterende `assert_write_enabled()` (BFHMETA_WRITE=1).
- `safe_operation()` om alle DB-kald i modulet (logger + fallback-besked).
- Validering i modal: ved fejl vises besked, modal forbliver åben (ingen reload).

## Test (TDD)

| Hvad | Hvordan | Fil |
|------|---------|-----|
| Junction SQL-byggere | Pure unit-tests (forventet SQL-streng) | `tests/testthat/test-fct_sql.R` |
| `set_junction` replace-roundtrip | fake_db: slet+indsæt → korrekt slut-sæt | `tests/testthat/test-db.R` |
| `set_junction` rollback på fejl | fake_db: fejlende insert → uændret tilstand | `tests/testthat/test-db.R` |
| Modal-flow | testServer: `open_id` → modal-state; Gem → reload | `tests/testthat/test-mod_indikator_crud.R` |
| Tom selektion | `set_junction(id, key, integer(0))` → kun delete | `tests/testthat/test-db.R` |

`fake_db` udvides med `get_junction`, `junction_options`, `set_junction`.

## Scope & versionering

Markant større end v0: 3 junctions × (select/insert/delete/options) + modal +
fane + tests. Version-bump til **0.2.0** (`feat:`, pre-1.0 MINOR).

## Ikke i scope (YAGNI)

- Diff-baseret m2m-skrivning (replace dækker behovet).
- Optimistisk concurrency / låsning.
- Inline-redigering af m2m i oversigtstabellen (kun via modal).
- Redigering af parent-tabellerne selv (kun valg blandt eksisterende).
