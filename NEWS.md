# BFHmetadata (development)

## Nye features
* Signal-gennemgang er markant hurtigere, og man kan arbejde næsten med det
  samme: diagrammer scannes pr. indikator (ét parquet-load deles af alle
  diagrammer på samme indikator), resultater vises løbende mens scannet kører
  (progressivt scan med Stop-knap), og indlæste indikator-slices gemmes i en
  dags-cache på disk, så gentagne scans samme dag springer parquet-lageret
  helt over. Ny afkrydsning "Ignorér dags-cache" tvinger genindlæsning.
  Baggrund: parquet-lageret består af ~172k bittesmå dags-partitionsfiler,
  hvor åbne-omkostningen dominerer totalt over datamængden.
* Appen åbner hurtigere: en fanes data hentes først når fanen åbnes (før
  hentede opstarten alle modulers referencedata), og referencedata som
  dropdown-lister og diagram-indeks genbruges i stedet for at blive hentet
  forfra hver gang — også når en modal åbnes. Egne ændringer slår igennem
  med det samme, også på tværs af faner.
* Signal-scan henter alle median-knæk i ét databasekald i stedet for ét pr.
  diagram (ved ~600 diagrammer sparer det ~600 forespørgsler til Supabase).
* Kompaktering af parquet-lageret direkte fra appen: ved opstart tilbyder en
  dialog at samle hver indikators mange dagsfiler i én fil i et delt
  _compact/-spejl i lageret — så bliver også dagens FØRSTE scan hurtigt, og
  alle brugere (og BFHddl-pipelinen) deler gevinsten. Kompakteringen kører i
  baggrunden med statusvisning, kan afbrydes, og spejlet tages kun i brug
  når det er kompakteret i dag — ellers læses der råt som hidtil.
* Parquet-mappen huskes mellem sessioner og er forudfyldt i sidefeltet.
* Kompaktering og cache styres nu af kildens FINGERAFTRYK i stedet for
  kalenderdag: appen opdager selv, hvilke indikatorer der har fået nye eller
  ændrede data, og tilbyder kun at kompaktere netop dem (sjældent opdaterede
  datasæt som SundK røres aldrig). Spejl og cache for uændrede indikatorer
  forbliver gyldige på tværs af dage, og regenererer man data flere gange
  samme dag, læses de nye data automatisk — uden at røre "Ignorér cache".
  Dialogen ved opstart vises kun, når der faktisk ER noget at kompakte, og
  en ny knap på startsiden ("Tjek og kompaktér parquet-lager") kører samme
  tjek on demand. Kendt blindvinkel: ændringer der KUN rører gammel historik
  (uden nye/ændrede seneste dage) opdages ikke — brug force-refresh eller
  knappen efter manuel historik-omskrivning.

## Interne ændringer
* Fingeraftryk (source_fingerprint): 1 readdir + stats på nyeste K
  partitioner (målt: 0,1 s for største indikator med 7.097 partitioner;
  fuld sweep ~10-20 s, kørt chunket i baggrunden). Manifest v2 med
  per-indikator entries (fingerprint + compacted_at); v1-manifester
  migreres ved at alt regnes som ændret én gang. run_compaction() er
  inkrementel og merger uændrede entries; RDS-cachens nøgle bruger
  fingeraftryk i stedet for dato.
* Nyt dags-cache-lag (fct_cache.R): én RDS pr. indikator pr. dag under
  R_user_dir (overstyrbar via option bfhmeta.cache_dir), NULL caches aldrig,
  korrupte filer ignoreres, auto-prune efter 7 dage. scan_diagram() kan
  modtage et preloadet slice via slice_loader (per-indikator-genbrug).
  Scan-skedulering er injicérbar (option bfhmeta.scan_scheduler) af
  testhensyn. Nye Imports: rlang, later.
* Nyt app-cache-lag (fct_db_cache.R): make_db_cached() memoiserer
  read-mostly-accessors i et delt session-lager (nøgle = accessor-navn +
  argumenter), rydder ved enhver skrivning og videresender ukendte
  accessors uændret. Nyt lazy-init-lag (fct_lazy.R): lazy_module()
  registrerer et moduls server-funktion ved første besøg på fanen.
  Nyt batch-opslag af median-knæk (build_median_batch_sql +
  medians_by_diagram) med fallback til per-diagram ved fejl.

# BFHmetadata 0.7.0

## Nye features
* Diagram-CRUD: ny "Diagrammer"-fane med filterbar oversigt over ALLE
  diagrammer (indikator, enhed, status, type) og formular-modal til
  opret/redigér/slet. Blød duplikat-advarsel ved samme indikator/enhed/type.
* Diagrammer kan også redigeres direkte fra indikator-modalen: ny
  "Diagrammer"-sektion viser indikatorens diagrammer, og formular åbnes med
  indikator forudfyldt og låst (retur til indikator-modalen efter gem).
* Slet-guard: diagrammer med median-knæk kan ikke slettes — venlig besked
  foreslår deaktivering eller sletning af knækkene først.

# BFHmetadata 0.6.0

## Nye features
* Organisations-oversættelse (tblOrganisationOversaettelse) kan nu redigeres
  i appen som opslagstabel: navn-fra-data + organisatorisk enhed (dropdown).
  Første fase af fuld-CRUD-planen — se
  docs/superpowers/specs/2026-08-10-fuld-crud-design.md.

# BFHmetadata 0.5.0

## Nye features
* Signal-gennemgang (Fase B — review-UI): peg app'en på en parquet-mappe, scan
  filtrerede aktive Seriediagrammer for Anhøj-signal, og gennemgå dem i en
  interaktiv ggiraph-graf. Klik en observation for at registrere et faseskift
  direkte i tblDiagrammerMedian (tilføj/forhåndsvis/fjern), og bladr hurtigt
  mellem diagrammer. Fem filtre: Overafdeling, Afsnit, Datapakke, Datasæt,
  Indikator. Datavindue kan veksle mellem alle data og seneste N observationer.

## Interne ændringer
* Nyt headless scan-lag (fct_scan.R) + interaktivt chart-lag
  (fct_chart_interactive.R). Nye Imports: ggplot2, ggiraph.

# BFHmetadata 0.4.0

## Nye features
* Signal-gennemgang (Fase A — motor): indlæser lokale parquet-slices, bygger
  diagram-indeks fra Supabase og beregner Anhøj-signal pr. aktivt Seriediagram
  via BFHcharts (signal vurderet på seneste fase efter median-knæk). DB-accessors
  til at læse og skrive median-knæk (tblDiagrammerMedian). Diagram-indekset
  resolver org-niveauer (overafdeling/afdeling/afsnit) via rekursiv ancestry.

## Interne ændringer
* Vendored parquet-/median-logik fra BFHddl (Supabase-fodret, ingen Access-kobling).
* Nye Imports: arrow, dplyr, BFHcharts.

# BFHmetadata 0.3.0

## Nye features
* Startside med flise-grid hvor man vælger tabel/område at arbejde med.
* Generisk inline-redigering af de 6 simple opslagstabeller (Faggrupper,
  Datakilder, Dataprodukter, Diagramtyper, Organisations-niveauer,
  Indikator-niveauer): redigér celler direkte i tabellen, tilføj og slet rækker.
* Personer-tabel med inline-redigering inkl. relations-kolonne: organisatorisk
  enhed vælges via dropdown direkte i cellen (viser navn, gemmer id).
* Slet-beskyttelse: en post der er i brug kan ikke slettes (DB-FK fanges, og
  datakilder tjekkes på app-niveau da relationen ikke er DB-enforced).

## Interne ændringer
* Metadata-drevet design: ét generisk modul (mod_lookup_table) + LOOKUP_TABLES-
  config driver alle 6 tabeller. Nye rene SQL-byggere (unit-testet).

# BFHmetadata 0.2.0

## Nye features
* Kompakt oversigtstabel over indikatorer (aktiv-status, hierarki-placering,
  id, navn) med per-række åbn-knap.
* Modal-redigering: fuld adgang til alle felter, direkte FK-relationer og
  many-to-many-relationer (faggrupper, dataprodukter, organisation) vist med
  tekst-værdier i stedet for rå id'er.
* Two-fane-layout adskiller kompakt oversigt fra inline-redigering.

## Interne ændringer
* M2m-relationer skrives atomisk via replace-strategi i poolWithTransaction.
* Nye rene SQL-byggere for junction-tabeller (unit-testet).

# BFHmetadata 0.1.0

## Nye features
* Første version: CRUD på tblIndikatorer mod Supabase (load/create/update/
  soft-delete), inline DT-redigering af sikre tekstfelter, sidebar-form med
  FK-dropdowns vist som tekst-labels.
* Write-guard (BFHMETA_WRITE=1) som friktion mod utilsigtet skrivning.
