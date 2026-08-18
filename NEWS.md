# BFHmetadata (development)

## Breaking changes
* **Signal-gennemgangen beregner nu signaler på den samme serie som
  diagrammerne faktisk viser.** Tidligere blev Anhøj-signaler beregnet på rå
  dagsdata, mens BFHddl aggregerer til uge/måned pr. diagram
  (`periode_aggregering`) før chart-generering. Gennemgangen viste altså
  signaler for en anden tidsserie end den klinikerne ser.

  Målt på produktionsdata gav det forskelligt signal for 12 af 39 enheder
  (31 %) på én indikator — i begge retninger, altså både signaler der ikke
  fandtes i diagrammet og manglende signaler der gjorde. Eksisterende
  vurderinger baseret på gennemgangen bør derfor gentages.

  Konsekvenser: "Seneste N" tæller nu **perioder** (uger/måneder), ikke dage —
  default er hævet fra 24 til 36, så gennemgangen viser lige så mange
  observationer som den genererede PDF. Tallet "n obs" er nu antal
  perioder, ikke antal dage.

* **Median-knæk gemmes med den aggregering de blev sat under.** Et knæk
  gemmes som en dato, men betyder reelt en position i serien — samme dato
  giver forskellige faseskift ved forskellig aggregering (fx flytter
  2025-03-17 sig til 2025-04-01 under måneds-aggregering, fordi ingen
  periode starter midt i måneden). Knæk sat under en anden aggregering end
  diagrammet bruger nu, indgår derfor **ikke** i beregningen; de vises i
  stedet som ignorerede i knæk-tabellen med en advarsel ved grafen, så
  faserne ikke ændrer sig usynligt. Eksisterende knæk er stemplet med
  diagrammets nuværende periode (de lå alle i forvejen på den periodes
  grænse), så ingen knæk falder ud ved opgraderingen.

## Nye features
* Kaskade-filtre på tværs af hierarki-dimensionerne: På Diagram-siden
  begrænser et valgt Datapakke-filter nu både Datasæt- og
  Indikator-valgene, og et valgt Datasæt begrænser Indikator-valgene.
  Samme kaskade i Signal-gennemgangens sidebar (multi-select: valg der
  stadig er gyldige bevares). Indikator-siden havde allerede
  Datapakke → Datasæt-kaskaden.
* Diagram-grid'ets Indikator-dropdown viser nu kun indikatorer under
  rækkens registrerede datasæt (niveau-udledt), så et diagram ikke ved en
  fejl kan flyttes til en indikator i et helt andet datasæt. Rækkens
  nuværende værdi vises altid; rækker uden datasæt får hele listen.
* Indikator-hierarkiet (datasæt/datapakker, `tblIndikatorHierarki`) kan nu
  redigeres direkte i appen: ny fane "Indikator-hierarki" med samme
  inline-trærediger som organisations-strukturen (dobbeltklik en celle,
  Forælder/Niveau som dropdowns med cyklus-værn) plus et redigerbart
  Aktiv-flueben pr. node. Dermed er fuld-CRUD-designet komplet — alle
  metadata-tabeller kan vedligeholdes uden Access.
* Indikator-hierarki-siden har kaskade-filtre som Indikator-siden: vælg en
  Datapakke og/eller et Datasæt for kun at se den gren af træet.
  Forælder-dropdown'en tilbyder fortsat hele træet, så en node kan flyttes
  ud af den viste gren.
* Indikator-modalens og indikator-grid'ets "Datasæt"-dropdown tilbyder kun
  AKTIVE hierarki-noder ved nyvalg; en eksisterende værdi der peger på en
  inaktiv node bevares og vises med "(inaktiv)"-suffix — ingen stille
  datamutation ved deaktivering af et datasæt.
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

* Ventetid er nu synlig: fane-skift sker med det samme (modulets data hentes
  lige efter, med "Henter …"-notifikation imens), og fingeraftryk-sweepen ved
  opstart viser "Tjekker parquet-lager for ændringer…" mens den kører. Før
  kunne flise-klik føles "døde", mens appen arbejdede i baggrunden.

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

## Bug fixes
* Inline-redigeringer i grid'ene (Indikatorer, Indikator-hierarki,
  Organisation, opslagstabeller, Diagrammer) gik tabt, medmindre man
  bagefter klikkede i en checkbox: excelR sender celle-ændringer og
  selektioner på samme Shiny-input uden event-prioritet, og jexcels
  markør-flytning umiddelbart efter en celle-commit overskrev
  ændrings-payloaden i Shinys input-batch. Serveren diffar nu også
  selektions-payloadens fullData (som bærer hele grid'ets indhold), så
  den overskrevne ændring altid gemmes alligevel.
* Datapakke- og Datasæt-filtrene blandede niveauerne sammen: "Datapakke" og
  "Datasæt" blev udledt som "noden FK'en peger på + dens forælder", men
  indikatorerne peger på blandede niveauer (94 % på Indikatorsamling-noder),
  så Datasæt-filteret viste samlingsnavne og Datapakke-filteret datasætnavne.
  Begge udledes nu niveau-bevidst som forfaderen på niveauet 'Datapakke' hhv.
  'Datasæt' (rekursiv CTE, delt af Indikator-oversigten, diagram-admin og
  signal-gennemgangen). Indikator-grid'et viser nu Datapakke + Datasæt som
  låst kontekst, og den redigerbare FK-kolonne hedder "Hierarki-placering"
  (før misvisende "Datasæt" — værdien er typisk en indikatorsamling).

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
