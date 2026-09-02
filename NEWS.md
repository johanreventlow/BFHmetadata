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
* **Indikatorer kan nu slettes permanent.** Hidtil kunne en indikator kun
  deaktiveres — en fejloprettet indikator blev derfor liggende for altid. Ny
  knap "Slet valgte" på Indikatorer-fanen sletter den valgte indikator og dens
  relationer (faggrupper, dataprodukter, organisation) i én transaktion.
  Sletningen er guardet: har indikatoren diagrammer, blokeres den med besked om
  hvor mange, så man selv tager stilling til diagrammerne først i stedet for at
  få en rå constraint-fejl. Bekræftelsesdialogen fryser den valgte række og
  siger tydeligt, at handlingen ikke kan fortrydes, og henviser til
  "Deaktivér valgte" når indikatoren blot skal skjules.
* **Grid'ene understøtter nu range-selektion af flere rækker** (klik +
  shift-klik / træk) i både indikator- og diagramtabellen, samt en knap
  "Vælg alle viste" der markerer alle rækker i det aktuelt filtrerede
  resultat. Forarbejde til bulk-redigering
  (`docs/plans/2026-08-30-bulk-redigering-design.md`) — en ny knap
  "Redigér valgte (N)" viser selektionens størrelse, men er endnu disabled;
  selve batch-skrivningen leveres i en senere leverance.
* **Diagram-sidens Periode-valg tilbyder nu hele ordforrådet: dag, uge,
  maaned, kvartal og aar.** Valglisten var et DISTINCT-udtræk af værdier i
  brug og kunne derfor aldrig tilbyde en NY periode (hønen-og-ægget — "dag"
  og "kvartal" fandtes ikke, før en række allerede brugte dem). Nu er listen
  det kanoniske ordforråd plus evt. legacy-værdier fra databasen (bagest,
  så de stadig kan ses og genvælges). Både signal-gennemgangens beregning
  (`period_to_en`) og BFHddl's pipeline forstår alle fem værdier i
  forvejen — ingen ændring nødvendig i BFHddl.
* **Signal-gennemgang viser indikatorens hierarki-kontekst.** Under
  navigations-overskriften ("3/12 — Indikator · Enhed") står nu en dæmpet
  linje med "Datasæt: …" og — når indikatoren hører til en — "Samling: …"
  (indikatorsamlingen under datasættet), så man kan se hvor indikatoren
  hører til, ikke kun navnet. Diagram-indekset beriges med
  indikatorsamlingen via det niveau-bevidste forfader-opslag.
* **Diagrammer-fanen kan redigere `aggreger_egne_og_boern`.** Ny
  checkbox-kolonne "Egne+børn" i diagram-grid'et (mellem Aggregering og
  Aktiv) og et tilsvarende flueben "Aggregér egne data + børn" i
  opret/redigér-modalen. Ældre admin-udtræk uden kolonnen viser FALSE i
  stedet for at fejle.
* **Oprulning: opt-in "egne rækker + børn"
  (`tblDiagrammer.aggreger_egne_og_boern`).** Hidtil vandt en enheds egne
  rådata alene: havde fx en afdeling egne rækker (henvisninger uden kendt
  afsnit), taltes afsnittenes data hverken med i afdelingens serie eller i
  hospitalstotalen. Med flaget sat på enhedens diagram-række lægges
  undertræets aggregat oveni de egne rækker — synkroniseret 1:1 med BFHddl
  (branch `claude/aggreger-egne-og-boern`), så signal-gennemgangen ser
  samme serier som produktionsgraferne. Default FALSE = præcis hidtidig
  adfærd (pinnet af tests); migration
  `migration/09_aggreger_egne_og_boern.sql`. Flaget er sat for de 9
  afdelings-rækker på "Henvist til rygestopkursus".
* **Signal-gennemgang: NA-alarm.** Perioder, hvis beregningsgrundlag
  indeholder NA (fx en bidragyder med eksplicit manglende tal i en periode
  ved hierarki-oprulning, hvor summeringen bevidst bruger `na.rm = FALSE`),
  udgik før tavst af grafen. Nu vises en advarsel ved diagrammet ("N
  perioder udgår af beregningen…"), og scan-oversigten tæller, hvor mange
  diagrammer der er ramt — også dem uden signal, som ellers aldrig ville
  blive åbnet.
* **Mål-fanen viser nu diagrammets målgruppe.** Ny readOnly-kolonne
  "Målgruppe" i mål-grid'et (mellem Type og Retning), så to mål på samme
  indikator/enhed med forskellige målgrupper kan skelnes. "Nyt mål"-modalens
  diagram-vælger medtager også målgruppen i labelen, når diagrammet har en.
  Målgruppen hører til diagrammet og redigeres fortsat på Diagram-fanen.
* **Nyt hierarki-felt: "Kort navn i titel" (`brug_kort_navn_i_titel`).**
  Indikator-hierarkiets grid og "Ny node"-formular kan nu redigere det nye
  boolean-flag på `tblIndikatorHierarki` (migrering
  `migration/08_brug_kort_navn_i_titel.sql`). Flaget er opt-in pr. datasæt:
  BFHddl viser `hierarki_navn_kort` i chart-titlens datasæt-linje, når det er
  sat (fx LUP → "Patienttilfredshed (LUP)"), mens det lange `hierarki_navn`
  forbliver autoritativt til dataportal-generering. Den generiske
  hierarki-editor har samtidig fået understøttelse for felttypen
  `checkbox` (rendering, inline-validering og formular), så fremtidige
  boolean-kolonner blot skal registreres i `HIERARCHY_TABLES`.
* **Signal-gennemgang: filtrér median-flade diagrammer fra visningen.** Nyt
  afkrydsningsfelt "Skjul: halvdelen el. flere obs. på medianen" skjuler
  diagrammer, hvor mindst halvdelen af observationerne (pr. fase) ligger på
  medianen — dér er run chart-reglerne upålidelige, og graferne skifter selv
  centerlinjen til gennemsnit. Filter-reglen ser altid på medianen (spejler
  BFHcharts' auto-mean-betingelse, inkl. helt konstante serier) og virker i
  alle visningstilstande, også "Vis alle".
* **Gem-og-reload-loop i redigerings-grids er stoppet (ekko-værn).** excelR's
  widget sender ved hvert re-render selv payloads på gridets Shiny-input (et
  data-ekko plus et selektions-ekko når markøren genskabes), og modulerne
  diff'er alle payloads og genindlæser efter gem/afvisning. En vedvarende
  repræsentationsforskel (fx checkbox `true` vs. `TRUE`, tal- eller
  datoformatering) kunne derfor blive til et selvkørende
  gem→reload→ekko→gem-loop uden fejlmeddelelse, hvor appen "arbejdede" i ring,
  indtil siden blev genindlæst manuelt. Fixet er i to lag: (1) et
  klient-script dropper de payloads, et grid selv affyrer synkront under sit
  re-render — de gengiver kun, hvad serveren netop har renderet; (2) et
  server-side værn genkender en diff, der er identisk med den netop
  behandlede lige efter et reload, og springer den over. Gælder alle fire
  grid-moduler (opslagstabeller, hierarkier, diagrammer og mål).
* **Flydende genoptagelse efter tabt forbindelse.** Når websocket-forbindelsen
  til Shiny-processen tabes (dvale, låst maskine, netværksskift), lagde Shiny
  et mørkt overlay over appen, som derefter var død indtil manuel
  genindlæsning. Nu undertrykkes overlayet; en diskret toast i hjørnet viser
  "genopretter…", klienten poller serveren og genindlæser siden automatisk,
  når den svarer igen — og den fane, brugeren stod på, genåbnes (gemt i
  sessionStorage). Står en redigerings-modal åben, genindlæses der ikke bag
  om ryggen på brugeren: toasten tilbyder i stedet en "Genindlæs"-knap, så
  halvfærdig indtastning kan kopieres først. `session$allowReconnect(TRUE)`
  er samtidig slået til, så hosting med reconnect-understøttelse (Connect/
  Shiny Server Pro) kan genoptage sessionen helt uden genindlæsning.
* Appen kan nu installeres og bruges til database-CRUD uden R-pakken Arrow og
  uden lokale parquet-data. Signal-gennemgangen viser særskilt, om data mangler,
  Arrow mangler, eller enkelte diagrammer har læsefejl; disse tilstande påvirker
  ikke excelR-redigering eller andre faner. Startup-kompaktering tilbyder ikke
  en operation, som maskinen mangler Arrow til.
* Hierarki-dropdowns viser nu træstrukturen visuelt (depth-first-orden
  med indrykning) i stedet for en flad alfabetisk liste:
  Hierarki-placering på Indikator-siden (grid + modal), Forælder-dropdowns
  på hierarki-siderne (grid + "Ny node") og Organisatorisk enhed på
  Personer-tabellen. Autocomplete-søgning virker uændret.
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
* Sletning af en opslagsrække og deaktivering af en indikator kræver nu
  bekræftelse i en dialog i stedet for at ske øjeblikkeligt ved klik.
  Rækken/indikatoren fryses ved dialog-visning, så et evt. selektionsskift
  mens dialogen er åben ikke ændrer hvad der rent faktisk rammes.
* Database-fejl vises nu med en dansk, forståelig besked (fx "posten er i
  brug", "værdien findes allerede", "feltet må ikke være tomt",
  "forbindelsen blev afbrudt") i stedet for "Fejl ved gem/slet (se log)".
* Alle gem-, slet- og opdatér-kald mod databasen viser nu en kort
  ventevisning, mens kaldet kører.
* Indikator-oversigten viser en forklarende tom-tilstand ("Ingen
  indikatorer at vise" + en "Ryd filtre"-knap), når de valgte filtre ikke
  matcher nogen rækker, i stedet for et tomt grid.
* Opslagstabellen Faggrupper redigeres nu via en opt-in excelR-adapter, der
  sender pålidelige enkeltcelle-hændelser direkte fra browseren i stedet
  for at diffe hele tabellen ved hver ændring.
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
* Forberedt (ikke kørt) migration til den kommende bulk-redigering med
  fortryd: tidsstempler + trigger, unikhed på indikator_navn_teknisk, FK på
  datakilde, NOT NULL + unikhed på junction-tabellerne, og en ændringslog i
  et separat audit-schema (uden for public, så den ikke eksponeres via
  Supabases Data API). Se migration/06_preflight.sql og
  migration/07_migration.sql — kør ikke uden en verificeret backup
  (migration/backup.sh) og et grønt preflight-tjek først.
* Batch-kontrakten i DB-laget til bulk-redigering (Leverance 2 af
  `docs/plans/2026-08-30-bulk-redigering-design.md`, endnu ikke koblet til
  UI): serverside-allowlists pr. tabel (`BULK_INDIKATOR_FIELDS`/
  `BULK_DIAGRAM_FIELDS` i metadata.R — ukendt felt afvises uden DB-kald),
  SQL-buildere til lås/skriv/audit (fct_sql.R), og
  `bulk_update`/`bulk_undo` (fct_db.R): én transaktion pr. batch med
  `SELECT … FOR UPDATE` i stabil id-rækkefølge, førværdi-sammenligning mod
  det UI'et sidst viste (stale → hele batchen afvises), og fortryd der
  afviser fuldt ved konflikt i stedet for delvis rollback. Ændringer
  auditeres i `audit."tblAendringslog"` — den log der faktisk er deployeret
  (`migration/07_migration.sql`); designets oprindelige skitse med to
  tabeller blev aldrig oprettet. Integrationstests kører mod engangstabeller
  efter `dev/bulk_probe.R`-mønstret, inkl. tvungen fejl midt i en batch med
  bevis for fuld rollback.

## Bug fixes
* **Siden kunne ikke scrolles efter at en indikator var redigeret i modalen.**
  Bootstrap låser sidescroll ved at sætte `overflow: hidden` på `<body>`, når
  en modal åbnes, og ruller først låsen tilbage, når modalens lukke-event
  fyrer. Udebliver det event, står låsen tilbage — sammen med en efterladt
  backdrop, der ligger som et usynligt klik-skjold over siden. En vagt
  (`bfh-modal-scroll-guard.js`) rydder nu body-tilstanden, når der ikke længere
  er en synlig modal. Den lytter både på lukke-eventet og — fordi netop det
  event kan udeblive — direkte på ændringer i `<body>`, og den rører intet, så
  længe en modal stadig er åben, så bevidste modal-skift (bekræftelse af
  ændret indikator-id, diagram-swap, fortryd) er uændrede.

  Vagten fjerner symptomet, men **rodårsagen er ikke fastslået**: flere veje
  kan efterlade låsen — `showModal()` oven på en åben modal (appen gør det
  bevidst flere steder, og kompakterings-sweep'en kan gøre det uopfordret midt
  i en redigering), eller en lukning der ikke når at afslutte sin transition,
  fordi grid'et re-renderes tungt i samme runde. Findes årsagen senere, bør
  den fixes ved kilden; vagten kan blive stående som værn.
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
