# Origin-main hardening og redning af CRUD-forbedringer

**Dato:** 2026-08-21  
**Godkendt udgangspunkt:** `origin/main` ved `40a1716ff9a59b2e8897b4919bb6ddc0a9a80eeb`  
**Arbejdsgren:** `feat/origin-main-hardening`

## Formål

Appen skal kunne bruges som en hurtig, tæt og regnearksagtig editor af
Supabase-data på computere, der ikke har lokale parquet-data eller R-pakken
Arrow. De brugbare krav fra den forældede gren `feat/crud-fundament` skal
genimplementeres oven på den aktuelle excelR-arkitektur; de gamle UI-patches
må ikke merges eller cherry-pickes ind.

Arbejdet leveres i selvstændige trin. Første leverance isolerer signaldata fra
CRUD. Derefter følger UX- og sikkerhedshardening, databasekontrol og til sidst
bulk-redigering med kontrolleret fortryd.

## Låste produktkrav

1. `origin/main` er den eneste kodebase, der bygges videre på.
2. excelR bevares som tabelmotor i denne ændring.
3. Inline-redigering, høj datatæthed og en Excel/Access-lignende arbejdsgang er
   vigtigere end DT-kompatibilitet.
4. Manglende lokale signaldata må aldrig blokere database-CRUD.
5. Browser-events er ubetroet input og må ikke direkte bestemme SQL-kolonner,
   rækker eller typer.
6. En batch er alt-eller-intet og skrives sammen med sin auditpost i samme
   PostgreSQL-transaktion.
7. Fortryd må ikke overskrive nyere ændringer tavst.
8. Ingen schemaændring køres mod Supabase uden read-only schema-verifikation og
   særskilt godkendelse.

## Ikke omfattet

- Ingen tilbagevenden til DT for indikator-, opslag-, hierarki- eller
  diagramtabeller.
- Ingen tabelmotor-migration i denne leverance.
- Ingen automatisk kørsel af gamle migrationsfiler fra
  `feat/crud-fundament`.
- Ingen implicit fortsættelse efter en fejlet eller konfliktende batch.
- Ingen afhængighed af lokale parquet-filer for appstart eller almindelig CRUD.

## Valg af tabelmotor

### Beslutning

excelR bevares. Den aktuelle kode har allerede PK-baseret selektion,
typed dropdowns, kompakte kolonnebredder, inline-diff og genindlæsning efter
skrivninger. Et skifte nu ville både øge risikoen og gøre det sværere at skelne
widgetfejl fra de hardening-fejl, dette arbejde skal løse.

### Senere evalueringsspor

`muiDataGrid` er den mest interessante fremtidige kandidat på grund af
virtualisering, redigering og en moderne React-baseret gridmodel. Den nuværende
R-wrapper er dog ung, dens serverlag er markeret eksperimentelt, og nyttige
funktioner som column pinning kan kræve MUI Pro. En senere prototype må derfor
måle arbejdshastighed, datatæthed, tastaturflow, Shiny-eventstabilitet og
licensomkostning mod excelR, før et skifte foreslås.

`editbl` fravælges i denne omgang, fordi det bygger på DT og ikke løser det
visuelle tæthedsproblem. `rhandsontable` fravælges, fordi Shiny-inputbindingen
har dokumenterede synkroniseringsforbehold.

## Komponentgrænser

### Grid-adapter

`R/fct_excel_table.R` er en UI-adapter uden databasekendskab. Den:

- renderer data og kolonnemetadata til excelR;
- udleder valg fra en stabil primærnøgle;
- oversætter et browser-event til nul eller flere celleændringer;
- returnerer feltets visningsnavn og rå browserværdi, men vælger aldrig en
  SQL-kolonne;
- tolererer sortering og rerender uden at skifte postidentitet.

Adapterens kontrakt skal gøre det muligt senere at afprøve en anden widget
uden at ændre database-accessorerne.

### CRUD-moduler

Domænemodulerne ejer filtre, valg, status, bekræftelser og validering. Et
inline-write følger denne kæde:

1. Find den stabile PK i det serverkendte datasæt.
2. Map det viste feltnavn gennem en serverside-allowlist.
3. Konvertér værdien til feltets deklarerede type.
4. Kør domænevalidering.
5. Kald én database-accessor.
6. Ryd relevant cache og genindlæs den bekræftede databasetilstand.
7. Vis en kort dansk succes- eller fejlstatus.

Ved fejl skal grid'et vise den sidst bekræftede databaseværdi. Manipulerede,
ukendte eller stale events giver intet databasekald.

### Database-accessorer

Database-accessorerne ejer parameteriseret SQL, write-guard, transaktioner og
cache-invalidering. UI-kode må ikke konstruere SQL. Dynamiske tabel- og
kolonnenavne skal komme fra statiske, serverside-konfigurationer og valideres
før interpolation.

## Robusthed uden lokale signaldata

Signalmodulet forbliver lazy og initialiseres først, når brugeren åbner fanen.
Det opererer med fire synlige tilstande:

| Tilstand | Betydning | Effekt uden for signalfanen |
|---|---|---|
| Klar | Arrow og relevante parquet-data er tilgængelige | Ingen |
| Ingen lokale data | Mappe mangler, er tom eller har ingen relevante filer | Ingen |
| Arrow mangler | Signaldata findes muligvis, men kan ikke læses | Ingen |
| Læsefejl | En fil eller et forventet schema er ugyldigt | Ingen |

Arrow flyttes fra `Imports` til `Suggests`. `requireNamespace("arrow")` kaldes
først efter, at koden har fundet mindst én relevant parquet-fil. En manglende
eller tom mappe returnerer den typede tilstand "ingen lokale data" uden at
indlæse Arrow.

Fejl isoleres pr. indikator/signal, så en korrupt fil ikke stopper resten af
scannet. Resultatvisningen skelner mellem signal, ingen lokale data og
læsefejl. Ingen af disse tilstande må lukke Shiny-sessionen, databasepoolen
eller andre moduler.

Databasekonfiguration til en installeret app skal findes gennem en eksplicit
sti eller en pakket ikke-hemmelig konfigurationsfil. Password og andre
hemmeligheder forbliver miljøvariable og må ikke pakkes eller logges.

## UX- og sikkerhedshardening

Følgende krav genimplementeres semantisk oven på excelR:

- Bekræftelse før deaktivering og fysisk sletning.
- Den valgte PK fryses, når dialogen åbnes, og ryddes efter succes, annullering
  eller valideringsfejl.
- Ref-count eller tilsvarende guard køres før sletning, men databaseconstraints
  er fortsat den autoritative sidste barriere.
- Browserfelter, celletyper og række-ID'er valideres mod serverside-metadata.
- Databasefejl oversættes til korte danske meddelelser uden SQL eller
  legitimationsoplysninger.
- Langvarige kald viser en ikke-blokerende ventestatus, som altid fjernes igen.
- Tomme filterresultater forklarer forskellen mellem "ingen data" og "filtre
  udelukker alle" og tilbyder at rydde filtrene.
- Rækker er kompakte, padding er minimal, dropdowns er søgbare, og vandret
  scrolling foretrækkes frem for brede formularflader.
- Pagination indføres ikke som standard, fordi den bryder regnearksflowet.

## Bulk-redigering og fortryd

Bulk-redigering sætter én typed værdi i ét tilladt felt på et eksplicit sæt
stabile indikator-ID'er. Synlige rækkenumre eller et aktuelt filterudtryk er
ikke en batchidentitet.

Serverflowet er:

1. Modtag ID'er, felt og rå værdi.
2. Genfind ID'erne i databasen og afvis manglende, dublerede eller inaktive
   poster efter den valgte operationstype.
3. Map feltet gennem en særskilt bulk-allowlist og konvertér værdien til den
   deklarerede PostgreSQL/R-type.
4. Start én transaktion og lås alle målposter i stabil ID-rækkefølge.
5. Sammenlign versionsstempel eller forventet førværdi for at opdage samtidige
   ændringer.
6. Skriv alle ændringer og én typed auditpost pr. række/felt med samme
   `batch_id`.
7. Commit kun, hvis alle writes og auditwrites lykkes.
8. Ryd cache og genindlæs fra databasen efter commit.

Fortryd er en ny transaktion. Den låser de samme poster, kontrollerer at den
nuværende værdi stadig svarer til batchens `vaerdi_efter`, og skriver den
typede `vaerdi_foer` tilbage. Hvis blot én post er ændret siden batchen, afvises
hele fortrydelsen med en konfliktrapport. En batch kan ikke fortrydes delvist.

Auditdata placeres i et ikke-eksponeret `audit`-schema. `anon` og
`authenticated` får ingen schema- eller tabeladgang. Eventuelle funktioner er
`SECURITY INVOKER`, medmindre en senere særskilt sikkerhedsvurdering begrunder
andet.

## Leverancer

### Leverance 1: Signal/CRUD-isolation

- Valgfri Arrow-afhængighed.
- Typede signaltilstande og handlingsanvisende UI.
- Robust tom/manglende/korrupte parquet-data.
- Sikker konfigurationssti for installeret app.
- Fokuserede tests og fuld regressionstest.

### Leverance 2: excelR UX- og sikkerhedshardening

- Event-allowlists, typeguards og stale-selection guards.
- Bekræftelser med fastfrosset PK.
- Strukturerede fejl, ventestatus og tomme tilstande.
- Kompakt grid-verifikation i rigtig browser.

### Leverance 3: Database- og migrationsrevision

- Read-only introspektion af aktuelle tabeller, constraints, indexes, triggers,
  schemas og privileges.
- Sammenligning med de gamle foreslåede migrationer.
- Ny idempotent migration, kun for dokumenterede mangler.
- PostgreSQL succes- og rollback-rehearsal før Supabase-kørsel.

### Leverance 4: Bulk og fortryd

- Typed batchkontrakt, audit og konfliktkontrol.
- En transaktion pr. batch eller fortrydelse.
- UI til valg, forhåndsvisning, bekræftelse og resultatrapport.
- Integrationstest med tvungen fejl midt i batchen.

### Senere spike: alternativ gridmotor

En isoleret muiDataGrid-prototype må først startes, når leverance 1-4 er
stabile. Den må ikke erstatte excelR uden brugerprøve og målbar forbedring.

## Verifikation

Hver leverance bruger testdrevet udvikling og skal bestå:

1. Fokuserede unit- og `shiny::testServer`-tests.
2. Den fulde `devtools::test()`-suite.
3. Relevante PostgreSQL-integrationstests med både commit og tvungen rollback.
4. Package build/check, når dependencies eller pakningsfiler ændres.
5. Browserprøver af grid-state og events, ikke kun HTML-snapshots.

De manuelle browserprøver dækker mindst:

1. Tastaturredigering på tværs af flere celler og kolonner.
2. Søgbare FK-dropdowns samt kompakt række- og kolonnelayout.
3. Fejlende write, hvor cellen vender tilbage til databaseværdien.
4. Manglende lokale parquet-data, mens CRUD fortsat virker.
5. Bulk med succes, konflikt, rollback og efterfølgende fortryd.

Baseline på `40a1716` er 1.292 beståede tests, 0 fejl, 0 advarsler og 15
eksplicit skippede databaseintegrationstests (`BFHMETA_WRITE != 1`). Fokuserede
og fulde testresultater rapporteres hver for sig.

## Migrations- og driftsport

Før en schemaændring kan godkendes, skal arbejdet dokumentere:

- den aktuelle live-definition og privileges for hvert berørt objekt;
- præcist hvilke mangler migrationen retter;
- backup- og restore-kommandoer med validerede mål;
- en rigtig PostgreSQL succes- og rollback-kørsel;
- at audit-schemaet ikke eksponeres gennem Supabase Data API;
- at eksisterende CRUD fortsat kan læse og skrive efter migrationen.

Migrationen forberedes og testes lokalt, men køres ikke mod Supabase som en
implicit del af kodeimplementeringen.
