# excelR UX-adapter

**Dato:** 2026-08-22

**Status:** Design godkendt i chat; skriftlig gennemlæsning afventer

**Branchgrundlag:** `feat/origin-main-hardening` ved `dc8796f`

## Formål

BFHmetadata beholder excelR som tabelmotor. Redigering skal opleves som et
kompakt dataark i Excel eller Access: brugeren skal kunne bevæge sig hurtigt med
tastaturet, se mange rækker og kolonner samtidig og fortsætte arbejdet, mens den
senest ændrede celle gemmes i Supabase.

Den nuværende excelR-binding sender ændring og selektion på samme Shiny-input
uden event-prioritet. En markørflytning kan derfor overskrive ændringseventet i
samme input-batch. Appen kompenserer ved at sende og diff'e hele tabellen, og
flere moduler genindlæser hele grid'et efter en gemning. Det er robustgjort, men
giver unødvendig trafik og kan nulstille den arbejdsposition, brugeren er i.

Denne leverance indfører en BFHmetadata-ejet adapter omkring excelR. excelR og
den medfølgende jspreadsheet-version forkes ikke.

## Prioritering

Funktionerne prioriteres i denne rækkefølge:

1. Hurtig enkeltcelleredigering med Enter, Tab og piletaster.
2. Samme typede værdi på et eksplicit sæt markerede rækker.
3. Kopier og indsæt forskellige værdier over flere celler.

Kun punkt 1 implementeres fuldt i denne UX-leverance. Adapteren leverer stabile
række-ID'er til punkt 2, men bulk-write, audit og fortryd hører til den særskilte
bulk-leverance. Punkt 3 bevarer indledningsvis den eksisterende adfærd og må ikke
fremstilles som en atomisk databaseoperation.

## Valgt løsning

Et lille app-ejet JavaScript-lag kobles på hver excelR-instans og kommunikerer
med et fælles R-hjælpelag. Laget komponerer med widget'et og må ikke ændre filer
i den installerede excelR-pakke.

Tre alternativer blev vurderet:

- CSS alene løser ikke eventkollision eller fuld genindlæsning.
- En lokal fork af excelR giver fuld kontrol, men påfører projektet ansvar for
  widget-bindingen og en ældre jspreadsheet-version.
- Den valgte adapter giver en eksplicit eventkontrakt og lokal visuel feedback
  med væsentligt mindre vedligeholdelsesbyrde end en fork.

## Komponenter

### JavaScript-adapter

Adapteren initialiseres pr. grid-instans og har følgende ansvar:

- komponere med excelR's eksisterende callbacks uden at registrere globale,
  kolliderende handlers;
- sende én entydig celleevent med `priority: "event"`;
- sende selektion på en separat inputkanal;
- føre et register over seneste event pr. celle;
- markere celler som ventende, gemte eller afviste;
- anvende serverens kanoniske værdi ved afvisning uden at udløse et nyt write;
- ignorere svar fra en ældre grid-generation eller en overhalet celleevent;
- bevare aktiv celle, scroll og klient-sortering ved ack eller reject.

### R-adapter

Det fælles R-lag har følgende ansvar:

- definere den autoritative mapping fra grid-kolonne til databasefelt;
- validere eventform, grid-generation, PK, kolonne og rå værdi;
- typekonvertere ud fra serverens metadata;
- kalde modulets eksisterende validerings- og writefunktion;
- opdatere modulets kanoniske lokale data efter succes;
- sende et struktureret ack eller reject tilbage til præcis den celleevent;
- levere stabile valgte PK'er til eksisterende rækkehandlinger og senere bulk.

Adapteren kender ikke konkrete tabeller eller SQL. Hvert modul leverer data,
PK-felt, kolonnemetadata og en callback for én valideret celleændring.

Modulets kanoniske data og dets render-trigger holdes adskilt. En succesfuld
cellepatch opdaterer den kanoniske række, men må ikke i sig selv invalidere
`renderExcel()`. En ny `grid_generation` oprettes kun ved en bevidst fuld
render, eksempelvis efter filterskift, opret/slet eller fail-closed reload.

## Eventkontrakt

Browseren sender følgende minimale payload ved en commit:

- `event_id`: unik, monoton identitet inden for grid-instansen;
- `grid_generation`: identitet for den renderede dataversion;
- `row_pk`: værdien fra grid'ets skjulte PK-kolonne;
- `column_index`: visuel kolonneposition;
- `raw_value`: den nye, ufortolkede celleværdi.

Browseren sender ikke et databasefeltnavn, der stoles på. Serveren mapper
`column_index` gennem den aktuelle kolonneallowlist og afviser skjulte,
read-only, ukendte eller ikke-redigerbare kolonner.

Alle adapterstyrede grids beholder skjult PK som første datakolonne og har
kolonne-drag slået fra. Hvis kolonnemetadata og den renderede kolonneorden ikke
matcher samme `grid_generation`, afvises eventet; der gættes ikke på et felt.

Serverens svar indeholder:

- samme `event_id` og `grid_generation`;
- `status`, enten `saved` eller `rejected`;
- den kanoniske, serialiserede celleværdi;
- ved afvisning en kort, sikker dansk brugerbesked.

Et svar må kun ændre en celle, hvis eventet stadig er det seneste registrerede
event for den celle og generationen fortsat vises. Det forhindrer et langsomt
svar i at overskrive en nyere redigering.

## Gemmeflow

1. excelR committer den lokale celle og flytter markøren efter sin normale
   tastaturadfærd.
2. Adapteren sender eventet og markerer cellen diskret som ventende.
3. Brugeren kan straks fortsætte i en anden celle.
4. Shiny behandler events sekventielt. Serveren mapper og validerer eventet og
   udfører det eksisterende enkeltcelle-write.
5. Ved succes patches modulets lokale kanoniske række uden en fuld
   grid-genindlæsning, og der sendes ack.
6. Ved valideringsfejl bruges den kendte kanoniske modulværdi. Ved en DB-fejl
   genlæses den konkrete række eller celle målrettet, fordi et forbindelsesudfald
   kan opstå efter commit og derfor gøre write-resultatet tvetydigt. Kun den
   berørte celle gendannes eller bekræftes ud fra genlæsningen, og der sendes
   reject eller saved med den faktisk lagrede værdi.

Programmatisk gendannelse skal have en suppress-guard, så den ikke skaber et nyt
ændringsevent. Flere hurtige ændringer i forskellige celler kan være ventende
samtidig. Flere ændringer i samme celle afgøres af det seneste event-ID; et
ældre svar må hverken rydde status eller gendanne værdien.

Der indføres ikke offline-kø. Hvis Shiny-forbindelsen mistes, er ventende
markeringer ikke dokumentation for en gemt ændring; en genforbundet eller ny
session genindlæser den kanoniske databasetilstand.

## Samtidighed

Denne leverance ændrer ikke databasens samtidighedskontrakt. Indtil tabellerne
har et verificeret versionsfelt eller en compare-and-swap-kontrakt, er
enkeltcellewrites fortsat seneste skrivning vinder.

Eventkontrakten reserverer plads til en senere `expected_version`, men klientens
gamle celleværdi må ikke bruges som bevis på databaseversion. UX-adapteren må
ikke beskrives som beskyttelse mod redigering fra to computere.

## Visuelt design

Grid'et er én konsekvent kompakt arbejdsflade; der tilføjes ikke en
tæthedsvælger i denne leverance.

- Rækkehøjde: cirka 28 px.
- Cellepadding: cirka 2 px lodret og 6 px vandret.
- Celler vises på én linje med ellipsis i stedet for at gøre rækken høj.
- Den fulde værdi kan aflæses ved fokus eller hover og redigeres i cellens
  editor; felter der kræver længere formularredigering forbliver i modal.
- Grid'et bruger den resterende viewport-højde med intern lodret og vandret
  scrolling.
- Kolonneoverskriften forbliver synlig under lodret scrolling.
- Der indføres ikke pagination.
- Read-only-kolonner har diskret grå baggrund.
- Redigerbare celler og den aktive celle er tydelige uden kraftig permanent
  farvelægning.

Cellestatus vises lokalt:

- `pending`: diskret gul markering;
- `saved`: kort grøn markering, som forsvinder automatisk;
- `rejected`: rød markering samt kort dansk besked i modulets statusområde eller
  notifikation.

Status må ikke alene formidles med farve; fejl har altid tekst. Markeringerne må
ikke skjule excelR's aktive celle- eller selektionsramme.

## Tastatur og direkte redigering

Adapteren skal bevare og browserverificere følgende flow:

- Enter committer og flytter én række ned;
- Tab og Shift+Tab committer og flytter frem eller tilbage;
- piletaster flytter markøren, når cellen ikke er i tekstredigering;
- Escape annullerer den igangværende lokale redigering;
- søgbare FK-dropdowns kan fortsat filtreres ved indtastning;
- checkboxceller kan ændres uden at gøre en efterfølgende tekstændring til et
  tabt event;
- kopiering bevares.

Adapteren må ikke omdefinere native tastaturadfærd, medmindre en rigtig
browsertest påviser, at excelR ikke leverer det specificerede flow.

## Selektion og bulkgrænse

Selektion sendes på en separat kanal som stabile PK'er og cellegrænser.
Rækkehandlinger må aldrig udlede identitet fra den oprindelige serverrækkefølge,
når brugeren har sorteret grid'et.

Denne leverance må gerne gøre multi-række-selektion tilgængelig for UI'et, men
må ikke udføre et bulk-write som en løkke af uafhængige commits. Funktionen
"sæt samme værdi" kræver senere:

- eksplicitte og genvaliderede PK'er;
- typed feltallowlist;
- én databaseforbindelse og én transaktion;
- audit pr. række og felt med fælles batch-ID;
- konfliktkontrol og atomisk fortryd.

Flercelle-paste kan i denne leverance fortsat resultere i flere tydeligt
uafhængige enkeltcellewrites. UI'et må ikke love, at hele indsætningen enten
lykkes eller fejler samlet. En senere paste-batch skal bruge samme transaktions-
og auditkontrakt som øvrig bulk.

## Fejl og brugerbeskeder

- Ugyldig eventform, ukendt PK, ukendt kolonne og read-only-felt afvises før DB.
- Type- og domænefejl vises på dansk ved den berørte celle.
- Databasefejl logges via eksisterende sikker fejlhåndtering, men SQL,
  forbindelsesoplysninger og credentials vises aldrig i browseren.
- En fejl i én celle må ikke genindlæse eller gendanne andre celler.
- Hvis den kanoniske værdi ikke kan genfindes sikkert efter en DB-fejl, låses
  grid'et kort og modulets data genindlæses fail-closed fra databasen.

## Udrulning

Udrulningen sker i mindre batches med stop og manuel kontrol efter hvert trin:

1. Fælles adapter og én enkel opslagstabel.
2. Indikator-grid'et med alle rækker og
   `abx_forbrug_samlet_pr_udskrivelse` som fast manuel røgtest.
3. Diagram-grid'et.
4. Organisations- og indikatorhierarki.
5. De resterende generiske opslagstabeller.

Hvert modul optes særskilt ind i adapteren. Den gamle fuldtabel-diff forbliver
aktiv i endnu ikke migrerede moduler. I et migreret modul deaktiveres legacy-
payloaden som write-kilde fra første adaptertest; den kan midlertidigt læses til
selektion, men må aldrig skrive. Ny og gammel eventvej kan dermed ikke skrive
samme ændring to gange.

Den gamle eventvej fjernes fra et modul, når dets browsertests og manuelle
røgtest er bestået. Der fortsættes ikke automatisk til næste modulbatch.

## Teststrategi

### Rene R-tests

- payloadform og obligatoriske felter;
- mapping fra kolonneposition til serverfelt;
- afvisning af ukendt, skjult og read-only kolonne;
- typed konvertering af tekst, heltal, boolean og FK;
- event- og generationssammenligning;
- kanonisk serialisering af `NA`, tom tekst, tal og boolean.

### Modultests

- succes kalder præcis ét forventet write og patches lokal moduldata;
- valideringsfejl kalder ikke databasen og returnerer reject;
- databasefejl returnerer kanonisk værdi og påvirker ikke andre celler;
- selektion bruger PK efter klient-sortering;
- gammel og ny eventvej kan ikke dobbeltgemme;
- cacheinvalidationssignaler udsendes fortsat efter et write.

### Browsertests

Der tilføjes en rigtig browserbaseret testvej; `testServer` alene kan ikke bevise
excelR's JavaScript-rækkefølge eller fokusadfærd. Testene dækker:

- Enter, Tab, Shift+Tab, piletaster og Escape;
- mindst tre hurtige ændringer, før første ack er modtaget;
- celleændring umiddelbart efterfulgt af selektionsændring;
- checkboxændring efterfulgt af tekstændring;
- successmarkering uden fuld genrender;
- tvungen databasefejl, hvor kun den ene celle gendannes;
- bevaret fokus, scroll og sortering efter ack og reject;
- søgning og valg i en stor FK-dropdown;
- bred tabel med vandret scrolling og fast overskrift;
- ingen tabte events ved kopier/indsæt af et lille område. Hvis excelR ikke
  udsender én entydig callback pr. ændret celle, deaktiveres flercelle-paste i
  migrerede grids, indtil den senere atomiske paste-kontrakt er implementeret.

### Manuel røgtest

På Indikator-siden indlæses produktionslignende datamængde. Indikatoren
`abx_forbrug_samlet_pr_udskrivelse` bruges til en tilladt redigering, hurtig
navigering og fuld reload. Testværdien gendannes og verificeres efter reload.

## Acceptkriterier

Leverancen er først klar for et migreret grid, når:

1. Ingen almindelig enkeltcelleændring kræver fuld grid-genindlæsning ved
   succes.
2. Ændring plus øjeblikkelig markørflytning resulterer i præcis ét DB-write.
3. Brugeren kan fortsætte med Enter eller Tab, mens tidligere celler gemmes.
4. En tvungen fejl gendanner kun den fejlede celle med synlig tekstbesked.
5. Stale ack eller reject kan ikke overskrive en nyere celleværdi.
6. PK og felt valideres serverside mod den aktuelle gridkonfiguration.
7. Scroll, sortering og aktiv celle bevares gennem normal gemning.
8. Grid'et viser markant flere rækker pr. viewport end den nuværende opsætning
   uden at gøre tekst ulæselig.
9. Fokuserede R-tests, browsertests og den fulde testpakke er grønne.
10. Den manuelle Supabase-røgtest er bestået, og testværdien er gendannet.

## Ikke omfattet

- Skift til muiDataGrid, DT eller en anden tabelmotor.
- Fork eller opgradering af excelR/jspreadsheet.
- Databaseversionering eller compare-and-swap.
- Bulktransaktion, auditlog og fortryd.
- Atomisk flercelle-paste.
- Nye opret- eller sletteflows ud over at bevare eksisterende funktionalitet.
- Ændringer i parquet- eller signal-gennemgangen.
