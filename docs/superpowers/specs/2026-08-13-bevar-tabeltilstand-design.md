# Bevar tabeltilstand efter redigering

## Formål

Når brugeren filtrerer en oversigt, åbner en post, gemmer eller lukker dialogen, skal vedkommende vende tilbage til samme arbejdsudsnit. Appen må ikke utilsigtet vise alle poster igen.

Løsningen omfatter både appens egne dropdownfiltre og DataTables-tilstand: global søgetekst, kolonne-sortering og aktuel side.

## Nuværende årsag

Diagram- og indikatoroversigten bygger dynamiske filterfelter med `renderUI()`. Efter en gemning genindlæses data, filter-UI'et bygges igen, og `selected` sættes fast til den tomme værdi ("Alle").

DT-tabellerne bliver tilsvarende genrenderet efter dataændringer. Uden eksplicit state-bevarelse opretter browseren en ny tabelvisning med standardsøgning, standardsortering og første side.

Signalreviewets filtre er statiske og nulstilles ikke af den beskrevne gem/luk-cyklus. Opslagstabeller uden filtrerings- eller navigationsfunktioner kræver ingen ny tilstand.

## Design

### Dropdownfiltre

En lille fælles hjælper vælger den værdi, der skal anvendes, når et dynamisk filter genopbygges:

1. Læs den aktuelle inputværdi isoleret, så en almindelig filterændring ikke i sig selv genopbygger samme kontrol.
2. Bevar værdien, hvis den stadig findes blandt de nye valgmuligheder.
3. Brug den tomme værdi ("Alle"), hvis den valgte værdi ikke længere findes.

Dette anvendes på diagramoversigtens indikator-, organisations-, datapakke- og datasætfilter samt indikatoroversigtens datapakkefilter.

Indikatoroversigtens datasætfilter er kaskaderende. Ved genindlæsning bevares det valgte datasæt kun, hvis det fortsat er en gyldig mulighed under den valgte datapakke. Hvis brugeren skifter datapakke, og det hidtidige datasæt ikke findes under den nye datapakke, nulstilles kun datasætfilteret til "Alle".

Statiske statusfiltre ændres ikke og beholder dermed Shiny-inputtets eksisterende værdi.

### DataTables-tilstand

Filtrerbare eller sideinddelte DT-tabeller konfigureres til at bevare deres klienttilstand gennem Shiny-genrenderinger i den aktuelle browserfane. Den bevarede tilstand omfatter:

- global søgetekst;
- aktiv kolonne og sorteringsretning;
- aktuel side, når den fortsat findes efter dataændringen.

Tilstanden afgrænses til den aktuelle Shiny/browser-session. Den skal ikke dukke op igen i en ny fane eller ved en senere appstart. Der bruges derfor ikke permanent lokal browserlagring.

Hvis en dataændring reducerer antallet af sider, skal DataTables lande på en gyldig side. Søgning og sortering bevares fortsat.

Tabeller uden søgning, sortering eller paging får ikke unødig state-håndtering.

## Dataflow

1. Brugeren vælger filtre og eventuelt søgning, sortering og side.
2. Brugeren åbner en post og gemmer en ændring.
3. Serveren genindlæser data fra databasen som hidtil.
4. Dynamiske dropdowns genopbygges med den hidtidige værdi, hvis den stadig er gyldig.
5. DT-tabellen genrenderes og gendanner sin sessionsbundne klienttilstand.
6. Brugeren ser det samme arbejdsudsnit med de opdaterede data.

Ingen database-, validerings- eller gemmeadfærd ændres.

## Fejl- og kanttilfælde

- Hvis den redigerede eller slettede post fjerner den sidste forekomst af en valgt dropdownværdi, skifter det berørte filter til "Alle" i stedet for at fastholde en ugyldig værdi.
- Hvis et kaskaderende datasæt ikke er gyldigt under en ny datapakke, nulstilles kun datasættet.
- Hvis en bevaret side ikke længere findes, korrigerer DataTables siden til et gyldigt indeks.
- Lukning af en dialog uden gemning må ikke påvirke nogen filter- eller tabeltilstand.

## Teststrategi

Regressionstests skal verificere:

- at hvert dynamisk diagramfilter gengives med den aktuelle gyldige værdi efter data-refresh;
- at indikatoroversigtens datapakke- og datasætfilter bevares efter refresh;
- at et bortfaldet valg nulstilles til "Alle";
- at datasætfilteret bevares eller nulstilles korrekt ved skift af datapakke;
- at relevante DT-konfigurationer bevarer søgning, sortering og side i sessionen uden permanent lagring;
- at eksisterende fokuserede modul- og app-UI-tests fortsat er grønne.

