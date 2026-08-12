# Inline-redigering af organisationshierarkiet

## Formål

Organisationshierarkiet under menupunktet Organisation skal kunne redigeres
direkte i tabellen. Brugeren skal ikke længere åbne hver node i en modal for at
ændre dens felter. Oprettelse og sletning skal fortsat ske med knapper og
dialoger.

Løsningen bygger videre på den eksisterende `DT`-tabel og det generiske
hierarki-modul. `rhandsontable` indføres ikke.

## Brugeroplevelse

Tabellen beholder hierarkiets depth-first-rækkefølge og visuelle indrykning.
Hver række repræsenterer fortsat én node og er knyttet til nodens skjulte,
stabile database-id.

Følgende felter kan redigeres inline:

- teknisk navn;
- langt navn;
- kort navn;
- forælder;
- niveau.

Tekstfelter redigeres som tekstceller. Forælder og niveau redigeres med
dropdowns, der viser læsbare labels, men sender de tilhørende id'er til
serveren. En redigering afsluttes efter DT's normale celle-redigeringsflow.

Når en celleændring er gemt, vises en kort succesnotifikation. Der indføres
ingen separat Gem-knap og ingen lokal samling af ugemte ændringer.

Knappen **Ny node** og den eksisterende oprettelsesdialog bevares. En
eksisterende række kan markeres, hvorefter knappen **Slet valgt** åbner en
bekræftelsesdialog. Sletning udføres først efter bekræftelse.

## Dataflow ved inline-redigering

1. Klienten sender række, kolonne og ny celleværdi til Shiny.
2. Serveren finder noden via det stabile id og oversætter den viste kolonne til
   konfigurationsfeltet i `HIERARCHY_TABLES`.
3. Den nye værdi normaliseres til den forventede datatype. Tom tekst bliver
   `NA_character_`; tom forælder bliver `NA_integer_`; niveau og valgt
   forælder oversættes til integer-id'er.
4. Serveren validerer ændringen mod den senest indlæste komplette node.
5. Serveren kalder den eksisterende `db$update_node()` med alle redigerbare
   værdier for noden, så databasegrænsefladen ikke skal ændres.
6. Efter en vellykket skrivning genindlæses noderne fra databasen. Tabellen
   beregner hierarkiets rækkefølge og indrykning igen, hvilket også håndterer en
   ændret forælder eller et ændret navn.

Kun én celleændring behandles per event. Shiny-sessionens normale sekventielle
eventbehandling sikrer, at to hurtige ændringer ikke skriver ud fra hver sin
lokale kopi uden genindlæsning imellem.

## Validering

Det lange navn, som er modulets `display_col`, er fortsat obligatorisk. Den
tekniske og korte betegnelse må følge de nuværende regler og kan derfor være
tomme, hvis databasen tillader det.

Ved ændring af forælder gælder følgende:

- tom værdi betyder rodnode;
- noden selv kan ikke vælges;
- en efterkommer kan ikke vælges, fordi det ville skabe en cyklus;
- dropdownens valgmuligheder udelader nodens egen subtree;
- serveren gentager cykluskontrollen uafhængigt af klientens valgmuligheder.

Niveaukontrollen bevarer den eksisterende semantik: et niveau, der ikke er
dybere end forælderens, giver en blød advarsel, men ændringen gemmes.

## Fejlhåndtering og konsistens

Databaseværdien er autoritativ. Ved validerings- eller databasefejl
genindlæses noderne, så cellen og resten af tabellen gendannes til den senest
gemte tilstand. Brugeren får en tydelig advarselsnotifikation. Tabellen må ikke
efterlade en værdi, der kun findes i browseren.

Ved en vellykket ændring invaliderer den eksisterende cache-wrapper fortsat de
relevante organisationsdata gennem `db$update_node()`.

Sletning bevarer de nuværende sikkerhedsregler: en node med børn kan ikke
slettes, og databasefejl fra øvrige referencer vises som en advarsel. Hvis den
markerede node ikke længere findes, genindlæses tabellen uden sletning.

## Modulafgrænsning

Inline-funktionaliteten implementeres konfigurationsdrevet i
`mod_hierarchy`, så modulets felter fortsat kommer fra `HIERARCHY_TABLES`.
Den aktiveres for organisationsstrukturen uden at kopiere et særskilt
organisationsmodul. Hjælpefunktioner til kolonnemapping, værdinormalisering og
validering holdes rene, hvor det er praktisk, så de kan enhedstestes uden en
kørende browser.

Databaselaget og SQL-builderne ændres kun, hvis implementeringen afdækker et
konkret behov. Det forventede design genbruger den eksisterende komplette
`update_node(id, values)`-operation.

## Teststrategi

Eksisterende tests for hierarkiets rækkefølge, oprettelse, flytning og sletning
skal fortsat bestå. Nye tests skal mindst dække:

- mapping fra en redigeret tabelkolonne til korrekt databasefelt;
- straks-gemning af hvert af de tre tekstfelter;
- straks-gemning af niveau og forælder som id'er;
- flytning til rod;
- afvisning af egen node og efterkommer som forælder;
- obligatorisk langt navn;
- blød niveauadvarsel med gennemført gemning;
- genindlæsning og brugerbesked ved databasefejl;
- rækkevalg og sletning gennem bekræftelsesflowet;
- bevaret oprettelsesdialog.

Der tilføjes desuden en UI-test, som fastholder, at hierarkitabellen er
inline-redigerbar, og at knapperne **Ny node** og **Slet valgt** findes.

## Afgrænsning

Følgende er ikke en del af ændringen:

- masseindsættelse eller indsætning fra regneark;
- undo/redo på tværs af gemte ændringer;
- batch-gemning;
- drag-and-drop af hierarkinoder;
- samtidighedslåsning mellem flere brugeres sessioner;
- udskiftning af `DT` med en ny tabelafhængighed.
