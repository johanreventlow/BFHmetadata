# Server-side indikatorsøgning i diagramformularen

## Formål

Diagramformularen skal fortsat kunne vælge blandt alle indikatorer, inklusive
inaktive, uden at hele indikatorlisten indlejres i modalens HTML. Dermed skal
Shinys advarsel om mange valgmuligheder for `diagram-d_indikator` forsvinde,
og formularen skal åbne hurtigere ved store indikatorlister.

## Design

`.diagram_form_ui()` viser indikatorfeltet som et tomt `selectizeInput`, bortset
fra den aktuelt valgte indikator ved redigering. Efter at modalen er vist,
registrerer det kaldende servermodul hele indikatorlisten med
`updateSelectizeInput(..., server = TRUE)`. Selectize søger herefter i listen på
serveren i stedet for at modtage alle muligheder som HTML.

Det eksisterende valg bevares ved redigering. Ved oprettelse starter feltet med
den tomme mulighed `(vælg)`. Alle indikatorer fra `diagram_form_options()` er
søgbare; der indføres ingen filtrering på aktiv-status.

Den delte formular bruges fra to flows:

- Diagram-menuens opret/redigér-dialog initialiserer server-side valg efter
  `showModal()`.
- Indikator-dialogens diagramflow har indikatoren låst. Det fortsætter med et
  skjult input med den aktuelle værdi og et deaktiveret tekstfelt, og behøver
  ikke den store søgbare valgliste.

Database-accessors, validering, duplikatkontrol og gemmeformat ændres ikke.

## Fejl- og kanttilfælde

Hvis et eksisterende diagram peger på en indikator, som ikke findes i den
aktuelle valgliste, bevares id'et og vises med en neutral fallback-label, så en
åbning af dialogen ikke utilsigtet nulstiller værdien. Tomt valg normaliseres
fortsat til `NA_integer_` af den eksisterende formularindsamling.

## Test

Regressionstests skal bevise, at:

- en stor indikatorliste ikke indlejres i modalens HTML og ikke udløser
  large-options-advarslen;
- servermodulet initialiserer indikatorfeltet med server-side Selectize;
- alle indikatorer, inklusive en inaktiv fixture, findes i server-valgene;
- eksisterende og ukendte valgte indikator-id'er bevares;
- det låste indikatorflow fortsat sender den korrekte indikator ved gem.

