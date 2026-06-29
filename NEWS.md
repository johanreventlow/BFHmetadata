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
