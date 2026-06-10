# BFHmetadata 0.3.0

## Nye features
* Startside med flise-grid hvor man vælger tabel/område at arbejde med.
* Generisk inline-redigering af de 6 simple opslagstabeller (Faggrupper,
  Datakilder, Dataprodukter, Diagramtyper, Organisations-niveauer,
  Indikator-niveauer): redigér celler direkte i tabellen, tilføj og slet rækker.
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
