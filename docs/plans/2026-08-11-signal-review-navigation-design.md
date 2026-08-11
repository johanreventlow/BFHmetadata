# Design: Signal-gennemgang — diagram-liste, uden-signal-visning og fase-statistik

**Dato:** 2026-08-11
**Status:** Godkendt (tilgang A)
**Berører:** `R/mod_signal_review.R`, `R/fct_scan.R` (evt.), tests

## Baggrund

Signal-gennemgang-siden kan i dag kun bladre sekventielt gennem diagrammer
med signal. Tre brugerønsker:

1. **Klikbar diagram-liste** — spring direkte til et vilkårligt diagram i
   stedet for kun Forrige/Næste.
2. **Checkbox "vis også uden signal"** — gennemgå også ok-scannede
   diagrammer uden Anhøj-signal.
3. **Fase-statistik pr. fase** — serielængde og antal kryds (observeret)
   samt forventet maks. serielængde og forventet min. antal kryds, som i
   PDF-rapporterne (BFHddl/BFHcharts, `bfh_qic`-beregningen).

## Beslutninger (afklaret med bruger)

- Diagram-listen placeres som **kollapsibelt panel i sidebar** (accordion
  under scan-knappen).
- Checkbox-scope: **kun ok-scannede** diagrammer (status `"ok"`).
  `ingen_data`/`fejl` holdes ude af liste og bladring — de har ingen
  tegnbar graf — men tælles fortsat i scan-opsummeringen.
- Valgt tilgang: **A — én scannet liste + afledt visning** (se nedenfor).

## Fravalgte tilgange

- **B: separat "alle diagrammer"-browser ved siden af `signal_list`** —
  to parallelle navigationsmodeller giver dobbelt UI-logik og høj risiko
  for stale-cursor-bugs (netop dét modulet tidligere har haft Task 7-guards
  imod).
- **C: checkbox som scan-parameter (re-scan ved skifte)** — unødvendigt:
  scan-cachen indeholder allerede alle ok-resultater; re-scan af ~600
  diagrammer er dårlig UX for et rent visningsfilter.

## Design (tilgang A)

### 1. State-model

- `scanned_list` (reactiveVal, erstatter `signal_list` som sandhedskilde):
  df med alle kandidat-rækker der scannedes OK — både med og uden signal —
  plus kolonnerne `signal` (logical) og `status`. Vokser løbende under det
  progressive scan, præcis som `signal_list` gør i dag
  (`.scan_process_group` tilføjer rækker i cand-rækkefølge, så cursor
  aldrig forskubbes af nye rækker).
- `view_list` (reactive): `scanned_list` filtreret efter checkboxen —
  fra: kun `signal == TRUE`; til: alle med `status == "ok"`.
- `cursor` peger ind i `view_list`.
- **Cursor-bevarelse ved checkbox-skifte:** det aktuelle diagram_id slås
  op i den nye visning; findes det, sættes cursor til dets nye position,
  ellers cursor = 1. Brugeren mister ikke sin plads midt i gennemgangen.

### 2. Sidebar-liste (kollapsibelt panel)

- `bslib::accordion` i sidebar under scan-knappen.
- Én række pr. diagram i `view_list`: statusikon (⚠ signal / ✓ ok uden
  signal) + trunkeret "indikator · org". Klik → `cursor(i)`. Aktuel række
  markeres visuelt.
- Checkboxen "Vis også diagrammer uden signal" placeres lige over listen.
- Listen opdateres løbende mens scan kører (samme reaktive kilde).

### 3. Fase-statistik-tabel

- Kompakt tabel mellem graf og faseskift-knapper. Én række pr. fase fra
  `sc$summary` (BFHcharts `format_qic_summary` — samme kilde som
  PDF-rapporternes SPC-tabel):
  - fase
  - antal observationer (anvendelige)
  - serielængde: "observeret / maks. X" (`laengste_loeb` /
    `laengste_loeb_max`)
  - antal kryds: "observeret / min. Y" (`antal_kryds` / `antal_kryds_min`)
  - signalmarkering (`anhoej_signal`)
- **Preview-konsistens:** ved forhåndsvisning af faseskift genberegnes
  tabellen fra preview-`qic_result` (samme genberegning som grafen), så
  tallene altid matcher den viste graf.

### 4. Fejlhåndtering

- Tom `scanned_list`/tom visning degraderer som i dag ("0 diagrammer…").
- Fase-tabellen tåler `summary = NULL` (fejl-scannede) → tom/skjult tabel.
- DB-kald forbliver `safe_operation`-værnede (jf. fix
  `fix/signal-review-db-connection-resilience`).

### 5. Tests (testServer)

- Checkbox-filtrering: fra → kun signal-diagrammer; til → alle ok-scannede.
- Cursor-bevarelse ved checkbox-toggle (samme diagram forbliver aktivt).
- Klik-navigation fra sidebar-listen sætter cursor korrekt.
- Fase-tabellens indhold: observeret/forventet-kolonner matcher
  `sc$summary`.
- Preview: fase-tabellen afspejler preview-genberegningen.
- Eksisterende tests justeres hvor `signal_list`-semantikken ændres;
  modulets eksponerede retur-liste beholder bagudkompatible navne hvor
  muligt (testene afgør).

## Konsekvenser

- Moderat refaktor af `mod_signal_review.R`s navigations-state; alle
  læsere af `signal_list` skal justeres til `view_list`/`scanned_list`.
- Ingen ændringer i DB-lag eller scan-motor påkrævet — cachen indeholder
  allerede alt (`.scan_process_group` gemmer `res` for hvert diagram
  uanset signal, og `ctx$sig`/`ctx$status` kender alle kandidaters status).
- Ingen breaking changes i public API (modulet er internt).
