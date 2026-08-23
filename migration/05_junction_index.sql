-- migration/05_junction_index.sql
-- Fase 1.7 — index på junction-tabellernes indikator_id.
--
-- Baggrund: de tre junction-tabeller har hverken PK eller index. Hver
-- modal-åbning slår relationer op med WHERE "indikator_id" = $1, hvilket
-- i dag koster en sekventiel gennemsøgning — 3.866 rækker alene i
-- tblForbindIndikatorerOrganisation (målt 2026-08-20).
--
-- CONCURRENTLY: undgår at blokere writes mens index'et bygges. Kan derfor
-- IKKE køre inde i en transaktion — hvert statement køres for sig.
--
-- BEMÆRK: Fase 2's composite UNIQUE (indikator_id, <parent_id>) giver samme
-- prefix-index og gør disse tre overflødige. De DROPPES i 07_migration.sql —
-- men først EFTER at UNIQUE er oprettet og verificeret i kataloget.
-- Fejler UNIQUE, bliver disse index'er stående som det, de er: en gevinst
-- der ikke koster noget.

CREATE INDEX CONCURRENTLY IF NOT EXISTS ix_forbind_faggrupper_indikator
  ON "tblForbindIndikatorerFaggrupper" ("indikator_id");

CREATE INDEX CONCURRENTLY IF NOT EXISTS ix_forbind_dataprodukter_indikator
  ON "tblForbindIndikatorerDataprodukter" ("indikator_id");

CREATE INDEX CONCURRENTLY IF NOT EXISTS ix_forbind_organisation_indikator
  ON "tblForbindIndikatorerOrganisation" ("indikator_id");
