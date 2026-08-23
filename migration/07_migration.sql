-- migration/07_migration.sql
-- Fase 2.3. Køres EFTER 06_preflight.sql og EFTER verificeret backup.
-- Alt i én transaktion → ingen delvis skemaændring.
-- (Undtagelse: DROP INDEX af Fase 1-index'erne ligger til sidst og er bevidst
--  placeret EFTER at UNIQUE er oprettet.)
BEGIN;

-- --- 1) Tidsstempler ------------------------------------------------------
ALTER TABLE "tblIndikatorer"
  ADD COLUMN IF NOT EXISTS "created_at" timestamptz NOT NULL DEFAULT now(),
  ADD COLUMN IF NOT EXISTS "updated_at" timestamptz NOT NULL DEFAULT now(),
  ADD COLUMN IF NOT EXISTS "updated_by" text;

COMMENT ON COLUMN "tblIndikatorer"."updated_by" IS
  'Udfyldes KUN af bulk-redigering og fortryd. Modal-gem, celle-redigering og '
  'soft-delete rører den ikke — kolonnen er derfor ikke komplet sporbarhed.';

CREATE OR REPLACE FUNCTION set_updated_at() RETURNS trigger AS $$
BEGIN
  NEW."updated_at" := now();
  RETURN NEW;
END $$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_indikatorer_updated_at ON "tblIndikatorer";
CREATE TRIGGER trg_indikatorer_updated_at
  BEFORE UPDATE ON "tblIndikatorer"
  FOR EACH ROW EXECUTE FUNCTION set_updated_at();

-- --- 2) Unikhed + FK ------------------------------------------------------
-- Eksakt unikhed (ikke normaliseret): værdier der kun adskiller sig i
-- store/små bogstaver eller mellemrum accepteres fortsat. Bevidst valg.
ALTER TABLE "tblIndikatorer"
  ADD CONSTRAINT uq_indikatorer_navn_teknisk UNIQUE ("indikator_navn_teknisk");

ALTER TABLE "tblIndikatorer"
  ADD CONSTRAINT fk_indikatorer_datakilde
  FOREIGN KEY ("datakilde") REFERENCES "tblDatakilder" ("Id");

-- --- 3) Junction: NOT NULL FØR UNIQUE ------------------------------------
-- NOT NULL er ikke valgfrit: Postgres regner NULL-værdier som indbyrdes
-- forskellige, så UNIQUE på nullable kolonner forhindrer IKKE dubletter.
ALTER TABLE "tblForbindIndikatorerFaggrupper"
  ALTER COLUMN "indikator_id" SET NOT NULL,
  ALTER COLUMN "faggruppe_id" SET NOT NULL,
  ADD CONSTRAINT uq_forbind_faggrupper UNIQUE ("indikator_id", "faggruppe_id");

ALTER TABLE "tblForbindIndikatorerDataprodukter"
  ALTER COLUMN "indikator_id" SET NOT NULL,
  ALTER COLUMN "dataprodukt_id" SET NOT NULL,
  ADD CONSTRAINT uq_forbind_dataprodukter UNIQUE ("indikator_id", "dataprodukt_id");

ALTER TABLE "tblForbindIndikatorerOrganisation"
  ALTER COLUMN "indikator_id" SET NOT NULL,
  ALTER COLUMN "organisations_id" SET NOT NULL,
  ADD CONSTRAINT uq_forbind_organisation UNIQUE ("indikator_id", "organisations_id");

-- --- 4) Ændringslog i ikke-eksponeret schema -----------------------------
-- IKKE i public: tabellen indeholder brugernavn og før/efter-værdier, og nye
-- public-tabeller kan blive tilgængelige via Supabases Data API.
CREATE SCHEMA IF NOT EXISTS audit;
REVOKE ALL ON SCHEMA audit FROM anon, authenticated;

CREATE TABLE IF NOT EXISTS audit."tblAendringslog" (
  "id"                 bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  "batch_id"           uuid        NOT NULL,   -- bevidst UDEN default
  "tidspunkt"          timestamptz NOT NULL DEFAULT now(),
  "bruger"             text,
  "tabel"              text        NOT NULL,
  "post_id"            bigint      NOT NULL,
  "kolonne"            text        NOT NULL,
  "vaerdi_foer"        jsonb       NOT NULL,
  "vaerdi_efter"       jsonb       NOT NULL,
  "vaerdi_type"        text        NOT NULL,
  "fortrudt_tidspunkt" timestamptz,
  "fortrudt_af"        text
);

COMMENT ON COLUMN audit."tblAendringslog"."batch_id" IS
  'Ingen kolonne-default med vilje: en default ville give nyt UUID pr. række '
  'og ødelægge grupperingen. Genereres én gang pr. batch af skriveren.';

CREATE INDEX IF NOT EXISTS ix_aendringslog_batch ON audit."tblAendringslog" ("batch_id");
CREATE INDEX IF NOT EXISTS ix_aendringslog_post  ON audit."tblAendringslog" ("tabel", "post_id");

REVOKE ALL ON audit."tblAendringslog" FROM anon, authenticated;

COMMIT;

-- --- 5) Drop Fase 1-index'erne — FØRST nu, hvor UNIQUE er oprettet ------
-- Composite UNIQUE (indikator_id, <parent_id>) giver samme prefix-index.
-- Fejlede COMMIT ovenfor, køres dette IKKE, og index'erne bliver stående.
DROP INDEX CONCURRENTLY IF EXISTS ix_forbind_faggrupper_indikator;
DROP INDEX CONCURRENTLY IF EXISTS ix_forbind_dataprodukter_indikator;
DROP INDEX CONCURRENTLY IF EXISTS ix_forbind_organisation_indikator;
