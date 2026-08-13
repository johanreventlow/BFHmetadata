-- Opt-in-flag for nulfyldning af tomme perioder i hændelsestællinger.
-- Læses af BFHddl::fill_empty_periods() ved rendering (se BFHddl
-- docs/DATA_CONVENTIONS.md §3b): TRUE for tælle-indikatorer hvor
-- "ingen række = 0 hændelser"; FALSE (default) hvor en tom periode
-- betyder "ingen data/måling". Kør mod Supabase FØR BFHmetadata
-- opgraderes — app'ens indikator-SELECT medtager kolonnen.
ALTER TABLE "tblIndikatorer"
  ADD COLUMN IF NOT EXISTS "nulfyld_tomme_perioder" BOOLEAN NOT NULL DEFAULT FALSE;
