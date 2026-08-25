-- Opt-in: BFHddl viser hierarki_navn_kort i chart-titlens datasaet-linje
-- naar flaget er sat (fx LUP, hvor det lange navn er autoritativt for
-- dataportal-generering men for langt til PDF-headeren).
-- Anvendt paa Supabase 2026-08-25 (migrering "add_brug_kort_navn_i_titel").
ALTER TABLE "tblIndikatorHierarki"
  ADD COLUMN IF NOT EXISTS brug_kort_navn_i_titel boolean NOT NULL DEFAULT false;

COMMENT ON COLUMN "tblIndikatorHierarki".brug_kort_navn_i_titel IS
  'Opt-in: vis hierarki_navn_kort i chart-titlens datasaet-linje (BFHddl build_chart_title). Det lange hierarki_navn forbliver autoritativt til dataportal-generering.';
