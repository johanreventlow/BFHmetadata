-- Opt-in pr. diagram-række: enhedens serie = egne rådata-rækker PLUS
-- oprulning af flaggede børn (i stedet for enten/eller). DEFAULT FALSE =
-- hidtidig adfærd for alle eksisterende rækker (fuldt bagudkompatibelt;
-- kilder der selv leverer flere org-niveauer må aldrig få TRUE — det ville
-- dobbelttælle). Anvendt på Supabase 2026-08-26 (migration
-- "aggreger_egne_og_boern") sammen med TRUE på de 9 afdelings-rækker for
-- indikator 2214 (henvist_til_rygestopkursus): org 2, 4, 5, 8, 10, 11, 12,
-- 18, 19.
ALTER TABLE "tblDiagrammer"
  ADD COLUMN "aggreger_egne_og_boern" BOOLEAN NOT NULL DEFAULT FALSE;

COMMENT ON COLUMN "tblDiagrammer"."aggreger_egne_og_boern" IS
  'TRUE: enhedens serie = egne rækker + oprulning af flaggede børn. FALSE (default): egne rækker vinder (børn ignoreres når egne data findes). Sæt kun TRUE når rådata tagger hver hændelse på præcis én enhed.';
