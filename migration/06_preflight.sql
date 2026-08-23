-- migration/06_preflight.sql
-- Aborterer med en fejl, hvis data ikke kan bære Fase 2's constraints.
-- Oprydning er en SEPARAT, reviewet beslutning — ikke noget der improviseres her.
DO $$
DECLARE n bigint;
BEGIN
  -- a) dubletter på teknisk navn (tom streng tæller IKKE som NULL)
  SELECT count(*) INTO n FROM (
    SELECT "indikator_navn_teknisk" FROM "tblIndikatorer"
    WHERE "indikator_navn_teknisk" IS NOT NULL
    GROUP BY 1 HAVING count(*) > 1) x;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % dublet-grupper på indikator_navn_teknisk', n; END IF;

  -- b) orphans på datakilde
  SELECT count(*) INTO n FROM "tblIndikatorer" i
  LEFT JOIN "tblDatakilder" d ON d."Id" = i."datakilde"
  WHERE i."datakilde" IS NOT NULL AND d."Id" IS NULL;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % orphans på datakilde', n; END IF;

  -- c) junction: NULL-komponenter
  SELECT count(*) INTO n FROM "tblForbindIndikatorerFaggrupper"
  WHERE "indikator_id" IS NULL OR "faggruppe_id" IS NULL;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % NULL-komponenter i faggrupper', n; END IF;

  SELECT count(*) INTO n FROM "tblForbindIndikatorerDataprodukter"
  WHERE "indikator_id" IS NULL OR "dataprodukt_id" IS NULL;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % NULL-komponenter i dataprodukter', n; END IF;

  SELECT count(*) INTO n FROM "tblForbindIndikatorerOrganisation"
  WHERE "indikator_id" IS NULL OR "organisations_id" IS NULL;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % NULL-komponenter i organisation', n; END IF;

  -- d) junction: forældreløse komponenter på begge sider
  SELECT count(*) INTO n FROM "tblForbindIndikatorerFaggrupper" j
  LEFT JOIN "tblIndikatorer" i ON i."id" = j."indikator_id"
  WHERE j."indikator_id" IS NOT NULL AND i."id" IS NULL;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % indikator-orphans i faggrupper', n; END IF;

  SELECT count(*) INTO n FROM "tblForbindIndikatorerFaggrupper" j
  LEFT JOIN "tblFaggrupper" p ON p."Id" = j."faggruppe_id"
  WHERE j."faggruppe_id" IS NOT NULL AND p."Id" IS NULL;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % faggruppe-orphans i faggrupper', n; END IF;

  SELECT count(*) INTO n FROM "tblForbindIndikatorerDataprodukter" j
  LEFT JOIN "tblIndikatorer" i ON i."id" = j."indikator_id"
  WHERE j."indikator_id" IS NOT NULL AND i."id" IS NULL;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % indikator-orphans i dataprodukter', n; END IF;

  SELECT count(*) INTO n FROM "tblForbindIndikatorerDataprodukter" j
  LEFT JOIN "tblDataprodukter" p ON p."Id" = j."dataprodukt_id"
  WHERE j."dataprodukt_id" IS NOT NULL AND p."Id" IS NULL;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % dataprodukt-orphans i dataprodukter', n; END IF;

  SELECT count(*) INTO n FROM "tblForbindIndikatorerOrganisation" j
  LEFT JOIN "tblIndikatorer" i ON i."id" = j."indikator_id"
  WHERE j."indikator_id" IS NOT NULL AND i."id" IS NULL;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % indikator-orphans i organisation', n; END IF;

  SELECT count(*) INTO n FROM "tblForbindIndikatorerOrganisation" j
  LEFT JOIN "tblOrganisationStruktur" p ON p."Id" = j."organisations_id"
  WHERE j."organisations_id" IS NOT NULL AND p."Id" IS NULL;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % organisations-orphans i organisation', n; END IF;

  -- e) junction: dublet-par
  SELECT count(*) INTO n FROM (
    SELECT 1 FROM "tblForbindIndikatorerFaggrupper"
    GROUP BY "indikator_id", "faggruppe_id" HAVING count(*) > 1) x;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % dublet-par i faggrupper', n; END IF;

  SELECT count(*) INTO n FROM (
    SELECT 1 FROM "tblForbindIndikatorerDataprodukter"
    GROUP BY "indikator_id", "dataprodukt_id" HAVING count(*) > 1) x;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % dublet-par i dataprodukter', n; END IF;

  SELECT count(*) INTO n FROM (
    SELECT 1 FROM "tblForbindIndikatorerOrganisation"
    GROUP BY "indikator_id", "organisations_id" HAVING count(*) > 1) x;
  IF n > 0 THEN RAISE EXCEPTION 'PREFLIGHT: % dublet-par i organisation', n; END IF;

  RAISE NOTICE 'PREFLIGHT OK — alle constraints kan lægges på.';
END $$;
