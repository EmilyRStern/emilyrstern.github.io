-- =============================================================================
-- schema.sql
-- Federal Grant Disruption Database — minimal 6-table schema
-- EPPS 6354 — Emily Stern — Spring 2026
--
-- Research question:
--   How much federal grant funding has been disrupted since the 2025
--   administration change — either rescinded at the opportunity level
--   (Grants.gov listing vanished) or terminated at the award level
--   (per the five agency termination feeds) — and which agencies and
--   recipients are most affected?
--
-- Every table in this schema earns its keep with respect to that question.
-- Anything that did not directly support the research question (small code
-- vocabularies, multi-valued account fields, transaction-level modifications,
-- the parsed event log, the separate snapshots table) was dropped or inlined.
--
-- Target engine: DuckDB.
-- Load order follows FK dependencies top-to-bottom.
-- =============================================================================

-- 1. agencies — federal agencies with self-referencing hierarchy --------------
-- Used by: opportunities, awards (×2 roles), terminations.
-- Why it earns its keep: the research question asks "which agencies are
-- most affected" — canonical agency naming is needed for clean GROUP BY.
-- The self-FK lets us roll NIH/CDC/SAMHSA up to HHS without separate tables.
CREATE TABLE IF NOT EXISTS agencies (
  agency_id          INTEGER PRIMARY KEY,
  agency_code        VARCHAR NOT NULL UNIQUE,
  agency_name        VARCHAR NOT NULL,
  agency_level       VARCHAR NOT NULL CHECK (agency_level IN ('agency','sub_agency','office')),
  parent_agency_id   INTEGER REFERENCES agencies(agency_id)
);

-- 2. recipients — UEI-keyed organizations receiving awards --------------------
-- Used by: awards.
-- Why it earns its keep: the research question asks "which recipients are
-- most affected." The self-FK on parent_uei lets us roll campus-level
-- recipients up to system-level (e.g., all UC campuses to UC Office of the
-- President). State is inlined as a string (a 50-row dimension table for
-- states isn't worth its join cost at this scale).
CREATE TABLE IF NOT EXISTS recipients (
  recipient_uei          VARCHAR PRIMARY KEY,
  recipient_name         VARCHAR NOT NULL,
  recipient_parent_uei   VARCHAR REFERENCES recipients(recipient_uei),
  state_code             VARCHAR,
  city_name              VARCHAR,
  congressional_district VARCHAR
);

-- 3. opportunities — Grants.gov listings (what was promised) ------------------
-- One row per opportunity_id from Grants.gov. Volatility on this table is
-- captured in change_log (Type-2 SCD).
-- cfda_number is stored inline as VARCHAR. The source data sometimes lists
-- multiple CFDAs in a comma-separated string; the basic schema captures only
-- the primary one. Multi-CFDA opportunities (a small minority) lose secondary
-- CFDAs in this version — a documented 1NF compromise for schema simplicity.
CREATE TABLE IF NOT EXISTS opportunities (
  opportunity_id            VARCHAR PRIMARY KEY,
  opportunity_number        VARCHAR,
  title                     VARCHAR NOT NULL,
  agency_id                 INTEGER REFERENCES agencies(agency_id),
  cfda_number               VARCHAR,                      -- primary CFDA only
  cfda_title                VARCHAR,                      -- denormalized; updated when Assistance Listings refresh
  opportunity_status        VARCHAR,
  post_date                 DATE,
  close_date                DATE,
  archive_date              DATE,
  estimated_total_funding   DOUBLE,
  award_ceiling             DOUBLE,
  award_floor               DOUBLE,
  description               VARCHAR,
  is_active                 BOOLEAN DEFAULT TRUE
);

-- 4. awards — USA Spending awards (what got funded) ---------------------------
-- One row per assistance_award_unique_key. Award-level rather than
-- transaction-level: total_obligated_amount and total_outlayed_amount are
-- the cumulative figures from USA Spending. Transaction-level detail is
-- not needed to answer "was this funding disrupted."
-- award_id_fain is the human-readable identifier and the join key from
-- the termination CSVs.
CREATE TABLE IF NOT EXISTS awards (
  assistance_award_unique_key VARCHAR PRIMARY KEY,
  award_id_fain               VARCHAR UNIQUE,
  recipient_uei               VARCHAR REFERENCES recipients(recipient_uei),
  awarding_agency_id          INTEGER REFERENCES agencies(agency_id),
  funding_agency_id           INTEGER REFERENCES agencies(agency_id),
  opportunity_id              VARCHAR REFERENCES opportunities(opportunity_id),
  cfda_number                 VARCHAR,
  cfda_title                  VARCHAR,
  award_date                  DATE,
  period_of_perf_start        DATE,
  period_of_perf_end          DATE,
  total_obligated_amount      DOUBLE,
  total_outlayed_amount       DOUBLE,
  pop_state_code              VARCHAR,
  description                 VARCHAR,
  pulled_at                   TIMESTAMPTZ DEFAULT now()
);

-- 5. terminations — unified across the 5 source agencies ---------------------
-- The dependent variable for the research question. The five source CSVs
-- (NSF, NIH, EPA, CDC, SAMHSA) project into this single shape with
-- source_agency_id as the discriminator. The (source_agency_id, grant_id)
-- pair is unique by construction.
-- The source CSVs also have a free-text event_history field with a
-- chronological log of events. For the basic schema, this is kept as a
-- single notes column rather than parsed into a separate events table.
-- award_id_fain FK is unique-where-not-null with explicit handling of
-- the multi-fiscal-year FAIN-collision case in joining queries.
CREATE TABLE IF NOT EXISTS terminations (
  termination_id                INTEGER PRIMARY KEY,
  source_agency_id              INTEGER NOT NULL REFERENCES agencies(agency_id),
  grant_id                      VARCHAR NOT NULL,
  award_id_fain                 VARCHAR REFERENCES awards(award_id_fain),
  current_status                VARCHAR,                  -- 'Terminated' | 'At-risk' | 'Restored' | 'Possibly Reinstated' | ...
  has_been_terminated           BOOLEAN,
  has_been_reinstated           BOOLEAN,
  has_been_frozen               BOOLEAN,
  first_termination_date        DATE,
  latest_termination_date       DATE,
  latest_reinstatement_date     DATE,
  award_value                   DOUBLE,                   -- $ at risk
  award_outlaid                 DOUBLE,                   -- $ already paid out
  award_remaining               DOUBLE,                   -- $ remaining at termination
  post_termination_deobligation DOUBLE,                   -- $ formally clawed back
  cfda_number                   VARCHAR,
  notes                         VARCHAR,                  -- includes the source event_history text
  usaspending_url               VARCHAR,
  pulled_at                     TIMESTAMPTZ DEFAULT now(),
  UNIQUE (source_agency_id, grant_id)
);

-- 6. change_log — Type-2 SCD audit trail for opportunities -------------------
-- The rescission-detection mechanism. Every detected ADDED, REMOVED, or
-- MODIFIED is appended with the snapshot date and (for MODIFIED rows) the
-- field name and old/new values. Never overwritten.
-- The proposal had a separate `snapshots` table; in the basic schema the
-- snapshot_date is inlined here. If snapshot-level metadata becomes needed
-- (record counts, ingestion notes), promoting it to a separate table is
-- a one-step migration.
CREATE TABLE IF NOT EXISTS change_log (
  change_id        INTEGER PRIMARY KEY,
  opportunity_id   VARCHAR NOT NULL REFERENCES opportunities(opportunity_id),
  snapshot_date    TIMESTAMPTZ NOT NULL,
  change_type      VARCHAR NOT NULL CHECK (change_type IN ('ADDED','REMOVED','MODIFIED')),
  field_changed    VARCHAR,
  old_value        VARCHAR,
  new_value        VARCHAR,
  detected_at      TIMESTAMPTZ DEFAULT now()
);


-- =============================================================================
-- ANALYTICAL VIEWS — directly aligned with the research question
-- =============================================================================

-- v_disruption_by_agency — answers "which agencies are most affected"
CREATE OR REPLACE VIEW v_disruption_by_agency AS
SELECT
  ag.agency_name,
  COUNT(DISTINCT cl.opportunity_id) FILTER (
    WHERE cl.change_type = 'REMOVED'
  )                                                                AS rescinded_opportunities,
  SUM(CASE WHEN cl.change_type = 'REMOVED'
           THEN o.estimated_total_funding END)                     AS rescinded_dollars,
  COUNT(DISTINCT tm.termination_id) FILTER (
    WHERE tm.has_been_terminated
  )                                                                AS terminated_awards,
  SUM(CASE WHEN tm.has_been_terminated
           THEN tm.award_value END)                                AS terminated_dollars
FROM agencies ag
LEFT JOIN opportunities o   ON o.agency_id        = ag.agency_id
LEFT JOIN change_log    cl  ON cl.opportunity_id  = o.opportunity_id
LEFT JOIN terminations  tm  ON tm.source_agency_id = ag.agency_id
GROUP BY ag.agency_name;

-- v_disruption_by_recipient — answers "which recipients are most affected"
CREATE OR REPLACE VIEW v_disruption_by_recipient AS
SELECT
  COALESCE(parent.recipient_name, child.recipient_name) AS recipient_or_parent,
  COUNT(DISTINCT a.award_id_fain)                       AS terminated_awards,
  SUM(tm.award_value)                                   AS at_risk_dollars,
  SUM(tm.post_termination_deobligation)                 AS deobligated_dollars
FROM terminations tm
JOIN awards     a       ON a.award_id_fain         = tm.award_id_fain
JOIN recipients child   ON child.recipient_uei     = a.recipient_uei
LEFT JOIN recipients parent ON parent.recipient_uei = child.recipient_parent_uei
WHERE tm.has_been_terminated = TRUE
GROUP BY recipient_or_parent;

-- v_confirmed_rescissions — opportunities that vanished without being awarded
CREATE OR REPLACE VIEW v_confirmed_rescissions AS
SELECT
  o.opportunity_id,
  o.title,
  ag.agency_name,
  cl.snapshot_date AS removed_date,
  o.cfda_number,
  o.estimated_total_funding,
  -- Did any USA Spending award appear under this CFDA after the post date?
  EXISTS (
    SELECT 1 FROM awards a
    WHERE a.cfda_number = o.cfda_number
      AND a.award_date >= o.post_date
  )                                                   AS has_matching_award,
  -- Did any source agency log a termination on a matching award?
  EXISTS (
    SELECT 1
    FROM awards a
    JOIN terminations tm ON tm.award_id_fain = a.award_id_fain
    WHERE a.cfda_number = o.cfda_number
      AND tm.has_been_terminated = TRUE
  )                                                   AS source_agency_terminated
FROM change_log cl
JOIN opportunities o ON o.opportunity_id = cl.opportunity_id
LEFT JOIN agencies ag ON ag.agency_id    = o.agency_id
WHERE cl.change_type = 'REMOVED';
