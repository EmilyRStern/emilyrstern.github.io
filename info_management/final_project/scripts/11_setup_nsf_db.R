# =============================================================================
# 11_setup_nsf_db.R
# Build the NSF-scoped 6-table schema (agencies, opportunities, change_log,
# recipients, awards, terminations) per er_diagram v2 (2026-04-30).
#
# Output: data/final/nsf_volatility.duckdb (empty tables; load step is separate)
#
# Schema deltas vs. the original 00_setup_db.R:
#   * NSF-only scope: terminations drops source_agency_id; agencies has 1 row
#   * opportunities + opportunity_number, close_date, plural cfda_numbers, is_active
#   * awards + period_of_perf_start/_end, awarding_sub_agency_name, total_outlayed_amount,
#             action_date, is_active
#   * terminations + reinstated, reinstatement_date, post_termination_deobligation,
#                    nsf_total_budget, estimated_remaining, project_title,
#                    directorate, division, nsf_program_name, is_active
# Safe to re-run (CREATE TABLE IF NOT EXISTS throughout).
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────
required_pkgs <- c("DBI", "duckdb")
missing_pkgs  <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
library(DBI); library(duckdb)

# ── 1. Resolve DB path ────────────────────────────────────────────────────────
.find_project_root <- function(start) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in seq_len(8)) {
    if (dir.exists(file.path(d, "data")) &&
        dir.exists(file.path(d, "data", "usaspending_raw"))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate project root containing data/usaspending_raw/")
}
PROJECT_ROOT <- tryCatch(
  .find_project_root(dirname(rstudioapi::getSourceEditorContext()$path)),
  error = function(e) .find_project_root(getwd())
)
DB_PATH <- file.path(PROJECT_ROOT, "data", "final", "nsf_volatility.duckdb")
dir.create(dirname(DB_PATH), showWarnings = FALSE, recursive = TRUE)

message("Building NSF schema at: ", DB_PATH)
con <- dbConnect(duckdb(), dbdir = DB_PATH)

# ── 2. agencies ───────────────────────────────────────────────────────────────
dbExecute(con, "
CREATE TABLE IF NOT EXISTS agencies (
  agency_id          INTEGER PRIMARY KEY,
  agency_code        VARCHAR NOT NULL UNIQUE,    -- e.g. 'NSF', 'NSF-BIO'
  agency_name        VARCHAR NOT NULL,
  agency_level       VARCHAR,                    -- 'top' | 'sub' | 'office'
  parent_agency_id   INTEGER REFERENCES agencies(agency_id)
);
")

# Seed: NSF top-level FIRST (so the self-FK can resolve), then directorates.
# DuckDB checks FKs row-by-row during a multi-row INSERT, so the parent must
# already exist in the same table before any child references it.
dbExecute(con, "
INSERT OR IGNORE INTO agencies (agency_id, agency_code, agency_name, agency_level, parent_agency_id)
VALUES (1, 'NSF', 'National Science Foundation', 'top', NULL);
")
dbExecute(con, "
INSERT OR IGNORE INTO agencies (agency_id, agency_code, agency_name, agency_level, parent_agency_id) VALUES
  (2,  'NSF-BIO', 'Biological Sciences',                                'sub', 1),
  (3,  'NSF-CSE', 'Computer and Information Science and Engineering',   'sub', 1),
  (4,  'NSF-EDU', 'STEM Education',                                     'sub', 1),
  (5,  'NSF-ENG', 'Engineering',                                        'sub', 1),
  (6,  'NSF-GEO', 'Geosciences',                                        'sub', 1),
  (7,  'NSF-MPS', 'Mathematical and Physical Sciences',                 'sub', 1),
  (8,  'NSF-SBE', 'Social, Behavioral, and Economic Sciences',          'sub', 1),
  (9,  'NSF-TIP', 'Technology, Innovation, and Partnerships',           'sub', 1),
  (10, 'NSF-OIA', 'Integrative Activities',                             'sub', 1),
  (11, 'NSF-OPP', 'Polar Programs',                                     'sub', 1),
  (12, 'NSF-OD',  'Office of the Director',                             'sub', 1);
")

# ── 3. opportunities ──────────────────────────────────────────────────────────
dbExecute(con, "
CREATE TABLE IF NOT EXISTS opportunities (
  opportunity_id            VARCHAR PRIMARY KEY,
  agency_id                 INTEGER REFERENCES agencies(agency_id),
  opportunity_number        VARCHAR,            -- human-readable, e.g. NSF-25-501
  title                     VARCHAR NOT NULL,
  cfda_numbers              VARCHAR,            -- comma-separated; multiple CFDAs allowed
  post_date                 DATE,
  close_date                DATE,
  archive_date              DATE,
  estimated_total_funding   DOUBLE,
  award_ceiling             DOUBLE,
  award_floor               DOUBLE,
  description               VARCHAR,
  is_active                 BOOLEAN DEFAULT TRUE,
  first_seen_snapshot_id    INTEGER,
  last_seen_snapshot_id     INTEGER
);
")

# ── 4. change_log (audit / weak entity) ──────────────────────────────────────
dbExecute(con, "
CREATE SEQUENCE IF NOT EXISTS change_log_seq;
")
dbExecute(con, "
CREATE TABLE IF NOT EXISTS change_log (
  change_id      INTEGER PRIMARY KEY DEFAULT nextval('change_log_seq'),
  opportunity_id VARCHAR NOT NULL REFERENCES opportunities(opportunity_id),
  snapshot_id    INTEGER,
  snapshot_date  TIMESTAMPTZ DEFAULT now(),
  change_type    VARCHAR NOT NULL CHECK (change_type IN ('ADDED','REMOVED','MODIFIED')),
  field_changed  VARCHAR,
  old_value      VARCHAR,
  new_value      VARCHAR,
  detected_at    TIMESTAMPTZ DEFAULT now()
);
")

# Snapshots metadata (referenced by change_log.snapshot_id; lightweight)
dbExecute(con, "
CREATE SEQUENCE IF NOT EXISTS snapshots_seq;
")
dbExecute(con, "
CREATE TABLE IF NOT EXISTS snapshots (
  snapshot_id   INTEGER PRIMARY KEY DEFAULT nextval('snapshots_seq'),
  snapshot_date TIMESTAMPTZ NOT NULL DEFAULT now(),
  source        VARCHAR DEFAULT 'grants.gov',
  record_count  INTEGER,
  notes         VARCHAR
);
")

# ── 5. recipients ─────────────────────────────────────────────────────────────
dbExecute(con, "
CREATE TABLE IF NOT EXISTS recipients (
  recipient_uei  VARCHAR PRIMARY KEY,
  recipient_name VARCHAR,
  parent_uei     VARCHAR REFERENCES recipients(recipient_uei),
  state_code     VARCHAR,
  city_name      VARCHAR,
  county_name    VARCHAR,
  zip_code       VARCHAR
);
")

# ── 6. awards ─────────────────────────────────────────────────────────────────
# award_unique_key (assistance_award_unique_key) is the PK because USAspending
# guarantees one per award. award_id_fain is a unique alternate key.
# ROWS HERE = AWARDS, NOT TRANSACTIONS. Loader must aggregate the 57,812
# transaction rows in usaspending_nsf.parquet down to ~37,535 awards.
dbExecute(con, "
CREATE TABLE IF NOT EXISTS awards (
  award_unique_key            VARCHAR PRIMARY KEY,
  award_id_fain               VARCHAR UNIQUE,
  recipient_uei               VARCHAR REFERENCES recipients(recipient_uei),
  awarding_agency_id          INTEGER REFERENCES agencies(agency_id),
  awarding_sub_agency_name    VARCHAR,            -- NSF directorate (BIO, CISE, ENG…)
  cfda_number                 VARCHAR,            -- single CFDA per award (e.g. 47.049)
  cfda_title                  VARCHAR,
  total_obligated_amount      DOUBLE,
  total_outlayed_amount       DOUBLE,
  period_of_perf_start        DATE,
  period_of_perf_end          DATE,
  action_date                 DATE,               -- max action_date across transactions
  description                 VARCHAR,
  is_active                   BOOLEAN,            -- period_of_perf_end >= today
  pulled_at                   TIMESTAMPTZ DEFAULT now()
);
")

# ── 6b. transactions (transaction-level USAspending data; ~57k NSF rows) ───
dbExecute(con, "
CREATE TABLE IF NOT EXISTS transactions (
  transaction_unique_key      VARCHAR PRIMARY KEY,
  award_unique_key            VARCHAR REFERENCES awards(award_unique_key),
  award_id_fain               VARCHAR,
  recipient_uei               VARCHAR,
  cfda_number                 VARCHAR,
  awarding_sub_agency_name    VARCHAR,
  action_date                 DATE,
  action_type_description     VARCHAR,
  federal_action_obligation   DOUBLE,
  assistance_type_description VARCHAR,
  transaction_description     VARCHAR
);
")

# ── 7. terminations ──────────────────────────────────────────────────────────
# NSF-only scope; source_agency_id removed (would always be NSF).
# award_id_fain joins to awards.award_id_fain (nsf_terminations.grant_id ≡ FAIN).
dbExecute(con, "
CREATE SEQUENCE IF NOT EXISTS terminations_seq;
")
dbExecute(con, "
CREATE TABLE IF NOT EXISTS terminations (
  termination_id                 INTEGER PRIMARY KEY DEFAULT nextval('terminations_seq'),
  award_id_fain                  VARCHAR REFERENCES awards(award_id_fain),
  project_title                  VARCHAR,
  current_status                 VARCHAR,                -- e.g. 'Terminated', 'Reinstated'
  latest_termination_date        DATE,
  reinstated                     BOOLEAN,
  reinstatement_date             DATE,
  post_termination_deobligation  DOUBLE,                 -- THE headline metric ($)
  nsf_total_budget               DOUBLE,
  estimated_remaining            DOUBLE,
  directorate                    VARCHAR,
  division                       VARCHAR,
  nsf_program_name               VARCHAR,
  is_active                      BOOLEAN                  -- NOT terminated, OR reinstated
);
")

# ── 8. Analytical views ───────────────────────────────────────────────────────

# Opportunities that disappeared (REMOVED) without a matching award — i.e.
# "rescinded" promises. Joins on cfda numbers, splitting comma-separated values.
dbExecute(con, "
CREATE OR REPLACE VIEW v_rescinded_opportunities AS
SELECT
  c.opportunity_id,
  o.opportunity_number,
  o.title,
  o.estimated_total_funding,
  o.cfda_numbers,
  c.detected_at AS removed_date,
  EXISTS (
    SELECT 1 FROM awards a
    WHERE a.cfda_number IN (
      SELECT TRIM(unnested) FROM unnest(string_split(o.cfda_numbers, ',')) AS t(unnested)
    )
  ) AS has_matching_award
FROM change_log c
JOIN opportunities o ON c.opportunity_id = o.opportunity_id
WHERE c.change_type = 'REMOVED';
")

# Disruption summary by directorate (the lead chart for the research question).
# ABS the deobligation column so the chart shows positive disruption-dollar
# values; USAspending records deobligations as negative.
dbExecute(con, "
CREATE OR REPLACE VIEW v_disruption_by_directorate AS
SELECT
  COALESCE(t.directorate, a.awarding_sub_agency_name) AS directorate,
  COUNT(*)                                            AS n_terminated_awards,
  ABS(COALESCE(SUM(t.post_termination_deobligation), 0)) AS total_deobligated,
  COALESCE(SUM(t.nsf_total_budget), 0)                AS total_at_risk_budget,
  SUM(CASE WHEN t.reinstated THEN 1 ELSE 0 END)       AS n_reinstated
FROM terminations t
LEFT JOIN awards a ON a.award_id_fain = t.award_id_fain
GROUP BY 1
ORDER BY total_deobligated DESC NULLS LAST;
")

# Disruption summary by recipient institution.
dbExecute(con, "
CREATE OR REPLACE VIEW v_disruption_by_recipient AS
SELECT
  r.recipient_uei,
  r.recipient_name,
  r.state_code,
  COUNT(t.termination_id)                                AS n_terminated_awards,
  ABS(COALESCE(SUM(t.post_termination_deobligation), 0)) AS total_deobligated
FROM terminations t
JOIN awards     a ON a.award_id_fain = t.award_id_fain
JOIN recipients r ON r.recipient_uei = a.recipient_uei
GROUP BY 1,2,3
ORDER BY total_deobligated DESC NULLS LAST;
")

# Per-award data-quality flags (NULL outlays = reporting lag, not $0)
dbExecute(con, "
CREATE OR REPLACE VIEW v_awards_with_quality AS
SELECT
  a.*,
  CASE WHEN a.total_outlayed_amount IS NULL THEN 'not_yet_reported'
       ELSE 'reported' END AS outlay_reporting_status,
  CASE
    WHEN a.period_of_perf_end IS NULL                              THEN 'unknown'
    WHEN a.period_of_perf_start > current_date                     THEN 'not_yet_started'
    WHEN a.period_of_perf_end >= current_date                      THEN 'in_progress'
    WHEN a.period_of_perf_end <  current_date - INTERVAL 1 YEAR    THEN 'ended_over_1yr'
    ELSE                                                                'ended_under_1yr'
  END AS pop_status,
  (a.action_date >= current_date - INTERVAL 90 DAY) AS action_in_reporting_lag_window
FROM awards a;
")

# % disbursed (excludes NULL outlays; clamps $1-$2 rounding noise)
dbExecute(con, "
CREATE OR REPLACE VIEW v_disbursement_progress AS
SELECT
  award_unique_key, award_id_fain, awarding_sub_agency_name,
  total_obligated_amount,
  LEAST(total_outlayed_amount, total_obligated_amount) AS effective_outlay,
  CASE WHEN total_obligated_amount > 0 THEN
    LEAST(100, ROUND(100.0 *
      LEAST(total_outlayed_amount, total_obligated_amount)
      / total_obligated_amount, 1))
  END AS pct_disbursed
FROM awards
WHERE total_outlayed_amount IS NOT NULL
  AND total_obligated_amount IS NOT NULL
  AND total_obligated_amount > 0;
")

# Active-funding portfolio summary
dbExecute(con, "
CREATE OR REPLACE VIEW v_active_funding AS
SELECT
  awarding_sub_agency_name AS directorate,
  COUNT(*)                                                          AS n_active_awards,
  SUM(total_obligated_amount)                                       AS total_obligated_active,
  COUNT(*) FILTER (WHERE total_outlayed_amount IS NOT NULL)         AS n_with_outlay_reported,
  SUM(total_outlayed_amount)                                        AS total_outlaid_reported,
  ROUND(100.0 * COUNT(*) FILTER (WHERE total_outlayed_amount IS NOT NULL)
              / NULLIF(COUNT(*), 0), 1)                             AS pct_outlay_reported
FROM awards
WHERE is_active = true
GROUP BY 1
ORDER BY total_obligated_active DESC NULLS LAST;
")

# Monthly obligations by directorate — the disruption-over-time chart
dbExecute(con, "
CREATE OR REPLACE VIEW v_monthly_obligations AS
SELECT
  date_trunc('month', action_date)                      AS month,
  awarding_sub_agency_name                              AS directorate,
  COUNT(*)                                              AS n_transactions,
  SUM(federal_action_obligation)                        AS net_obligation,
  SUM(CASE WHEN federal_action_obligation >= 0
           THEN federal_action_obligation ELSE 0 END)   AS gross_obligated,
  ABS(SUM(CASE WHEN federal_action_obligation < 0
           THEN federal_action_obligation ELSE 0 END))  AS gross_deobligated,
  COUNT(*) FILTER (WHERE federal_action_obligation < 0) AS n_deobligations
FROM transactions
WHERE action_date IS NOT NULL
GROUP BY 1, 2
ORDER BY 1, 2;
")

# ── 9. Done ──────────────────────────────────────────────────────────────────
tabs <- dbListTables(con)
message("\n=== NSF schema ready ===")
message("Tables: ", paste(sort(tabs), collapse = ", "))
for (t in sort(tabs)) {
  cols <- dbGetQuery(con, sprintf("DESCRIBE %s", t))
  message(sprintf("  %s (%d cols)", t, nrow(cols)))
}
dbDisconnect(con, shutdown = TRUE)
message("DB closed: ", DB_PATH)
