# =============================================================================
# 00_setup_db.R
# Creates the DuckDB database and all tables for the Federal Grant Volatility
# Tracking project (EPPS 6354 – Emily Stern, Spring 2026).
#
# Run this ONCE before running any ingestion scripts.
# Safe to re-run: uses CREATE TABLE IF NOT EXISTS throughout.
# =============================================================================

# ── 0. Package check ──────────────────────────────────────────────────────────
required_pkgs <- c("duckdb", "DBI")
missing_pkgs  <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}
library(DBI)
library(duckdb)

# ── 1. Connect (creates file if it does not exist) ────────────────────────────
DB_PATH <- here::here("data", "grants_volatility.duckdb")
# If you are not using {here}, replace with an explicit absolute path, e.g.:
# DB_PATH <- "C:/Users/yourname/.../final_project/data/grants_volatility.duckdb"

# Resolve path relative to this script's location if 'here' is unavailable
if (!requireNamespace("here", quietly = TRUE)) {
  DB_PATH <- file.path(dirname(rstudioapi::getSourceEditorContext()$path),
                       "..", "data", "grants_volatility.duckdb")
}

message("Opening DuckDB at: ", DB_PATH)
con <- dbConnect(duckdb(), dbdir = DB_PATH)

# ── 2. agencies ───────────────────────────────────────────────────────────────
dbExecute(con, "
CREATE TABLE IF NOT EXISTS agencies (
  agency_id   INTEGER PRIMARY KEY,  -- auto-assigned surrogate key
  agency_code VARCHAR NOT NULL UNIQUE,  -- e.g. 'HHS', 'DOE'
  agency_name VARCHAR NOT NULL
);
")

# ── 3. opportunities ──────────────────────────────────────────────────────────
dbExecute(con, "
CREATE TABLE IF NOT EXISTS opportunities (
  opportunity_id             VARCHAR PRIMARY KEY,  -- Grants.gov numeric ID stored as text
  opportunity_number         VARCHAR,              -- human-readable, e.g. HHS-2025-ACL-001
  title                      VARCHAR NOT NULL,
  agency_code                VARCHAR,
  agency_name                VARCHAR,
  opportunity_status         VARCHAR,              -- posted | forecasted | closed | archived
  post_date                  DATE,
  close_date                 DATE,
  archive_date               DATE,
  estimated_total_funding    DOUBLE,
  award_ceiling              DOUBLE,
  award_floor                DOUBLE,
  cfda_numbers               VARCHAR,              -- comma-sep when multiple; primary join key
  funding_instrument_type    VARCHAR,              -- G=Grant, CA=Coop Agreement, etc.
  funding_activity_category  VARCHAR,              -- primary category code
  eligible_applicants        VARCHAR,
  description                VARCHAR,
  first_seen_snapshot_id     INTEGER,
  last_seen_snapshot_id      INTEGER,
  is_active                  BOOLEAN DEFAULT TRUE  -- set FALSE when REMOVED detected
);
")

# ── 4. categories ─────────────────────────────────────────────────────────────
dbExecute(con, "
CREATE TABLE IF NOT EXISTS categories (
  category_code VARCHAR PRIMARY KEY,  -- Grants.gov code, e.g. 'HL', 'ED', 'NR'
  category_name VARCHAR NOT NULL
);
")

# Seed with known Grants.gov funding activity category codes
dbExecute(con, "
INSERT OR IGNORE INTO categories VALUES
  ('AG', 'Agriculture'),
  ('AR', 'Arts'),
  ('BC', 'Business and Commerce'),
  ('CD', 'Community Development'),
  ('CP', 'Consumer Protection'),
  ('DPR', 'Disaster Prevention and Relief'),
  ('ED', 'Education'),
  ('ELT', 'Employment, Labor and Training'),
  ('EN', 'Energy'),
  ('ENV', 'Environment'),
  ('FN', 'Food and Nutrition'),
  ('HL', 'Health'),
  ('HO', 'Housing'),
  ('HU', 'Humanities'),
  ('IIJ', 'Income Security and Social Services'),
  ('IS', 'Information and Statistics'),
  ('LJL', 'Law, Justice and Legal Services'),
  ('NR', 'Natural Resources'),
  ('OZ', 'Opportunity Zone Benefits'),
  ('RD', 'Regional Development'),
  ('ST', 'Science and Technology and Other Research and Development'),
  ('T', 'Transportation'),
  ('ACA', 'Affordable Care Act'),
  ('RA', 'Recovery Act'),
  ('O', 'Other');
")

# ── 5. opportunity_categories ─────────────────────────────────────────────────
dbExecute(con, "
CREATE TABLE IF NOT EXISTS opportunity_categories (
  opportunity_id VARCHAR NOT NULL REFERENCES opportunities(opportunity_id),
  category_code  VARCHAR NOT NULL REFERENCES categories(category_code),
  PRIMARY KEY (opportunity_id, category_code)
);
")

# ── 6. snapshots ──────────────────────────────────────────────────────────────
dbExecute(con, "
CREATE TABLE IF NOT EXISTS snapshots (
  snapshot_id    INTEGER PRIMARY KEY,  -- auto-increment via sequence
  snapshot_date  TIMESTAMPTZ NOT NULL DEFAULT now(),
  source         VARCHAR DEFAULT 'grants.gov',
  record_count   INTEGER,
  notes          VARCHAR
);
")

# ── 7. change_log ─────────────────────────────────────────────────────────────
# change_type values:
#   ADDED    – opportunity appeared in this snapshot but not the previous one
#   REMOVED  – opportunity was in the previous snapshot but not this one
#   MODIFIED – opportunity exists in both snapshots but a field value changed
dbExecute(con, "
CREATE TABLE IF NOT EXISTS change_log (
  change_id       INTEGER PRIMARY KEY,
  opportunity_id  VARCHAR NOT NULL,
  snapshot_id     INTEGER NOT NULL REFERENCES snapshots(snapshot_id),
  change_type     VARCHAR NOT NULL CHECK (change_type IN ('ADDED','REMOVED','MODIFIED')),
  field_changed   VARCHAR,        -- NULL for ADDED/REMOVED; field name for MODIFIED
  old_value       VARCHAR,        -- NULL for ADDED
  new_value       VARCHAR,        -- NULL for REMOVED
  detected_at     TIMESTAMPTZ DEFAULT now()
);
")

# ── 8. awards (USASpending.gov) ───────────────────────────────────────────────
dbExecute(con, "
CREATE TABLE IF NOT EXISTS awards (
  award_id              VARCHAR PRIMARY KEY,  -- USASpending unique award key
  cfda_number           VARCHAR,              -- links to opportunities.cfda_numbers
  assistance_type_code  VARCHAR,              -- 02=Block Grant, 03=Formula, 04=Project, 05=Other
  recipient_name        VARCHAR,
  recipient_uei         VARCHAR,              -- Unique Entity Identifier (replaced DUNS)
  recipient_state       VARCHAR,
  awarding_agency_name  VARCHAR,
  awarding_agency_code  VARCHAR,
  awarding_sub_agency   VARCHAR,
  award_amount          DOUBLE,
  face_value_of_loan    DOUBLE,
  award_date            DATE,
  period_of_perf_start  DATE,
  period_of_perf_end    DATE,
  description           VARCHAR,
  pulled_at             TIMESTAMPTZ DEFAULT now()
);
")

# ── 9. Analytical views ───────────────────────────────────────────────────────

# missing_grants: opportunities that disappeared without a matching award
dbExecute(con, "
CREATE OR REPLACE VIEW missing_grants AS
SELECT
  c.opportunity_id,
  o.opportunity_number,
  o.title,
  o.agency_name,
  o.funding_activity_category,
  c.detected_at                  AS removed_date,
  o.estimated_total_funding,
  o.award_ceiling,
  o.cfda_numbers,
  o.post_date,
  o.close_date,
  -- flag whether ANY award exists with a matching CFDA number
  EXISTS (
    SELECT 1 FROM awards a
    WHERE a.cfda_number IN (
      -- split comma-separated CFDA numbers for multi-CFDA opportunities
      SELECT TRIM(unnested)
      FROM unnest(string_split(o.cfda_numbers, ',')) AS t(unnested)
    )
  ) AS has_matching_award
FROM change_log c
JOIN opportunities o ON c.opportunity_id = o.opportunity_id
WHERE c.change_type = 'REMOVED'
ORDER BY c.detected_at DESC;
")

# confirmed_rescissions: REMOVED opportunities with NO matching award
dbExecute(con, "
CREATE OR REPLACE VIEW confirmed_rescissions AS
SELECT * FROM missing_grants WHERE has_matching_award = FALSE;
")

# change_summary: counts per snapshot for the timeline chart
dbExecute(con, "
CREATE OR REPLACE VIEW change_summary AS
SELECT
  s.snapshot_date::DATE  AS snap_date,
  c.change_type,
  COUNT(*)               AS n
FROM change_log c
JOIN snapshots s ON c.snapshot_id = s.snapshot_id
GROUP BY s.snapshot_date::DATE, c.change_type
ORDER BY snap_date;
")

# ── 10. Done ──────────────────────────────────────────────────────────────────
message("\n=== Schema created successfully ===")
message("Tables: ", paste(dbListTables(con), collapse = ", "))
dbDisconnect(con, shutdown = TRUE)
message("Database connection closed.")
