# =============================================================================
# 13_load_postgres.R
# Populate the Neon Postgres schema from the NSF-filtered outputs in
# data/final/. Uses DuckDB locally as the fastest reader/aggregator for the
# parquet, then streams data frames to Postgres via dbWriteTable.
#
# Load order respects FKs:
#   1. recipients     (distinct UEIs from usaspending_nsf.parquet)
#   2. opportunities  (from grants_gov_nsf.csv)
#   3. awards         (aggregate transactions → awards from parquet)
#   4. terminations   (from nsf_terminations.csv, joined to awards.award_id_fain)
#
# agencies is seeded by the DDL script. snapshots / change_log are populated
# by future Grants.gov diff runs, not here.
#
# Safe to re-run: TRUNCATE … RESTART IDENTITY CASCADE clears the data tables
# (NOT agencies) before inserting, so each run lands a clean copy.
# =============================================================================

# ── 0. Packages + env ─────────────────────────────────────────────────────────
required_pkgs <- c("RPostgres", "DBI", "duckdb", "glue", "dplyr")
missing_pkgs  <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
suppressPackageStartupMessages({
  library(RPostgres); library(DBI); library(duckdb); library(glue); library(dplyr)
})

.find_project_root <- function(start) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in seq_len(8)) {
    if (dir.exists(file.path(d, "data", "usaspending_raw"))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate project root")
}
PROJECT_ROOT <- tryCatch(
  .find_project_root(dirname(rstudioapi::getSourceEditorContext()$path)),
  error = function(e) .find_project_root(getwd())
)
ren <- file.path(PROJECT_ROOT, ".Renviron")
if (file.exists(ren)) readRenviron(ren)

FINAL <- file.path(PROJECT_ROOT, "data", "final")
PARQUET <- file.path(FINAL, "usaspending_nsf.parquet")
GRANTS  <- file.path(FINAL, "grants_gov_nsf.csv")
TERMS   <- file.path(FINAL, "nsf_terminations.csv")

stopifnot(file.exists(PARQUET), file.exists(GRANTS), file.exists(TERMS))

parse_pg_url <- function(url) {
  m <- regmatches(url, regexec(
    "^postgres(?:ql)?://([^:]+):([^@]+)@([^:/]+)(?::(\\d+))?/([^?]+)(?:\\?(.*))?$", url))[[1]]
  list(user = m[2], password = m[3], host = m[4],
       port = if (nzchar(m[5])) as.integer(m[5]) else 5432L, dbname = m[6])
}
p <- parse_pg_url(Sys.getenv("NEON_DB_URL"))
pg <- dbConnect(Postgres(),
  host = p$host, port = p$port, dbname = p$dbname,
  user = p$user, password = p$password, sslmode = "require")
message("Connected to Neon: ", p$host, "/", p$dbname)

duck <- dbConnect(duckdb(), ":memory:")
dbExecute(duck, "PRAGMA threads = 8;")

# ── Helper: clean truncate of data tables (keep agencies) ────────────────────
message("Clearing existing data tables (keeping agencies)...")
dbExecute(pg, "TRUNCATE transactions, terminations, awards, change_log,
                       snapshots, opportunities, recipients
               RESTART IDENTITY CASCADE;")

# ── 1. recipients ────────────────────────────────────────────────────────────
message("\n[1/4] recipients ─────────────────────────────────────────────")
t0 <- Sys.time()
recipients_df <- dbGetQuery(duck, glue("
  SELECT DISTINCT
    recipient_uei,
    -- pick the most-recently-seen name/place (window over action_date)
    FIRST(recipient_name        ORDER BY action_date DESC) AS recipient_name,
    NULL::VARCHAR                                          AS parent_uei,
    FIRST(recipient_state_code  ORDER BY action_date DESC) AS state_code,
    FIRST(recipient_city_name   ORDER BY action_date DESC) AS city_name,
    FIRST(recipient_county_name ORDER BY action_date DESC) AS county_name,
    FIRST(recipient_zip_code    ORDER BY action_date DESC) AS zip_code
  FROM read_parquet('{PARQUET}')
  WHERE recipient_uei IS NOT NULL AND recipient_uei <> ''
  GROUP BY recipient_uei
"))
message(glue("  {format(nrow(recipients_df), big.mark=',')} distinct recipients staged"))
dbWriteTable(pg, "recipients", recipients_df, append = TRUE, row.names = FALSE)
message(glue("  Inserted in {round(as.numeric(difftime(Sys.time(), t0, units='secs')), 1)}s"))

# ── 2. opportunities ─────────────────────────────────────────────────────────
message("\n[2/4] opportunities ──────────────────────────────────────────")
t0 <- Sys.time()
opps_df <- dbGetQuery(duck, glue("
  SELECT
    CAST(OpportunityID AS VARCHAR)            AS opportunity_id,
    1                                          AS agency_id,  -- NSF top
    OpportunityNumber                          AS opportunity_number,
    OpportunityTitle                           AS title,
    CFDANumbers                                AS cfda_numbers,
    PostDate                                   AS post_date,
    TRY_CAST(CloseDate    AS DATE)             AS close_date,
    TRY_CAST(ArchiveDate  AS DATE)             AS archive_date,
    TRY_CAST(EstimatedTotalProgramFunding AS DOUBLE) AS estimated_total_funding,
    TRY_CAST(AwardCeiling AS DOUBLE)           AS award_ceiling,
    TRY_CAST(AwardFloor   AS DOUBLE)           AS award_floor,
    Description                                AS description,
    is_active,
    NULL::INTEGER                              AS first_seen_snapshot_id,
    NULL::INTEGER                              AS last_seen_snapshot_id
  FROM read_csv_auto('{GRANTS}', SAMPLE_SIZE=-1, HEADER=true, IGNORE_ERRORS=true)
  WHERE OpportunityID IS NOT NULL
  -- collapse Forecast/Synopsis duplicates: keep the synopsis row when both exist
  QUALIFY ROW_NUMBER() OVER (
    PARTITION BY OpportunityID
    ORDER BY CASE WHEN LOWER(record_type) LIKE '%synopsis%' THEN 0 ELSE 1 END,
             LastUpdatedDate DESC NULLS LAST
  ) = 1
"))
message(glue("  {format(nrow(opps_df), big.mark=',')} unique opportunities staged"))
dbWriteTable(pg, "opportunities", opps_df, append = TRUE, row.names = FALSE)
message(glue("  Inserted in {round(as.numeric(difftime(Sys.time(), t0, units='secs')), 1)}s"))

# ── 3. awards (aggregate transactions → awards) ──────────────────────────────
message("\n[3/4] awards ─────────────────────────────────────────────────")
t0 <- Sys.time()

# Directorate lookup from CFDA (the parquet doesn't carry sub-agency for chunks).
# Reference: NSF Assistance Listings catalog.
awards_df <- dbGetQuery(duck, glue("
  WITH tx AS (
    SELECT
      assistance_award_unique_key,
      award_id_fain,
      recipient_uei,
      recipient_name,
      cfda_number,
      cfda_title,
      total_obligated_amount,
      total_outlayed_amount_for_overall_award,
      period_of_performance_start_date,
      period_of_performance_current_end_date,
      action_date,
      transaction_description
    FROM read_parquet('{PARQUET}')
    WHERE assistance_award_unique_key IS NOT NULL
      AND recipient_uei IS NOT NULL AND recipient_uei <> ''
  ),
  agg AS (
    SELECT
      assistance_award_unique_key                     AS award_unique_key,
      ANY_VALUE(award_id_fain)                        AS award_id_fain,
      ANY_VALUE(recipient_uei)                        AS recipient_uei,
      1                                                AS awarding_agency_id,
      CASE ANY_VALUE(cfda_number)
        WHEN '47.041' THEN 'ENG' WHEN '47.049' THEN 'MPS'
        WHEN '47.050' THEN 'GEO' WHEN '47.070' THEN 'CISE'
        WHEN '47.074' THEN 'BIO' WHEN '47.075' THEN 'SBE'
        WHEN '47.076' THEN 'EDU' WHEN '47.078' THEN 'OPP'
        WHEN '47.079' THEN 'OD'  WHEN '47.083' THEN 'OIA'
        WHEN '47.084' THEN 'TIP' ELSE 'OTHER'
      END                                             AS awarding_sub_agency_name,
      ANY_VALUE(cfda_number)                          AS cfda_number,
      ANY_VALUE(cfda_title)                           AS cfda_title,
      MAX(total_obligated_amount)                     AS total_obligated_amount,
      MAX(total_outlayed_amount_for_overall_award)    AS total_outlayed_amount,
      MIN(period_of_performance_start_date)           AS period_of_perf_start,
      MAX(period_of_performance_current_end_date)     AS period_of_perf_end,
      MAX(action_date)                                AS action_date,
      ANY_VALUE(transaction_description)              AS description,
      (MAX(period_of_performance_current_end_date) >= current_date) AS is_active
    FROM tx
    GROUP BY assistance_award_unique_key
  )
  SELECT * FROM agg
  -- award_id_fain UNIQUE constraint: drop rows with NULL FAIN to avoid PG error
  WHERE award_id_fain IS NOT NULL
"))
message(glue("  Aggregated to {format(nrow(awards_df), big.mark=',')} unique awards (PK: award_unique_key)"))

# Some FAINs may collide across multiple unique_keys (rare). Dedup just in case.
awards_df <- awards_df |> distinct(award_id_fain, .keep_all = TRUE)
message(glue("  After award_id_fain dedup: {format(nrow(awards_df), big.mark=',')}"))

# Drop awards whose recipient_uei isn't in recipients (safety; FK).
recipient_uei_set <- dbGetQuery(pg, "SELECT recipient_uei FROM recipients")$recipient_uei
n_before <- nrow(awards_df)
awards_df <- awards_df |> filter(recipient_uei %in% recipient_uei_set)
if (nrow(awards_df) < n_before)
  message(glue("  Dropped {n_before - nrow(awards_df)} awards with no matching recipient"))

awards_df$pulled_at <- Sys.time()
dbWriteTable(pg, "awards", awards_df, append = TRUE, row.names = FALSE)
message(glue("  Inserted in {round(as.numeric(difftime(Sys.time(), t0, units='secs')), 1)}s"))

# ── 3b. transactions (full transaction-level data; ~57k rows) ───────────────
message("\n[3b/4] transactions ──────────────────────────────────────────")
t0 <- Sys.time()
tx_df <- dbGetQuery(duck, glue("
  SELECT
    assistance_transaction_unique_key AS transaction_unique_key,
    assistance_award_unique_key       AS award_unique_key,
    award_id_fain,
    recipient_uei,
    cfda_number,
    CASE cfda_number
      WHEN '47.041' THEN 'ENG' WHEN '47.049' THEN 'MPS'
      WHEN '47.050' THEN 'GEO' WHEN '47.070' THEN 'CISE'
      WHEN '47.074' THEN 'BIO' WHEN '47.075' THEN 'SBE'
      WHEN '47.076' THEN 'EDU' WHEN '47.078' THEN 'OPP'
      WHEN '47.079' THEN 'OD'  WHEN '47.083' THEN 'OIA'
      WHEN '47.084' THEN 'TIP' ELSE 'OTHER'
    END                               AS awarding_sub_agency_name,
    action_date,
    action_type_description,
    federal_action_obligation,
    assistance_type_description,
    transaction_description
  FROM read_parquet('{PARQUET}')
  WHERE assistance_transaction_unique_key IS NOT NULL
"))
message(glue("  {format(nrow(tx_df), big.mark=',')} transactions staged"))

# FK to awards.award_unique_key — drop tx whose award didn't make it in
award_keys <- dbGetQuery(pg, "SELECT award_unique_key FROM awards")$award_unique_key
n_before <- nrow(tx_df)
tx_df <- tx_df |> filter(award_unique_key %in% award_keys) |>
                  distinct(transaction_unique_key, .keep_all = TRUE)
message(glue("  After FK + PK dedup: {format(nrow(tx_df), big.mark=',')} (dropped {n_before - nrow(tx_df)})"))
dbWriteTable(pg, "transactions", tx_df, append = TRUE, row.names = FALSE)
message(glue("  Inserted in {round(as.numeric(difftime(Sys.time(), t0, units='secs')), 1)}s"))

# ── 4. terminations ──────────────────────────────────────────────────────────
message("\n[4/4] terminations ───────────────────────────────────────────")
t0 <- Sys.time()
terms_df <- dbGetQuery(duck, glue("
  SELECT
    CAST(grant_id AS VARCHAR)                         AS award_id_fain,
    project_title,
    status                                            AS current_status,
    termination_date                                  AS latest_termination_date,
    reinstated,
    TRY_CAST(reinstatement_date AS DATE)              AS reinstatement_date,
    TRY_CAST(post_termination_deobligation AS DOUBLE) AS post_termination_deobligation,
    TRY_CAST(nsf_total_budget AS DOUBLE)              AS nsf_total_budget,
    TRY_CAST(estimated_remaining AS DOUBLE)           AS estimated_remaining,
    directorate,
    division,
    nsf_program_name,
    is_active
  FROM read_csv_auto('{TERMS}', SAMPLE_SIZE=-1, HEADER=true, IGNORE_ERRORS=true)
"))
message(glue("  {format(nrow(terms_df), big.mark=',')} termination rows staged"))

# FK to awards.award_id_fain: NULL out ones that don't match (otherwise FK fails).
fain_set <- dbGetQuery(pg, "SELECT award_id_fain FROM awards")$award_id_fain
n_match  <- sum(terms_df$award_id_fain %in% fain_set, na.rm = TRUE)
message(glue("  {format(n_match, big.mark=',')} of those match a row in awards"))
terms_df$award_id_fain[!terms_df$award_id_fain %in% fain_set] <- NA
dbWriteTable(pg, "terminations", terms_df, append = TRUE, row.names = FALSE)
message(glue("  Inserted in {round(as.numeric(difftime(Sys.time(), t0, units='secs')), 1)}s"))

# ── 5. Done ──────────────────────────────────────────────────────────────────
counts <- dbGetQuery(pg, "
  SELECT 'agencies'      AS tbl, COUNT(*) AS n FROM agencies      UNION ALL
  SELECT 'recipients',         COUNT(*)        FROM recipients    UNION ALL
  SELECT 'opportunities',      COUNT(*)        FROM opportunities UNION ALL
  SELECT 'awards',             COUNT(*)        FROM awards        UNION ALL
  SELECT 'transactions',       COUNT(*)        FROM transactions  UNION ALL
  SELECT 'terminations',       COUNT(*)        FROM terminations  UNION ALL
  SELECT 'change_log',         COUNT(*)        FROM change_log    UNION ALL
  SELECT 'snapshots',          COUNT(*)        FROM snapshots
  ORDER BY tbl;")
message("\n=== Final row counts on Neon ===")
print(counts)

dbDisconnect(duck, shutdown = TRUE)
dbDisconnect(pg)
message("Done.")
