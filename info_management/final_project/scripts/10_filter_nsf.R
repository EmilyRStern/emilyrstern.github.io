# =============================================================================
# 10_filter_nsf.R
# Filter every raw data source down to NSF-only records and write them into
# data/final/. Designed to be run once after all raw data is in place.
#
# Strategy:
#   * Use DuckDB everywhere we can. DuckDB lazy-scans Parquet/CSV files and
#     pushes the WHERE clause down to file readers, so for the multi-GB
#     USAspending data only the NSF rows are ever materialized in RAM.
#   * Filter rule (per Emily, 2026-04-30):
#         awarding_agency_name ILIKE '%national science foundation%'
#         (cleaned parquet drops awarding_agency_code in some chunks, so we
#          rely on the canonical agency name match.)
#     For Grants.gov / AssistanceListings we also accept CFDA prefix '47.'
#     because every NSF program lives under ALN/CFDA 47.xxx.
#   * Other-agency files (epa_/cdc_/nih_/samhsa_terminations) and non-agency
#     context files (HPI, multiTimeline, 118 doc list, govt PDFs) are skipped.
#   * IMPORTANT: We only scan _staging_chunks (canonical, all 15 source files).
#     _staging_per_file is a duplicate of one source file and would double-count.
#
# Each output gets an `is_active` column with source-appropriate logic:
#   * USAspending awards: period_of_performance_current_end_date >= today
#   * Grants.gov opps  : opportunity_status IN ('posted','forecasted')
#   * AssistanceListings: TRUE if Program Title is not the placeholder
#   * nsf_terminations : NOT terminated, OR reinstated
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────
required_pkgs <- c("duckdb", "DBI", "fs", "glue")
missing_pkgs  <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(DBI); library(duckdb); library(fs); library(glue)

# ── 1. Paths ──────────────────────────────────────────────────────────────────
# Resolve PROJECT_ROOT robustly: walk up from the script (or cwd) until we find
# the data/ folder. Avoids being fooled by RStudio's "active document" path.
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

DATA_DIR      <- file.path(PROJECT_ROOT, "data")
FINAL_DIR     <- file.path(DATA_DIR, "final")
USAS_PARQUET  <- file.path(DATA_DIR, "usaspending_raw", "cleaned_outputs",
                           "_staging_chunks")
GRANTS_CSV    <- file.path(DATA_DIR, "grants_data",
                           "grants_complete_dataset.csv")
ALN_CSV       <- file.path(DATA_DIR,
                           "AssistanceListings_DataGov_PUBLIC_CURRENT.csv")
NSF_TERMS_CSV <- file.path(DATA_DIR, "nsf_terminations.csv")

dir_create(FINAL_DIR)

message("Project root : ", PROJECT_ROOT)
message("Final folder : ", FINAL_DIR)
message("")

# ── 2. Open in-memory DuckDB ─────────────────────────────────────────────────
con <- dbConnect(duckdb(), dbdir = ":memory:")
dbExecute(con, "PRAGMA threads = 8;")
dbExecute(con, "PRAGMA memory_limit = '8GB';")

# ── 3. USAspending → NSF parquet ─────────────────────────────────────────────
message("━━━ USAspending → NSF parquet ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
t0 <- Sys.time()

usas_glob <- file.path(USAS_PARQUET, "**", "*.parquet")
out_parquet <- file.path(FINAL_DIR, "usaspending_nsf.parquet")

dbExecute(con, glue("
  COPY (
    SELECT *,
           (period_of_performance_current_end_date >= current_date) AS is_active
    FROM read_parquet('{usas_glob}', union_by_name = true)
    WHERE LOWER(awarding_agency_name) LIKE '%national science foundation%'
  ) TO '{out_parquet}' (FORMAT PARQUET, COMPRESSION ZSTD);
"))

n_usas <- dbGetQuery(con, glue(
  "SELECT COUNT(*) AS n FROM read_parquet('{out_parquet}')"))$n
message(glue("  NSF rows written: {format(n_usas, big.mark=',')}"))

qa <- dbGetQuery(con, glue("
  SELECT COALESCE(source_file, 'unknown')                             AS source_file,
         COUNT(*)                                                     AS nsf_rows,
         SUM(CASE WHEN is_active THEN 1 ELSE 0 END)                   AS active_rows,
         ROUND(SUM(TRY_CAST(federal_action_obligation AS DOUBLE)), 2) AS total_obligation
  FROM read_parquet('{out_parquet}')
  GROUP BY 1
  ORDER BY 1;
"))
write.csv(qa, file.path(FINAL_DIR, "usaspending_nsf_summary.csv"),
          row.names = FALSE)
print(qa)
message(glue("  USAspending filter took {round(as.numeric(difftime(Sys.time(), t0, units='secs')),1)}s"))

# ── 4. Grants.gov full extract → NSF CSV ─────────────────────────────────────
message("\n━━━ Grants.gov full extract → NSF CSV ━━━━━━━━━━━━━━━━━━━━━━━━━")
t0 <- Sys.time()

if (file_exists(GRANTS_CSV)) {
  out_grants <- file.path(FINAL_DIR, "grants_gov_nsf.csv")
  dbExecute(con, glue("
    COPY (
      SELECT *,
             (LOWER(COALESCE(record_type,'')) LIKE '%synopsis%'
              OR LOWER(COALESCE(record_type,'')) LIKE '%forecast%') AS is_active
      FROM read_csv_auto('{GRANTS_CSV}', SAMPLE_SIZE = -1, IGNORE_ERRORS = true)
      WHERE LOWER(AgencyName) LIKE '%national science foundation%'
         OR AgencyCode LIKE 'NSF%'
         OR CFDANumbers LIKE '47.%'
         OR CFDANumbers LIKE '%,47.%'
    ) TO '{out_grants}' (FORMAT CSV, HEADER, QUOTE '\"');
  "))
  n_grants <- dbGetQuery(con, glue(
    "SELECT COUNT(*) AS n FROM read_csv_auto('{out_grants}')"))$n
  message(glue("  NSF rows written: {format(n_grants, big.mark=',')}"))
}
message(glue("  Grants.gov filter took {round(as.numeric(difftime(Sys.time(), t0, units='secs')),1)}s"))

# ── 5. Assistance Listings → NSF CSV ─────────────────────────────────────────
message("\n━━━ AssistanceListings → NSF CSV ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
t0 <- Sys.time()

if (file_exists(ALN_CSV)) {
  out_aln <- file.path(FINAL_DIR, "assistance_listings_nsf.csv")
  dbExecute(con, glue("
    COPY (
      SELECT *,
             (\"Program Title\" <> 'Not Applicable')                 AS is_active
      FROM read_csv_auto('{ALN_CSV}', SAMPLE_SIZE = -1, IGNORE_ERRORS = true,
                         HEADER = true)
      WHERE LOWER(\"Federal Agency (030)\") LIKE '%national science foundation%'
         OR \"Program Number\" LIKE '47.%'
    ) TO '{out_aln}' (FORMAT CSV, HEADER, QUOTE '\"');
  "))
  n_aln <- dbGetQuery(con, glue(
    "SELECT COUNT(*) AS n FROM read_csv_auto('{out_aln}')"))$n
  message(glue("  NSF rows written: {format(n_aln, big.mark=',')}"))
}
message(glue("  AssistanceListings filter took {round(as.numeric(difftime(Sys.time(), t0, units='secs')),1)}s"))

# ── 6. NSF terminations → CSV with is_active ─────────────────────────────────
message("\n━━━ nsf_terminations → CSV ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
if (file_exists(NSF_TERMS_CSV)) {
  out_terms <- file.path(FINAL_DIR, "nsf_terminations.csv")
  dbExecute(con, glue("
    COPY (
      SELECT *,
             (
               COALESCE(LOWER(CAST(reinstated AS VARCHAR)),'false') = 'true'
               OR COALESCE(LOWER(CAST(terminated AS VARCHAR)),'true') = 'false'
             ) AS is_active
      FROM read_csv_auto('{NSF_TERMS_CSV}', SAMPLE_SIZE = -1, IGNORE_ERRORS = true,
                         HEADER = true)
    ) TO '{out_terms}' (FORMAT CSV, HEADER, QUOTE '\"');
  "))
  n_terms <- dbGetQuery(con, glue(
    "SELECT COUNT(*) AS n, SUM(CASE WHEN is_active THEN 1 ELSE 0 END) AS active
     FROM read_csv_auto('{out_terms}')"))
  message(glue("  Rows: {format(n_terms$n, big.mark=',')} | active: {format(n_terms$active, big.mark=',')}"))
}

# ── 7. Done ──────────────────────────────────────────────────────────────────
dbDisconnect(con, shutdown = TRUE)

message("\n━━━ NSF FILTER COMPLETE ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
print(dir_info(FINAL_DIR)[, c("path", "size", "modification_time")])
