# =============================================================================
# 02_ingest_awards.R
# Pulls federal grant award records from USASpending.gov and loads them into
# the `awards` table in DuckDB.
#
# API documentation:
#   https://api.usaspending.gov/  (fully documented, no API key required)
#   Endpoint used: POST /api/v2/search/spending_by_award/
#
# Award type codes for grants:
#   02 = Block Grant
#   03 = Formula Grant
#   04 = Project Grant  ← most common for Grants.gov opportunities
#   05 = Cooperative Agreement
#
# Strategy: pull awards from Jan 2025 onward (the period of interest for
# volatility research). Re-running this script adds new awards without
# duplicating existing ones (upsert on award_id).
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────
required_pkgs <- c("httr2", "jsonlite", "duckdb", "DBI", "dplyr", "lubridate")
missing_pkgs  <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(httr2)
library(jsonlite)
library(duckdb)
library(DBI)
library(dplyr)
library(lubridate)

# ── 1. Configuration ──────────────────────────────────────────────────────────
USAS_API_URL   <- "https://api.usaspending.gov/api/v2/search/spending_by_award/"
PAGE_LIMIT     <- 100    # USASpending max per page is 100
MAX_PAGES      <- 200    # cap at 20,000 awards; increase for broader pull
START_DATE     <- "2025-01-01"
END_DATE       <- format(Sys.Date(), "%Y-%m-%d")
# Resolve path to DuckDB file (works interactively, sourced, or via Rscript).
# Override explicitly if needed:
# DB_PATH <- "C:/Users/Emily/Documents/final_project/data/grants_volatility.duckdb"
DB_PATH <- tryCatch(
  normalizePath(
    file.path(dirname(rstudioapi::getSourceEditorContext()$path),
              "..", "data", "grants_volatility.duckdb"),
    mustWork = FALSE),
  error = function(e) file.path(getwd(), "data", "grants_volatility.duckdb")
)

# Fields to request from the API.
# NOTES from the official API contract (confirmed March 2026):
#  - "CFDA Number" is only returned for non-loan assistance types (02-05). ✓
#  - "recipient_id" is an internal hash+level key, NOT the UEI.
#    The actual UEI field is "Recipient UEI".
#  - "Award ID" is the human-readable award identifier (e.g. ASST-NON-...).
#  - "generated_internal_id" is USASpending's stable numeric internal key.
#  - "Start Date" / "End Date" are non-loan assistance specific. ✓
FIELDS <- c(
  "Award ID",            # human-readable award ID – used as primary key
  "generated_internal_id",  # stable internal numeric ID (backup PK)
  "Recipient Name",
  "Recipient UEI",       # Unique Entity Identifier (replaced DUNS in 2022)
  "Award Amount",
  "Total Outlays",
  "Awarding Agency",
  "Awarding Agency Code",
  "Awarding Sub Agency",
  "Award Date",
  "Start Date",
  "End Date",
  "CFDA Number",         # only returned for award_type_codes 02-05 ✓
  "Description",
  "Place of Performance State Code"
)

# ── 2. Helper: build request body ─────────────────────────────────────────────
build_body <- function(page = 1) {
  list(
    subawards = FALSE,
    fields    = FIELDS,
    sort      = "Award Amount",
    order     = "desc",
    limit     = PAGE_LIMIT,
    page      = page,
    filters   = list(
      award_type_codes = list("02", "03", "04", "05"),
      time_period      = list(
        list(start_date = START_DATE, end_date = END_DATE)
      )
    )
  )
}

# ── 3. Helper: fetch one page ─────────────────────────────────────────────────
fetch_awards_page <- function(page = 1) {
  resp <- request(USAS_API_URL) |>
    req_method("POST") |>
    req_headers(
      "Content-Type" = "application/json",
      "Accept"       = "application/json"
    ) |>
    req_body_json(build_body(page)) |>
    req_timeout(90) |>
    req_retry(max_tries = 3, backoff = ~ 10) |>
    req_perform()

  if (resp_status(resp) != 200) {
    stop("USASpending API error on page ", page,
         ": status ", resp_status(resp))
  }

  resp_body_json(resp, simplifyVector = TRUE)
}

# ── 4. Helper: parse one page of results into a data frame ───────────────────
parse_awards_page <- function(result_list) {
  results <- result_list$results
  if (is.null(results) || length(results) == 0) return(data.frame())

  df <- as.data.frame(results, stringsAsFactors = FALSE)

  # Rename API fields to our schema column names.
  # Field names come back exactly as requested (with spaces).
  col_map <- c(
    "Award ID"                        = "award_id",
    "generated_internal_id"           = "usas_internal_id",   # bonus stable key
    "Recipient Name"                  = "recipient_name",
    "Recipient UEI"                   = "recipient_uei",       # correct UEI field
    "Award Amount"                    = "award_amount",
    "Total Outlays"                   = "face_value_of_loan",  # re-used for outlays
    "Awarding Agency"                 = "awarding_agency_name",
    "Awarding Agency Code"            = "awarding_agency_code",
    "Awarding Sub Agency"             = "awarding_sub_agency",
    "Award Date"                      = "award_date",
    "Start Date"                      = "period_of_perf_start",
    "End Date"                        = "period_of_perf_end",
    "CFDA Number"                     = "cfda_number",
    "Description"                     = "description",
    "Place of Performance State Code" = "recipient_state"
  )

  # Keep only columns that exist in the response
  existing_cols <- intersect(names(col_map), names(df))
  df <- df[, existing_cols, drop = FALSE]
  names(df) <- col_map[existing_cols]

  # Type coercions
  if ("award_amount" %in% names(df))
    df$award_amount <- suppressWarnings(as.numeric(df$award_amount))
  if ("award_date" %in% names(df))
    df$award_date <- suppressWarnings(as.Date(df$award_date))
  if ("period_of_perf_start" %in% names(df))
    df$period_of_perf_start <- suppressWarnings(as.Date(df$period_of_perf_start))
  if ("period_of_perf_end" %in% names(df))
    df$period_of_perf_end <- suppressWarnings(as.Date(df$period_of_perf_end))

  # Add fields not returned by API but in our schema
  df$awarding_agency_code  <- NA_character_
  df$face_value_of_loan    <- NA_real_
  df$assistance_type_code  <- NA_character_
  df$pulled_at             <- Sys.time()

  # Ensure award_id is non-null (drop rows without it)
  df <- df[!is.na(df$award_id) & nzchar(df$award_id), ]
  df
}

# ── 5. Pull all pages ─────────────────────────────────────────────────────────
message("=== USASpending.gov Award Ingestion ===")
message("Date range: ", START_DATE, " to ", END_DATE)
message("Started at: ", Sys.time())

all_awards    <- list()
total_records <- Inf

for (page_num in seq_len(MAX_PAGES)) {
  message(sprintf("  Fetching page %d / ~%s...",
                  page_num,
                  if (is.infinite(total_records)) "?" else ceiling(total_records / PAGE_LIMIT)))

  result <- tryCatch(
    fetch_awards_page(page = page_num),
    error = function(e) {
      message("  ERROR: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result)) break

  # On first page, log totals
  if (page_num == 1) {
    total_records <- as.integer(result$page_metadata$total %||% Inf)
    message("  Total award records available: ", total_records)
  }

  parsed <- tryCatch(
    parse_awards_page(result),
    error = function(e) {
      message("  WARNING: parse error on page ", page_num, ": ", conditionMessage(e))
      data.frame()
    }
  )

  if (nrow(parsed) == 0) {
    message("  No more records. Done.")
    break
  }

  all_awards[[page_num]] <- parsed
  message(sprintf("  Page %d: %d awards parsed (cumulative: %d)",
                  page_num, nrow(parsed), sum(sapply(all_awards, nrow))))

  # Check for last page
  has_next <- isTRUE(result$page_metadata$hasNext)
  if (!has_next) {
    message("  API indicates no more pages.")
    break
  }

  Sys.sleep(0.3)  # polite pause (API has no stated rate limit but be courteous)
}

awards_df <- bind_rows(all_awards)
message(sprintf("\nTotal awards parsed: %d", nrow(awards_df)))

if (nrow(awards_df) == 0) {
  stop("No awards were parsed. Check API connectivity and parameters.")
}

# ── 6. Load into DuckDB ───────────────────────────────────────────────────────
message("Connecting to DuckDB: ", DB_PATH)
con <- dbConnect(duckdb(), dbdir = DB_PATH)

# Ensure all schema columns exist in our data frame (fill missing with NA)
schema_cols <- c("award_id", "cfda_number", "assistance_type_code",
                 "recipient_name", "recipient_uei", "recipient_state",
                 "awarding_agency_name", "awarding_agency_code",
                 "awarding_sub_agency", "award_amount", "face_value_of_loan",
                 "award_date", "period_of_perf_start", "period_of_perf_end",
                 "description", "pulled_at")

for (col in schema_cols) {
  if (!col %in% names(awards_df)) awards_df[[col]] <- NA
}
awards_df <- awards_df[, schema_cols]

# Write to staging table, then upsert
dbWriteTable(con, "awards_staging", awards_df, overwrite = TRUE, temporary = TRUE)

# Insert only records not already in the awards table
dbExecute(con, "
  INSERT INTO awards
  SELECT * FROM awards_staging
  WHERE award_id NOT IN (SELECT award_id FROM awards)
")

new_count <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM awards")$n

# ── 7. Quick coverage report ──────────────────────────────────────────────────
# How many disappeared Grants.gov opportunities have a matching CFDA in awards?
coverage <- dbGetQuery(con, "
  SELECT
    COUNT(DISTINCT c.opportunity_id)                        AS removed_opps,
    COUNT(DISTINCT CASE WHEN a.cfda_number IS NOT NULL
                        THEN c.opportunity_id END)          AS with_matching_award,
    COUNT(DISTINCT CASE WHEN a.cfda_number IS NULL
                        THEN c.opportunity_id END)          AS without_award
  FROM change_log c
  JOIN opportunities o ON c.opportunity_id = o.opportunity_id
  LEFT JOIN awards a
    ON a.cfda_number IN (
       SELECT TRIM(u) FROM unnest(string_split(o.cfda_numbers, ',')) t(u)
    )
  WHERE c.change_type = 'REMOVED'
")

message("\n=== Award Ingestion Complete ===")
message("Total awards in DB : ", new_count)
if (coverage$removed_opps > 0) {
  message("--- Cross-reference coverage ---")
  message("Removed opportunities      : ", coverage$removed_opps)
  message("With matching award record : ", coverage$with_matching_award)
  message("WITHOUT award (rescissions): ", coverage$without_award)
} else {
  message("No REMOVED events yet (run 03_diff_snapshots.R after collecting 2+ snapshots).")
}

dbDisconnect(con, shutdown = TRUE)
message("Finished at: ", Sys.time())

# ── Utility ───────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a)) a else b
