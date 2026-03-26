# =============================================================================
# 01_ingest_grants.R
# Pulls a full snapshot from the Grants.gov Search API and loads it into
# the DuckDB database.
#
# API: NEW RESTful API launched March 2025 (replaces apply07.grants.gov)
#   Endpoint: POST https://api.grants.gov/v1/api/search2
#   Docs:     https://www.grants.gov/api/api-guide
#   Auth:     None required
#
# Request uses mixed case (oppStatuses, etc.) but response is snake_case.
# Pagination uses page_number / page_size (not startRecordNum / rows).
# opportunity_id is now a UUID string, not a numeric ID.
#
# Each run creates one row in `snapshots` and upserts all opportunity records
# into `opportunities`. Run 03_diff_snapshots.R afterwards to detect changes.
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
GRANTS_API_URL <- "https://api.grants.gov/v1/api/search2"
PAGE_SIZE      <- 100    # adjust down if you hit timeouts; up to ~500 is usually fine
MAX_PAGES      <- 500    # safety cap; ~50,000 records at 100/page

# DB path: auto-detects from RStudio active doc, falls back to getwd()/data/
# Override explicitly if needed:
# DB_PATH <- "C:/Users/Emily/Documents/final_project/data/grants_volatility.duckdb"
DB_PATH <- tryCatch(
  normalizePath(
    file.path(dirname(rstudioapi::getSourceEditorContext()$path),
              "..", "data", "grants_volatility.duckdb"),
    mustWork = FALSE),
  error = function(e) file.path(getwd(), "data", "grants_volatility.duckdb")
)

# ── 2. Helper: fetch one page ─────────────────────────────────────────────────
# NOTE: oppStatuses is intentionally omitted. Both the pipe-separated string
# ("posted|forecasted|closed|archived") and the JSON array format silently
# return hitCount: 0 on this endpoint. Omitting the filter returns all
# current listings, which is the correct behavior for a full snapshot.
fetch_page <- function(start_rec = 0) {
  body_json <- list(
    rows           = PAGE_SIZE,
    startRecordNum = start_rec
  )

  resp <- request(GRANTS_API_URL) |>
    req_method("POST") |>
    req_headers(
      "Content-Type" = "application/json",
      "Accept"       = "application/json"
    ) |>
    req_body_json(body_json) |>
    req_timeout(90) |>
    req_retry(
      max_tries = 4,
      backoff   = ~ 10,                     # 10-second wait between retries
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504)
    ) |>
    req_perform()

  if (resp_status(resp) != 200) {
    stop(sprintf("API returned HTTP %d at startRecord %d", resp_status(resp), start_rec))
  }

  resp_body_json(resp, simplifyVector = TRUE)
}

# ── 3. Helper: DIAGNOSTIC – print response structure on first page ────────────
# Confirmed structure (live API, March 2026):
#   result$errorcode          – 0 = success
#   result$data$hitCount      – total matching records
#   result$data$startRecord   – current offset
#   result$data$oppHits       – opportunity records
#     When simplifyVector=TRUE this comes back as a data.frame (not a list).
#     Use names(oppHits) / oppHits[1, ] — NOT oppHits[[1]] — to inspect it.
print_response_structure <- function(result) {
  message("\n--- API Response Structure (first page) ---")
  message("Top-level keys : ", paste(names(result), collapse = ", "))
  message("data sub-keys  : ", paste(names(result$data), collapse = ", "))
  message("hitCount       : ", result$data$hitCount)
  message("startRecord    : ", result$data$startRecord)

  hits <- result$data$oppHits

  if (is.data.frame(hits)) {
    message("oppHits class  : data.frame (", nrow(hits), " rows x ", ncol(hits), " cols)")
    message("Column names   : ", paste(names(hits), collapse = ", "))
    if (nrow(hits) > 0) {
      message("Sample values (first row):")
      for (nm in names(hits)[seq_len(min(12, ncol(hits)))]) {
        val <- hits[[nm]][1]
        if (is.list(val)) val <- paste(unlist(val), collapse = ",")
        message("  ", nm, " = ", as.character(val)[1])
      }
    }
  } else {
    n_hits <- length(hits)
    message("oppHits class  : list (length ", n_hits, ")")
    if (n_hits > 0) {
      first_rec <- hits[[1]]
      message("Fields in first oppHit: ", paste(names(first_rec), collapse = ", "))
      message("Sample values (first record):")
      for (nm in names(first_rec)[seq_len(min(12, length(first_rec)))]) {
        val <- first_rec[[nm]]
        if (length(val) > 1) val <- paste(val, collapse = ",")
        message("  ", nm, " = ", as.character(val)[1])
      }
    }
  }
  message("-------------------------------------------\n")
}

# ── 4. Helper: extract the oppHits list from a response ───────────────────────
extract_results <- function(result) {
  # Confirmed: results live at result$data$oppHits
  # simplifyVector=TRUE may return a data.frame; use nrow/length accordingly.
  hits <- result$data$oppHits
  if (is.null(hits)) return(list(key = NULL, data = NULL))
  n <- if (is.data.frame(hits)) nrow(hits) else length(hits)
  if (n > 0) return(list(key = "data$oppHits", data = hits))
  list(key = NULL, data = NULL)
}

# ── 5. Helper: total record count ─────────────────────────────────────────────
extract_total <- function(result) {
  # Confirmed: result$data$hitCount
  val <- result$data$hitCount
  if (!is.null(val)) return(as.integer(val))
  Inf
}

# ── 6. Helper: are there more records to fetch? ───────────────────────────────
has_more_records <- function(result, current_start, page_size) {
  total <- extract_total(result)
  data  <- extract_results(result)$data
  # oppHits may be a data.frame (use nrow) or a list (use length)
  fetched <- if (is.data.frame(data)) nrow(data) else length(data)
  if (is.infinite(total)) return(fetched >= page_size)  # fallback: full page = more
  (current_start + fetched) < total
}

# ── 7. Helper: parse one page of results into a flat data frame ───────────────
# The new API uses snake_case in responses; old API used camelCase.
# This function handles both by checking for whichever version exists.

# parse_grants_date: handles multiple date formats returned by Grants.gov
#   "MM/DD/YYYY"  – most common in oppHits responses
#   "YYYY-MM-DD"  – ISO 8601 (used in some fields / future-proofing)
#   Numeric days-since-epoch (rare edge case from simplifyVector coercion)
parse_grants_date <- function(x) {
  if (is.null(x) || all(is.na(x))) return(as.Date(rep(NA, length(x))))
  # Already a Date vector?
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  result <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))   # MM/DD/YYYY
  # Fall back to ISO for any that didn't parse
  iso_mask <- is.na(result) & !is.na(x) & nzchar(x)
  if (any(iso_mask))
    result[iso_mask] <- suppressWarnings(as.Date(x[iso_mask], format = "%Y-%m-%d"))
  result
}

coalesce_field <- function(df, ...) {
  candidates <- c(...)
  for (nm in candidates) {
    if (nm %in% names(df)) return(df[[nm]])
  }
  rep(NA_character_, nrow(df))
}

parse_opportunity <- function(raw) {
  if (is.null(raw) || length(raw) == 0) return(data.frame())
  if (!is.data.frame(raw)) raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(data.frame())

  data.frame(
    # New API snake_case first, old API camelCase as fallback
    opportunity_id = as.character(
      coalesce_field(raw, "opportunity_id", "id")),
    opportunity_number = as.character(
      coalesce_field(raw, "opportunity_number", "number", "opportunityNumber")),
    title = as.character(
      coalesce_field(raw, "opportunity_title", "title")),
    agency_code = as.character(
      coalesce_field(raw, "agency_code", "agencyCode")),
    agency_name = as.character(
      coalesce_field(raw, "agency_name", "agencyName")),
    opportunity_status = as.character(
      coalesce_field(raw, "opportunity_status", "opportunityStatus")),
    post_date = parse_grants_date(
      coalesce_field(raw, "post_date", "openDate", "postDate")),
    close_date = parse_grants_date(
      coalesce_field(raw, "close_date", "closeDate")),
    archive_date = parse_grants_date(
      coalesce_field(raw, "archive_date", "archiveDate")),
    estimated_total_funding = suppressWarnings(as.numeric(
      coalesce_field(raw, "estimated_total_program_funding",
                     "estimatedTotalProgramFunding"))),
    award_ceiling = suppressWarnings(as.numeric(
      coalesce_field(raw, "award_ceiling", "awardCeiling"))),
    award_floor = suppressWarnings(as.numeric(
      coalesce_field(raw, "award_floor", "awardFloor"))),
    # ALN = new name for CFDA. Handle both.
    cfda_numbers = sapply(seq_len(nrow(raw)), function(i) {
      val <- NULL
      for (nm in c("aln", "cfda_numbers", "cfdaNumbers", "cfda")) {
        if (nm %in% names(raw) && !is.null(raw[[nm]][[i]])) {
          val <- raw[[nm]][[i]]
          break
        }
      }
      if (is.null(val) || length(val) == 0) NA_character_
      else paste(val, collapse = ",")
    }),
    funding_instrument_type = as.character(
      coalesce_field(raw, "funding_instrument", "fundingInstrumentType",
                     "fundingInstrument")),
    funding_activity_category = as.character(
      coalesce_field(raw, "funding_category", "fundingActivityCategory",
                     "fundingCategory")),
    eligible_applicants = sapply(seq_len(nrow(raw)), function(i) {
      for (nm in c("applicant_types", "eligibleApplicants", "eligible_applicants")) {
        if (nm %in% names(raw) && !is.null(raw[[nm]][[i]]))
          return(paste(raw[[nm]][[i]], collapse = ","))
      }
      NA_character_
    }),
    description = as.character(
      coalesce_field(raw, "summary", "synopsis", "description")),
    stringsAsFactors = FALSE
  )
}

# ── 8. Pull all pages (offset-based) ─────────────────────────────────────────
message("=== Grants.gov Snapshot Ingestion ===")
message("Endpoint : ", GRANTS_API_URL)
message("Started  : ", Sys.time())

all_opps   <- list()
start_rec  <- 0
total_recs <- Inf
diag_done  <- FALSE

for (batch in seq_len(MAX_PAGES)) {
  message(sprintf("  Batch %d (startRecordNum = %d)...", batch, start_rec))

  result <- tryCatch(
    fetch_page(start_rec = start_rec),
    error = function(e) {
      message("  ERROR: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result)) { message("  Stopping due to fetch error."); break }

  # First batch: print structure so you can verify field names
  if (!diag_done) {
    print_response_structure(result)
    diag_done  <- TRUE
    total_recs <- extract_total(result)
    message(sprintf("  Total records available: %s",
                    if (is.infinite(total_recs)) "unknown" else total_recs))
  }

  extracted <- extract_results(result)
  n_extracted <- if (is.data.frame(extracted$data)) nrow(extracted$data)
                 else length(extracted$data)
  if (is.null(extracted$data) || n_extracted == 0) {
    message("  No results returned. Done.")
    break
  }

  parsed <- tryCatch(
    parse_opportunity(extracted$data),
    error = function(e) {
      message("  WARNING: parse error on batch ", batch, ": ", conditionMessage(e))
      data.frame()
    }
  )

  n_batch <- nrow(parsed)
  if (n_batch > 0) all_opps[[batch]] <- parsed
  cumulative <- sum(sapply(Filter(is.data.frame, all_opps), nrow))
  message(sprintf("  Batch %d: %d records (cumulative: %d / %s)",
                  batch, n_batch, cumulative,
                  if (is.infinite(total_recs)) "?" else total_recs))

  if (!has_more_records(result, start_rec, PAGE_SIZE)) {
    message("  All records fetched.")
    break
  }

  start_rec <- start_rec + PAGE_SIZE
  Sys.sleep(0.4)
}

opportunities_df <- bind_rows(all_opps)
message(sprintf("\nTotal parsed: %d records from %d pages",
                nrow(opportunities_df), length(all_opps)))

if (nrow(opportunities_df) == 0) {
  message("\n!! No records parsed. The API response structure may have changed.")
  message("!! Check the diagnostic output above and update parse_opportunity()")
  message("!! to match the actual field names returned.")
  stop("No records parsed — see diagnostic output above.")
}

# ── 9. Write to DuckDB ────────────────────────────────────────────────────────
message("\nConnecting to DuckDB: ", DB_PATH)
con <- dbConnect(duckdb(), dbdir = DB_PATH)

# Insert snapshot metadata
dbExecute(con, "
  INSERT INTO snapshots (snapshot_date, source, record_count, notes)
  VALUES (now(), 'grants.gov', ?, ?)
", params = list(nrow(opportunities_df),
                  sprintf("grants.gov v1/api/search2 – %d records", nrow(opportunities_df))))

snapshot_id <- dbGetQuery(con, "SELECT MAX(snapshot_id) AS sid FROM snapshots")$sid
message("Snapshot ID: ", snapshot_id)

# Add snapshot tracking columns
opportunities_df <- opportunities_df |>
  mutate(
    first_seen_snapshot_id = snapshot_id,
    last_seen_snapshot_id  = snapshot_id,
    is_active              = TRUE
  )

# Upsert: write to staging, then merge
dbWriteTable(con, "opp_staging", opportunities_df, overwrite = TRUE, temporary = TRUE)

dbExecute(con, "
  UPDATE opportunities
  SET last_seen_snapshot_id = ?
  WHERE opportunity_id IN (SELECT opportunity_id FROM opp_staging)
    AND is_active = TRUE
", params = list(snapshot_id))

dbExecute(con, "
  INSERT INTO opportunities
  SELECT * FROM opp_staging
  WHERE opportunity_id NOT IN (SELECT opportunity_id FROM opportunities)
")

# Upsert agencies
agencies_df <- opportunities_df |>
  filter(!is.na(agency_code), agency_code != "NA",
         !is.na(agency_name),  agency_name != "NA") |>
  distinct(agency_code, agency_name)

if (nrow(agencies_df) > 0) {
  dbWriteTable(con, "agency_staging", agencies_df, overwrite = TRUE, temporary = TRUE)
  dbExecute(con, "
    INSERT INTO agencies (agency_id, agency_code, agency_name)
    SELECT
      (SELECT COALESCE(MAX(agency_id), 0) FROM agencies) + ROW_NUMBER() OVER () AS agency_id,
      agency_code, agency_name
    FROM agency_staging
    WHERE agency_code NOT IN (SELECT agency_code FROM agencies)
  ")
}

# ── 10. Summary ───────────────────────────────────────────────────────────────
opp_count <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM opportunities")$n
message("\n=== Ingestion Complete ===")
message("Opportunities in DB : ", opp_count)
message("Snapshot ID         : ", snapshot_id)
message("Run 03_diff_snapshots.R to detect changes from the previous snapshot.")

dbDisconnect(con, shutdown = TRUE)
message("Finished: ", Sys.time())

`%||%` <- function(a, b) if (!is.null(a)) a else b
