# =============================================================================
# 03_diff_snapshots.R
# Compares the two most recent Grants.gov snapshots and writes all detected
# changes to the `change_log` table.
#
# Change types detected:
#   ADDED    – opportunity_id present in CURRENT snapshot but not PREVIOUS
#   REMOVED  – opportunity_id present in PREVIOUS snapshot but not CURRENT
#   MODIFIED – present in both, but one or more tracked fields changed
#
# Run this AFTER every call to 01_ingest_grants.R.
# Requires at least 2 snapshots in the database.
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────
required_pkgs <- c("duckdb", "DBI", "dplyr", "tidyr")
missing_pkgs  <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(duckdb)
library(DBI)
library(dplyr)
library(tidyr)

# ── 1. Configuration ──────────────────────────────────────────────────────────
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

# Fields to compare between snapshots (add any field you want to track mutations on)
TRACKED_FIELDS <- c(
  "title",
  "opportunity_status",
  "close_date",
  "estimated_total_funding",
  "award_ceiling",
  "award_floor",
  "agency_code",
  "funding_activity_category"
)

# ── 2. Connect ────────────────────────────────────────────────────────────────
message("=== Snapshot Differencing ===")
message("Started at: ", Sys.time())
con <- dbConnect(duckdb(), dbdir = DB_PATH)

# ── 3. Identify the two most recent snapshots ─────────────────────────────────
snap_info <- dbGetQuery(con, "
  SELECT snapshot_id, snapshot_date, record_count
  FROM snapshots
  ORDER BY snapshot_id DESC
  LIMIT 2
")

if (nrow(snap_info) < 2) {
  message("Only ", nrow(snap_info), " snapshot(s) found. Need at least 2 to diff.")
  message("Run 01_ingest_grants.R again after a few hours/days and retry.")
  dbDisconnect(con, shutdown = TRUE)
  stop("Insufficient snapshots for differencing.")
}

current_id  <- snap_info$snapshot_id[1]
previous_id <- snap_info$snapshot_id[2]

message(sprintf("Comparing snapshot %d (%s, %d records) vs %d (%s, %d records)",
                current_id,  format(snap_info$snapshot_date[1], "%Y-%m-%d %H:%M"),
                snap_info$record_count[1],
                previous_id, format(snap_info$snapshot_date[2], "%Y-%m-%d %H:%M"),
                snap_info$record_count[2]))

# ── 4. Load the two snapshot datasets ────────────────────────────────────────
# We use last_seen_snapshot_id to reconstruct "which records were in snapshot N"
prev_ids <- dbGetQuery(con, sprintf("
  SELECT opportunity_id FROM opportunities
  WHERE first_seen_snapshot_id <= %d
    AND (last_seen_snapshot_id >= %d OR last_seen_snapshot_id IS NULL)
", previous_id, previous_id))$opportunity_id

curr_ids <- dbGetQuery(con, sprintf("
  SELECT opportunity_id FROM opportunities
  WHERE first_seen_snapshot_id <= %d
    AND (last_seen_snapshot_id >= %d OR last_seen_snapshot_id IS NULL)
", current_id, current_id))$opportunity_id

message(sprintf("Previous snapshot: %d IDs | Current snapshot: %d IDs",
                length(prev_ids), length(curr_ids)))

# ── 5. Detect ADDED ──────────────────────────────────────────────────────────
added_ids <- setdiff(curr_ids, prev_ids)
message(sprintf("ADDED   : %d opportunities", length(added_ids)))

added_log <- if (length(added_ids) > 0) {
  data.frame(
    opportunity_id = added_ids,
    snapshot_id    = current_id,
    change_type    = "ADDED",
    field_changed  = NA_character_,
    old_value      = NA_character_,
    new_value      = NA_character_,
    stringsAsFactors = FALSE
  )
} else data.frame()

# ── 6. Detect REMOVED ────────────────────────────────────────────────────────
removed_ids <- setdiff(prev_ids, curr_ids)
message(sprintf("REMOVED : %d opportunities", length(removed_ids)))

removed_log <- if (length(removed_ids) > 0) {
  # Update is_active flag for removed opportunities
  id_list_sql <- paste0("('", paste(removed_ids, collapse = "','"), "')")
  dbExecute(con, sprintf("
    UPDATE opportunities SET is_active = FALSE
    WHERE opportunity_id IN %s
  ", id_list_sql))

  data.frame(
    opportunity_id = removed_ids,
    snapshot_id    = current_id,
    change_type    = "REMOVED",
    field_changed  = NA_character_,
    old_value      = NA_character_,
    new_value      = NA_character_,
    stringsAsFactors = FALSE
  )
} else data.frame()

# ── 7. Detect MODIFIED ───────────────────────────────────────────────────────
# For opportunities present in BOTH snapshots, compare tracked fields.
# We compare the current DB state against the values recorded in change_log
# (i.e., last-known state). This is a simplified Type-2 SCD approach.

common_ids <- intersect(prev_ids, curr_ids)
message(sprintf("Checking %d common IDs for field-level changes...", length(common_ids)))

modified_log_list <- list()

if (length(common_ids) > 0) {
  # Pull current values for common IDs
  id_list_sql <- paste0("('", paste(common_ids, collapse = "','"), "')")
  current_vals <- dbGetQuery(con, sprintf("
    SELECT opportunity_id, %s
    FROM opportunities
    WHERE opportunity_id IN %s
  ", paste(TRACKED_FIELDS, collapse = ", "), id_list_sql))

  # Pull previous known values: use last MODIFIED entry per field, or ADDED value.
  # Simpler approach: compare current DB values against values from snapshot N-1.
  # Since we upsert (overwrite) on each ingest, we rely on change_log to reconstruct
  # prior values. For the very first diff, there's no prior modification, so we only
  # detect structural ADDED/REMOVED.

  # Pull last known field values from change_log for these IDs
  last_known <- dbGetQuery(con, sprintf("
    SELECT opportunity_id, field_changed, new_value
    FROM change_log
    WHERE opportunity_id IN %s
      AND change_type = 'MODIFIED'
      AND snapshot_id < %d
    QUALIFY ROW_NUMBER() OVER (
      PARTITION BY opportunity_id, field_changed
      ORDER BY snapshot_id DESC
    ) = 1
  ", id_list_sql, current_id))

  # For each tracked field, compare current value vs. last known value
  for (field in TRACKED_FIELDS) {
    if (!field %in% names(current_vals)) next

    # Get previous values: from change_log if available, else from opportunities table
    # (first-seen value -- only works if we stored it; here we use change_log)
    prev_field <- last_known[last_known$field_changed == field, c("opportunity_id", "new_value")]
    names(prev_field)[2] <- "prev_value"

    curr_field <- current_vals[, c("opportunity_id", field)]
    names(curr_field)[2] <- "curr_value"
    curr_field$curr_value <- as.character(curr_field$curr_value)

    # Merge
    comparison <- merge(curr_field, prev_field, by = "opportunity_id", all.x = TRUE)

    # Identify changes (skip if prev is NA, meaning first time we see this field)
    changed <- comparison[
      !is.na(comparison$prev_value) &
      !is.na(comparison$curr_value) &
      comparison$prev_value != comparison$curr_value, ]

    if (nrow(changed) > 0) {
      mod_df <- data.frame(
        opportunity_id = changed$opportunity_id,
        snapshot_id    = current_id,
        change_type    = "MODIFIED",
        field_changed  = field,
        old_value      = changed$prev_value,
        new_value      = changed$curr_value,
        stringsAsFactors = FALSE
      )
      modified_log_list[[field]] <- mod_df
    }
  }
}

modified_log <- bind_rows(modified_log_list)
message(sprintf("MODIFIED: %d field-level changes across %d opportunities",
                nrow(modified_log),
                length(unique(modified_log$opportunity_id))))

# ── 8. Write all changes to change_log ───────────────────────────────────────
all_changes <- bind_rows(added_log, removed_log, modified_log)

if (nrow(all_changes) > 0) {
  # Assign change_id (auto-increment)
  max_id <- dbGetQuery(con, "SELECT COALESCE(MAX(change_id), 0) AS m FROM change_log")$m
  all_changes$change_id  <- seq(max_id + 1, max_id + nrow(all_changes))
  all_changes$detected_at <- Sys.time()

  # Reorder to match schema
  all_changes <- all_changes[, c("change_id", "opportunity_id", "snapshot_id",
                                  "change_type", "field_changed",
                                  "old_value", "new_value", "detected_at")]

  dbWriteTable(con, "change_log_staging", all_changes, overwrite = TRUE, temporary = TRUE)
  dbExecute(con, "INSERT INTO change_log SELECT * FROM change_log_staging")
  message(sprintf("Wrote %d change records to change_log.", nrow(all_changes)))
} else {
  message("No changes detected between the two snapshots.")
}

# ── 9. Rescission summary ─────────────────────────────────────────────────────
rescissions <- tryCatch(
  dbGetQuery(con, "SELECT COUNT(*) AS n FROM confirmed_rescissions"),
  error = function(e) data.frame(n = NA)
)
message("\n--- Current rescission count (REMOVED with no award match): ",
        rescissions$n, " ---")
message("Query the `confirmed_rescissions` view in DuckDB for full details.")

# ── 10. Cleanup ───────────────────────────────────────────────────────────────
dbDisconnect(con, shutdown = TRUE)
message("Finished at: ", Sys.time())
