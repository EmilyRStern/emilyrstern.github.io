# =============================================================================
# 99_run_pipeline.R
# Master runner script. Runs the full ingestion pipeline in sequence:
#   1. Ingest Grants.gov snapshot
#   2. Diff against previous snapshot (detects ADDED/REMOVED/MODIFIED)
#   3. Ingest USASpending awards (optional; run less frequently)
#
# FIRST TIME SETUP: Run 00_setup_db.R once before using this script.
#
# TYPICAL USAGE:
#   - Run this script every day or every few days to accumulate snapshots.
#   - For the first run, set FIRST_RUN <- TRUE to skip the diff step.
#   - After the first run, set FIRST_RUN <- FALSE.
#   - Run REFRESH_AWARDS <- TRUE periodically (weekly) to update award data.
# =============================================================================

# ── Configuration ─────────────────────────────────────────────────────────────
FIRST_RUN      <- FALSE   # Set TRUE only on your very first ingestion run
REFRESH_AWARDS <- TRUE    # Set TRUE to also pull USASpending data this run

# Resolve the scripts directory (works interactively and when sourced).
# Override explicitly if needed:
# SCRIPT_DIR <- "C:/Users/Emily/Documents/final_project/scripts"
SCRIPT_DIR <- tryCatch(
  dirname(normalizePath(rstudioapi::getSourceEditorContext()$path, mustWork = FALSE)),
  error = function(e) file.path(getwd(), "scripts")
)

# ── Logging ───────────────────────────────────────────────────────────────────
log_file <- file.path(SCRIPT_DIR, "..", "data",
                       paste0("pipeline_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
con_log  <- file(log_file, open = "wt")
sink(con_log, split = TRUE)  # write to both console and log file

message("╔══════════════════════════════════════════════════════════════╗")
message("║  Federal Grant Volatility Pipeline                          ║")
message("║  EPPS 6354 – Emily Stern – Spring 2026                      ║")
message("╚══════════════════════════════════════════════════════════════╝")
message("Pipeline started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message("First run mode  : ", FIRST_RUN)
message("Refresh awards  : ", REFRESH_AWARDS)
message("")

# ── Step 1: Ingest Grants.gov snapshot ───────────────────────────────────────
message("━━━ STEP 1: Ingest Grants.gov ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
t1 <- proc.time()
tryCatch({
  source(file.path(SCRIPT_DIR, "01_ingest_grants.R"), local = new.env())
  message(sprintf("Step 1 complete in %.1f seconds.", (proc.time() - t1)["elapsed"]))
}, error = function(e) {
  message("STEP 1 FAILED: ", conditionMessage(e))
  sink()
  close(con_log)
  stop(e)
})

# ── Step 2: Diff snapshots ────────────────────────────────────────────────────
message("")
message("━━━ STEP 2: Diff Snapshots ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

if (FIRST_RUN) {
  message("Skipping diff: FIRST_RUN = TRUE (no previous snapshot to compare against).")
  message("Set FIRST_RUN <- FALSE in future runs.")
} else {
  t2 <- proc.time()
  tryCatch({
    source(file.path(SCRIPT_DIR, "03_diff_snapshots.R"), local = new.env())
    message(sprintf("Step 2 complete in %.1f seconds.", (proc.time() - t2)["elapsed"]))
  }, error = function(e) {
    message("STEP 2 FAILED: ", conditionMessage(e))
    message("(Pipeline will continue with awards ingestion if enabled.)")
  })
}

# ── Step 3: Refresh awards (optional) ────────────────────────────────────────
message("")
message("━━━ STEP 3: Refresh USASpending Awards ━━━━━━━━━━━━━━━━━━━━━━━")

if (REFRESH_AWARDS) {
  t3 <- proc.time()
  tryCatch({
    source(file.path(SCRIPT_DIR, "02_ingest_awards.R"), local = new.env())
    message(sprintf("Step 3 complete in %.1f seconds.", (proc.time() - t3)["elapsed"]))
  }, error = function(e) {
    message("STEP 3 FAILED: ", conditionMessage(e))
    message("(Award data not updated this run.)")
  })
} else {
  message("Skipping awards refresh (REFRESH_AWARDS = FALSE).")
}

# ── Summary ───────────────────────────────────────────────────────────────────
message("")
message("━━━ PIPELINE COMPLETE ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
message("Finished: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message("Log saved to: ", log_file)
message("")
message("NEXT STEPS:")
message("  • Open the Shiny app: shiny::runApp('../shiny/app.R')")
message("  • Query the DB directly:")
message("    con <- DBI::dbConnect(duckdb::duckdb(), '../data/grants_volatility.duckdb')")
message("    DBI::dbGetQuery(con, 'SELECT * FROM confirmed_rescissions LIMIT 20')")

sink()
close(con_log)
