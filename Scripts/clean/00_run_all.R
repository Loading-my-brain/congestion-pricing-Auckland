# =============================================================================
# 00_run_all.R
# -----------------------------------------------------------------------------
# PURPOSE  Run every clean script in order and save ALL console output
#          (code + results) to one log file, e.g.
#          Scripts/clean/logs/run_log_2026-10-09_1430.txt
#
# HOW      1. Restart R (Ctrl + Shift + F10)
#          2. Open this file and click "Source" (or Ctrl + Shift + S)
#          3. Wait (~5-10 min; the LISA permutations and GWR take longest)
#          4. Check the SUMMARY at the end of the log: every script should
#             say OK
#
# SLOW STEPS ARE SKIPPED AUTOMATICALLY
#   04a  r5r PT routing      -> documentation only (inside if (FALSE)); the
#                               existing PT CSV is used
#   04b  TravelTime API      -> NOT run: not used in the thesis (licence
#                               unconfirmed), kept as a record only
#   04c  r5r free-flow cars  -> routing skipped because the CSV exists;
#                               only the comparison runs
#
# NOT IN THIS LIST (run by hand if needed)
#   09_interactive_travel_time_map.R  -> optional web map (.html)
#   99_render_scripts_to_word.R       -> Word print-outs of every script
# =============================================================================

scripts <- c("01_data_cleaning.R",
             "02_mode_shares_RQ1.R",
             "02b_od_cbd_flows.R",
             "03_spatial_clustering_RQ1.R",
             "04a_pt_times_r5r.R",
             # "04b_car_times_traveltime_api.R",   # not used (licensing) -> don't re-run API
             "04c_car_freeflow_check.R",
             "05_travel_times.R",
             "06_predictors_RQ2.R",
             "07_ols_gwr_RQ2.R",
             "07.1_GWR_CoefMaps.R",
             "08_rq3_charge_burden.R")

script_dir <- "Scripts/clean"
log_dir    <- file.path(script_dir, "logs")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
log_file   <- file.path(log_dir, paste0("run_log_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".txt"))

# Wider console so tables are not wrapped/cut in the log
options(width = 160, tibble.width = Inf, tibble.print_max = 50)

con <- file(log_file, open = "wt")
sink(con, split = TRUE)                 # output goes to the console AND the log
sink(con, type = "message", append = TRUE)

cat("RUN STARTED:", format(Sys.time()), "\n")
cat("R version:  ", R.version.string, "\n\n")

status <- character(0)
for (s in scripts) {
  cat("\n\n", strrep("#", 79), "\n# RUNNING: ", s, "\n", strrep("#", 79), "\n\n", sep = "")
  t0 <- Sys.time()
  res <- tryCatch({
    # Each script runs in its own fresh environment (like a fresh session)
    source(file.path(script_dir, s), echo = TRUE, max.deparse.length = Inf,
           local = new.env(), spaced = TRUE)
    "OK"
  }, error = function(e) {
    cat("\n!!! ERROR in", s, ":", conditionMessage(e), "\n")
    paste("ERROR:", conditionMessage(e))
  })
  status[s] <- paste0(res, "  (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)")
}

cat("\n\n", strrep("=", 79), "\nSUMMARY\n", strrep("=", 79), "\n", sep = "")
for (s in names(status)) cat(sprintf("%-35s %s\n", s, status[s]))
cat("\nRUN FINISHED:", format(Sys.time()), "\n")
cat("Log saved to:", normalizePath(log_file), "\n")

sink(type = "message")
sink()
close(con)
message("Done. Log saved to: ", normalizePath(log_file))
