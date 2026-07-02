# Test the Phase 3 incremental loader (wrangling/build_accounts_timeseries.R).
# Builds two small zips from the August-2025 sample (two pretend months), loads
# into a THROWAWAY DuckDB, checks counts/keys/idempotency/resume, then cleans up.
# Run from project root:  Rscript testcode/test_loader.R
suppressPackageStartupMessages(library(tidyverse))
source("wrangling/build_accounts_timeseries.R")   # also sources connect.R + extract_account.R

aug_dir <- "local/test_accounts/Accounts_Monthly_Data-August2025"
stopifnot(dir.exists(aug_dir))
files <- list.files(aug_dir, pattern = "\\.html$", full.names = TRUE)
set.seed(1); samp <- sample(files, min(60, length(files)))

# --- build two disjoint test zips (pretend July & August 2025) ----------------
zdir <- "local/_loader_test_zips"
unlink(zdir, recursive = TRUE, force = TRUE); dir.create(zdir, recursive = TRUE)
z_jul <- file.path(zdir, "Accounts_Monthly_Data-July2025.zip")
z_aug <- file.path(zdir, "Accounts_Monthly_Data-August2025.zip")
half <- length(samp) %/% 2
zip(z_jul, samp[1:half],                 flags = "-jq")
zip(z_aug, samp[(half + 1):length(samp)], flags = "-jq")
cat(sprintf("built 2 test zips (%d + %d files)\n", half, length(samp) - half))

# --- throwaway DB -------------------------------------------------------------
dbfile <- "local/_loader_test.duckdb"
unlink(dbfile, force = TRUE)
con <- ch_connect(dbfile); ch_init_schema(con)

# --- run the loader (local zips; no download) ---------------------------------
cat("\n===== RUN 1 =====\n")
ch_build_timeseries(con, zip_names = c(basename(z_jul), basename(z_aug)),
                    zips_dir = zdir, workers = 4)

cnt <- function() list(
  filings = dbGetQuery(con, "SELECT count(*) n FROM filings")$n,
  obs     = dbGetQuery(con, "SELECT count(*) n FROM observations")$n)
c1 <- cnt()

cat("\n== processed_files ==\n")
print(dbGetQuery(con, "SELECT zip_name, n_accounts, n_observations, status FROM processed_files ORDER BY zip_name"))
cat(sprintf("\nfilings=%d  observations=%d\n", c1$filings, c1$obs))

cat("\n== sample filings (note filing_id = company_periodend_pubmonth; submission_date = pub month) ==\n")
print(dbGetQuery(con, "SELECT filing_id, company_number, submission_date, period_end_date, dormant_status, source_zip FROM filings ORDER BY filing_id LIMIT 6"))

cat("\n== sample observations (period_end_date is the value's own period) ==\n")
print(dbGetQuery(con, "SELECT filing_id, metric, period_end_date, is_prior_period, value, unit FROM observations ORDER BY filing_id, metric LIMIT 10"))

cat("\n== metric coverage ==\n")
print(dbGetQuery(con, "SELECT metric, count(*) n FROM observations GROUP BY metric ORDER BY n DESC"))

# --- idempotency: force reprocessing both zips -> no new rows -----------------
cat("\n===== RUN 2 (skip_done = FALSE -> reprocess; expect 0 new) =====\n")
ch_build_timeseries(con, zip_names = c(basename(z_jul), basename(z_aug)),
                    zips_dir = zdir, skip_done = FALSE, workers = 4)
c2 <- cnt()
cat(sprintf("after reprocess: filings=%d (was %d)  observations=%d (was %d)\n",
            c2$filings, c1$filings, c2$obs, c1$obs))

# --- resume: with skip_done = TRUE, both are 'done' -> nothing to do ----------
cat("\n===== RUN 3 (skip_done = TRUE -> resume) =====\n")
ch_build_timeseries(con, zip_names = c(basename(z_jul), basename(z_aug)), zips_dir = zdir)

# --- verdict ------------------------------------------------------------------
ok_idempotent <- (c1$filings == c2$filings) && (c1$obs == c2$obs)
ok_keys <- all(grepl("^[A-Za-z0-9]+_[0-9]{8}_[0-9]{6}$",
                     dbGetQuery(con, "SELECT filing_id FROM filings")$filing_id))
ok_done <- nrow(dbGetQuery(con, "SELECT 1 FROM processed_files WHERE status='done'")) == 2
cat("\n===== VERDICT =====\n")
cat(sprintf("idempotent (no dup rows on reprocess): %s\n", ok_idempotent))
cat(sprintf("filing_id format valid:                %s\n", ok_keys))
cat(sprintf("processed_files has 2 done rows:       %s\n", ok_done))
cat(sprintf("ALL PASS: %s\n", ok_idempotent && ok_keys && ok_done))

# --- cleanup ------------------------------------------------------------------
ch_disconnect(con)
unlink(dbfile, force = TRUE)
unlink(zdir, recursive = TRUE, force = TRUE)
unlink("local/_extract_work", recursive = TRUE, force = TRUE)
cat("\ncleaned up throwaway DB, test zips, extract dir.\n")
