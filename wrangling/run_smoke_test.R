# Phase 6 SMOKE TEST — the gate before the multi-day full backfill.
# Downloads a handful of real monthly zips IN CODE (download.file, mode="wb",
# big timeout — same as download_monthlyaccounts.R, via the loader's
# ch_download_zip), loads them, builds the clean views, and runs an acceptance
# checklist (linkage / overlap / restatement / sparsity / timing).
#
# Run from the project root:  Rscript wrangling/run_smoke_test.R
#
# NOTE: each recent monthly zip is ~3-4 GB. The default 4 zips ~= 12-15 GB of
# download. Edit `smoke_zips` to taste; downloads land in `zips_dir` and are
# reused on re-runs (so a re-run is cheap).
suppressPackageStartupMessages(library(tidyverse))
source("wrangling/build_accounts_timeseries.R")   # loader + ch_resolve_zip_urls/ch_download_zip

# ===== CONFIGURE ==============================================================
# Same calendar month across several years maximises catching the SAME firms
# filing year after year (a firm's annual filings land ~12 months apart);
# consecutive years (2025/2024/2023) guarantee overlapping prior/current years.
smoke_zips <- c(
  "Accounts_Monthly_Data-June2025.zip",
  "Accounts_Monthly_Data-June2024.zip",
  "Accounts_Monthly_Data-June2023.zip",
  "Accounts_Monthly_Data-June2021.zip"
)
# Optional pre-iXBRL probe — older months are plain XBRL .xml, not iXBRL .html;
# include one to see whether the extractor needs an old-format branch:
# smoke_zips <- c(smoke_zips, "Accounts_Monthly_Data-June2012.zip")

smoke_db  <- "local/companieshouse_smoke.duckdb"     # disposable; set to CH_DB_PATH to keep as real series
zips_dir  <- "local/monthly_companieshouse_accounts" # download target (reused across runs)
keep_zips <- TRUE                                    # FALSE -> delete each zip after loading
workers   <- max(1, parallel::detectCores() - 1)
# =============================================================================

# --- acceptance-checklist report ---------------------------------------------
ch_smoke_report <- function(con) {
  cat("\n================ SMOKE-TEST REPORT ================\n")

  cat("\n-- processed zips --\n")
  print(dbGetQuery(con, "SELECT zip_name, n_accounts, n_observations, status
                         FROM processed_files ORDER BY zip_name"))
  tot <- dbGetQuery(con, "SELECT
      (SELECT count(*) FROM filings)                      AS filings,
      (SELECT count(*) FROM observations)                 AS observations,
      (SELECT count(DISTINCT company_number) FROM filings) AS companies")
  cat(sprintf("\nfilings=%s  observations=%s  distinct companies=%s\n",
              tot$filings, tot$observations, tot$companies))

  # 1. LINKAGE -- firms appearing across multiple filings / years
  multi <- dbGetQuery(con, "SELECT count(*) n FROM (
      SELECT company_number FROM filings GROUP BY company_number HAVING count(*) >= 2)")$n
  cat(sprintf("\n[linkage] companies with >=2 filings: %s\n", multi))
  cat("[linkage] firms with the most distinct accounting years:\n")
  print(dbGetQuery(con, "SELECT company_number,
        count(DISTINCT period_end_date) AS years,
        min(period_end_date) AS earliest, max(period_end_date) AS latest
      FROM firm_year_metrics GROUP BY company_number ORDER BY years DESC LIMIT 5"))
  top <- dbGetQuery(con, "SELECT company_number FROM firm_year_metrics WHERE metric='employees'
      GROUP BY company_number ORDER BY count(DISTINCT period_end_date) DESC LIMIT 1")$company_number
  if (length(top) == 1) {
    cat(sprintf("[linkage] reconciled series for %s:\n", top))
    print(dbGetQuery(con, sprintf("SELECT year, employees, turnover, net_assets, equity
        FROM firm_year WHERE company_number = '%s' ORDER BY year", top)))
  }

  # 2. OVERLAP / RESTATEMENT
  overlap <- dbGetQuery(con, "SELECT count(*) n FROM (
      SELECT company_number, metric, period_end_date FROM observations
      GROUP BY company_number, metric, period_end_date HAVING count(DISTINCT filing_id) >= 2)")$n
  dupclean <- dbGetQuery(con, "SELECT count(*) n FROM (
      SELECT company_number, metric, period_end_date FROM firm_year_metrics
      GROUP BY company_number, metric, period_end_date HAVING count(*) > 1)")$n
  cat(sprintf("\n[overlap] (company,metric,period) reported by >=2 filings: %s\n", overlap))
  cat(sprintf("[overlap] duplicate rows in firm_year_metrics (must be 0): %s\n", dupclean))
  cat("[overlap] example genuine restatements (kept newest != a superseded value):\n")
  print(dbGetQuery(con, "SELECT o.company_number, o.metric, o.period_end_date,
        fym.value AS kept_value, o.value AS superseded_value, fym.source_filing
      FROM observations o JOIN firm_year_metrics fym
        ON fym.company_number=o.company_number AND fym.metric=o.metric
       AND fym.period_end_date=o.period_end_date
      WHERE o.filing_id <> fym.source_filing AND o.value <> fym.value LIMIT 10"))

  # 3. SPARSITY
  cat("\n[sparsity] metric coverage:\n")
  print(dbGetQuery(con, "SELECT metric, count(DISTINCT company_number) AS companies,
        count(*) AS observations FROM observations GROUP BY metric ORDER BY companies DESC"))

  cat("\n(per-zip download + parse times are printed above; the full archive is ~216 monthly zips)\n")
}

# --- run ----------------------------------------------------------------------
con <- ch_connect(smoke_db)
ch_init_schema(con)
ch_init_views(con)

cat("Resolving download URLs from Companies House (current + historic pages) ...\n")
urls <- ch_resolve_zip_urls()
cat(sprintf("  %d monthly zips advertised across both pages.\n", nrow(urls)))
missing <- setdiff(smoke_zips, urls$zip_name)
if (length(missing) > 0)
  cat("  WARNING: not found on CH pages (check month spelling):\n    ",
      paste(missing, collapse = "\n    "), "\n")

t0 <- Sys.time()
ch_build_timeseries(con, zip_names = smoke_zips, zips_dir = zips_dir,
                    url_lookup = urls, delete_zip = !keep_zips, workers = workers)
cat(sprintf("\nTotal download+load wall time: %s\n",
            format(round(difftime(Sys.time(), t0, units = "mins"), 1))))

ch_smoke_report(con)

# resume check (cheap: should skip everything, no re-parse)
cat("\n[resume] re-running the same list (should skip all, nothing to process):\n")
ch_build_timeseries(con, zip_names = smoke_zips, zips_dir = zips_dir, url_lookup = urls)

# OPTIONAL: companies dimension -> enables firm_year <-> companies joins + lifecycle.
# Needs the geocoded live list from your existing download + geocode stages.
geo <- "local/companieshouse_livelist_geocoded.rds"
if (file.exists(geo)) {
  source("wrangling/load_livelist_to_db.R")
  cat("\nLoading companies dimension from", geo, "...\n")
  ch_load_livelist(con, readRDS(geo), snapshot_date = Sys.Date())
} else {
  cat("\n(companies dimension skipped:", geo, "not present — run the live-list + geocode",
      "stages to enable firm_year<->companies joins and lifecycle.)\n")
}

ch_disconnect(con)
cat("\nSmoke test complete. DB:", smoke_db, "\n")
