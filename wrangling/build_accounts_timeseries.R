# Incremental accounts-timeseries loader (Phase 3)
# ------------------------------------------------------------------------------
# Per-month: (download ->) unzip -> parse iXBRL (parallel) -> load into DuckDB
# (filings + observations, idempotent) -> record in processed_files -> delete.
#
# Processes a list of monthly zips NEWEST -> OLDEST. Takes an EXPLICIT zip list
# (so the same code runs the smoke test on scattered zips and the full backfill),
# or all zips in a directory. Resumable: skips zips already marked 'done'.
#
# Keys (see db/schema.sql):
#   submission_date = the zip's PUBLICATION MONTH (the bulk filename's date field
#                     is the period-end, not the submission date — verified).
#   filing_id       = <company_number>_<periodEnd:YYYYMMDD>_<pubMonth:YYYYMM>
#                     -> re-running a zip is a no-op; an amended re-file in a
#                        later month is kept as a distinct filing (lossless).
#
# Usage:
#   source("wrangling/build_accounts_timeseries.R")
#   con <- ch_connect()
#   urls <- ch_resolve_zip_urls()                       # name -> download URL
#   ch_build_timeseries(con,
#     zip_names = c("Accounts_Monthly_Data-June2025.zip",
#                   "Accounts_Monthly_Data-June2024.zip"),
#     url_lookup = urls)                                # downloads if absent
#   ch_disconnect(con)
suppressPackageStartupMessages({
  library(tidyverse)
  library(furrr)
  library(xml2)
})
source("db/connect.R")
source("db/extract_account.R")

# --- filename / zip helpers ---------------------------------------------------

# Publication month (first of month) from "Accounts_Monthly_Data-August2025.zip".
ch_zip_published_month <- function(zip_name) {
  m <- sub(".*-([A-Za-z]+)([0-9]{4})\\.zip$", "\\1 \\2", basename(zip_name))  # "August 2025"
  as.Date(paste("01", m), format = "%d %B %Y")
}

# Parse a bulk account filename: ProdNNN_YYMM_<companynumber>_<YYYYMMDD>.html
# company number is the 2nd-to-last field; the last field is the period-end date.
# Returns NULL for nested CIC zips / unparseable names (skipped, TODO).
ch_parse_bulk_filename <- function(path) {
  name <- basename(path)
  if (grepl("\\.zip$", name, ignore.case = TRUE)) return(NULL)
  parts <- strsplit(sub("\\.[^.]*$", "", name), "_")[[1]]
  if (length(parts) < 4) return(NULL)
  cn <- parts[length(parts) - 1]
  if (is.na(cn) || cn == "") return(NULL)
  file_date <- suppressWarnings(as.Date(parts[length(parts)], format = "%Y%m%d"))
  list(company_number = cn, file_date = file_date)
}

# Parse one account file into DB-ready rows. Returns list(filing, observations)
# or NULL on any failure (callers compact() these out).
ch_process_account_file <- function(path, published_month, source_zip) {
  meta <- ch_parse_bulk_filename(path)
  if (is.null(meta)) return(NULL)
  ex <- tryCatch(ch_extract_account(path), error = function(e) NULL)
  if (is.null(ex)) return(NULL)

  filing_period_end <- ex$filing$period_end
  if (is.na(filing_period_end)) filing_period_end <- meta$file_date   # fallback
  if (is.na(filing_period_end)) return(NULL)

  filing_id <- paste0(meta$company_number, "_",
                      format(filing_period_end, "%Y%m%d"), "_",
                      format(published_month, "%Y%m"))

  filing <- tibble(
    filing_id         = filing_id,
    company_number    = meta$company_number,
    submission_date   = published_month,
    period_start_date = ex$filing$period_start,
    period_end_date   = filing_period_end,
    dormant_status    = ex$filing$dormant,
    account_taxonomy  = NA_character_,        # TODO: detect FRS-102/105 from namespace
    source_zip        = source_zip,
    source_filename   = basename(path),
    extracted_at      = Sys.time()
  )

  observations <- ex$observations
  if (nrow(observations) > 0) {
    observations <- observations %>%
      transmute(filing_id = filing_id, company_number = meta$company_number,
                metric, period_end_date = period_end, period_start_date = period_start,
                is_prior_period, value, unit)
  } else {
    observations <- tibble(filing_id = character(), company_number = character(),
                           metric = character(), period_end_date = as.Date(character()),
                           period_start_date = as.Date(character()),
                           is_prior_period = logical(), value = double(), unit = character())
  }
  list(filing = filing, observations = observations)
}

# --- DB write helper ----------------------------------------------------------

# Idempotent bulk insert: register the data frame as a temp view and
# INSERT ... SELECT ... ON CONFLICT DO NOTHING. Returns rows inserted.
ch_insert_idempotent <- function(con, table, df, cols) {
  if (nrow(df) == 0) return(0L)
  df <- df[, cols, drop = FALSE]
  view <- paste0("__ins_", table)
  duckdb::duckdb_register(con, view, df)
  on.exit(duckdb::duckdb_unregister(con, view), add = TRUE)
  collist <- paste(cols, collapse = ", ")
  DBI::dbExecute(con, sprintf(
    "INSERT INTO %s (%s) SELECT %s FROM %s ON CONFLICT DO NOTHING",
    table, collist, collist, view))
}

# --- download helpers ---------------------------------------------------------

# Scrape the current + historic pages -> tibble(zip_name, url). Robust to
# whatever relative href path the archive uses.
ch_resolve_zip_urls <- function() {
  base  <- "https://download.companieshouse.gov.uk/"
  pages <- c("en_monthlyaccountsdata.html", "historicmonthlyaccountsdata.html")
  map_dfr(pages, function(pg) {
    doc  <- read_html(paste0(base, pg))
    href <- xml_attr(xml_find_all(
      doc, "//a[contains(@href, 'Accounts_Monthly_Data') and contains(@href, '.zip')]"), "href")
    tibble(zip_name = basename(href), url = paste0(base, href))
  }) %>% distinct(zip_name, .keep_all = TRUE)
}

ch_download_zip <- function(zip_name, dest_dir, url_lookup) {
  url <- url_lookup$url[url_lookup$zip_name == zip_name]
  if (length(url) == 0 || is.na(url[1])) stop("No download URL known for ", zip_name)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  dest <- file.path(dest_dir, zip_name)
  options(timeout = 36000)
  cat("  downloading", zip_name, "...\n"); t0 <- Sys.time()
  download.file(url[1], dest, mode = "wb")
  cat("   downloaded in", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
  dest
}

# --- load one zip -------------------------------------------------------------

# Unzip into a managed working dir (never the source), parse in parallel using
# whatever future plan is active, insert idempotently, record, then clean up.
ch_load_zip <- function(con, zip_path, archive_url = NA_character_,
                        extract_root = "local/_extract_work",
                        delete_extracted = TRUE, delete_zip = FALSE) {
  zip_name <- basename(zip_path)
  published_month <- ch_zip_published_month(zip_name)
  t0 <- Sys.time()

  workdir <- normalizePath(file.path(extract_root, sub("\\.zip$", "", zip_name)),
                           mustWork = FALSE)
  if (dir.exists(workdir)) unlink(workdir, recursive = TRUE, force = TRUE)
  dir.create(workdir, recursive = TRUE)
  unzip(zip_path, exdir = workdir)

  files <- list.files(workdir, full.names = TRUE, recursive = TRUE)
  files <- files[!grepl("\\.zip$", files, ignore.case = TRUE)]   # skip nested CIC zips
  cat(sprintf("  %s (pub %s): %d account files\n",
              zip_name, format(published_month, "%Y-%m"), length(files)))

  results <- future_map(files, ch_process_account_file,
                        published_month = published_month, source_zip = zip_name,
                        .options = furrr_options(
                          packages = c("xml2", "dplyr", "tibble"),
                          globals  = c("ch_process_account_file", "ch_extract_account",
                                       "ch_parse_bulk_filename", "ch_context_table",
                                       "ch_local_name", "ch_metric_tags"),
                          seed = TRUE)) %>% compact()

  filings      <- map_dfr(results, "filing")      %>% distinct(filing_id, .keep_all = TRUE)
  observations <- map_dfr(results, "observations") %>%
    distinct(filing_id, metric, period_end_date, .keep_all = TRUE)

  n_f <- ch_insert_idempotent(con, "filings", filings,
           c("filing_id", "company_number", "submission_date", "period_start_date",
             "period_end_date", "dormant_status", "account_taxonomy",
             "source_zip", "source_filename", "extracted_at"))
  n_o <- ch_insert_idempotent(con, "observations", observations,
           c("filing_id", "company_number", "metric", "period_end_date",
             "period_start_date", "is_prior_period", "value", "unit"))

  # record in the control table (refresh row if reprocessing)
  DBI::dbExecute(con, "DELETE FROM processed_files WHERE zip_name = ?", params = list(zip_name))
  ch_insert_idempotent(con, "processed_files",
    tibble(zip_name = zip_name, archive_url = archive_url,
           downloaded_at = NA, extracted_at = Sys.time(),
           n_accounts = length(files), n_observations = nrow(observations),
           status = "done"),
    c("zip_name", "archive_url", "downloaded_at", "extracted_at",
      "n_accounts", "n_observations", "status"))

  if (delete_extracted) unlink(workdir, recursive = TRUE, force = TRUE)
  if (delete_zip) unlink(zip_path, force = TRUE)

  cat(sprintf("  -> %d new filings, %d new observations, %s\n",
              n_f, n_o, format(round(difftime(Sys.time(), t0, units = "mins"), 2))))
  invisible(tibble(zip_name = zip_name, n_accounts = length(files),
                   n_filings = n_f, n_observations = n_o))
}

# --- orchestrator -------------------------------------------------------------

ch_build_timeseries <- function(con,
    zip_names = NULL,                  # explicit basenames; NULL -> all in zips_dir
    zips_dir  = "local/monthly_companieshouse_accounts",
    url_lookup = NULL,                 # tibble(zip_name, url); NULL -> no downloading
    newest_first = TRUE, limit = NULL, skip_done = TRUE,
    delete_extracted = TRUE, delete_zip = FALSE,
    workers = max(1, parallel::detectCores() - 1)) {

  if (is.null(zip_names)) {
    zip_names <- list.files(zips_dir, pattern = "Accounts_Monthly_Data-.*\\.zip$")
  }
  zip_names <- unique(basename(zip_names))
  zip_names <- zip_names[order(as.Date(sapply(zip_names, ch_zip_published_month),
                                       origin = "1970-01-01"), decreasing = newest_first)]

  if (skip_done) {
    done <- DBI::dbGetQuery(con, "SELECT zip_name FROM processed_files WHERE status = 'done'")$zip_name
    skipped <- intersect(zip_names, done)
    if (length(skipped) > 0) cat("Skipping", length(skipped), "already-done zip(s).\n")
    zip_names <- setdiff(zip_names, done)
  }
  if (!is.null(limit)) zip_names <- head(zip_names, limit)
  if (length(zip_names) == 0) { cat("Nothing to process.\n"); return(invisible(NULL)) }

  cat("Processing", length(zip_names), "zip(s), newest-first:\n  ",
      paste(zip_names, collapse = "\n  "), "\n")

  plan(multisession, workers = workers)
  on.exit(plan(sequential), add = TRUE)

  summaries <- list()
  for (zn in zip_names) {
    zip_path <- file.path(zips_dir, zn)
    if (!file.exists(zip_path)) {
      if (!is.null(url_lookup)) {
        zip_path <- ch_download_zip(zn, zips_dir, url_lookup)
      } else {
        cat("  MISSING (no url_lookup to download):", zn, "\n"); next
      }
    }
    url <- if (!is.null(url_lookup)) url_lookup$url[url_lookup$zip_name == zn][1] else NA_character_
    summaries[[zn]] <- ch_load_zip(con, zip_path, archive_url = url,
                                   delete_extracted = delete_extracted, delete_zip = delete_zip)
  }
  invisible(bind_rows(summaries))
}
