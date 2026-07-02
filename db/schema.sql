-- Companies House accounts time series — base schema (DuckDB)
-- Phase 1 of docs/guides/timeseries_accounts_db_plan.md
-- Idempotent: safe to run repeatedly (CREATE TABLE IF NOT EXISTS).
-- Company numbers are TEXT throughout (leading zeros are significant).

-- One row per company (dimension; upserted from the live list)
CREATE TABLE IF NOT EXISTS companies (
  company_number       TEXT PRIMARY KEY,
  company_name         TEXT,
  incorporation_date   DATE,          -- firm "birth"
  company_category     TEXT,
  company_status       TEXT,          -- current status (latest live-list refresh)
  sic_1 TEXT, sic_2 TEXT, sic_3 TEXT, sic_4 TEXT,
  postcode             TEXT,
  localauthority_code  TEXT,
  localauthority_name  TEXT,
  itl221cd             TEXT,
  itl221nm             TEXT,
  easting              DOUBLE,        -- coords as plain columns; rebuild sf in R on demand
  northing             DOUBLE,
  first_seen           DATE,          -- first live-list snapshot this firm appeared in
  last_seen            DATE           -- most recent snapshot; a gap => likely dissolved/removed
);

-- Append-only status snapshots (supports firm birth/death detection)
CREATE TABLE IF NOT EXISTS company_status_history (
  company_number TEXT,
  snapshot_date  DATE,                -- date of the live-list download
  company_status TEXT,
  PRIMARY KEY (company_number, snapshot_date)
);

-- One row per account document (append-only, idempotent on filing_id)
CREATE TABLE IF NOT EXISTS filings (
  filing_id         TEXT PRIMARY KEY, -- e.g. company_number || '_' || accountcode (submission date)
  company_number    TEXT,
  submission_date   DATE,             -- = accountcode; used to pick newest filing on restatement
  period_start_date DATE,
  period_end_date   DATE,             -- = enddate; the period these accounts cover
  dormant_status    TEXT,
  account_taxonomy  TEXT,             -- micro/small/full if detectable; helps explain sparsity
  source_zip        TEXT,
  source_filename   TEXT,
  extracted_at      TIMESTAMP
);

-- Long format: one row per (filing, metric, period) — the raw lossless layer.
-- Each filing emits a current-period AND a prior-period row per metric,
-- each dated by its own period_end_date. Restatements => multiple rows for the
-- same (company_number, metric, period_end_date) from filings with different
-- submission_date; the clean layer (phase 5) resolves "newest filing wins".
CREATE TABLE IF NOT EXISTS observations (
  filing_id         TEXT,
  company_number    TEXT,             -- denormalised for query convenience
  metric            TEXT,             -- 'employees','turnover','profit_loss','net_assets',...
  period_end_date   DATE,             -- the period THIS value refers to (current OR prior year)
  period_start_date DATE,
  is_prior_period   BOOLEAN,          -- TRUE = the comparative ("last year") figure in the filing
  value             DOUBLE,
  unit              TEXT,             -- 'GBP' | 'count'
  PRIMARY KEY (filing_id, metric, period_end_date)
);

-- Control table: which monthly zips have been processed (incremental, resumable runs)
CREATE TABLE IF NOT EXISTS processed_files (
  zip_name       TEXT PRIMARY KEY,
  archive_url    TEXT,
  downloaded_at  TIMESTAMP,
  extracted_at   TIMESTAMP,
  n_accounts     INTEGER,
  n_observations INTEGER,
  status         TEXT                 -- 'done' | 'failed' | 'partial'
);
