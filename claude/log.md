# Accounts Time-Series Database — Log & Usage

A running record of the per-firm accounts **DuckDB** database: how to interact with it, plus a dated change log as it's built out. Plan it follows: [../docs/guides/timeseries_accounts_db_plan.md](../docs/guides/timeseries_accounts_db_plan.md).

---

## Quick reference — how to use the DB

### Where things live

| Thing | Path |
|---|---|
| Database file (gitignored, under `local/`) | `local/companieshouse.duckdb` |
| Schema (table definitions) | [../db/schema.sql](../db/schema.sql) |
| Connection + init helpers | [../db/connect.R](../db/connect.R) |
| Create / re-init the database | [../db/create_database.R](../db/create_database.R) |

Engine: **DuckDB 1.5.2** (R package `duckdb`); `duckplyr` 1.2.1 also installed. Run R scripts **from the project root** so the relative paths resolve.

### Connect, query, disconnect

```r
source("db/connect.R")        # loads DBI + duckdb; defines the helpers below
library(dplyr)                # needed for tbl() / dbplyr translation

con <- ch_connect()           # opens local/companieshouse.duckdb (creates it if missing)

# --- dplyr / dbplyr: write dplyr, it runs inside DuckDB (lazy until collect()) ---
tbl(con, "filings") |>
  filter(period_end_date >= as.Date("2023-01-01")) |>
  count(company_number) |>
  collect()                   # pull results into a normal tibble

# --- raw SQL when you prefer ---
dbGetQuery(con, "SELECT count(*) AS n FROM observations")   # returns a data.frame
dbExecute(con, "DELETE FROM processed_files WHERE status = 'failed'")  # returns rows affected

ch_disconnect(con)            # closes the connection AND shuts the database down
```

### Connection options

- `ch_connect()` — read-write (default).
- `ch_connect(read_only = TRUE)` — for analysis / a future web layer querying **while** a load runs in another process. DuckDB allows **one read-write** connection at a time; a stray open R session holding the file will block writers, so `ch_disconnect()` when done.

### Writing data in (loader patterns)

```r
# Append a data frame (column names must match the table)
DBI::dbAppendTable(con, "observations", df)

# Idempotent insert — re-running a month/zip is a safe no-op on rows already present.
# This is the pattern the incremental loader relies on (keyed on the table's primary key).
dbExecute(con, "
  INSERT INTO processed_files (zip_name, status)
  VALUES ('Accounts_Monthly_Data-June2025.zip', 'done')
  ON CONFLICT DO NOTHING")
```

### (Re)create or inspect the schema

```bash
Rscript db/create_database.R          # idempotent: CREATE TABLE IF NOT EXISTS for all tables
```

```r
dbListTables(con)                     # list tables
dbGetQuery(con, "DESCRIBE observations")                       # columns + types + PK flags
dbGetQuery(con, "SELECT table_name, constraint_text
                 FROM duckdb_constraints()
                 WHERE constraint_type = 'PRIMARY KEY'")        # primary keys
```

---

## Schema reference

Five base tables. Company numbers are **TEXT** everywhere (leading zeros are significant). Full DDL with column comments: [../db/schema.sql](../db/schema.sql).

| Table | Purpose | Primary key |
|---|---|---|
| `companies` | One row per company (dimension; upserted from the live list). Holds name, incorporation date, status, SIC codes, postcode/LA/ITL2, easting/northing, `first_seen`/`last_seen`. | `company_number` |
| `company_status_history` | Append-only status snapshot per live-list refresh — supports detecting firm **births** (incorporation) and **closures** (status change / disappearance). | `(company_number, snapshot_date)` |
| `filings` | One row per account document. Submission date, period start/end, dormant flag, taxonomy, source zip/filename. | `filing_id` |
| `observations` | **Raw lossless layer**, long format: one row per (filing, metric, period). Each filing emits a current- and prior-period row per metric, each dated by its own `period_end_date`. Restatements appear as multiple rows for the same firm-year from different filings. | `(filing_id, metric, period_end_date)` |
| `processed_files` | Control table: which monthly zips are done (drives resumable, incremental runs). | `zip_name` |

> The derived **clean layer** (`firm_year_metrics` view + wide `firm_year`) — "newest filing wins" per firm-year-metric — is built later (plan Phase 5) and will be documented here when added.

---

## Extracting account data (parser)

The iXBRL parser is [../db/extract_account.R](../db/extract_account.R). It is **content-only** — the loader (Phase 3) adds `company_number` / `accountcode` / `filing_id` / source from the filename + zip.

```r
source("db/extract_account.R")
res <- ch_extract_account("path/to/account.html")
res$filing        # 1 row : company_name, period_start, period_end, dormant
res$observations  # long  : metric, period_start, period_end, is_prior_period, value, unit
```

- Values are **dated by their `contextRef` period** (durations for flows, instants for balance-sheet stocks), not by document order — so a filing's *submission date* is never confused with its *accounting year*.
- XBRL **scale** (×10^scale) and **sign** (negate if `-`) are applied; tags are matched by **local name** (prefix-agnostic).
- Metrics captured are configured in `ch_metric_tags` near the top of the file — **add a row to capture a new tag**. Current set: employees, turnover, gross/operating/before-tax profit, profit_loss, fixed/current assets, cash, debtors, creditors, net_assets, equity.
- **Known v1 gap:** only **non-dimensional** headline facts are taken. Where a filing reports a metric *only* in dimensional/segment contexts (commonly `ProfitLoss` and `Creditors` in large group accounts), it is **skipped** rather than risk wrongly summing segments. Balance-sheet stocks + employees capture reliably.

---

## Loading accounts (incremental loader)

The loader is [../wrangling/build_accounts_timeseries.R](../wrangling/build_accounts_timeseries.R) (sources `connect.R` + `extract_account.R`).

```r
source("wrangling/build_accounts_timeseries.R")
con  <- ch_connect()
urls <- ch_resolve_zip_urls()                 # scrape current + historic pages -> name->URL

# Smoke test / explicit list (downloads any zip not already in zips_dir):
ch_build_timeseries(con,
  zip_names = c("Accounts_Monthly_Data-June2025.zip",
                "Accounts_Monthly_Data-June2024.zip",
                "Accounts_Monthly_Data-June2023.zip"),
  url_lookup = urls)

# Full backfill (all zips found, newest-first, resumable; delete zips after to save disk):
# ch_build_timeseries(con, url_lookup = urls, delete_zip = TRUE)

ch_disconnect(con)
```

Behaviours:
- **Newest → oldest**; **resumable** — skips zips already `status='done'` in `processed_files`. `skip_done = FALSE` forces reprocessing (idempotent: 0 new rows).
- Per zip: unzip into `local/_extract_work` → parallel parse (`furrr`, workers = cores−1) → idempotent insert → record → delete extracted. `delete_zip = TRUE` also removes the multi-GB zip.
- `submission_date` = the zip's **publication month**; `filing_id = <company>_<periodEnd YYYYMMDD>_<pubMonth YYYYMM>`. An amended re-file in a later month is stored as a **distinct** filing (lossless); the clean layer picks newest.
- `account_taxonomy` is left `NA` for now (TODO: detect FRS-102/105 from namespace). CIC nested zips are skipped (TODO).

---

## Loading the live list (companies dimension)

[../wrangling/load_livelist_to_db.R](../wrangling/load_livelist_to_db.R) upserts the geocoded live list into `companies` and appends a `company_status_history` snapshot per refresh.

```r
source("wrangling/load_livelist_to_db.R")
con <- ch_connect()
ch.geo <- readRDS("local/companieshouse_livelist_geocoded.rds")   # sf (or any df with live-list cols)
ch_load_livelist(con, ch.geo, snapshot_date = "2025-03-01")

# lifecycle helpers
ch_company_births(con, "2024-01-01", "2024-12-31")   # incorporated in range (births)
ch_presumed_closed(con)                               # dropped off live list since latest snapshot
ch_closing_signals(con)                               # current status = strike-off / liquidation / ...
ch_disconnect(con)
```

- Upsert keeps `first_seen` fixed and refreshes name / status / SIC / geo / `last_seen` to the snapshot. Re-running a snapshot is idempotent (`company_status_history` keyed on company + date).
- Coordinates come from the sf geometry (or `easting`/`northing` columns) and are stored as plain numeric columns — rebuild sf in R with `st_as_sf(coords=c("easting","northing"), crs=27700)`.
- **Closures** are detected two ways: a closing **status** signal, or **disappearance** (`last_seen` < latest snapshot, since dissolved firms drop off the live list). **Births** = `incorporation_date`.

---

## Querying the clean layer (firm_year_metrics / firm_year)

Two always-live views reconcile raw `observations` (**newest filing wins** on overlap/restatement). Defined in [../db/views.sql](../db/views.sql); build/refresh with `Rscript db/create_views.R` or `ch_init_views(con)`.

- `firm_year_metrics` — long: one authoritative value per (company, metric, `period_end_date`), with `source_filing` provenance.
- `firm_year` — wide: one row per (company, accounting period); the 13 metrics as columns.

```r
con <- ch_connect(read_only = TRUE)
library(dplyr)
# employee/financial time series for one firm
tbl(con, "firm_year") |>
  filter(company_number == "01772901") |>
  select(year, employees, turnover, net_assets) |> arrange(year) |> collect()
# join the companies dimension (name, sector, geo)
tbl(con, "firm_year") |> left_join(tbl(con, "companies"), by = "company_number")
ch_disconnect(con)
```

For big-scale analysis / a web layer, `ch_materialise_clean(con)` snapshots both views into physical `firm_year_metrics_mat` / `firm_year_mat` tables.

---

## Notes & gotchas

- **One writer.** DuckDB locks the file for the single read-write connection. Close sessions you're not using; use `read_only = TRUE` for concurrent analysis.
- **Always `ch_disconnect()`** (it passes `shutdown = TRUE`) so the file isn't left locked.
- **`duckplyr` install bumped `dplyr` → 1.2.1** (plus `rlang`/`vctrs`/`lifecycle`). If existing tidyverse scripts behave oddly, check there first.
- **Running ad-hoc R via `Rscript -e '...'`**: single quotes inside the single-quoted shell string get eaten (breaks SQL string literals like `'PRIMARY KEY'`). Put multi-line R with SQL in a `.R` file and run `Rscript file.R` instead.
- The `package 'duckdb' was built under R version 4.5.2` message on load is harmless.

---

## Change log

### 2026-06-02 — Phase 1: foundations ✅

- Installed `duckdb` 1.5.2 and `duckplyr` 1.2.1 (binaries).
- Added [../db/schema.sql](../db/schema.sql) (5 base tables, idempotent), [../db/connect.R](../db/connect.R) (connection + multi-statement SQL runner + schema init), [../db/create_database.R](../db/create_database.R).
- Created `local/companieshouse.duckdb` and initialised all five tables.
- **Verified:** all primary keys present (incl. composite `observations` and `company_status_history` keys); `INSERT … ON CONFLICT DO NOTHING` idempotency works (two identical inserts → 1 row); all tables empty / DB left clean.

_Next: Phase 2 — parser upgrade (date each value by its `contextRef` period; add key financial tags), tested against `test_accounts/`._

### 2026-06-02 — Phase 2: parser upgrade ✅

- Added [../db/extract_account.R](../db/extract_account.R) — `ch_extract_account()` returns `list(filing, observations)`; dates values by `contextRef`, applies scale/sign, matches by local name, captures employees + financials as long rows. See [Extracting account data](#extracting-account-data-parser).
- Added [../testcode/test_extract_account.R](../testcode/test_extract_account.R) (reusable; uses the gitignored Gripple + August-2025 samples).
- **Verified:**
  - **Regression** — employees match the old positional extractor on all 4 Gripple filings.
  - **Context dating** — submission date correctly distinguished from accounting year (file submitted 2022-09-08 → FY2021).
  - **Linkage** — 4 Gripple filings → one clean employee series 2020–2024 (828, 901, 957, 932, 956); overlapping years consistent (no restatement); newest-filing-wins works.
  - **Sparsity** (50 random Aug-2025 small firms) — equity 96%, net_assets 84%, employees 82%, current_assets 74%, cash 50%, fixed/debtors ~37%; turnover/profit_loss only 2–4% → confirms small firms file balance-sheet-only.

_Next: Phase 3 — incremental loader (`wrangling/build_accounts_timeseries.R`): per-month download→extract→load→delete, taking an explicit zip list, writing `filings` + `observations` with `processed_files` control + idempotency._

### 2026-06-02 — Phase 3: incremental loader ✅

- Added [../wrangling/build_accounts_timeseries.R](../wrangling/build_accounts_timeseries.R): `ch_build_timeseries()` orchestrator + `ch_load_zip()`, `ch_process_account_file()`, `ch_resolve_zip_urls()` / `ch_download_zip()`, `ch_insert_idempotent()`. See [Loading accounts](#loading-accounts-incremental-loader).
- Confirmed the bulk filename's date field is the **period-end** (10/10 vs content); submission ordering therefore taken from the zip's **publication month**.
- Added [../testcode/test_loader.R](../testcode/test_loader.R) (throwaway DB + 2 synthetic zips built from the Aug-2025 sample).
- **Verified:** newest-first ordering; 60 filings / 538 observations; `filing_id` / `submission_date` / `period_end_date` all correct and distinct; **idempotent** reprocess (0 new rows, counts unchanged); **resume** skips done zips; metric coverage matches the Phase-2 sparsity profile. Only warnings are the benign "built under R version" notices.

_Next: Phase 4 — companies dimension & lifecycle: upsert the geocoded live list into `companies`; append a `company_status_history` snapshot per refresh; derive firm births (incorporation date) and closures (status change / disappearance between snapshots)._

### 2026-06-02 — Phase 4: companies dimension & lifecycle ✅

- Added [../wrangling/load_livelist_to_db.R](../wrangling/load_livelist_to_db.R): `ch_load_livelist()` (upsert `companies` + append status history), `ch_prepare_companies()` (maps geocoded-live-list / sf → schema, extracts coords, parses dates), and lifecycle helpers `ch_company_births()` / `ch_presumed_closed()` / `ch_closing_signals()`. See [Loading the live list](#loading-the-live-list-companies-dimension).
- Added [../testcode/test_livelist.R](../testcode/test_livelist.R) (synthetic two-snapshot scenario).
- **Verified:** all 11 checks pass — upsert refresh (rename; `first_seen` fixed; `last_seen` advances), disappearance → presumed-closed (C & E), status → closing signal (B striking off), incorporation → birth (D), `status_history` row counts, full idempotency on reload.
- **Real-data smoke:** combined Dec-2025 RDS (**3,574,067 rows, sf**) loaded in ~48s; `easting`/`northing` + `incorporation_date` 100% populated; statuses all "Active" (file is pre-filtered, so closure paths were exercised via the synthetic test).

_Next: Phase 5 — clean layer: `firm_year_metrics` view (newest filing wins per firm-year-metric) + wide `firm_year`, joined to `companies`._

### 2026-06-02 — Phase 5: clean layer ✅

- Added [../db/views.sql](../db/views.sql) (`firm_year_metrics` + `firm_year`), [../db/create_views.R](../db/create_views.R) runner, and `ch_init_views()` / `ch_materialise_clean()` in [../db/connect.R](../db/connect.R). See [Querying the clean layer](#querying-the-clean-layer-firm_year_metrics--firm_year).
- `firm_year_metrics`: newest-filing-wins per (company, metric, period_end) via `QUALIFY row_number()` ordered by `submission_date`, filing `period_end_date`, `filing_id`; keeps `source_filing`. `firm_year`: wide pivot (13 metric columns) per company-period.
- Added [../testcode/test_clean_layer.R](../testcode/test_clean_layer.R). **Verified** on a restatement scenario: 2023 employees reported as 100 (2024 filing, current) and 105 (2025 filing, restated prior) → reconciled to **105** from the newer filing; clean series 90/105/110; wide pivot + `companies` join correct; materialised tables identical to views.
- Views created in the persistent `local/companieshouse.duckdb`.

**Pipeline status: Phases 1–5 (the whole non-download build) complete.** `companieshouse.duckdb` has all 5 tables + 2 views, empty, ready to populate.

_Next: Phase 6 — the smoke-test **gate** (needs real downloads): run the full chain on a handful of scattered real zips + a live-list snapshot, then work through the plan's acceptance checklist before the multi-day Phase 7 backfill._

### 2026-06-02 — Phase 6 prep: smoke-test runner ready ⏳ (needs downloads)

- Added [../wrangling/run_smoke_test.R](../wrangling/run_smoke_test.R): downloads a configurable set of real monthly zips **in code** (`download.file(mode="wb")` via `ch_download_zip`, as in `download_monthlyaccounts.R`), loads them, builds the views, prints an acceptance-checklist report (linkage / overlap / restatement / sparsity / timing), confirms resume, and optionally loads the `companies` dimension if the geocoded live list is present. Writes to a disposable `local/companieshouse_smoke.duckdb` by default.
- **Verified URL resolution** (no GB downloads): `ch_resolve_zip_urls()` finds **198** monthly zips across the current + historic pages, **2010-01 → 2026-04**; recent files at the site root, archive files under `/archive/` — both handled. All candidate smoke zips resolve.
- **To run the gate:** edit `smoke_zips` in the script (default: June 2025/2024/2023/2021, ~12–15 GB), then `Rscript wrangling/run_smoke_test.R`.

_Then: Phase 7 — full backfill (`ch_build_timeseries(con, url_lookup = ch_resolve_zip_urls(), delete_zip = TRUE)`), newest→oldest, resumable, after the gate passes._
