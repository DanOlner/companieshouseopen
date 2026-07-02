# Plan: Per-Firm Accounts Time Series + Database

## Goal

Move from "latest accounts only, stored in flat RDS files" to a **per-firm time series** of company accounts, held in a **lightweight analytical database**.

Two concrete changes:

1. **Extract and link multiple accounts per firm** across many monthly archive files, building a time series of employees + key financials per company.
2. **Store it in a proper database** (not flat files / RDS) that fits the scale, works well with R and the tidyverse, and keeps the door open to a web-facing interface later.

Decisions taken up front (see [Decisions](#decisions-taken)):

- **History**: process the archive **most-recent-first**, *upserting* as we go — update firms already in the DB, add new ones, and flag firm **births** (incorporation date) and **closures** (status change / disappearance from the live list).
- **Fields**: **employees + key financials** (captured in one parse pass — re-downloading later to add fields is expensive).
- **Overlaps/restatements**: **store raw observations losslessly, plus a derived "clean" one-row-per-firm-per-year view.**
- **Web**: pick a DB usable locally now that can serve a web app/API later **without re-migrating the data**.

---

## Current state (recap)

The [wrangling/](../../wrangling/) pipeline today:

| Stage | Script | Output |
|---|---|---|
| 1 | [download_current_CH_livelist…R](../../wrangling/download_current_CH_livelist_n_save_reducedcopy_locally.R) | Live list of ~5M companies, reduced columns → RDS |
| 2 | [CH_livelist_addgeolocation.R](../../wrangling/CH_livelist_addgeolocation.R) | Live list + postcode→LA/ITL2/eastings-northings → `sf` RDS |
| 3 | [download_monthlyaccounts.R](../../wrangling/download_monthlyaccounts.R) | Downloads **only the last 12 months** of account zips |
| 4 | [extractinfo_fromaccountfiles.R](../../wrangling/extractinfo_fromaccountfiles.R) | Unzips, parses iXBRL per account (parallel `furrr`), one RDS per month |
| 5 | [combine_account_extracts_n_geocode.R](../../wrangling/combine_account_extracts_n_geocode.R) | **Keeps only the most-recent account per firm**, joins to live list → combined RDS |

Two facts about the source data that drive the design:

- **Each filing already carries two years of employees** — `AverageNumberEmployeesDuringPeriod` for the current *and* prior period ([functions.R:269-277](../../functions.R#L269-L277)). So successive filings **overlap** on shared years, and figures can be **restated**.
- **`enddate` (accounting-period end) ≠ `accountcode` (submission date).** The *period end* is the real time axis for a series; the submission date tells us which filing a value came from (and which to trust on conflict).

The archive ([historicmonthlyaccountsdata.html](https://download.companieshouse.gov.uk/historicmonthlyaccountsdata.html)) is `Accounts_Monthly_Data-<Month><Year>.zip`, 60 MB (early) to ~4 GB (recent). A live scrape (Phase-6 prep) found **~198 monthly zips spanning 2010 → 2026** — recent months at the site root, older months under `/archive/`. The full archive is several hundred GB of downloads — another reason to go recent-first and incremental.

---

## Database recommendation

**Use DuckDB as the system of record, with Parquet as the export/interchange layer, and Postgres/PostGIS reserved as the scale-out target if a high-concurrency web app later needs it.**

### Why DuckDB

- **Scale fit ("big but not huge").** Columnar, vectorised engine built for analytical scans and group-bys over tens of millions to low-billions of rows on a single machine. Our data is ~10⁸–10⁹ observation rows at most (see [sizing](#sizing-estimate)) — comfortably in range. Single-file database, no server.
- **Tidyverse-native.** Three complementary R paths, all installed-or-one-`install.packages`-away:
  - `duckdb` + `DBI` — persistent on-disk database, SQL, transactions, `INSERT … ON CONFLICT` upserts.
  - `dbplyr` (**already installed**) — write `dplyr`, it's translated to DuckDB SQL and runs in-engine (lazy; `collect()` to pull results).
  - `duckplyr` — a *drop-in* `dplyr` replacement that runs on DuckDB and **falls back to dplyr** for anything unsupported. It reached 1.1.0 and "fully joined the tidyverse" in 2025.
- **You already use Parquet** (`samplebatch.parquet`, the Python/SetFit side). DuckDB reads and writes Parquet natively, so the analytical store and the ML side share one columnar format.
- **Keeps the web door open** (your stated requirement) — see [Web path](#web-facing-path).

### Options considered

| Option | Scale fit | R / tidyverse | Web later | Spatial | Upsert / incremental | Ops burden | Verdict |
|---|---|---|---|---|---|---|---|
| **DuckDB** | Excellent — columnar, fast aggregations | Excellent — `duckdb`+`dbplyr`; `duckplyr` drop-in; native Parquet | Good — Shiny/Plumber now, DuckDB-WASM static explorer, MotherDuck hosted; clean migrate to PG | `spatial` ext, or store coords + rebuild `sf` | `INSERT…ON CONFLICT`, PKs, transactions | Minimal (embedded, 1 file) | ✅ **System of record** |
| SQLite | OK for lookups; slow on big analytical scans | Good — `RSQLite`+`dbplyr` | Good — ubiquitous, trivial to serve | SpatiaLite ext | Upsert supported | Minimal | Only if you need row-oriented transactional access |
| Postgres + PostGIS | Excellent, scales past one machine | Good — `RPostgres`+`dbplyr` | **Best** for concurrent multi-user apps/APIs | First-class PostGIS | Full | Higher — run/maintain a server | Migration target when a web app needs concurrency |
| Parquet + `arrow` (files only) | Great for analytics; you use it already | Good — `arrow`+`dplyr` | Good — static hosting / DuckDB-WASM | none native | **No** upsert / constraints / indexes | Minimal | Export/interchange layer, **not** the system of record |

Parquet-only fails the "upsert as we go + lifecycle flags" requirement (no constraints, no in-place update). SQLite is row-oriented and slow for the wide group-bys we'll run constantly. Postgres is the right answer *only* once concurrent multi-user writing/serving is real — and because our model is plain SQL tables + Parquet, that migration is mechanical (DuckDB can export straight to Postgres). That portability is exactly what "keep the door open" buys.

### Web-facing path (door kept open)

Same data model serves, in increasing order of effort:

- **Now / local**: query the `.duckdb` file directly from R (Shiny dashboard via `duckdb`, or a `plumber` REST API).
- **Cheap public explorer**: export clean tables to **Parquet** and query them client-side with **DuckDB-WASM** in the browser — host as static files (GitHub Pages / Netlify / S3), no server to run. A strong fit for the repo's "nice friendly open data" mission.
- **Hosted SQL endpoint**: **MotherDuck** (managed DuckDB) if you want a cloud endpoint without infra.
- **Scale-out**: migrate to **Postgres + PostGIS** when concurrent writes / many simultaneous users arrive. No re-modelling — tables and types carry over.

---

## Data model

Four core tables plus derived views. Company numbers are **TEXT** throughout (leading zeros are significant). The *raw* layer is append-only and lossless; the *clean* layer is derived.

```
companies              ── one row per company (dimension; upserted from live list)
company_status_history ── append-only snapshots of status (supports birth/death detection)
filings                ── one row per account document (append-only, idempotent)
observations           ── long format: one row per (filing, metric, period) — the raw lossless layer
processed_files        ── control table: which monthly zips are done (incremental runs)

→ firm_year_metrics    ── derived VIEW/table: one authoritative value per (firm, metric, year)
→ firm_year            ── derived wide table: one row per (firm, year), metrics as columns (tidyverse-friendly)
```

### Schema (DuckDB DDL sketch)

```sql
CREATE TABLE companies (
  company_number      TEXT PRIMARY KEY,
  company_name        TEXT,
  incorporation_date  DATE,         -- firm "birth"
  company_category    TEXT,
  company_status      TEXT,         -- current status (latest live-list refresh)
  sic_1 TEXT, sic_2 TEXT, sic_3 TEXT, sic_4 TEXT,
  postcode            TEXT,
  localauthority_code TEXT, localauthority_name TEXT,
  itl221cd TEXT, itl221nm TEXT,
  easting DOUBLE, northing DOUBLE,  -- coords as plain columns; rebuild sf in R on demand
  first_seen DATE,                  -- first live-list snapshot this firm appeared in
  last_seen  DATE                   -- most recent snapshot; gap ⇒ likely dissolved/removed
);

CREATE TABLE company_status_history (
  company_number TEXT,
  snapshot_date  DATE,              -- date of the live-list download
  company_status TEXT,
  PRIMARY KEY (company_number, snapshot_date)
);

CREATE TABLE filings (
  filing_id        TEXT PRIMARY KEY,  -- e.g. company_number || '_' || accountcode (submission date)
  company_number   TEXT,
  submission_date  DATE,              -- = accountcode; used to pick newest on restatement
  period_start_date DATE,
  period_end_date  DATE,              -- = enddate; the period these accounts cover
  dormant_status   TEXT,
  account_taxonomy TEXT,              -- micro/small/full if detectable; helps explain sparsity
  source_zip       TEXT,
  source_filename  TEXT,
  extracted_at     TIMESTAMP
);

CREATE TABLE observations (
  filing_id        TEXT,
  company_number   TEXT,             -- denormalised for query convenience
  metric           TEXT,             -- 'employees','turnover','profit_loss','net_assets',...
  period_end_date  DATE,             -- the period THIS value refers to (current OR prior year)
  period_start_date DATE,
  is_prior_period  BOOLEAN,          -- TRUE = the comparative ("last year") figure in the filing
  value            DOUBLE,
  unit             TEXT,             -- 'GBP' | 'count'
  PRIMARY KEY (filing_id, metric, period_end_date)
);

CREATE TABLE processed_files (
  zip_name      TEXT PRIMARY KEY,
  archive_url   TEXT,
  downloaded_at TIMESTAMP,
  extracted_at  TIMESTAMP,
  n_accounts    INTEGER,
  n_observations INTEGER,
  status        TEXT                 -- 'done' | 'failed' | 'partial'
);
```

### Why long-format `observations`

The long shape is what makes the overlap/restatement problem tidy:

- Each filing emits, **per metric**, a current-period row **and** a prior-period row — each **correctly dated by its own `period_end_date`**. The "two employee values" stop being positional `[1]`/`[2]` guesses and become two properly-dated facts.
- **Restatements** appear naturally as multiple rows for the same `(company_number, metric, period_end_date)` from filings with different `submission_date`s. Nothing is lost.
- Adding a new metric later = new rows, not a schema migration.

### Derived "clean" layer

A view (or materialised table refreshed after each load) collapses raw observations to one authoritative value per firm-year-metric — **newest filing wins** on conflict:

```sql
CREATE VIEW firm_year_metrics AS
SELECT company_number, metric, period_end_date,
       arg_max(o.value, f.submission_date) AS value   -- value from the latest-submitted filing
FROM observations o
JOIN filings f USING (filing_id)
GROUP BY company_number, metric, period_end_date;
```

Then a wide, tidyverse-friendly `firm_year` (one row per firm × year, metrics as columns) via `PIVOT`/`pivot_wider`, optionally joined to `companies` for sector/geography. This is the table most analysis and any web front-end would read.

---

## Extraction changes needed

The parser ([functions.R `get_accounts_data`](../../functions.R#L225-L279)) needs two upgrades.

### 1. Date each value by its context (correctness fix)

Today the code takes `employeevals[1]` / `[2]` and *assumes* order = this-year/last-year. For a reliable series, read each fact's **`contextRef`**, resolve the matching `<xbrli:context>` → `<period>` → `endDate`/`instant`, and attach the real period to the value. Pseudocode:

```r
# For each ix:nonFraction / ix:nonNumeric node of interest:
#   1. ctx_id   <- xml_attr(node, "contextRef")
#   2. context  <- xml_find_first(doc, sprintf("//*[@id='%s']", ctx_id), ns)
#   3. period_end   <- text of .//*[local-name()='endDate' or local-name()='instant']
#      period_start <- text of .//*[local-name()='startDate']   (NA for instants)
#   4. emit row: metric, value, period_start, period_end, is_prior_period
# is_prior_period = period_end < max(period_end across this filing's facts for that metric)
```

This makes employees, and every financial metric, land on the correct year regardless of document order.

### 2. Add key financial tags

Match on `contains(@name, …)` (as the existing code does) — tag names vary across taxonomy versions. Starter set:

| Metric | iXBRL name fragment(s) | Availability note |
|---|---|---|
| `employees` | `AverageNumberEmployeesDuringPeriod` | **High** — mandatory, incl. micro-entities |
| `net_assets` | `NetAssetsLiabilities` | **High** — on the balance sheet |
| `fixed_assets` | `FixedAssets` | High |
| `current_assets` | `CurrentAssets` | High |
| `cash` | `CashBankOnHand` | Medium |
| `creditors` | `Creditors` | Medium |
| `equity` | `Equity`, `ShareholderFunds` (older) | Medium |
| `turnover` | `TurnoverRevenue`, `Turnover` | **Lower** — often omitted by micro/small |
| `profit_loss` | `ProfitLoss`, `ProfitLossOnOrdinaryActivitiesBeforeTax` | **Lower** — often omitted by micro/small |

> **Reality check:** most CH-filed accounts are micro-entity (FRS 105) or small — frequently **balance-sheet only**. Expect `turnover`/`profit_loss` to be **sparse**, while employees and balance-sheet items are well populated. Capturing all of them now is still right (one parse pass; re-downloading later is the expensive path), but the clean layer should treat missing financials as expected, not error.

### Also handle / flag

- **Taxonomy drift 2008→2025.** Older years are plain **XBRL `.xml`**; newer are **iXBRL `.html`/`.xhtml`**. The parse path may need a branch for the older format. Record `account_taxonomy` per filing.
- **CIC nested zips** are currently skipped ([extractinfo_fromaccountfiles.R:53-55](../../wrangling/extractinfo_fromaccountfiles.R#L53-L55)) — still a TODO.
- **Dimensional-only metrics** *(confirmed in Phase 2).* Large/complex filings sometimes tag a figure (notably `ProfitLoss`, `Creditors`) *only* inside dimensional/segment contexts, with no non-dimensional total. The extractor takes non-dimensional **headline** facts and **skips** dimensional-only ones rather than risk wrongly summing segments — verified against Gripple's full group accounts. Balance-sheet stocks + employees capture reliably; revisit if P&L coverage on large filers becomes important.
- **Idempotency.** Key inserts on `filing_id` so re-running a month is safe (existing RDS-name check becomes the `processed_files` table). Duplicate same-day resubmissions dedupe on `filing_id`.

---

## Incremental processing design

Replace the "download *all* zips, then extract *all*" split with a **single per-month loop** (download → extract → load → delete), iterating **newest → oldest**. With 4 GB zips × 216 months, never hold more than one or two on disk.

```
for each month in archive, newest → oldest:
    if month in processed_files (status='done'): skip
    download zip  →  unzip  →  parse all accounts (parallel furrr, as today)
    build filings + observations rows
    INSERT … ON CONFLICT DO NOTHING   (idempotent on filing_id / PK)
    delete unzipped folder (+ optionally the zip)
    record month in processed_files
    stop when caught up to the desired depth (configurable)
```

Recent-first means you reach 2 years deep almost immediately (each filing carries this+last year), then every older month backfills further. New firms appear as encountered; existing firms accrue history. Re-runs resume where they left off. The loader takes an **explicit list of zip names** (not only a contiguous range), so the *same code* drives both the [smoke test](#test-run-smoke-test-before-the-full-backfill) and the full backfill.

### Firm births and closures

- **Birth** = `incorporation_date` (already in the live list → `companies`).
- **Closure / death** = no longer "Active". The live list contains only *current* companies (dissolved ones drop off), so detect deaths two ways:
  1. **Status transition** — `company_status` moving to `proposal-to-strike-off` / `dissolved` between snapshots (captured in `company_status_history`).
  2. **Disappearance** — present in a prior snapshot, absent now ⇒ `last_seen` stops advancing; flag as removed.

  → Refresh the live list on a schedule, **append a `company_status_history` row per snapshot**, and update `companies.last_seen`. Births/deaths are then derivable from incorporation dates and status/last-seen gaps. (If you later need exact dissolution dates, CH publishes them in other products.)

---

## Spatial handling

Keep geometry **out** of the database as a native type. Store `easting`/`northing` (and/or lat-long) as plain numeric columns on `companies`, and rebuild `sf` in R on demand:

```r
companies |>
  dplyr::filter(!is.na(easting)) |>
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)
```

Rationale: keeps the DB portable and the web migration trivial; avoids `sf`-in-DB friction. DuckDB's `spatial` extension and Postgres/PostGIS remain available if server-side spatial queries are needed later. The geocoding stage is unchanged in logic — it just writes coords into `companies` instead of (or alongside) an `sf` RDS.

---

## Revised pipeline

| Stage | Script (new/changed) | Action |
|---|---|---|
| 0 | `db/schema.sql`, `db/connect.R` | Create the `.duckdb` file + tables (DDL above); shared connection helper |
| 1 | download live list *(existing)* | unchanged download |
| 1b | `wrangling/load_livelist_to_db.R` *(new)* | Upsert `companies`; append `company_status_history` snapshot |
| 2 | geocode live list *(existing logic)* | Write coords/LA/ITL2 into `companies` |
| 3 | `wrangling/build_accounts_timeseries.R` *(new; merges download+extract)* | Per-month loop, newest→oldest, into `filings`+`observations`, with `processed_files` control |
| 4 | `wrangling/refresh_clean_views.R` *(new)* | (Re)build `firm_year_metrics` / `firm_year` |
| 5 | `wrangling/export_parquet.R` *(optional)* | Export clean tables to Parquet for ML side / web |

Functions stay modular: the upgraded `get_accounts_data` is reused; only orchestration and the storage target change.

---

## Test run (smoke test before the full backfill)

Before committing to the full multi-day extraction, validate the **whole chain** — parse → load → link → reconcile → clean view — on a **handful of hand-picked zips**.

**Pick the zips to *force* cross-filing linkage.** A firm files roughly once a year, and a given filing lands in one month, so two *adjacent* months rarely share a firm. To catch the *same firm across multiple filings* (the thing we actually want to test), choose zips **spread across years** — ideally the **same calendar month in several different years** (e.g. `June2025`, `June2024`, `June2023`, `June2021`), since a firm with a fixed year-end tends to file around the same time annually. Add one or two extra recent months for volume. Purely random previous-year zips work too and are more representative; same-month-across-years just maximises the overlap you're trying to stress-test.

**What this exercises that a single zip can't:** one filing already gives the two-period (this-year/last-year) dating, but only *multiple filings for one firm* exercise cross-filing **linkage** and **restatement reconciliation** (the prior-year figure in filing N vs the current-year figure in filing N+1 for the same period).

**Acceptance checklist — all should pass before the full run:**

- [ ] **Regression** — employee values for a known month match today's positional extraction (no accidental change in meaning).
- [ ] **Correct dating** — each value is dated by its `contextRef` period, not document order.
- [ ] **Linkage actually tested** — count of firms with ≥2 filings is `> 0` (don't just hope chance delivered it); inspect a firm with 3+ distinct period-end years and confirm its series is contiguous and sensible.
- [ ] **Restatement / overlap** — a shared year appears **once** in `firm_year_metrics` (newest filing wins) but **multiple times** in raw `observations`; spot-check a restated value.
- [ ] **Idempotency** — re-running a test zip inserts **0** new rows.
- [ ] **Lifecycle** — `incorporation_date` populated; a known dissolved / strike-off firm is flagged.
- [ ] **Sparsity sanity** — `employees` / balance-sheet metrics well populated; `turnover` / `profit` sparse but present for some firms — i.e. nothing silently dropped.
- [ ] **Timing benchmark** — record parse+load seconds and rows per zip; extrapolate (≈ #zips × per-zip time + download time) to **ground the multi-day estimate** before committing to it.

Only once this passes do we kick off the contiguous newest→oldest backfill.

---

## Rollout phases

1. **Foundations** — add `duckdb` + `duckplyr`; write `db/schema.sql`; create the database; connection helper.
2. **Parser upgrade** — context-aware period dating + financial tags; test against [test_accounts/](../../test_accounts/) samples; confirm employee values match today's output on a known month.
3. **Incremental loader** — per-month loop + `processed_files`, **taking an explicit list of target zips**; validate counts and idempotency (re-run a zip → no change).
4. **Companies dimension & lifecycle** — upsert from live list, status history, births/closures flags.
5. **Clean layer** — `firm_year_metrics` + wide `firm_year`; sanity-check restatement handling.
6. **Smoke test (gate)** — run stages 3–5 on the hand-picked test zips and pass the [Test-run checklist](#test-run-smoke-test-before-the-full-backfill). **Do not start the full backfill until this passes.**
7. **Full backfill** — the multi-day run: contiguous newest→oldest to the target depth, resumable via `processed_files`.
8. **(Later) Web** — DuckDB-WASM static explorer, Shiny dashboard, or Plumber API over the same tables.

---

## Sizing estimate

Rough order of magnitude: ~2–3M filings/year recently; up to ~9 metrics × up to 2 periods ⇒ ≤ ~18 observation rows/filing → ~30–50M obs/year. A few recent years ≈ 10⁸ rows; the full 2008–2025 archive ≈ low 10⁹ rows. DuckDB handles both on a laptop; the database file is plausibly single-digit to low-tens of GB. Firmly "big but not huge".

---

## Open questions / deferred

- **Depth target** for the first backfill (how many recent years before pausing)?
- **Live-list refresh cadence** (drives birth/death resolution) — monthly alongside the accounts run?
- **Financial metric shortlist** — confirm the starter set above is the right priority, or trim/extend.
- **Older-format parsing** — decide when to invest in the pre-iXBRL `.xml` branch vs. starting the series from the iXBRL era.
- **CIC nested zips** — in or out of scope for v1.

---

## Decisions taken

| Question | Decision |
|---|---|
| History depth / strategy | Recent-first, **upsert** as we go; flag firm **births** (incorporation date) and **closures** |
| Fields to extract | **Employees + key financials**, captured in one parse pass |
| Overlaps / restatements | **Store raw observations + derived clean view** |
| Web interface | **Keep the door open** — local now, no re-migration later (DuckDB → Parquet/WASM → Postgres if needed) |

---

## Appendix: packages & sources

**R packages** — installed: `DBI`, `dbplyr`, `arrow`, `sf`. To add: `duckdb`, `duckplyr`.

**Sources**
- [Companies House — monthly accounts (current)](https://download.companieshouse.gov.uk/en_monthlyaccountsdata.html)
- [Companies House — historic monthly accounts archive (2008→present)](https://download.companieshouse.gov.uk/historicmonthlyaccountsdata.html)
- [duckplyr — DuckDB-backed dplyr](https://duckplyr.tidyverse.org/)
- [tidyverse blog — "duckplyr fully joins the tidyverse" (2025)](https://www.tidyverse.org/blog/2025/06/duckplyr-1-1-0/)
- [duckplyr ↔ dbplyr/DuckDB interoperability](https://duckplyr.tidyverse.org/articles/duckdb.html)
