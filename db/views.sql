-- Clean / derived layer (Phase 5) — views over the raw lossless tables.
-- Idempotent: CREATE OR REPLACE.

-- firm_year_metrics: one authoritative value per (company, metric, value-period).
-- "Newest filing wins" on overlap/restatement: among all filings that report a
-- value for a given period, keep the one from the latest filing, ordered by
-- submission_date, then the filing's own period_end, then filing_id (tiebreak).
-- Keeps provenance (which filing the surviving value came from).
CREATE OR REPLACE VIEW firm_year_metrics AS
SELECT o.company_number,
       o.metric,
       o.period_end_date,
       o.value,
       o.unit,
       f.filing_id       AS source_filing,
       f.submission_date AS source_submission
FROM observations o
JOIN filings f USING (filing_id)
QUALIFY row_number() OVER (
          PARTITION BY o.company_number, o.metric, o.period_end_date
          ORDER BY f.submission_date DESC, f.period_end_date DESC, f.filing_id DESC
        ) = 1;

-- firm_year: wide, one row per (company, accounting period); metrics as columns.
-- Reads from the reconciled firm_year_metrics, so each cell is the newest value.
CREATE OR REPLACE VIEW firm_year AS
SELECT company_number,
       year(period_end_date) AS year,
       period_end_date,
       max(value) FILTER (WHERE metric = 'employees')         AS employees,
       max(value) FILTER (WHERE metric = 'turnover')          AS turnover,
       max(value) FILTER (WHERE metric = 'gross_profit')      AS gross_profit,
       max(value) FILTER (WHERE metric = 'operating_profit')  AS operating_profit,
       max(value) FILTER (WHERE metric = 'profit_loss')       AS profit_loss,
       max(value) FILTER (WHERE metric = 'profit_before_tax') AS profit_before_tax,
       max(value) FILTER (WHERE metric = 'fixed_assets')      AS fixed_assets,
       max(value) FILTER (WHERE metric = 'current_assets')    AS current_assets,
       max(value) FILTER (WHERE metric = 'cash')              AS cash,
       max(value) FILTER (WHERE metric = 'debtors')           AS debtors,
       max(value) FILTER (WHERE metric = 'creditors')         AS creditors,
       max(value) FILTER (WHERE metric = 'net_assets')        AS net_assets,
       max(value) FILTER (WHERE metric = 'equity')            AS equity
FROM firm_year_metrics
GROUP BY company_number, period_end_date;
