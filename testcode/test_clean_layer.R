# Test the Phase 5 clean layer (db/views.sql): newest-filing-wins reconciliation
# of overlapping/restated years, plus the wide firm_year pivot. Throwaway DB.
# Run from project root:  Rscript testcode/test_clean_layer.R
suppressPackageStartupMessages(library(tidyverse))
source("db/connect.R")

dbfile <- "local/_clean_test.duckdb"; unlink(dbfile, force = TRUE)
con <- ch_connect(dbfile); ch_init_schema(con); ch_init_views(con)

# small insert helper (only the named columns; rest default NULL)
ins <- function(table, df, cols) {
  duckdb::duckdb_register(con, "v_tmp", df[, cols, drop = FALSE])
  on.exit(duckdb::duckdb_unregister(con, "v_tmp"), add = TRUE)
  dbExecute(con, sprintf("INSERT INTO %s (%s) SELECT %s FROM v_tmp",
                         table, paste(cols, collapse = ","), paste(cols, collapse = ",")))
}

# company X
ins("companies",
    tibble(company_number = "X", company_name = "XCO LTD",
           incorporation_date = as.Date("2019-01-01"), company_status = "Active",
           first_seen = as.Date("2025-08-01"), last_seen = as.Date("2025-08-01")),
    c("company_number", "company_name", "incorporation_date", "company_status",
      "first_seen", "last_seen"))

# two filings: F1 (FY2023, submitted Aug-2024), F2 (FY2024, submitted Aug-2025)
ins("filings",
    tibble(filing_id = c("X_20231231_202408", "X_20241231_202508"),
           company_number = "X",
           submission_date = as.Date(c("2024-08-01", "2025-08-01")),
           period_end_date = as.Date(c("2023-12-31", "2024-12-31"))),
    c("filing_id", "company_number", "submission_date", "period_end_date"))

# observations: F1 reports 2023(cur)+2022(prior); F2 reports 2024(cur)+2023(prior, RESTATED)
obs <- tribble(
  ~filing_id,            ~metric,     ~pe,          ~prior, ~value,
  "X_20231231_202408",   "employees", "2023-12-31", FALSE,  100,
  "X_20231231_202408",   "employees", "2022-12-31", TRUE,    90,
  "X_20231231_202408",   "turnover",  "2023-12-31", FALSE,  500000,
  "X_20231231_202408",   "turnover",  "2022-12-31", TRUE,   450000,
  "X_20241231_202508",   "employees", "2024-12-31", FALSE,  110,
  "X_20241231_202508",   "employees", "2023-12-31", TRUE,   105,    # <- restates 2023
  "X_20241231_202508",   "turnover",  "2024-12-31", FALSE,  550000,
  "X_20241231_202508",   "turnover",  "2023-12-31", TRUE,   505000  # <- restates 2023
) %>% transmute(filing_id, company_number = "X", metric,
                period_end_date = as.Date(pe), is_prior_period = prior, value,
                unit = if_else(metric == "employees", "count", "GBP"))
ins("observations", obs,
    c("filing_id", "company_number", "metric", "period_end_date", "is_prior_period", "value", "unit"))

cat("== RAW observations for employees (2023 appears TWICE: 100 from F1, 105 from F2) ==\n")
print(dbGetQuery(con, "SELECT filing_id, period_end_date, value FROM observations
                       WHERE metric='employees' ORDER BY period_end_date, filing_id"))

cat("\n== firm_year_metrics: reconciled employees (2023 should be 105, sourced from F2) ==\n")
emp <- dbGetQuery(con, "SELECT period_end_date, value, source_filing FROM firm_year_metrics
                        WHERE metric='employees' ORDER BY period_end_date")
print(emp)

cat("\n== firm_year (wide), joined to companies ==\n")
print(dbGetQuery(con, "SELECT c.company_name, fy.year, fy.employees, fy.turnover
                       FROM firm_year fy JOIN companies c USING (company_number)
                       ORDER BY fy.year"))

# materialise + confirm identical
ch_materialise_clean(con)
same <- dbGetQuery(con, "SELECT count(*) n FROM (
   SELECT * FROM firm_year EXCEPT SELECT * FROM firm_year_mat)")$n

# --- verdict ------------------------------------------------------------------
v2023 <- emp$value[emp$period_end_date == as.Date("2023-12-31")]
src23 <- emp$source_filing[emp$period_end_date == as.Date("2023-12-31")]
to2023 <- dbGetQuery(con, "SELECT turnover FROM firm_year WHERE year=2023")$turnover
checks <- c(
  "3 reconciled employee-years"   = nrow(emp) == 3,
  "2023 employees restated to 105"= identical(v2023, 105),
  "2023 sourced from F2 (newest)" = identical(src23, "X_20241231_202508"),
  "2022 employees = 90 (F1)"      = identical(emp$value[emp$period_end_date == as.Date("2022-12-31")], 90),
  "2024 employees = 110 (F2)"     = identical(emp$value[emp$period_end_date == as.Date("2024-12-31")], 110),
  "wide turnover 2023 = 505000"   = identical(to2023, 505000),
  "materialised == view"          = same == 0
)
cat("\n===== VERDICT =====\n")
for (nm in names(checks)) cat(sprintf("  [%s] %s\n", ifelse(checks[[nm]], "PASS", "FAIL"), nm))
cat(sprintf("\nALL PASS: %s\n", all(unlist(checks))))

ch_disconnect(con); unlink(dbfile, force = TRUE)
cat("cleaned up throwaway DB.\n")
