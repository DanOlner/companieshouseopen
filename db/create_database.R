# Create / initialise the Companies House accounts DuckDB database.
# Phase 1: creates local/companieshouse.duckdb and the base tables, then reports.
# Re-running is safe (schema uses CREATE TABLE IF NOT EXISTS).
# Run from the project root:  Rscript db/create_database.R
source("db/connect.R")

con <- ch_connect()

ch_init_schema(con)

cat("\nTables now in", CH_DB_PATH, ":\n")
print(dbListTables(con))

# Structural check: confirm the five expected base tables exist
expected <- c("companies", "company_status_history", "filings",
              "observations", "processed_files")
missing <- setdiff(expected, dbListTables(con))
if (length(missing) == 0) {
  cat("\nAll expected tables present. Database initialised OK.\n")
} else {
  cat("\nMISSING tables:", paste(missing, collapse = ", "), "\n")
}

ch_disconnect(con)
