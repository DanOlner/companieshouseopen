# Companies House accounts database — connection + init helpers (DuckDB)
# Phase 1 of docs/guides/timeseries_accounts_db_plan.md
#
# Source this file from the project root, then:
#   con <- ch_connect()
#   ch_init_schema(con)          # create base tables (idempotent)
#   tbl(con, "companies")        # dbplyr lazy table (write dplyr, runs in DuckDB)
#   ch_disconnect(con)
library(DBI)
library(duckdb)

# Default on-disk database location (under the gitignored local/ folder)
CH_DB_PATH <- "local/companieshouse.duckdb"

# Open a connection to the DuckDB database file (created if it doesn't exist).
# read_only = TRUE is handy for analysis / a future web layer querying alongside loads.
ch_connect <- function(path = CH_DB_PATH, read_only = FALSE) {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)
}

# Close a connection and shut the database down cleanly.
ch_disconnect <- function(con) {
  dbDisconnect(con, shutdown = TRUE)
}

# Execute a multi-statement .sql file one statement at a time.
# Strips -- line comments, splits on ';', skips blanks, runs inside a transaction.
ch_run_sql_file <- function(con, path) {
  sql <- paste(readLines(path, warn = FALSE), collapse = "\n")
  sql <- gsub("--[^\n]*", "", sql)                       # strip line comments
  statements <- trimws(strsplit(sql, ";", fixed = TRUE)[[1]])
  statements <- statements[nzchar(statements)]
  dbExecute(con, "BEGIN TRANSACTION")
  tryCatch(
    {
      for (s in statements) dbExecute(con, s)
      dbExecute(con, "COMMIT")
    },
    error = function(e) {
      dbExecute(con, "ROLLBACK")
      stop(e)
    }
  )
  invisible(length(statements))
}

# Create the base tables from db/schema.sql (idempotent).
ch_init_schema <- function(con, schema_path = "db/schema.sql") {
  n <- ch_run_sql_file(con, schema_path)
  cat("Ran", n, "schema statement(s) from", schema_path, "\n")
  invisible(con)
}

# Create/refresh the clean-layer views from db/views.sql (idempotent).
ch_init_views <- function(con, views_path = "db/views.sql") {
  n <- ch_run_sql_file(con, views_path)
  cat("Ran", n, "view statement(s) from", views_path, "\n")
  invisible(con)
}

# Snapshot the clean-layer views into physical tables (faster for big-scale
# analysis / a web layer). Re-run after a load to refresh.
ch_materialise_clean <- function(con) {
  DBI::dbExecute(con, "CREATE OR REPLACE TABLE firm_year_metrics_mat AS SELECT * FROM firm_year_metrics")
  DBI::dbExecute(con, "CREATE OR REPLACE TABLE firm_year_mat        AS SELECT * FROM firm_year")
  cat("Materialised firm_year_metrics_mat + firm_year_mat.\n")
  invisible(con)
}
