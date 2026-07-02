# Test the Phase 4 live-list loader + lifecycle logic (load_livelist_to_db.R).
# Synthetic two-snapshot scenario in a throwaway DB; deterministic, no big files.
# Run from project root:  Rscript testcode/test_livelist.R
suppressPackageStartupMessages(library(tidyverse))
source("wrangling/load_livelist_to_db.R")   # also sources db/connect.R

# live-list-shaped synthetic rows (raw IncorporationDate = DD/MM/YYYY)
mk <- function(number, name, status, inc, east, north) tibble(
  CompanyNumber = number, CompanyName = name, CompanyCategory = "Private Limited Company",
  CompanyStatus = status, IncorporationDate = inc,
  SICCode.SicText_1 = "62012 - Business and domestic software development",
  postcode = paste0("S", seq_along(number)), localauthority_code = "E08000019",
  localauthority_name = "Sheffield", ITL221CD = "TLD3", ITL221NM = "South Yorkshire",
  easting = east, northing = north)

# Snapshot 1 (2025-01-01): A, B, C, E all Active
snap1 <- bind_rows(
  mk("00000001", "ALPHA LTD",   "Active", "01/06/2010", 430000, 390000),
  mk("00000002", "BRAVO LTD",   "Active", "02/02/2012", 431000, 391000),
  mk("00000003", "CHARLIE LTD", "Active", "03/03/2013", 432000, 392000),
  mk("00000005", "ECHO LTD",    "Active", "05/05/2015", 433000, 393000))

# Snapshot 2 (2025-06-01): A (renamed), B (now striking off), D (newborn). C & E gone.
snap2 <- bind_rows(
  mk("00000001", "ALPHA HOLDINGS LTD", "Active",                          "01/06/2010", 430000, 390000),
  mk("00000002", "BRAVO LTD",          "Active - Proposal to Strike off", "02/02/2012", 431000, 391000),
  mk("00000004", "DELTA LTD",          "Active",                          "15/03/2025", 434000, 394000))

dbfile <- "local/_livelist_test.duckdb"; unlink(dbfile, force = TRUE)
con <- ch_connect(dbfile); ch_init_schema(con)

cat("===== LOAD SNAPSHOT 1 (2025-01-01) =====\n")
ch_load_livelist(con, snap1, "2025-01-01")

cat("\n===== LOAD SNAPSHOT 2 (2025-06-01) =====\n")
ch_load_livelist(con, snap2, "2025-06-01")

cat("\n== companies (note A renamed; first_seen fixed, last_seen advances; C/E stale) ==\n")
print(dbGetQuery(con, "SELECT company_number, company_name, company_status, incorporation_date,
                       first_seen, last_seen FROM companies ORDER BY company_number"))

cat("\n== company_status_history ==\n")
print(dbGetQuery(con, "SELECT company_number, snapshot_date, company_status
                       FROM company_status_history ORDER BY snapshot_date, company_number"))

cat("\n== lifecycle: presumed closed (dropped off live list) ==\n"); print(ch_presumed_closed(con))
cat("\n== lifecycle: closing-status signals ==\n");                  print(ch_closing_signals(con))
cat("\n== lifecycle: births in 2025 ==\n");                          print(ch_company_births(con, "2025-01-01", "2025-12-31"))

# --- idempotency: reload snapshot 2 -> no new companies / status rows ---------
cat("\n===== RELOAD SNAPSHOT 2 (idempotency) =====\n")
n_comp_before <- dbGetQuery(con, "SELECT count(*) n FROM companies")$n
n_hist_before <- dbGetQuery(con, "SELECT count(*) n FROM company_status_history")$n
ch_load_livelist(con, snap2, "2025-06-01")
n_comp_after <- dbGetQuery(con, "SELECT count(*) n FROM companies")$n
n_hist_after <- dbGetQuery(con, "SELECT count(*) n FROM company_status_history")$n

# --- verdict ------------------------------------------------------------------
get1 <- function(sql) dbGetQuery(con, sql)[1, 1]
checks <- c(
  "5 companies total"                  = n_comp_after == 5,
  "A renamed via upsert"               = get1("SELECT company_name FROM companies WHERE company_number='00000001'") == "ALPHA HOLDINGS LTD",
  "A first_seen kept at 2025-01-01"    = as.character(get1("SELECT first_seen FROM companies WHERE company_number='00000001'")) == "2025-01-01",
  "A last_seen advanced to 2025-06-01" = as.character(get1("SELECT last_seen  FROM companies WHERE company_number='00000001'")) == "2025-06-01",
  "C still stale at 2025-01-01"        = as.character(get1("SELECT last_seen  FROM companies WHERE company_number='00000003'")) == "2025-01-01",
  "status_history has 7 rows"          = n_hist_before == 7,
  "presumed-closed = C and E"          = setequal(ch_presumed_closed(con)$company_number, c("00000003","00000005")),
  "closing-signal = B"                 = identical(ch_closing_signals(con)$company_number, "00000002"),
  "births 2025 = D"                    = identical(ch_company_births(con,"2025-01-01","2025-12-31")$company_number, "00000004"),
  "idempotent companies"               = n_comp_before == n_comp_after,
  "idempotent status history"          = n_hist_before == n_hist_after
)
cat("\n===== VERDICT =====\n")
for (nm in names(checks)) cat(sprintf("  [%s] %s\n", ifelse(checks[[nm]], "PASS", "FAIL"), nm))
cat(sprintf("\nALL PASS: %s\n", all(unlist(checks))))

ch_disconnect(con); unlink(dbfile, force = TRUE)
cat("cleaned up throwaway DB.\n")
