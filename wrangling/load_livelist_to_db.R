# Load the (geocoded) Companies House live list into the DB (Phase 4)
# ------------------------------------------------------------------------------
# Upserts the live list into `companies` (a slowly-changing dimension) and
# appends one `company_status_history` row per company per refresh, so firm
# lifecycle is derivable:
#   * BIRTH   = incorporation_date (a column on `companies`)
#   * CLOSURE = either a closing status signal (strike-off / liquidation / ...),
#               or DISAPPEARANCE — present in an earlier snapshot but not the
#               latest (last_seen < latest snapshot), since dissolved firms drop
#               off the live list.
#
# Upsert keeps first_seen fixed and refreshes everything else (name, status,
# SIC, geo, last_seen) to the newest snapshot.
#
# Usage:
#   source("wrangling/load_livelist_to_db.R")
#   con <- ch_connect()
#   ch.geo <- readRDS("local/companieshouse_livelist_geocoded.rds")   # sf
#   ch_load_livelist(con, ch.geo, snapshot_date = "2025-03-01")
#   ch_presumed_closed(con); ch_closing_signals(con)                  # lifecycle
#   ch_disconnect(con)
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
})
source("db/connect.R")

# Map a live-list data frame / sf object to the `companies` schema.
# Accepts the geocoded-live-list column names (case-insensitive); coordinates
# come from sf geometry, or from easting/northing (or Eastings/Northings) cols.
ch_prepare_companies <- function(livelist, snapshot_date) {
  snapshot_date <- as.Date(snapshot_date)

  if (inherits(livelist, "sf")) {
    xy <- sf::st_coordinates(livelist)
    livelist <- sf::st_drop_geometry(livelist)
    livelist$easting  <- xy[, 1]
    livelist$northing <- xy[, 2]
  }
  df <- as_tibble(livelist)

  pick <- function(...) {                       # first matching column (case-insensitive) or NA
    for (nm in c(...)) {
      hit <- names(df)[tolower(names(df)) == tolower(nm)]
      if (length(hit) > 0) return(df[[hit[1]]])
    }
    rep(NA, nrow(df))
  }

  inc_fmt <- pick("incorporationdate_formatted")          # ISO, if present (combined data)
  inc_raw <- pick("IncorporationDate")                    # DD/MM/YYYY (raw live list)
  incorporation_date <- if (!all(is.na(inc_fmt))) {
    as.Date(substr(as.character(inc_fmt), 1, 10))
  } else {
    suppressWarnings(lubridate::dmy(as.character(inc_raw)))
  }

  tibble(
    company_number      = as.character(pick("CompanyNumber", "company_number")),
    company_name        = as.character(pick("CompanyName", "company_name")),
    incorporation_date  = incorporation_date,
    company_category    = as.character(pick("CompanyCategory", "company_category")),
    company_status      = as.character(pick("CompanyStatus", "company_status")),
    sic_1               = as.character(pick("SICCode.SicText_1", "sic_1")),
    sic_2               = as.character(pick("SICCode.SicText_2", "sic_2")),
    sic_3               = as.character(pick("SICCode.SicText_3", "sic_3")),
    sic_4               = as.character(pick("SICCode.SicText_4", "sic_4")),
    postcode            = as.character(pick("postcode", "Postcode_formatted")),
    localauthority_code = as.character(pick("localauthority_code")),
    localauthority_name = as.character(pick("localauthority_name")),
    itl221cd            = as.character(pick("ITL221CD", "itl221cd")),
    itl221nm            = as.character(pick("ITL221NM", "itl221nm")),
    easting             = as.numeric(pick("easting", "Eastings")),
    northing            = as.numeric(pick("northing", "Northings")),
    first_seen          = snapshot_date,
    last_seen           = snapshot_date
  ) %>%
    filter(!is.na(company_number), company_number != "") %>%
    distinct(company_number, .keep_all = TRUE)
}

# Upsert the live list + append a status snapshot. Returns counts.
ch_load_livelist <- function(con, livelist, snapshot_date) {
  comp <- ch_prepare_companies(livelist, snapshot_date)
  view <- "__ins_companies"
  duckdb::duckdb_register(con, view, comp)
  on.exit(duckdb::duckdb_unregister(con, view), add = TRUE)

  cols <- c("company_number", "company_name", "incorporation_date", "company_category",
            "company_status", "sic_1", "sic_2", "sic_3", "sic_4", "postcode",
            "localauthority_code", "localauthority_name", "itl221cd", "itl221nm",
            "easting", "northing", "first_seen", "last_seen")
  collist <- paste(cols, collapse = ", ")
  # refresh everything except the conflict key and first_seen
  setcols <- setdiff(cols, c("company_number", "first_seen"))
  setclause <- paste(sprintf("%s = excluded.%s", setcols, setcols), collapse = ", ")

  n_up <- DBI::dbExecute(con, sprintf(
    "INSERT INTO companies (%s) SELECT %s FROM %s
     ON CONFLICT (company_number) DO UPDATE SET %s", collist, collist, view, setclause))

  n_h <- DBI::dbExecute(con, sprintf(
    "INSERT INTO company_status_history (company_number, snapshot_date, company_status)
     SELECT company_number, last_seen, company_status FROM %s
     ON CONFLICT DO NOTHING", view))

  cat(sprintf("live-list %s: %d companies upserted, %d status-history rows added\n",
              as.character(as.Date(snapshot_date)), n_up, n_h))
  invisible(list(companies = n_up, status_rows = n_h))
}

# --- lifecycle helpers --------------------------------------------------------

# Firms that have dropped off the live list since the latest snapshot
# (last_seen older than the most recent refresh) => presumed dissolved/removed.
ch_presumed_closed <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT company_number, company_name, company_status, first_seen, last_seen
    FROM companies
    WHERE last_seen < (SELECT max(snapshot_date) FROM company_status_history)
    ORDER BY last_seen DESC")
}

# Firms whose CURRENT status signals an in-progress closure.
ch_closing_signals <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT company_number, company_name, company_status, last_seen
    FROM companies
    WHERE lower(company_status) LIKE '%strike%'
       OR lower(company_status) LIKE '%liquidat%'
       OR lower(company_status) LIKE '%administ%'
       OR lower(company_status) LIKE '%insolven%'
       OR lower(company_status) LIKE '%receiv%'
       OR lower(company_status) LIKE '%wound%'
       OR lower(company_status) LIKE '%dissolv%'")
}

# Firms incorporated within a date range (births).
ch_company_births <- function(con, from, to) {
  DBI::dbGetQuery(con,
    "SELECT company_number, company_name, incorporation_date
     FROM companies WHERE incorporation_date BETWEEN ? AND ?
     ORDER BY incorporation_date",
    params = list(as.character(as.Date(from)), as.character(as.Date(to))))
}
