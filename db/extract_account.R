# iXBRL account extractor for the time-series DB (Phase 2)
# ------------------------------------------------------------------------------
# Reworks the old positional get_accounts_data() (functions.R) for time series:
#   * dates each value by its contextRef period (NOT document order [1]/[2])
#   * applies XBRL scale (x10^scale) and sign (negate if sign="-")
#   * matches tags by LOCAL name only — the namespace prefix (ns5:/core:/...)
#     is assigned per-file by the filing software and must NOT be relied on
#   * captures employees + key financials as a LONG table (one row per
#     metric per period), feeding the `observations` table in db/schema.sql
#
# Returns CONTENT-derived fields only. The loader (phase 3) supplies
# company_number, accountcode (submission date), filing_id, source zip etc.
# from the filename/zip — this keeps the extractor pure and testable.
#
# v1 limitation: only NON-DIMENSIONAL (headline) facts are taken. Where a
# metric is reported solely in dimensional/segment contexts (e.g. ProfitLoss
# and Creditors in large group accounts), it is skipped rather than guessed.
library(xml2)
library(dplyr)
library(tibble)

# --- metric configuration -----------------------------------------------------
# Extensible: add a row to capture a new tag. Several local-names can map to one
# metric (taxonomy-version synonyms). type drives how the period is read:
#   'flow'  = duration fact (P&L, employees): period_start..period_end
#   'stock' = instant fact (balance sheet):   period_end only
ch_metric_tags <- tibble::tribble(
  ~metric,             ~localname,                                 ~type,
  "employees",         "AverageNumberEmployeesDuringPeriod",       "flow",
  "turnover",          "Turnover",                                 "flow",
  "turnover",          "TurnoverRevenue",                          "flow",
  "turnover",          "TurnoverGrossOperatingRevenue",            "flow",
  "gross_profit",      "GrossProfitLoss",                          "flow",
  "operating_profit",  "OperatingProfitLoss",                      "flow",
  "profit_loss",       "ProfitLoss",                               "flow",
  "profit_before_tax", "ProfitLossOnOrdinaryActivitiesBeforeTax",  "flow",
  "fixed_assets",      "FixedAssets",                              "stock",
  "current_assets",    "CurrentAssets",                            "stock",
  "cash",              "CashBankOnHand",                           "stock",
  "debtors",           "Debtors",                                  "stock",
  "creditors",         "Creditors",                                "stock",
  "net_assets",        "NetAssetsLiabilities",                     "stock",
  "equity",            "Equity",                                   "stock",
  "equity",            "ShareholderFunds",                         "stock"
)

# Strip the namespace prefix from a QName attribute value (ns5:Foo -> Foo).
ch_local_name <- function(x) sub("^[^:]*:", "", x)

# Build a lookup of contextRef id -> period + whether it carries a dimension.
ch_context_table <- function(doc) {
  ctx <- xml_find_all(doc, "//*[local-name()='context']")
  if (length(ctx) == 0) {
    return(tibble(ctx_id = character(), period_start = as.Date(character()),
                  period_end = as.Date(character()), has_member = logical()))
  }
  first_txt <- function(node, ln)
    xml_text(xml_find_first(node, paste0(".//*[local-name()='", ln, "']")))
  tibble(
    ctx_id      = xml_attr(ctx, "id"),
    start_raw   = vapply(ctx, first_txt, character(1), "startDate"),
    end_raw     = vapply(ctx, first_txt, character(1), "endDate"),
    instant_raw = vapply(ctx, first_txt, character(1), "instant"),
    has_member  = vapply(ctx, function(n)
      length(xml_find_all(n, ".//*[local-name()='explicitMember' or local-name()='typedMember']")) > 0,
      logical(1))
  ) %>%
    mutate(
      period_start = as.Date(dplyr::na_if(start_raw, "")),
      period_end   = as.Date(dplyr::coalesce(dplyr::na_if(end_raw, ""),
                                             dplyr::na_if(instant_raw, "")))
    ) %>%
    select(ctx_id, period_start, period_end, has_member)
}

# Extract one account file.
# Returns list(filing = <1-row tibble>, observations = <long tibble, 0+ rows>).
ch_extract_account <- function(filepath) {
  doc <- xml2::read_xml(filepath)

  ctx_tbl <- ch_context_table(doc)

  # Filing reference period_end = latest accounting-period end. Use durations
  # (the accounting year) so a signing/approval-date instant can't hijack it;
  # fall back to the latest instant if a filing somehow has no duration context.
  durations <- ctx_tbl %>% filter(!is.na(period_start), !is.na(period_end))
  if (nrow(durations) > 0) {
    filing_period_end   <- max(durations$period_end)
    filing_period_start <- durations$period_start[which.max(durations$period_end)]
  } else {
    filing_period_end   <- suppressWarnings(max(ctx_tbl$period_end, na.rm = TRUE))
    filing_period_start <- as.Date(NA)
  }
  if (!is.finite(as.numeric(filing_period_end))) filing_period_end <- as.Date(NA)

  # --- numeric facts -> long observations -------------------------------------
  nf <- xml_find_all(doc, "//*[local-name()='nonFraction']")
  observations <- tibble(
    metric = character(), period_start = as.Date(character()),
    period_end = as.Date(character()), is_prior_period = logical(),
    value = double(), unit = character()
  )
  if (length(nf) > 0) {
    facts <- tibble(
      localname = ch_local_name(xml_attr(nf, "name")),
      ctx_id    = xml_attr(nf, "contextRef"),
      text      = xml_text(nf),
      scale     = xml_attr(nf, "scale"),
      sign      = xml_attr(nf, "sign")
    )
    obs <- facts %>%
      inner_join(ch_metric_tags, by = "localname") %>%   # keep only target metrics
      left_join(ctx_tbl, by = "ctx_id") %>%
      filter(!has_member %in% TRUE) %>%                  # headline (non-dimensional) only
      mutate(
        value_raw = suppressWarnings(as.numeric(gsub(",", "", text))),
        value = value_raw *
          10^(ifelse(is.na(scale), 0, suppressWarnings(as.numeric(scale)))) *
          ifelse(!is.na(sign) & sign == "-", -1, 1)
      ) %>%
      filter(!is.na(value), !is.na(period_end)) %>%
      # one headline value per metric per period (non-dim should already be unique)
      group_by(metric, period_end) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        # stocks have no meaningful start; keep duration start only for flows
        period_start    = if_else(type == "flow", period_start, as.Date(NA)),
        is_prior_period = period_end < filing_period_end,
        unit            = if_else(metric == "employees", "count", "GBP")
      ) %>%
      select(metric, period_start, period_end, is_prior_period, value, unit) %>%
      arrange(metric, period_end)
    if (nrow(obs) > 0) observations <- obs
  }

  # --- filing-level metadata (text facts) -------------------------------------
  nn <- xml_find_all(doc, "//*[local-name()='nonNumeric']")
  nn_name <- ch_local_name(xml_attr(nn, "name"))
  get_nn <- function(frag) {
    v <- xml_text(nn[grepl(frag, nn_name, fixed = TRUE)])
    if (length(v) > 0) v[[1]] else NA_character_
  }

  filing <- tibble(
    company_name = get_nn("EntityCurrentLegalOrRegisteredName"),
    period_start = filing_period_start,
    period_end   = filing_period_end,
    dormant      = get_nn("EntityDormantTruefalse")
  )

  list(filing = filing, observations = observations)
}
