# Test the Phase 2 iXBRL extractor (db/extract_account.R) against local samples.
# Run from project root:  Rscript testcode/test_extract_account.R
# Data used (gitignored): local/test_accounts/01772901_*.xhtml (4 yrs of Gripple)
#                         local/test_accounts/Accounts_Monthly_Data-August2025/*.html
library(tidyverse)
source("db/extract_account.R")

# Minimal reproduction of the OLD positional extractor, for regression only:
# it grabs employee values in document order and calls [1]=thisyear, [2]=lastyear.
old_employees <- function(f) {
  doc <- xml2::read_xml(f); ns <- xml2::xml_ns(doc)
  as.numeric(xml2::xml_text(xml2::xml_find_all(
    doc, "//ix:nonFraction[contains(@name,'AverageNumberEmployeesDuringPeriod')]", ns = ns)))
}

parse_gripple_meta <- function(f) {           # 01772901_aa_2024-09-12.xhtml
  p <- strsplit(basename(f), "_")[[1]]
  list(company_number = p[1], accountcode = sub("\\.xhtml$", "", p[3]))
}

gripple <- sort(list.files("local/test_accounts", pattern = "^01772901.*\\.xhtml$", full.names = TRUE))
stopifnot(length(gripple) > 0)
res <- map(gripple, ch_extract_account)

# ---- 1. Per-filing summary ---------------------------------------------------
cat("===== PER-FILING SUMMARY (Gripple 01772901) =====\n")
walk2(res, gripple, function(r, f) {
  meta <- parse_gripple_meta(f)
  emp  <- filter(r$observations, metric == "employees")
  cur  <- filter(emp, !is_prior_period); pri <- filter(emp, is_prior_period)
  cat(sprintf("\n%s  (submitted %s)\n", basename(f), meta$accountcode))
  cat(sprintf("  company: %s\n", r$filing$company_name))
  cat(sprintf("  period:  %s -> %s | dormant=%s | %d obs across %d metrics\n",
              r$filing$period_start, r$filing$period_end, r$filing$dormant,
              nrow(r$observations), n_distinct(r$observations$metric)))
  cat(sprintf("  employees: current(%s)=%s  prior(%s)=%s\n",
              cur$period_end[1], cur$value[1], pri$period_end[1], pri$value[1]))
  cat("  metrics: ", paste(sort(unique(r$observations$metric)), collapse = ", "), "\n")
})

# ---- 2. Regression: new (context-dated) vs old (positional) employees --------
cat("\n===== REGRESSION: new vs old positional employees =====\n")
reg <- map2_dfr(res, gripple, function(r, f) {
  old <- old_employees(f)
  emp <- filter(r$observations, metric == "employees")
  tibble(file = basename(f),
         new_current = filter(emp, !is_prior_period)$value[1], old_thisyear = old[1],
         new_prior   = filter(emp,  is_prior_period)$value[1], old_lastyear = old[2])
}) %>% mutate(match = new_current == old_thisyear & new_prior == old_lastyear)
print(reg)
cat(sprintf("=> employees match old extractor on all files: %s\n", all(reg$match)))

# ---- 3. Cross-filing linkage: overlap + newest-filing-wins -------------------
cat("\n===== CROSS-FILING EMPLOYEE SERIES (linkage / restatement test) =====\n")
emp_all <- map2_dfr(res, gripple, function(r, f) {
  filter(r$observations, metric == "employees") %>%
    transmute(submitted = parse_gripple_meta(f)$accountcode,
              year = lubridate::year(period_end), period_end, employees = value, is_prior_period)
})
cat("\nRAW observations (same year appears from multiple filings = overlap):\n")
print(arrange(emp_all, year, submitted))

clean <- emp_all %>% group_by(period_end) %>% arrange(desc(submitted)) %>% slice(1) %>% ungroup()
cat("\nCLEAN series (newest filing wins per year):\n")
print(transmute(arrange(clean, year), year, employees, from_filing = submitted))

overlaps <- emp_all %>% group_by(year) %>% filter(n_distinct(submitted) > 1) %>%
  summarise(n_filings = n_distinct(submitted), distinct_values = n_distinct(employees), .groups = "drop")
cat("\nRestatement check on overlapping years (distinct_values=1 => consistent):\n")
print(overlaps)

# ---- 4. Financial coverage / sparsity on typical small firms -----------------
aug_dir <- "local/test_accounts/Accounts_Monthly_Data-August2025"
if (dir.exists(aug_dir)) {
  cat("\n===== FINANCIAL COVERAGE on a sample of August 2025 accounts =====\n")
  aug <- list.files(aug_dir, pattern = "\\.html$", full.names = TRUE)
  set.seed(1); aug_s <- sample(aug, min(50, length(aug)))
  aug_res <- map(aug_s, possibly(ch_extract_account, NULL))
  ok <- !map_lgl(aug_res, is.null)
  cat(sprintf("parsed %d/%d files OK\n", sum(ok), length(aug_s)))
  aug_obs <- imap_dfr(aug_res, function(r, i) if (!is.null(r)) mutate(r$observations, file = aug_s[i]))
  cov <- aug_obs %>% group_by(metric) %>% summarise(n_files = n_distinct(file), .groups = "drop") %>%
    mutate(pct_of_parsed = round(100 * n_files / sum(ok), 1)) %>% arrange(desc(n_files))
  print(cov)
} else {
  cat("\n(skipping financial-coverage sample: ", aug_dir, " not present)\n")
}

cat("\n===== DONE =====\n")
