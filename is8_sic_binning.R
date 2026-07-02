# ============================================================================
# BIN 5-DIGIT SICs INTO THE IS-8 INDUSTRIAL-STRATEGY GROWTH SECTORS
# ============================================================================
# Companies House records each firm's industry as a 5-digit SIC (SIC_5DIGIT_CODE).
# This assigns each 5-digit SIC to ONE of the 8 Industrial Strategy growth sectors
# ("IS-8") so firms can be counted / mapped by IS-8 category.
#
# Source of the IS-8 -> SIC definitions:
#   data/sectordefs/industrialstrategy2025sectordefs.csv
#   (copied from the RegionalEconomicTools project; column sic_code holds the SIC
#    codes the strategy lists for each sector, at MIXED digit lengths: 2, 3, 4 or 5).
#
# ---------------------------------------------------------------------------
# THE PARTITION RULE: most-specific definition wins, then a manual tie-break
# ---------------------------------------------------------------------------
# A single SIC can be claimed by more than one IS-8, in two ways:
#
#   (1) at DIFFERENT digit lengths. e.g. all of division 62 (computer programming)
#       is Digital & Technologies, but the strategy ALSO lists 6201 / 6202 / 62011
#       (games & bespoke software) under Creative Industries. A firm coded 62011 is
#       described more precisely by the 5-digit Creative entry than by the 2-digit
#       Digital one.
#       RULE: the MOST SPECIFIC (longest matching) definition wins. So 62011 -> Creative,
#       but a plain 62090 (no finer entry) -> Digital. This is the big advantage of
#       having full 5-digit codes here (the payments work was stuck at 2-digit and
#       could not make these splits). Specificity resolves the large majority of overlaps.
#
#   (2) at the SAME digit length - a genuine tie the data cannot break. After the
#       specificity rule, EXACTLY four divisions remain tied (all listed at 2-digit
#       under two sectors):
#         20  chemicals               Advanced manufacturing  vs  Foundational industries
#         58  publishing              Creative industries     vs  Digital & technologies
#         59  film / music            Creative industries     vs  Digital & technologies
#         60  broadcasting            Creative industries     vs  Digital & technologies
#       These are resolved by the is8_tiebreak table below - a judgement call, edit
#       it to change the mapping. (Current choices: chemicals -> Advanced manufacturing;
#       publishing/film/broadcasting -> Creative industries.)
#
# Clean Energy has no SIC codes in the strategy (all "frontier" subsectors with no
# SIC), so it never appears as a bin - a 5-digit SIC can never map to Clean Energy.
#
# If the definitions CSV is ever changed and a NEW same-length tie appears that is
# not in the tie-break table, bin_sic_to_is8() returns NA for it and warns, naming
# the code(s) to add.
# ============================================================================

# Assumes tidyverse is loaded (library(tidyverse)). qg() comes from functions.R;
# define a fallback so this file also works sourced on its own.
if (!exists("qg")) qg <- function(...) grepl(..., ignore.case = TRUE)


# Manual tie-break for genuine same-length collisions (see header).
# code = the (2-digit, here) SIC prefix that two IS-8s both claim; is8_keep = winner.
is8_tiebreak <- tibble::tribble(
  ~code, ~is8_keep,
  "20",  "Advanced manufacturing",   # chemicals: also Foundational industries
  "58",  "Creative industries",      # publishing: also Digital & technologies
  "59",  "Creative industries",      # film & music: also Digital & technologies
  "60",  "Creative industries"       # broadcasting: also Digital & technologies
)


# Long-form IS-8 definition table: one row per (IS-8, SIC code), code kept at its
# native digit length. Handy for auditing exactly which codes map where.
is8_sic_definitions <- function(defs_path = 'data/sectordefs/industrialstrategy2025sectordefs.csv') {
  read_csv(defs_path, show_col_types = FALSE) %>%
    filter(!is.na(sic_code)) %>%                     # drops Clean Energy + frontier-only rows
    transmute(
      is8 = case_when(
        qg('manufactur', sector_name) ~ "Advanced manufacturing",
        qg('foundation', sector_name) ~ "Foundational industries",
        qg('creative',   sector_name) ~ "Creative industries",
        qg('digital',    sector_name) ~ "Digital & technologies",
        qg('financ',     sector_name) ~ "Financial services",
        qg('life sci',   sector_name) ~ "Life sciences",
        qg('defence',    sector_name) ~ "Defence",
        qg('business',   sector_name) ~ "Professional & business services"
      ),
      code     = as.character(sic_code),
      code_len = nchar(code)
    ) %>%
    distinct(is8, code, code_len) %>%
    arrange(code_len, code, is8)
}


# Bin a vector of 5-digit SIC codes into IS-8 sectors.
#   sic5  : character or numeric vector of (up to) 5-digit SIC codes,
#           e.g. ch$SIC_5DIGIT_CODE
#   returns a character vector the same length / order as sic5, giving the IS-8
#           sector, or NA where the SIC is not part of any IS-8 sector.
bin_sic_to_is8 <- function(sic5,
                           defs_path = 'data/sectordefs/industrialstrategy2025sectordefs.csv',
                           tiebreak  = is8_tiebreak,
                           warn_unresolved_ties = TRUE) {

  defs <- is8_sic_definitions(defs_path)
  lens <- sort(unique(defs$code_len))               # digit lengths used by the defs (2:5)

  in_codes <- unique(as.character(sic5))
  in_codes <- in_codes[!is.na(in_codes)]

  # Match each input code to every definition that is a PREFIX of it (length L).
  matched <- map_dfr(lens, function(L) {
    tibble(input = in_codes) %>%
      filter(nchar(input) >= L) %>%
      mutate(code = substr(input, 1L, L), code_len = L) %>%
      # many-to-many is expected: a shared 2-digit code matches >1 IS-8 def row
      inner_join(defs, by = c("code", "code_len"), relationship = "many-to-many")
  })

  # Most-specific wins: keep only each input's longest matching definition. At that
  # length all surviving rows share the same `code`, so any remaining duplication is
  # a genuine same-length IS-8 tie (-> resolved by the tie-break table).
  resolved <- matched %>%
    group_by(input) %>%
    filter(code_len == max(code_len)) %>%
    summarise(
      code  = first(code),
      n_is8 = n_distinct(is8),
      is8_unique = first(is8),                       # only valid when n_is8 == 1
      .groups = "drop"
    ) %>%
    left_join(tiebreak, by = "code") %>%
    mutate(is8 = if_else(n_is8 == 1, is8_unique, is8_keep))

  # Flag any same-length tie not covered by the tie-break table (returns NA).
  unresolved <- resolved %>% filter(n_is8 > 1, is.na(is8))
  if (warn_unresolved_ties && nrow(unresolved) > 0) {
    warning("IS-8 same-length tie(s) with no tie-break entry: ",
            paste(sort(unique(unresolved$code)), collapse = ", "),
            " - add to is8_tiebreak. These return NA.")
  }

  # Map back to the original input order; NA = SIC not in any IS-8 sector.
  tibble(input = as.character(sic5)) %>%
    left_join(resolved %>% select(input, is8), by = "input") %>%
    pull(is8)
}


# ---------------------------------------------------------------------------
# USAGE (in testcode/initial_datadigging.R, after ch is built):
#
#   source('is8_sic_binning.R')
#   ch <- ch %>% mutate(IS8 = bin_sic_to_is8(SIC_5DIGIT_CODE))
#
#   # how many firms fall in each IS-8 sector (NA = not an IS-8 sector):
#   ch %>% st_set_geometry(NULL) %>% count(IS8, sort = TRUE)
#
#   # audit the mapping itself:
#   is8_sic_definitions() %>% print(n = Inf)
#
# NB only the first SIC (SIC_5DIGIT_CODE, from SICCode.SicText_1) is binned, matching
# the rest of that script. Firms can list up to 4 SICs; bin the others the same way
# if a multi-SIC view is wanted.
# ---------------------------------------------------------------------------
