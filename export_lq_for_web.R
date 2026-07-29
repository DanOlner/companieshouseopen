# EXPORT LQ DATA FOR THE D3 QUADRANT PAGE
#
# Writes docs/plots/lq-quadrant/data/:
#   meta.json        place names, sector names per level, section -> 2 digit parents
#   lq_section.json  } columnar arrays, one file per SIC level, all 350 LAs
#   lq_sic2.json     } the page loads meta + section + sic2 at startup and fetches
#   lq_sic3.json     } the deeper ones only when someone drills that far
#   lq_sic4.json
#   lq_sic5.json
#
# Everything is measured against GB at every level, so the x axis means the same
# thing however deep you drill - which is what lets the parent's circle be drawn
# faintly behind its children.

source('lq_functions.R')
library(jsonlite)

outdir <- 'docs/plots/lq-quadrant/data'
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)


# BUILD -------------------------------------------------------------------

chfile <- 'local/PROCESSED_accountextracts_n_livelist_geocoded_combined_-2026-06-01.rds'
ch <- readRDS(chfile)

ch_long <- ch_employee_timepoints(ch, localauthority_name)

levels_wanted <- c('section', 2:5)

res <- lq_from_long(ch_long, digitlevels = levels_wanted)

names_bylevel <- map(levels_wanted, ch_sic_name_lookup, ch = ch) %>% bind_rows()

yeartoplot <- res$yeartoplot %>% left_join(names_bylevel, by = c('siclevel', 'sic'))


# CHECKS ------------------------------------------------------------------
# The drill-down assumes each level is an exact partition of the one below.
# If these don't hold, the page will silently show children that don't add up
# to their parent.
#
# Counts are carried as doubles, so summing hundreds of thousands of them leaves
# floating point dust - sic4 into sic3 comes out at ~1e-13. Anything real would be
# at least 1 whole employee, so the tolerance sits far below that and far above
# the noise.
tol <- 1e-6

sections <- sic_section_lookup()

# 1. Every 2 digit code in the data maps to exactly one section
codes_in_data <- ch_long %>%
  mutate(sic2 = str_sub(SIC_5DIGIT_CODE, 1, 2)) %>%
  distinct(sic2) %>% pull(sic2)

cat("2 digit codes in data:", length(codes_in_data),
    "| mapped to a section:", sum(codes_in_data %in% sections$sic2), "\n")
stopifnot(all(codes_in_data %in% sections$sic2))

# 2. Section employment == sum of its 2 digit children, per place.
# This is the one that catches the NULL SIC_SECTION_LETTER problem: using CH's own
# column here would leave 126,774 firms in the 2 digit totals but out of the
# section totals.
sec_totals <- yeartoplot %>%
  filter(siclevel == 'sicsection') %>%
  select(GEOGRAPHY_NAME, section = sic, section_jobs = JOBCOUNT)

kid_totals <- yeartoplot %>%
  filter(siclevel == 'sic2') %>%
  left_join(sections %>% select(sic2, section), by = c('sic' = 'sic2')) %>%
  group_by(GEOGRAPHY_NAME, section) %>%
  summarise(child_jobs = sum(JOBCOUNT), .groups = 'drop')

recon <- sec_totals %>%
  full_join(kid_totals, by = c('GEOGRAPHY_NAME', 'section')) %>%
  mutate(diff = abs(coalesce(section_jobs, 0) - coalesce(child_jobs, 0)))

cat("section vs summed 2 digit children - max abs diff:", max(recon$diff), "\n")
stopifnot(max(recon$diff) < tol)

# 3. Same for each digit level against the one above it
for(lv in 3:5){
  parent_lv <- paste0('sic', lv - 1)
  child_lv <- paste0('sic', lv)

  kids <- yeartoplot %>%
    filter(siclevel == child_lv) %>%
    mutate(parent = str_sub(sic, 1, lv - 1)) %>%
    group_by(GEOGRAPHY_NAME, parent) %>%
    summarise(child_jobs = sum(JOBCOUNT), .groups = 'drop')

  parents <- yeartoplot %>%
    filter(siclevel == parent_lv) %>%
    select(GEOGRAPHY_NAME, parent = sic, parent_jobs = JOBCOUNT)

  d <- parents %>% full_join(kids, by = c('GEOGRAPHY_NAME', 'parent')) %>%
    mutate(diff = abs(coalesce(parent_jobs, 0) - coalesce(child_jobs, 0)))

  cat(child_lv, "summed into", parent_lv, "- max abs diff:", max(d$diff), "\n")
  stopifnot(max(d$diff) < tol)
}


# DROP SECTORS NOT WANTED ON THE PAGE -------------------------------------
#
# Extraterritorial organisations and bodies: section U, 2 digit 99, and the codes
# beneath them. A couple of hundred employees throwing an LQ over 30, which just
# stretches the x axis for something nobody wants to look at.
#
# Filtered here, AFTER the LQs and the partition checks, so every other sector's
# numbers are untouched and still match what the R plots produce. Dropping it
# before the LQ step would shift every denominator instead.

drop_before <- nrow(yeartoplot)

yeartoplot <- yeartoplot %>%
  filter(
    !(siclevel == 'sicsection' & sic == 'U'),
    !(siclevel != 'sicsection' & str_detect(sic, '^99'))
  )

cat("dropped", drop_before - nrow(yeartoplot), "extraterritorial rows\n")


# WRITE -------------------------------------------------------------------

# Stable level keys for the web side
level_key <- c(sicsection = 'section', sic2 = 'sic2', sic3 = 'sic3',
               sic4 = 'sic4', sic5 = 'sic5')

yeartoplot <- yeartoplot %>% mutate(levelkey = level_key[siclevel])

# The SIC names carry the odd stray byte from upstream - 22230 has 0xC6 where an
# apostrophe belongs ("builders' ware of plastic"). fetch().json() assumes UTF-8
# and throws on anything else, so the whole page would fail to load over one byte.
clean_utf8 <- function(x){
  x <- iconv(x, from = 'latin1', to = 'UTF-8')   # always yields valid UTF-8
  x <- gsub('Æ', "'", x, fixed = TRUE)      # the mangled apostrophe
  enc2utf8(x)
}

yeartoplot <- yeartoplot %>%
  mutate(sic_name = clean_utf8(sic_name), GEOGRAPHY_NAME = clean_utf8(GEOGRAPHY_NAME))

places <- sort(unique(yeartoplot$GEOGRAPHY_NAME))

# One sector index per level, ordered by code so the JSON is stable between runs
sector_index <- yeartoplot %>%
  distinct(levelkey, sic, sic_name) %>%
  arrange(levelkey, sic) %>%
  group_by(levelkey) %>%
  mutate(idx = row_number() - 1) %>%
  ungroup()

meta <- list(
  generated = as.character(Sys.Date()),
  source = basename(chfile),
  note = paste("Companies House employee counts, this year vs last year in each",
               "firm's accounts. LQ measured against GB at every level."),
  places = places,
  levels = unname(level_key),
  # as.list, or jsonlite drops the names and writes a bare array.
  # Section range read off the data rather than hardcoded, so it stays honest
  # when sections get filtered out above.
  levelLabels = as.list(c(
    section = local({
      s <- sort(sector_index$sic[sector_index$levelkey == 'section'])
      paste0('Section (', s[1], '-', s[length(s)], ')')
    }),
    sic2 = '2 digit', sic3 = '3 digit', sic4 = '4 digit', sic5 = '5 digit')),
  sectors = map(unname(level_key), function(lv){
    s <- sector_index %>% filter(levelkey == lv)
    list(codes = s$sic, names = s$sic_name)
  }) %>% set_names(unname(level_key)),
  # Only section -> 2 digit needs stating; 3/4/5 digit parents are code prefixes
  sic2ToSection = sections %>%
    filter(sic2 %in% (sector_index %>% filter(levelkey == 'sic2') %>% pull(sic))) %>%
    { set_names(as.list(.$section), .$sic2) }
)

write_json(meta, file.path(outdir, 'meta.json'), auto_unbox = TRUE, digits = NA)
cat("wrote meta.json\n")

place_idx <- set_names(seq_along(places) - 1, places)

for(lv in unname(level_key)){

  sidx <- sector_index %>% filter(levelkey == lv)
  sic_idx <- set_names(sidx$idx, sidx$sic)

  d <- yeartoplot %>%
    filter(levelkey == lv) %>%
    arrange(GEOGRAPHY_NAME, sic) %>%
    transmute(
      p = unname(place_idx[GEOGRAPHY_NAME]),
      s = unname(sic_idx[sic]),
      lq = round(LQ, 4),
      # pct change in LQ, and in the place's own employee count
      dlq = round(pct_change_LQ, 2),
      djobs = round(pct_change_jobs, 2),
      jobs = as.integer(JOBCOUNT),
      share = round(sector_regional_proportion, 6)
    )

  # NA can't round-trip as a number; the page treats null as "no value"
  payload <- list(
    level = lv, n = nrow(d),
    p = d$p, s = d$s, lq = d$lq, dlq = d$dlq,
    djobs = d$djobs, jobs = d$jobs, share = d$share
  )

  f <- file.path(outdir, paste0('lq_', lv, '.json'))
  write_json(payload, f, auto_unbox = TRUE, na = 'null', digits = NA)
  cat("wrote", basename(f), "-", nrow(d), "rows,",
      round(file.size(f) / 1024^2, 2), "MB\n")

}

cat("\ntotal data dir:", round(sum(file.size(list.files(outdir, full.names = TRUE))) / 1024^2, 2), "MB\n")


# Every file must be valid UTF-8 or the browser refuses it outright
for(f in list.files(outdir, pattern = '\\.json$', full.names = TRUE)){
  raw <- readBin(f, 'raw', file.size(f))
  ok <- !is.na(iconv(rawToChar(raw), from = 'UTF-8', to = 'UTF-8'))
  cat(if(ok) 'valid UTF-8:' else 'NOT UTF-8:', basename(f), '\n')
  stopifnot(ok)
}
