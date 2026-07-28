# COMPANIES HOUSE -> LOCATION QUOTIENT PLOTS
#
# Uses the two employee counts in each CH account (this year / last year) as two
# timepoints, so each sector-and-place gets an LQ at both, and the change between
# them marks direction of travel on the plot:
#   green bubble = share of local employment growing, red = shrinking, size = how much
#   x axis = LQ on a log scale, blue line at LQ 1 = national average share
#   faint bubbles = every other place, solid = the place being highlighted
#   text on the right = employee count and % of that place's employment
#   error bars = span from last year's LQ to this year's
#
# Ported from RegionalEconomicTools (prepcode/industrial_strategy_datalinkage.R:312,
# originally worked out in bits_of_code/BradfordExplore.R:1149).

source('lq_functions.R')


# LOAD --------------------------------------------------------------------

# Latest processed CH file (firm-level, geocoded, live list joined to account extracts)
chfile <- 'local/PROCESSED_accountextracts_n_livelist_geocoded_combined_-2026-06-01.rds'

ch <- readRDS(chfile)

# 3,208,644 firms; 2,281,550 (71%) have an employee count in BOTH years,
# which is what the pipeline keeps
nrow(ch)
table(!is.na(ch$Employees_thisyear) & !is.na(ch$Employees_lastyear)) %>% prop.table()

# SIC codes are a fixed 5 characters, so some carry a trailing space. '7011 ' is a
# real SIC recorded only to 4 digits; 'None ' means no SIC at all and those firms
# get dropped by the pipeline (leaving them in would put them in the LQ denominators)
table(grepl('^[0-9]', ch$SIC_5DIGIT_CODE))


# RUN ---------------------------------------------------------------------

# PICK YOUR GEOGRAPHY. Available in this file:
#   localauthority_name / localauthority_code
#   ITL221NM / ITL221CD
geography <- 'localauthority_name'
# geography <- 'ITL221NM'

# PICK YOUR SIC DIGIT LEVELS. 2:5 gives all four; a single level is much quicker.
digitlevels <- 2:5

results <- ch_lq_pipeline(ch, !!geography, digitlevels = digitlevels)

# $lqs        - both timepoints, all sectors, all places
# $yeartoplot - latest timepoint only, with slope + all-time min/max joined on, plot-ready
# Both have a 'sic' column plus 'siclevel' saying which digit level the row came from
results$lqs
results$yeartoplot

# Add readable sector names for the y axis
names_bylevel <- map(digitlevels, ch_sic_name_lookup, ch = ch) %>% bind_rows()

results <- map(results, ~ .x %>% left_join(names_bylevel, by = c('siclevel','sic')))


# SANITY CHECKS -----------------------------------------------------------

# Sector proportions should sum to 1 within each timepoint and SIC level
results$lqs %>%
  distinct(siclevel, DATE, sic, sector_total_proportion) %>%
  group_by(siclevel, DATE) %>%
  summarise(total = sum(sector_total_proportion), .groups = 'drop')

# Slopes: with two timepoints this is log(LQ this year) - log(LQ last year),
# so exp(slope) - 1 is the % change in the place's share of that sector
results$yeartoplot %>%
  filter(siclevel == 'sic2') %>%
  summarise(across(slope, list(min = min, median = median, max = max), .names = "{.fn}"))


# SAVE --------------------------------------------------------------------

saveRDS(results$lqs, 'local/CH_lqs_bothtimepoints.rds')
saveRDS(results$yeartoplot, 'local/CH_yeartoplot.rds')


# PLOT --------------------------------------------------------------------

place <- 'Sheffield'

# y axis labels are cut to 60 characters by default - CH ships the full SIC names,
# some of which run past 130 characters and leave no room for the plot panel.
# Raise truncate_labels (or set NULL) if you'd rather have the whole name.

# 2 digit sectors, dropping ones where the place has fewer than 100 recorded employees
twodigit <- results$yeartoplot %>%
  filter(siclevel == 'sic2') %>%
  filter(!grepl('household own|membership', sic_name, ignore.case = TRUE))#swamp the axis, mean little here

p <- plot_lq_for_place(
  twodigit,
  place = place,
  sector_col = sic_name,
  min_jobcount = 100,
  title = paste0(place, ": CH employee LQs, 2 digit sectors\n(green = share growing, red = shrinking)")
)

p

# ggsave(plot = p, filename = paste0('local/images/CH_LQ_', gsub(' ', '', place), '_sic2.png'),
#        width = 10, height = 12)


# The original Bradford version split production from everything else, since
# production sectors have very different LQ ranges. Production is 2 digit 01-43.
twodigit <- twodigit %>%
  mutate(
    production = ifelse(as.numeric(sic) %in% c(1:43), 'production', 'other')
  )

p_production <- plot_lq_for_place(
  twodigit %>% filter(production == 'production'),
  place = place, sector_col = sic_name, min_jobcount = 100,
  title = paste0(place, ": production")
)

p_other <- plot_lq_for_place(
  twodigit %>% filter(production == 'other'),
  place = place, sector_col = sic_name, min_jobcount = 100,
  title = paste0(place, ": everything else")
)

p_production
p_other


# 5 digit is where this data earns its keep - BRES is disclosure-limited down there.
# Needs a higher job threshold or the plot is unreadable.
p5 <- plot_lq_for_place(
  results$yeartoplot %>% filter(siclevel == 'sic5'),
  place = place,
  sector_col = sic_name,
  min_jobcount = 250,
  title = paste0(place, ": CH employee LQs, 5 digit sectors")
)

p5
