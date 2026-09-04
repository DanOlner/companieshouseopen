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


# LQ EXPERIMENTS ==========================================================
#
# The plot above has two known weaknesses:
#   (a) every other place is a faint bubble cloud - you can see there IS a spread
#       but not who is who, so "Sheffield vs Leeds" is guesswork
#   (b) direction of change is encoded as bubble SIZE, which reads badly for a
#       signed quantity and competes with the LQ position for attention
#
# Four alternatives below, each attacking a different part of that.
# They all run off results$yeartoplot.
#
# The quadrant plot wants pct_change_LQ / pct_change_jobs, which get_lqs_and_slopes()
# gained after the first version of this file. If your results object (or a saved
# local/CH_yeartoplot.rds) was built before that, pct_change_LQ is derived from
# slope on the fly, but change = 'jobs' needs both timepoints and can't be
# recovered - re-run the RUN section above for that.
'pct_change_jobs' %in% names(results$yeartoplot)


# 0. SET UP THE COMPARISON PLACES -----------------------------------------

corecities <- uk_core_cities()

# Check the names actually match this data before relying on them.
# Belfast is the only miss - the LA lookup in the processed file is GB only.
setdiff(corecities, unique(results$yeartoplot$GEOGRAPHY_NAME))

southyorkshire <- c('Barnsley', 'Doncaster', 'Rotherham', 'Sheffield')
setdiff(southyorkshire, unique(results$yeartoplot$GEOGRAPHY_NAME))

twodigit <- results$yeartoplot %>%
  filter(siclevel == 'sic2') %>%
  filter(!grepl('household own|membership|extra', sic_name, ignore.case = TRUE))

fivedigit <- results$yeartoplot %>% filter(siclevel == 'sic5')

# WATCH OUT: tiny sectors produce wild LQs and will sit at the top of anything
# ordered by LQ. Sector 99 (extraterritorial organisations) and 92 (gambling)
# both do this for Sheffield - a few hundred employees giving an LQ over 10.
# min_jobcount is the blunt lever; filtering on the place's own share is sharper:
twodigit %>%
  filter(GEOGRAPHY_NAME == 'Sheffield') %>%
  slice_max(LQ, n = 8) %>%
  select(sic_name, JOBCOUNT, sector_regional_proportion, LQ)

# e.g. drop anything under a quarter of a percent of the place's employment
twodigit_solid <- twodigit %>%
  group_by(sic) %>%
  filter(any(GEOGRAPHY_NAME == 'Sheffield' & sector_regional_proportion > 0.0025)) %>%
  ungroup()


# 1. QUADRANT: concentration against direction ----------------------------
#
# Gets change off bubble size and onto its own axis. Reading:
#   top right    = already concentrated here AND still growing
#   bottom right = the specialism is real but eroding - usually the one to look at
#   top left     = not a specialism yet, but heading that way
#   bottom left  = under-represented and falling further behind

# The y axis is pseudo-log by default: one sector doubling its LQ would otherwise
# flatten every other sector onto the zero line. Note the uneven gridlines that
# comes with - y_scale = 'linear' if you'd rather have the honest raw spread.
q_lq <- plot_lq_quadrant(twodigit, place = 'Sheffield', min_jobcount = 100)
q_lq

plot_lq_quadrant(twodigit, place = 'Sheffield', min_jobcount = 100, y_scale = 'linear')

# The same plot against raw employment change rather than LQ change. Worth doing
# both: LQ change is relative to the national trend, so a sector can add people
# while its LQ falls (it grew, but slower than everywhere else). Where these two
# disagree is usually where the interesting story is.
q_jobs <- plot_lq_quadrant(twodigit, place = 'Sheffield', min_jobcount = 100,
                           change = 'jobs')
q_jobs

# 5 digit version - needs a higher threshold to stay readable
plot_lq_quadrant(fivedigit, place = 'Sheffield', min_jobcount = 250, label_n = 20)


# 2. DUMBBELL: named places instead of a bubble cloud ---------------------
#
# One row per sector, one coloured dot per place, grey bar showing the spread.
# Sectors ordered by Sheffield's LQ, so its specialisms sit at the top and you can
# read straight across to see who else has them. The ordering place gets a dark
# halo so the eye can follow it down the rows.

d_core <- plot_lq_dumbbell(twodigit, places = corecities, order_by = 'Sheffield',
                           min_jobcount = 100, top_n = 30)
d_core

# Fewer places is much easier to read. Sheffield against its obvious comparators:
d_peers <- plot_lq_dumbbell(twodigit,
                            places = c('Sheffield','Leeds','Manchester','Birmingham'),
                            order_by = 'Sheffield', min_jobcount = 100, top_n = 30)
d_peers


# 3. DISTRIBUTION: is this LQ actually unusual? ---------------------------
#
# An LQ of 1.5 means something different in a sector that's evenly spread across
# the country than in one concentrated in three places. The box shows the spread
# of every LA's LQ for that sector, so you can see whether a place is genuinely an
# outlier or just mid-pack. The main plot's faint bubble cloud was groping at this
# but couldn't be read off.
#
# WHY THE MEDIAN ISN'T AT LQ = 1
#
# The median marker sits either side of the blue line and that is not a bug.
# What the LQ pins at 1 is the EMPLOYMENT-WEIGHTED MEAN across LAs, not the
# median: sum(LQ * LA total employment) / GB total == 1 for every sector, exactly
# (holds to machine precision - see the nested checks further down for the same
# identity written out). The median carries no such guarantee.
#
# In practice 80 of the 88 two digit sectors have their median BELOW 1. That's
# not a finding about those sectors, it's the arithmetic: LQ is bounded at zero
# but unbounded above, so it's right-skewed, and a right-skewed distribution has
# its median below its mean. The mean is stuck at 1, so the median lands under it.
#
# The eight exceptions are sectors where LQ FALLS as the LA gets bigger (log size
# vs log LQ correlates about -0.41 for them, versus ~0 for everything else):
# extractive industries that only exist in a few small places (tobacco 5.29,
# metal ores 2.09, coal 1.71), plus the ubiquitous local trades every small town
# has proportionally more of than a big city - specialised construction 1.11,
# motor trade 1.08, machinery repair 1.05. Small LAs carry little employment
# weight, so they can sit well above 1 without moving the weighted mean.
#
# TWO THINGS THIS MEANS WHEN READING THE PLOT
#
# 1. The median is unweighted, so it treats Rutland the same as Birmingham. It's
#    the middle LOCAL AUTHORITY, not the middle worker. "Above the median" is a
#    weaker claim than it sounds.
# 2. Don't read "median below 1" as most places being under-represented. Compare
#    the place against the SPREAD, not the median against 1 - Sheffield's basic
#    metals sitting outside the whole body of the violin is the real signal.
#
# Incidentally the median mark doesn't move when you switch to the log axis:
# medians survive monotone transforms unchanged. The log scale changes the
# violin's shape (the density is computed on log10(LQ)) but not the median line.

# The numbers quoted above, re-runnable, so they can't quietly go stale when the
# CH extract is updated. Uses every 2 digit sector, not the filtered `twodigit`.
lq2 <- results$yeartoplot %>% filter(siclevel == 'sic2', LQ > 0)

# a. the identity: employment-weighted mean LQ is exactly 1 in every sector
lq2 %>%
  group_by(sic) %>%
  summarise(weighted_mean = sum(LQ * region_totalsize) / first(totalsize),
            .groups = 'drop') %>%
  summarise(sectors = n(), max_deviation_from_1 = max(abs(weighted_mean - 1)))

# b. where each sector's median LA sits, and whether LQ tracks how big the LA is
median_position <- lq2 %>%
  group_by(sic, sic_name) %>%
  summarise(
    median_LQ = median(LQ),
    #negative = LQ falls as the LA gets bigger
    cor_with_LA_size = cor(log(region_totalsize), log(LQ)),
    .groups = 'drop'
  ) %>%
  mutate(median_above_1 = median_LQ > 1)

# how many sit each side (was 8 above, 80 below)
median_position %>% count(median_above_1)

# the exceptions themselves (was tobacco 5.29, metal ores 2.09, coal 1.71...)
median_position %>% filter(median_above_1) %>% arrange(-median_LQ) %>%
  select(sic_name, median_LQ, cor_with_LA_size)

# c. and the reason they're exceptions: LQ falling away as the LA gets bigger
# (was about -0.41 for the eight, against ~0 for everything else)
median_position %>%
  group_by(median_above_1) %>%
  summarise(sectors = n(), mean_cor_with_LA_size = mean(cor_with_LA_size))


dist <- plot_lq_distribution(twodigit,
                             places = c('Sheffield','Leeds','Manchester'),
                             order_by = 'Sheffield', min_jobcount = 100, top_n = 25)
dist

# shape = 'violin' swaps the box for the actual density, with the quartiles kept
# as dotted lines inside it. Worth flipping between the two: the box tells you
# where the middle half sits, the violin tells you whether the spread is one lump
# or several. A sector held up by a handful of places has a long thin tail that
# the box just reports as outliers.
dist_violin <- plot_lq_distribution(twodigit,
                                    places = c('Sheffield','Leeds','Manchester'),
                                    order_by = 'Sheffield', min_jobcount = 100,
                                    top_n = 25, shape = 'violin')
dist_violin

dist_violin <- plot_lq_distribution(twodigit,
                                    places = c('Sheffield','Rotherham','Barnsley','Doncaster'),
                                    order_by = 'Sheffield', min_jobcount = 100,
                                    top_n = 25, shape = 'violin', palettename = 'Paired')
dist_violin


# 4. NESTED: a region and the places inside it ----------------------------
#
# Runs the LQ three times over: South Yorkshire as one region against GB, then its
# four LAs against GB, then the same four against South Yorkshire itself.
# parent_las is just a vector of LA names, so this works for any MCA or ad hoc
# grouping - it isn't tied to the ITL2 boundaries in the data.
#
# Takes about three times as long as a single pipeline run. Pass digitlevels = 2
# while iterating.

sy_nested <- ch_lq_nested(ch, parent_las = southyorkshire,
                          parent_name = 'South Yorkshire',
                          digitlevels = 2:5)

sy_nested <- sy_nested %>% left_join(names_bylevel, by = c('siclevel','sic'))

saveRDS(sy_nested, 'local/CH_SouthYorkshire_nested_lqs.rds')

# CHECKS ON THE NESTED MATHS
# Note the denominators here: the pipeline only emits rows for place-and-sector
# combinations that actually exist, so you cannot sum region_totalsize over the
# rows present and expect the group total. Use the totalsize column, which
# already carries it. (Getting this wrong makes correct output look broken.)

# Weight each LA's LQ by that LA's total employment, divide by the group total:
# must be exactly 1 per sector when the parent is the denominator. Comes out at
# 2e-16, i.e. floating point noise.
sy_nested %>%
  filter(denominator == 'parent', siclevel == 'sic2') %>%
  group_by(sic) %>%
  summarise(identity = sum(LQ * region_totalsize) / first(totalsize), .groups = 'drop') %>%
  summarise(max_deviation_from_1 = max(abs(identity - 1)))

# South Yorkshire's total employment should be its four LAs added up
sy_nested %>%
  filter(denominator == 'national', level == 'child') %>%
  distinct(GEOGRAPHY_NAME, region_totalsize) %>%
  summarise(from_the_four_las = sum(region_totalsize))

sy_nested %>% filter(level == 'parent') %>% pull(region_totalsize) %>% unique()

# Left panel: South Yorkshire (black diamond) and its four LAs, all against GB.
# Right panel: the four LAs against South Yorkshire's own mix, so the region sits
# at 1 by definition (the blue line) and you see who pulls the region's number up.
#
# The pair to look for: a sector where all four LAs sit right of 1 in the left
# panel but cluster ON 1 in the right panel. That's a genuinely regional strength,
# spread evenly - not one town's employer showing up as a regional statistic.
n_sy <- plot_lq_nested(sy_nested %>% filter(siclevel == 'sic2'),
                       min_jobcount = 100, top_n = 25)
n_sy

# 5 digit, where CH beats BRES on disclosure - the steel and metals detail
n_sy5 <- plot_lq_nested(sy_nested %>% filter(siclevel == 'sic5'),
                        min_jobcount = 150, top_n = 25)
n_sy5


# SAVE THE EXPERIMENTS ----------------------------------------------------

# ggsave('local/images/exp_quadrant_sheffield.png', q_lq, width = 11, height = 8)
# ggsave('local/images/exp_dumbbell_peers.png', d_peers, width = 11, height = 10)
# ggsave('local/images/exp_distribution.png', dist, width = 11, height = 10)
# ggsave('local/images/exp_nested_sy.png', n_sy, width = 13, height = 10)
