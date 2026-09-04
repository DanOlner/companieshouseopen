# LOCATION QUOTIENT FUNCTIONS FOR COMPANIES HOUSE DATA
#
# Ported from RegionalEconomicTools so this repo stands alone.
# Sources:
#   functions/misc_functions.R  - add_location_quotient_and_proportions, compute_slope_or_zero,
#                                 LQ_baseplot, addplacename_to_LQplot
#   functions/adhoc_functions.R - bres_countjobs_by_SICdigitlevel, getLQs_and_attachedstuff
#   prepcode/industrial_strategy_datalinkage.R:312 - the CH -> LQ pipeline
#   bits_of_code/BradfordExplore.R:1149 - the original working-out
#
# THE IDEA: CH accounts give an employee count for the current AND previous period.
# Treat those as two timepoints, compute an LQ at each, and the change in log(LQ)
# between them becomes the direction-of-travel marker on the plot (green = growing
# share, red = shrinking, size = magnitude).
#
# GOTCHA inherited from the original: the plot draws slope > 0 and slope < 0 as
# separate layers, so a sector whose share didn't move at all (slope exactly 0)
# gets no solid marker. Rare at 2 digit, less so at 5 digit in small places.
#
# Column names are deliberately kept as GEOGRAPHY_NAME / DATE / JOBCOUNT so output
# is interchangeable with the BRES pipeline in RegionalEconomicTools.

library(tidyverse)
library(sf)


# CORE LQ MATHS -----------------------------------------------------------

# Add location quotient and the proportions it's built from.
# Expects a df already filtered to a single timepoint.
add_location_quotient_and_proportions <- function(df, regionvar, lq_var, valuevar){

  regionvar <- enquo(regionvar)
  lq_var <- enquo(lq_var)
  valuevar <- enquo(valuevar)

  df <- df %>%
    group_by(!!regionvar) %>%
    mutate(
      region_totalsize = sum(!!valuevar, na.rm = T),#a. total per region, for regional denominator
      sector_regional_proportion = !!valuevar / region_totalsize#b. regional sector proportion
    ) %>%
    group_by(!!lq_var) %>%
    mutate(
      total_sectorsize = sum(!!valuevar, na.rm = T),#c. summed for EACH SECTOR, nationally
    ) %>%
    ungroup() %>%
    mutate(
      totalsize = sum(!!valuevar, na.rm = T),#d. summed for the WHOLE country, for national denominator
      sector_total_proportion = total_sectorsize / totalsize#e. national sector proportion
    ) %>%
    mutate(
      LQ = sector_regional_proportion / sector_total_proportion#f. location quotient!
    ) %>%
    mutate(LQ_log = log(LQ))

  return(df)

}


# Compute series of slopes within groups safely, returning 0 if it can't calculate.
# Kept for the general (many timepoints) case - see lq_change_between_timepoints
# for the two-timepoint CH case, which is the same number without fitting 200k models.
compute_slope_or_zero <- function(data, ..., y, x, includepercentchange = F) {

  groups <- quos(...)
  y <- enquo(y)
  x <- enquo(x)

  get_slope <- function(data) {
    model <- lm(data = data, formula = as.formula(paste0(quo_name(y), " ~ ", quo_name(x))))
    coef(model)[2]
  }

  safe_get_slope <- possibly(get_slope, otherwise = 0)

  result = data %>%
    group_by(!!!groups) %>%
    nest() %>%
    mutate(slope = map_dbl(data, safe_get_slope)) %>%
    select(-data) %>%
    ungroup()

  if(includepercentchange){

    result = result %>%
      mutate(
        percentchangepertimeunit = (exp(slope) -1) * 100
      )

  }

  return(result)

}


# Two-timepoint version of the above.
# A line through two points has slope (y2 - y1) / (x2 - x1), so this is
# arithmetically identical to compute_slope_or_zero() but runs in seconds rather
# than fitting one lm per sector-per-place. Non-finite results (LQ of zero gives
# log = -Inf) become 0, matching what possibly(otherwise = 0) did.
lq_change_between_timepoints <- function(data, ..., y = LQ_log, x = DATE, includepercentchange = F){

  groups <- quos(...)
  y <- enquo(y)
  x <- enquo(x)

  result <- data %>%
    group_by(!!!groups) %>%
    arrange(!!x, .by_group = TRUE) %>%
    summarise(
      slope = (last(!!y) - first(!!y)) / (last(!!x) - first(!!x)),
      .groups = 'drop'
    ) %>%
    mutate(
      slope = ifelse(is.finite(slope), slope, 0)
    )

  if(includepercentchange){

    result = result %>%
      mutate(
        percentchangepertimeunit = (exp(slope) -1) * 100
      )

  }

  return(result)

}


# CH DATA PREP ------------------------------------------------------------

# Turn the CH firm-level file into the long two-timepoint job counts the LQ code wants.
#
# geography_col: column name, bare or quoted - localauthority_name, 'ITL221NM', etc
# Returns GEOGRAPHY_NAME / DATE (1 = last year, 2 = this year) / SIC_5DIGIT_CODE / JOBCOUNT
ch_employee_timepoints <- function(ch, geography_col, drop_missing_geography = TRUE){

  geography_col <- ensym(geography_col)

  ch_flat <- ch

  #Drop geometry if it's still an sf object - summarise on sf is painfully slow
  if(inherits(ch_flat, 'sf')) ch_flat <- st_set_geometry(ch_flat, NULL)

  ch_sums <- ch_flat %>%
    #Keep only firms with employees in BOTH years, even if it's zero.
    #Otherwise a firm appearing in one timepoint only would look like growth/shrinkage.
    filter(!is.na(Employees_thisyear) & !is.na(Employees_lastyear)) %>%
    select(GEOGRAPHY_NAME = !!geography_col, SIC_5DIGIT_CODE,
           Employees_thisyear, Employees_lastyear) %>%
    #Codes are a fixed 5 characters, so some carry a trailing space - '7011 ' is a
    #genuine SIC recorded only to 4 digits, but 'None ' means no SIC at all
    mutate(SIC_5DIGIT_CODE = str_trim(as.character(SIC_5DIGIT_CODE)))

  nosic <- sum(!grepl('^[0-9]', ch_sums$SIC_5DIGIT_CODE) | is.na(ch_sums$SIC_5DIGIT_CODE))

  if(nosic > 0){
    #Must drop these rather than leave them as a sector: they'd otherwise sit in
    #the regional and national totals that the LQ denominators are built from
    cat("Dropping", nosic, "firms with no usable SIC code\n")
    ch_sums <- ch_sums %>% filter(grepl('^[0-9]', SIC_5DIGIT_CODE))
  }

  if(drop_missing_geography){
    ch_sums <- ch_sums %>% filter(!is.na(GEOGRAPHY_NAME))
  }

  ch_sums <- ch_sums %>%
    group_by(SIC_5DIGIT_CODE, GEOGRAPHY_NAME) %>%
    summarise(
      employeecount_thisyear = sum(Employees_thisyear),
      employeecount_lastyear = sum(Employees_lastyear),
      .groups = 'drop'
    )

  #Make those into pseudo dates in an order we can get an LQ size change from
  ch_sums %>%
    pivot_longer(employeecount_thisyear:employeecount_lastyear,
                 names_to = 'timepoint', values_to = 'JOBCOUNT') %>%
    mutate(
      DATE = ifelse(timepoint == 'employeecount_lastyear', 1, 2)
    )

}


# Roll 5 digit SIC up to 2, 3 or 4 digit by truncating the code
count_jobs_by_SICdigitlevel <- function(digitlevel, df){

  df <- df %>%
    mutate(newsic = str_sub(SIC_5DIGIT_CODE, 1, digitlevel)) %>%
    group_by(DATE, GEOGRAPHY_NAME, newsic) %>%
    summarise(
      JOBCOUNT = sum(JOBCOUNT),
      .groups = 'drop'
    )

  names(df)[names(df) == 'newsic'] = paste0('sic', digitlevel)

  return(df)

}


# SIC 2007 sections (A-U), derived from the 2 digit code by range.
#
# Deliberately NOT taken from CH's own SIC_SECTION_LETTER column: that is NULL for
# 126,774 firms which do have a perfectly good numeric SIC, and three 2 digit codes
# (70, 74, 98) carry two different values in it. Deriving from the code instead
# guarantees each level is an exact partition of the one below, which is what the
# drill-down in the web plot depends on.
sic_section_lookup <- function(){

  ranges <- tribble(
    ~section, ~from, ~to, ~section_name,
    'A',  1,  3, 'Agriculture, forestry and fishing',
    'B',  5,  9, 'Mining and quarrying',
    'C', 10, 33, 'Manufacturing',
    'D', 35, 35, 'Electricity, gas, steam and air conditioning supply',
    'E', 36, 39, 'Water supply; sewerage and waste management',
    'F', 41, 43, 'Construction',
    'G', 45, 47, 'Wholesale and retail trade; repair of motor vehicles',
    'H', 49, 53, 'Transportation and storage',
    'I', 55, 56, 'Accommodation and food service activities',
    'J', 58, 63, 'Information and communication',
    'K', 64, 66, 'Financial and insurance activities',
    'L', 68, 68, 'Real estate activities',
    'M', 69, 75, 'Professional, scientific and technical activities',
    'N', 77, 82, 'Administrative and support service activities',
    'O', 84, 84, 'Public administration and defence',
    'P', 85, 85, 'Education',
    'Q', 86, 88, 'Human health and social work activities',
    'R', 90, 93, 'Arts, entertainment and recreation',
    'S', 94, 96, 'Other service activities',
    'T', 97, 98, 'Activities of households',
    'U', 99, 99, 'Activities of extraterritorial organisations and bodies'
  )

  #The 21 ranges cover exactly the 88 divisions SIC 2007 defines (34, 40, 44, 48,
  #54, 57, 67, 76, 83 and 89 don't exist)
  ranges %>%
    mutate(sic2 = map2(from, to, ~ sprintf('%02d', .x:.y))) %>%
    unnest(sic2) %>%
    select(sic2, section, section_name)

}


# Roll up to any level the pipeline understands: 2, 3, 4, 5 or 'section'.
# Everything else in the pipeline keys off names(df)[3], so the section column is
# called sicsection and the level reads as 'sicsection' downstream.
count_jobs_by_siclevel <- function(level, df){

  if(identical(as.character(level), 'section')){

    out <- df %>%
      mutate(sic2 = str_sub(SIC_5DIGIT_CODE, 1, 2)) %>%
      left_join(sic_section_lookup() %>% select(sic2, section), by = 'sic2')

    unmapped <- sum(is.na(out$section))
    if(unmapped > 0){
      warning(unmapped, " rows have a 2 digit code outside the SIC 2007 sections")
    }

    out <- out %>%
      filter(!is.na(section)) %>%
      group_by(DATE, GEOGRAPHY_NAME, section) %>%
      summarise(JOBCOUNT = sum(JOBCOUNT), .groups = 'drop')

    names(out)[names(out) == 'section'] <- 'sicsection'
    return(out)

  }

  count_jobs_by_SICdigitlevel(as.integer(level), df)

}


# LQ per timepoint, plus slopes and all-time min/max, for one SIC digit level.
# Returns a list: $lqs (both timepoints) and $yeartoplot (latest timepoint, plot-ready).
get_lqs_and_slopes <- function(df, quiet = FALSE){

  levelcolname = names(df)[3]

  if(!quiet) cat("SIC level: ", levelcolname, "\n")

  lqs <- df %>%
    group_split(DATE) %>%
    map(
      add_location_quotient_and_proportions,
      regionvar = GEOGRAPHY_NAME,
      lq_var = !!sym(levelcolname),#string name to symbol
      valuevar = JOBCOUNT
    ) %>%
    bind_rows()

  LQ_slopes <- lq_change_between_timepoints(
    data = lqs,
    GEOGRAPHY_NAME, !!sym(levelcolname),#slopes found within whatever grouping vars are added here
    y = LQ_log, x = DATE)

  #Filter down to a single timepoint to plot
  yeartoplot <- lqs %>% filter(DATE == max(DATE))#use latest

  yeartoplot <- yeartoplot %>%
    left_join(LQ_slopes, by = c('GEOGRAPHY_NAME', levelcolname))

  #Min/max LQ over time per sector and place, to add as range bars.
  #A place-and-sector with no LQ at either timepoint gives NA rather than the
  #+/-Inf that a bare min()/max() would return (and warn about)
  safe_min <- function(x) if(all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
  safe_max <- function(x) if(all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

  minmaxes <- lqs %>%
    group_by(GEOGRAPHY_NAME, !!sym(levelcolname)) %>%
    summarise(
      min_LQ_all_time = safe_min(LQ),
      max_LQ_all_time = safe_max(LQ),
      .groups = 'drop'
    ) %>%
    mutate(
      min_LQ_all_time = ifelse(is.infinite(min_LQ_all_time), NA, min_LQ_all_time),
      max_LQ_all_time = ifelse(is.infinite(max_LQ_all_time), NA, max_LQ_all_time)
    )

  yeartoplot <- yeartoplot %>%
    left_join(minmaxes, by = c('GEOGRAPHY_NAME', levelcolname))

  #Raw employment change alongside the LQ change. The two say different things:
  #LQ can fall while employment rises, if the sector grew faster everywhere else.
  jobchange <- lqs %>%
    select(GEOGRAPHY_NAME, !!sym(levelcolname), DATE, JOBCOUNT) %>%
    pivot_wider(names_from = DATE, values_from = JOBCOUNT, names_prefix = 'jobs_t') %>%
    mutate(
      pct_change_jobs = ifelse(jobs_t1 > 0, (jobs_t2 - jobs_t1) / jobs_t1 * 100, NA_real_)
    ) %>%
    select(GEOGRAPHY_NAME, !!sym(levelcolname), jobs_lastyear = jobs_t1, pct_change_jobs)

  yeartoplot <- yeartoplot %>%
    left_join(jobchange, by = c('GEOGRAPHY_NAME', levelcolname)) %>%
    #exp(slope) - 1 turns the log-LQ slope back into a readable % change in LQ
    mutate(pct_change_LQ = (exp(slope) - 1) * 100)

  return(list(lqs = lqs, yeartoplot = yeartoplot))

}


# Whole pipeline: CH firm file -> LQs with direction-of-change, at every SIC digit level asked for.
#
# geography_col: column name, bare or quoted - localauthority_name, 'ITL221NM', etc
# digitlevels:   which SIC digit levels to produce, e.g. 2:5 or just 2
#
# Returns a list: $lqs and $yeartoplot, both stacked across digit levels with a
# 'siclevel' column saying which, and the SIC code in a common 'sic' column.
ch_lq_pipeline <- function(ch, geography_col, digitlevels = 2:5, quiet = FALSE){

  geography_col <- ensym(geography_col)

  ch_long <- ch_employee_timepoints(ch, !!geography_col)

  lq_from_long(ch_long, digitlevels = digitlevels, quiet = quiet)

}


# The back half of ch_lq_pipeline, taking the long two-timepoint counts directly.
# Split out because the nested work needs to re-run LQs over regrouped or
# subsetted versions of the same long data, and re-reading the CH file each time
# would be silly.
lq_from_long <- function(ch_long, digitlevels = 2:5, quiet = FALSE){

  #digitlevels may include 'section' as well as 2:5
  bylevel <- map(digitlevels, count_jobs_by_siclevel, ch_long)

  lq_results <- map(bylevel, get_lqs_and_slopes, quiet = quiet)

  #Stack, renaming the per-level sic column to a common name and recording which level it was
  stack <- function(which_df){
    map(lq_results, ~ .x[[which_df]] %>%
          mutate(siclevel = names(.)[3], LQ = ifelse(is.nan(LQ), 0, LQ)) %>%
          rename(sic = names(.)[3])) %>%
      bind_rows()
  }

  list(
    lqs = stack('lqs'),
    yeartoplot = stack('yeartoplot')
  )

}


# Sector name lookup straight from the CH data, for labelling plots.
# CH carries names at 2, 3 and 5 digit; 4 digit has no name column so gets the code.
ch_sic_name_lookup <- function(ch, digitlevel){

  #Sections come from the derived lookup, not the data
  if(identical(as.character(digitlevel), 'section')){

    return(
      sic_section_lookup() %>%
        distinct(sic = section, sic_name = paste0(section, ' : ', section_name)) %>%
        mutate(siclevel = 'sicsection')
    )

  }

  ch_flat <- ch
  if(inherits(ch_flat, 'sf')) ch_flat <- st_set_geometry(ch_flat, NULL)

  namecol <- switch(as.character(digitlevel),
                    '2' = 'SIC_2DIGIT_NAME',
                    '3' = 'SIC_3DIGIT_NAME',
                    '5' = 'SIC_5DIGIT_NAME',
                    NULL)

  #Trim and drop the 'None' placeholder exactly as ch_employee_timepoints does,
  #or the keys won't match the pipeline output
  codes <- ch_flat %>%
    filter(grepl('^[0-9]', SIC_5DIGIT_CODE)) %>%
    mutate(sic = str_trim(str_sub(as.character(SIC_5DIGIT_CODE), 1, digitlevel))) %>%
    filter(!is.na(sic), sic != '')

  lookup <- if(is.null(namecol)){

    codes %>%
      distinct(sic) %>%
      mutate(sic_name = sic)

  } else {

    codes %>%
      select(sic, sic_name = all_of(namecol)) %>%
      #Prefer a real name where one exists, but never drop a code: CH leaves the
      #name blank here and there, and a missing label would plot as NA
      arrange(sic, is.na(sic_name)) %>%
      distinct(sic, .keep_all = TRUE) %>%
      mutate(sic_name = coalesce(sic_name, sic))

  }

  #Tag with the level so joins can key on both. A 4-digit-only code like '7011'
  #is a valid key at more than one level, and wants a different name at each.
  lookup %>% mutate(siclevel = paste0('sic', digitlevel))

}


# PLOTTING ----------------------------------------------------------------

# Cut sector labels to a readable length. Goes via a lookup and make.unique so two
# names sharing a long prefix don't collapse into a single factor level.
shorten_sector_labels <- function(df, sector_col, truncate_labels = 60){

  sector_col <- enquo(sector_col)

  if(is.null(truncate_labels)) return(df)

  nm <- rlang::as_name(sector_col)

  labels <- df %>%
    distinct(!!sector_col) %>%
    mutate(
      .shortlabel = make.unique(str_trunc(as.character(!!sector_col), truncate_labels),
                                sep = ' ')
    )

  df %>%
    left_join(labels, by = nm) %>%
    mutate(!!nm := .shortlabel) %>%
    select(-.shortlabel)

}


# Base plot: every place as faint green/red bubbles, so one place can be overlaid on top.
# Bubble size is magnitude of change, colour is direction. x is LQ on a log scale.
LQ_baseplot <- function(df, alpha = 0.1, shape = 16, sector_name, LQ_column, change_over_time,
                        labelcolumn, enforce_levels = FALSE){

  sector_name <- enquo(sector_name)
  LQ_column <- enquo(LQ_column)
  change_over_time <- enquo(change_over_time)
  labelcolumn <- enquo(labelcolumn)

  lvls <- levels(df %>% pull(!!sector_name))

  p <- ggplot() +
    geom_point(
      data = df %>% filter(!!change_over_time > 0),
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time),
      alpha = alpha,
      shape = shape,
      colour = 'green'
    ) +
    geom_point(
      data = df %>% filter(!!change_over_time < 0),
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time * -1),
      alpha = alpha,
      shape = shape,
      colour = 'red'
    ) +
    scale_size_continuous(range = c(1,17)) +
    scale_x_log10() +
    geom_vline(xintercept = 1, colour = 'blue') +
    guides(size = 'none') +
    ylab("")

  #Keep empty factor levels on the y axis (needed when faceting/splitting a plot
  #so several panels share one sector ordering)
  if(enforce_levels) p = p + scale_y_discrete(limits = lvls, drop = FALSE)

  if(!rlang::quo_is_missing(labelcolumn)){

    p = p + geom_text(
      data = df %>% filter(!!change_over_time > 0),
      aes(y = !!sector_name, x = !!LQ_column, label = !!labelcolumn),
      colour = 'green', size = 3
    ) + geom_text(
      data = df %>% filter(!!change_over_time < 0),
      aes(y = !!sector_name, x = !!LQ_column, label = !!labelcolumn),
      colour = 'red', size = 3
    )

  }

  return(p)

}


# Overlay a single place on the base plot, in solid colour with a dark outline.
# It expects:
#   a df with a region column and a sector column (an ordered factor, ordered before it gets here)
#   a column with the LQ value
#   a column of change over time showing growth or shrinkage
# Optionally:
#   value_column + sector_regional_proportion - both needed for the size/percent text labels
#   min_LQ_all_time + max_LQ_all_time - both needed for the range bars
addplacename_to_LQplot <- function(df, plot_to_addto, placename, shapenumber = 16, backgroundcolour = 'black',
                                   setalpha = 1,
                                   region_name, sector_name, change_over_time, value_column, LQ_column,
                                   sector_regional_proportion,
                                   min_LQ_all_time, max_LQ_all_time, value_col_ismoney = TRUE,
                                   nudgepos = 0, maxLQvalmultiplier = 3,
                                   useplacenameforminmaxdisplay = FALSE, overridetextpos = NULL){

  region_name <- enquo(region_name)
  sector_name <- enquo(sector_name)
  change_over_time <- enquo(change_over_time)
  LQ_column <- enquo(LQ_column)
  min_LQ_all_time <- enquo(min_LQ_all_time)
  max_LQ_all_time <- enquo(max_LQ_all_time)

  #Work out how far right the plot needs to go, so text labels have somewhere to sit.
  #Use the all-time max LQ if we've been given it, otherwise this timepoint's LQ.
  if(rlang::quo_is_missing(max_LQ_all_time)){

    if(useplacenameforminmaxdisplay){
      maxLQval <- df %>% filter(!!region_name == placename) %>%
        filter(!!LQ_column == max(!!LQ_column, na.rm = T)) %>% pull(!!LQ_column)
    } else {
      maxLQval <- df %>% filter(!!LQ_column == max(!!LQ_column, na.rm = T)) %>% pull(!!LQ_column)
    }

  } else {

    if(useplacenameforminmaxdisplay){
      maxLQval <- df %>% filter(!!region_name == placename) %>%
        filter(!!max_LQ_all_time == max(!!max_LQ_all_time, na.rm = T)) %>% pull(!!LQ_column)
    } else {
      maxLQval <- df %>% filter(!!max_LQ_all_time == max(!!max_LQ_all_time, na.rm = T)) %>%
        pull(!!max_LQ_all_time)
    }

  }

  #Ties can give more than one row back; we only need the number.
  #Fall back to this timepoint's largest LQ if the range columns were all NA.
  maxLQval <- maxLQval[1]
  if(length(maxLQval) == 0 || !is.finite(maxLQval)){
    maxLQval <- df %>% pull(!!LQ_column) %>% max(na.rm = TRUE)
  }

  plot_to_addto <- plot_to_addto +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time > 0),
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time * 1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha,
      position = position_nudge(y = nudgepos)
    ) +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time < 0),
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time * -1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha,
      position = position_nudge(y = nudgepos)
    ) +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time > 0),
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time),
      shape = shapenumber,
      colour = 'green',
      alpha = setalpha,
      position = position_nudge(y = nudgepos)
    ) +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time < 0),
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time * -1),
      shape = shapenumber,
      colour = 'red',
      alpha = setalpha,
      position = position_nudge(y = nudgepos)
    )

  #Need both of these for the text labels, don't display if either missing
  if(!(missing(value_column) | missing(sector_regional_proportion))){

    value_column <- enquo(value_column)
    sector_regional_proportion <- enquo(sector_regional_proportion)

    textx <- if(is.null(overridetextpos)) maxLQval * maxLQvalmultiplier else overridetextpos

    labeltext <- if(value_col_ismoney){
      quo(paste0('£', !!value_column, 'M, ', round(!!sector_regional_proportion * 100, 2), '%'))
    } else {
      quo(paste0(!!value_column, ', ', round(!!sector_regional_proportion * 100, 2), '%'))
    }

    plot_to_addto <- plot_to_addto +
      geom_text(
        data = df %>% filter(!!region_name == placename),
        aes(y = !!sector_name, x = !!textx, label = !!labeltext),
        hjust = 1, alpha = 0.7, size = 3,
        position = position_nudge(y = nudgepos)
      )

  }

  #Need both of these for the range bars, don't display if either missing
  if(!(rlang::quo_is_missing(min_LQ_all_time) | rlang::quo_is_missing(max_LQ_all_time))){

    plot_to_addto <- plot_to_addto +
      geom_errorbar(
        data = df %>% filter(!!region_name == placename),
        aes(y = !!sector_name, xmin = !!min_LQ_all_time, xmax = !!max_LQ_all_time),
        width = 0.1,
        position = position_nudge(y = nudgepos)
      )

  }

  plot_to_addto <- plot_to_addto +
    coord_cartesian(xlim = c(0.1, maxLQval * maxLQvalmultiplier))

  return(plot_to_addto)

}


# One-call wrapper: from ch_lq_pipeline()$yeartoplot to a finished plot for one place.
#
# yeartoplot:    a single SIC digit level's rows (filter siclevel yourself if you ran several)
# place:         the value in GEOGRAPHY_NAME to highlight
# sector_col:    bare column to use as the y axis label (sic, or sic_name if you joined names on)
# min_jobcount:  drop sectors where the place has fewer than this many employees recorded
# truncate_labels: cut y axis labels to this many characters, NULL to leave alone.
#                  CH carries the full SIC names, some of which run to 130 characters
#                  and squash the plot panel to nothing
# show_range_bars: needs min_LQ_all_time / max_LQ_all_time present; with two timepoints
#                  these bars just span last year to this year
plot_lq_for_place <- function(yeartoplot, place, sector_col = sic, min_jobcount = 0,
                              truncate_labels = 60,
                              alpha = 0.03, shapenumber = 16, maxLQvalmultiplier = 3,
                              show_range_bars = TRUE, title = NULL){

  sector_col <- enquo(sector_col)
  sector_colname <- rlang::as_name(sector_col)

  if(!place %in% yeartoplot$GEOGRAPHY_NAME){
    stop("'", place, "' not found in GEOGRAPHY_NAME. Check spelling / geography level.")
  }

  #Keep only sectors where the place clears the job count threshold
  keep <- yeartoplot %>%
    filter(GEOGRAPHY_NAME == place, JOBCOUNT >= min_jobcount, !is.na(!!sector_col)) %>%
    pull(!!sector_col) %>%
    unique()

  if(length(keep) == 0){
    stop("No sectors left for '", place, "' at min_jobcount = ", min_jobcount, ". Try lower.")
  }

  df <- yeartoplot %>%
    filter(!is.na(!!sector_col), (!!sector_col) %in% keep)

  df <- shorten_sector_labels(df, !!sector_col, truncate_labels)

  #Order sectors by this place's LQ, descending
  sectorLQorder <- df %>%
    filter(GEOGRAPHY_NAME == place) %>%
    arrange(-LQ) %>%
    pull(!!sector_col)

  df <- df %>%
    mutate(!!sector_colname := factor(!!sector_col, levels = sectorLQorder, ordered = TRUE))

  p <- LQ_baseplot(df = df, alpha = alpha, sector_name = !!sector_col,
                   LQ_column = LQ, change_over_time = slope)

  has_range <- show_range_bars &&
    all(c('min_LQ_all_time','max_LQ_all_time') %in% names(df))

  if(has_range){

    p <- addplacename_to_LQplot(
      df = df, plot_to_addto = p, placename = place, shapenumber = shapenumber,
      min_LQ_all_time = min_LQ_all_time, max_LQ_all_time = max_LQ_all_time,
      value_column = JOBCOUNT, sector_regional_proportion = sector_regional_proportion,
      region_name = GEOGRAPHY_NAME, sector_name = !!sector_col,
      change_over_time = slope, LQ_column = LQ,
      value_col_ismoney = FALSE, maxLQvalmultiplier = maxLQvalmultiplier,
      useplacenameforminmaxdisplay = TRUE)

  } else {

    p <- addplacename_to_LQplot(
      df = df, plot_to_addto = p, placename = place, shapenumber = shapenumber,
      value_column = JOBCOUNT, sector_regional_proportion = sector_regional_proportion,
      region_name = GEOGRAPHY_NAME, sector_name = !!sector_col,
      change_over_time = slope, LQ_column = LQ,
      value_col_ismoney = FALSE, maxLQvalmultiplier = maxLQvalmultiplier,
      useplacenameforminmaxdisplay = TRUE)

  }

  if(!is.null(title)) p <- p + ggtitle(title)

  return(p)

}


# ---------------------------------------------------------------------------
# LQ EXPERIMENTS: alternative ways of reading the same numbers
# ---------------------------------------------------------------------------

# Core cities as they appear in localauthority_name. RegionalEconomicTools builds
# this list at ITL3; CH only carries LA, so it's rebuilt here.
# Belfast won't match - the LA lookup in the processed file is GB only. The
# RegionalEconomicTools version has the same gap ("check... false will be Belfast").
uk_core_cities <- function(){
  c('Belfast', 'Birmingham', 'Bristol, City of', 'Cardiff', 'Glasgow City',
    'Leeds', 'Liverpool', 'Manchester', 'Newcastle upon Tyne', 'Nottingham',
    'Sheffield')
}


# NESTED GEOGRAPHY --------------------------------------------------------

# LQs for a group of LAs treated as one region, AND for each LA inside it,
# against two different denominators:
#
#   denominator = 'national' - the usual LQ against GB. Says "is this sector
#     over-represented here compared with the country". Parent and children are
#     on one comparable scale, so you can see whether an LA carries the region's
#     specialism or dilutes it.
#
#   denominator = 'parent' - each LA against the parent region's own sector mix.
#     Says "within South Yorkshire, where does this sector actually sit". The
#     parent is 1 by construction here, so it isn't drawn.
#
# The two can disagree, and that's the interesting case: a sector can be strong
# across the whole region (high national LQ everywhere) while being evenly spread
# within it (parent LQ near 1 for every LA).
#
# parent_las: character vector of localauthority_name values, so this works for
#   MCAs, travel-to-work areas or any ad hoc grouping, not just ONS geographies.
ch_lq_nested <- function(ch, parent_las, parent_name = 'Parent region',
                         digitlevels = 2:5, quiet = FALSE){

  ch_long <- ch_employee_timepoints(ch, localauthority_name)

  missing_las <- setdiff(parent_las, unique(ch_long$GEOGRAPHY_NAME))
  if(length(missing_las) > 0){
    warning("Not found in localauthority_name: ", paste(missing_las, collapse = ', '))
  }

  #1. Parent as a single region, against GB. Every other LA stays separate so the
  #national denominator is still the whole country.
  parent_long <- ch_long %>%
    mutate(
      GEOGRAPHY_NAME = ifelse(GEOGRAPHY_NAME %in% parent_las, parent_name, GEOGRAPHY_NAME)
    ) %>%
    group_by(GEOGRAPHY_NAME, DATE, SIC_5DIGIT_CODE) %>%
    summarise(JOBCOUNT = sum(JOBCOUNT), .groups = 'drop')

  parent_national <- lq_from_long(parent_long, digitlevels, quiet)$yeartoplot %>%
    filter(GEOGRAPHY_NAME == parent_name) %>%
    mutate(level = 'parent', denominator = 'national')

  #2. Children against GB - computed over all LAs, then filtered, so the
  #denominator stays national rather than becoming the region
  child_national <- lq_from_long(ch_long, digitlevels, quiet)$yeartoplot %>%
    filter(GEOGRAPHY_NAME %in% parent_las) %>%
    mutate(level = 'child', denominator = 'national')

  #3. Children against the parent - subset FIRST, so the region is the denominator
  child_parent <- lq_from_long(
    ch_long %>% filter(GEOGRAPHY_NAME %in% parent_las), digitlevels, quiet
  )$yeartoplot %>%
    mutate(level = 'child', denominator = 'parent')

  bind_rows(parent_national, child_national, child_parent)

}


# PLOTS -------------------------------------------------------------------

#Use ggrepel where it's available, plain text where it isn't
maybe_repel <- function(...){
  if(requireNamespace('ggrepel', quietly = TRUE)){
    ggrepel::geom_text_repel(..., max.overlaps = Inf, min.segment.length = 0.2,
                             segment.colour = 'grey60', segment.size = 0.3)
  } else {
    geom_text(...)
  }
}

#Dark2 tops out at 8 colours and the core cities list is 11, so fall back to a
#generated hue scale rather than letting brewer silently drop places
place_colour_scale <- function(n, palettename = 'Dark2'){
  if(n <= 8) scale_colour_brewer(palette = palettename) else scale_colour_hue(l = 45, c = 90)
}

#Shared: pick the sectors to show, based on one place's LQ ranking
top_sectors_for <- function(df, place, sector_col, min_jobcount, top_n){
  sector_col <- enquo(sector_col)
  out <- df %>%
    filter(GEOGRAPHY_NAME == place, JOBCOUNT >= min_jobcount, LQ > 0,
           !is.na(!!sector_col))
  if(nrow(out) == 0){
    stop("No sectors for '", place, "' at min_jobcount = ", min_jobcount, ". Try lower.")
  }
  out %>% slice_max(LQ, n = top_n, with_ties = FALSE) %>% pull(!!sector_col)
}


# 1. QUADRANT: size against direction ------------------------------------
#
# The main plot encodes change as bubble size, which is hard to read and can't
# show a signed quantity well. This puts it on its own axis instead.
#   x = LQ (log), y = % change. Four readable quadrants.
#
# change = 'LQ'   - % change in the location quotient (consistent with the main plot)
# change = 'jobs' - % change in the place's actual employee count. These differ:
#   a sector can add employees while its LQ falls, if it grew faster nationally.
plot_lq_quadrant <- function(yeartoplot, place, sector_col = sic_name,
                             min_jobcount = 100, change = c('LQ','jobs'),
                             y_scale = c('pseudolog','linear'),
                             label_n = 25, truncate_labels = 45, title = NULL){

  sector_col <- enquo(sector_col)
  change <- match.arg(change)
  y_scale <- match.arg(y_scale)

  #pct_change_LQ and pct_change_jobs were added to the pipeline after the first
  #version of this file, so a results object (or a saved CH_yeartoplot.rds) built
  #before that won't have them. pct_change_LQ is just a transform of slope, so
  #derive it rather than making you re-run. pct_change_jobs needs both timepoints
  #and can't be recovered from this frame, so that one has to be a hard stop.
  if(!'change_pct' %in% names(yeartoplot)){

    if(change == 'LQ' && !'pct_change_LQ' %in% names(yeartoplot)){
      if(!'slope' %in% names(yeartoplot)) stop("No 'slope' column - is this a yeartoplot frame?")
      yeartoplot <- yeartoplot %>% mutate(pct_change_LQ = (exp(slope) - 1) * 100)
    }

    if(change == 'jobs' && !'pct_change_jobs' %in% names(yeartoplot)){
      stop("No 'pct_change_jobs' column. This frame predates it being added to ",
           "the pipeline - re-run ch_lq_pipeline() to get it.")
    }

  }

  df <- yeartoplot %>%
    filter(GEOGRAPHY_NAME == place, JOBCOUNT >= min_jobcount, LQ > 0,
           !is.na(!!sector_col)) %>%
    mutate(change_pct = if(change == 'LQ') pct_change_LQ else pct_change_jobs) %>%
    filter(is.finite(change_pct))

  if(nrow(df) == 0){
    stop("Nothing to plot for '", place, "' at min_jobcount = ", min_jobcount, ".")
  }

  df <- shorten_sector_labels(df, !!sector_col, truncate_labels) %>%
    mutate(
      quadrant = case_when(
        LQ >= 1 & change_pct >= 0 ~ 'Concentrated, growing',
        LQ >= 1 & change_pct <  0 ~ 'Concentrated, shrinking',
        LQ <  1 & change_pct >= 0 ~ 'Under-represented, growing',
        TRUE                      ~ 'Under-represented, shrinking'
      )
    )

  tolabel <- df %>% slice_max(JOBCOUNT, n = label_n, with_ties = FALSE)

  #A single sector doubling its LQ flattens everything else onto the zero line on
  #a linear axis. Pseudo-log stays linear near zero and compresses the tails, so
  #the bulk stays readable without hiding the outliers. Set y_scale = 'linear' to
  #see the raw spread.
  yscale <- if(y_scale == 'pseudolog'){
    scale_y_continuous(
      transform = scales::pseudo_log_trans(sigma = 5, base = 10),
      breaks = c(-100, -50, -25, -10, 0, 10, 25, 50, 100, 200, 500)
    )
  } else {
    scale_y_continuous()
  }

  ggplot(df, aes(x = LQ, y = change_pct)) +
    geom_hline(yintercept = 0, colour = 'grey40') +
    geom_vline(xintercept = 1, colour = 'blue') +
    geom_point(aes(size = JOBCOUNT, colour = quadrant), alpha = 0.8) +
    maybe_repel(data = tolabel, aes(label = !!sector_col), size = 2.7) +
    scale_x_log10(labels = scales::comma) +
    yscale +
    scale_size_continuous(range = c(1.5, 13), labels = scales::comma) +
    scale_colour_manual(values = c(
      'Concentrated, growing'        = '#1b7837',
      'Concentrated, shrinking'      = '#c0392b',
      'Under-represented, growing'   = '#5aa2d0',
      'Under-represented, shrinking' = '#9a9a9a'
    )) +
    labs(
      x = 'LQ, log scale (right of the blue line = more concentrated than GB)',
      y = paste0('% change in ', if(change == 'LQ') 'LQ' else 'employees'),
      colour = NULL, size = 'employees',
      title = title %||% paste0(place, ': sector concentration against direction of travel')
    ) +
    theme(legend.position = 'bottom')

}


# 2. DUMBBELL: place against place ---------------------------------------
#
# Replaces the faint bubble cloud with named places you can actually pick out.
# One row per sector, one dot per place, a grey span showing the spread between them.
plot_lq_dumbbell <- function(yeartoplot, places, sector_col = sic_name,
                             order_by = places[1], min_jobcount = 100,
                             top_n = 30, truncate_labels = 55, title = NULL){

  sector_col <- enquo(sector_col)

  keep <- top_sectors_for(yeartoplot, order_by, !!sector_col, min_jobcount, top_n)

  df <- yeartoplot %>%
    filter(GEOGRAPHY_NAME %in% places, (!!sector_col) %in% keep, LQ > 0) %>%
    shorten_sector_labels(!!sector_col, truncate_labels)

  nm <- rlang::as_name(sector_col)

  ord <- df %>% filter(GEOGRAPHY_NAME == order_by) %>% arrange(LQ) %>% pull(!!sector_col)
  df <- df %>% mutate(!!nm := factor(!!sector_col, levels = ord, ordered = TRUE))

  spans <- df %>%
    group_by(!!sector_col) %>%
    summarise(lo = min(LQ, na.rm = TRUE), hi = max(LQ, na.rm = TRUE), .groups = 'drop')

  ggplot() +
    geom_vline(xintercept = 1, colour = 'blue') +
    geom_segment(data = spans,
                 aes(y = !!sector_col, yend = !!sector_col, x = lo, xend = hi),
                 colour = 'grey75', linewidth = 1.1) +
    #Dark halo behind the ordering place so the eye can follow it down the rows
    geom_point(data = df %>% filter(GEOGRAPHY_NAME == order_by),
               aes(y = !!sector_col, x = LQ), size = 4.6, colour = 'grey20') +
    geom_point(data = df, aes(y = !!sector_col, x = LQ, colour = GEOGRAPHY_NAME),
               size = 3, alpha = 0.9) +
    scale_x_log10(labels = scales::comma) +
    place_colour_scale(length(unique(df$GEOGRAPHY_NAME))) +
    labs(x = 'LQ, log scale', y = NULL, colour = NULL,
         title = title %||% paste0('LQ by sector, ordered by ', order_by)) +
    theme(legend.position = 'bottom')

}


# 3. DISTRIBUTION: is this LQ actually unusual? ---------------------------
#
# An LQ of 1.5 means different things in different sectors - some are evenly
# spread across the country, some are concentrated in two or three places.
# This shows the full spread of every LA's LQ per sector, with chosen places on it.
#
# shape = 'box'    - quartiles and outliers. Precise, and easy to read a place's
#                    position off, but says nothing about the shape of the spread.
# shape = 'violin' - the actual density. Shows bimodality and long tails that a
#                    box hides: a sector concentrated in a handful of places looks
#                    quite different from one that is evenly spread but wide.
#                    Quartile lines are drawn inside it so you don't lose what the
#                    box was telling you.
#
# Note the density is computed on log10(LQ), not raw LQ, because ggplot applies
# the scale transform before the stat. That is the right way round here - it means
# the violin's shape matches the axis it is drawn against.
plot_lq_distribution <- function(yeartoplot, places, sector_col = sic_name,
                                 order_by = places[1], min_jobcount = 100,
                                 top_n = 25, truncate_labels = 55, palettename = 'Dark2',
                                 shape = c('box','violin'), title = NULL){

  sector_col <- enquo(sector_col)
  shape <- match.arg(shape)

  keep <- top_sectors_for(yeartoplot, order_by, !!sector_col, min_jobcount, top_n)

  allplaces <- yeartoplot %>%
    filter((!!sector_col) %in% keep, LQ > 0, !is.na(!!sector_col)) %>%
    shorten_sector_labels(!!sector_col, truncate_labels)

  nm <- rlang::as_name(sector_col)

  ord <- allplaces %>%
    filter(GEOGRAPHY_NAME == order_by) %>% arrange(LQ) %>% pull(!!sector_col)
  allplaces <- allplaces %>%
    mutate(!!nm := factor(!!sector_col, levels = ord, ordered = TRUE))

  marked <- allplaces %>% filter(GEOGRAPHY_NAME %in% places)

  spread_layer <- if(shape == 'violin'){

    #scale = 'width' so every row is the same height: we're comparing where a
    #place sits within each sector, not how many LAs report each sector.
    #quantiles / quantile.linetype are the ggplot2 4.0 spelling - draw_quantiles
    #still works but is deprecated (and its warning misnames the replacement).
    geom_violin(data = allplaces, aes(y = !!sector_col, x = LQ),
                orientation = 'y', scale = 'width', trim = TRUE,
                #not grey92: that is exactly theme_grey's panel colour, so the
                #violin bodies vanish and you only see the outline
                fill = 'grey98', colour = 'grey55', linewidth = 0.3,
                quantiles = c(0.25, 0.5, 0.75),
                quantile.linetype = 'dotted', quantile.colour = 'grey40',
                quantile.linewidth = 0.3)

  } else {

    geom_boxplot(data = allplaces, aes(y = !!sector_col, x = LQ),
                 colour = 'grey55', outlier.size = 0.4, outlier.alpha = 0.2,
                 linewidth = 0.3)

  }

  xlab <- if(shape == 'violin'){
    'LQ, log scale - violin shows the density across every LA, quartiles dotted'
  } else {
    'LQ, log scale - box shows the spread across every LA in the data'
  }

  ggplot() +
    geom_vline(xintercept = 1, colour = 'blue') +
    spread_layer +
    geom_point(data = marked, aes(y = !!sector_col, x = LQ, colour = GEOGRAPHY_NAME),
               size = 2.8, alpha = 0.9) +
    scale_x_log10(labels = scales::comma) +
    place_colour_scale(length(unique(marked$GEOGRAPHY_NAME)), palettename) +
    labs(x = xlab, y = NULL, colour = NULL,
         title = title %||% paste0('Where these places sit in the national LQ spread')) +
    theme(legend.position = 'bottom')

}


# 4. NESTED: region and its parts on one sector ordering -------------------
#
# Takes ch_lq_nested() output. Sectors are ordered by the PARENT's national LQ,
# so the region's story stays legible while you see which LA is driving it.
# Left panel: everyone against GB. Right panel: the LAs against the region itself
# (the parent is 1 by construction there, so only the blue line represents it).
plot_lq_nested <- function(nested, sector_col = sic_name, min_jobcount = 100,
                           top_n = 25, truncate_labels = 55, title = NULL){

  sector_col <- enquo(sector_col)

  parent_name <- nested %>% filter(level == 'parent') %>% pull(GEOGRAPHY_NAME) %>% unique()

  keep <- nested %>%
    filter(level == 'parent', denominator == 'national') %>%
    top_sectors_for(parent_name, !!sector_col, min_jobcount, top_n)

  df <- nested %>%
    filter((!!sector_col) %in% keep, LQ > 0, !is.na(!!sector_col)) %>%
    shorten_sector_labels(!!sector_col, truncate_labels)

  nm <- rlang::as_name(sector_col)

  ord <- df %>%
    filter(level == 'parent', denominator == 'national') %>%
    arrange(LQ) %>% pull(!!sector_col)

  df <- df %>%
    mutate(
      !!nm := factor(!!sector_col, levels = ord, ordered = TRUE),
      facet = ifelse(denominator == 'national',
                     'vs GB', paste0('vs ', parent_name))
    ) %>%
    mutate(facet = factor(facet, levels = c('vs GB', paste0('vs ', parent_name))))

  ggplot() +
    geom_vline(xintercept = 1, colour = 'blue') +
    geom_point(data = df %>% filter(level == 'child'),
               aes(y = !!sector_col, x = LQ, colour = GEOGRAPHY_NAME),
               size = 2.8, alpha = 0.9) +
    geom_point(data = df %>% filter(level == 'parent'),
               aes(y = !!sector_col, x = LQ),
               shape = 23, size = 3.6, fill = 'black', colour = 'black') +
    facet_wrap(~facet, scales = 'free_x') +
    scale_x_log10(labels = scales::comma) +
    scale_colour_brewer(palette = 'Dark2') +
    labs(x = 'LQ, log scale', y = NULL, colour = NULL,
         title = title %||% paste0(parent_name, ' and its local authorities'),
         caption = paste0('Black diamond = ', parent_name,
                          '. Sectors ordered by its LQ against GB.')) +
    theme(legend.position = 'bottom')

}
