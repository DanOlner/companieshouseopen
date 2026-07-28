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

  bylevel <- map(digitlevels, count_jobs_by_SICdigitlevel, ch_long)

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

  #Shorten labels via a lookup, so two names sharing a long prefix don't collapse
  #into one factor level
  if(!is.null(truncate_labels)){

    labels <- df %>%
      distinct(!!sector_col) %>%
      mutate(
        .shortlabel = make.unique(str_trunc(as.character(!!sector_col), truncate_labels),
                                  sep = ' ')
      )

    df <- df %>%
      left_join(labels, by = sector_colname) %>%
      mutate(!!sector_colname := .shortlabel) %>%
      select(-.shortlabel)

  }

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
