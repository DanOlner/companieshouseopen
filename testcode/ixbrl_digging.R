# DIG INTO IXBRL TAGS
#Various tests
library(tidyverse)
library(sf)
options(scipen = 99)
source('functions.R')


#See what the names of the tags are, matched against values
#TODO: search several accounts, look for what frequencies we get for different iXBRL tags
doc <- read_xml("local/test_accounts/01772901_aa_2024-09-12.xhtml")

# Define namespaces dynamically
ns <- xml_ns(doc)

# Extract all <ix:nonNumeric> and <ix:nonFraction> nodes
non_numeric_nodes <- xml_find_all(doc, "//ix:nonNumeric", ns = ns)
non_fraction_nodes <- xml_find_all(doc, "//ix:nonFraction", ns = ns)

# Extract the 'name' attribute from these nodes
non_numeric_names <- xml_attr(non_numeric_nodes, "name")
non_fraction_names <- xml_attr(non_fraction_nodes, "name")

# Combine into a single vector
all_names <- c(non_numeric_names, non_fraction_names)

# Remove any NA values (nodes without a 'name' attribute)
all_names <- all_names[!is.na(all_names)]

# Print the resulting vector of names
print(all_names)

# Optional: Combine names with their text content for detailed inspection
all_details <- c(
  paste("Name:", non_numeric_names, "Value:", xml_text(non_numeric_nodes)),
  paste("Name:", non_fraction_names, "Value:", xml_text(non_fraction_nodes))
)

all_details %>% as.data.frame() %>% View

# Remove entries where the name is NA
all_details <- all_details[!is.na(all_names)]

print(all_details)

all_details[grepl('profit',all_details,ignore.case = T)]

#Dataframe version please
details.df <- bind_rows(
  data.frame(
    name = non_numeric_names, value = xml_text(non_numeric_nodes)
  ),
  data.frame(

    name = non_fraction_names, value = xml_text(non_fraction_nodes)
  )
)


# ENHANCED EXTRACTION (claude code) ----
# Capture all critical iXBRL attributes for proper numeric handling
# Key gotchas:
#   - sign="-" means negate the value (displayed positive, actual negative)
#   - scale: multiply by 10^scale (e.g. scale="3" = thousands)
#   - format="ixt2:nocontent" means empty value
#   - contextRef distinguishes time periods (FY vs cfwd vs bfwd)

details_enhanced.df <- bind_rows(
  # Non-numeric (text/boolean values)
  data.frame(
    type = "nonNumeric",
    name = xml_attr(non_numeric_nodes, "name"),
    value = xml_text(non_numeric_nodes),
    format = xml_attr(non_numeric_nodes, "format"),
    contextRef = xml_attr(non_numeric_nodes, "contextRef"),
    # These won't apply to nonNumeric but keep structure consistent
    scale = NA_character_,
    sign = NA_character_,
    decimals = NA_character_,
    unitRef = NA_character_
  ),
  # Numeric values (need scale/sign handling!)
  data.frame(
    type = "nonFraction",
    name = xml_attr(non_fraction_nodes, "name"),
    value = xml_text(non_fraction_nodes),
    format = xml_attr(non_fraction_nodes, "format"),
    contextRef = xml_attr(non_fraction_nodes, "contextRef"),
    scale = xml_attr(non_fraction_nodes, "scale"),
    sign = xml_attr(non_fraction_nodes, "sign"),
    decimals = xml_attr(non_fraction_nodes, "decimals"),
    unitRef = xml_attr(non_fraction_nodes, "unitRef")
  )
)

# Compute actual numeric value with scale and sign adjustments
details_enhanced.df <- details_enhanced.df %>%
  mutate(
    # Strip commas and convert to numeric
    value_raw = as.numeric(gsub(",", "", value)),
    # Apply scale factor (10^scale)
    value_scaled = ifelse(
      !is.na(scale),
      value_raw * (10 ^ as.numeric(scale)),
      value_raw
    ),
    # Apply sign (negate if sign="-")
    value_actual = ifelse(
      !is.na(sign) & sign == "-",
      -value_scaled,
      value_scaled
    )
  )

# View entries with sign="-" (these would be wrong without adjustment)
details_enhanced.df %>% filter(sign == "-") %>% View

# Summary of unique tag names by type
tag_summary <- details_enhanced.df %>%
  group_by(type, name) %>%
  summarise(
    count = n(),
    example_value = first(value),
    has_sign_negative = any(sign == "-", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(type, desc(count))

tag_summary %>% View


# FUNCTION TO EXTRACT FROM MULTIPLE FILES ----
# Wrap extraction in a function to map over multiple xhtml files

extract_ixbrl_tags <- function(filepath) {

  doc <- read_xml(filepath)
  ns <- xml_ns(doc)

  non_numeric_nodes <- xml_find_all(doc, "//ix:nonNumeric", ns = ns)
  non_fraction_nodes <- xml_find_all(doc, "//ix:nonFraction", ns = ns)

  df <- bind_rows(
    data.frame(
      type = "nonNumeric",
      name = xml_attr(non_numeric_nodes, "name"),
      value = xml_text(non_numeric_nodes),
      format = xml_attr(non_numeric_nodes, "format"),
      contextRef = xml_attr(non_numeric_nodes, "contextRef"),
      scale = NA_character_,
      sign = NA_character_,
      decimals = NA_character_,
      unitRef = NA_character_
    ),
    data.frame(
      type = "nonFraction",
      name = xml_attr(non_fraction_nodes, "name"),
      value = xml_text(non_fraction_nodes),
      format = xml_attr(non_fraction_nodes, "format"),
      contextRef = xml_attr(non_fraction_nodes, "contextRef"),
      scale = xml_attr(non_fraction_nodes, "scale"),
      sign = xml_attr(non_fraction_nodes, "sign"),
      decimals = xml_attr(non_fraction_nodes, "decimals"),
      unitRef = xml_attr(non_fraction_nodes, "unitRef")
    )
  )

  df$source_file <- basename(filepath)

  return(df)
}

# Example: extract from all test account files
# test_files <- list.files("local/test_accounts", pattern = "\\.xhtml$", full.names = TRUE)
# all_extracts <- map_dfr(test_files, extract_ixbrl_tags)
#
# # Tag frequency across all files
# all_extracts %>%
#   group_by(name) %>%
#   summarise(
#     n_files = n_distinct(source_file),
#     total_occurrences = n()
#   ) %>%
#   arrange(desc(n_files))


# Get list of companies to examine some with large employee numbers and look for wage bills----

ch = readRDS('local/PROCESSED_accountextracts_n_livelist_geocoded_combined_Oct2025.rds')

# Just firms with 50+ employees in latest year (26945)
# And accounts submitted in August so I can look for them

# Actual account dates vary, it would appear
# Let's just filter on company number
chsub = ch %>% 
  filter(
    Employees_thisyear >= 100,
    str_sub(accountcode,1,6) %in% c('202507','202508','202506')
    )

# That's a very small number but OK...

# Let's look at some of the accounts in there
# I've downloaded accounts for August 2025
allaccounts <- list.files('local/test_accounts/Accounts_Monthly_Data-August2025', '*.html', full.names = T)

# Matches from above
viewthese = allaccounts[qg(paste0(chsub$CompanyNumber, collapse = "|"), allaccounts)]

rez = extract_ixbrl_tags(viewthese[1])









