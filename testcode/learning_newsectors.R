# Test ideas for getting bespoke sector info from firms in the companies house data
library(tidyverse)
library(clipr)
library(sf)
source('functions.R')
source('adhoc_functions.R')

ch = readRDS('local/PROCESSED_accountextracts_n_livelist_geocoded_combined_Oct2025.rds')

# Nope, breaks!
# clipr::write_clip(g(ch))

# Save sample
write_csv(sample_n(ch,500), 'local/sample_ch.csv')

# We can just look at South Yorkshire and get a sense of the firms
sy = ch %>% filter(ITL221NM == 'South Yorkshire')

# Breaking down size of firms
#Code nabbed from bradford cluster qmd in regecon project
firmcount <- sy %>% st_set_geometry(NULL) %>% filter(Employees_thisyear > 0)

firmcount <- firmcount %>% 
  mutate(
    sizecategory = case_when(
      Employees_thisyear == 1 ~ "1",
      between(Employees_thisyear,2,4) ~ "2-4",
      between(Employees_thisyear,5,9) ~ "5-9",
      between(Employees_thisyear,10,20) ~ "10-20",
      between(Employees_thisyear,21,50) ~ "21-50",
      between(Employees_thisyear,51,999999) ~ "51+"
    ),
    sizecategory = factor(sizecategory, levels = c('1','2-4','5-9','10-20','21-50','51+'))
  )

firmcount <- ch %>% st_set_geometry(NULL) %>% filter(localauthority_name == 'Bradford', Employees_thisyear > 0)

firmcount <- firmcount %>% 
  mutate(
    sizecategory = case_when(
      Employees_thisyear == 1 ~ "1",
      between(Employees_thisyear,2,4) ~ "2-4",
      between(Employees_thisyear,5,9) ~ "5-9",
      between(Employees_thisyear,10,20) ~ "10-20",
      between(Employees_thisyear,21,50) ~ "21-50",
      between(Employees_thisyear,51,100) ~ "51-100",
      between(Employees_thisyear,101,999999) ~ "101+"
    ),
    sizecategory = factor(sizecategory, levels = c('1','2-4','5-9','10-20','21-50','51-100','101+'))
    )

table(firmcount$sizecategory)

#table from that to plot
firmtable <- tibble(
  `Firm size` = levels(firmcount$sizecategory),
  `Count` = table(firmcount$sizecategory),
  `Percent of firms` = paste0(round(table(firmcount$sizecategory) %>% prop.table() * 100,1),"%")
)

# Add in employee count and percent
employees = firmcount %>% 
  group_by(sizecategory) %>%
  summarise(
    totalemployees = sum(Employees_thisyear)
  ) %>% 
  mutate(
    employeepercent = paste0(round((totalemployees/sum(totalemployees))*100,1),"%")
  )


firmtable = bind_cols(firmtable,employees %>% select(-sizecategory))


# Testing Companies House / website search----

# Let's use a subsample. Having done the table above, what do the largest firms look like?
sy100 = sy %>% filter(Employees_thisyear > 100)





# TEST DOMAIN GUESS THEN VERIFY APPROACH----

# Claude code stuck into ad hoc functions

# TEST:
# Test with a known company

# Test each stage above. Gripple shouldn't be that hard...
firm_name = 'Gripple LTD'

normalise_for_domain(firm_name)

# We know one of these is correct
# Some sites might need the www to resolve correctly?
generate_domain_candidates(firm_name)

# Does this work?
domain_responds('gripple.com')

domain_exists('gripple.com')


result <- find_company_website(
  company_name = "Gripple"
)
# Should find gripple.com or gripple.co.uk without needing search API

# Fuller search
result <- find_company_website(
  company_name = "Gripple Limited",
  postcode = "S4 7UQ",
  company_number = "01772901"
)

# Batch process with fallback to Google search
set.seed(67)
# sy100 %>% sample_n(10)

results <- sy100 %>% sample_n(10) |>
  mutate(
    website = pmap(
      list(CompanyName, postcode, CompanyNumber),
      ~ find_company_website(..1, ..2, ..3, search_fn = google_search)
    )
  ) |>
  unnest_wider(website)

# Check how many avoided API calls
table(results$method)
# domain_guess: 85  <- free!
# search_api: 12    <- paid
# not_found: 3


# All good approaches there but missing easy wins
# Let's try bits of that to speed things up
# 10 seconds for 100 companies...
# Dropped to 1.2 minutes with extra variations to check
# Which still shouldn't take much more than 30 minutes to check all SY 10+ employee firms
set.seed(67)
testfirms = sy100 %>% sample_n(100) 

# The first of these is correct...
generate_domain_candidates(testfirms$CompanyName[1])

# Some need the www prefix to respond
# domain_exists(generate_domain_candidates(testfirms$CompanyName[1])[16])
# domain_responds(generate_domain_candidates(testfirms$CompanyName[1])[16])

# Domain exists is much faster

# Let's just test against our test firms now...
x = Sys.time()

testfirms = testfirms %>% 
  mutate(
    website = map_chr(CompanyName, guess_domain)
    )

print(Sys.time() - x)

table(!is.na(testfirms$website))

testfirms %>% select(CompanyName,website) %>% View


# A lot of very good matches from this simple approach
# Let's test out making some of the obvious fails better

# CHOICES HOMECARE (SOUTH YORKS) LIMITED
testfirms$CompanyName[5]
# Ended up getting "www.choices.org.uk"
testfirms$website[5]

# Despite that being quite low down on the candidate list...
generate_domain_candidates(testfirms$CompanyName[5])

# OK, fixed that by adding more candidates in - slower but more accurate

# Correct site name:
# https://choiceshomecare.co.uk



# ANALYSING WEBSITE TEXT----

# We can do some basic validation checks at this stage too.

x = get_websitefrontpage(testfirms$website[1])
cat(x)
x = get_websitefrontpage(testfirms$website[1], 'about')

# Try also

# Not much use, this function!
# get_page_meta('www.sbd-apparel.com')

x = get_clean_text(testfirms$website[1])
x = get_clean_text('gripple.com')
clipr::write_clip(x)


# Test reticulate approach, accessing local LLM
library(reticulate)

# One-time setup: create a Python environment with sentence-transformers
# conda_create("r-embeddings", packages = c("sentence-transformers"))
use_condaenv("r-embeddings")

# Import the library
st <- import("sentence_transformers")

# Load a model (downloads once, then cached locally)
# all-MiniLM-L6-v2 is fast and good quality (~80MB)
model <- st$SentenceTransformer("all-MiniLM-L6-v2")

# Get embeddings - works on single text or vector
get_local_embedding <- function(text) {
  model$encode(text, convert_to_numpy = TRUE)
}

# Batch encode is much faster than one-at-a-time
texts <- c("solar energy renewable power", "medical devices healthcare")
embeddings <- model$encode(texts)  # Returns matrix: n_texts × 384 dimensions

















