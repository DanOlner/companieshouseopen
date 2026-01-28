# Test ideas for getting bespoke sector info from firms in the companies house data
library(tidyverse)
library(clipr)
library(sf)
library(tmap)
library(furrr)
library(plotly)
library(DataEditR)
source('functions.R')
source('adhoc_functions.R')

options(scipen = 999)

# Set up parallel processing (uses all available cores minus one)
plan(multisession, workers = availableCores() - 1)

ch = readRDS('local/PROCESSED_accountextracts_n_livelist_geocoded_combined_Dec2025.rds')

# Nope, breaks!
# clipr::write_clip(g(ch))

# Save sample
write_csv(sample_n(ch,500), 'local/sample_ch.csv')

# We can just look at South Yorkshire and get a sense of the firms
sy = ch %>% filter(ITL221NM == 'South Yorkshire')

saveRDS(sy, 'local/sy_ch_PROCESSED_Dec2025.rds')

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

firmtable

# How many firms with 5+ employees? 7089. OK then.
firmcount %>% filter(Employees_thisyear > 4) %>% nrow()#7089 = 81% of employees in CH
firmcount %>% filter(Employees_thisyear > 9) %>% nrow()#3661 = 65% of employees in CH

# Testing Companies House / website search----

# Let's use a subsample. Having done the table above, what do the largest firms look like?
sy100 = sy %>% filter(Employees_thisyear > 100)

# Person on innovation board
# ch %>% filter(qg('agemaspark', CompanyName)) %>% View


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

# results <- sy100 %>% sample_n(10) |>
#   mutate(
#     website = pmap(
#       list(CompanyName, postcode, CompanyNumber),
#       ~ find_company_website(..1, ..2, ..3, search_fn = google_search)
#     )
#   ) |>
#   unnest_wider(website)

# Check how many avoided API calls
# table(results$method)
# domain_guess: 85  <- free!
# search_api: 12    <- paid
# not_found: 3


# All good approaches there but missing easy wins
# Let's try bits of that to speed things up
# 10 seconds for 100 companies...
# Dropped to 1.2 minutes with extra variations to check
# Which still shouldn't take much more than 30 minutes to check all SY 10+ employee firms
set.seed(67)
testfirms = sy %>% sample_n(100) 

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
generate_domain_candidates('TITAN INTERIOR SOLUTIONS LIMITED')

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




# Testing getting all links from a page to search for a contact or about page reference
# Extract all non-empty hrefs
doc <- read_html("http://gripple.com")

all_links <- xml_attr(
  xml_find_all(doc, "//a[@href and string-length(@href) > 0]"),
  "href"
)

all_links[qg('contact', all_links)]





# Let's see about finding a postcode or other firm feature on a page
# These all with spaces removed
testfirms$postcode

x = get_clean_text('gripple.com/about-gripple/contact-us/') %>% toupper() %>% gsub(' ','', .)
qg('S47UQ', x)

# Or get full non cleaned page? Yeah, on front page if we get it all
x = get_websitefrontpage('gripple.com') %>% toupper() %>% gsub(' ','', .)
qg('S47UQ', x)



# Testing getting all links from a page to search for a contact or about page reference
# Extract all non-empty hrefs
doc <- read_html("http://gripple.com")

all_links <- xml_attr(
  xml_find_all(doc, "//a[@href and string-length(@href) > 0]"),
  "href"
)

all_links[qg('about', all_links)]



# Test if postcode present on page or contact page
# debugonce(check_for_postcode)
results <- pmap(testfirms, check_for_postcode)

# 23% from just the main page... 
# Up to 31% from contact and about. Hmm. Not awful, could be better!
table(sapply(results, "[[", 1))

testfirms = testfirms %>% 
  mutate(
    website_validatedbypostcode = sapply(results, "[[", 1)
  )

# For some firms that didn't get a match
# Test if any other website candidates were actually the correct ones
# Using a different function that gets all existing sites

# Testing with these
# The website column is the original single guess
testfirms.false = testfirms %>% 
  filter(!website_validatedbypostcode, !is.na(website))

candidates = return_all_existing_candidate_domains('SOLARFRAME HOLDINGS LIMITED')

# Test each website candidate
for(candidate in candidates){
  
  firmtopassin = testfirms.false %>% 
    filter(CompanyName == 'SOLARFRAME HOLDINGS LIMITED') %>% 
    mutate(website = candidate)
  
  result = check_for_postcode(candidate, firmtopassin$postcode)
  
  if(result){
    
    cat('Positive: ', candidate, '\n')
    break
    
  }
  
}

# Test Claude's batch version of the above...
# Using furrr::future_map2_chr for parallel processing

# 1.6 mins for 20 firms...
# 3 mins for 60 firms

# Say we searched just firms 10+employees in latest year
# That's 1868 + 1057 + 330 + 195 = 3450
# 1868 + 1057 + 330 + 195 
# Only 3 hours? Not disastrous
# ((3450/60) * 3) /60

x = Sys.time()

# chk <- testfirms.false %>% slice(1:20) %>% 
testfirms <- sy100[1:10,] %>%
  mutate(
    validated_website = future_map2_chr(
      CompanyName,
      postcode,
      find_validated_website,
      max_candidates = 10,
      .progress = TRUE
    )
  )

print(Sys.time() - x)

#Stick em together to check both
combo = testfirms %>% 
  st_set_geometry(NULL) %>% 
  left_join(
    testfirms.false %>% st_set_geometry(NULL) %>% select(CompanyName,accountcode,validated_website),
    by = c('CompanyName','accountcode')
  )

# This is a bit of mess, but...
combo = combo %>% 
  mutate(
    validated_website_final = case_when(
      website_validatedbypostcode ~ website,
      !is.na(validated_website) ~ validated_website,
      .default = NA
    )
  )

saveRDS(combo,'local/testing_firmwebsites.rds')

# So difference from guessing 1st website to trying 10 is...
# 31% to 41%. Woop!
table(combo$website_validatedbypostcode)
table(!is.na(combo$validated_website_final))




# Guessing the numbers will drop for smaller firms
sy10to100 = sy %>% filter(between(Employees_thisyear,10,100))

set.seed(67)
testfirms10to100 = sample_n(sy10to100,100)

x = Sys.time()

testfirms10to100 = testfirms10to100 %>% 
  mutate(
    website = map_chr(CompanyName, guess_domain)
  )

print(Sys.time() - x)

table(!is.na(testfirms10to100$website))

# Aaand postcode validate...
x = Sys.time()

results10to100 <- pmap(testfirms10to100, check_for_postcode)

print(Sys.time() - x)

# 22% validation for firms with 10 to 100 employees. Not awful.
# Nearly six minutes for 100 firms. SY has 3255 of those, so...
# 3.5 hours (not counting the initial website search)
# Wonder if it can be parallelised? Probably.
table(sapply(results10to100, "[[", 1))



# Check latest refactor that lets us keep clean text...
# chk = pmap(testfirms10to100[1:10,], check_for_postcode)

# Does this return the expected list?
# Test with a known positive from the main page, get also about page text
find_validated_website(
  testfirms.false$CompanyName[2], 
  testfirms.false$postcode[2],
  max_candidates = 10
)


# Check directly
result <- check_for_postcode("gripple.com", "S47UQ")
result
result$about_text  # Check if this is NA or has content

clipr::write_clip(paste0(result$main_text,result$about_text))

# Check for full search
chk <- future_map2(
  sy100$CompanyName[1:10],
  sy100$postcode[1:10],
  find_validated_website,
  max_candidates = 10,
  .progress = TRUE
) |> bind_rows()# to tibble

joined = sy100 %>% slice(1:10) %>% cbind(chk)

# Might be a scope issue?
# Check with known matches
# knownpositives = combo %>% filter(!is.na(validated_website_final))
# 
# chk <- future_map2(
#   knownpositives$CompanyName[1:10],
#   knownpositives$postcode[1:10],
#   find_validated_website,
#   max_candidates = 10,
#   .progress = TRUE
# )



# 60kb for 10 firms. So roughly 350 times bigger for a decent number
# Which would be ~20mb. Totally fine, huzzah.
pryr::object_size(chk)


# Testing storing the website text so we don't have to get it twice 
# To do the ML similarity
x = get_page_content('gripple.com')

# Get a sample of various



# Test reticulate approach, accessing local LLM
# library(reticulate)
# 
# # One-time setup: create a Python environment with sentence-transformers
# # conda_create("r-embeddings", packages = c("sentence-transformers"))
# use_condaenv("r-embeddings")
# 
# # Import the library
# st <- import("sentence_transformers")
# 
# # Load a model (downloads once, then cached locally)
# # all-MiniLM-L6-v2 is fast and good quality (~80MB)
# model <- st$SentenceTransformer("all-MiniLM-L6-v2")
# 
# # Get embeddings - works on single text or vector
# get_local_embedding <- function(text) {
#   model$encode(text, convert_to_numpy = TRUE)
# }
# 
# # Batch encode is much faster than one-at-a-time
# texts <- c("solar energy renewable power", "medical devices healthcare")
# embeddings <- model$encode(texts)  # Returns matrix: n_texts × 384 dimensions

# ch %>% filter(qg('gripple', CompanyName)) %>% View

sectordefs = read_csv('data/sectordefs/foursector_definitions.csv')

# Save reduced version, just two columns
write_csv(sectordefs %>% select(sector_name,description),'data/sectordefs/foursector_definitions_twocols.csv')


# Check batch outputs

readRDS('local/website_validatebatches/directguess_batch1') %>% View
readRDS('local/website_validatebatches/directguess_batch2') %>% View
readRDS('local/website_validatebatches/directguess_batch3') %>% View

table(!is.na(readRDS('local/website_validatebatches/directguess_batch1')$website))
table(!is.na(readRDS('local/website_validatebatches/directguess_batch2')$website))
table(!is.na(readRDS('local/website_validatebatches/directguess_batch3')$website))



# Some tests on samples to get set up----

# First, what's in one of the batch results?
# Ah - it recylced the 10 rows I put in and just ran it five times
# Hence why I used so much of the mojeev data
b = readRDS('local/website_validatebatches/backup/websitevalidate_batch11')

# So just checking on a FULL batch I'm just running...
# Yeah that looks a lot better!
fb = readRDS('local/website_validatebatches/websitevalidate_batch1')

# OK, let's use the sample to test the ML run while that's completing
# Pull out the first 10 from each batch, those ones should be valid
getfirst10 = function(filename){
  df = readRDS(filename)
  df %>% slice(1:10)
}
  
# filenames = list.files('local/website_validatebatches/backup', full.names = T)
# 
# samplebatch = map(filenames, getfirst10) %>% bind_rows()
# 
# # What's the hit rate? Probably a decent rep sample here
# # Not bad at all, 42%
# table(!is.na(samplebatch$website))
# table(!is.na(samplebatch$website)) %>% prop.table() * 100
# 
# saveRDS(samplebatch, 'local/samplebatch.rds')


# We got most of the results now, so that'll be more firms
# Let's try and use those... can then just run this to get all
# First entry is backup folder, don't keep that
filenames = list.files('local/website_validatebatches', full.names = T)
filenames = filenames[2:length(filenames)]

samplebatch = map(filenames, readRDS) %>% bind_rows()

# What's the hit rate? Probably a decent rep sample here
# Not bad at all, 42.7%
table(!is.na(samplebatch$website))
table(!is.na(samplebatch$website)) %>% prop.table() * 100

saveRDS(samplebatch, 'local/samplebatch2.rds')



# TEST PROCESS WEB VALIDATED FIRMS READY FOR SECTOR SCORES----

# Checking again after adding 'other' category to the sentence_transformer scorer

# Testing with the combined sample batch above, 740 firms
# This'll let us set everything up, test timings etc

samplebatch = readRDS('local/samplebatch2.rds')

# Python will want a combined 'site_text' from the about page, that's all I need to alter
samplebatch = samplebatch %>% 
  unite(site_text, c("main_text", "about_text"), remove = F) %>% 
  mutate(
    site_text = ifelse(site_text == 'NA_NA', NA, site_text)#fails to combine NAs properly
  )

# Check that made strings twice as long... tick
samplebatch %>% mutate(across(site_text:about_text, str_length))

# Keep only rows with data it can use
samplebatch.withsites = samplebatch %>% filter(!is.na(website))

# How many did each method get?
# Aroun 78% guess to mojeek 22%. Not a great loss if we don't spend money on that search.
# Should compare to brave, though pricing there for storage rules it out...
# 1565 firms is the final number with matching websites, out of 3661 firms
table(samplebatch.withsites$website_source) %>% prop.table() * 100

# Save as parquet to translate over to python
arrow::write_parquet(samplebatch.withsites,'local/samplebatch.parquet')

# test small sample
set.seed(67)
arrow::write_parquet(samplebatch.withsites %>% sample_n(10),'local/samplebatch_testsmall.parquet')

# Test processed...
# 315 firms in 2 seconds!
# Rerun with BAAI/bge-large-en-v1.5 - 1 minute for 315. Still fine.
sectorresult = arrow::read_parquet('local/samplebatch_firms_classified.parquet')
saveRDS(sectorresult,'local/sectorscores_sample.rds')


# CHECKS ON SAMPLE OF OUTPUT SECTOR SCORES----

# Rerunning for larger model

# From sector result above
sectorresult = readRDS('local/sectorscores_sample.rds')

# Let's just look at spread of values first.
# For which they'll need to be long.
# Before normalising them...
sl = sectorresult %>% 
  select(sim_health_tech:sim_other) %>% 
  pivot_longer(cols = sim_health_tech:sim_other, names_to = 'sector', values_to = 'score')

ggplot(sl, aes(x = score, y = sector)) +
  geom_jitter(height = 0.2, alpha = 0.5)

# Other basic checks
# Opposite sectors?
# They shouldn't necessary be negatively correlated...?
cor(sectorresult$sim_clean_energy, sectorresult$sim_not_clean_energy)
cor(sectorresult$sim_health_tech, sectorresult$sim_health_non_tech)

# All?
pairs(sectorresult %>% select(sim_health_tech:sim_other), panel = panel.smooth)

# Let's look at those for specific firms
ggplot(sectorresult, aes(x = sim_health_tech, y = sim_health_non_tech)) +
  geom_point()

ggplot(sectorresult, aes(x = sim_clean_energy, y = sim_not_clean_energy)) +
  geom_point()

ggplot(sectorresult, aes(x = sim_advanced_manufacturing, y = sim_manufacturing_not_advanced)) +
  geom_point()


# Testing versions where we find a difference score for those
# We can then look at a few...
sectorresult = sectorresult %>% 
  mutate(
    healthtech_diff = sim_health_tech - sim_health_non_tech,
    cleantech_diff = sim_clean_energy - sim_not_clean_energy,
    advmanuf_diff = sim_advanced_manufacturing - sim_manufacturing_not_advanced
  )

diffscores = sectorresult %>% 
  select(healthtech_diff:advmanuf_diff) %>% 
  pivot_longer(healthtech_diff:advmanuf_diff, names_to = 'sector', values_to = 'score')

ggplot(diffscores, aes(x = score, y = sector)) +
  geom_jitter(height = 0.2, alpha = 0.5) +
  geom_vline(xintercept = 0)

# Diff scores *by themselves* don't help - they'll tell us the health score diff
# For firms with no health connection

# Let's have a preview of what were highest scoring sectors, see if that gets anyhere close
sectorresult %>% filter(best_sector == 'health_tech') %>% arrange(desc(sim_health_tech)) %>% View

# Yeah as suspected, that's not useful in a context where we're not getting scores for all other sectors
# Using the raw scores might be a better approach, plus possibly a SetFit approach

# Attempt to make a scorer in the CSV that scores 'other' and can try to rule out being any of our other sectors
# Save all sector text as the source
# We'll have more of this soon, but...
# write_csv(sectorresult %>% select(CompanyName,site_text), 'local/allfirmwebtext.csv')
# write_csv(sectorresult %>% select(CompanyName,site_text), 'data/allfirmwebtext.csv')

# Now we've got 'other'...
# Simple test - exclude any whether 'other' score is higher
# Alternative: where 'other' score is some SD higher once normalised
othertest1 = sectorresult %>% 
  mutate(
    across(sim_health_tech:sim_manufacturing_not_advanced, ~. > sim_other, .names = "other_is_lower_{col}" )
  )

# How many does that exclude from each category?

# Check on specific sector...
healthtech = othertest1 %>% 
  filter(
    other_is_lower_sim_health_tech,
    best_sector == 'health_tech'
    ) %>% 
  arrange(desc(sim_health_tech))

cleantech = othertest1 %>% 
  filter(
    other_is_lower_sim_health_tech,
    best_sector == 'clean_energy'
    ) %>% 
  arrange(desc(sim_clean_energy))

advm = othertest1 %>% 
  filter(
    other_is_lower_sim_advanced_manufacturing,
    best_sector == 'advanced_manufacturing'
    ) %>% 
  arrange(desc(sim_advanced_manufacturing))

# Better! Not perfect. Some actual training might work better.

# Test manually marking successes and fails to help update learning
# sectorresult$manual_check = FALSE
# sectorresult.edit = sectorresult %>% select(-c(site_text,main_text,about_text))
# out <- data_edit(sectorresult.edit)



# EXPLORE MAP MAKING OPTIONS----

# Maybe use the advm result first as it looks pretty spot on. Can check for type 1/2 errors at some point.

# First, link back up with main SY CH file
sy = readRDS('local/sy_ch_PROCESSED_Dec2025.rds')

# Just keep matches for now
sy = sy %>% 
  inner_join(
    sectorresult %>% select(CompanyName,CompanyNumber,accountcode,website,website_source,sim_health_tech:advmanuf_diff),
    by = c('CompanyName','CompanyNumber','accountcode')
  )

# Some quick checks...
ggplot(sy, aes(x = sim_advanced_manufacturing, y = SIC_SECTION_NAME)) +
  geom_jitter(height = 0.2, alpha = 0.5)

# Or...
ggplot(
  sy %>% select(SIC_SECTION_NAME, sim_health_tech:sim_other) %>% 
    pivot_longer(cols = sim_health_tech:sim_other, names_to = 'sim_sector', values_to = 'score'),
  aes(x = score, y = SIC_SECTION_NAME)) +
  geom_jitter(height = 0.2, alpha = 0.5) +
  facet_wrap(~sim_sector)

# Oh of course it'll be the same number of firms in each... not the best way to do that



# Try various direct mappings before tidying
# Note, age of firm should prob correlate to advancedness? Hmm a bit
# cor(sy$age_of_firm_years,sy$sim_advanced_manufacturing)

p = tm_basemap("OpenStreetMap", alpha = 0.5) +
  tm_shape(sy, is.main = T) +
  tm_symbols(
    fill = 'sim_advanced_manufacturing',
    col = 'white',
    size = 0.5,
    fill.scale = tm_scale_continuous(values = "-matplotlib.rd_yl_gn")
    )

p

# Interactive...
tmap_mode('view')

# Just a selection of advanced manuf
tm_shape(sy %>% filter(CompanyNumber %in% advm$CompanyNumber), is.main = T) +
  tm_symbols(
    fill = 'sim_advanced_manufacturing',
    # fill = 'sim_clean_energy',
    # fill = 'sim_health_tech',
    col = 'white',
    size = 'Employees_thisyear',
    fill.scale = tm_scale_continuous(values = "-matplotlib.rd_yl_gn")
  )



# CHECK TESTFIT OUTPUT BEFORE PROVIDING PROPER EXAMPLES----

# Just trained on claude-provided diffs. Not enough of them really, but let's see.
testfit.result = arrow::read_parquet('local/samplebatch_setfit_classified.parquet')

tfl = testfit.result %>% 
  select(setfit_health_tech:setfit_advanced_manufacturing) %>% 
  pivot_longer(cols = setfit_health_tech:setfit_advanced_manufacturing, names_to = 'sector', values_to = 'score')

ggplot(tfl, aes(x = score, y = sector)) +
  geom_jitter(height = 0.2, alpha = 0.5)

pairs(
  testfit.result %>% 
    select(setfit_health_tech:setfit_advanced_manufacturing), 
  # filter_at(vars(starts_with('setfit')),  all_vars(. > 0.5))
  panel = panel.smooth)






