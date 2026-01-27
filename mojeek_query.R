# Mojeek website candidate search
# Paid API, £3 per 1000 queries
library(httr2)
library(jsonlite)
library(tidyverse)
library(furrr)
source('adhoc_functions.R')

# Set up parallel processing (uses all available cores minus one)
plan(multisession, workers = availableCores() - 1)


# TEST----



# Pick one we know has website
# But that website guessing didn't capture

# From some I made earlier...
combo = readRDS('local/testing_firmwebsites.rds')

# Combos that had no results
combonegs = combo %>% filter(is.na(validated_website_final))

# A known yes
candidatefirm = combonegs %>% filter(qg('HAYWOOD AND PADGETT LIMITED',CompanyName))
# A known unknown!
# candidatefirm = combonegs %>% filter(qg('player',CompanyName))

searchstring = paste0(candidatefirm$CompanyName, " ", candidatefirm$localauthority_name)

mojeek_result = mojeek_search_urls(searchstring, keyring::key_get("MOJEEK_KEY"), 10)

# Keep only base URLs
base_results = urltools::suffix_extract(urltools::domain(mojeek_result)) %>% select(host) %>% pull



# I know number three here should be a match...
# And the about page does have the correct postcode, as does the front page

# Just confirm 'guess website' doesn't work
x = find_validated_website(
  candidatefirm$CompanyName, 
  candidatefirm$postcode, 
  # candidates = base_results, #NULL here defaults to 'guess website'
  max_candidates = 10, verbose = T
)

# And...yep! Huzzah.
x = find_validated_website(
  candidatefirm$CompanyName, 
  candidatefirm$postcode, 
  candidates = base_results, 
  max_candidates = 10, verbose = T
)

# So now test on ALL firms here with no guess result
# Let's do it separately, as we want to be able to parallelise the web search
# And that might hit mojeek rate limits if it's all together

# Mojeek rate limit on my account is 10 per second, so let's keep to nine
testfirms = combonegs %>% slice(1:50)

results = get_mojeek_candidates_batch(testfirms)

# Returns df with results as lists in own column
# results$mojeek_candidates

validate_results <- future_pmap(
  list(
    company_name = results$CompanyName,
    postcode = results$postcode,
    candidates = results$mojeek_candidates
  ),
  find_validated_website,
  max_candidates = 10,
  .progress = TRUE
) |> bind_rows()

# That isn't maybe a huge addition to the number of hits is it?
# I suppose an extra 20% not to be sniffed at
table(!is.na(validate_results$website))

# We could see if Brave does any better?
# https://api-dashboard.search.brave.com/documentation/pricing
# Hmm, cost of rights to store data are not cheap




# Out of interest, do we get a better hit rate if we use all 40 possible results?
# Or even 20?
# Hmm the number it returns is arbitrary, slightly.
results2 = get_mojeek_candidates_batch(testfirms, n = 20)

# Returns df with results2 as lists in own column
results2$mojeek_candidates

validate_results2 <- future_pmap(
  list(
    company_name = results2$CompanyName,
    postcode = results2$postcode,
    candidates = results2$mojeek_candidates
  ),
  find_validated_website,
  max_candidates = 20,
  .progress = TRUE
) |> bind_rows()

# Ooo up to 37% extra.
table(!is.na(validate_results2$website))




















