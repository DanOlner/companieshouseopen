# Mojeek website candidate search
# Paid API, £3 per 1000 queries
library(httr2)
library(jsonlite)
library(tidyverse)

# TEST----


# API key set via keyring - no plain text anywhere
# keyring::key_set("MOJEEK_KEY")
mojeek_search_urls <- function(q, api_key, n = 10) {
  req <- request("https://www.mojeek.com/search") |>
    req_url_query(
      api_key = api_key,
      q = q,
      t = n,            # top N
      fmt = "json",
      rb = "GB", rbb = 10,
      lb = "EN", lbb = 100,
      clufmt = 1,       # 1 result per hostname (per 10 results)
      fe = ".companieshouse.gov.uk,.linkedin.com,.facebook.com,.1stdirectory.co.uk,.yell.com,.ukbizdb.co.uk,.companydirectorcheck.com,.companydirectorcheck.com,.bigreddirectory.com,.uksmallbusinessdirectory.co.uk,.locallinkup.com,.jobsxl.co.uk,.essentialrecruitment.co.uk,.opencorporates.com"
    )
  
  resp <- req |> req_perform() |> resp_body_string()
  js <- fromJSON(resp)
  
  # Extract URL candidates
  urls <- js$response$results$url
  unique(urls)
}

# Pick one we know has website
# But that website guessing didn't capture

# From some I made earlier...
combo = readRDS('local/testing_firmwebsites.rds')

# Combos that had no results
combonegs = combo %>% filter(is.na(validated_website_final))

# Now this one was tricky...
candidatefirm = combonegs %>% filter(qg('HAYWOOD AND PADGETT LIMITED',CompanyName))

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

# And...
x = find_validated_website(
  candidatefirm$CompanyName, 
  candidatefirm$postcode, 
  candidates = base_results, 
  max_candidates = 10, verbose = T
)

# Checking behaviour on known success








