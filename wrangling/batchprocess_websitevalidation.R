# Go through a live companies house dataframe
# And for each firm, try and find a valid website
# Do this in two ways.
# First - guess website name. Faster, surprisingly accurate if trying several obvious guesses.
# Second - use mojeek's API to get top results and use those.
# If either having a matching postcode in the main, about or contact page, validate.
# And keep the main and about page text for using in sector_embeddings.py or the setfit scripts to get sector prob scores
# ---
# Save the results in batches in case anything breaks along the way
# Combine when finished
library(tidyverse)
library(sf)
library(furrr)
source('functions.R')
source('adhoc_functions.R')

# Set up parallel processing (uses all available cores minus one)
plan(multisession, workers = availableCores() - 1)

# Set up vars

batchsize = 50#test size for each batch
output_dir = 'local/website_validatebatches'

# Get latest data for south yorkshire
# filtered in @testcode/test_learning_newsectors.R
sy = readRDS('local/sy_ch_PROCESSED_Dec2025.rds')
# Just doing firms with 10+ employees for now
sy = sy %>% filter(Employees_thisyear >= 10)


# Process training sample - firms not in South Yorkshire
# Already filtered to 10+ employees
sy = readRDS('local/ch_trainingsample.rds')

# Test sample
# set.seed(67)
# sy = sy %>% sample_n(150)

# Check batch folder is made locally
# (i.e. won't be github-synced, but could be dropbox synced if running different batches)
if (!dir.exists(output_dir)) dir.create(output_dir)



# PROCESS EACH BATCH----

y = Sys.time()

batches <- sy %>%
  mutate(batchnum = ceiling(row_number() / batchsize)) %>%
  group_split(batchnum, .keep = FALSE)

cat('Number of batches: ', length(batches),'\n')

count <- 1

for (batch in batches) {
  
  # Test break
  # if(count == 4) break
  
  x = Sys.time()
  
  cat('Batch ', count, '\n')
  
  # Check batch doesn't already exist
  
  # filename = paste0(output_dir,'/websitevalidate_batch_',count)
  filename = paste0(output_dir,'/websitevalidate_batch_traininsample_',count)
  
  if(!file.exists(filename)){
  
  ## 1. Direct website guesses----
  
  cat('Direct website guesses...\n')
  
  batchresult <- future_map2(
    batch$CompanyName,
    batch$postcode,
    find_validated_website,
    max_candidates = 10,
    .progress = TRUE
  ) |> bind_rows()# to tibble
  
  # Add in company name, number for matching
  batchresult = batch %>%
    st_set_geometry(NULL) %>% 
    select(CompanyName,CompanyNumber,accountcode,localauthority_name,postcode) %>% 
  cbind(batchresult)
  
  # Add in flag to show where any website came from
  batchresult = batchresult %>% 
    mutate(
      website_source = ifelse(is.na(website), NA, 'direct_guess')
    )
  
  # 2. For any fails there, try mojeek----
  mojeek_candidates_added = get_mojeek_candidates_batch(
    batchresult %>% filter(is.na(website)),#Any with no match from stage 1
    n = 20)
  
  # Returns df with websites as lists in own column
  # mojeek_candidates_added$mojeek_candidates
  
  # Re-run validate website check
  validate_websites_mojeek <- future_pmap(
    list(
      company_name = mojeek_candidates_added$CompanyName,
      postcode = mojeek_candidates_added$postcode,
      candidates = mojeek_candidates_added$mojeek_candidates
    ),
    find_validated_website,
    max_candidates = 20,
    .progress = TRUE
  ) |> bind_rows()
  
  table(!is.na(validate_websites_mojeek$website))
  
  # Combine so matches correct firms
  mojeek_firms = batchresult %>% filter(is.na(website)) %>%
    select(-c(website,main_text,about_text)) %>% 
    cbind(validate_websites_mojeek)
  
  # Add in flag to show where any website came from
  mojeek_firms = mojeek_firms %>% 
    mutate(
      website_source = ifelse(is.na(website), NA, 'mojeek_search')
    )
  
  
  #Join both
  batchresultfinal = batchresult %>% 
    left_join(
      mojeek_firms %>% select(-localauthority_name,-postcode),
      by = c('CompanyName','CompanyNumber','accountcode')
    ) %>% 
    mutate(
      website = coalesce(website.x, website.y),
      main_text = coalesce(main_text.x, main_text.y),
      about_text = coalesce(about_text.x, about_text.y),
      website_source = coalesce(website_source.x, website_source.y)
      ) %>% 
    select(-contains(c('.x','.y')))
  
  saveRDS(batchresultfinal, filename)
  
  print('Batch time:')
  print(Sys.time() - x)
  
  } else {
    
    cat('Batch file already present, moving on.\n')
    
  }
  
  count <- count + 1
  
}

print('Total time:')
print(Sys.time() - y)







