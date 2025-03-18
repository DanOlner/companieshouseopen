#Extract information from account files - store results in single saved file
library(tidyverse)

#Create save folder if it's not present already
if (!dir.exists('local/account_extracts')) dir.create('local/account_extracts')

#Downloaded in download_monthlyaccounts.R
allzips <- list.files('local/monthly_companieshouse_accounts', '*.zip', full.names = T)



#It'll be doing this in alphatical not date order
#That doesn't matter, dates are processed later
for(zipname in allzips){
  
  
  #Unzip each in turn, extract all info for each, store
  #Then delete each in turn
  #(Av of 3 to 6 mins per unzip?)
  x <- Sys.time()
  cat('Unzipping', zipname,'...\n')
  unzip(zipname,exdir=gsub('.zip','',zipname))
  print(Sys.time()-x)
  
  
  #Get company numbers from inside the unzipped folder - each filename has company number
  
  #Will need this later to delete
  extractedaccounts_location <- paste0('local/monthly_companieshouse_accounts/',
                                       gsub('.zip','',basename(zipname)))
  
  allaccounts <- list.files(extractedaccounts_location, full.names = T)
  
  #(For getting names if extracting directly from unzipped folder, returning list)
  # zip_contents <- unzip(zipname, list = TRUE)
  # accounts <- zip_contents %>% 
  #   mutate(
  #     companynumber = str_split(Name, "_", simplify = TRUE)[, 3]
  #   ) 

  #Get same-ordered data to add in once extracted...  
  accounts <- tibble(
    Name = basename(allaccounts),
    companynumber = str_split(Name, "_", simplify = TRUE)[, 3],
    accountcode = str_split(Name, "_", simplify = TRUE)[, 4] %>% gsub('.html','',.)
  )
  
  cat('Extracting account info...\n')
  
  #Extract data from each account, put into dataframe
  account.extract <- 
    # map(allaccounts[1:1000],#test with small number
    map(allaccounts,
        get_accounts_data,
        # ziplocation = zipname,#Only if extracting directly from zip
        .progress = T) %>% bind_rows
  
  
  #Add in company number / account number (same order)
  account.extract <- account.extract %>% 
    # bind_cols(accounts %>% select(companynumber,accountcode) %>% slice(1:1000)) %>% #test 1000
    bind_cols(accounts %>% select(companynumber,accountcode)) %>%
    relocate(companynumber, .after = Company) %>% 
    relocate(accountcode, .after = companynumber)
    
  cat('Saving RDS...\n')
  
  saveRDS(account.extract,paste0('local/account_extracts/',gsub('.zip','',basename(zipname)),'.rds'))
  
  cat('Deleting previous unzipped accounts...\n')
  
  #Delete unzipped folder and contents
  #https://stackoverflow.com/a/65831178/5023561
  unlink(extractedaccounts_location, recursive = TRUE, force = TRUE)
  
  print(paste0('Done ',zipname))
  print(Sys.time() - x)
  
}

