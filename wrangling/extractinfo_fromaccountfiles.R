#Extract information from account files - store results in single saved file
#NOTE: it's possible to access account files directly from the zips, but it's much slower doing it that way
#For extracting from national data, better/faster to unzip each file then delete after extraction, and repeat
library(tidyverse)
library(furrr)
library(progressr)
source('functions.R')

#Set up parallel plan
plan(multisession) # or multicore on Unix/Mac

#Set up progressr handler
handlers(global = TRUE)
handlers("txtprogressbar")  # or "progress" for a nicer one



#Create save folder if it's not present already
if (!dir.exists('local/account_extracts')) dir.create('local/account_extracts')

#Downloaded in download_monthlyaccounts.R
allzips <- list.files('local/monthly_companieshouse_accounts', '*.zip', full.names = T)

#ALso get any filenames present for the saved rds extraction result
#In case the loop needs to be re-run for any reason, save faffing about
#(Delete files if they want replacing)
extracted_files <- list.files('local/account_extracts', '*.rds', full.names = F) %>% gsub('.rds','',.)

#It'll be doing this in alphabetical not date order
#That doesn't matter, dates are processed later
for(zipname in allzips){
  
  #Check if extract has already happened before continuing
  if(!basename(zipname) %>% gsub('.zip','',.) %in% extracted_files){
  
    #Unzip each in turn, extract all info for each, store
    #Then delete each in turn
    #(Av of 3 to 6 mins per unzip?)
    x <- Sys.time()
    cat('Unzipping', zipname,'...\n')
    unzip_result <- unzip(zipname,exdir=gsub('.zip','',zipname))
    print(Sys.time()-x)

    #unzip() returns silently on a corrupt/truncated archive (extracts nothing),
    #which later surfaces as a confusing str_split subscript error. Catch it here
    #and skip this zip so the rest of the loop can continue.
    if(length(unzip_result) == 0){
      warning('unzip produced no files - archive likely corrupt/truncated, skipping: ', zipname)
      next
    }


    #Get company numbers from inside the unzipped folder - each filename has company number
    
    #Will need this later to delete
    extractedaccounts_location <- paste0('local/monthly_companieshouse_accounts/',
                                         gsub('.zip','',basename(zipname)))
    
    allaccounts <- list.files(extractedaccounts_location, full.names = T)
    
    #remove any nested zip files for now - these are CICs.
    #todo: process CIC zip files
    allaccounts <- allaccounts[!qg('.zip',allaccounts)]
    
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
      accountcode = str_split(Name, "_", simplify = TRUE)[, 4] %>% gsub('.html','',.),
      accountpresent = accountcode != ''#Occasional acccounts with nothing in, need to filter those out
    )
    
    # table(accounts$accountpresent)
    
    #remove any rows from both allaccounts file locations 
    #(Has only happened once for one account file so far but...)
    #and accounts tibble
    #Order will remain same
    allaccounts <- allaccounts[accounts$accountpresent]
    accounts <- accounts %>% filter(accountpresent)
    
    cat('Extracting account info for',length(allaccounts),'accounts ...\n')
    
    #Extract data from each account, put into dataframe
    # account.extract <- 
    #   # map(allaccounts[1:1000],#test with small number
    #   map(allaccounts,
    #       get_accounts_data,
    #       # ziplocation = zipname,#Only if extracting directly from zip
    #       .progress = T) %>% bind_rows
    
    #Parallel version
    account.extract <- with_progress({
      # future_map(allaccounts[1:1000],#test with small number
      future_map(allaccounts,
                 get_accounts_data,
                 .progress = TRUE) %>% bind_rows()
    })
    
    
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
  
  } else {#end if test of already extracted file
   
    cat(basename(zipname),'already extracted from, moving on...\n')
    
  }
  
}

