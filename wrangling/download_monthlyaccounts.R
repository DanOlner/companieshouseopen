#Grab all current monthly account files for the last year from Companies House
#Check if any already present on local drive
library(tidyverse)
library(xml2)




# Load the HTML file
doc <- read_html("https://download.companieshouse.gov.uk/en_monthlyaccountsdata.html")

# Extract all <a> tags with hrefs containing "Accounts_Monthly_Data"
zip_links <- xml_attr(
  xml_find_all(doc, "//a[contains(@href, 'Accounts_Monthly_Data') and contains(@href, '.zip')]"),
  "href"
)

#CHECK IF ANY EXISTING FOLDERS ALREADY DOWNLOADED, ONLY GET NEW ONES
existing_zips <- list.files('local/monthly_companieshouse_accounts', full.names = F)

#Testing for zips
zip_links <- zip_links[!zip_links %in% existing_zips]

#Append those to the web URL for download
zip_links_urls <- paste0('https://download.companieshouse.gov.uk/', zip_links)

# Define the folder to save the ZIP files
output_folder <- "local/monthly_companieshouse_accounts"

#Also, create it if it's not present already
if (!dir.exists('local/monthly_companieshouse_accounts')) dir.create('local/monthly_companieshouse_accounts')

# Set the full path to save the file
file_path <- file.path(output_folder, zip_links)



#DOWNLOAD ZIPS LOCALLY
#Set global timeout to very bigly indeed
#ten hours per file should do it!
#Max download time so far is ~12 mins, av ~6 mins.
options(timeout = 36000)

for(i in 1:length(zip_links)){
  
  cat('Starting',i,'out of',length(zip_links),', file:',file_path[i],'\n')
  
  x <- Sys.time()
  
  #Download a single file - several GB
  tryCatch({
    
    download.file(zip_links_urls[i], file_path[i], mode = "wb")  # Binary mode for ZIP files
    
    message(paste("Downloaded"))
  }, error = function(e) {
    message(paste("Failed to download", "Error:", e$message))
  })
  
  print(Sys.time()-x)
  
  # print("Unzipping...")
  
  # unzip(paste0(output_folder,'/',zip_links[i]),exdir=paste0(output_folder,'/',gsub('.zip','',zip_links[i])))
  
  cat('Finished everything for',i,'out of',length(zip_links),', file:',file_path[i],'\n')
  print(Sys.time()-x)
  
}

