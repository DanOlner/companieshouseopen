#Companies house bulk download (~3gb)
#Monthly updates from here:
#https://www.gov.uk/guidance/companies-house-data-products
library(tidyverse)
library(sf)
library(tmap)
library(pryr)
library(xml2)
source('functions.R')
#library(data.table)

# 1. COMPANIES HOUSE FULL UK COMPANIES LIST

## LOAD----

# ch <- read_csv('localdata/BasicCompanyDataAsOneFile-2024-11-01.csv')
# 
# #check match against SY place names... this is more than we get against a postcode match below. Keep town name.
# table(grepl('sheffield|rotherham|doncaster|barnsley',ch$RegAddress.PostTown,ignore.case = T))
# 
# #reduce to only the columns we need for now and resave for ease of loading
# ch <- ch %>% 
#   select(CompanyName,CompanyNumber,RegAddress.PostCode,RegAddress.PostTown,CompanyCategory,CompanyStatus,CountryOfOrigin,IncorporationDate,SICCode.SicText_1:SICCode.SicText_4,URI)
# 
# #3.8gb in memory before col select, 2gb after
# object_size(ch)
# 
# saveRDS(ch,'localdata/BasicCompanyDataAsOneFile-2024-11-01_reducedcols.rds')

ch <- readRDS('localdata/BasicCompanyDataAsOneFile-2024-11-01_reducedcols.rds')

#Does every business have postcodes? Not quite
table(!is.na(ch$RegAddress.PostCode))

#Check on some that don't...
ch %>% filter(is.na(RegAddress.PostCode)) %>% slice_sample(n = 100) %>% View

#Check on those that do!
ch %>% filter(!is.na(RegAddress.PostCode)) %>% slice_sample(n = 100) %>% Viewr



## USE POSTCODE LOOKUP TO FILTER DOWN TO SOUTH YORKSHIRE----

#NOTE: USING LATEST CODEPOINT OPEN INSTEAD, SEE BELOW
#BETTER MATCH
#PROCESSED HERE: 
#https://github.com/DanOlner/utilities/blob/master/postcodes_to_localauthorities_makelookup.R

#"Postcode (May 2021) to OA (2021) to LSOA to MSOA to LTLA to UTLA to RGN to CTRY (May 2021) Best Fit Lookup in EW"
#"Postcodes are best-fitted by plotting the location of the postcode's mean address into the areas of the output geographies."
#Yep that'll do.
#https://geoportal.statistics.gov.uk/datasets/ons::postcode-may-2021-to-oa-2021-to-lsoa-to-msoa-to-ltla-to-utla-to-rgn-to-ctry-may-2021-best-fit-lookup-in-ew/about
# pclookup <- read_csv("localdata/pcd_oa_lsoa_msoa_ltla_utla_rgn_ctry_ew_may_2021_lu/pcd_oa_lsoa_msoa_ltla_utla_rgn_ctry_ew_may_2021_lu_v2.csv")

#Local/unitary authorities
#unique(pclookup$ltla22nm)
#unique(pclookup$utla22nm)

#The four LAs in South Yorkshire then... tick
# unique(pclookup$ltla22nm)[grepl('sheffield|rotherham|doncaster|barnsley',unique(pclookup$ltla22nm),ignore.case = T)]

#Use those to match again CH postcodes to extract just South Yorkshire businesses
#47K postcodes
# sy.pclookup <- pclookup %>% filter(grepl('sheffield|rotherham|doncaster|barnsley',ltla22nm,ignore.case = T))

#Both have same capital / space format, should have good match numbers...
# table(sy.pclookup$pcd %in% ch$RegAddress.PostCode)

#Check whether postcode match changes if formats matched




#Try with actual codepoint open latest file - the above is 2021, it's missing some latest
#Though some others might drop out, so let's see...
#Located / made here? 
#YPERN/R/utilities/local/postcode_localauthority_lookup_2024-11-20.csv
#257mb
pc <- read_csv('https://www.dropbox.com/scl/fi/0873bsx2ka1d3clhs8e07/postcode_localauthority_lookup_2024-11-20.csv?rlkey=rtda1srm2nlzmqxc2ms6fllno&dl=1')

#check LA match here... tick
pc$localauthority_name[grepl('sheffield|rotherham|doncaster|barnsley',pc$localauthority_name,ignore.case = T)] %>% unique

#That's fewer postcodes than the one above...
sy.pc <- pc %>% filter(grepl('sheffield|rotherham|doncaster|barnsley',localauthority_name,ignore.case = T))


#How many SY business matches do we get? 53217. Fewer than I'm getting in FAME.
# table(ch$RegAddress.PostCode %in% sy.pclookup$pcd)

#How many from latest codepoint open, assuming the LA matches above are actually getting all of SY?
#75740 - BETTER!! OK then.
table(ch$RegAddress.PostCode %in% sy.pc$Postcode)



#Reasonably close match on the number here, but might be different firms
table(grepl('sheffield|rotherham|doncaster|barnsley',ch$RegAddress.PostTown,ignore.case = T))

#Are those overlapping firms though? Do we get either:
#SY postcodes NOT in the place name search?
#Place names NOT in the SY postcode search?

#Get both subsets and check
sy.viapostcode <- ch %>% filter(RegAddress.PostCode %in% sy.pc$Postcode)
sy.viaplacename <- ch %>% filter(grepl('sheffield|rotherham|doncaster|barnsley',RegAddress.PostTown,ignore.case = T))

#Check on matching company name
#How many via postcode in via placename? (Same number of matches will of course come up in both directions)
#Actually, different firms- about 4000 don't match
table(sy.viapostcode$CompanyName %in% sy.viaplacename$CompanyName)

#So that's 49892 common via both methods
#Leaving a good chunk of falses in both that don't match

#Let's look at those. 4200 of these
#This is MATCHES ON POSTCODE WHERE WE COULDN'T GET LOCAL AUTHORITY NAME MATCH
#That's a lot of named places in SY (including many "South Yorkshire")
#And typos e.g. SHEFFILED
#So let's trust the postcodes...
sy.viapostcode[!sy.viapostcode$CompanyName %in% sy.viaplacename$CompanyName,] %>% View

#Then... what about place names we have that postcode matching didn't get?
#First one checked - S4 7AA - is new (postcode lookup is 2021 I think)
#Some others are beyond SY boundaries e.g. Eckington
sy.viaplacename[!sy.viaplacename$CompanyName %in% sy.viapostcode$CompanyName,] %>% View

#Conclusion: non postcode matches are probably going to be beyond SY, mostly


#KEEP ONLY SY COMPANIES HOUSE POSTCODE MATCHES
#AND GEOCODE THEM
ch.geo <- ch %>%
  rename(Postcode = RegAddress.PostCode) %>% 
  left_join(
    sy.pc %>% select(Postcode,localauthority_code,localauthority_name,Eastings,Northings),
    by = 'Postcode'
  ) %>% 
  filter(!is.na(Eastings)) %>% #will be some missing due to rogue postcodes
  st_as_sf(coords = c("Eastings", "Northings"), crs = 27700)

#Save to look at in QGIS... yep all looking great
st_write(ch.geo, 'localdata/QGIS/companieshouse_southyorkshire_geo.shp')

#Save as RDS for use elsewhere via github
saveRDS(ch.geo,'data/companieshouse_southyorkshire_geopoints.rds')

ch.geo <- readRDS('data/companieshouse_southyorkshire_geopoints.rds')




# 2. EXTRACTING FROM COMPANIES HOUSE ACCOUNTS FILES----

## Start by testing with a single one. Using iXBRL format----

#Downloaded entire accounts files, cos scraping would take longer (though that might be an option too)
#Via here: https://download.companieshouse.gov.uk/en_accountsdata.html
#Each file is monthly accounts
#"Data is only available for electronically filed accounts, which currently stands at about 75% of the 2.2 million accounts we expect to be filed each year."

# Load a file I know has got employee counts in
# doc <- read_xml("localdata/Accounts_Bulk_Data-2024-11-19/Prod223_3832_00056710_20240430.html")
# 
# doc <- read_xml("localdata/Accounts_Bulk_Data-2024-11-19/Prod223_3832_14688350_20240331.html")
# 
# # Extract company name
# company_name <- xml_text(xml_find_first(doc, "//ix:nonNumeric[@name='bus:EntityCurrentLegalOrRegisteredName']"))
# company_name <- xml_text(xml_find_first(doc, "//ix:nonNumeric[@name='frs-bus:EntityCurrentLegalOrRegisteredName']"))
# 
# #Testing contains method for matching various
# company_name <- xml_text(
#   xml_find_first(
#     doc,
#     "//ix:nonNumeric[contains(@name, 'EntityCurrentLegalOrRegisteredName')]",
#     ns = c(ix = "http://www.xbrl.org/2013/inlineXBRL")
#   )
# )
# 
# 
# # Extract employee numbers for 2024 and 2023
# employees_2024 <- xml_text(xml_find_first(doc, "//ix:nonFraction[@name='core:AverageNumberEmployeesDuringPeriod' and @contextRef='D0']"))
# employees_2023 <- xml_text(xml_find_first(doc, "//ix:nonFraction[@name='core:AverageNumberEmployeesDuringPeriod' and @contextRef='D11']"))
# 
# print(paste("Company:", company_name, "| Employees (2024):", employees_2024, "| Employees (2023):", employees_2023))
# 
# 
# #Test function version... tick
# debugonce(get_accounts_data)
# get_accounts_data('localdata/Accounts_Bulk_Data-2024-11-19/Prod223_3832_00056710_20240430.html')
# 
# get_accounts_data('localdata/Accounts_Bulk_Data-2024-11-19/Prod223_3832_14688350_20240331.html')
# 
# get_accounts_data('localdata/Accounts_Bulk_Data-2024-11-19/Prod223_3832_02641794_20240630.html')



  #OK, now check match of accounts versus SY businesses
#(Can check against all UK businesses later)

#File names have company number and then filing date
#Can separate thus
#This is the latest, only 16K account filings
accounts <- list.files("~/localdata/Accounts_Bulk_Data-2024-11-19",'*.html', full.names = T) %>%
  as_tibble() %>%
  rename(filelocation = value)

#Add on shorter filename for ease of processing, extract company number
accounts <- accounts %>% 
  mutate(
    value = basename(filelocation),#pull out just short final name
    companynumber = str_split(value, "_", simplify = TRUE)[, 3]
  ) %>% 
  select(-value)


#OK, check if any accounts matches this month in SY
table(accounts$companynumber %in% ch.geo$CompanyNumber)

#Yes - let's just view those
ch.geo %>% filter(CompanyNumber %in% accounts$companynumber) %>% View



#Test how many of these we can extract employee number from. Function that up.
#Just for South Yorkshire for now
employee.numbers <- map(accounts$filelocation[accounts$companynumber %in% ch.geo$CompanyNumber],get_accounts_data, .progress = T) %>% bind_rows

#Add back in company numbers
employee.numbers$companynumber <- accounts$companynumber[accounts$companynumber %in% ch.geo$CompanyNumber]


#Check on those not being picked up - what's happening with those accounts?
#Example: 02641794
#OK, fixed that - we now have all firm names being extracted
#Some still with NA employee number - let's just check the originals don't have them
#E.g. 02309294 "LJT Motors Limited"... yep no employee values
#Or 14771941 "ARCHERS INVESTMENT COMPANY LIMITED"... "Dormant accounts"
#For which there's an xml field, so let's try and get that... ah there's a lot of dormant companies, OK
#https://www.yourcompanyformations.co.uk/blog/dormant-company-explained/

#remaining ones without employee counts, the script has been scrambled and is unreadable 
#But note: we're getting MUCH higher positive values than FAME has (which is about 50%)
#Look:
table(!is.na(employee.numbers$Employees_thisyear[employee.numbers$dormantstatus=='false'])) %>% prop.table


#The issue might be the ~25% who don't file electronic accounts, among other things

#Just check with a few samples that the order of employee year is correct
#Pick to look at...
#OK LOOKING GOOD
View(employee.numbers %>% filter(dormantstatus == 'false'))
#10634903 UK200: tick
#09038383 Quando: tick
#09735145 pet repair: tick
#05880174 H & E Electrical Services Limited: tick
#08421033 Mexborough mini market: tick



#REPEAT FOR SEPTEMBER 2024 MONTHLY DATA----

#To check computational demand etc
#192K entries in a month ... err now somehow gone up to 359K on a second run
#Think OS must still have been indexing?
#Stored locally only
accounts <- list.files("~/localdata/monthly_companieshouse_accounts/Accounts_Monthly_Data-September2024",'*.html', full.names = T) %>%
  as_tibble() %>%
  rename(filelocation = value)

#Add on shorter filename for ease of processing, extract company number
accounts <- accounts %>% 
      mutate(
        value = basename(filelocation),#pull out just short final name
        companynumber = str_split(value, "_", simplify = TRUE)[, 3]
      ) %>% 
  select(-value)


#OK, check if any accounts matches this month in SY - yes, 4841
table(accounts$companynumber %in% ch.geo$CompanyNumber)

#Yes - let's just view those
# ch.geo %>% filter(CompanyNumber %in% accounts$companynumber) %>% View

#Extract employee numbers from accounts files
employee.numbers <- map(accounts$filelocation[accounts$companynumber %in% ch.geo$CompanyNumber],get_accounts_data, .progress = T) %>% bind_rows

#Add back in company numbers
employee.numbers$companynumber <- accounts$companynumber[accounts$companynumber %in% ch.geo$CompanyNumber]

#Check what proportion of firms we got employees for here... 99%, bonza
table(!is.na(employee.numbers$Employees_thisyear[employee.numbers$dormantstatus=='false'])) %>% prop.table

employee.numbers %>% filter(dormantstatus == 'false') %>% View

#Note: ENTRY MISTAKES E.G. THE TOP LISTED EMPLOYEE COUNT IS ACTUALLY THE INCOME - 147K.
#Watch out for those.



#Test version extracting straight from zip file rather than unzipping first
zip_contents <- unzip("~/localdata/monthly_companieshouse_accounts/Accounts_Monthly_Data-September2023.zip", list = TRUE)

accounts <- zip_contents %>% 
  mutate(
    companynumber = str_split(Name, "_", simplify = TRUE)[, 3]
  ) 

#A LOT slower, but not stupidly so and a lot more HD-sane
#Bear in mind this is just for SY though - national would be something else, probably better to do on unzipped files and slice up differently
#As each unzip would contain national accounts
employee.numbers <- 
  map(accounts$Name[accounts$companynumber %in% ch.geo$CompanyNumber],
      get_accounts_data,
      ziplocation = '~/localdata/monthly_companieshouse_accounts/Accounts_Monthly_Data-September2023.zip',
      .progress = T) %>% bind_rows

#Add back in company numbers
employee.numbers$companynumber <- accounts$companynumber[accounts$companynumber %in% ch.geo$CompanyNumber]

#Check what proportion of firms we got employees for here... 99%, bonza
table(!is.na(employee.numbers$Employees_thisyear[employee.numbers$dormantstatus=='false'])) %>% prop.table

employee.numbers %>% filter(dormantstatus == 'false') %>% View










## Work on bulk account downloading for processing all accounts for the last year----

#Which in theory should give us all live accounts given yearly submission necessary, but let's see

#First - test whether I actually need to unzip to access the individual files
#Might be easier to avoid
#Use list arg to just get names... wow, fast
zip_contents <- unzip("~/localdata/monthly_companieshouse_accounts/Accounts_Monthly_Data-September2023.zip", list = TRUE)

#Test getting one file read int read_xml... tick
doc <- read_xml(unz("~/localdata/monthly_companieshouse_accounts/Accounts_Monthly_Data-September2023.zip", zip_contents$Name[1]))




# Load the HTML file
doc <- read_html("https://download.companieshouse.gov.uk/en_monthlyaccountsdata.html")

# Extract all <a> tags with hrefs containing "Accounts_Monthly_Data"
zip_links <- xml_attr(
  xml_find_all(doc, "//a[contains(@href, 'Accounts_Monthly_Data') and contains(@href, '.zip')]"),
  "href"
)

#CHECK IF ANY EXISTING FOLDERS ALREADY DOWNLOADED, ONLY GET NEW ONES
# existing_folders <- list.dirs('~/localdata/monthly_companieshouse_accounts', full.names = F)
existing_zips <- list.files('~/localdata/monthly_companieshouse_accounts', full.names = F)

#These folders are already present and unzipped
# zip_links[!gsub('.zip','',zip_links) %in% existing_folders]

#These are the ones to keep and download
# zip_links[!gsub('.zip','',zip_links) %in% existing_folders]

# zip_links <- zip_links[!gsub('.zip','',zip_links) %in% existing_folders]

#Testing for zips
zip_links <- zip_links[!zip_links %in% existing_zips]

#Append those to the web URL for download
zip_links_urls <- paste0('https://download.companieshouse.gov.uk/', zip_links)

# Define the folder to save the ZIP files
output_folder <- "~/localdata/monthly_companieshouse_accounts"

# Set the full path to save the file
file_path <- file.path(output_folder, zip_links)




#Download...
#Not unzipping - pulling files directly from the zips to save on disk space (slower but more efficient for HD)
#THIS IS GOING TO TAKE SOME HOURS ON FIRST RUN!

# Create a handle with custom options
# handle <- curl::new_handle()
# curl::handle_setopt(handle, timeout = 0)  # Set timeout to 0 (infinite)

# Download the file using the handle
# curl_download("https://example.com/file.zip", "file.zip", handle = handle)

#Set global timeout to very bigly indeed
options(timeout = 36000)#ten hours per file, belt and braces!

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


#Test download with smaller daily file
# tryCatch({
#   download.file("https://download.companieshouse.gov.uk/Accounts_Bulk_Data-2024-11-16.zip", paste0(output_folder,'/test.zip'), mode = "wb")  # Binary mode for ZIP files
#   message(paste("Downloaded:", file_name))
# }, error = function(e) {
#   message(paste("Failed to download:", file_name, "Error:", e$message))
# })
# 
# unzip(paste0(output_folder,'/test.zip'),exdir=paste0(output_folder,'/test'))






#QUICK AND DIRTY GRAB OF ALL EMPLOYEE NUMBER / DORMANT DETAILS FOR THE LAST YEAR FOR SOUTH YORKSHIRE----

#So I can get on with playing with it. Mull deeper / further processing as we go

#Everage of 20 mins per file, largest december one took an hour
#Doing directly from zip muuuuch slower, don't repeat for national processing!

#Get all zip file locations
#Run each
#Extract then save, can combine after running

allzips <- list.files('~/localdata/monthly_companieshouse_accounts', '*.zip', full.names = T)

# results <- list()

for(zipname in allzips){
  
  x <- Sys.time()
  
  #Test version extracting straight from zip file rather than unzipping first
  zip_contents <- unzip(zipname, list = TRUE)
  
  accounts <- zip_contents %>% 
    mutate(
      companynumber = str_split(Name, "_", simplify = TRUE)[, 3]
    ) 
  
  #Get just for SOUTH YORKSHIRE
  #A LOT slower, but not stupidly so and a lot more HD-sane
  #Bear in mind this is just for SY though - national would be something else, probably better to do on unzipped files and slice up differently
  employee.numbers <- 
    map(accounts$Name[accounts$companynumber %in% ch.geo$CompanyNumber],
        get_accounts_data,
        ziplocation = zipname,
        .progress = T) %>% bind_rows
  
  #Add back in company numbers
  employee.numbers$companynumber <- accounts$companynumber[accounts$companynumber %in% ch.geo$CompanyNumber]
  
  #store
  # results[[length(results)+1]] <- employee.numbers
  
  saveRDS(employee.numbers,paste0('localdata/SouthYorkshire_accounts_saves/',gsub('.zip','',basename(zipname)),'_SY.rds'))
  
  print(paste0('Done ',zipname))
  print(Sys.time() - x)

}






# COMBINE / EXAMINE SOUTH YORKSHIRE EMPLOYEE NUMBER DATA----

#Reload south Yorkshire employee data extracted from accounts files, compile into one
employee.numbers.all <- map(list.files(path = 'localdata/SouthYorkshire_accounts_saves', pattern = '*.rds', full.names = T), readRDS) %>% bind_rows()

#NOTE: companies that do have more than one year's accounts picked up here e.g. gripple, we've got the beginnings of an employee count time series there. Can report this as exactly what it is "companies most recent and last year employee count from accounts submitted to CH".
#Number this is possible for may not be very high, but it's doable for a proportion

#51969 rows
#How many unique company numbers? 46138
length(unique(employee.numbers.all$companynumber))

#That's a lot of businesses not submitting electronic accounts in the last year
#Still, that's -->
#60% of firms, so that's starting to look more like the FAME numbers (though that's 50% so slight improvement)
(length(unique(employee.numbers.all$companynumber)) / nrow(ch.geo)) * 100

#Let's try and get those random dates all parsed
#test which does the better job... tick!
# lubridate::parse_date_time(employee.numbers$enddate, orders = c("dmy","ymd"))

employee.numbers.all <- employee.numbers.all %>% 
  mutate(
    enddate_formatted = lubridate::parse_date_time(enddate, orders = c("dmy","ymd"))
  ) 

#Got every single date
table(is.na(employee.numbers.all$enddate_formatted))

#Check employee number counts
#Keep only non dormant firms for this
nond <- employee.numbers.all %>% filter(dormantstatus == 'false')

table(is.na(nond$Employees_thisyear))
table(is.na(nond$Employees_lastyear))

#And both, just to check... 35K firms with records for this and last year for employee counts
table(!is.na(nond$Employees_lastyear) & !is.na(nond$Employees_thisyear))

#When are accounts mostly submitted?

#https://stackoverflow.com/a/66583089/5023561
by_month <- function(x,n=1){
  seq(min(x,na.rm=T),max(x,na.rm=T),by=paste0(n," months"))
}

ggplot(nond,aes(x = enddate_formatted)) +
  geom_histogram(breaks = by_month(nond$enddate_formatted))



#OK, let's just do a fun check on this data. Looking JUST at active firms with records for employees for the last pair of years
#(Noting that due to dates of accounting submission, those pair of years won't perfectly line up)

#Just confirm how many matches there are from the employee data we want to use... ALL of them. Nice.
#35792
table(nond$companynumber[!is.na(nond$Employees_lastyear) & !is.na(nond$Employees_thisyear)] %in% ch.geo$CompanyNumber)

#That's for firms with employee data for both of the years we extracted from.
#What about just for the most recent year?
#All of them again, 42920
table(nond$companynumber[!is.na(nond$Employees_thisyear)] %in% ch.geo$CompanyNumber)



#TODO: neater way to examine firms with different employee / numbers / dates

#Join with geo-data - all firms with employee data in most recent year...
ch.geo.thisyear <- ch.geo %>% 
  right_join(
    nond %>% filter(!is.na(Employees_thisyear)),
    by = c('CompanyNumber' = 'companynumber')
  )

#... and all firms with employee data for both years
ch.geo.twoyears <- ch.geo %>% 
  right_join(
    nond %>% filter(!is.na(Employees_lastyear) & !is.na(Employees_thisyear)),
    by = c('CompanyNumber' = 'companynumber')
  )



#Sanity check - business names are the same??
#Tick - formatting from the actual accounts is a bit nicer
ch.geo.twoyears %>% select(CompanyName,Company) %>% View



#So, initial question:
#IS THERE ANY SOUTH YORKSHIRE GEOGRAPHY TO PLACES WHERE BUSINESS EMPLOYMENT IS GROWING VS NOT?
#Probably filtering to firms over a certain size.

#Let's filter a few other things out
#1. Some firms put financials into the employee box, wrong data
#Well maybe only one
ch.geo.twoyears <- ch.geo.twoyears %>% filter(Employees_thisyear < 10000)

#Some have zeroes - fine, those are likely new firms. But no good for us with this test.
table(ch.geo.twoyears$Employees_thisyear == 0)
table(ch.geo.twoyears$Employees_lastyear == 0)
#So those are mostly firms with zero in both... can come back and mull who those are
table(ch.geo.twoyears$Employees_lastyear == 0 & ch.geo.twoyears$Employees_thisyear == 0)

#27000 firms left now
ch.geo.twoyears <- ch.geo.twoyears %>% 
  filter(
    Employees_thisyear != 0,
    Employees_lastyear != 0
    )


#... and for all firms with employee data in latest year...
#Dropped to 33353 from 42920
ch.geo.thisyear <- ch.geo.thisyear %>% 
  filter(
    Employees_thisyear != 0
  )




#Wow - orbital umbrella (Barnsley payroll company) dropped employee numbers by a quarter (about 1000 to about 750)
#But what does that mean? Are they still on contract...? Can't find news on it
#Would be good to check against financials change.

#First, what's a quick way to get a sense of the overall pattern of change across all SY companies?
#Just plot the diff range...
ch.geo.twoyears <- ch.geo.twoyears %>% 
  mutate(
    employee_diff_percent = ((Employees_thisyear - Employees_lastyear)/Employees_lastyear) * 100
  )

#View to check, cols a bit confused now
View(ch.geo.twoyears %>% select(Company,Employees_thisyear,Employees_lastyear,employee_diff_percent))


#Let's do diff range broken down by sector type, for which we need...
#Just using the main sector for now before seeing what can be squeezed from the others

#NOTE: Quick test of how many sectors (up to four) firms label themselves with
#Sequentially filled in, 1 then 2 then 3 then 4, if filled in, so...
#100% with a single sector filled in and:
table(!is.na(ch.geo.twoyears$SICCode.SicText_2)) %>% prop.table * 100#13.5% with two sectors
table(!is.na(ch.geo.twoyears$SICCode.SicText_3)) %>% prop.table * 100#4.55% with three sectors
table(!is.na(ch.geo.twoyears$SICCode.SicText_4)) %>% prop.table * 100#1.78% with four sectors

#Label that and check correlation with firm size - do larger firms put in more SICs?
ch.geo.twoyears <- ch.geo.twoyears %>% 
  mutate(
    no_siclabels = case_when(
      !is.na(ch.geo.twoyears$SICCode.SicText_4) ~ 4,
      !is.na(ch.geo.twoyears$SICCode.SicText_3) ~ 3,
      !is.na(ch.geo.twoyears$SICCode.SicText_2) ~ 2,
      .default = 1
    )
  )  

# ggplot(ch.geo.twoyears, aes(x = Employees_thisyear, fill = factor(no_siclabels))) +
#   geom_density(alpha=0.5) +
#   coord_cartesian(xlim = c(0,30))
# 
# ggplot(ch.geo.twoyears, aes(y = Employees_thisyear, x = factor(no_siclabels))) +
#   geom_boxplot() 

# summary(lm(data = ch.geo.twoyears, formula = log(Employees_thisyear) ~ factor(no_siclabels)))


#Anyway! Let's attach SIC codes (just for the main 5 digit for now)
ch.geo.twoyears <- ch.geo.twoyears %>% 
  mutate(
    SIC_5DIGIT_CODE = substr(SICCode.SicText_1,1,5)
  ) %>% 
  left_join(
    read_csv('https://github.com/DanOlner/ukcompare/raw/master/data/SIClookup.csv'),
    by = 'SIC_5DIGIT_CODE'
  )

ch.geo.thisyear <- ch.geo.thisyear %>% 
  mutate(
    SIC_5DIGIT_CODE = substr(SICCode.SicText_1,1,5)
  ) %>% 
  left_join(
    read_csv('https://github.com/DanOlner/ukcompare/raw/master/data/SIClookup.csv'),
    by = 'SIC_5DIGIT_CODE'
  )

#Basics: employee number per SIC section
ch.geo.twoyears %>% 
  st_set_geometry(NULL) %>% 
  group_by(SIC_SECTION_NAME) %>% 
  summarise(employees = sum(Employees_thisyear)) %>% 
  arrange(-employees) %>% 
  print(n=25)



#OK, check employee number diff change - filter for 10+employees in most recent year
#Actually better to filter on LAST year (there are a lot of healthcare / care homes that had e.g. 1 employee last year and apparently 64 this year...)

#Select view and get group means and add over top
chforplot <- ch.geo.twoyears %>% filter(Employees_lastyear > 9, !grepl('households|extraterrit',SIC_SECTION_NAME, ignore.case = T), !is.na(SIC_SECTION_NAME))

section_diff_means <- chforplot %>% 
  st_set_geometry(NULL) %>% 
  group_by(SIC_SECTION_NAME) %>% 
  summarise(mean_diff_percent = mean(employee_diff_percent))

ggplot() +
  geom_jitter(data = chforplot, aes(y = employee_diff_percent, x = fct_reorder(SIC_SECTION_NAME, employee_diff_percent, .fun = mean)),
    alpha = 0.25, width = 0.25) +
  geom_hline(yintercept = 0) +
  coord_flip(ylim = c(-100,100)) 
  # geom_point(data = section_diff_means, aes(y = mean_diff_percent, x = fct_reorder(SIC_SECTION_NAME, section_diff_means)))
  



#Check on performing arts related sectors... using any with employees in latest year
arts <- (ch.geo.thisyear %>% filter(SIC_SECTION_NAME == "Arts, entertainment and recreation"))
View(arts)
unique(arts$SIC_5DIGIT_NAME)
unique(arts$SIC_2DIGIT_NAME)

#Compare - how many firms getting from each of these? Not huge diff...
#418
ch.geo.thisyear %>% filter(SIC_2DIGIT_NAME == "90 : Creative, arts and entertainment activities") %>% nrow
#376
ch.geo.twoyears %>% filter(SIC_2DIGIT_NAME == "90 : Creative, arts and entertainment activities") %>% nrow

#Write for elsewhere
write_csv(ch.geo.thisyear %>% st_set_geometry(NULL) %>% filter(SIC_2DIGIT_NAME == "90 : Creative, arts and entertainment activities"), 
          'localdata/creativeartsentertainmentSIC90.csv')

#And also pubs etc


#Let's start mulling if there's any geography
#Think I want to aggregate diffs to a grid square (for places with enough firms, grey out the rest)
#But let's just do some eyeballing first

#That last plot shows there's a pretty even distribution
#But picking on section name...
sy <- st_read('localdata/QGIS/sy_localauthorityboundaries.shp')

#Check points against map...
tmap_mode('view')

ch.geo.twoyears %>% filter(Employees_lastyear > 9, SIC_SECTION_NAME == "Manufacturing") %>% nrow
ch.geo.thisyear %>% filter(SIC_SECTION_NAME == "Manufacturing") %>% nrow

tm_shape(sy) +
  tm_borders() +
  tm_shape(ch.geo.twoyears %>% filter(Employees_lastyear > 9, SIC_SECTION_NAME == "Manufacturing")) +
  # tm_shape(ch.geo.twoyears %>% filter(Employees_lastyear > 9, SIC_SECTION_NAME == "Information and communication")) +
  # tm_shape(ch.geo.twoyears %>% filter(Employees_lastyear > 9, SIC_SECTION_NAME == "Construction")) +
  # tm_shape(ch.geo.twoyears %>% filter(Employees_lastyear > 9, SIC_SECTION_NAME == "Human health and social work activities")) +
  # tm_shape(ch.geo.twoyears %>% filter(Employees_lastyear > 9, SIC_SECTION_NAME == "Arts, entertainment and recreation")) +
  # tm_shape(ch.geo.twoyears %>% filter(SIC_2DIGIT_NAME == "90 : Creative, arts and entertainment activities")) +
  # tm_shape(ch.geo.twoyears %>% filter(SIC_3DIGIT_NAME == "563 : Beverage serving activities")) +
  tm_dots(col = 'employee_diff_percent', border.alpha = 0.1, size = 'Employees_thisyear', style = 'fisher', scale = 2)

#Hmm. Employee counts seem to have distinct places too
tm_shape(sy) +
  tm_borders() +
  tm_shape(ch.geo.twoyears %>% filter(Employees_lastyear < 50, SIC_SECTION_NAME == "Manufacturing")) +
  # tm_shape(ch.geo.twoyears %>% filter(SIC_2DIGIT_NAME == "90 : Creative, arts and entertainment activities")) +
  tm_dots(col = 'Employees_thisyear', size = 'Employees_thisyear', style = 'fisher')



#Version to view employees with counts in most recent year only
#Hmm. Employee counts seem to have distinct places too
tm_shape(sy) +
  tm_borders() +
  # tm_shape(ch.geo.twoyears %>% filter(Employees_lastyear < 50, SIC_SECTION_NAME == "Manufacturing")) +
  tm_shape(ch.geo.thisyear %>% filter(SIC_2DIGIT_NAME == "90 : Creative, arts and entertainment activities")) +
  tm_dots(col = 'Employees_thisyear', size = 'Employees_thisyear', style = 'fisher', scale = 2)






## Examine all iXBRL tags in a particular doc to see what we've got----

#Gripple as example
doc <- read_xml("~/localdata/monthly_companieshouse_accounts/Accounts_Monthly_Data-September2024/Prod224_2476_01772901_20231231.html")

# Define namespaces dynamically
ns <- xml_ns(doc)

# Extract all <ix:nonNumeric> and <ix:nonFraction> tags
non_numeric_nodes <- xml_find_all(doc, "//ix:nonNumeric", ns = ns)
non_fraction_nodes <- xml_find_all(doc, "//ix:nonFraction", ns = ns)

# Combine the text content of these nodes into a single vector
all_tags <- c(
  xml_text(non_numeric_nodes),
  xml_text(non_fraction_nodes)
)

# Print the resulting vector
print(all_tags)

# Optional: To inspect the corresponding tag names and attributes
all_tags_with_names <- c(
  paste("Tag Name:", xml_name(non_numeric_nodes), "Value:", xml_text(non_numeric_nodes)),
  paste("Tag Name:", xml_name(non_fraction_nodes), "Value:", xml_text(non_fraction_nodes))
)

print(all_tags_with_names)



#See what the names of the tags are, matched against values
#TODO: search several accounts, look for what frequencies we get for different iXBRL tags
doc <- read_xml("~/localdata/monthly_companieshouse_accounts/Accounts_Monthly_Data-September2024/Prod224_2476_01772901_20231231.html")

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

# Remove entries where the name is NA
all_details <- all_details[!is.na(all_names)]

print(all_details)

all_details[grepl('profit',all_details,ignore.case = T)]





# COMBINE SOUTH YORKSHIRE DATA AGAIN FOR SHINY APP----

#Shiny in separate project currently SYMCA_shiny

#Same as above - get current employee counts, join to CH data, join SIC lookup
#But keep all
employee.numbers.all <- map(list.files(path = 'localdata/SouthYorkshire_accounts_saves', pattern = '*.rds', full.names = T), readRDS) %>% bind_rows() %>% filter(dormantstatus == 'false')


#Join employee number to CH data, will filter us down to SY at this point via employee number
ch.geo.sy <- ch.geo %>% 
  right_join(
    employee.numbers.all,
    by = c('CompanyNumber' = 'companynumber')
  ) %>% 
  select(-dormantstatus)

#Join SIC lookup
ch.geo.sy <- ch.geo.sy %>% 
  mutate(
    SIC_5DIGIT_CODE = substr(SICCode.SicText_1,1,5)
  ) %>% 
  left_join(
    read_csv('https://github.com/DanOlner/ukcompare/raw/master/data/SIClookup.csv'),
    by = 'SIC_5DIGIT_CODE'
  ) %>% 
  relocate(SIC_5DIGIT_NAME, .after = SIC_5DIGIT_CODE) %>% 
  relocate(SIC_2DIGIT_CODE_NUMERIC, .after = SIC_2DIGIT_CODE)

#reminder, how many have employee count in the latest account year?
table(!is.na(ch.geo.sy$Employees_thisyear)) %>% prop.table()

#98.6% with employees at least this year. Let's just keep those.
ch.geo.sy <- ch.geo.sy %>% 
  filter(!is.na(Employees_thisyear))

#30mb in memory
pryr::object_size(ch.geo.sy)

#SAVE DIRECTLY TO SHINY DATA FOLDER
saveRDS(ch.geo.sy,"../SYMCA_shiny/data/companieshouse_employees_n_sectors_southyorkshire.rds")



















