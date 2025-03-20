#Various tests
library(tidyverse)
library(sf)
options(scipen = 99)
source('functions.R')




# CHECK ON POSTCODE MATCHING TO CH LIVE LIST - WHAT PLACES DIDN'T MATCH?----

#Looking at the entire current live list and which companies' postcodes found no match

#Created in download_current....
ch <- readRDS('local/BasicCompanyDataAsOneFile-2025-03-01.rds')

#With geolocation / local authority lookup via intersection with postcode location
#Created in CH_livelist_geolocation
ch.geo <- readRDS('local/companieshouse_livelist_geocoded.rds')

#Get live list rows with no postcode match
ch.nomatch <- ch %>% filter(!CompanyNumber %in% ch.geo$CompanyNumber)

#Currently 5% didn't get match
(nrow(ch.nomatch)/nrow(ch))*100


#Any entries for this? Some...
table(!is.na(ch$RegAddress.CareOf))
ch %>% filter(!is.na(RegAddress.CareOf)) %>% View


#But we want to look at places with no postcode match...
#Many just don't have any, many of those aren't UK addresses

#Looking at all with no PC but some address details....
ch.nomatch.withaddressdetails <- ch %>% filter(is.na(RegAddress.PostCode), !is.na(RegAddress.AddressLine1))

#That's a muuuuch smaller number with possible addresses to match against
#And most of those are non UK / company type is not a great guide

#Still some UK ones in there with full addresses but...
#0.5% now
(nrow(ch.nomatch.withaddressdetails)/nrow(ch))*100

#So: of the 5% that don't match, all but 0.5% of it don't have any address to match against at al
#The remainder is mixed but mostly non UK.



# CATEGORY CHECKS----

#Good proportion "proposal to strike off", which means will be closing down the account
#https://companydoctor.co.uk/active-proposal-to-strike-off/
table(ch$CompanyStatus)

#Total proportion of active vs various reasons not active
#91.3% active
table(ch$CompanyStatus) %>% prop.table() * 100

table(ch$CompanyCategory)

#Then there's...
#Which just means "no sig transactions in the last year"
#https://www.gov.uk/dormant-company/dormant-for-companies-house
table(ch$SICCode.SicText_1 == '99999 - Dormant Company')

#Overlap? Weirdly not much
#Around 98% of active not dormant
table(ch$CompanyStatus,ch$SICCode.SicText_1 == '99999 - Dormant Company')
table(ch$CompanyStatus,ch$SICCode.SicText_1 == '99999 - Dormant Company') %>% prop.table(margin = 1) * 100







# ACCOUNT EXTRACT CHECKS----

extract.locs <- list.files('local/account_extracts', full.names = T)

#Stick into single df
extracts <- extract.locs %>% map(readRDS) %>% bind_rows
#pryr::object_size(extracts) 732.65mb in memory

extracts <- extracts %>% 
  mutate(hasatleastoneemployeecount = !is.na(Employees_thisyear))

table(extracts$dormantstatus)

#Overlap with dormant status or not?
#Oh that field is not super helpful as is!
table(extracts$dormantstatus, extracts$hasatleastoneemployeecount)

#Needs checking against SIC code 1 entry for "99999 Dormant" to make sure

#check how many accounts per firm are in extracts (usually going to one)
account.count <- extracts %>% 
  group_by(companynumber) %>% 
  summarise(count = n())

#Well that's readable
table(account.count$count)

#To pick a random company number with 6 entries in the last year...
#Looks like what's happened: they've submitted several years' accounts backdated
extracts %>% filter(companynumber == '05570992') %>% View

#Check on some of the 2s...
extracts %>% filter(companynumber %in% account.count$companynumber[account.count$count ==2]) %>%
  arrange(companynumber) %>% 
  View





# COMPARE PARALLEL TO NON PARALLEL ACCOUNTS PROCESSING----

#Using furrr
#This section assumes a monthly accounts zip has already been downloaded
#And then set up using extractinfo_fromaccountfiles.R





# CHECK DETAILS IN RANDOM EXTRACTED ACOUNTS FILE----

x <- readRDS('local/account_extracts/Accounts_Monthly_Data-April2024.rds')

#52mb in memory
pryr::object_size(x)

g(x)



# CHECK ON AVAILABLE DATA IN IXBRL FORMAT IN ACCOUNTS----

#Will vary, so need to extract from a few accounts and see what's there.
#How many match what's listed in the URI guide here?
#https://assets.publishing.service.gov.uk/media/5d08c0f340f0b6094a379078/uniformResourceIdentifiersCustomerGuide.pdf

#Taxonomies may have changed over time, so what's as common as we can get it?
#https://resources.companieshouse.gov.uk/infoAndGuide/faq/accountsDataProduct.shtml






# EXAMINE COMBINED LIVE LIST / ACCOUNT EXTRACTS FOR EMPLOYEE NUMBER----

both <- readRDS('local/accountextracts_n_livelist_geocoded_combined.rds')

#Count of firms per LA
both %>% 
  st_set_geometry(NULL) %>% 
  group_by(localauthority_name) %>% 
  summarise(n()) %>% 
  View

#Join SIC lookup to first SIC code
both <- both %>% 
  mutate(
    SIC_5DIGIT_CODE = substr(SICCode.SicText_1,1,5)
  ) %>% 
  left_join(
    read_csv('https://github.com/DanOlner/ukcompare/raw/master/data/SIClookup.csv'),
    by = 'SIC_5DIGIT_CODE'
  ) %>% 
  relocate(SIC_5DIGIT_NAME, .after = SIC_5DIGIT_CODE) %>% 
  relocate(SIC_2DIGIT_CODE_NUMERIC, .after = SIC_2DIGIT_CODE)

#drop all dormant. From 3255134 to 3173230
both <- both %>% filter(SIC_5DIGIT_CODE!="99999")


#Let's make map of manufacturing
if (!dir.exists('local/qgis')) dir.create('local/qgis')

st_write(
  both %>% filter(SIC_SECTION_NAME == 'Manufacturing') %>% select(localauthority_name,Employees_thisyear),
  'local/qgis/livelist_manuf_section.geojson')

st_write(
  both %>% filter(qg('information',SIC_SECTION_NAME)) %>% select(localauthority_name,Employees_thisyear),
  'local/qgis/livelist_ICT_section.geojson')

st_write(
  both %>% filter(qg('agri',SIC_SECTION_NAME)) %>% select(localauthority_name,Employees_thisyear),
  'local/qgis/livelist_agri_section.geojson')










