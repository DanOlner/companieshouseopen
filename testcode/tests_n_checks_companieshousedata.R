#Various tests
library(tidyverse)
library(sf)
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

#Then there's...
#Which just means "no sig transactions in the last year"
#https://www.gov.uk/dormant-company/dormant-for-companies-house
table(ch$SICCode.SicText_1 == '99999 - Dormant Company')

#Overlap? Weirdly not much
table(ch$CompanyStatus,ch$SICCode.SicText_1 == '99999 - Dormant Company')




#CHECK ON AVAILABLE DATA IN IXBRL FORMAT IN ACCOUNTS----

#Will vary, so need to extract from a few accounts and see what's there.
#How many match what's listed in the URI guide here?
#https://assets.publishing.service.gov.uk/media/5d08c0f340f0b6094a379078/uniformResourceIdentifiersCustomerGuide.pdf

#Taxonomies may have changed over time, so what's as common as we can get it?
#https://resources.companieshouse.gov.uk/infoAndGuide/faq/accountsDataProduct.shtml













