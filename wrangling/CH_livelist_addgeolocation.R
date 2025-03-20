#Geocode current Companies House live list
#(Saved locally in download_current_CH_livelist_n_save_reducedcopy_locally.R)
library(tidyverse)
library(sf)

#Postcode / local authority lookup made here:
#https://github.com/DanOlner/utilities/blob/master/postcodes_to_localauthorities_makelookup.R
#~260mb
# pc <- read_csv('https://www.dropbox.com/scl/fi/0873bsx2ka1d3clhs8e07/postcode_localauthority_lookup_2024-11-20.csv?rlkey=rtda1srm2nlzmqxc2ms6fllno&dl=1')

#version with ITL2 included
pc <- read_csv('https://www.dropbox.com/scl/fi/iaa3vli7zxl7b93aekyyh/postcode_localauthority_itl2_lookup_2025-03-19.csv?rlkey=jp5v00hm1erfsi20ft0ggp4jk&dl=1')


#Companies house live list
ch <- readRDS('local/BasicCompanyDataAsOneFile-2025-03-01.rds')

#As much as possible, get postcode fields into matching format
#Most do already match, but a few more might get linked
#(Postcodes are manually entered by firms)
pc$Postcode_formatted <- toupper(pc$Postcode) %>% gsub(' ', '', .)
ch$Postcode_formatted <- toupper(ch$RegAddress.PostCode) %>% gsub(' ', '', .)

#Check match number for each... gets 60 more on this run. Rounding error.
# table(pc$Postcode %in% ch$RegAddress.PostCode)
# table(pc$Postcode_formatted %in% ch$Postcode_formatted)

table(ch$RegAddress.PostCode %in% pc$Postcode)
table(ch$Postcode_formatted %in% pc$Postcode_formatted)

#Add postcode geo-location into the live list
#Not a perfect match but better than alternatives 
#See https://github.com/DanOlner/FirmAnalysis/blob/bcf06e46849eb7e08501c596955a247e5dadbe00/companieshousedata.R#L45
ch.geo <- ch %>%
  # rename(Postcode = RegAddress.PostCode) %>% 
  left_join(
    pc %>% select(Postcode_formatted,localauthority_code,localauthority_name,ITL221CD,ITL221NM,Eastings,Northings),
    by = 'Postcode_formatted'
  ) %>% 
  filter(!is.na(Eastings)) %>% #will be some missing due to rogue postcodes, throws error in geocoding next
  st_as_sf(coords = c("Eastings", "Northings"), crs = 27700) %>% 
  rename(postcode = Postcode_formatted)

#table(is.na(ch.geo$postcode))
#table(is.na(ch.geo$ITL221NM))

#Save as RDS
saveRDS(ch.geo,'local/companieshouse_livelist_geocoded.rds')
