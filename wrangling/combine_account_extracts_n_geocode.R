#Combine account extracts with geocoded company live list
library(tidyverse)
source('functions.R')

#current companies live list with geolocation / local authorities added
#KEEP ONLT ACTIVE ACCOUNTS - 
#see 
ch <- readRDS('local/companieshouse_livelist_geocoded.rds')
#pryr::object_size(ch) #3.4gb in memory

#Combine currently monthly accounts extractions into a single df
extract.locs <- list.files('local/account_extracts', full.names = T)

#Stick into single df
extracts <- extract.locs %>% map(read_csv, show_col_types = F) %>% bind_rows
#pryr::object_size(extracts) 18.42mb in memory

