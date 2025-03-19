#Combine account extracts with geocoded company live list
library(tidyverse)
source('functions.R')

#current companies live list with geolocation / local authorities added
ch <- readRDS('local/companieshouse_livelist_geocoded.rds') 
#pryr::object_size(ch) #3.4gb in memory

#KEEP ONLT ACTIVE ACCOUNTS - 
#see https://github.com/DanOlner/companieshouseopen/blob/6ac76b4c60db56ce6894735e6312aba7fb978dd5/testcode/tests_n_checks_companieshousedata.R#L48
#Note: keeping "dormant" firms for now to see what job counts they have
#Dormant can just mean "no financial activity in last year"
ch <- ch %>% filter(CompanyStatus == 'Active')

#Combine currently monthly accounts extractions into a single df
extract.locs <- list.files('local/account_extracts', full.names = T)

#Stick into single df
extracts <- extract.locs %>% map(readRDS) %>% bind_rows
#pryr::object_size(extracts) 732.65mb in memory


#For now, just going to keep the latest accounts
#Firms with multiple accounts up to 8, have clearly submitted backdated ones this year