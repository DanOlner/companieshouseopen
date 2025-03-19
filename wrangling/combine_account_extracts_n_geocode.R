#Combine account extracts with geocoded company live list
library(tidyverse)
source('functions.R')

# GET COMPANIES HOUSE LIVE LIST----

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

#Drop duplicate submitted accounts (they'll have the same date / be exactly the same)
#Account code is the date of submission
extracts <- extracts %>% 
  distinct(accountcode, companynumber, .keep_all = T)

#Format date
extracts <- extracts %>% 
  mutate(
    enddate_formatted = lubridate::parse_date_time(
      enddate,
      orders = c("dmy","ymd","mdy"))
  ) 



# GET AND PROCESS EXTRACTED ACCOUNTS DATA----

#For now, just going to keep the latest accounts
#Firms with multiple accounts up to 8, have clearly submitted backdated ones this year
#See https://github.com/DanOlner/companieshouseopen/blob/f2d59df75617905b2522c9be483bfb4db4f1fc5d/testcode/tests_n_checks_companieshousedata.R#L74

#For firms with multiple accounts, keep most recent date
# x <- extracts %>% 
#   group_by(companynumber) %>% 
#   filter(enddate_formatted == max(enddate_formatted)) %>% 
#   ungroup()

#Version applying to all companies is very slow
#Let's split and find newest date just for those with more than one set of accounts
#(Having filtered out the odd multiple submission of the same accounts above)
account.count <- extracts %>% 
  group_by(companynumber) %>% 
  summarise(count = n()) %>% 
  ungroup()

table(account.count$count)

#Keep only multiples to pick most decent accounts date
multiples <- extracts %>% filter(companynumber %in% account.count$companynumber[account.count$count != 1]) %>%
  arrange(companynumber) 

mostrecent <- multiples %>% 
    group_by(companynumber) %>%
    filter(enddate_formatted == max(enddate_formatted)) %>%
    ungroup()

#Tick, one row per company
length(unique(mostrecent$companynumber))

#Some duplicates still
#Presumably must be some firms that submitted accounts with the same date?
# mostrecent.count <- mostrecent %>% 
#   group_by(companynumber) %>% 
#   summarise(count = n()) %>% 
#   ungroup()
# 
# table(mostrecent.count$count)
# 
# mostrecent %>% filter(companynumber %in% mostrecent.count$companynumber[mostrecent.count$count > 1]) %>% View

#Ah that one's easy to filter out duplicates - the account code is the same... doing above


#combine again
extracts.singleaccounts <- bind_rows(
  mostrecent,
  extracts %>% filter(companynumber %in% account.count$companynumber[account.count$count == 1])
)

#Confirm, should be one company per row... TICK
length(unique(extracts.singleaccounts$companynumber))


#Only need the one date column now
extracts.singleaccounts <- extracts.singleaccounts %>% 
  select(-enddate) %>% 
  rename(enddate = enddate_formatted)


# CHECK MATCH BETWEEN LIVE LIST AND EXTRACTED ACCOUNTS----

#Check match on companies house number... 8.2% with no match in the live list
table(extracts.singleaccounts$companynumber %in% ch$CompanyNumber)
table(extracts.singleaccounts$companynumber %in% ch$CompanyNumber) %>% prop.table() * 100

#Look at the falses - what accounts are those? Many NI - postcodes are GB only so that makes sense
#A lot of others are dissolved - no longer live
extracts.singleaccounts %>% filter(!companynumber %in% ch$CompanyNumber) %>% View




# LINK LIVE LIST AND ACCOUNTS EXTRACTS----

#Currently ~3.2 million businesses
#Keep as sf object with point geometry for locations
both <- ch %>% 
  inner_join(
    extracts.singleaccounts,
    by = c('CompanyNumber' = 'companynumber')
  )










#2.4gb
pryr::object_size(both)

saveRDS(both, 'local/accountextracts_n_livelist_geocoded_combined.rds')




