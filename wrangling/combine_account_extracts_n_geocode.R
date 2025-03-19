#Combine account extracts with geocoded company live list
library(tidyverse)

#current companies live list with geolocation / local authorities added
#3.4gb in memory
ch <- readRDS('local/companieshouse_livelist_geocoded.rds')

