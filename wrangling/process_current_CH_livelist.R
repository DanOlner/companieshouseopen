#GET CURRENT LIVE LIST OF COMPANIES DIRECT FROM COMPANIES HOUSE
#Reduce to just the useful columns and save locally
#Create local directories if they're not already present
library(tidyverse)

#https://stackoverflow.com/questions/4216753/folder-management-with-r-check-existence-of-directory-and-create-it-if-it-does
if (!dir.exists('local')) dir.create('local')

