#Digging into the combined live list / extracted accounts info
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(RColorBrewer)
source('functions.R')

#The joined CH live list, geocoded with LA and ITL2 lookup, and account extracts added
ch <- readRDS('local/accountextracts_n_livelist_geocoded_combined.rds')
#2.48gb in version as of March 2025
#pryr::object_size(ch)

#Add in SIC lookup
ch <- ch %>% 
  mutate(
    SIC_5DIGIT_CODE = substr(SICCode.SicText_1,1,5)
  ) %>% 
  left_join(
    read_csv('https://github.com/DanOlner/ukcompare/raw/master/data/SIClookup.csv'),
    by = 'SIC_5DIGIT_CODE'
  ) %>% 
  relocate(SIC_5DIGIT_NAME, .after = SIC_5DIGIT_CODE) %>% 
  relocate(SIC_2DIGIT_CODE_NUMERIC, .after = SIC_2DIGIT_CODE)

#drop dormant firms
ch <- ch %>% filter(SIC_5DIGIT_CODE!="99999")


#AVERAGE AGE OF FIRMS IN ITL2 REGIONS PER SIC SECTION----

#Date-ify incorporation data
ch <- ch %>%
  mutate(
  incorporationdate_formatted = lubridate::parse_date_time(
    IncorporationDate,
    orders = c("dmy","ymd","mdy"))
  ) 

#table(is.na(ch$incorporationdata_formatted))

ch <- ch %>% 
  mutate(
    age_of_firm_years = (difftime(today(),incorporationdate_formatted) / 365) %>% as.numeric()
  )


#Any obvious differences overall? No, this isn't very informative!
ggplot(ch, aes(x = age_of_firm_years, fill = ITL221NM)) +
  geom_density(alpha = 0.2) +
  scale_x_log10() +
  guides(fill = "none")

ggplot(ch, aes(x = age_of_firm_years)) +
  geom_histogram(aes(y=..density..)) +
  facet_wrap(~ITL221NM)

ggplot(ch, aes(y = age_of_firm_years, x = fct_reorder(ITL221NM,age_of_firm_years))) +
  scale_y_log10() +
  geom_boxplot() +
  coord_flip()

#Just manuf
ggplot(
  # ch %>% filter(qg('manuf',SIC_SECTION_NAME)),
  ch %>% filter(qg('information',SIC_SECTION_NAME)),
  aes(y = age_of_firm_years, x = fct_reorder(ITL221NM,age_of_firm_years))) +
  scale_y_log10() +
  geom_boxplot() +
  coord_flip()




#High resolution map of modal SIC section type across GB----

#Make a UK size grid
#BNG is in metres...
#Use companies house bounding box for size
sq = st_make_grid(ch, cellsize = 5000, square = T)
sq = st_make_grid(ch, cellsize = 5000, square = F)

#Turn into sf object so gridsquares can have IDs to group by
sq <- sq %>% st_sf() %>% mutate(id = 1:nrow(.))

plot(sq)

#Intersection...
overlay <- st_intersection(ch,sq)

#Before getting modal section, need to summarise thus
#Get sum of employees per SIC section in each grid square

#Also need to reduce section number, see which to combine
# unique(ch$SIC_SECTION_NAME)


#This no longer needs to be geo, which will speed up
#Can link back to grids once done
section.summary <- overlay %>% 
  st_set_geometry(NULL) %>% 
  filter(Employees_thisyear > 0) %>% #Only firms with employees recorded in latest year
  filter(!qg('households|extraterr', SIC_SECTION_NAME)) %>% #sections to leave out when summing; keeping separate to make change easier
  group_by(id,SIC_SECTION_NAME) %>%
  summarise(totalemployees = sum(Employees_thisyear)) %>% 
  filter(!is.na(SIC_SECTION_NAME)) %>% 
  group_by(id) %>% 
  filter(sum(totalemployees) >= 50)#keep only gridsquares where total employee count is more than / equal to 100
  # filter(sum(totalemployees) >= 100)#keep only gridsquares where total employee count is more than / equal to 100


#Confirm only 100+ per grid square... tick
section.summary %>% 
  group_by(id) %>% 
  summarise(sum = sum(totalemployees)) %>% 
  ungroup() %>% 
  summarise(min = min(sum))


#Find modal sector
modal.sector <- section.summary %>%
  group_by(id) %>% 
  summarise(modal_sector = SIC_SECTION_NAME[which.max(totalemployees)])

#Link that back into the grid squares...
#Use right join to drop empties
sq.modal <- sq %>% 
  right_join(
    modal.sector,
    by = 'id'
  )

itl2 <- st_read('../RegionalEcons_web/data/ITL_geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp') %>% st_simplify(preserveTopology = T, dTolerance = 100)


#https://stackoverflow.com/a/33144808/5023561
#Make different pastel-ish colours
n <- length(unique(sq.modal$modal_sector))
set.seed(101)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))

tmap_mode('view')

tm_shape(
  sq.modal
  # sq.modal %>% mutate(combined_label = paste0(ITL221NM,', ',modal_sector))
  ) +
  tm_polygons('modal_sector', fill.scale = tm_scale_categorical(values = col_vector), id="modal_sector", col_alpha = 0) +
tm_shape(itl2) +
  tm_borders(col = 'black')
  
  
  
  
  









