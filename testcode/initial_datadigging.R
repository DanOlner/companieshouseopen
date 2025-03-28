#Digging into the combined live list / extracted accounts info
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(cols4all)
library(RColorBrewer)
library(plotly)
options(scipen = 99)
source('functions.R')

#https://github.com/cols4all/cols4all-R
#c4a_gui()

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


#Date-ify incorporation date
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



# LOOK FOR TOO-BIG EMPLOYEE COUNT FIRMS----

#Some of the the employee values are VERY WRONG - usually because the IXBRL value has e.g. ended up with the asset value by mistake

#Look at some of the possibly-too-big values
#Only ~400 of those in total for the last year's accounts
toobig <- ch %>% st_set_geometry(NULL) %>% select(CompanyName,CompanyNumber,accountcode,Employees_thisyear,Employees_lastyear,ITL221NM) %>% pivot_longer(Employees_thisyear:Employees_lastyear, names_to = 'employees_year', values_to = 'employees_count') %>% filter(employees_count > 1000)
# 
ggplot(
  toobig,
  aes(x = employees_count, y = employees_year)) +
  geom_jitter(height = 0.1)

#Just in SY
toobig %>% filter(qg('south y',ITL221NM)) %>% View


#AVERAGE AGE OF FIRMS IN ITL2 REGIONS PER SIC SECTION----



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
# sq = st_make_grid(ch, cellsize = 5000, square = T)
sq = st_make_grid(ch, cellsize = 5000, square = F)

# sq = st_make_grid(ch, cellsize = 1000, square = F)

#Turn into sf object so gridsquares can have IDs to group by
sq <- sq %>% st_sf() %>% mutate(id = 1:nrow(.))

# plot(sq)

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
  filter(sum(totalemployees) >= 25)#keep only gridsquares where total employee count is more than / equal to 100
  # filter(sum(totalemployees) >= 50)#keep only gridsquares where total employee count is more than / equal to 100
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
  tm_polygons('modal_sector', fill.scale = tm_scale_categorical(values = col_vector), id="modal_sector", col_alpha = 0, fill_alpha = 0.65) +
  # tm_view(set.view = c(7, 51, 4)) +
tm_shape(itl2) +
  tm_borders(col = 'black') +
  tm_view(set_view = c(-1.598452,52.740283,8))
  # tm_view(bbox = "England")
  
  




# Version of same that picks out specific geographies for higher rez----

#Haven't tested how long it would take to do this for higher rez for whole GB

place <- ch %>% filter(qg('south y',ITL221NM))
place <- ch %>% filter(qg('yorkshire',ITL221NM))#gets all four yorkshires
place <- ch %>% filter(qg('manchester',ITL221NM))
place <- ch %>% filter(qg('london',ITL221NM))#this will get several!

#The north!
place <- ch %>% filter(qg('yorkshire|manchester|lancas|mersey',ITL221NM))#gets all four yorkshires


#Names all match (predictably - the names in the postcode lookup came from same shapefile!)
table(unique(place$ITL221NM) %in% itl2$ITL221NM)

sq = st_make_grid(place, cellsize = 1000, square = F)

#Turn into sf object so gridsquares can have IDs to group by
sq <- sq %>% st_sf() %>% mutate(id = 1:nrow(.))

plot(sq)

#Intersection...
overlay <- st_intersection(place,sq)

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
  filter(sum(totalemployees) >= 10)#keep only gridsquares where total employee count is more than / equal to 100
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
  tm_polygons('modal_sector', fill.scale = tm_scale_categorical(values = col_vector), id="modal_sector", col_alpha = 0, fill_alpha = 0.65) +
  tm_shape(itl2 %>% filter(ITL221NM %in% place$ITL221NM)) +
  tm_borders(col = 'black')








# MAP: MEDIAN AGE OF FIRM / FIRM COUNT----

sq = st_make_grid(ch, cellsize = 1000, square = F)

#Turn into sf object so gridsquares can have IDs to group by
sq <- sq %>% st_sf() %>% mutate(id = 1:nrow(.))

# plot(sq)

#Intersection...
# overlay <- st_intersection(ch,sq)

#Filtering out London to get better firm age map for rest of UK
overlay <- st_intersection(ch %>% filter(!qg('london', ITL221NM)),sq)

#Before getting modal section, need to summarise thus
#Get sum of employees per SIC section in each grid square

#Also need to reduce section number, see which to combine
# unique(ch$SIC_SECTION_NAME)


#This no longer needs to be geo, which will speed up
#Can link back to grids once done
age.summary <- overlay %>% 
  st_set_geometry(NULL) %>% 
  filter(Employees_thisyear > 0) %>% #Only firms with employees recorded in latest year
  filter(!qg('households|extraterr', SIC_SECTION_NAME)) %>% #sections to leave out when summing; keeping separate to make change easier
  group_by(id) %>%
  summarise(
    median_age_of_firm_years = as.numeric(median(age_of_firm_years)),
    firmcount = n()
    ) %>% 
  filter(firmcount >= 10)
  
#Any firmcount / age rel?
#Hmm- older firms is a little curious
ggplot(age.summary, aes(x = median_age_of_firm_years, y = firmcount)) +
  geom_point()

#And where the hell has 60K firms in one 5km hex?? Accountant firm? Those two spots massive outliers
#Let's look. Central London, obv
x <- overlay %>% filter(id %in% age.summary$id[age.summary$firmcount > 60000])

#quite a mix...
table(x$SIC_SECTION_NAME)



#Link that back into the grid squares...
#Use right join to drop empties
sq.map <- sq %>% 
  right_join(
    age.summary,
    by = 'id'
  ) %>% 
  mutate(
    hoverdisplay = paste0('Median age: ', round(median_age_of_firm_years,1), ' yrs, firm count: ', firmcount)
  )


tmap_mode('view')

#Median age
# tm_shape(sq.map) +
#   tm_polygons('median_age_of_firm_years', , id="hoverdisplay", col_alpha = 0, fill_alpha = 0.65, fill.scale = tm_scale_intervals(style = 'fisher', n = 11, values = "-orange_blue_diverging")) +
#   tm_shape(itl2) +
#   tm_borders(col = 'black')

#Firm count
tm_shape(sq.map) +
  tm_polygons('firmcount', , id="hoverdisplay", col_alpha = 0, fill_alpha = 0.65, fill.scale = tm_scale_intervals(style = 'fisher', n = 11, values = "-rd_yl_bu")) +
  # tm_view() +
  tm_shape(itl2) +
  tm_borders(col = 'black')













  
# DIG INTO EMPLOYEEE CHANGE IN THE LAST YEAR TO GET QUICK LIVE GROWTH SNAPSHOT----

#First thing: what proportion in this data have both year's employment count?
table(!is.na(ch$Employees_thisyear))
table(!is.na(ch$Employees_lastyear) & !is.na(ch$Employees_lastyear))

#86%
table(!is.na(ch$Employees_thisyear)) %>% prop.table() * 100
#70%
table(!is.na(ch$Employees_lastyear) & !is.na(ch$Employees_lastyear)) %>% prop.table() * 100

#All firms with more than zero employees this year: 74%
table(ch$Employees_thisyear > 0) %>% prop.table() * 100

#All firms with more than zero employees in either year: 81%
table(ch$Employees_thisyear > 0 | ch$Employees_lastyear > 0) %>% prop.table() * 100

#Excluding any zero employees for eiher year: 55% (just employees are zero in latest year is 54% so not hugely different)
table(!is.na(ch$Employees_lastyear) & !is.na(ch$Employees_lastyear) & (ch$Employees_thisyear > 0 | ch$Employees_lastyear > 0)) %>% prop.table() * 100

#... at least one employee in each year: 53.2%
table(!is.na(ch$Employees_lastyear) & !is.na(ch$Employees_lastyear) & ch$Employees_thisyear > 0 & ch$Employees_lastyear > 0) %>% prop.table() * 100


#TODO: We can do some work on whether each of those cats is representative of the business mix as a whole in the dataset another time




#Firms we can check for employee change, keeping only firms that had at least one employee at each timepoint
ch.ec <- ch %>% filter(!is.na(Employees_lastyear) & !is.na(Employees_lastyear) & Employees_thisyear > 0 & Employees_lastyear > 0)

#Also filter DOWN to sensible employee counts (to exclude error-made employee numbers)
#May be some false positives here - need to check
ch.ec <- ch.ec %>% filter(Employees_thisyear < 7000 & Employees_lastyear < 7000)



#Right some crude digging first, SIC section level
#Growth rate change for 10+ employee firms
ch.ec.10pluslastyr <- ch.ec %>% 
  filter(Employees_lastyear >= 10) %>%
  # filter(Employees_lastyear < 10) %>% 
  mutate(employee_percentchange = percent_change(Employees_lastyear,Employees_thisyear))

View(ch.ec.10pluslastyr %>% select(Employees_lastyear,Employees_thisyear,employee_percentchange))


#Break down by section / ITL2 zone - do percent change overall per section / ITL2 zone
#Mining removed here because no ITL2 zone where it has enough firm count (apart from one)
empchange.summary <- ch.ec.10pluslastyr %>% 
  st_set_geometry(NULL) %>% 
  filter(!qg('households|extraterr|mining', SIC_SECTION_NAME)) %>% #sections to leave out when summing; keeping separate to make change easier
  group_by(ITL221NM,SIC_SECTION_NAME) %>%
  summarise(
    total_employmentlastyear = sum(Employees_lastyear),
    total_employmentthisyear = sum(Employees_thisyear),
    employment_percentchange = percent_change(total_employmentlastyear,total_employmentthisyear),
    firmcount = n()
  ) %>% 
  filter(!is.na(SIC_SECTION_NAME))

#Save pre-processed summary to be used in report
# saveRDS(empchange.summary,'data/reportdata/ch_employmentpercentchange_itl2.rds')
# empchange.summary <- readRDS('data/reportdata/ch_employmentpercentchange_itl2.rds')
# 

#Plot
p <- ggplot(
  # empchange.summary %>% filter(firmcount > 19) %>%  mutate(SY = ITL221NM == 'Merseyside') %>% mutate(SIC_SECTION_NAME = fct_reorder(SIC_SECTION_NAME, employment_percentchange)),
  empchange.summary %>% filter(firmcount > 19) %>%  mutate(SY = ITL221NM == 'South Yorkshire') %>% mutate(SIC_SECTION_NAME = fct_reorder(SIC_SECTION_NAME, employment_percentchange)),
  aes(y = SIC_SECTION_NAME, x = employment_percentchange, group = ITL221NM, colour = SY, size = SY, alpha = SY)) +
  geom_jitter(width = 0.001, height = 0.1) +
  scale_color_manual(values = c('black','red')) +
  scale_alpha_manual(values = c(0.3,1)) +
  coord_cartesian(xlim = c(-25,25)) +
  geom_vline(xintercept = 0, alpha = 0.5, colour = 'green') +
  ylab("") +
  ggtitle('Percent change in employment between most recent accounts and previous year\nAll ITL2 zones, South Yorkshire overlaid in red - hover for place name')


#Who are those SY transportation/storage firms growing so much?
#Oh - it's firms with the wrong values in their employee fields... filtering accordingly
# ch.ec.10pluslastyr %>% filter(ITL221NM == 'South Yorkshire', qg('transport',SIC_SECTION_NAME)) %>% select(CompanyName,RegAddress.AddressLine1,incorporationdate_formatted,localauthority_name,Employees_thisyear,Employees_lastyear,employee_percentchange) %>% st_set_geometry(NULL) %>% View
ggplotly(p, width = 1200, height = 700, tooltip = 'ITL221NM')



#Version for four places vs...
empchange.summary <- ch.ec.10pluslastyr %>% 
  st_set_geometry(NULL) %>% 
  filter(!qg('households|extraterr|mining', SIC_SECTION_NAME)) %>% #sections to leave out when summing; keeping separate to make change easier
  group_by(localauthority_name,SIC_SECTION_NAME) %>%
  summarise(
    total_employmentlastyear = sum(Employees_lastyear),
    total_employmentthisyear = sum(Employees_thisyear),
    employment_percentchange = percent_change(total_employmentlastyear,total_employmentthisyear),
    firmcount = n()
  ) %>% 
  filter(!is.na(SIC_SECTION_NAME))

#Save pre-processed summary to be used in report
# saveRDS(empchange.summary,'data/reportdata/ch_employmentpercentchange_4places.rds')



#Plot
p <- ggplot() +
  geom_point(
    position = position_nudge(y = 0.2),
    # data = empchange.summary %>% filter(firmcount > 19),
    data = empchange.summary %>% filter(
      firmcount > 5,
      qg('Belfast|Birmingham|Bristol|Cardiff|Glasgow|Leeds|Liverpool|Manchester|Tyne|Nottingham', localauthority_name)#just core cities (minus four places below)
      ),
    aes(y = SIC_SECTION_NAME, x = employment_percentchange, group = localauthority_name),
    alpha = 0.5)

#Add four places
p <- p + 
  geom_point(
    position = position_nudge(y = -0.2),
    data = empchange.summary %>% filter(firmcount > 5, qg('sheff|rotherham|doncaster|barnsley',localauthority_name)),
    aes(y = fct_reorder(SIC_SECTION_NAME, employment_percentchange), x = employment_percentchange, colour = localauthority_name),
    size = 7, shape = 17) +
  scale_colour_brewer(palette = 'Paired', direction = 1) +
  coord_cartesian(xlim = c(-25,25)) +
  geom_vline(xintercept = 0, alpha = 0.5, colour = 'green') +
  ylab("") +
  ggtitle('Percent change in employment between most recent accounts and previous year\nCore cities, with SY four LAs overlaid, hover for place name')


#p

ggplotly(p, width = 1200, height = 900, tooltip = 'localauthority_name')


#Education in Doncaster??
ch.ec.10pluslastyr %>% filter(localauthority_name == 'Doncaster', qg('educ',SIC_SECTION_NAME))  %>% select(CompanyName,RegAddress.AddressLine1,incorporationdate_formatted,localauthority_name,Employees_thisyear,Employees_lastyear,employee_percentchange) %>% st_set_geometry(NULL) %>% View





#Save one for firms with fewer than 10 employees...
ch.ec.lessthan10lasty <- ch.ec %>% 
  filter(Employees_lastyear < 10) %>%
  mutate(employee_percentchange = percent_change(Employees_lastyear,Employees_thisyear))

empchange.summary <- ch.ec.lessthan10lasty %>% 
  st_set_geometry(NULL) %>% 
  filter(!qg('households|extraterr|mining', SIC_SECTION_NAME)) %>% #sections to leave out when summing; keeping separate to make change easier
  group_by(localauthority_name,SIC_SECTION_NAME) %>%
  summarise(
    total_employmentlastyear = sum(Employees_lastyear),
    total_employmentthisyear = sum(Employees_thisyear),
    employment_percentchange = percent_change(total_employmentlastyear,total_employmentthisyear),
    firmcount = n()
  ) %>% 
  filter(!is.na(SIC_SECTION_NAME))

#Save pre-processed summary to be used in report
# saveRDS(empchange.summary,'data/reportdata/ch_employmentpercentchange_4places_fewerthan10employees.rds')





# DIG MORE INTO EMPLOYEE PERCENT CHANGE TO LOOK FOR HIGH GROWTH SECTORS etc----

#Pick just on SY for now - look at single firms and their % change for one sector
sy.change <- ch.ec.10pluslastyr %>% 
  select(CompanyName,CompanyNumber,ITL221NM,localauthority_name,accountcode,incorporationdate_formatted,Employees_thisyear,Employees_lastyear,employee_percentchange, SIC_SECTION_NAME) %>% 
  filter(
    qg('south y', ITL221NM) 
    # qg('manuf', SIC_SECTION_NAME)
  )

#saveRDS(sy.change, 'data/reportdata/SY_allfirms_tenplusemployees_percentchangeemployees.rds')

#Interesting digging into the top grower there, SBD Apparel. Large growth after SCR 1.5M grant, now shrinking again?
#https://www.thestar.co.uk/business/sheffield-jobs-powerlifting-firm-sbd-apparel-set-to-make-dozens-redundant-after-move-to-ps9m-new-factory-4725400

#Percent change vs sector for SY?
p <- ggplot(sy.change,aes(y = SIC_SECTION_NAME, x = employee_percentchange, group = CompanyName)) +
  geom_jitter(height = 0.1, alpha = 0.3) +
  geom_vline(xintercept = 0, alpha = 0.5, colour = 'green') +
  ylab("") +
  ggtitle('Percent change in employment between most recent accounts and previous year\nAll SY firms with 10+ employees, hover for firm name')


ggplotly(p, width = 1200, height = 700, tooltip = 'CompanyName')







# MAP OF EMPLOYEE PERCENT CHANGE----

#Ooo I might want another map. Though if doing that, should sum employees per hex grid really
sq = st_make_grid(ch, cellsize = 1000, square = F)

#Turn into sf object so gridsquares can have IDs to group by
sq <- sq %>% st_sf() %>% mutate(id = 1:nrow(.))

#Intersection... as we're summing all, keep firms all firms with at least one employee in both time periods
overlay <- st_intersection(ch.ec,sq)

#Try some other sectors
# overlay <- st_intersection(ch.ec %>% filter(qg('manuf',SIC_SECTION_NAME)),sq)
# overlay <- st_intersection(ch.ec %>% filter(qg('information',SIC_SECTION_NAME)),sq)
# overlay <- st_intersection(ch.ec %>% filter(qg('public',SIC_SECTION_NAME)),sq)

#And places
overlay <- st_intersection(ch.ec %>% filter(
  # qg('information',SIC_SECTION_NAME),
  qg('yorkshire|manchester',ITL221NM)
  ),
  sq)

#This no longer needs to be geo, which will speed up
#Can link back to grids once done
empchange.summary <- overlay %>% 
  st_set_geometry(NULL) %>% 
  filter(!qg('households|extraterr', SIC_SECTION_NAME)) %>% #sections to leave out when summing; keeping separate to make change easier
  group_by(id) %>%
  summarise(
    total_employmentlastyear = sum(Employees_lastyear),
    total_employmentthisyear = sum(Employees_thisyear),
    employment_percentchange = percent_change(total_employmentlastyear,total_employmentthisyear),
    firmcount = n()
  ) %>% 
  filter(firmcount >= 5)

#Link that back into the grid squares...
#Use right join to drop empties
sq.map <- sq %>% 
  right_join(
    empchange.summary,
    by = 'id'
  ) %>% 
  mutate(
    hoverdisplay = paste0('Employee percent chage: ', round(employment_percentchange,1), '%, firm count: ', firmcount)
  )


tmap_mode('view')

#Median age
# tm_shape(sq.map) +
#   tm_polygons('median_age_of_firm_years', , id="hoverdisplay", col_alpha = 0, fill_alpha = 0.65, fill.scale = tm_scale_intervals(style = 'fisher', n = 11, values = "-orange_blue_diverging")) +
#   tm_shape(itl2) +
#   tm_borders(col = 'black')

#Firm count
tm_shape(sq.map) +
  tm_polygons('employment_percentchange', , id="hoverdisplay", col_alpha = 0, fill.scale = tm_scale_intervals(style = 'fisher', n = 11, midpoint = 0, values = "rd_yl_bu")) +
  # tm_polygons('employment_percentchange', , id="hoverdisplay", col_alpha = 0, fill_alpha = 0.65, fill.scale = tm_scale_intervals(style = 'fisher', n = 11, values = "-rd_yl_bu")) +
  # tm_view() +
  tm_shape(itl2) +
  tm_borders(col = 'black')






# LOOK AT SOME FIRMS----

#Fourjaw: ICT is first category, do they put themselves in manuf second?
ch %>% filter(qg('fourjaw', CompanyName)) %>% select(CompanyName,RegAddress.AddressLine1,incorporationdate_formatted,localauthority_name,Employees_thisyear,Employees_lastyear,SICCode.SicText_1:SICCode.SicText_4) %>% st_set_geometry(NULL) %>% View















