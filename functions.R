#functions
library(tidyverse)
library(sf)
library(parallel)
library(httr)
library(jsonlite)
library(xml2)

#Helper functions
g <- function(x) glimpse(x)


#OSM postcode fetch
get_fulladdress_from_osm <- function(business_name, place_name, quotebusiness = F) {
  
  base_url <- "https://nominatim.openstreetmap.org/search"  
  
  # if(quotebusiness){
    # query <- paste('"',business_name,'"', city_name, sep = ", ")
  # } else {
  query <- paste(business_name, place_name, sep = ", ")
  # }
  
  #Limit to 1, get top one
  response <- GET(base_url, query = list(q = query, format = "json", limit = 1))
  
  #return the full result. If length is zero, can check later
  fromJSON(content(response, as = "text"))
  
}



#Function to get the UK postcode using Google Places API
get_uk_postcode_google_places_new <- function(business_name, place_name, api_key) {
  
  base_url <- "https://places.googleapis.com/v1/places:searchText"
  
  # Create the request body (the JSON object with the textQuery)
  body <- list(
    textQuery = paste(business_name, place_name,"UK",sep=",") 
  )
  
  # Set the headers including the API key and Content-Type
  headers <- add_headers(
    "Content-Type" = "application/json",
    "X-Goog-Api-Key" = api_key,
    "X-Goog-FieldMask" = "places.addressComponents"
    # "X-Goog-FieldMask" = "places.displayName,places.formattedAddress"
  )
  
  # Send the POST request
  response <- POST(base_url, 
                   body = body, 
                   encode = "json",  # Ensures the body is sent as JSON
                   headers)
  
  # Check the status and content of the response
  status_code(response)
  # content(response, "text")  # Prints the content of the response
  
  #Parsed as list, no json messin
  result <- content(response, "parsed")  # Prints the content of the response
  
  #Postcode doesn't stay in the same list index
  # result$places[[1]]$addressComponents[[8]]$longText
  
  #One easy way rather than dig through list and backtrack:
  #convert to data frame, find right column, postcode is previous column
  df <- result %>% data.frame()
  
  which(names(df) == 'places.addressComponents..postal_code.')
  
  #previous column should contain postcode = return this
  df[,which(names(df) == 'places.addressComponents..postal_code.')-1]
  
}






#Via https://stackoverflow.com/questions/51828712/r-regular-expression-for-extracting-uk-postcode-from-an-address-is-not-ordered
address_to_postcode <- function(addresses) {
  
  # 1. Convert addresses to upper case
  addresses = toupper(addresses)
  
  # 2. Regular expression for UK postcodes:
  pcd_regex = "[Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z])))) {0,1}[0-9][A-Za-z]{2})"
  
  # 3. Check if a postcode is present in each address or not (return TRUE if present, else FALSE)
  present <- grepl(pcd_regex, addresses)
  
  # 4. Extract postcodes matching the regular expression for a valid UK postcode
  postcodes <- regmatches(addresses, regexpr(pcd_regex, addresses))
  
  # 5. Return NA where an address does not contain a (valid format) UK postcode
  postcodes_out <- list()
  postcodes_out[present] <- postcodes
  postcodes_out[!present] <- NA
  
  # 6. Return the results in a vector (should be same length as input vector)
  return(do.call(c, postcodes_out))
}




sectorpairs_distancezscores <- function(famedata,sectortype,sectorname1,sectorname2, permutenumber = 1000){
  
  sectortype <- enquo(sector_name)
  
  #Calculate pairwise distances between firms in sector x and sector y
  sector_x <- famedata %>% filter(grepl(sectorname1,!!sectortype))
  sector_y <- famedata %>% filter(grepl(sectorname2,!!sectortype))
  
  #Get distances between all firms in both of the sector pairs
  dist_matrix <- st_distance(sector_x, sector_y)
  
  
  #For the null, get equivalent number of distances between random firms
  #Repeat to get distributions of means and density peaks
  
  #Faster parallel
  num_cores <- detectCores()
  
  #~3x time savings for higher iterations, lower for lower iterations
  matrices_list <- mclapply(1:permutenumber, function(x) repeatrandomfirms(famedata,sector_x,sector_y), mc.cores = num_cores)
  
  # Convert each matrix to a vector and combine them into a single vector
  # combined_vector <- do.call(c, lapply(matrices_list, as.vector))
  
  #Extract means for each of the iterations to get spread for z score / t value
  permuted_means <- sapply(matrices_list, mean)
  
  #Get z score for means
  zscore_means <- (mean(as.numeric(dist_matrix)) - mean(permuted_means))/sd(permuted_means)
  
  
  
  
  #Get density max point for pair
  max_density_value <- getmaxdensity(dist_matrix)
  
  #Repeat for permuted random firms
  permuted_max_density_vals <- sapply(matrices_list, getmaxdensity)

  
  
  
}




repeatrandomfirms <- function(famedata,sector_x,sector_y){
  
  sector_x_randomfirms <- famedata[sample(nrow(famedata),nrow(sector_x)),]
  sector_y_randomfirms <- famedata[sample(nrow(famedata),nrow(sector_y)),]
  
  return(st_distance(sector_x_randomfirms, sector_y_randomfirms))
  
}


getmaxdensity <- function(values){
  
  density_est <- density(values)
  
  # Find the value at which the density is maximized
  density_est$x[which.max(density_est$y)]
  
}


#ONS BUSINESS DEMOGRAPHY MANUAL AGGREGATION OF LAs
#E.g. separate LAs in 2017 aggregated counts for separate LAs that became unitary/combined authorities
#Columns to sum will be any that aren't 'code' or 'region'
#Some larger units won't match some years, but this should get them all to a harmonised geography
harmonise_localauthorities <- function(df){
  
  #Get local authority 2022 list to filter down
  la22 <- read_csv('data/Local_Authority_Districts_December_2022_UK.csv') %>% mutate(LAD22NM = gsub(",", "", LAD22NM))
  
  #create groups based on 2021-22 local authority groupings
  #Note: https://stackoverflow.com/a/26813671/5023561
  #"^ Asserts that we are at the start. $ Asserts that we are at the end."
  #(Stops Northampton from e.g. pulling in North Northamptonshire)
  df <- df %>% 
    mutate(
      group_las = case_when(
        grepl('bournemouth|christch|poole',region,ignore.case = T) ~ 'Bournemouth Christchurch and Poole',
        grepl('Aylesbur|Chiltern|South Bu|Wycombe', region, ignore.case = T) ~ 'Buckinghamshire',
        grepl('East Dor|North Dor|Purbeck|West Dor|Weymouth', region, ignore.case = T) ~ 'Dorset',
        grepl('Suffolk Coastal|Waveney', region, ignore.case = T) ~ 'East Suffolk',
        grepl('Edmundsbury|Forest Heath', region, ignore.case = T) ~ 'West Suffolk',
        grepl('Corby|East Northamptonshire|Kettering|Wellingborough', region, ignore.case = T) ~ 'North Northamptonshire',#changed in 2021
        grepl('Daventry|^Northampton$|South Northamptonshire', region, ignore.case = T) ~ 'West Northamptonshire',#changed in 2021
        grepl('Taunton|West Somerset', region, ignore.case = T) ~ 'Somerset West and Taunton',#changed in 2021
        
        grepl('Shepway', region, ignore.case = T) ~ 'Folkestone and Hythe',#"Folkestone and Hythe" --> purely a rename from Shepway in earlier time points
        region == 'Herefordshire' ~ "Herefordshire County of",# "Herefordshire County of" --> just named differently, "Herefordshire" in earlier data.
        
        .default = region
      )
    )
  
  #Aggregate counts in any columns that are not code or region
  df <- df %>% 
    group_by(group_las) %>% 
    summarise(across(c(-code,-region), sum)) %>% 
    rename(region = group_las)
  
  #Keep only LAs in 2022, not other aggregates. Can aggregate MCAs etc ourselves later
  df %>% filter(region %in% la22$LAD22NM)
  
}



#Helper function for reading in from the ONS business demography xlsx
firm_read <- function(sheetname){ 
  
  x <- readxl::read_excel('localdata/businessdemographyexceltables2022.xlsx', range = paste0(sheetname,"!A4:D500")) %>% 
    rename(code = ...1, region = ...2) %>% 
    mutate(region = gsub("\\*|,", "", region))#remove offending characters that stop full matching
  
  # x <- x %>% 
  #   filter(code!='Footnotes:')
  
  #Sometimes, just once in fact, there's a 'footnotes' column that messes things up in 'code'
  #So use 'region' for this check
  #drop any extra NA rows at the end, using arbitrary column
  x <- x %>% filter(!is.na(region))
  
  #Also might be extraneous columns...
  #https://forum.posit.co/t/modern-updated-dplyr-way-to-remove-columns-with-na-values/150552/3
  x %>% select(where(~ !any(is.na(.))))

}



#Get all harmonised firm data from demography excel, all in one function
get_all_firm_demography_data <- function(sheetsearchtext, returnlong = F){
  
  #Get sheet names
  #Remove ones we don't want (sic2007 case varies, obv!)
  #Leaving out SIC codes and firm survival for now
  contents <- readxl::read_excel('localdata/businessdemographyexceltables2022.xlsx',
                                 range = "Contents!A5:B45", 
                                 col_names = c('table','contents')) %>% 
    filter(!grepl('SIC2007', contents, ignore.case  = T))
  
  
  #Get local authority 2022 list to test we got matches
  la22 <- read_csv('data/Local_Authority_Districts_December_2022_UK.csv') %>% mutate(LAD22NM = gsub(",", "", LAD22NM))
  
  #Search for specific sheets to combine
  #Amazingly, names aren't case consistent across sheets...
  sheets_to_get <- contents %>% 
    filter(grepl(sheetsearchtext, contents, ignore.case = T)) %>% 
    select(table) %>% 
    pull() %>% 
    as.list
  
  # debugonce(firm_read)
  list_of_sheets <- purrr::map(sheets_to_get, firm_read)
  
  #Drop straight into harmonising function...
  death_sheet_list <- purrr::map(list_of_sheets, harmonise_localauthorities)
  
  #All good?
  print('Local authority matches?')
  print(table(la22$LAD22NM %in% death_sheet_list[[1]]$region))
  print(table(la22$LAD22NM %in% death_sheet_list[[2]]$region))
  print(table(la22$LAD22NM %in% death_sheet_list[[3]]$region))
  print(table(la22$LAD22NM %in% death_sheet_list[[4]]$region))
  
  #Now just need to merge on region / possibly add LA codes back in
  combined <- purrr::reduce(death_sheet_list, left_join, by = 'region')
  
  #Add LA code back in, return df
  combined <- combined %>% 
    left_join(
      la22 %>% select(code = LAD22CD, region = LAD22NM)
    ) %>% 
    relocate(code)
  
  if(returnlong){
  
  #Make long, years in one column, make them numeric
    combined <- combined %>% 
    pivot_longer(
      cols = contains('20'), names_to = 'year', values_to = 'count'
    ) %>% 
      mutate(year = as.numeric(year))
    
    return(combined)
    
  } else {
    
    return(combined)
    
  }

}






#Get two-variable regression slope and SE safely (don't break if no result returned)
get_slope_and_se_safely <- function(data, ..., y, x) {
  
  groups <- quos(...)  
  y <- enquo(y)
  x <- enquo(x) 
  
  #Function to compute slope
  get_slope_and_se <- function(data) {
    
    model <- lm(data = data, formula = as.formula(paste0(quo_name(y), " ~ ", quo_name(x))))
    
    slope <- coef(model)[2]
    se <- summary(model)$coefficients[2, 2]
    return(list(slope = slope, se = se))
    
  }
  
  #Make it a safe function using purrr::possibly
  safe_get_slope <- possibly(get_slope_and_se, otherwise = list(slope = NA, se = NA))
  
  #Group and summarize
  data %>%
    group_by(!!!groups) %>%
    nest() %>%
    mutate(result = map(data, safe_get_slope)) %>% 
    mutate(slope = map_dbl(result, "slope"),
           se = map_dbl(result, "se")) %>%
    select(-data, -result)
  
}




#READ COMPANIES HOUSE ACCOUNTS FILE AND EXTRACT DATA FOR WHATEVER YEARS WE CAN GET (OFTEN 2)
#If zip location supplied, extract file directly from zip, to avoid having to unzip whole thing
get_accounts_data <- function(filename, ziplocation = NULL){

  if(is.null(ziplocation)){
    doc <- xml2::read_xml(filename)
  } else {
    doc <- xml2::read_xml(unz(ziplocation, filename))
  }
  
  # Dynamically extract all namespaces
  ns <- xml_ns(doc)
  
  
  
  enddate <- xml_text(
    xml_find_first(
      doc,
      "//ix:nonNumeric[contains(@name, 'EndDateForPeriodCoveredByReport')]",
      # ns = c(ix = "http://www.xbrl.org/2013/inlineXBRL")
      ns = ns
    )
  )
  
  #//ix:nonNumeric[contains(@name, 'EntityCurrentLegalOrRegisteredName')] matches any <ix:nonNumeric> tag where the name attribute contains EntityCurrentLegalOrRegisteredName
  company_name <- xml_text(
    xml_find_first(
      doc,
      "//ix:nonNumeric[contains(@name, 'EntityCurrentLegalOrRegisteredName')]",
      # ns = c(ix = "http://www.xbrl.org/2013/inlineXBRL")
      ns = ns
    )
  )
  
  
  #Is company dormant?
  dormant <- xml_text(
    xml_find_first(
      doc,
      "//ix:nonNumeric[contains(@name, 'EntityDormantTruefalse')]",
      ns = ns
    )
  )
  
  
  #Get any employee values (max two per account submitted - this and last year - and a lot with just one year)
  employeevals <- xml_text(
    xml_find_all(
      doc,
        "//ix:nonFraction[contains(@name, 'AverageNumberEmployeesDuringPeriod')]",
      ns = ns
    )
  )
  
  # Extract employee numbers for 2024 and 2023
  # employees_2024 <- xml2::xml_text(xml2::xml_find_first(doc, "//ix:nonFraction[@name='core:AverageNumberEmployeesDuringPeriod' and @contextRef='D0']"))
  # employees_2023 <- xml2::xml_text(xml2::xml_find_first(doc, "//ix:nonFraction[@name='core:AverageNumberEmployeesDuringPeriod' and @contextRef='D11']"))
  
  #Need to check it's getting the correct year order if more than one employee count
  return(list(Company = company_name, enddate = enddate, dormantstatus = dormant, Employees_thisyear = as.numeric(employeevals[1]), Employees_lastyear = as.numeric(employeevals[2])))

}


