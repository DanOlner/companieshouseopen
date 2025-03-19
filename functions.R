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
  
  return(list(Company = company_name, enddate = enddate, dormantstatus = dormant, Employees_thisyear = as.numeric(employeevals[1]), Employees_lastyear = as.numeric(employeevals[2])))

}


