#GET CURRENT LIVE LIST OF COMPANIES DIRECT FROM COMPANIES HOUSE
#Reduce to just the useful columns and save locally
#Create local directories if they're not already present
library(tidyverse)
library(xml2)

# GET COMPANIES LIVE LIST, KEEP USEFUL COLUMNS, SAVE LOCALLY----

#Get zip direct from website
#The date on the download name changes so will have to get newest version each time...

# Load the HTML page that contains the download link
doc <- read_html("https://download.companieshouse.gov.uk/en_output.html")

# Extract all <a> tags with hrefs containing ""
#Should just be one attribute for the zip with the latest companies list
#Only the date will vary
zip_links <- xml_attr(
  xml_find_all(doc, "//a[contains(@href, 'BasicCompanyDataAsOneFile') and contains(@href, '.zip')]"),
  "href"
)

#Temporary zip download
#Via https://stackoverflow.com/a/79311678/5023561
#Zip is around 0.5gb
url1 <- paste0('https://download.companieshouse.gov.uk/',zip_links[1])
p1f <- tempfile(fileext=".zip")
download.file(url1, p1f, mode="wb")

#Stick into dataframe
ch <- read_csv(p1f)

#If need to save full version for checks elsewhere
# saveRDS(ch,'local/ch_livelistcheck.rds')

#3.8gb prior to column reduction
#(March 2025)
# pryr::object_size(ch)

# #reduce to only the columns we need for now and resave for ease of loading
ch <- ch %>% select(CompanyName,CompanyNumber,RegAddress.CareOf,RegAddress.AddressLine1,RegAddress.AddressLine2,RegAddress.PostCode,RegAddress.PostTown,CompanyCategory,CompanyStatus,CountryOfOrigin,IncorporationDate,SICCode.SicText_1:SICCode.SicText_4,URI)
# 
# ... 2gb after
# pryr::object_size(ch)

#SAVE AS COMPRESSED R OBJECT 
#Save locally; check 'local' folder exists and make if not
if (!dir.exists('local')) dir.create('local')

#Use filename including date used on CH website
#reduces to 188mb as of March 2025
saveRDS(
  ch,
  paste0('local/',gsub(pattern = 'zip',replacement = 'rds', x = zip_links[1])) 
  )





