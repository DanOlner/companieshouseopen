## COMPANIES HOUSE OPEN

OK, so it's already open data (see below) - but it's not very accessible. What this repo does:

* Code for getting company account information for the latest live Companies House (CH) data, then extracting useful data from those accounts, geocoding it and doing a few other useful things. End result: nice friendly open Companies House data that can be used for regional analysis much more easily.





## THINGS ABOUT THE CODE

The [wrangling](https://github.com/DanOlner/companieshouseopen/tree/master/wrangling) folder contains each processing stage. Those are (in rough order, though some can be run whenever useful):

1. **[download_current_CH_livelist_n_save_reducedcopy_locally.R](https://github.com/DanOlner/companieshouseopen/blob/master/wrangling/download_current_CH_livelist_n_save_reducedcopy_locally.R)** -- Gets the latest CH live list of companies from the website, keeps the useful columns and saves as a compressed RDS locally. Creates a 'local' folder to put this in if it doesn't exist.
2. 
 