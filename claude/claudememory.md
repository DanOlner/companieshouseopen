# Companies House Open Data Project - Reference

This doc is mainly claude-generated.

## Main Dataset: `ch` (3.2 million rows)

Loaded from: `local/PROCESSED_accountextracts_n_livelist_geocoded_combined_Oct2025.rds`

Sample at: `local/sample_ch.csv` (500 rows)

### Data Sources Combined

This dataset merges three sources:
1. **Companies House live list** - basic company registration data
2. **Geocoding** - postcode lookup to local authorities and ITL2 regions
3. **Account extracts** - employee counts and dormancy status from iXBRL filings

### Column Reference

| Column | Description |
|--------|-------------|
| `CompanyName` | Registered company name |
| `CompanyNumber` | Unique CH identifier (e.g., "14006481", "SC666382" for Scottish) |
| `RegAddress.*` | Registered address fields (CareOf, AddressLine1, AddressLine2, PostCode, PostTown) |
| `CompanyCategory` | Company type (e.g., "Private Limited Company", "PRI/LTD BY GUAR/NSC") |
| `CompanyStatus` | Status (e.g., "Active") |
| `CountryOfOrigin` | Usually "United Kingdom" |
| `IncorporationDate` | Date incorporated (DD/MM/YYYY format) |
| `SICCode.SicText_1` to `_4` | Up to 4 SIC codes with descriptions |
| `URI` | Companies House data URI |
| `postcode` | Cleaned postcode (no spaces) |
| `localauthority_code` | ONS LA code (e.g., "E08000015") |
| `localauthority_name` | LA name (e.g., "Wirral") |
| `ITL221CD` | ITL2 region code (e.g., "TLD7") |
| `ITL221NM` | ITL2 region name (e.g., "Merseyside") |
| `geometry` | Easting/Northing coordinates as text |
| `accountcode` | Account filing date code (YYYYMMDD format) |
| `dormantstatus` | "true"/"false" - whether company is dormant |
| `Employees_thisyear` | Employee count from latest accounts (NA if not reported) |
| `Employees_lastyear` | Employee count from previous year |
| `enddate` | Account period end date (ISO format) |
| `SIC_5DIGIT_CODE` | Primary 5-digit SIC code |
| `SIC_5DIGIT_NAME` | SIC code description |
| `SIC_2DIGIT_CODE` | 2-digit SIC division |
| `SIC_2DIGIT_NAME` | Division name |
| `SIC_3DIGIT_CODE` | 3-digit SIC group |
| `SIC_3DIGIT_NAME` | Group name |
| `SIC_SECTION_LETTER` | SIC section letter (A-S) |
| `SIC_SECTION_CODE` | Section with range (e.g., "G (45-47)") |
| `SIC_SECTION_NAME` | Section name (e.g., "Wholesale and retail trade") |
| `incorporationdate_formatted` | ISO formatted incorporation date |
| `age_of_firm_years` | Calculated firm age in years |

### Key Characteristics

- **Coverage**: ~3.2 million UK firms with submitted accounts
- **Employee data**: Many firms report 0 or NA (dormant/micro entities don't always report)
- **Dormant firms**: Marked with `dormantstatus = "true"`, often have NA employee counts
- **Geography**: Geocoded via postcode to LA and ITL2 regions (~95% match rate)
- **SIC codes**: Hierarchical from 5-digit (most specific) to section letter (broadest)

### Common Filters

```r
# Active non-dormant firms with 10+ employees
ch %>% filter(dormantstatus == "false", Employees_thisyear >= 10)

# Firms in a specific sector (e.g., Manufacturing = Section C)
ch %>% filter(SIC_SECTION_LETTER == "C")

# Firms in South Yorkshire
ch %>% filter(ITL221NM == "South Yorkshire")
```

---

## Sector Classification via Website Text (Data City / RTIC approach)

Summary of how commercial providers infer bespoke sector types beyond SIC codes.

### Key Finding: Website text is primary, not accounts

Data City's "Real Time Industrial Classifications" (RTICs) are built primarily from **company website text**, not accounts filings. The [Cambridge Econometrics government report](https://assets.publishing.service.gov.uk/media/6880db342b6fd60b7c160f34/250723_Defining_and_Measuring_the_UK_Digital_Economy_publish.pdf) states RTICs use "machine learning and website scraping" to "group companies that describe their activity similarly in their website text."

### The RTIC Pipeline

1. **Company ↔ URL matching** - Link Companies House entity to a website (the hardest part)
2. **Website text capture** - Scrape homepage + key pages
3. **Taxonomy + training set** - Define sectors, create small training sets (10-20 sites per vertical)
4. **Supervised text classification** - ML model trained on website language
5. **Expert QA loop** - Manual checking of samples, ~90% accuracy threshold

### Role of Companies House Data

- **Entity anchoring**: CH identifiers used to match/validate website-to-company links
- **Standard variables**: Employee counts, incorporation date, registered address for analysis
- **NOT for classification**: No evidence they parse iXBRL narrative text to classify sectors

### Implications for DIY Approach

A lightweight version would need:
- Reliable company → URL matching (often the bottleneck)
- Website text extraction
- Embeddings + similarity or simple supervised classifier
- Manual QA on samples

Firms without a matched website cannot be captured by this approach.

### Sources
- [Cambridge Econometrics: Defining UK Digital Economy](https://assets.publishing.service.gov.uk/media/6880db342b6fd60b7c160f34/250723_Defining_and_Measuring_the_UK_Digital_Economy_publish.pdf)
- [Data City: Behind the Scenes of RTIC Creation](https://thedatacity.com/blog/behind-the-scenes-of-rtic-creation/)
- [Beauhurst: Companies House Data](https://www.beauhurst.com/blog/companies-house-data/)

---

## Company → Website Matching in R

Ideas for finding and validating company websites from Companies House names.

### Step 1: Search for Websites

**Option A: Google Custom Search API**
```r
library(httr2)

google_search <- function(query, api_key, cx) {
  resp <- request("https://www.googleapis.com/customsearch/v1") |>
    req_url_query(
      key = api_key,
      cx = cx,
      q = query,
      num = 5  # top 5 results
    ) |>
    req_perform() |>
    resp_body_json()

  # Extract URLs and titles
  map_dfr(resp$items, ~ tibble(
    title = .x$title,
    url = .x$link,
    snippet = .x$snippet
  ))
}

# Usage: search with company name + "UK"
results <- google_search("Gripple Limited UK", api_key, cx)
```
- Free tier: 100 queries/day
- Paid: $5 per 1000 queries after that
- Setup: https://developers.google.com/custom-search/v1/overview

**Option B: SerpAPI (wrapper for multiple engines)**
```r
serp_search <- function(query, api_key) {
  resp <- request("https://serpapi.com/search") |>
    req_url_query(
      api_key = api_key,
      engine = "google",
      q = query,
      location = "United Kingdom"
    ) |>
    req_perform() |>
    resp_body_json()

  map_dfr(resp$organic_results, ~ tibble(
    title = .x$title,
    url = .x$link,
    snippet = .x$snippet
  ))
}
```
- Free tier: 100 searches/month
- More reliable than scraping Google directly

**Option C: DuckDuckGo (no API, but scrapeable)**
```r
library(rvest)

ddg_search <- function(query) {
  url <- paste0("https://html.duckduckgo.com/html/?q=", URLencode(query))
  page <- read_html(url)

  tibble(
    title = page |> html_elements(".result__title") |> html_text(),
    url = page |> html_elements(".result__url") |> html_attr("href")
  )
}
```
- Free but rate-limited; be polite with delays
- Less precise than Google for UK companies

### Step 2: Select Best URL Candidate

**Heuristics to score/filter results:**

```r
score_url_match <- function(company_name, url, title, snippet) {
  score <- 0
  name_clean <- tolower(gsub(" (LIMITED|LTD|PLC)$", "", company_name, ignore.case = TRUE))
  name_words <- unlist(strsplit(name_clean, " "))

  # 1. Company name words in domain (+3 each)
  domain <- urltools::domain(url)
  for (word in name_words) {
    if (grepl(word, domain, ignore.case = TRUE)) score <- score + 3
  }

  # 2. .co.uk domain (+2)
  if (grepl("\\.co\\.uk$", domain)) score <- score + 2

  # 3. Company name in title (+2)
  if (grepl(name_clean, tolower(title))) score <- score + 2

  # 4. Penalise aggregator sites (-5)
  aggregators <- c("linkedin.com", "facebook.com", "yell.com", "endole.co.uk",
                   "companieshouse.gov.uk", "duedil.com", "checkacompany.co.uk")
  if (any(sapply(aggregators, function(a) grepl(a, domain)))) score <- score - 5

  return(score)
}

# Pick top-scoring URL
results <- results |>
  mutate(score = pmap_dbl(list(company_name, url, title, snippet), score_url_match)) |>
  arrange(desc(score))

best_url <- results$url[1]
```

### Step 3: Validate the Match

**Check website content for confirmation:**

```r
library(polite)  # respectful scraping

validate_website <- function(url, company_name, postcode = NULL, company_number = NULL) {
  session <- bow(url, user_agent = "Academic research bot")
  page <- scrape(session)

  if (is.null(page)) return(list(valid = FALSE, confidence = 0))

  text <- page |> html_text2() |> tolower()

  checks <- c(
    name_found = grepl(tolower(company_name), text),
    postcode_found = if (!is.null(postcode)) grepl(tolower(postcode), text) else NA,
    company_number_found = if (!is.null(company_number)) grepl(company_number, text) else NA
  )

  confidence <- sum(checks, na.rm = TRUE) / sum(!is.na(checks))

  list(
    valid = confidence >= 0.5,
    confidence = confidence,
    checks = checks
  )
}
```

### Step 4: Extract Website Text for Classification

```r
extract_site_text <- function(url) {
  session <- bow(url)
  page <- scrape(session)

  if (is.null(page)) return(NA_character_)

  # Remove script/style content
  page |>
    html_elements("body") |>
    html_text2() |>
    str_squish()
}

# For deeper extraction, also grab key pages
extract_about_page <- function(base_url) {
  about_patterns <- c("/about", "/about-us", "/company", "/who-we-are")

  for (pattern in about_patterns) {
    try_url <- paste0(base_url, pattern)
    text <- extract_site_text(try_url)
    if (!is.na(text) && nchar(text) > 100) return(text)
  }

  return(NA_character_)
}
```

### Practical Considerations

| Issue | Mitigation |
|-------|------------|
| Rate limits | Use `Sys.sleep(2)` between requests; `polite` package handles this |
| API costs | Start with free tiers; batch queries; cache results |
| False matches | Use multiple validation checks (name, postcode, company number) |
| No website | Many small firms don't have one; flag as "unmatched" |
| Aggregator sites | Explicitly filter LinkedIn, Yell, etc. |
| Redirects/errors | Wrap in `tryCatch`; log failures for manual review |

### Suggested Workflow for Testing

```r
# 1. Start with a small sample (e.g., 50 firms with 100+ employees)
test_firms <- ch |>
  filter(Employees_thisyear >= 100, ITL221NM == "South Yorkshire") |>
  sample_n(50)

# 2. Search + score + validate
results <- test_firms |>
  mutate(
    search_results = map(CompanyName, ~ google_search(paste(.x, "UK"))),
    best_url = map_chr(search_results, ~ .x$url[which.max(.x$score)]),
    validation = pmap(list(best_url, CompanyName, postcode, CompanyNumber), validate_website)
  )

# 3. Manual review of low-confidence matches
results |> filter(map_dbl(validation, "confidence") < 0.5) |> View()
```

### R Packages Needed

```r
install.packages(c("httr2", "rvest", "polite", "urltools"))
```

---

## Domain-Guess-Then-Verify Approach

A cost-saving strategy: guess plausible domains from company names before falling back to search APIs. Can cut API calls by 80-90% for firms with "clean" names.

### The Idea

1. **Normalise** the company name (strip LTD/LIMITED/PLC, punctuation, "THE", etc.)
2. **Generate candidate domains** (name.co.uk, name.com, etc.)
3. **Check if domain exists** (DNS lookup or HTTP HEAD request)
4. **Validate** it's the right firm (check for company name, postcode, company number on page)
5. **Only if validation fails**, fall back to search API

### Why This Works

- Many UK companies use predictable domain patterns: `companyname.co.uk`
- Larger/established firms especially likely to have matching domains
- DNS/HTTP checks are free and fast
- Saves search API quota for genuinely hard cases

### R Implementation

```r
library(httr2)
library(rvest)
library(curl)

# Step 1: Normalise company name to domain-friendly string
normalise_for_domain <- function(company_name) {
  name <- company_name |>
    tolower() |>
    # Remove common suffixes
    str_remove_all("\\s*(limited|ltd|plc|llp|lp|inc|corporation|corp)\\s*$") |>
    # Remove "the" at start
    str_remove("^the\\s+") |>
    # Remove punctuation
    str_remove_all("[^a-z0-9\\s]") |>
    # Collapse whitespace
    str_squish()

  # Create variants
  list(
    joined = str_remove_all(name, "\\s"),           # "acmewidgets"
    hyphenated = str_replace_all(name, "\\s", "-"), # "acme-widgets"
    first_word = str_extract(name, "^\\w+")         # "acme"
  )
}

# Step 2: Generate candidate domains
generate_domain_candidates <- function(company_name) {
  variants <- normalise_for_domain(company_name)

  tlds <- c(".co.uk", ".com", ".uk", ".org.uk", ".net")

  # Generate all combinations
  candidates <- expand.grid(
    name = unlist(variants),
    tld = tlds,
    stringsAsFactors = FALSE
  ) |>
    mutate(domain = paste0(name, tld)) |>
    pull(domain) |>
    unique()

  return(candidates)
}

# Step 3: Check if domain exists (fast DNS check)
domain_exists <- function(domain) {
  tryCatch({
    # Try to resolve DNS
    nslookup(domain)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Alternative: HTTP HEAD request (slower but more reliable)
domain_responds <- function(domain, timeout = 5) {
  url <- paste0("https://", domain)

  tryCatch({
    resp <- request(url) |>
      req_timeout(timeout) |>
      req_method("HEAD") |>
      req_error(is_error = ~ FALSE) |>
      req_perform()

    return(resp_status(resp) < 400)
  }, error = function(e) {
    # Try http if https fails
    tryCatch({
      resp <- request(paste0("http://", domain)) |>
        req_timeout(timeout) |>
        req_method("HEAD") |>
        req_error(is_error = ~ FALSE) |>
        req_perform()

      return(resp_status(resp) < 400)
    }, error = function(e) {
      return(FALSE)
    })
  })
}

# Step 4: Validate domain matches the company
validate_domain_match <- function(domain, company_name, postcode = NULL,
                                   company_number = NULL, timeout = 10) {
  url <- paste0("https://", domain)

  tryCatch({
    resp <- request(url) |>
      req_timeout(timeout) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()

    if (resp_status(resp) >= 400) return(list(valid = FALSE, confidence = 0))

    # Parse homepage text
    page_text <- resp |>
      resp_body_html() |>
      html_text2() |>
      tolower()

    # Check for anchors
    name_clean <- tolower(str_remove(company_name, "\\s*(LIMITED|LTD|PLC)$"))

    checks <- c(
      name_found = grepl(name_clean, page_text, fixed = TRUE),
      postcode_found = if (!is.null(postcode)) {
        grepl(tolower(postcode), page_text, fixed = TRUE)
      } else NA,
      company_number_found = if (!is.null(company_number)) {
        grepl(company_number, page_text, fixed = TRUE)
      } else NA
    )

    confidence <- sum(checks, na.rm = TRUE) / sum(!is.na(checks))

    list(
      valid = confidence >= 0.5,
      confidence = confidence,
      checks = checks,
      url = url
    )

  }, error = function(e) {
    list(valid = FALSE, confidence = 0, error = as.character(e))
  })
}

# Step 5: Full pipeline - guess first, search if needed
find_company_website <- function(company_name, postcode = NULL,
                                  company_number = NULL,
                                  search_fn = NULL) {

  # Generate and test candidates
  candidates <- generate_domain_candidates(company_name)

  for (domain in candidates) {
    # Quick existence check
    if (!domain_responds(domain)) next

    # Validate it's the right company
    result <- validate_domain_match(domain, company_name, postcode, company_number)

    if (result$valid) {
      return(list(
        url = result$url,
        method = "domain_guess",
        confidence = result$confidence
      ))
    }
  }

  # Fall back to search API if provided
  if (!is.null(search_fn)) {
    search_results <- search_fn(paste(company_name, "UK"))
    if (nrow(search_results) > 0) {
      return(list(
        url = search_results$url[1],
        method = "search_api",
        confidence = NA
      ))
    }
  }

  return(list(url = NA, method = "not_found", confidence = 0))
}
```

### Usage Example

```r
# Test with a known company
result <- find_company_website(
  company_name = "Gripple Limited",
  postcode = "S9 1RS",
  company_number = "01772901"
)
# Should find gripple.com or gripple.co.uk without needing search API

# Batch process with fallback to Google search
results <- test_firms |>
  mutate(
    website = pmap(
      list(CompanyName, postcode, CompanyNumber),
      ~ find_company_website(..1, ..2, ..3, search_fn = google_search)
    )
  ) |>
  unnest_wider(website)

# Check how many avoided API calls
table(results$method)
# domain_guess: 85  <- free!
# search_api: 12    <- paid
# not_found: 3
```

### Optimisation Tips

| Tip | Why |
|-----|-----|
| Try `.co.uk` first | Most common for UK companies |
| Use DNS before HTTP | `nslookup` is faster than full request |
| Cache DNS results | Many firms share hosting; cache negative results too |
| Parallelise checks | `furrr::future_map` for batch processing |
| Set short timeouts | Don't wait 30s for dead domains |

### When This Doesn't Work

- **Generic names**: "Quality Services Ltd" → too many possible domains
- **Abbreviations**: "ABC Holdings" could be anything
- **Subsidiary companies**: May use parent company domain
- **Non-trading names**: Trading name differs from registered name
- **No website**: Small/dormant firms often don't have one

For these cases, fall back to search API or flag for manual review.

### Related Approaches / Sources

- [Company URL matching techniques](https://thedatacity.com/blog/mapping-company-distribution-using-registered-and-operating-addresses-2/) - Data City's approach
- [DNS lookup in R](https://cran.r-project.org/web/packages/curl/vignettes/intro.html) - `curl::nslookup()`
- [httr2 for HTTP requests](https://httr2.r-lib.org/) - Modern R HTTP client
