# Companies House Open Data Project - Reference

This doc is mainly claude-generated, with a few human additions / edits.

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

## ML/NLP Classification of Firms from Website Text

Approaches and research on using machine learning and NLP to classify companies into bespoke sectors beyond standard SIC codes, using website text as the primary data source.

### Why Website Text Beats SIC Codes

Standard Industrial Classification (SIC) codes have well-documented limitations for understanding firm activities:

- **Coarse granularity**: SIC codes are limited to ~700 categories, insufficient for emerging sectors like AI, cleantech, or health tech
- **Self-reported and outdated**: Firms choose their own codes at registration and rarely update them
- **"Other" category overuse**: Major companies like Amazon and Facebook fall into generic "other business services" categories
- **No innovation signal**: A biotech R&D firm and a pharmaceutical distributor may share the same code
- **Slow to adapt**: New industries (e.g., "prompt engineering", "carbon capture") have no SIC representation

Website text offers richer, more current information about what firms actually do. As [Marra & Baldassari (2022)](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0270041) note, this allows understanding "what firms do in a more penetrating and updated way than referring to standard industrial classification codes."

### The General Pipeline

Whether commercial or research, most approaches follow a similar pattern:

1. **Company ↔ URL matching** - Link Companies House entity to a website (often the hardest part)
2. **Website text capture** - Scrape homepage + key pages (about, services)
3. **Taxonomy definition** - Define target sectors with descriptions or example texts
4. **Text classification** - ML model trained on website language (embeddings, transformers, or few-shot)
5. **QA loop** - Manual checking of samples to refine training data

### Research Examples

#### Text Mining Instead of SIC Codes (Marra & Baldassari, 2022)

This [PLOS ONE study](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0270041) uses text mining and semantic algorithms to tag innovative firms from website and corporate purpose text. Key findings:

- Extracts keywords to generate tags for firms' activities, specialisations, and competences
- Enables measurement of "industrial proximity" by matching firms' keywords
- Tested on 583 innovative firms in Italy
- Code available on [GitHub](https://github.com/cbaldassari/2022-plosone)

#### BERT for Industry Classification (Jagrič & Herman, 2024)

This [MDPI study](https://www.mdpi.com/2078-2489/15/2/89) applies BERT to classify business descriptions into 13 industry categories:

- Achieved 83.5%-92.6% accuracy across industry classes (88.23% overall, F1=0.88)
- Can harness real-time web data for up-to-date classification
- Demonstrates transformer models work well for this task

#### Emerging Industry Classification with BERT (2024)

A [ScienceDirect paper](https://www.sciencedirect.com/science/article/pii/S030643792400142X) focuses on classifying emerging industries:

- Achieved 84.11%-99.66% accuracy across 16 industry classifications
- Identifies clusters of firms transcending existing classification systems
- Highlights how data-driven approaches adapt to changing industrial landscapes

#### Comparative NLP Models for Company Classification (2024)

This [MDPI comparative study](https://www.mdpi.com/2078-2489/15/2/77) tested multiple approaches:

- Used RoBERTa-base transformer on Compustat dataset (44,033 US companies)
- Enhanced zero-shot methodology using TF-IDF to extract sector-specific vocabulary
- Explored ChatGPT for dataset generation where company descriptions are lacking

### Few-Shot Learning with SetFit

[SetFit](https://huggingface.co/blog/setfit) is particularly well-suited for firm classification because:

- **Minimal labelled data**: Works with just 8-20 examples per class
- **No prompts needed**: Unlike GPT-3, doesn't require prompt engineering
- **Fast and cheap**: Trains in ~30 seconds on GPU, costs ~$0.025 vs $0.70 for T-Few
- **Competitive accuracy**: Outperforms GPT-3 on RAFT benchmark while being 1600x smaller
- **Calibrated probabilities**: Outputs meaningful confidence scores

Recent [ModernBERT integration](https://moshewasserblat.medium.com/new-results-on-setfit-modernbert-for-text-classification-with-few-shot-training-53c154df7c0e) (2024) shows 50% improvement over baselines in few-shot scenarios.

### Commercial Implementations

**The Data City - Real-Time Industrial Classifications (RTICs)**

The Data City developed RTICs as an alternative to SIC codes, classifying companies based on how they describe themselves on their websites. Their [methodology](https://thedatacity.com/blog/behind-the-scenes-of-rtic-creation/):

- **Website text as primary signal**: Scrapes up to 75 pages per company for 1.6+ million UK firms
- **Supervised ML approach**: Train classifier with positive examples (firms in target sector) and negative examples (firms outside sector) to identify discriminative keywords
- **Expert QA loop**: Industry experts define taxonomies, validate results, maintain 90% minimum confidence threshold
- **Multi-label classification**: Companies can belong to multiple RTICs, reflecting diverse business activities
- **Coverage**: 500+ industry classifications across 9 million UK companies
- **Update cycle**: Annual reviews with biannual updates for key sectors

The UK Government adopted RTICs for [measuring the digital economy](https://www.gov.uk/government/publications/defining-and-measuring-the-uk-digital-economy/defining-and-measuring-the-uk-digital-economy-phase-2-report), noting they "provide a means of capturing 'true' company activity, especially for frontier sectors and technologies."

**Dun & Bradstreet**

D&B ran a proof of concept using deep learning for SIC classification on UK Companies House data in 2018, in partnership with Evolution AI. Key details from [InformationWeek coverage](https://www.informationweek.com/machine-learning-ai/dun-bradstreet-eyes-blockchain-machine-learning-projects):

- Scraped website text for all UK companies and ran through neural network trained to determine company type
- Deep learning approach "truly understands context and nuance" - e.g., recognising "CNC" means different things in engineering vs policing contexts
- Results: 40% of primary SICs were changed to provide clearer classification; 6% were validated
- Estimated manual equivalent: 2.5 years and $10 million to update the entire UK database
- Adopted "human/machine hybrid approach" with algorithms as prep tools and humans for final verification

### Role of Companies House Data

- **Entity anchoring**: CH identifiers used to match/validate website-to-company links
- **Standard variables**: Employee counts, incorporation date, registered address for analysis
- **Validation signal**: Postcode on website confirms correct company-URL match
- **NOT for classification**: No evidence parsing iXBRL narrative text improves sector classification

### Implications for DIY Approach

A lightweight version needs:
- Reliable company → URL matching (often the bottleneck; ~40% success rate typical)
- Website text extraction (homepage + about page)
- Embeddings + similarity OR few-shot classifier (SetFit recommended)
- Manual QA on samples (~50-100 labelled examples for validation)

Firms without a matched website cannot be captured by this approach.

### Sources

**Research Papers**
- [Marra & Baldassari (2022): Using text data instead of SIC codes](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0270041) - PLOS ONE
- [Jagrič & Herman (2024): AI Model for Industry Classification](https://www.mdpi.com/2078-2489/15/2/89) - MDPI Information
- [Emerging Industry Classification with BERT (2024)](https://www.sciencedirect.com/science/article/pii/S030643792400142X) - Information Systems
- [Comparative Analysis of NLP Models (2024)](https://www.mdpi.com/2078-2489/15/2/77) - MDPI Information
- [Business Trajectories with Transformer Classification](https://arxiv.org/pdf/2306.10034) - arXiv

**Tools & Methods**
- [SetFit: Few-Shot Learning](https://huggingface.co/blog/setfit) - Hugging Face
- [SetFit with ModernBERT (2024)](https://moshewasserblat.medium.com/new-results-on-setfit-modernbert-for-text-classification-with-few-shot-training-53c154df7c0e) - Medium
- [AWS: Fine-tuning Sentence Transformers](https://aws.amazon.com/blogs/machine-learning/create-and-fine-tune-sentence-transformers-for-enhanced-classification-accuracy/) - AWS ML Blog

**Policy & Commercial**
- [Cambridge Econometrics: Defining UK Digital Economy](https://assets.publishing.service.gov.uk/media/6880db342b6fd60b7c160f34/250723_Defining_and_Measuring_the_UK_Digital_Economy_publish.pdf) - UK Government
- [UK Gov: Defining and Measuring the Digital Economy Phase 2](https://www.gov.uk/government/publications/defining-and-measuring-the-uk-digital-economy/defining-and-measuring-the-uk-digital-economy-phase-2-report) - Uses Data City RTICs
- [The Data City: Behind the Scenes of RTIC Creation](https://thedatacity.com/blog/behind-the-scenes-of-rtic-creation/) - Methodology details
- [The Data City: Real-Time SIC Codes](https://thedatacity.com/real-time-sic-codes/) - Product overview
- [D&B: Blockchain and ML Projects](https://www.informationweek.com/machine-learning-ai/dun-bradstreet-eyes-blockchain-machine-learning-projects) - InformationWeek
- [Beauhurst: Companies House Data](https://www.beauhurst.com/blog/companies-house-data/)

---

## Company → Website Matching in R

Ideas for finding and validating company websites from Companies House names.

### Step 1: Search for Websites

**Option A: Google Custom Search API (sadly no longer an option, Google is ending it)**
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

---

## Bespoke Sector Classification from Website Text

Approaches for assigning probability scores to custom sector categories using website text, with Bayesian updating from additional signals.

### Target Sectors (Initial Set)

| Sector | Description | Key Signals |
|--------|-------------|-------------|
| Clean Energy | Renewables, solar, wind, batteries, grid tech | solar, wind, renewable, battery, grid, carbon, sustainability |
| Health Tech | Medical devices, digital health, biotech | medical, health, patient, clinical, diagnostic, biotech, pharma |
| Defence | Military, security, aerospace defence | defence, military, security, aerospace, radar, surveillance |
| Advanced Manufacturing | High-tech production, automation, precision engineering | manufacturer, precision, automation, robotics, CNC, engineering |

### Option 1: Keyword Dictionary Scoring (Simplest)

Fast and interpretable. Define weighted keyword lists per sector.

```r
# Define sector keywords with weights
sector_keywords <- list(
  clean_energy = c(
    "solar" = 3, "wind" = 3, "renewable" = 3, "battery" = 2, "grid" = 2,
    "carbon" = 2, "sustainability" = 2, "green energy" = 3, "photovoltaic" = 3,
    "turbine" = 2, "hydrogen" = 2, "net zero" = 2
  ),
  health_tech = c(
    "medical" = 3, "health" = 2, "patient" = 3, "clinical" = 3, "diagnostic" = 3,
    "biotech" = 3, "pharmaceutical" = 2, "healthcare" = 2, "therapy" = 2,
    "device" = 1, "hospital" = 2, "nhs" = 2
  ),
  defence = c(
    "defence" = 3, "defense" = 3, "military" = 3, "security" = 2, "aerospace" = 2,
    "radar" = 3, "surveillance" = 2, "ammunition" = 3, "naval" = 3, "mod" = 2
  ),
  advanced_manufacturing = c(
    "manufacturer" = 2, "manufacturing" = 2, "precision" = 2, "automation" = 3,
    "robotics" = 3, "cnc" = 3, "engineering" = 1, "machining" = 2, "fabrication" = 2,
    "prototype" = 2, "assembly" = 1
  )
)

# Score text against sector keywords
score_sector_keywords <- function(text, keywords) {
  text_lower <- tolower(text)
  score <- 0
  matches <- c()

  for (kw in names(keywords)) {
    if (grepl(kw, text_lower, fixed = TRUE)) {
      score <- score + keywords[kw]
      matches <- c(matches, kw)
    }
  }

  list(score = score, matches = matches)
}

# Apply to all sectors
classify_text_keywords <- function(text) {
  results <- lapply(names(sector_keywords), function(sector) {
    res <- score_sector_keywords(text, sector_keywords[[sector]])
    data.frame(
      sector = sector,
      score = res$score,
      matches = paste(res$matches, collapse = ", ")
    )
  })
  bind_rows(results) |> arrange(desc(score))
}

# Example with Gripple text
# classify_text_keywords(gripple_text)
# Returns: advanced_manufacturing (score ~6), clean_energy (score ~3)
```

**Pros**: Fast, explainable, no dependencies
**Cons**: Misses synonyms, requires manual keyword curation

### Option 2: TF-IDF + Cosine Similarity (Lightweight ML)

Compare text to sector "seed documents" using term frequency.

```r
library(tidytext)
library(widyr)

# Create seed descriptions for each sector
seed_texts <- tibble(
  sector = c("clean_energy", "health_tech", "defence", "advanced_manufacturing"),
  text = c(
    "solar panels wind turbines renewable energy battery storage grid infrastructure sustainability carbon reduction green technology photovoltaic cells hydrogen fuel cells net zero emissions",
    "medical devices healthcare technology patient monitoring clinical diagnostics biotech pharmaceutical digital health telemedicine hospital equipment therapeutic devices NHS healthcare",
    "defence military aerospace security systems radar surveillance ammunition naval systems MOD contracts military equipment combat systems tactical communications",
    "precision manufacturing automation robotics CNC machining engineering fabrication assembly line industrial production prototyping quality control lean manufacturing"
  )
)

# Calculate TF-IDF for seeds (do once, cache)
seed_tfidf <- seed_texts |>
  unnest_tokens(word, text) |>
  count(sector, word) |>
  bind_tf_idf(word, sector, n)

# Score new text against seeds
score_text_tfidf <- function(new_text, seed_tfidf) {
  new_words <- tibble(text = new_text) |>
    unnest_tokens(word, text) |>
    count(word)

  # Join with seed vocabulary and sum TF-IDF weights
  new_words |>
    inner_join(seed_tfidf, by = "word", relationship = "many-to-many") |>
    group_by(sector) |>
    summarise(score = sum(tf_idf * n), .groups = "drop") |>
    arrange(desc(score))
}
```

**Pros**: Handles vocabulary variation better, weights rare terms higher
**Cons**: Still bag-of-words, misses context

### Option 3: Embeddings + Similarity (Most Powerful)

Use pre-trained embeddings for semantic matching. Requires Python bridge or API.

```r
# Option A: Use OpenAI embeddings via API
library(httr2)

get_embedding <- function(text, api_key) {
  resp <- request("https://api.openai.com/v1/embeddings") |>
    req_auth_bearer_token(api_key) |>
    req_body_json(list(
      input = text,
      model = "text-embedding-3-small"
    )) |>
    req_perform() |>
    resp_body_json()

  unlist(resp$data[[1]]$embedding)
}

# Pre-compute sector embeddings (cache these!)
sector_descriptions <- c(
  clean_energy = "Renewable energy company specialising in solar, wind, batteries and grid technology for sustainable power generation",
  health_tech = "Medical technology company developing diagnostic devices, digital health platforms and pharmaceutical solutions",
  defence = "Defence and aerospace company providing military equipment, security systems and surveillance technology",
  advanced_manufacturing = "Precision engineering and manufacturing company using automation, robotics and CNC machining"
)

# Cosine similarity
cosine_sim <- function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

# Classify new company
classify_with_embeddings <- function(company_text, sector_embeddings, api_key) {
  company_emb <- get_embedding(company_text, api_key)

  similarities <- sapply(sector_embeddings, function(s) cosine_sim(company_emb, s))

  tibble(
    sector = names(similarities),
    similarity = similarities
  ) |> arrange(desc(similarity))
}
```

**Pros**: Captures semantic meaning, handles synonyms, generalises well
**Cons**: API costs, latency, requires embedding cache management

---

## Bayesian Multi-Signal Classification

Combine website text with SIC codes and colocation for robust probability estimates.

### The Framework

For each firm, estimate P(sector | evidence) where evidence includes:
- **T**: Website text
- **S**: Assigned SIC code
- **L**: Location (colocation with known sector firms)

Using Bayes' theorem with conditional independence assumption:

```
P(sector | T, S, L) ∝ P(T | sector) × P(S | sector) × P(L | sector) × P(sector)
```

### Step 1: SIC Code Priors

Map SIC codes to sector probabilities. Some codes strongly indicate a sector; others are ambiguous.

```r
# Define P(sector | SIC) mappings
# Values are rough priors - refine with domain knowledge
sic_sector_priors <- tribble(
  ~SIC_2DIGIT_CODE, ~clean_energy, ~health_tech, ~defence, ~advanced_manufacturing,
  "35",             0.4,           0.0,          0.0,      0.1,    # Electricity/gas
  "26",             0.1,           0.2,          0.2,      0.3,    # Electronics
  "27",             0.2,           0.0,          0.0,      0.3,    # Electrical equipment
  "28",             0.05,          0.1,          0.1,      0.4,    # Machinery
  "29",             0.05,          0.0,          0.1,      0.3,    # Motor vehicles
  "30",             0.0,           0.0,          0.3,      0.2,    # Other transport (aerospace)
  "21",             0.0,           0.5,          0.0,      0.1,    # Pharmaceuticals
  "32",             0.0,           0.3,          0.1,      0.2,    # Medical instruments
  "62",             0.05,          0.1,          0.1,      0.0,    # Software
  "72",             0.05,          0.15,         0.1,      0.05    # R&D
)

get_sic_prior <- function(sic_code, sector) {
  prior_row <- sic_sector_priors |> filter(SIC_2DIGIT_CODE == sic_code)
  if (nrow(prior_row) == 0) return(0.01)  # Default low prior for unmapped SICs
  prior_row[[sector]]
}
```

### Step 2: Text Likelihood

Convert keyword/embedding scores to probabilities.

```r
# Normalise keyword scores to pseudo-probabilities
# Using softmax-style transformation
text_score_to_likelihood <- function(scores) {
  # scores is a named vector: c(clean_energy = 5, health_tech = 2, ...)
  exp_scores <- exp(scores / 3)  # Temperature parameter controls sharpness
  exp_scores / sum(exp_scores)
}

# Example: if keyword scores are c(4, 1, 0, 6)
# Likelihoods become roughly c(0.18, 0.05, 0.03, 0.74)
```

### Step 3: Colocation Signal

Firms near known sector clusters get a boost.

```r
# Pre-compute: % of firms in each postcode district that are in each sector
# (requires a labelled training set)

# Simplified: manual lookup for key clusters
colocation_boosts <- tribble(
  ~postcode_prefix, ~clean_energy, ~health_tech, ~defence, ~advanced_manufacturing,
  "CB",             0.0,           0.2,          0.0,      0.1,    # Cambridge - biotech
  "OX",             0.0,           0.15,         0.05,     0.1,    # Oxford
  "BS",             0.0,           0.0,          0.15,     0.1,    # Bristol - aerospace
  "S9",             0.0,           0.0,          0.0,      0.2,    # Sheffield - manufacturing
  "CV",             0.0,           0.0,          0.0,      0.15,   # Coventry - automotive/mfg
  "AB",             0.15,          0.0,          0.0,      0.1     # Aberdeen - energy
)

get_colocation_factor <- function(postcode, sector) {
  prefix <- str_extract(postcode, "^[A-Z]{1,2}")
  boost_row <- colocation_boosts |> filter(postcode_prefix == prefix)
  if (nrow(boost_row) == 0) return(1.0)  # No boost/penalty
  1 + boost_row[[sector]]  # Multiplicative factor
}
```

### Step 4: Combine with Bayes

```r
classify_firm_bayesian <- function(
  company_text,
  sic_2digit,
  postcode,
  sectors = c("clean_energy", "health_tech", "defence", "advanced_manufacturing")
) {

  # 1. Get text scores and convert to likelihoods
  text_results <- classify_text_keywords(company_text)
  text_scores <- setNames(text_results$score, text_results$sector)
  text_likelihoods <- text_score_to_likelihood(text_scores[sectors])

  # 2. Get SIC priors
  sic_priors <- sapply(sectors, function(s) get_sic_prior(sic_2digit, s))

  # 3. Get colocation factors
  coloc_factors <- sapply(sectors, function(s) get_colocation_factor(postcode, s))

  # 4. Combine: posterior ∝ likelihood × prior × colocation
  raw_posteriors <- text_likelihoods * sic_priors * coloc_factors

  # 5. Normalise to probabilities
  posteriors <- raw_posteriors / sum(raw_posteriors)

  tibble(
    sector = sectors,
    text_likelihood = text_likelihoods,
    sic_prior = sic_priors,
    colocation_factor = coloc_factors,
    posterior = posteriors
  ) |> arrange(desc(posterior))
}

# Example: Gripple Limited
# SIC 25990 (Other fabricated metal products) → 2-digit = 25 → manufacturing prior
# Postcode S9 → Sheffield manufacturing cluster
# Text: "manufacturer", "solar solutions", "wire joining"
#
# Result: advanced_manufacturing ~0.75, clean_energy ~0.15, others low
```

### Practical Workflow

```r
# 1. Start with firms that have matched websites
firms_with_sites <- ch |>
  filter(!is.na(matched_url), Employees_thisyear >= 10)

# 2. Batch fetch website text (with caching!)
firms_with_sites <- firms_with_sites |>
  mutate(
    site_text = map_chr(matched_url, possibly(get_clean_text, NA_character_))
  )

# 3. Apply Bayesian classification
firms_classified <- firms_with_sites |>
  filter(!is.na(site_text)) |>
  mutate(
    classification = pmap(
      list(site_text, SIC_2DIGIT_CODE, postcode),
      classify_firm_bayesian
    )
  ) |>
  unnest(classification)

# 4. Filter to high-confidence assignments
high_confidence <- firms_classified |>
  filter(posterior >= 0.6) |>
  select(CompanyName, CompanyNumber, sector, posterior, text_likelihood, sic_prior)
```

### Advantages of Bayesian Approach

| Benefit | Explanation |
|---------|-------------|
| **Handles missing data** | No website? Use SIC + colocation only |
| **Interpretable** | Can see contribution of each signal |
| **Updateable** | Add new signals (e.g., employee count, company age) easily |
| **Calibrated uncertainty** | Low posterior = genuinely uncertain, not forced classification |
| **Prior knowledge** | Domain expertise encoded in SIC mappings |

### Calibration and Validation

```r
# 1. Create small labelled test set (50-100 firms, manually verified)
# 2. Compare predicted probabilities to actual labels
# 3. Plot calibration curve: P(correct | predicted_prob in bin)
# 4. Adjust temperature parameter and priors if needed

# Simple calibration check
validation_set |>
  mutate(prob_bin = cut(posterior, breaks = seq(0, 1, 0.1))) |>
  group_by(prob_bin) |>
  summarise(
    n = n(),
    accuracy = mean(predicted_sector == true_sector)
  )
# Ideally: 0.8-0.9 bin has ~85% accuracy, 0.5-0.6 bin has ~55% accuracy
```

### Example: Gripple Analysis

Website text signals:
- "manufacturer" → advanced_manufacturing (+2)
- "solar solutions" → clean_energy (+3)
- "wire joining", "tensioning", "suspension systems" → advanced_manufacturing context
- "employee-owned", "sheffield" → no direct sector signal

Combined with:
- SIC 25990 (fabricated metal products) → manufacturing prior ~0.3
- Postcode S9 (Sheffield) → manufacturing colocation boost

Expected posterior: **advanced_manufacturing ~0.7**, clean_energy ~0.2 (solar mentioned but not core business)

---

## Refining Health Tech Classification with Embeddings

The challenge: "health tech" should capture medical devices, digital health platforms, and biotech - but NOT care homes, dental practices, GP surgeries, or general healthcare providers. Basic embedding similarity often conflates these because they share vocabulary like "health", "patient", "care".

### The Problem Illustrated

```
Health Tech (WANT):           Non-Tech Health (DON'T WANT):
- Medical device manufacturer  - Care home
- Digital health platform      - Dental practice
- Biotech/pharma R&D          - GP surgery
- Clinical diagnostics        - Physiotherapy clinic
- Health AI/ML                - Nursing agency
```

Both use words like "health", "patient", "care", "clinical" - but the tech firms emphasize innovation, devices, software, research.

### Option 1: Improved Sector Description (Quick Fix)

Make the reference description explicitly contrast with non-tech health:

```python
SECTOR_DESCRIPTIONS = {
    "health_tech": """Medical technology and digital health company.
    Develops diagnostic devices, medical equipment, health software platforms,
    pharmaceutical products, biotech research, clinical AI, patient monitoring systems,
    telemedicine platforms, health data analytics.
    NOT a care provider, nursing home, dental practice, GP surgery, or healthcare staffing agency.
    Focus on technology, innovation, R&D, devices, software, platforms."""
}
```

This helps but doesn't fully solve the problem - embeddings don't handle negation well.

### Option 2: Multiple Reference Embeddings (Better)

Instead of one description, use multiple example texts from known health tech firms:

```python
from sentence_transformers import SentenceTransformer
import numpy as np

# Known health tech company descriptions (curated examples)
HEALTH_TECH_EXAMPLES = [
    "We develop AI-powered diagnostic imaging software for early cancer detection",
    "Our wearable devices monitor cardiac patients remotely with real-time alerts",
    "Pharmaceutical research company developing novel antibody therapies",
    "Digital health platform connecting patients with specialists via telemedicine",
    "Medical device manufacturer specialising in minimally invasive surgical tools",
    "Biotech firm using CRISPR gene editing for rare disease treatments",
    "Health data analytics platform for hospital resource optimisation",
]

# Known NON-health-tech (care providers) - to contrast against
NON_HEALTH_TECH_EXAMPLES = [
    "Residential care home providing 24-hour support for elderly residents",
    "NHS GP surgery serving the local community with appointments and prescriptions",
    "Dental practice offering check-ups, fillings, and cosmetic dentistry",
    "Nursing agency supplying temporary healthcare staff to hospitals",
    "Physiotherapy clinic helping patients recover from injuries",
    "Home care service providing daily living support for vulnerable adults",
]

def create_health_tech_classifier(model):
    """Create embeddings for health tech vs non-health-tech."""
    pos_embeddings = model.encode(HEALTH_TECH_EXAMPLES)
    neg_embeddings = model.encode(NON_HEALTH_TECH_EXAMPLES)

    # Centroid of positive examples
    pos_centroid = np.mean(pos_embeddings, axis=0)
    neg_centroid = np.mean(neg_embeddings, axis=0)

    return pos_centroid, neg_centroid

def classify_health_tech(text, model, pos_centroid, neg_centroid):
    """
    Score how 'health tech' vs 'non-tech health' a company is.
    Returns score from -1 (definitely care home) to +1 (definitely health tech).
    """
    text_emb = model.encode(text)

    # Cosine similarity to each centroid
    pos_sim = np.dot(text_emb, pos_centroid) / (np.linalg.norm(text_emb) * np.linalg.norm(pos_centroid))
    neg_sim = np.dot(text_emb, neg_centroid) / (np.linalg.norm(text_emb) * np.linalg.norm(neg_centroid))

    # Difference: positive = health tech, negative = care provider
    return pos_sim - neg_sim, pos_sim, neg_sim
```

### Option 3: SetFit Few-Shot Learning (Recommended for Production)

SetFit is designed for exactly this: training a classifier with very few examples (8-16 per class). It fine-tunes sentence-transformers efficiently.

```python
# pip install setfit

from setfit import SetFitModel, Trainer, TrainingArguments
from datasets import Dataset

# Training data: just 10-20 examples per class is enough
train_data = {
    "text": [
        # Health tech examples (label=1)
        "Medical device company developing cardiac monitoring systems",
        "AI-powered diagnostic platform for radiology departments",
        "Biotech firm researching mRNA vaccine technologies",
        "Digital health app for diabetes management and glucose tracking",
        "Pharmaceutical company specialising in oncology treatments",
        "Wearable health technology for remote patient monitoring",
        "Clinical decision support software using machine learning",
        "Medical imaging equipment manufacturer",

        # Non-tech health examples (label=0)
        "Residential care home for elderly with dementia",
        "NHS dental practice offering family dentistry",
        "Home care agency providing personal care assistants",
        "Physiotherapy and rehabilitation clinic",
        "GP surgery serving 8000 registered patients",
        "Nursing home with 24-hour nursing care",
        "Domiciliary care provider for vulnerable adults",
        "Private hospital offering elective surgery",
    ],
    "label": [1, 1, 1, 1, 1, 1, 1, 1,  # health tech
              0, 0, 0, 0, 0, 0, 0, 0]   # non-tech health
}

# Create dataset
dataset = Dataset.from_dict(train_data)
train_dataset = dataset.shuffle(seed=42)

# Load and train SetFit model
model = SetFitModel.from_pretrained("sentence-transformers/all-MiniLM-L6-v2")

trainer = Trainer(
    model=model,
    train_dataset=train_dataset,
    args=TrainingArguments(
        batch_size=8,
        num_epochs=1,  # SetFit needs very few epochs
    ),
)

trainer.train()

# Save for later use
model.save_pretrained("models/health_tech_classifier")

# Classify new companies
def is_health_tech(text):
    """Returns probability of being health tech (0-1)."""
    probs = model.predict_proba([text])[0]
    return probs[1]  # Probability of class 1 (health tech)

# Test
print(is_health_tech("AI diagnostic imaging for cancer detection"))  # ~0.9
print(is_health_tech("Care home providing dementia support"))         # ~0.1
```

### Option 4: Contrastive Fine-Tuning (Most Powerful)

If you have more labelled data (50+ examples), train the base embedding model to push health tech and care providers apart in embedding space:

```python
from sentence_transformers import SentenceTransformer, InputExample, losses
from torch.utils.data import DataLoader

# Pairs: (anchor, positive, negative)
# Anchor = health tech, Positive = another health tech, Negative = care provider
train_examples = [
    InputExample(texts=[
        "Medical device manufacturer for cardiac monitoring",
        "Wearable health sensors for hospital patients",  # similar (health tech)
        "Residential nursing home for elderly care"        # dissimilar (care)
    ]),
    InputExample(texts=[
        "Digital health platform with telemedicine",
        "Health AI software for clinical decisions",
        "GP surgery with family doctor services"
    ]),
    # ... more triplets
]

model = SentenceTransformer("all-MiniLM-L6-v2")

train_dataloader = DataLoader(train_examples, shuffle=True, batch_size=8)
train_loss = losses.TripletLoss(model)

model.fit(
    train_objectives=[(train_dataloader, train_loss)],
    epochs=3,
    warmup_steps=10,
)

model.save("models/health_tech_embeddings")
```

### Practical Recommendations

| Approach | Data Needed | Effort | Quality |
|----------|-------------|--------|---------|
| Better description | 0 | Low | Fair |
| Multiple examples + centroid | ~10 per class | Low | Good |
| SetFit few-shot | 8-20 per class | Medium | Very Good |
| Contrastive fine-tuning | 50+ triplets | High | Excellent |

**Start with Option 2** (multiple examples) for quick improvement, then move to **SetFit (Option 3)** if you need higher accuracy. SetFit is particularly good because:
- Works with very few labelled examples
- Fast to train (minutes, not hours)
- Produces calibrated probabilities
- Easy to update with new examples

### Building a Training Set

To create training data, identify known health tech vs care providers:

```r
# In R: find likely health tech firms by SIC + keywords
likely_health_tech <- ch |>
  filter(
    SIC_2DIGIT_CODE %in% c("21", "26", "32", "72"),  # Pharma, electronics, instruments, R&D
    str_detect(tolower(CompanyName), "tech|digital|bio|pharma|medical device|diagnostic")
  ) |>
  sample_n(50)

# Find likely care providers
likely_care_providers <- ch |>
  filter(
    SIC_2DIGIT_CODE %in% c("86", "87", "88"),  # Health, residential care, social work
    str_detect(tolower(CompanyName), "care|nursing|home|surgery|dental|physio")
  ) |>
  sample_n(50)

# Manually verify a subset, then use website text as training data
```

### Integration with Existing Pipeline

Once you have a trained health tech classifier, integrate it:

```python
from setfit import SetFitModel

# Load pre-trained classifier
health_tech_model = SetFitModel.from_pretrained("models/health_tech_classifier")

def classify_sectors_with_health_tech_filter(text, general_model, sector_embeddings):
    """
    Two-stage classification:
    1. General sector similarity
    2. If health_tech scores high, verify with specialist classifier
    """
    # Stage 1: General classification
    general_scores = classify_text(text, general_model, sector_embeddings)

    # Stage 2: If health_tech is top candidate, verify
    if general_scores["health_tech"] > 0.3:
        health_tech_prob = health_tech_model.predict_proba([text])[0][1]

        # Adjust score based on specialist classifier
        if health_tech_prob < 0.5:
            # Likely a care provider, not health tech
            general_scores["health_tech"] *= 0.3  # Penalise

    return general_scores
```

---

## Health Tech vs Health Non-Tech: Positive Correlation Considerations

When using embedding similarity for sector classification, health_tech and health_nontech scores often correlate positively. This section explores why and what to do about it.

### Why Positive Correlation Happens

1. **Shared vocabulary**: Both sectors use words like "health", "patient", "care", "clinical", "medical". Embedding models capture this surface-level similarity.

2. **Embedding space geometry**: Cosine similarity measures angle, not position. Two sector reference points can both be "close" to a company text if they occupy nearby regions of the embedding space. Health-related concepts cluster together.

3. **all-MiniLM-L6-v2 limitations**: This model (384 dimensions, 22M parameters) is optimised for speed and general semantic similarity, not fine-grained domain distinctions. It wasn't trained to distinguish "tech company in health domain" from "health service provider".

4. **Reference description overlap**: If both sector descriptions contain similar terms, their embeddings will be similar, making discrimination harder.

### Can You Still Separate Them?

**Yes, but not directly from raw similarity scores.** Options:

| Approach | How It Works | When to Use |
|----------|--------------|-------------|
| **Score difference** | `health_tech - health_nontech` | Quick fix; works if tech firms score higher on tech than nontech |
| **Score ratio** | `health_tech / health_nontech` | Similar to difference; may amplify small distinctions |
| **Rank-based** | Use relative ranking across all sectors | If absolute scores are unreliable but ordering is meaningful |
| **Binary classifier** | Train separate model on health_tech vs health_nontech | Best accuracy; requires labelled examples |

### Factors Affecting Discriminability

1. **Website text quality**
   - Homepages often contain marketing fluff, not discriminative content
   - "About" pages may be more informative
   - Some firms have minimal text (brochure sites)
   - Boilerplate (cookie notices, footer links) adds noise

2. **Model capacity**
   - all-MiniLM-L6-v2: Fast but limited nuance
   - all-mpnet-base-v2: Better quality, 2x slower
   - BGE/GTE models: State-of-art, require more compute
   - Domain-specific models: PubMedBERT etc. for health text

3. **Genuine conceptual overlap**
   - Some firms legitimately straddle both (e.g., digital health platform that also provides care)
   - The boundary may be fuzzy in reality, not just in the model

4. **Sector description engineering**
   - Current descriptions may not emphasise discriminative features
   - Adding "NOT a care home, NOT a GP surgery" doesn't help embeddings (they don't handle negation)
   - Need to emphasise what makes tech different: "software", "devices", "R&D", "platform", "AI"

### Diagnostic Steps

```python
# 1. Check correlation empirically
import numpy as np
scores = result_df[["sim_health_tech", "sim_health_nontech"]].dropna()
print(f"Correlation: {scores.corr().iloc[0,1]:.3f}")

# 2. Look at score distributions
scores.plot.scatter(x="sim_health_tech", y="sim_health_nontech")

# 3. Check separation
scores["diff"] = scores["sim_health_tech"] - scores["sim_health_nontech"]
print(scores["diff"].describe())

# 4. Examine edge cases - high on both
ambiguous = result_df[
    (result_df["sim_health_tech"] > 0.4) &
    (result_df["sim_health_nontech"] > 0.4)
]
print(ambiguous[["CompanyName", "sim_health_tech", "sim_health_nontech"]].head(20))
```

### Recommendations

1. **First**: Check empirical correlation and score distributions. If correlation is >0.7, raw scores won't discriminate well.

2. **Quick fix**: Use `health_tech - health_nontech` as a derived feature. Positive = likely tech, negative = likely care provider.

3. **Better fix**: Rewrite sector descriptions to maximise contrast:
   - health_tech: emphasise "software platform", "medical devices", "diagnostic equipment", "biotech R&D", "clinical AI", "health data analytics"
   - health_nontech: emphasise "care home", "nursing", "GP surgery", "dental practice", "physiotherapy", "domiciliary care"

4. **Best fix**: Train a binary SetFit classifier with 10-20 examples of each. This learns the boundary directly rather than relying on pre-computed embeddings.

5. **Model upgrade**: Try `all-mpnet-base-v2` (768 dims) or `BAAI/bge-small-en-v1.5` for better semantic discrimination.

### Example: Improved Sector Descriptions

```python
# More discriminative descriptions
SECTOR_DESCRIPTIONS = {
    "health_tech": """Medical technology company developing software platforms,
    diagnostic devices, clinical AI systems, or pharmaceutical products.
    Focus on R&D, innovation, medical devices, health data analytics,
    telemedicine platforms, biotech research, wearable health sensors.""",

    "health_nontech": """Healthcare service provider such as care home,
    nursing home, GP surgery, dental practice, physiotherapy clinic,
    domiciliary care agency, or hospital. Delivers patient care,
    medical appointments, residential support, or clinical treatments."""
}
```

The key is to use vocabulary that appears in one type but not the other.
