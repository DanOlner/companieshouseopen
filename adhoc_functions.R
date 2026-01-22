# Ad hoc functions


# CLAUDE CODE MADE FUNCTIONS----

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
  words <- str_split(name, "\\s+")[[1]]

  list(
    joined = str_remove_all(name, "\\s"),           # "acmewidgets"
    hyphenated = str_replace_all(name, "\\s", "-"), # "acme-widgets"
    first_word = words[1],                          # "acme"
    first_two = if (length(words) >= 2) paste0(words[1:2], collapse = "") else NULL,
    first_two_hyphen = if (length(words) >= 2) paste0(words[1:2], collapse = "-") else NULL,
    first_three = if (length(words) >= 3) paste0(words[1:3], collapse = "") else NULL,
    first_three_hyphen = if (length(words) >= 3) paste0(words[1:3], collapse = "-") else NULL
  )
}

# Step 2: Generate candidate domains
generate_domain_candidates <- function(company_name) {
  variants <- normalise_for_domain(company_name)

  tlds <- c(".co.uk", ".com", ".uk", ".org.uk", ".net")

  # Generate all combinations
  base_candidates <- expand.grid(
    name = unlist(variants),
    tld = tlds,
    stringsAsFactors = FALSE
  ) |>
    mutate(domain = paste0(name, tld)) |>
    pull(domain) |>
    unique()

  # Add www. prefixed versions for all candidates
  www_candidates <- paste0("www.", base_candidates)

  # Combine: try non-www first (more common), then www versions
  candidates <- c(base_candidates, www_candidates)
  
  # Order by length - longer ones matching first are more likely to be correct
  candidates = candidates[order(-nchar(candidates), candidates)]

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
  # if (!is.null(search_fn)) {
  #   search_results <- search_fn(paste(company_name, "UK"))
  #   if (nrow(search_results) > 0) {
  #     return(list(
  #       url = search_results$url[1],
  #       method = "search_api",
  #       confidence = NA
  #     ))
  #   }
  # }
  
  return(list(url = NA, method = "not_found", confidence = 0))
}


# Simple domain finder for use in mutate()
# Returns first existing domain or NA_character_
guess_domain <- function(company_name) {
  candidates <- generate_domain_candidates(company_name)
  
  cat(company_name,'\n')

  for (domain in candidates) {
    if (domain_exists(domain)) {
      return(domain)
    }
  }

  return(NA_character_)
}

