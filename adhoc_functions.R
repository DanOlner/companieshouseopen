# Ad hoc functions


# CLAUDE CODE MADE FUNCTIONS----

# With some human additions / tweaks

library(httr2)
library(rvest)
library(curl)

# Guessing and validating website names from company names----

# Using just normalise, generate and guess_domain
# We'll do some actual website content checks next

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
# validate_domain_match <- function(domain, company_name, postcode = NULL,
#                                   company_number = NULL, timeout = 10) {
#   url <- paste0("https://", domain)
#   
#   tryCatch({
#     resp <- request(url) |>
#       req_timeout(timeout) |>
#       req_error(is_error = ~ FALSE) |>
#       req_perform()
#     
#     if (resp_status(resp) >= 400) return(list(valid = FALSE, confidence = 0))
#     
#     # Parse homepage text
#     page_text <- resp |>
#       resp_body_html() |>
#       html_text2() |>
#       tolower()
#     
#     # Check for anchors
#     name_clean <- tolower(str_remove(company_name, "\\s*(LIMITED|LTD|PLC)$"))
#     
#     checks <- c(
#       name_found = grepl(name_clean, page_text, fixed = TRUE),
#       postcode_found = if (!is.null(postcode)) {
#         grepl(tolower(postcode), page_text, fixed = TRUE)
#       } else NA,
#       company_number_found = if (!is.null(company_number)) {
#         grepl(company_number, page_text, fixed = TRUE)
#       } else NA
#     )
#     
#     confidence <- sum(checks, na.rm = TRUE) / sum(!is.na(checks))
#     
#     list(
#       valid = confidence >= 0.5,
#       confidence = confidence,
#       checks = checks,
#       url = url
#     )
#     
#   }, error = function(e) {
#     list(valid = FALSE, confidence = 0, error = as.character(e))
#   })
# }

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


return_all_existing_candidate_domains <- function(company_name) {
  candidates <- generate_domain_candidates(company_name)

  cat(company_name, '\n')

  existing <- c()
  for (domain in candidates) {
    if (domain_exists(domain)) {
      existing <- c(existing, domain)
    }
  }

  if (length(existing) == 0) {
    return(NA_character_)
  }

  return(existing)
}



# Check website contents----


# get_page_meta <- function(domain) {
#   url <- paste0("https://", domain)
#   
#   tryCatch({
#     doc <- request(url) |> req_timeout(10) |> req_perform() |> resp_body_html()
#     
#     list(
#       title = html_element(doc, "title") |> html_text(),
#       description = html_element(doc, "meta[name='description']") |> html_attr("content"),
#       og_description = html_element(doc, "meta[property='og:description']") |> html_attr("content"),
#       h1 = html_elements(doc, "h1") |> html_text() |> paste(collapse = " ")
#     )
#   }, error = function(e) list(title = NA, description = NA, og_description = NA, h1 = NA))
# }



#' Clean text from an already-parsed HTML document
#'
#' Removes script, style, nav, etc. and returns cleaned text.
#' Works on the doc in place, so pass a copy if you need the original.
clean_doc_text <- function(doc) {
  if (is.null(doc)) return(NA_character_)

  tryCatch({
    # Clone the doc by re-parsing (xml_new_root doesn't reliably clone documents)
    doc_copy <- read_html(as.character(doc))

    # Remove script, style, noscript, nav, footer, header tags
    xml_remove(html_elements(doc_copy, "script, style, noscript, nav, footer, header"))

    # Extract and clean text
    doc_copy |> html_text2() |> tolower() |> str_squish()
  }, error = function(e) NA_character_)
}

#' Fetch page and return both text and parsed HTML document
#'
#' Single HTTP request that returns everything needed for postcode checking
#' and link extraction, avoiding duplicate fetches.
#' Returns raw text (for postcode matching) and cleaned text (for analysis).
get_page_content <- function(domain, subdomain = NULL, timeout = 10) {
  url <- paste0("https://", domain)
  if (!is.null(subdomain)) url <- paste0(url, "/", subdomain)

  cat("Trying to get", url, "\n")

  tryCatch({
    resp <- request(url) |>
      req_timeout(timeout) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()

    if (resp_status(resp) >= 400) {
      return(list(text = NULL, doc = NULL, clean_text = NA_character_, success = FALSE))
    }

    doc <- resp_body_html(resp)
    page_text <- doc |> html_text() |> tolower()
    clean_text <- clean_doc_text(doc)

    list(text = page_text, doc = doc, clean_text = clean_text, success = TRUE)

  }, error = function(e) {
    list(text = NULL, doc = NULL, clean_text = NA_character_, success = FALSE)
  })
}

#' Extract all links from parsed HTML document
get_links_from_doc <- function(doc) {
  if (is.null(doc)) return(character(0))
  xml_attr(
    xml_find_all(doc, "//a[@href and string-length(@href) > 0]"),
    "href"
  )
}


get_clean_text <- function(domain) {
  url <- paste0("https://", domain)

  tryCatch({
    resp <- request(url) |>
      req_timeout(10) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()

    if (resp_status(resp) >= 400) return(NA_character_)

    doc <- resp_body_html(resp)
    clean_doc_text(doc)

  }, error = function(e) NA_character_)
}



# Check if postcode is in a website's main or contact page
# check_for_postcode <- function(website, postcode, ...) {
#   
#   x <- get_websitefrontpage(website) %>%  
#     toupper() %>% 
#     gsub(' ', '', .)
#   
#   postcodepresent = qg(postcode, x)[1]
#   
#   # If false, let's look for a contact page
#   if(!postcodepresent){
#     
#     cat('Trying contact page...\n')
#     
#     doc <- read_html(paste0('https://',website))
#     
#     all_links <- xml_attr(
#       xml_find_all(doc, "//a[@href and string-length(@href) > 0]"),
#       "href"
#     )
#     
#     if(!is.null(all_links)){
#       
#       contactpage = all_links[qg('contact', all_links)][1]
#       
#       # Check if it's a relative path
#       if(str_sub(contactpage,1,1)=='/'){
#         contactpage = paste0(website,contactpage)
#       }
#       
#       # Repeat postcode check
#       x <- get_websitefrontpage(contactpage) %>% 
#         toupper() %>% 
#         gsub(' ', '', .)
#       
#       postcodepresent = qg(postcode, x)[1]
#       
#     }
#     
#     return(postcodepresent)
#     
#   }
#   
# }



#' Check for postcode and extract cleaned text from website
#'
#' Fetches main page and about page, returning cleaned text from both.
#' Only fetches contact page if postcode not yet found.
#'
#' @param website Domain to check
#' @param postcode Postcode to search for (spaces will be removed for matching)
#' @param ... Additional arguments (ignored, allows use with pmap)
#' @return A list with:
#'   - validated: TRUE if postcode found, FALSE otherwise
#'   - main_text: cleaned text from main page
#'   - about_text: cleaned text from about page (NA if not found)
check_for_postcode <- function(website, postcode, ...) {

  # Helper to check postcode in text
  postcode_in_text <- function(text) {
    if (is.null(text)) return(FALSE)
    text_clean <- text |> toupper() |> gsub(' ', '', x = _)
    qg(postcode, text_clean)[1]
  }

  # Helper to make URLs absolute
  make_absolute <- function(path, base) {
    if (is.na(path)) return(NA)
    if (grepl("^https?://", path)) return(path)
    if (grepl("^/", path)) return(paste0("https://", base, path))
    paste0("https://", base, "/", path)
  }

  # Initialize result
  result <- list(
    validated = FALSE,
    main_text = NA_character_,
    about_text = NA_character_
  )

  # Fetch main page ONCE - get both text and parsed doc
  content <- get_page_content(website)

  if (!content$success) return(result)

  # Store cleaned main page text

  result$main_text <- content$clean_text

  # Check main page for postcode
  postcode_found <- postcode_in_text(content$text)
  if (postcode_found) {
    cat('Tick! (main page)\n')
    result$validated <- TRUE
  }

  # Get links from the ALREADY FETCHED document
  all_links <- get_links_from_doc(content$doc)

  if (length(all_links) == 0) return(result)

  # ALWAYS fetch about page for its content
  about_link <- all_links[qg("about", all_links)][1]
  cat("Trying about page: \n", about_link,"\n")

  if (!is.na(about_link)) {
    full_url <- make_absolute(about_link, website)
    subpage_domain <- gsub("^https?://", "", full_url)

    about_content <- get_page_content(subpage_domain)

    if (about_content$success) {
      result$about_text <- about_content$clean_text

      # Check about page for postcode if not yet found
      if (!result$validated && postcode_in_text(about_content$text)) {
        cat('Tick! (about page)\n')
        result$validated <- TRUE
      }
    }
  }

  # Only check contact page if postcode still not found
  if (!result$validated) {
    cat("Trying contact page...\n")
    contact_link <- all_links[qg("contact", all_links)][1]

    if (!is.na(contact_link)) {
      full_url <- make_absolute(contact_link, website)
      subpage_domain <- gsub("^https?://", "", full_url)

      contact_content <- get_page_content(subpage_domain)

      if (contact_content$success && postcode_in_text(contact_content$text)) {
        cat('Tick! (contact page)\n')
        result$validated <- TRUE
      }
    }
  }

  result
}


#' Find a validated website for a company by checking postcode
#'
#' Gets all candidate domains for a company name, then checks each one
#' to see if the company's postcode appears on the site. Returns the first
#' website that contains the postcode, along with the scraped text.
#'
#' @param company_name Company name to generate domain candidates from
#' @param postcode Postcode to look for on the website (spaces removed)
#' @param candidates if NULL, guess website names from company name. If a vector supplied, use those domains to search.
#' @param max_candidates Maximum number of candidate websites to check (default 5)
#' @param verbose Print progress messages (default TRUE)
#' @return A list with:
#'   - website: The validated website URL, or NA_character_ if none found
#'   - main_text: Cleaned text from main page
#'   - about_text: Cleaned text from about page
find_validated_website <- function(company_name, postcode, candidates = NULL, max_candidates = 5, verbose = TRUE) {

  # Initialize empty result

  empty_result <- list(
    website = NA_character_,
    main_text = NA_character_,
    about_text = NA_character_
  )

  # Get all existing domain candidates, if no domain candidates via mojeek or elsewhere not passed in
  if(is.null(candidates)){
    candidates <- return_all_existing_candidate_domains(company_name)
  } 

  # If no candidates exist, return empty
  if (length(candidates) == 1 && is.na(candidates)) {
    if (verbose) cat("No domain candidates found for:", company_name, "\n")
    return(empty_result)
  }

  # Limit candidates if max_candidates is set
  if (!is.null(max_candidates) && length(candidates) > max_candidates) {
    if (verbose) cat("Limiting to first", max_candidates, "of", length(candidates), "candidates\n")
    candidates <- candidates[1:max_candidates]
  }

  if (verbose) cat("Checking", length(candidates), "candidate domains for postcode", postcode, "\n")

  # Check each candidate in turn
  for (candidate in candidates) {
    if (verbose) cat("  Testing:", candidate, "... ")

    check_result <- tryCatch(
      check_for_postcode(candidate, postcode),
      error = function(e) list(validated = FALSE, main_text = NA_character_, about_text = NA_character_)
    )

    if (check_result$validated) {
      if (verbose) cat("VALIDATED\n")
      return(list(
        website = candidate,
        main_text = check_result$main_text,
        about_text = check_result$about_text
      ))
    } else {
      if (verbose) cat("no match\n")
    }
  }

  if (verbose) cat("No validated website found for:", company_name, "\n")
  return(empty_result)
}

