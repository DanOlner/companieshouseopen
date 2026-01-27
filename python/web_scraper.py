"""
Web scraping utilities for extracting clean text from websites.
Mirrors the get_clean_text function from adhoc_functions.R

Setup:
    pip install requests beautifulsoup4 lxml
"""

import re
import requests
from bs4 import BeautifulSoup


def get_clean_text(domain, timeout=10):
    """
    Fetch a webpage and extract clean text content.

    Removes script, style, noscript, nav, footer, header tags before
    extracting text. Returns lowercase text with collapsed whitespace.

    Args:
        domain: Domain name (e.g., "gripple.com" or "www.gripple.co.uk")
        timeout: Request timeout in seconds

    Returns:
        Clean text string, or None if request fails
    """
    url = f"https://{domain}"

    try:
        resp = requests.get(
            url,
            timeout=timeout,
            headers={"User-Agent": "Mozilla/5.0 (compatible; research bot)"}
        )

        if resp.status_code >= 400:
            return None

        soup = BeautifulSoup(resp.content, "lxml")

        # Remove unwanted tags
        for tag in soup.find_all(["script", "style", "noscript", "nav", "footer", "header"]):
            tag.decompose()

        # Extract text
        text = soup.get_text(separator=" ")

        # Lowercase and collapse whitespace
        text = text.lower()
        text = re.sub(r"\s+", " ", text).strip()

        return text

    except Exception:
        return None


def get_page_meta(domain, timeout=10):
    """
    Extract metadata from a webpage (title, description, h1 tags).

    Args:
        domain: Domain name
        timeout: Request timeout in seconds

    Returns:
        Dict with title, description, og_description, h1 keys (or None values)
    """
    url = f"https://{domain}"

    try:
        resp = requests.get(
            url,
            timeout=timeout,
            headers={"User-Agent": "Mozilla/5.0 (compatible; research bot)"}
        )

        if resp.status_code >= 400:
            return {"title": None, "description": None, "og_description": None, "h1": None}

        soup = BeautifulSoup(resp.content, "lxml")

        # Title
        title_tag = soup.find("title")
        title = title_tag.get_text().strip() if title_tag else None

        # Meta description
        desc_tag = soup.find("meta", attrs={"name": "description"})
        description = desc_tag.get("content") if desc_tag else None

        # Open Graph description
        og_tag = soup.find("meta", attrs={"property": "og:description"})
        og_description = og_tag.get("content") if og_tag else None

        # H1 tags
        h1_tags = soup.find_all("h1")
        h1 = " ".join(tag.get_text().strip() for tag in h1_tags) if h1_tags else None

        return {
            "title": title,
            "description": description,
            "og_description": og_description,
            "h1": h1
        }

    except Exception:
        return {"title": None, "description": None, "og_description": None, "h1": None}


def batch_get_clean_text(domains, timeout=10):
    """
    Fetch clean text for multiple domains.

    Args:
        domains: List of domain names
        timeout: Request timeout per domain

    Returns:
        Dict mapping domain -> clean text (or None)
    """
    results = {}
    for i, domain in enumerate(domains):
        print(f"[{i+1}/{len(domains)}] Fetching {domain}...")
        results[domain] = get_clean_text(domain, timeout)
    return results


# ============================================================================
# Test
# ============================================================================

# Only run if script executed directly, not when imported
if __name__ == "__main__":
    # Test with Gripple
    test_domain = "gripple.com"

    print(f"Testing get_clean_text on {test_domain}...")
    text = get_clean_text(test_domain)

    if text:
        print(f"\nText length: {len(text)} characters")
        print(f"\nFirst 500 chars:\n{text[:500]}...")
    else:
        print("Failed to fetch page")

    print("\n" + "="*60)
    print("Testing get_page_meta...")
    meta = get_page_meta(test_domain)
    for key, value in meta.items():
        print(f"  {key}: {value[:100] if value else None}...")
