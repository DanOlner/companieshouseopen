"""
Extract keywords and keyphrases from company website text using KeyBERT.

Takes a parquet file with website text and adds a column with extracted
keyphrases summarising what each company does.

Usage:
    python extract_keywords.py --input ../local/samplebatch.parquet --output ../local/samplebatch_keywords.parquet

Requires:
    pip install keybert
"""

import os
import argparse
import time
import pandas as pd
from keybert import KeyBERT

# Use the same embedding model as SetFit classifiers for consistency
DEFAULT_MODEL = "BAAI/bge-base-en-v1.5"

# Default extraction settings
DEFAULT_TOP_N = 5
DEFAULT_NGRAM_RANGE = (1, 3)  # Extract 1-word to 3-word phrases


def create_keyword_model(model_name: str = None) -> KeyBERT:
    """
    Create a KeyBERT model with specified embedding model.

    Args:
        model_name: Sentence transformer model name. If None, uses DEFAULT_MODEL.

    Returns:
        KeyBERT model instance
    """
    if model_name is None:
        model_name = DEFAULT_MODEL

    print(f"Loading KeyBERT with model: {model_name}")
    return KeyBERT(model=model_name)


def extract_keywords_from_text(
    text: str,
    model: KeyBERT,
    top_n: int = DEFAULT_TOP_N,
    ngram_range: tuple = DEFAULT_NGRAM_RANGE,
    diversity: float = 0.5
) -> str:
    """
    Extract keywords from a single text.

    Args:
        text: Text to extract keywords from
        model: KeyBERT model
        top_n: Number of keywords to extract
        ngram_range: Tuple of (min_ngram, max_ngram) for phrase length
        diversity: 0-1, higher values give more diverse keywords (uses MMR)

    Returns:
        Comma-separated string of keywords, e.g. "medical devices, diagnostic imaging, NHS"
    """
    if not text or pd.isna(text) or len(str(text).strip()) < 50:
        return ""

    try:
        # Use Maximal Marginal Relevance for diversity
        keywords = model.extract_keywords(
            str(text),
            keyphrase_ngram_range=ngram_range,
            stop_words="english",
            top_n=top_n,
            use_mmr=True,
            diversity=diversity
        )
        # keywords is list of (phrase, score) tuples
        phrases = [kw[0] for kw in keywords]
        return ", ".join(phrases)

    except Exception as e:
        print(f"Warning: keyword extraction failed: {e}")
        return ""


def extract_keywords_batch(
    texts: list,
    model: KeyBERT,
    top_n: int = DEFAULT_TOP_N,
    ngram_range: tuple = DEFAULT_NGRAM_RANGE,
    diversity: float = 0.5,
    show_progress: bool = True
) -> list:
    """
    Extract keywords from a list of texts.

    Args:
        texts: List of text strings
        model: KeyBERT model
        top_n: Number of keywords per text
        ngram_range: Tuple of (min_ngram, max_ngram)
        diversity: 0-1 for keyword diversity
        show_progress: Print progress updates

    Returns:
        List of comma-separated keyword strings
    """
    results = []
    total = len(texts)

    for i, text in enumerate(texts):
        if show_progress and (i + 1) % 100 == 0:
            print(f"  Processed {i + 1}/{total} texts...")

        keywords = extract_keywords_from_text(
            text, model, top_n, ngram_range, diversity
        )
        results.append(keywords)

    return results


def process_dataframe(
    df: pd.DataFrame,
    text_column: str,
    model: KeyBERT,
    output_column: str = "keywords",
    top_n: int = DEFAULT_TOP_N,
    ngram_range: tuple = DEFAULT_NGRAM_RANGE,
    diversity: float = 0.5
) -> pd.DataFrame:
    """
    Add keywords column to dataframe.

    Args:
        df: Input dataframe
        text_column: Column containing website text
        model: KeyBERT model
        output_column: Name for new keywords column
        top_n: Number of keywords per text
        ngram_range: Tuple of (min_ngram, max_ngram)
        diversity: 0-1 for keyword diversity

    Returns:
        Dataframe with keywords column added
    """
    texts = df[text_column].fillna("").tolist()

    print(f"Extracting keywords from {len(texts)} texts...")
    keywords = extract_keywords_batch(
        texts, model, top_n, ngram_range, diversity
    )

    result_df = df.copy()
    result_df[output_column] = keywords

    return result_df


def main():
    parser = argparse.ArgumentParser(
        description="Extract keywords from company website text using KeyBERT"
    )
    parser.add_argument(
        "--input", "-i",
        required=True,
        help="Input parquet file path"
    )
    parser.add_argument(
        "--output", "-o",
        required=True,
        help="Output parquet file path"
    )
    parser.add_argument(
        "--text-column", "-t",
        default="site_text",
        help="Column containing text to analyse (default: site_text)"
    )
    parser.add_argument(
        "--output-column",
        default="keywords",
        help="Name for output keywords column (default: keywords)"
    )
    parser.add_argument(
        "--top-n", "-n",
        type=int,
        default=DEFAULT_TOP_N,
        help=f"Number of keywords to extract (default: {DEFAULT_TOP_N})"
    )
    parser.add_argument(
        "--min-ngram",
        type=int,
        default=1,
        help="Minimum words per keyphrase (default: 1)"
    )
    parser.add_argument(
        "--max-ngram",
        type=int,
        default=3,
        help="Maximum words per keyphrase (default: 3)"
    )
    parser.add_argument(
        "--diversity",
        type=float,
        default=0.5,
        help="Keyword diversity 0-1, higher = more diverse (default: 0.5)"
    )
    parser.add_argument(
        "--model", "-m",
        default=DEFAULT_MODEL,
        help=f"Sentence transformer model (default: {DEFAULT_MODEL})"
    )

    args = parser.parse_args()

    # Load data
    print(f"Loading data from: {args.input}")
    df = pd.read_parquet(args.input)
    print(f"Loaded {len(df)} rows")

    # Check text column exists
    if args.text_column not in df.columns:
        available = ", ".join(df.columns[:10])
        raise ValueError(
            f"Column '{args.text_column}' not found. Available columns: {available}..."
        )

    # Create model
    model = create_keyword_model(args.model)

    # Process
    ngram_range = (args.min_ngram, args.max_ngram)
    start_time = time.time()

    result_df = process_dataframe(
        df,
        args.text_column,
        model,
        output_column=args.output_column,
        top_n=args.top_n,
        ngram_range=ngram_range,
        diversity=args.diversity
    )

    elapsed = time.time() - start_time

    # Save
    result_df.to_parquet(args.output)
    print(f"\nResults saved to: {args.output}")

    # Summary
    print(f"\n{'='*60}")
    print(f"Keyword extraction complete")
    print(f"{'='*60}")
    print(f"Processed {len(result_df)} rows in {elapsed:.2f} seconds")
    print(f"Average: {elapsed / len(result_df) * 1000:.2f} ms per row")

    # Count how many got keywords
    has_keywords = result_df[args.output_column].str.len() > 0
    print(f"Texts with keywords: {has_keywords.sum()} ({has_keywords.mean()*100:.1f}%)")

    # Show samples
    print(f"\nSample results:")
    display_cols = ["CompanyName"] if "CompanyName" in result_df.columns else []
    display_cols.append(args.output_column)
    display_cols = [c for c in display_cols if c in result_df.columns]

    # Show rows that have keywords
    sample = result_df[has_keywords][display_cols].head(10)
    for _, row in sample.iterrows():
        name = row.get("CompanyName", "Unknown")
        kw = row[args.output_column]
        print(f"  {name[:40]:<40} | {kw[:60]}")


if __name__ == "__main__":
    main()
