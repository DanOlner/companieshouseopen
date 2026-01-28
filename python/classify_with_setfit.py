"""
Classify company text using trained SetFit binary classifiers.

Loads pre-trained models from ../models/ and applies them to a parquet dataframe.
Each sector classifier outputs a probability score (0-1) for whether the firm
belongs to that sector.

Usage:
    python classify_with_setfit.py --input ../local/samplebatch.parquet --output ../local/samplebatch_setfit_classified.parquet

Requires trained models from train_binary_classifiers.py
"""

import os
import argparse
import time
import pandas as pd
from setfit import SetFitModel

# Directory where trained models are stored
MODELS_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "models")

# Default sectors to classify (must have trained models)
DEFAULT_SECTORS = ["health_tech", "clean_energy", "defence", "advanced_manufacturing"]


def load_all_classifiers(sectors: list = None) -> dict:
    """
    Load all trained SetFit classifiers.

    Args:
        sectors: List of sector names to load. If None, uses DEFAULT_SECTORS.

    Returns:
        Dict of {sector_name: SetFitModel}
    """
    if sectors is None:
        sectors = DEFAULT_SECTORS

    models = {}
    for sector in sectors:
        model_path = os.path.join(MODELS_DIR, f"{sector}_classifier")
        if os.path.exists(model_path):
            print(f"Loading classifier: {sector}")
            models[sector] = SetFitModel.from_pretrained(model_path)
        else:
            print(f"Warning: No trained model found for '{sector}' at {model_path}")

    if not models:
        raise FileNotFoundError(
            f"No trained models found in {MODELS_DIR}. "
            "Run train_binary_classifiers.py first."
        )

    return models


def classify_texts_batch(texts: list, models: dict, batch_size: int = 32) -> dict:
    """
    Classify a batch of texts with all sector models.

    Args:
        texts: List of text strings to classify
        models: Dict of {sector_name: SetFitModel}
        batch_size: Batch size for prediction

    Returns:
        Dict of {sector_name: list of probabilities}
    """
    results = {}

    for sector_name, model in models.items():
        print(f"  Classifying with {sector_name} model...")
        # predict_proba returns array of [prob_negative, prob_positive] for each text
        probs = model.predict_proba(texts, batch_size=batch_size)
        # Extract probability of positive class (is this sector)
        results[sector_name] = [float(p[1]) for p in probs]

    return results


def classify_dataframe(
    df: pd.DataFrame,
    text_column: str,
    models: dict,
    batch_size: int = 32,
    threshold: float = 0.5
) -> pd.DataFrame:
    """
    Classify all rows in a dataframe using SetFit models.

    Adds columns:
    - setfit_{sector}: probability score for each sector
    - setfit_best_sector: sector with highest probability (or 'other' if none above threshold)
    - setfit_best_prob: highest probability score

    Args:
        df: Input dataframe
        text_column: Column containing text to classify
        models: Dict of {sector_name: SetFitModel}
        batch_size: Batch size for prediction
        threshold: Minimum probability to assign a sector (otherwise 'other')

    Returns:
        Dataframe with classification columns added
    """
    # Handle missing text
    texts = df[text_column].fillna("").tolist()

    print(f"Classifying {len(texts)} texts...")
    results = classify_texts_batch(texts, models, batch_size)

    # Add results to dataframe
    result_df = df.copy()

    for sector_name, probs in results.items():
        result_df[f"setfit_{sector_name}"] = probs

    # Find best sector for each row
    sector_cols = [f"setfit_{s}" for s in models.keys()]

    # Get best score and sector
    result_df["setfit_best_prob"] = result_df[sector_cols].max(axis=1)
    result_df["setfit_best_sector_raw"] = result_df[sector_cols].idxmax(axis=1).str.replace("setfit_", "")

    # Apply threshold - if best score < threshold, assign 'other'
    result_df["setfit_best_sector"] = result_df.apply(
        lambda row: row["setfit_best_sector_raw"] if row["setfit_best_prob"] >= threshold else "other",
        axis=1
    )

    # Clean up temporary column
    result_df = result_df.drop(columns=["setfit_best_sector_raw"])

    return result_df


def main():
    parser = argparse.ArgumentParser(
        description="Classify company text using trained SetFit models"
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
        help="Column containing text to classify (default: site_text)"
    )
    parser.add_argument(
        "--batch-size", "-b",
        type=int,
        default=32,
        help="Batch size for prediction (default: 32)"
    )
    parser.add_argument(
        "--threshold",
        type=float,
        default=0.5,
        help="Minimum probability to assign a sector (default: 0.5)"
    )
    parser.add_argument(
        "--sectors", "-s",
        nargs="+",
        default=None,
        help="Sectors to classify (default: all available)"
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

    # Load models
    print(f"\nLoading classifiers from: {MODELS_DIR}")
    models = load_all_classifiers(args.sectors)
    print(f"Loaded {len(models)} classifiers: {list(models.keys())}")

    # Classify
    print(f"\nClassifying with batch size {args.batch_size}...")
    start_time = time.time()

    result_df = classify_dataframe(
        df,
        args.text_column,
        models,
        batch_size=args.batch_size,
        threshold=args.threshold
    )

    elapsed = time.time() - start_time

    # Save results
    result_df.to_parquet(args.output)
    print(f"\nResults saved to: {args.output}")

    # Summary
    print(f"\n{'='*60}")
    print(f"Classification complete")
    print(f"{'='*60}")
    print(f"Processed {len(result_df)} rows in {elapsed:.2f} seconds")
    print(f"Average: {elapsed / len(result_df) * 1000:.2f} ms per row")

    print(f"\nSector distribution (threshold={args.threshold}):")
    print(result_df["setfit_best_sector"].value_counts())

    # Show sample results
    print(f"\nSample results:")
    display_cols = ["CompanyName"] if "CompanyName" in result_df.columns else []
    display_cols += [f"setfit_{s}" for s in models.keys()]
    display_cols += ["setfit_best_sector", "setfit_best_prob"]

    # Only include columns that exist
    display_cols = [c for c in display_cols if c in result_df.columns]
    print(result_df[display_cols].head(10).to_string())


if __name__ == "__main__":
    main()
