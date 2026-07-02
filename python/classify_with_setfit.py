"""
Classify company text using trained SetFit binary classifiers.

Loads pre-trained models from ../models/{model_set}/ and applies them to a parquet dataframe.
Each sector classifier outputs a probability score (0-1) for whether the firm
belongs to that sector.

Usage:
    python classify_with_setfit.py --input ../local/samplebatch.parquet --output ../local/out.parquet
    python classify_with_setfit.py --model-set health_detail --input ... --output ...

Requires trained models from train_binary_classifiers.py
"""

import os
import argparse
import time
import yaml
import pandas as pd
from setfit import SetFitModel

# Root of the repo (one level up from python/)
REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

DEFAULT_MODELS_DIR = os.path.join(REPO_ROOT, "models")
DEFAULT_DATA_DIR = os.path.join(REPO_ROOT, "training_data")


def load_sectors_for_set(set_name: str, data_dir: str) -> list:
    """
    Load the list of sectors for a model set from its YAML definition.

    Falls back to scanning the model directory if no set YAML is found.
    """
    set_path = os.path.join(data_dir, "sets", f"{set_name}.yaml")
    if os.path.exists(set_path):
        with open(set_path, "r") as f:
            data = yaml.safe_load(f)
        return data["sectors"]

    # Fallback: infer from what's in the model directory
    models_dir = os.path.join(DEFAULT_MODELS_DIR, set_name)
    if os.path.exists(models_dir):
        sectors = [
            entry[:-len("_classifier")]
            for entry in os.listdir(models_dir)
            if entry.endswith("_classifier")
        ]
        if sectors:
            print(f"Warning: No set YAML found for '{set_name}', inferred sectors from model directory: {sectors}")
            return sectors

    raise FileNotFoundError(
        f"Cannot determine sectors for model set '{set_name}'. "
        f"No set YAML found at {set_path} and no trained models found."
    )


def load_all_classifiers(set_name: str, models_base_dir: str, sectors: list = None) -> dict:
    """
    Load all trained SetFit classifiers for a model set.

    Args:
        set_name: Name of the model set (subdirectory under models_base_dir)
        models_base_dir: Root models directory
        sectors: Explicit list of sectors. If None, infers from model directory.

    Returns:
        Dict of {sector_name: SetFitModel}
    """
    models_dir = os.path.join(models_base_dir, set_name)

    if sectors is None:
        if not os.path.exists(models_dir):
            raise FileNotFoundError(
                f"No model directory found at {models_dir}. "
                f"Run train_binary_classifiers.py --model-set {set_name} first."
            )
        sectors = [
            entry[:-len("_classifier")]
            for entry in os.listdir(models_dir)
            if entry.endswith("_classifier")
        ]

    models = {}
    for sector in sectors:
        model_path = os.path.join(models_dir, f"{sector}_classifier")
        if os.path.exists(model_path):
            print(f"Loading classifier: {sector}")
            models[sector] = SetFitModel.from_pretrained(model_path)
        else:
            print(f"Warning: No trained model found for '{sector}' at {model_path}")

    if not models:
        raise FileNotFoundError(
            f"No trained models found in {models_dir}. "
            f"Run train_binary_classifiers.py --model-set {set_name} first."
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
        probs = model.predict_proba(texts, batch_size=batch_size)
        results[sector_name] = [float(p[1]) for p in probs]
    return results


def classify_dataframe(
    df: pd.DataFrame,
    text_column: str,
    models: dict,
    batch_size: int = 32,
    threshold: float = 0.5,
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
    texts = df[text_column].fillna("").tolist()

    print(f"Classifying {len(texts)} texts...")
    results = classify_texts_batch(texts, models, batch_size)

    result_df = df.copy()

    for sector_name, probs in results.items():
        result_df[f"setfit_{sector_name}"] = probs

    sector_cols = [f"setfit_{s}" for s in models.keys()]

    result_df["setfit_best_prob"] = result_df[sector_cols].max(axis=1)
    result_df["setfit_best_sector_raw"] = result_df[sector_cols].idxmax(axis=1).str.replace("setfit_", "")

    result_df["setfit_best_sector"] = result_df.apply(
        lambda row: row["setfit_best_sector_raw"] if row["setfit_best_prob"] >= threshold else "other",
        axis=1,
    )

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
        "--model-set", "-m",
        default="general",
        help="Model set to use for classification (default: general)"
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
        help="Explicit list of sectors to classify (default: all in the model set)"
    )
    parser.add_argument(
        "--models-dir",
        default=DEFAULT_MODELS_DIR,
        help=f"Root directory for trained models (default: {DEFAULT_MODELS_DIR})"
    )
    parser.add_argument(
        "--data-dir",
        default=DEFAULT_DATA_DIR,
        help=f"Training data directory, used to resolve set definitions (default: {DEFAULT_DATA_DIR})"
    )

    args = parser.parse_args()

    # Load data
    print(f"Loading data from: {args.input}")
    df = pd.read_parquet(args.input)
    print(f"Loaded {len(df)} rows")

    if args.text_column not in df.columns:
        available = ", ".join(df.columns[:10])
        raise ValueError(
            f"Column '{args.text_column}' not found. Available columns: {available}..."
        )

    # Resolve sectors
    sectors = args.sectors
    if sectors is None:
        sectors = load_sectors_for_set(args.model_set, args.data_dir)

    # Load models
    print(f"\nLoading classifiers from: {args.models_dir}/{args.model_set}/")
    models = load_all_classifiers(args.model_set, args.models_dir, sectors)
    print(f"Loaded {len(models)} classifiers: {list(models.keys())}")

    # Classify
    print(f"\nClassifying with batch size {args.batch_size}...")
    start_time = time.time()

    result_df = classify_dataframe(
        df,
        args.text_column,
        models,
        batch_size=args.batch_size,
        threshold=args.threshold,
    )

    elapsed = time.time() - start_time

    # Save results
    result_df.to_parquet(args.output)
    print(f"\nResults saved to: {args.output}")

    print(f"\n{'='*60}")
    print(f"Classification complete")
    print(f"{'='*60}")
    print(f"Model set: {args.model_set}")
    print(f"Processed {len(result_df)} rows in {elapsed:.2f} seconds")
    print(f"Average: {elapsed / len(result_df) * 1000:.2f} ms per row")

    print(f"\nSector distribution (threshold={args.threshold}):")
    print(result_df["setfit_best_sector"].value_counts())

    print(f"\nSample results:")
    display_cols = ["CompanyName"] if "CompanyName" in result_df.columns else []
    display_cols += [f"setfit_{s}" for s in models.keys()]
    display_cols += ["setfit_best_sector", "setfit_best_prob"]
    display_cols = [c for c in display_cols if c in result_df.columns]
    print(result_df[display_cols].head(10).to_string())


if __name__ == "__main__":
    main()
