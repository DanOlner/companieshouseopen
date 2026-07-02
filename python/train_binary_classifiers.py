"""
Train binary SetFit classifiers for sector classification.

Each classifier answers: "Is this firm in sector X?" (yes/no with probability)

Training data is loaded from YAML files in training_data/sectors/.
Model sets (groupings of sectors) are defined in training_data/sets/.

Usage:
    python train_binary_classifiers.py
    python train_binary_classifiers.py --model-set general
    python train_binary_classifiers.py --model-set health_detail --data-dir ../training_data

To add a new sector:
    1. Create training_data/sectors/my_sector.yaml
    2. Add 'my_sector' to a set file in training_data/sets/
    3. Run with --model-set <that set>

To add a new model set:
    1. Create training_data/sets/my_set.yaml listing sectors to include
    2. Run with --model-set my_set
"""

import os
import argparse
import yaml
from setfit import SetFitModel, Trainer, TrainingArguments
from datasets import Dataset

# Base model - can swap for smaller/faster if needed
BASE_MODEL = "BAAI/bge-base-en-v1.5"

# Root of the repo (one level up from python/)
REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# Default directories
DEFAULT_DATA_DIR = os.path.join(REPO_ROOT, "training_data")
DEFAULT_MODELS_DIR = os.path.join(REPO_ROOT, "models")


# =============================================================================
# YAML loading
# =============================================================================

def load_sector_data(sector_name: str, data_dir: str) -> dict:
    """
    Load training data for one sector from its YAML file.

    Returns dict with keys 'positive' and 'negative' (lists of strings).
    """
    path = os.path.join(data_dir, "sectors", f"{sector_name}.yaml")
    if not os.path.exists(path):
        raise FileNotFoundError(
            f"No training data found for sector '{sector_name}' at {path}"
        )
    with open(path, "r") as f:
        data = yaml.safe_load(f)

    if "positive" not in data or "negative" not in data:
        raise ValueError(
            f"Sector file {path} must have 'positive' and 'negative' keys"
        )

    return {
        "positive": [str(s).strip() for s in data["positive"]],
        "negative": [str(s).strip() for s in data["negative"]],
    }


def load_model_set(set_name: str, data_dir: str) -> dict:
    """
    Load a model set definition from its YAML file.

    Returns dict with keys:
        sectors: list of sector names
        cross_list_positives: bool
    """
    path = os.path.join(data_dir, "sets", f"{set_name}.yaml")
    if not os.path.exists(path):
        raise FileNotFoundError(
            f"No model set found named '{set_name}' at {path}\n"
            f"Available sets: {list_available_sets(data_dir)}"
        )
    with open(path, "r") as f:
        data = yaml.safe_load(f)

    if "sectors" not in data:
        raise ValueError(f"Set file {path} must have a 'sectors' key")

    return {
        "sectors": data["sectors"],
        "cross_list_positives": data.get("cross_list_positives", False),
        "description": data.get("description", ""),
    }


def list_available_sets(data_dir: str) -> list:
    sets_dir = os.path.join(data_dir, "sets")
    if not os.path.exists(sets_dir):
        return []
    return [f[:-5] for f in os.listdir(sets_dir) if f.endswith(".yaml")]


def list_available_sectors(data_dir: str) -> list:
    sectors_dir = os.path.join(data_dir, "sectors")
    if not os.path.exists(sectors_dir):
        return []
    return [f[:-5] for f in os.listdir(sectors_dir) if f.endswith(".yaml")]


# =============================================================================
# Training
# =============================================================================

def train_binary_classifier(
    sector_name: str,
    positive_examples: list,
    negative_examples: list,
    models_dir: str,
) -> SetFitModel:
    """
    Train a binary SetFit classifier for one sector.

    Args:
        sector_name: Name of the sector (used for saving)
        positive_examples: List of text examples that ARE this sector
        negative_examples: List of text examples that are NOT this sector
        models_dir: Directory to save the trained model

    Returns:
        Trained SetFitModel
    """
    print(f"\n{'='*60}")
    print(f"Training classifier for: {sector_name}")
    print(f"{'='*60}")
    print(f"Positive examples: {len(positive_examples)}")
    print(f"Negative examples: {len(negative_examples)}")

    train_data = {
        "text": positive_examples + negative_examples,
        "label": [1] * len(positive_examples) + [0] * len(negative_examples),
    }
    dataset = Dataset.from_dict(train_data)

    print(f"Loading base model: {BASE_MODEL}")
    model = SetFitModel.from_pretrained(BASE_MODEL)

    trainer = Trainer(
        model=model,
        train_dataset=dataset,
        args=TrainingArguments(
            batch_size=8,
            num_epochs=1,
        ),
    )

    print("Training...")
    trainer.train()

    os.makedirs(models_dir, exist_ok=True)
    model_path = os.path.join(models_dir, f"{sector_name}_classifier")
    model.save_pretrained(model_path)
    print(f"Model saved to: {model_path}")

    return model


def test_classifier(model: SetFitModel, sector_name: str, data_dir: str):
    """Run quick tests on a trained classifier using sector-specific test cases."""
    print(f"\n{'='*60}")
    print(f"Testing classifier: {sector_name}")
    print(f"{'='*60}")

    # Sector-specific test cases
    test_cases = {
        "health_tech": [
            ("Medical device company developing AI-powered diagnostic imaging", "high"),
            ("Digital health platform for remote patient monitoring", "high"),
            ("Biotech firm researching gene therapies for rare diseases", "high"),
            ("AI marketing analytics platform for e-commerce", "low"),
            ("Care home providing residential nursing for elderly", "low"),
            ("Machine learning consultancy for retail businesses", "low"),
            ("Software development company", "?"),
            ("Healthcare recruitment agency", "?"),
        ],
        "clean_energy": [
            ("Solar panel installation and renewable energy systems", "high"),
            ("Wind turbine maintenance and offshore wind services", "high"),
            ("Heat pump installer specialising in air source systems", "high"),
            ("Gas boiler installation and central heating services", "low"),
            ("Electrical contractor for commercial properties", "low"),
            ("Environmental consultancy for planning applications", "low"),
            ("Energy management consultancy", "?"),
            ("Building services engineering company", "?"),
        ],
        "defence": [
            ("Defence contractor manufacturing armoured vehicle components", "high"),
            ("Aerospace company supplying military aircraft systems", "high"),
            ("Radar and electronic warfare systems manufacturer", "high"),
            ("Commercial security services and CCTV installation", "low"),
            ("Private aircraft charter and aviation services", "low"),
            ("Precision engineering for automotive industry", "low"),
            ("Aerospace component manufacturer", "?"),
            ("Security consultancy services", "?"),
        ],
        "advanced_manufacturing": [
            ("Precision CNC machining for aerospace and medical sectors", "high"),
            ("Metal 3D printing and additive manufacturing services", "high"),
            ("Industrial automation and robotic systems integrator", "high"),
            ("General steel fabrication and welding services", "low"),
            ("Car body repair and vehicle restoration", "low"),
            ("Food processing and packaging company", "low"),
            ("Engineering company", "?"),
            ("Manufacturing services", "?"),
        ],
    }

    tests = test_cases.get(sector_name, [
        ("Generic test text 1", "?"),
        ("Generic test text 2", "?"),
    ])

    print("\nTest predictions (expected: high/low/?):")
    print("-" * 60)

    for text, expected in tests:
        probs = model.predict_proba([text])[0]
        prob_positive = probs[1]

        bar = "█" * int(prob_positive * 20) + "░" * (20 - int(prob_positive * 20))

        if expected == "high":
            marker = "✓" if prob_positive > 0.5 else "✗"
        elif expected == "low":
            marker = "✓" if prob_positive < 0.5 else "✗"
        else:
            marker = "?"

        print(f"{prob_positive:.2f} [{bar}] {marker} {text[:50]}...")


def train_model_set(
    set_name: str,
    data_dir: str = DEFAULT_DATA_DIR,
    models_base_dir: str = DEFAULT_MODELS_DIR,
) -> dict:
    """
    Train all classifiers for a named model set.

    Models are saved to models_base_dir/{set_name}/.

    Args:
        set_name: Name of the set (must have a matching YAML in data_dir/sets/)
        data_dir: Root of training data directory
        models_base_dir: Root of models directory

    Returns:
        Dict of {sector_name: trained model}
    """
    model_set = load_model_set(set_name, data_dir)
    sectors = model_set["sectors"]
    cross_list = model_set["cross_list_positives"]
    models_dir = os.path.join(models_base_dir, set_name)

    print(f"\nModel set: {set_name}")
    if model_set["description"]:
        print(f"Description: {model_set['description']}")
    print(f"Sectors: {sectors}")
    print(f"Cross-list positives: {cross_list}")
    print(f"Output dir: {models_dir}")

    # Load all sector data upfront (needed for cross-listing)
    all_sector_data = {}
    for sector in sectors:
        all_sector_data[sector] = load_sector_data(sector, data_dir)

    trained_models = {}

    for sector_name in sectors:
        positives = all_sector_data[sector_name]["positive"]
        negatives = all_sector_data[sector_name]["negative"].copy()

        if cross_list:
            extra = sum(
                len(other["positive"])
                for s, other in all_sector_data.items()
                if s != sector_name
            )
            for other_sector, other_data in all_sector_data.items():
                if other_sector != sector_name:
                    negatives.extend(other_data["positive"])
            print(f"\n{sector_name}: adding {extra} cross-listed negatives from other sectors")

        model = train_binary_classifier(
            sector_name=sector_name,
            positive_examples=positives,
            negative_examples=negatives,
            models_dir=models_dir,
        )
        trained_models[sector_name] = model
        test_classifier(model, sector_name, data_dir)

    return trained_models


def load_classifier(sector_name: str, set_name: str, models_base_dir: str = DEFAULT_MODELS_DIR) -> SetFitModel:
    """Load a previously trained classifier from a named model set."""
    model_path = os.path.join(models_base_dir, set_name, f"{sector_name}_classifier")
    if not os.path.exists(model_path):
        raise FileNotFoundError(
            f"No trained model found at {model_path}. Run training first."
        )
    return SetFitModel.from_pretrained(model_path)


def classify_text_all_sectors(text: str, set_name: str, models: dict = None, models_base_dir: str = DEFAULT_MODELS_DIR) -> dict:
    """
    Classify a text against all trained sector classifiers in a model set.

    Args:
        text: The text to classify
        set_name: Model set to use
        models: Pre-loaded dict of {sector_name: model}. If None, loads from disk.
        models_base_dir: Root models directory

    Returns:
        Dict of {sector_name: probability}
    """
    if models is None:
        models_dir = os.path.join(models_base_dir, set_name)
        models = {}
        for entry in os.listdir(models_dir):
            if entry.endswith("_classifier"):
                sector = entry[:-len("_classifier")]
                try:
                    models[sector] = load_classifier(sector, set_name, models_base_dir)
                except FileNotFoundError:
                    print(f"Warning: Could not load model for {sector}")

    results = {}
    for sector_name, model in models.items():
        probs = model.predict_proba([text])[0]
        results[sector_name] = float(probs[1])

    return results


if __name__ == "__main__":
    import time

    parser = argparse.ArgumentParser(
        description="Train SetFit binary classifiers for a named model set"
    )
    parser.add_argument(
        "--model-set", "-m",
        default="general",
        help="Name of the model set to train (default: general)"
    )
    parser.add_argument(
        "--data-dir", "-d",
        default=DEFAULT_DATA_DIR,
        help=f"Path to training data directory (default: {DEFAULT_DATA_DIR})"
    )
    parser.add_argument(
        "--models-dir",
        default=DEFAULT_MODELS_DIR,
        help=f"Root directory for saving models (default: {DEFAULT_MODELS_DIR})"
    )
    parser.add_argument(
        "--list-sets",
        action="store_true",
        help="List available model sets and exit"
    )
    parser.add_argument(
        "--list-sectors",
        action="store_true",
        help="List available sectors and exit"
    )
    args = parser.parse_args()

    if args.list_sets:
        sets = list_available_sets(args.data_dir)
        print(f"Available model sets in {args.data_dir}/sets/:")
        for s in sorted(sets):
            print(f"  {s}")
        exit(0)

    if args.list_sectors:
        sectors = list_available_sectors(args.data_dir)
        print(f"Available sectors in {args.data_dir}/sectors/:")
        for s in sorted(sectors):
            print(f"  {s}")
        exit(0)

    print("=" * 60)
    print("SetFit Binary Classifier Training")
    print("=" * 60)
    print(f"Base model: {BASE_MODEL}")
    print(f"Model set:  {args.model_set}")
    print(f"Data dir:   {args.data_dir}")
    print(f"Models dir: {args.models_dir}/{args.model_set}")

    start_time = time.time()
    models = train_model_set(
        set_name=args.model_set,
        data_dir=args.data_dir,
        models_base_dir=args.models_dir,
    )
    elapsed = time.time() - start_time

    print(f"\n{'='*60}")
    print(f"Training complete in {elapsed:.1f} seconds")
    print(f"{'='*60}")

    # Demo classification
    print("\n\nDemo: Classifying example texts with all models")
    print("-" * 60)

    demo_texts = [
        "Medical imaging AI startup using deep learning for cancer detection",
        "Digital marketing agency specialising in SEO and social media",
        "Care home for elderly residents with dementia support",
    ]

    for text in demo_texts:
        print(f"\n{text[:70]}...")
        scores = classify_text_all_sectors(text, args.model_set, models, args.models_dir)
        for sector, prob in sorted(scores.items(), key=lambda x: -x[1]):
            bar = "█" * int(prob * 20) + "░" * (20 - int(prob * 20))
            print(f"  {sector}: {prob:.2f} [{bar}]")
