# SetFit Classification Guide

## What is SetFit and why use it here?

[SetFit](https://github.com/huggingface/setfit) (Sentence Transformer Fine-Tuning) is a framework for training text classifiers with very few labelled examples — typically 10-20 positives and 15-25 negatives per class. This makes it well suited to the problem here: identifying which sector a company belongs to based on a short text description scraped from their website, without needing thousands of labelled training examples.

Each classifier is **binary**: it answers "is this firm in sector X?" with a probability score between 0 and 1. Running several classifiers in parallel gives a score per sector for each company.

---

## Folder structure

```
companieshouseopen/
├── training_data/
│   ├── sectors/            # One YAML file per sector, containing positive/negative examples
│   │   ├── health_tech.yaml
│   │   ├── clean_energy.yaml
│   │   ├── defence.yaml
│   │   └── advanced_manufacturing.yaml
│   └── sets/               # One YAML file per model set, grouping sectors together
│       └── general.yaml
├── models/
│   └── general/            # Trained models saved here, one subfolder per sector
│       ├── health_tech_classifier/
│       ├── clean_energy_classifier/
│       └── ...
└── python/
    ├── train_binary_classifiers.py   # Train models from YAML data
    └── classify_with_setfit.py       # Apply trained models to a parquet dataframe
```

---

## Key concepts

### Sectors

A **sector** is a single binary classifier. Its training data lives in `training_data/sectors/<sector_name>.yaml` and contains:

- **positives** — text snippets that ARE this sector
- **negatives** — text snippets that are NOT this sector, including "hard negatives" (things that look similar but aren't, e.g. care homes for health_tech)

### Model sets

A **model set** is a named group of sectors trained and stored together. Defined in `training_data/sets/<set_name>.yaml`.

The rationale for having sets is that different classification tasks call for different groupings:

- **`general`** — the broad four-sector sweep run against the full company dataset to find candidates
- **`health_detail`** (example) — a deeper cut within health tech, with sub-classifiers like `preventative_health` and `child_health_tech`, run only on firms that already scored highly in `general`

Models for each set are saved to `models/<set_name>/` so sets never interfere with each other.

### Cross-listing positives

A set YAML can set `cross_list_positives: true`. When enabled, the positives from each sector in the set are automatically added as negatives for all other sectors in that set. This is especially useful for fine-grained sets like `health_detail`, where you want `preventative_health` to learn to distinguish itself from `child_health_tech` and vice versa.

---

## Adding new training data

### Add a new sector

1. Create `training_data/sectors/my_sector.yaml`:

```yaml
name: my_sector
description: One-line description of what this sector covers

positive:
  - "Example text that clearly belongs to this sector"
  - |
    Multi-line example using the pipe character.
    Useful for longer web text snippets.

negative:
  # Key confusers - things that look similar but aren't
  - "Example that looks related but isn't this sector"
  - "Another negative example"
```

2. Add the sector to a set (or create a new set — see below).

### Add a new model set

Create `training_data/sets/my_set.yaml`:

```yaml
name: my_set
description: What this set is for
cross_list_positives: false   # set true to auto-add other sectors' positives as negatives

sectors:
  - my_sector
  - another_sector
```

---

## Step-by-step workflow

### 1. Check what's available

```bash
# List available model sets
python python/train_binary_classifiers.py --list-sets

# List available sector YAML files
python python/train_binary_classifiers.py --list-sectors
```

### 2. Train a model set

```bash
# Train the general set (default)
python python/train_binary_classifiers.py

# Train a specific set
python python/train_binary_classifiers.py --model-set health_detail

# Use a different training data directory
python python/train_binary_classifiers.py --model-set general --data-dir /path/to/training_data
```

Training runs each sector classifier in sequence, saves models to `models/<set_name>/`, and prints a quick test report showing predictions on held-out examples.

### 3. Classify a dataset

```bash
# Classify using the general set (default)
python python/classify_with_setfit.py \
    --input local/samplebatch.parquet \
    --output local/samplebatch_classified.parquet

# Classify using a specific model set
python python/classify_with_setfit.py \
    --model-set health_detail \
    --input local/health_candidates.parquet \
    --output local/health_candidates_detail.parquet

# Adjust the threshold for assigning a sector (default 0.5)
python python/classify_with_setfit.py \
    --model-set general \
    --threshold 0.6 \
    --input local/samplebatch.parquet \
    --output local/samplebatch_classified.parquet

# Classify against only a subset of sectors in the set
python python/classify_with_setfit.py \
    --model-set general \
    --sectors health_tech clean_energy \
    --input local/samplebatch.parquet \
    --output local/samplebatch_classified.parquet
```

### 4. Output columns

The classifier adds these columns to the output parquet:

| Column | Description |
|---|---|
| `setfit_<sector>` | Probability score 0–1 for each sector |
| `setfit_best_sector` | Sector with highest score, or `other` if none exceeds threshold |
| `setfit_best_prob` | The highest probability score |

---

## Typical two-stage workflow

For deep sub-sector analysis, a two-stage approach avoids running slow fine-grained models against the full dataset:

```bash
# Stage 1: broad sweep across all companies
python python/classify_with_setfit.py \
    --model-set general \
    --input local/all_companies.parquet \
    --output local/all_companies_general.parquet

# (In R or Python: filter to health_tech score > 0.5)

# Stage 2: detailed health sub-classification on candidates only
python python/classify_with_setfit.py \
    --model-set health_detail \
    --input local/health_candidates.parquet \
    --output local/health_candidates_detail.parquet
```

---

## Tips for improving classifier quality

- **Hard negatives matter most.** A care home is an easy negative for health_tech; a GP practice IT system is a hard one. The more hard negatives you include, the sharper the boundary.
- **Use real web text snippets** where possible — they match what the model will see at inference time better than hand-written descriptions.
- **Comment your examples.** The YAML supports `#` comments — note where a specific example came from or why it was added. This helps future editing.
- **`cross_list_positives: true`** is most valuable when sectors in a set are easily confused with each other (e.g. health sub-sectors). For the general set covering very different sectors it makes less difference.
- After training, the test report printed to the terminal shows ✓/✗ for each expected high/low prediction — a quick sanity check before running against real data.
