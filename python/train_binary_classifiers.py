"""
Train binary SetFit classifiers for sector classification.

Each classifier answers: "Is this firm in sector X?" (yes/no with probability)

Usage:
    python train_binary_classifiers.py

This will train classifiers and save them to ../models/
To add more sectors, add entries to SECTOR_TRAINING_DATA below.
"""

import os
from setfit import SetFitModel, Trainer, TrainingArguments
from datasets import Dataset

# Base model - can swap for smaller/faster if needed
BASE_MODEL = "BAAI/bge-base-en-v1.5"

# Output directory for trained models
MODELS_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "models")

# =============================================================================
# TRAINING DATA - Edit these examples to improve classifier quality
#
# Guidelines:
# - Aim for 10-20 positive examples and 15-25 negative examples per sector
# - Negatives should include "hard negatives" - things that LOOK similar but aren't
# - Use real website text snippets where possible (more realistic than made-up descriptions)
# =============================================================================

SECTOR_TRAINING_DATA = {
    "health_tech": {
        "positive": [
            # Medical devices
            "Medical device company developing cardiac monitoring systems and wearable health sensors",
            "We manufacture diagnostic imaging equipment for hospitals and clinics worldwide",
            "Surgical robotics company creating minimally invasive surgical systems",
            "Point of care diagnostics manufacturer specialising in rapid testing devices",
            "Implantable medical devices for cardiac rhythm management",

            # Digital health / health software
            "AI-powered diagnostic platform helping radiologists detect cancer earlier",
            "Electronic health records software connecting GPs, hospitals and pharmacies",
            "Telemedicine platform enaInIbling remote consultations with specialists",
            "Clinical decision support software using machine learning",
            "Mobile health app for diabetes management and glucose tracking",

            # Biotech / pharma tech
            "Biotechnology company researching mRNA vaccine technologies",
            "Gene therapy company developing treatments for rare diseases",
            "Pharmaceutical technology firm specialising in drug delivery systems",
            "Bioinformatics company providing genomic analysis services",
            "Clinical trials software platform for pharmaceutical companies",

            # Health data / analytics
            "Health data analytics platform helping NHS trusts improve patient outcomes",
            "Population health management software for integrated care systems",
            "Medical imaging AI company specialising in pathology analysis",         

        ],
        "negative": [
            # AI/ML/tech that is NOT health-related (key confusers!)
            "AI-powered marketing analytics platform for e-commerce businesses",
            "Machine learning solutions for retail inventory optimisation",
            "Data science consultancy helping businesses leverage their data",
            "Artificial intelligence for advertising and customer targeting",
            "Predictive analytics platform for financial services",
            "Digital transformation consultancy helping businesses grow online",
            "We build chatbots and AI assistants for customer service",
            "Computer vision solutions for manufacturing quality control",

            # Health services that are NOT tech (care providers)
            "Residential care home providing 24-hour nursing for elderly residents",
            "GP surgery serving 8000 registered patients in South Yorkshire",
            "Dental practice offering NHS and private dentistry",
            "Physiotherapy and sports injury rehabilitation clinic",
            "Domiciliary care agency providing home care services",
            "Nursing home with specialist dementia care unit",
            "Mental health counselling and psychotherapy services",
            "Private hospital offering elective surgery and consultations",

            # General other businesses
            "Accountancy firm providing tax and audit services",
            "Law firm specialising in commercial property and conveyancing",
            "Recruitment agency for engineering and manufacturing sectors",
            "Web design agency creating websites for small businesses",
            "Commercial cleaning services for offices and industrial premises",
            "Construction company specialising in residential developments",

            # Specific examples summarised from web text
            # Primary Care Doncaster
            """We work behind the scenes with local GP practices to provide additional essential care. 
            We support resilience and quality improvement within general practice and provide corporate support 
            and infrastructure for PCNs and practices."""
        ],
    },

    "clean_energy": {
        "positive": [
            # Solar
            "Solar panel installation company serving domestic and commercial customers",
            "We design and install photovoltaic systems for homes and businesses across Yorkshire",
            "Solar farm developer and operator with 50MW of installed capacity",
            "Rooftop solar specialists providing renewable energy solutions",

            # Wind
            "Wind turbine manufacturer supplying components to offshore wind farms",
            "Onshore wind farm developer and operator",
            "Wind turbine maintenance and servicing company",
            "Blade inspection and repair services for wind energy sector",

            # Heat pumps / energy efficiency
            "Air source heat pump installation specialists for residential properties",
            "Ground source heat pump systems for commercial buildings",
            "Energy efficiency consultancy helping businesses reduce carbon footprint",
            "Retrofit insulation and energy efficiency upgrades for housing",

            # Hydrogen / batteries / storage
            "Green hydrogen production facility using renewable electricity",
            "Battery energy storage systems for grid balancing",
            "EV charging infrastructure installer and operator",
            "Electric vehicle charge point network across South Yorkshire",

            # General clean energy
            "Renewable energy developer focused on solar and wind projects",
            "Clean energy consultancy helping organisations achieve net zero",
            "Carbon management and sustainability consultancy",
            "District heating network operator using waste heat recovery",
        ],
        "negative": [
            # Oil & gas / fossil fuels (key confusers!)
            "Gas boiler installation and servicing company",
            "Oil and gas engineering services",
            "Petroleum distribution and fuel delivery",
            "Natural gas pipeline contractor",
            "Heating oil supplier for domestic customers",
            "Gas engineer providing boiler repairs and servicing",

            # General energy that ISN'T clean
            "Diesel generator hire and sales",
            "Backup power systems using diesel generators",
            "Fuel station operator with petrol and diesel",

            # Construction / trades (might mention 'energy')
            "General building contractor and property developer",
            "Electrical contractor for commercial and industrial premises",
            "HVAC installation and maintenance services",
            "Plumbing and heating contractor",

            # Environmental but not energy
            "Waste management and recycling company",
            "Environmental consultancy for planning applications",
            "Asbestos removal and environmental remediation",

            # General other
            "Accountancy firm providing business advisory services",
            "Recruitment agency for engineering sector",
            "IT managed services provider",
            "Commercial property management company",
        ],
    },

    "defence": {
        "positive": [
            # Defence manufacturing
            "Defence contractor manufacturing armoured vehicle components",
            "Munitions manufacturer supplying the Ministry of Defence",
            "Military communications systems and secure radio equipment",
            "Ballistic protection systems for military and law enforcement",

            # Aerospace defence
            "Aerospace defence company supplying aircraft components to military programmes",
            "Unmanned aerial vehicle manufacturer for defence applications",
            "Military helicopter maintenance and overhaul services",
            "Fighter aircraft component supplier with MOD contracts",

            # Defence electronics / systems
            "Radar systems manufacturer for naval and air defence",
            "Electronic warfare systems and countermeasures",
            "Military simulation and training systems",
            "Secure communications and encryption technology for defence",

            # Defence services / supply chain
            "Tier one supplier to major defence prime contractors",
            "MOD approved supplier of precision machined components",
            "Defence logistics and supply chain management",
            "Security cleared engineering services for defence sector",

            # Naval / maritime defence
            "Naval vessel systems integration and support",
            "Submarine component manufacturer",
            "Maritime surveillance systems",
            "Torpedo and underwater weapons systems",
        ],
        "negative": [
            # Security that ISN'T defence (key confusers!)
            "Commercial security services and manned guarding",
            "CCTV installation and alarm systems for businesses",
            "Cybersecurity consultancy for commercial organisations",
            "Private security company providing door staff and event security",

            # Aerospace that ISN'T defence
            "Commercial aircraft maintenance and repair",
            "Aviation training school for private pilots",
            "Aircraft charter and private jet hire",
            "Airport ground handling services",

            # Engineering that ISN'T defence
            "Precision engineering for automotive sector",
            "Industrial machinery manufacturer",
            "Steel fabrication for construction industry",
            "General mechanical engineering services",

            # Emergency services (might mention 'defence')
            "Fire and rescue equipment supplier",
            "Ambulance services and patient transport",
            "First aid training provider",

            # General other
            "Law firm specialising in commercial contracts",
            "Management consultancy firm",
            "Recruitment agency for technical roles",
            "Web development and digital marketing agency",
        ],
    },

    "advanced_manufacturing": {
        "positive": [
            # Precision engineering / CNC
            "Precision CNC machining company specialising in aerospace components",
            "5-axis CNC machining centre for complex geometries",
            "High precision engineering for medical device and aerospace sectors",
            "Precision turned parts manufacturer with Swiss-type lathes",

            # Additive manufacturing / 3D printing
            "Metal 3D printing service bureau for aerospace and medical",
            "Additive manufacturing facility with SLM and EBM capabilities",
            "Rapid prototyping and low volume production using 3D printing",
            "Industrial 3D printing for functional end-use parts",

            # Composites / advanced materials
            "Carbon fibre composite manufacturer for motorsport and aerospace",
            "Advanced composite materials for lightweight structures",
            "Specialist coatings and surface treatments for aerospace",
            "Superalloy and titanium machining specialists",

            # Automation / robotics
            "Industrial automation and robotic systems integrator",
            "Collaborative robot solutions for manufacturing",
            "Automated assembly systems for high volume production",
            "Machine vision and quality inspection systems",

            # Industry 4.0 / smart manufacturing
            "Smart factory solutions with IoT and data analytics",
            "Manufacturing execution systems and digital twin technology",
            "Predictive maintenance solutions using machine learning",
            "Digital manufacturing consultancy and Industry 4.0 implementation",

            # High value manufacturing
            "Precision surface engineering and electroplating for aerospace",
            "Laser cutting and profiling service for precision components",
            "Investment casting foundry for aerospace and energy sectors",
            "Cleanroom manufacturing for semiconductor and medical devices",
        ],
        "negative": [
            # Basic manufacturing (key confusers!)
            "General steel fabrication and welding",
            "Sheet metal work and press forming",
            "Traditional foundry producing iron castings",
            "Manual machine shop with conventional lathes and mills",

            # Construction / trades
            "Structural steelwork for construction",
            "Metal fabrication for building industry",
            "Industrial maintenance and plant repair",
            "Welding and fabrication contractor",

            # Automotive aftermarket (not advanced)
            "Car body repair and vehicle restoration",
            "Exhaust and tyre fitting centre",
            "Vehicle MOT and servicing garage",

            # Food / consumer manufacturing
            "Food processing and packaging company",
            "Bakery and confectionery manufacturer",
            "Plastic injection moulding for consumer products",
            "Packaging manufacturer for food industry",

            # General other
            "IT services and software development",
            "Marketing agency and creative services",
            "Recruitment consultancy for manufacturing sector",
            "Health and safety training provider",
            "Accountancy and business advisory firm",
            "Commercial cleaning services",
        ],
    },
}


def train_binary_classifier(sector_name: str, positive_examples: list, negative_examples: list) -> SetFitModel:
    """
    Train a binary SetFit classifier for one sector.

    Args:
        sector_name: Name of the sector (used for saving)
        positive_examples: List of text examples that ARE this sector
        negative_examples: List of text examples that are NOT this sector

    Returns:
        Trained SetFitModel
    """
    print(f"\n{'='*60}")
    print(f"Training classifier for: {sector_name}")
    print(f"{'='*60}")
    print(f"Positive examples: {len(positive_examples)}")
    print(f"Negative examples: {len(negative_examples)}")

    # Prepare dataset
    train_data = {
        "text": positive_examples + negative_examples,
        "label": [1] * len(positive_examples) + [0] * len(negative_examples)
    }
    dataset = Dataset.from_dict(train_data)

    # Load base model
    print(f"Loading base model: {BASE_MODEL}")
    model = SetFitModel.from_pretrained(BASE_MODEL)

    # Train
    trainer = Trainer(
        model=model,
        train_dataset=dataset,
        args=TrainingArguments(
            batch_size=8,
            num_epochs=1,  # SetFit typically needs very few epochs
        ),
    )

    print("Training...")
    trainer.train()

    # Save model
    os.makedirs(MODELS_DIR, exist_ok=True)
    model_path = os.path.join(MODELS_DIR, f"{sector_name}_classifier")
    model.save_pretrained(model_path)
    print(f"Model saved to: {model_path}")

    return model


def load_classifier(sector_name: str) -> SetFitModel:
    """Load a previously trained classifier."""
    model_path = os.path.join(MODELS_DIR, f"{sector_name}_classifier")
    if not os.path.exists(model_path):
        raise FileNotFoundError(f"No trained model found at {model_path}. Run training first.")
    return SetFitModel.from_pretrained(model_path)


def test_classifier(model: SetFitModel, sector_name: str):
    """Run some quick tests on a trained classifier."""
    print(f"\n{'='*60}")
    print(f"Testing classifier: {sector_name}")
    print(f"{'='*60}")

    # Sector-specific test cases
    test_cases = {
        "health_tech": [
            # Should be HIGH
            ("Medical device company developing AI-powered diagnostic imaging", "high"),
            ("Digital health platform for remote patient monitoring", "high"),
            ("Biotech firm researching gene therapies for rare diseases", "high"),
            # Should be LOW (confusers)
            ("AI marketing analytics platform for e-commerce", "low"),
            ("Care home providing residential nursing for elderly", "low"),
            ("Machine learning consultancy for retail businesses", "low"),
            # Ambiguous
            ("Software development company", "?"),
            ("Healthcare recruitment agency", "?"),
        ],
        "clean_energy": [
            # Should be HIGH
            ("Solar panel installation and renewable energy systems", "high"),
            ("Wind turbine maintenance and offshore wind services", "high"),
            ("Heat pump installer specialising in air source systems", "high"),
            # Should be LOW (confusers)
            ("Gas boiler installation and central heating services", "low"),
            ("Electrical contractor for commercial properties", "low"),
            ("Environmental consultancy for planning applications", "low"),
            # Ambiguous
            ("Energy management consultancy", "?"),
            ("Building services engineering company", "?"),
        ],
        "defence": [
            # Should be HIGH
            ("Defence contractor manufacturing armoured vehicle components", "high"),
            ("Aerospace company supplying military aircraft systems", "high"),
            ("Radar and electronic warfare systems manufacturer", "high"),
            # Should be LOW (confusers)
            ("Commercial security services and CCTV installation", "low"),
            ("Private aircraft charter and aviation services", "low"),
            ("Precision engineering for automotive industry", "low"),
            # Ambiguous
            ("Aerospace component manufacturer", "?"),
            ("Security consultancy services", "?"),
        ],
        "advanced_manufacturing": [
            # Should be HIGH
            ("Precision CNC machining for aerospace and medical sectors", "high"),
            ("Metal 3D printing and additive manufacturing services", "high"),
            ("Industrial automation and robotic systems integrator", "high"),
            # Should be LOW (confusers)
            ("General steel fabrication and welding services", "low"),
            ("Car body repair and vehicle restoration", "low"),
            ("Food processing and packaging company", "low"),
            # Ambiguous
            ("Engineering company", "?"),
            ("Manufacturing services", "?"),
        ],
    }

    # Use sector-specific tests if available, otherwise use generic
    if sector_name in test_cases:
        tests = test_cases[sector_name]
    else:
        tests = [
            ("Generic test text 1", "?"),
            ("Generic test text 2", "?"),
        ]

    print("\nTest predictions (expected: high/low/?):")
    print("-" * 60)

    for text, expected in tests:
        probs = model.predict_proba([text])[0]
        prob_positive = probs[1]  # Probability of being this sector

        # Visual indicator
        bar = "█" * int(prob_positive * 20) + "░" * (20 - int(prob_positive * 20))

        # Check if prediction matches expectation
        if expected == "high":
            marker = "✓" if prob_positive > 0.5 else "✗"
        elif expected == "low":
            marker = "✓" if prob_positive < 0.5 else "✗"
        else:
            marker = "?"

        print(f"{prob_positive:.2f} [{bar}] {marker} {text[:50]}...")


def train_all_sectors():
    """Train classifiers for all sectors defined in SECTOR_TRAINING_DATA."""
    trained_models = {}

    for sector_name, data in SECTOR_TRAINING_DATA.items():
        model = train_binary_classifier(
            sector_name=sector_name,
            positive_examples=data["positive"],
            negative_examples=data["negative"],
        )
        trained_models[sector_name] = model

        # Test the model
        test_classifier(model, sector_name)

    return trained_models


def classify_text_all_sectors(text: str, models: dict = None) -> dict:
    """
    Classify a text against all trained sector classifiers.

    Args:
        text: The text to classify
        models: Dict of {sector_name: model}. If None, loads from disk.

    Returns:
        Dict of {sector_name: probability}
    """
    if models is None:
        models = {}
        for sector_name in SECTOR_TRAINING_DATA.keys():
            try:
                models[sector_name] = load_classifier(sector_name)
            except FileNotFoundError:
                print(f"Warning: No model found for {sector_name}")

    results = {}
    for sector_name, model in models.items():
        probs = model.predict_proba([text])[0]
        results[sector_name] = float(probs[1])  # Probability of positive class

    return results


if __name__ == "__main__":
    import time

    print("="*60)
    print("SetFit Binary Classifier Training")
    print("="*60)
    print(f"Base model: {BASE_MODEL}")
    print(f"Output directory: {MODELS_DIR}")
    print(f"Sectors to train: {list(SECTOR_TRAINING_DATA.keys())}")

    start_time = time.time()

    # Train all sectors
    models = train_all_sectors()

    elapsed = time.time() - start_time
    print(f"\n{'='*60}")
    print(f"Training complete in {elapsed:.1f} seconds")
    print(f"{'='*60}")

    # Demo: classify a few example texts
    print("\n\nDemo: Classifying example texts with all models")
    print("-" * 60)

    demo_texts = [
        "Medical imaging AI startup using deep learning for cancer detection",
        "Digital marketing agency specialising in SEO and social media",
        "Care home for elderly residents with dementia support",
    ]

    for text in demo_texts:
        print(f"\n{text[:70]}...")
        scores = classify_text_all_sectors(text, models)
        for sector, prob in sorted(scores.items(), key=lambda x: -x[1]):
            bar = "█" * int(prob * 20) + "░" * (20 - int(prob * 20))
            print(f"  {sector}: {prob:.2f} [{bar}]")
