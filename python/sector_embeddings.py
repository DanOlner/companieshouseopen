"""
Sector classification using sentence-transformers embeddings.
Reads company data from parquet, classifies into bespoke sectors.

Setup:
    pip install sentence-transformers pandas pyarrow numpy
"""

import pandas as pd
import numpy as np
from sentence_transformers import SentenceTransformer


# Sector definitions - descriptions used to create reference embeddings
SECTOR_DESCRIPTIONS = {
    "clean_energy": "Renewable energy company specialising in solar panels, wind turbines, batteries, grid technology, hydrogen fuel cells, sustainable power generation, carbon reduction, net zero",
    "health_tech": "Medical technology company developing diagnostic devices, digital health platforms, pharmaceutical solutions, patient monitoring, clinical diagnostics, biotech, telemedicine",
    "defence": "Defence and aerospace company providing military equipment, security systems, surveillance technology, radar systems, naval systems, ammunition, MOD contracts",
    "advanced_manufacturing": "Precision engineering and manufacturing company using automation, robotics, CNC machining, fabrication, prototyping, industrial production, quality control"
}


def load_model(model_name="all-MiniLM-L6-v2"):
    """Load sentence transformer model. Downloads on first use (~80MB for MiniLM)."""
    print(f"Loading model: {model_name}")
    return SentenceTransformer(model_name)


def get_sector_embeddings(model):
    """Pre-compute embeddings for each sector description."""
    print("Computing sector reference embeddings...")
    return {
        sector: model.encode(desc, convert_to_numpy=True)
        for sector, desc in SECTOR_DESCRIPTIONS.items()
    }


def cosine_similarity(a, b):
    """Compute cosine similarity between two vectors."""
    return np.dot(a, b) / (np.linalg.norm(a) * np.linalg.norm(b))


def classify_text(text, model, sector_embeddings):
    """
    Classify a single text against all sectors.
    Returns dict of sector -> similarity score.
    """
    if not text or pd.isna(text):
        return {sector: 0.0 for sector in sector_embeddings}

    text_embedding = model.encode(text, convert_to_numpy=True)

    return {
        sector: cosine_similarity(text_embedding, emb)
        for sector, emb in sector_embeddings.items()
    }


def classify_dataframe(df, text_column, model, sector_embeddings, batch_size=32):
    """
    Classify all rows in a dataframe.
    Adds columns for each sector's similarity score plus best_sector and best_score.
    """
    texts = df[text_column].fillna("").tolist()

    print(f"Encoding {len(texts)} texts in batches of {batch_size}...")
    embeddings = model.encode(texts, batch_size=batch_size, show_progress_bar=True)

    # Calculate similarities for each sector
    results = {sector: [] for sector in sector_embeddings}

    print("Computing sector similarities...")
    for text_emb in embeddings:
        for sector, sector_emb in sector_embeddings.items():
            if np.linalg.norm(text_emb) == 0:
                results[sector].append(0.0)
            else:
                results[sector].append(cosine_similarity(text_emb, sector_emb))

    # Add sector columns to dataframe
    result_df = df.copy()
    for sector, scores in results.items():
        result_df[f"sim_{sector}"] = scores

    # Add best match columns
    sector_cols = [f"sim_{s}" for s in sector_embeddings]
    result_df["best_sector"] = result_df[sector_cols].idxmax(axis=1).str.replace("sim_", "")
    result_df["best_score"] = result_df[sector_cols].max(axis=1)

    return result_df


# ============================================================================
# Main workflow
# ============================================================================

# Only run if script run directly, not if imported
if __name__ == "__main__":
    # 1. Load model (first run downloads it)
    model = load_model("all-MiniLM-L6-v2")

    # 2. Pre-compute sector embeddings
    sector_embeddings = get_sector_embeddings(model)

    # 3. Test with example text (Gripple)
    # test_text = """market-leading manufacturer delivering time-saving simplicity and problem-solving innovation to customers worldwide. building services civil construction agriculture landscaping solar solutions utilities rail world-leading manufacturer of wire joining & tensioning systems. employee-owned company. browse the site game changing innovations at gripple we design, build and manufacture products to help you overcome your toughest daily challenges and achieve your goals faster and easier. learn more about gripple here we're no ordinary business 100% employee owned, everyone at gripple has skin in the game and is committed to delivering the best products, quality and service for our customers. learn more about gripple here smarter, lighter, faster – gripple suspension products change the game discover our innovative overhead suspension range designed to allow you to hang a range of commercial, industrial and retail products with speed and flexibility. discover our suspension range the original and best wire joining and tensioning solutions no matter what your fencing experience our solutions will help you deliver professional fencing results. for new installs or repairs, with gripple you can join and tension right every time. explore our fencing solutions this isn’t just a rail dropper install faster, work safer and get more done in each possession window using our swiftline rail dropper. discover the swiftline ole range power-tie changes the game in direct fix solar cable management. engineered for lasting performance, power-tie combines the robust strength and endurance of steel banding with the ease of install and versatility of cable ties. discover power-tie here the smarter way to suspend designed for fast, secure suspension, b-lock is your go-to solution for lighting, ductwork, electrical, and signage installs. discover our b-lock range a faster, safer way to suspend m&e services without the hassle engineered for speed and simplicity, fast trak transforms how you install electrical containment, pipework, ductwork, and more. explore our fast trak range a smarter solution for fast, safe external fibre installations engineered in collaboration with industry experts, our fibre cable support kit speeds up ftth and fttp installs with fewer fixings and faster tensioning. learn more about our fibre cable support kit product led. people powered. established in 1989, we are the original innovators and manufacturers of wire joining and tensioning devices, as well as anchoring, bracing, and suspension systems that have changed the game for many industries. with our innovative, engineered solutions, we’re helping customers in agriculture, construction, erosion control, rail, and solar, to overcome their toughest daily challenges and achieve their goals faster and easier. latest gripple news & case studies building services carleton high school, pontefract 27 november 2025 skills street 2 corporate gripple helps shape yorkshire’s future workforce with new interactive zone at skills street 10 october 2025 gripple mep hires 4193 building services gripple launches hangpro for faster and safer suspensions 22 september 2025 building services vastly simplified installation "we’ve used gripple on past projects and always been delighted with the installation times we’ve been able to achieve. we’ve used fast trak on this project for the first time, and the tool free way in which you can adjust the brackets and secure the tracks vastly simplifies the installation of containment." site supervisor - eyre building services group limited seismic bracing product ranges and technical services provided were unbeatable "we have used gripple’s products in the past, but the new product ranges and technical services provided by gripple were unbeatable" manager, ng bailey landscaping & nurseries the service and products were excellent "the service and products offered by gripple were excellent. easy and quick to install - this was a key driver supporting our decision to use gripple" site supervisor, jw morris civil construction time saving benefits "on site we faced an unexpected issue, by using gripple solutions we were able to quickly resolve this issue and get back on schedule" site supervisor, airco building services perfect bespoke solution "gripple provided us with a bespoke solution that not only complemented the design and architecture of the new and original parts of the building but could also be installed quickly and efficiently. working with gripple to create our required solution couldn’t have been easier. the gripple sales manager made regular site visits, testing the bespoke solution prior, during and after the installation to ensure it met our requirements." terry whittaker - contracts engineer, w h good ltd why use gripple? significant time and cost savings on your project complete, off-site solutions which minimise health and safety concerns innovative, patented solutions designed by an in-house team of engineers considerable reductions in packaging, vehicle movements & embodied co2 a range of support services at every stage of your project, provided by our technical team enquire now interested in our range of gripple solutions? fill out our enquiry form and one of our local gripple experts will be in touch ready to assist. contact details company details we would love to keep in touch with relevant information which we feel would be of interest to you? see our privacy policy here for more details. yes, please keep in touch with relevant information. no thanks, i don't want to hear from gripple. select an alternative."""

    # Other examples...
    # www.primarycaredoncaster.co.uk
    test_text = """skip to content rapidhealth: contact your gp practice online rapid health is our online patient triage system; rapid health can be used to access self-care information, submit medical and administrative requests, and even register new patients to the surgery. get started with our online booking tool patchs: contact your gp practice online answer 4 simple questions and patchs will get you help quickly. get started with patchs accurx: accurx: contact us online if you need help with a non-urgent medical or admin request, you can now contact us online. submit a new request klinik: contact us online get help from your gp with klinik! you can submit any medical query, including an appointment request. start here systmconnect: contact your gp practice online no need to call or wait in a queue. request an appointment. engage: contact us online with engage consult you can ask the practice a question, complete an online consultation, and get 24/7 nhs self-care advice. start here we’re here to help practices help their patients “our mission is to help practices, help their patients, and we believe passionately in primary care and harnessing it’s collective power“ find out more about us quote / testimonial: it’s our job to make sure that general practices in doncaster are in the best place to succeed, so that everyone in our area can get access to the quality healthcare they need in a way that’s as convenient as possible. as part of this, we provide some additional services on behalf of our member practices – if your practice books you a weekend appointment, or if you visit the health bus, you could be using our services! we’re not here to replace your usual gp. we work behind the scenes with local practices to provide high quality, additional essential care where it’s needed most. we’re here to harness the collective power of primary care and to bring it within easy reach of everyone living in our communities our core goals delivery of services and initiatives at scale on behalf of practices and pcns support resilience and quality improvement within general practice provide corporate support and infrastructure for pcns and practices system leadership and influence on behalf of general practice discover our services at primary care doncaster, we are dedicated to supporting the health and well-being of our community. our comprehensive range of services are designed to enhance primary care delivery, providing tailored solutions for patients and healthcare providers alike. explore our services nhs 111, out of hours and emergencies 111 is the nhs non-emergency number. it’s fast, easy and free. call 111 and speak to a highly trained adviser, supported by healthcare professionals. they will ask you a series of questions to assess your symptoms and immediately direct you to the best medical care for you. nhs 111 is available 24 hours a day, 365 days a year. calls are free from landlines and mobile phones. in case of a life-threatening emergency, please dial 999. nhs app the nhs app allows you to access a range of nhs services. you can download the nhs app on your phone or tablet. you can also access the same services in a web browser by logging in through the nhs website. you must be aged 13 or over to use the nhs app. you also need to be registered with a gp surgery in england or the isle of man. find out more about who can use the nhs app. find out more"""
    
    print("\n--- Single text classification ---")
    scores = classify_text(test_text, model, sector_embeddings)
    for sector, score in sorted(scores.items(), key=lambda x: -x[1]):
        print(f"  {sector}: {score:.3f}")

    # 4. Batch classify from parquet
    # df = pd.read_parquet("../local/firms_to_classify.parquet")
    # result_df = classify_dataframe(df, "site_text", model, sector_embeddings)
    # result_df.to_parquet("../local/firms_classified.parquet")
    # print(f"\nClassified {len(result_df)} firms")
    # print(result_df[["CompanyName", "best_sector", "best_score"]].head(20))
