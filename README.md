# Decoding the Pedestrian Tourist Street Experience (PTSE)

**An Open-Source Text Analytics Framework Systematically Fortifying Computational Scale with Qualitative Depth**

---

## 📄 About This Repository

This repository accompanies the research study:

> Sharma, S., Khan, S., & Chauhan, R. (2026). Decoding the pedestrian tourist street experience (PTSE): An open-source text analytics framework systematically fortifying computational scale with qualitative depth. _Tourism Recreation Research_. https://doi.org/10.1080/02508281.2026.2637475


### Summary
This repository provides the full codebase and workflow for the PTSE framework, which leverages large-scale user-generated reviews to analyze pedestrian tourist street experiences.

**Key Contribution:**

> The PTSE framework systematically strengthens quantitative topic modelling results with qualitative depth. It achieves this by:
> - Extracting relevant data and representative documents from topic models
> - Providing custom scripts and interactive HTML programs for systematic, rigorous qualitative coding and interpretation
> - Enabling researchers to move beyond surface-level topic labels and generate context-rich, nuanced insights

The framework integrates:
- **Structural Topic Modelling (STM)**
- **Qualitative analysis (with custom coding tools)**
- **LLM-based sentiment analysis**

to generate context-rich insights into tourist experiences, mobility, and place attachment.

---


## 🗂️ Repository Structure & Key Scripts

The analysis pipeline is organized into the following main folders and scripts:

### 1. Data Pre-Processing (`1. Data Pre-Processing/`)
- `Gangtok/` and `Shimla/` subfolders contain:
  - `1_Import_and_temporal_(...).R`: Import and temporal feature engineering
  - `2_spatial_data_cleaning_(...).R`: Spatial data cleaning and location extraction
  - `3_text_features_(...).R`: Text pre-processing and feature extraction

### 2. Quantitative Modelling (`2. Quantitative Modelling/`)
- `Gangtok_topic_modelling.R`, `Shimla_topic_modelling.R`: Structural Topic Modelling (STM) for each site
- `topic_plots.R`: Visualization of topic model results
- `searchK_Data/`: Data for model selection (searchK)

### 3. Qualitative Exploration (`3. Qualitative Exploration/`)
- `extract_best_quotes.R`: Extracts representative quotes for each topic
- `extract_topDocs_(Gangtok).R`, `extract_topDocs_(Shimla).R`: Extracts top documents and context for qualitative coding
- `Functions/stm_context_extractor.r`: Robust function for extracting topic keywords and representative documents from STM models
- **HTML Program:**
  - `HTML Program/Interactive_Coder_no_ham_memory_fixed.html`: Interactive tool for manual, systematic qualitative coding of topic documents

### 4. Sentiment Analysis (`4. Sentiment Analysis/`)
- `create_sample.r`: Prepares sentiment analysis samples
- `sentiment_analysis_gpt0ss.R`: LLM-based sentiment analysis (OpenAI/Ollama)
- **HTML Programs:**
  - `HTML Program/Interactive_Sentiment_Coder.html`: Manual sentiment coding tool
  - `HTML Program/Interactive_Sentiment_Coder_3labels.html`: Sentiment coding with 3-label support (Positive/Neutral/Negative)

### 5. Mixed Methods (`5. Mixed Methods/`)
- `Gangtok_mixed.R`, `Shimla_mixed.R`: Integrates topic, qualitative, and sentiment results for each site

> **Run scripts step-by-step in the above order. Each stage depends on outputs from the previous stage.**

---

---


## 🧰 Dependencies

- R (recommended version: 4.0+)
- R packages:
   - `quanteda`
   - `stm`
   - `spacyr`
   - `lubridate`
   - `tidyverse`
- Optional/Advanced:
   - OpenAI API (for LLM-based sentiment analysis)
   - **Ollama** (for free, local LLM execution; requires Ollama to be installed and running on your system)

> **Note:** Ollama is a free, open-source tool for running LLMs locally. To use LLM-based scripts with Ollama, install Ollama and ensure it is running. See https://ollama.com/ for installation instructions and supported models.

---

## ▶️ How to Use

1. **Install required R packages** (see above)
2. **Follow the pipeline**:
   - Data collection (web scraping)
   - Data preprocessing
   - Feature engineering
   - Topic modelling (STM)
   - Qualitative interpretation
   - Sentiment analysis (lexicon + LLM)
   - Mixed-methods integration
3. **LLM-based scripts** require API access and may incur cost.
4. **Raw data** may not be fully shared due to platform restrictions (e.g., TripAdvisor policies).

---

## ⚠️ Important Notes

- Results may vary slightly due to the stochastic nature of topic models.
- Preprocessing decisions (e.g., POS filtering) significantly influence results.
- LLM-based scripts require API keys and may incur cost.
- The study relies on publicly available user-generated content (TripAdvisor), processed under ethical guidelines without collecting personally identifiable information.

---

## 📖 Citation

**APA:**
> Sharma, S., Khan, S., & Chauhan, R. (2026). Decoding the pedestrian tourist street experience (PTSE): An open-source text analytics framework systematically fortifying computational scale with qualitative depth. _Tourism Recreation Research_. https://doi.org/10.1080/02508281.2026.2637475

**BibTeX:**
```
@article{sharma2026ptse,
  title={Decoding the pedestrian tourist street experience (PTSE): An open-source text analytics framework systematically fortifying computational scale with qualitative depth},
  author={Sharma, Sahil and Khan, Sonia and Chauhan, Rohit},
  journal={Tourism Recreation Research},
  year={2026},
  doi={10.1080/02508281.2026.2637475}
}
```

---

## 🙏 Acknowledgments

The authors declare no conflict of interest.

The study relies on publicly available user-generated content (TripAdvisor), processed under ethical guidelines without collecting personally identifiable information.

---


---

## 📬 Contact

For questions or collaboration, please contact: **sahilsharmahimalaya@gmail.com**
