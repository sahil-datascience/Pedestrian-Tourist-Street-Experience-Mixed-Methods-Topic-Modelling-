


#----------------------------------- Qualitative Exploration -----------------------------------#
library(tidyverse)

review_df <- read_csv("Data/tidy/gangtok_reviews_3.csv")

stm_model_14 <- readRDS("Data/r_data/stm_model_k14.rds")

### Explore

## Create CSV of topics, keywords, representative docs
# source the function file
source("3. Qualitative Exploration/Functions/stm_context_extractor.R")

# Change raw_text to Main_Text in review_df
review_df <- rename(review_df, Main_Text = raw_text)  

# # call it

# STM 14
create_csv <- stm_context_extractor(stm_model_14, review_df,
                                  n_topics = 14, n_keywords = 15,
                                  n_docs = 10,
                                  csv_out = "3. Qualitative Exploration/Data/stm_14.csv")

## Create HTML of topics, keywords, representative docs
source("3. Qualitative Exploration/Functions/Context_in_HTML.R")

context_in_HTML(
        csv_in  = "3. Qualitative Exploration/Data/stm_14.csv",
        html_out = "3. Qualitative Exploration/Data/STM_Topics_K14.html"
)


#-----------------------------------------------------------------------------------------#



