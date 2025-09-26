

#---------------------------------- Qualitative Exploration ----------------------------------#

library(tidyverse)

review_df <- read_csv("Data/tidy/shimla_reviews_3.csv")

stm_model_21 <- readRDS("Data/r_data/stm_model_k21.rds")

## Create CSV of topics, keywords, representative docs
# source the function file
source("3. Qualitative Exploration/Functions/stm_context_extractor.R")


# K 21
create_csv <- stm_context_extractor(stm_model_21, review_df,
                                  n_topics = 25, n_keywords = 15,
                                  n_docs = 10, csv_out = "3. Qualitative Exploration/Data/stm_21.csv")
## Create HTML of topics, keywords, representative docs
source("3. Qualitative Exploration/Functions/Context_in_HTML.R")

context_in_HTML(
        csv_in  = "3. Qualitative Exploration/Data/stm_21.csv",
        html_out = "3. Qualitative Exploration/Data/STM_Topics_K21.html"
        
)
#-------------------------------------------------------------------------------------------------#

