

library(tidyverse)
library(stm)


#---------------------------------------------- Gangtok ------------------------------------------------#
# ## Load Data

# read metadata
review_df <- read_csv("Data/tidy/gangtok_reviews_3.csv")


# Quick Pre-Processing

# Add Visitor Type for Meta Data
review_df <- review_df %>% 
        mutate(tourist_origin = ifelse(Countries == "India", "Domestic", "International"),
               year = lubridate::year(Date))
# Missing as "Unknown"
review_df$tourist_origin <- ifelse(is.na(review_df$tourist_origin), "Unknown", review_df$tourist_origin)

review_df

(table(review_df$tourist_origin) / 1360)*100


#------------------------------------- Sentiment Data
folder_path <- "4. Sentiment Analysis/Data/final_sentiments"

# Read Data
gangtok_sentiments <- read_csv(file.path(folder_path, "gangtok_gpt_oss20b.csv")) %>%
        mutate(source = "gangtok")

# mutate sentiment_score
gangtok_sentiments <- gangtok_sentiments %>%
        mutate(sentiment_score = case_when(
                llm_label == "Positive" ~ 1,
                llm_label == "Negative" ~ -1,
                llm_label == "Neutral" ~ 0,
                TRUE ~ NA_real_
        ))

# Join 
review_df <- review_df %>%
        left_join(gangtok_sentiments %>% dplyr::select(doc_id, llm_label, sentiment_score), by = "doc_id")

#table(review_df$llm_label)
#table(review_df$Season)
#table(review_df$tourist_origin)

#------------------------------------ Start Topic Modeling
#out
# read document feature matrix
dfm <- readRDS("Data/r_data/gangtok_dfm.rds")
# Convert dfm to stm format
out <- convert(dfm, to = "stm")
# Fit the model
set.seed(12345)
names(review_df)


stm_model_14 <- stm(
        documents = out$documents,
        vocab = out$vocab,
        data = review_df,
        K = 14)

# Topic Proportions
theta_matrix <- as_tibble(stm_model_14$theta)

colnames(theta_matrix) <- paste0("Topic_", 1:ncol(theta_matrix))

# Combine with review_df
review_df <- bind_cols(review_df, theta_matrix)

# Keep only relevant columns
review_df <- review_df %>% dplyr::select(
        -Date,-Cities,-Countries, -pos_filtered_text, -dfm_text,
        -token_filter_text
        
)

review_df

# save rds
saveRDS(review_df, "Data/r_data/gangtok_topic_sentiment_data.rds")

#------------------------------------ Topic Sentiments




