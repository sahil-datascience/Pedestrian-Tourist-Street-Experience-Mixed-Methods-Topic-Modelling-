

# Load libraries
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

# Join 
review_df <- review_df %>%
        left_join(gangtok_sentiments %>% dplyr::select(doc_id, llm_label), by = "doc_id")

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

#------------------------------------- Ordinal Regression

library(MASS)


# Convert llm_label to ordered factor
review_df$llm_label <- factor(review_df$llm_label, levels = c("Negative", "Neutral", "Positive"), ordered = TRUE)
review_df$Season <- factor(review_df$Season, levels = c("Winter (Dec-Feb)","Spring (March-May)", "Summer (Jun-Aug)", "Autumn (Sep-Nov)"),
                           ordered = FALSE)

# Set Winter as baseline (reference level)
review_df$Season <- relevel(review_df$Season, ref = "Winter (Dec-Feb)")

# Fit ordinal logistic regression model
model <- polr(llm_label ~ Season + Topic_13, data = review_df, Hess = TRUE)

summary(model)

exp(coef(model))


# Suppose you have Topic_1 ... Topic_21
topic_vars <- paste0("Topic_", 1:14)

formula <- as.formula(
        paste("llm_label ~ Season +", paste(topic_vars, collapse = " + "))
)

model_all <- polr(formula, data = review_df, Hess = TRUE)
summary(model_all)

#-------------------------------- Simple Approach ----------------------------#

# Collapse sentiment into Negative vs Non-Negative
review_df$neg_dummy <- ifelse(review_df$llm_label == "Negative", 1, 0)
table(review_df$neg_dummy)

library(dplyr)
library(tidyr)

# Example: average topic proportions by sentiment
topic_vars <- paste0("Topic_", 1:14)

topic_sentiment <- review_df %>%
        group_by(llm_label) %>%
        summarise(across(all_of(topic_vars), mean))

topic_sentiment_long <- topic_sentiment %>%
        pivot_longer(cols = all_of(topic_vars), names_to = "Topic", values_to = "AvgProp")

topic_sentiment_long

library(ggplot2)

ggplot(topic_sentiment_long, aes(x = Topic, y = AvgProp, fill = llm_label)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Topic proportions by Sentiment",
             y = "Average topic proportion") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


prop.table(table(review_df$Season, review_df$llm_label), 1)

ggplot(review_df, aes(x = Season, fill = llm_label)) +
        geom_bar(position = "fill") +
        labs(title = "Sentiment distribution by Season", y = "Proportion") +
        theme_minimal()

prop.table(table(review_df$tourist_origin, review_df$llm_label), 1)
ggplot(review_df, aes(x = tourist_origin, fill = llm_label)) +
        geom_bar(position = "fill") +
        labs(title = "Sentiment distribution by Tourist Origin", y = "Proportion") +
        theme_minimal()


# Logistic regression: Negative vs Non-Negative
topic_vars <- paste0("Topic_", 1:13) # drop Topic_21 or whichever


formula <- as.formula(
        paste("neg_dummy ~ Season + tourist_origin +", paste(topic_vars, collapse = " + "))
)

logit_model <- glm(formula, data = review_df, family = binomial)
summary(logit_model)

# Odds ratios
exp(coef(logit_model))

library(glmnet)

X <- as.matrix(review_df[, topic_vars])
y <- review_df$neg_dummy

cvfit <- cv.glmnet(X, y, family = "binomial", alpha = 1) # LASSO
coef(cvfit, s = "lambda.min")

#---------------------- Topic Proportions by Sentiment ----------------------#
library(dplyr)
library(tidyr)
library(ggplot2)

topic_vars <- paste0("Topic_", 1:14)

# 1. compute mean topic proportion by sentiment
topic_sentiment <- review_df %>%
        dplyr::select(all_of(c("llm_label", topic_vars))) %>%
        pivot_longer(cols = all_of(topic_vars), names_to = "Topic", values_to = "Prop") %>%
        group_by(Topic, llm_label) %>%
        summarise(mean_prop = mean(Prop, na.rm = TRUE), .groups = "drop")

# 2. plot
ggplot(topic_sentiment, aes(x = Topic, y = mean_prop, fill = llm_label)) +
        geom_col(position = "dodge") +
        labs(title = "Mean topic proportion by Sentiment",
             x = "Topic", y = "Mean topic proportion", fill = "Sentiment") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################

# compute mean per sentiment, then difference (Negative - Positive)
diff_df <- topic_sentiment %>%
        pivot_wider(names_from = llm_label, values_from = mean_prop) %>%
        mutate(
                Negative = coalesce(Negative, 0),
                Positive = coalesce(Positive, 0),
                Neutral = coalesce(Neutral, 0),
                diff_neg_pos = Negative - Positive
        ) %>%
        arrange(desc(diff_neg_pos)) %>%
        mutate(Topic = factor(Topic, levels = Topic))

# plot difference sorted
ggplot(diff_df, aes(x = Topic, y = diff_neg_pos)) +
        geom_col() +
        labs(title = "Difference in mean topic proportion (Negative - Positive)",
             y = "Negative - Positive (mean proportion)", x = "Topic") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


###############################

threshold <- 0.15

topic_presence <- review_df %>%
        pivot_longer(cols = all_of(topic_vars), names_to = "Topic", values_to = "Prop") %>%
        mutate(present = ifelse(Prop > threshold, 1, 0)) %>%
        group_by(Topic, llm_label) %>%
        summarise(prevalence = mean(present, na.rm = TRUE), .groups = "drop")

ggplot(topic_presence, aes(x = Topic, y = prevalence, fill = llm_label)) +
        geom_col(position = "dodge") +
        labs(title = paste0("Proportion of docs with Topic proportion >", threshold),
             x = "Topic", y = "Prevalence (fraction of docs)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

        