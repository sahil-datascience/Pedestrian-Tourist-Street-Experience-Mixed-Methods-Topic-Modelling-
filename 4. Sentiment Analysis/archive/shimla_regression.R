# packages
library(tidyverse)
library(lubridate)
library(stm)
library(broom)        # tidy regression output
library(sandwich)     # robust se
library(lmtest)       # coeftest
library(MASS)         # polr (ordered logit) if you use it
# install.packages if missing


#--------------------------------------------- Preparation & Checks ---------------------------------------------#
# Load metadata + check sentiment variable
review_df <- read_csv("Data/tidy/shimla_reviews_3.csv") %>%
        mutate(Date = as_date(Date),
               year = lubridate::year(Date),
               # tourist origin: adjust column name for country variable if different
               tourist_origin = case_when(
                       Countries == "India" ~ "Domestic",
                       is.na(Countries) ~ "Unknown",
                       TRUE ~ "International"
               ),
               tourist_origin = factor(tourist_origin, levels = c("Domestic", "International", "Unknown")),
               Season = factor(Season, levels = c("Winter (Dec-Feb)", "Spring (March-May)",
                                                  "Summer (Jun-Aug)", "Autumn (Sep-Nov)")))

# load sentiment file (your gpt_oss20b output)
shimla_sentiments <- read_csv("4. Sentiment Analysis/Data/final_sentiments/shimla_gpt_oss20b.csv") %>%
        mutate(sentiment_score = case_when(
                llm_label == "Positive" ~ 1L,
                llm_label == "Neutral"  ~ 0L,
                llm_label == "Negative" ~ -1L,
                TRUE ~ NA_integer_
        ))

# join
review_df <- review_df %>%
        left_join(shimla_sentiments %>% dplyr::select(doc_id, sentiment_score), by = "doc_id")

# quick checks
table(review_df$sentiment_score, useNA = "ifany")
table(review_df$tourist_origin, useNA = "ifany")
summary(review_df$Season)


#--------------------------------------------- Regression Analysis ---------------------------------------------#

# estimateEffect() -topic prevalence as function of sentiment + covariates

# load your fitted stm model (you already have stm_model_21)
# stm_model_21 <- stm(...)
dfm <- readRDS("Data/r_data/shimla_dfm.rds")
# Convert dfm to stm format
out <- convert(dfm, to = "stm")
# Fit the model
set.seed(12345)

stm_model_21 <- stm(
        documents = out$documents,
        vocab = out$vocab,
        data = review_df,
        K = 21)

# Ensure metadata in stm_model_21$meta matches review_df (same order)
# stm() was called with data=review_df earlier so they should match.
identical(nrow(review_df), nrow(stm_model_21$theta))  # should be TRUE

# Convert sentiment to factor (useful for estimateEffect)
review_df <- review_df %>%
        mutate(sentiment_fac = factor(sentiment_score,
                                      levels = c(-1, 0, 1),
                                      labels = c("Negative", "Neutral", "Positive")))

# Estimate effects for all topics: model topic proportions (1:K) as function of sentiment, season, tourist_origin
prep <- estimateEffect(1:21 ~ sentiment_fac + Season + tourist_origin,
                       stmobj = stm_model_21,
                       metadata = review_df,
                       uncertainty = "Global")  # "Global" or "Local" gives different uncertainty treatment

# Inspect results for a few topics
summary(prep, topics = c(1,5,10))  # replace with topic IDs of interest

# Plot the estimated effect of sentiment on a topic (example topic 13)
# Example: plot effect of Negative vs Positive sentiment on Topic 13
plot(prep, 
     covariate = "sentiment_fac", 
     method = "difference",
     cov.value1 = "Negative",    # baseline
     cov.value2 = "Positive",    # comparison
     topics = 13,
     xlab = "Estimated difference in topic prevalence\n(Negative - Positive)",
     main = "Topic 13: Prevalence difference (Negative vs Positive)",
     labeltype = "custom",
     custom.labels = "Tourist Pressure & Overcrowding")  # optional nicer label

# Plot multiple topics in a grid (useful to create a small multiples of topics of interest)
topics_to_plot <- c(3, 9, 13, 18)  # replace with IDs of interest
par(mfrow = c(2,2))                # 2x2 grid of plots
for (t in topics_to_plot) {
        plot(prep, 
             covariate = "sentiment_fac",
             method = "difference",
             cov.value1 = "Negative",
             cov.value2 = "Positive",
             topics = t,
             xlab = "Difference in topic prevalence (Neg - Pos)",
             main = paste("Topic", t))
}
par(mfrow = c(1,1))


# Regression on topic proportions (alternative approach)
# B1 — Linear regression (treat sentiment as numeric -1..1)
# Extract theta (topic proportions)
theta <- stm_model_21$theta   # matrix: N_docs x K
colnames(theta) <- paste0("T", 1:ncol(theta))

# Bind to metadata
theta_df <- as_tibble(theta) %>%
        mutate(doc_id = review_df$doc_id) %>%
        bind_cols(review_df %>% dplyr::select(doc_id, sentiment_score, Season, tourist_origin))

# Fit linear model: sentiment_score ~ topic proportions + Season + tourist_origin
lm_form <- as.formula(paste0("sentiment_score ~ ", paste(colnames(theta), collapse = " + "),
                             " + Season + tourist_origin"))

lm_fit <- lm(lm_form, data = theta_df)

# Robust standard errors:
coeftest(lm_fit, vcov = vcovHC(lm_fit, type = "HC1"))

# Tidy output
library(broom)
tidy_lm <- tidy(lm_fit, conf.int = TRUE)
head(tidy_lm, 30)


#