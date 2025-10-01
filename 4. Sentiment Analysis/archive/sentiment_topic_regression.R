

# Load libraries
library(tidyverse)
library(stm)



#---------------------------------------------------- Shimla ----------------------------------------------------#
# ## Load Data

# read metadata
review_df <- read_csv("Data/tidy/shimla_reviews_3.csv")


# Quick Pre-Processing

# Add Visitor Type for Meta Data
review_df <- review_df %>% 
        mutate(tourist_origin = ifelse(Countries == "India", "Domestic", "International"),
               year = lubridate::year(Date))
# Missing as "Unknown"
review_df$tourist_origin <- ifelse(is.na(review_df$tourist_origin), "Unknown", review_df$tourist_origin)

review_df

(table(review_df$tourist_origin) / 3652) * 100




#------------------------------------- Sentiment Data
folder_path <- "4. Sentiment Analysis/Data/final_sentiments"

# Read Data
shimla_sentiments <- read_csv(file.path(folder_path, "shimla_gpt_oss20b.csv")) %>%
        mutate(source = "shimla")

# Sentiment into numeric score
shimla_sentiments <- shimla_sentiments %>%
        mutate(sentiment_score = case_when(
                llm_label == "Positive" ~ 1,
                llm_label == "Neutral" ~ 0,
                llm_label == "Negative" ~ -1,
                TRUE ~ NA_real_
        ))
shimla_sentiments
# Join 
review_df <- review_df %>%
        left_join(shimla_sentiments %>% dplyr::select(doc_id, sentiment_score), by = "doc_id")

table(review_df$sentiment_score, useNA = "ifany")

# Make sure season is Factor


#------------------------------------ Start Topic Modeling
#out
# read document feature matrix
dfm <- readRDS("Data/r_data/shimla_dfm.rds")
# Convert dfm to stm format
out <- convert(dfm, to = "stm")
# Fit the model
set.seed(12345)
names(review_df)


stm_model_21 <- stm(
        documents = out$documents,
        vocab = out$vocab,
        data = review_df,
        K = 21)
#plot(stm_model_21, main = "STM Model for Shimla (K 21)")


#----------------- Sentiment Analysis
prep <- estimateEffect(1:21 ~ sentiment_score * Season + tourist_origin, 
                       stm_model_21, meta = review_df, uncertainty = "Global")
prep


summary(prep, topics = c(1,5,12))



# plot topic 5 predicted values across sentiment (-1,0,1) for each season
plot(prep, covariate = "sentiment_score", 
     topics = 5, 
     model = stm_model_21,
     cov.value1 = c(-1, 0, 1), # numeric x-values
     method = "pointestimate",
     xlab = "Sentiment (neg  -1 ; neu 0 ; pos +1)",
     main = "Topic 5: Predicted prevalence by sentiment & season")

#-------------------------------------------------------------------------------

library(dplyr)
library(tibble)
library(ggplot2)
library(purrr)

# --- 1. Extract tidy coef tables for each topic safely ---
coef_list <- map(1:21, function(k) {
        s <- summary(prep, topics = k)        # summary for topic k
        tab <- as.data.frame(s$tables[[1]])   # the coefficients table (as data.frame)
        # move rownames (terms) into a column called "term"
        tab <- rownames_to_column(tab, var = "term")
        tab$topic <- paste0("Topic_", k)
        tab
})

coef_df <- bind_rows(coef_list)

# Optional: inspect column names to know how Estimate and SE are named
# print(names(coef_df))

# --- 2. Find the correct column names for Estimate and Std.Error robustly ---
est_col <- names(coef_df)[ grepl("^Estimate$", names(coef_df)) |
                                   grepl("Estimate", names(coef_df), ignore.case = TRUE) ][1]
se_col  <- names(coef_df)[ grepl("Std", names(coef_df), ignore.case = TRUE) ][1]

if(is.na(est_col) | is.na(se_col)) stop("Could not find Estimate/Std.Error columns — run `names(coef_df)` to inspect.")

# --- 3. Filter for sentiment_score rows and compute 95% CIs ---
sentiment_coefs <- coef_df %>%
        filter(term == "sentiment_score") %>%
        transmute(
                topic,
                Estimate = .data[[est_col]],
                StdError = .data[[se_col]],
                lower = Estimate - 1.96 * StdError,
                upper = Estimate + 1.96 * StdError,
                p.value = if("Pr...t.." %in% names(coef_df)) .data[["Pr...t.."]] else NA_real_  # optional, may have different name
        )

# If p-value column name differs, inspect names(coef_df) and adjust above.

# Quick check
print(sentiment_coefs)

# --- 4. Plot the coefficients with CIs ---
ggplot(sentiment_coefs, aes(x = Estimate, y = reorder(topic, Estimate))) +
        geom_point(size = 2) +
        geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        labs(
                title = "Sentiment effect on topic prevalence (Shimla, 21 topics)",
                x = "Coefficient for sentiment_score (−1 = Negative; 0 = Neutral; +1 = Positive)",
                y = ""
        ) +
        theme_minimal(base_size = 12)


#------------------------------- Season-specific slopes

library(dplyr)
library(purrr)
library(broom)

# Extract coefficients for each topic
coef_list <- lapply(1:21, function(k) {
        s <- summary(prep, topics = k)
        df <- as.data.frame(s$tables[[1]])
        df$term <- rownames(df)
        df$topic <- paste0("Topic_", k)
        df
})
coef_df <- bind_rows(coef_list)

# Compute per-season slopes
slopes <- coef_df %>%
        filter(grepl("sentiment_score", term)) %>%
        dplyr::select(topic, term, Estimate) %>%
        pivot_wider(names_from = term, values_from = Estimate, values_fill = 0) %>%
        mutate(
                slope_Autumn = sentiment_score,
                slope_Spring = sentiment_score + `sentiment_score:SeasonSpring (March-May)`,
                slope_Summer = sentiment_score + `sentiment_score:SeasonSummer (Jun-Aug)`,
                slope_Winter = sentiment_score + `sentiment_score:SeasonWinter (Dec-Feb)`
        ) %>%
        dplyr::select(topic, starts_with("slope_")) %>%
        pivot_longer(-topic, names_to = "season", values_to = "slope")

# Plot across seasons
ggplot(slopes, aes(x = slope, y = reorder(topic, slope), color = season)) +
        geom_point(position = position_dodge(width = 0.5)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        labs(
                title = "Season-specific sentiment slopes across topics",
                x = "Slope of sentiment effect (negative = more negative reviews)",
                y = "Topic"
        ) +
        theme_minimal()
