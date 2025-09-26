
#----------------------------- Gangtok (Sikkim) Mixed Methods Analysis -----------------------------#

# Sentiments
gangtok_sentiments <- read_csv("4. Sentiment Analysis/Data/final_sentiments/gangtok_gpt_oss20b.csv")
dim(gangtok_sentiments)

# Convert Sentiments into Numeric
gangtok_sentiments <- gangtok_sentiments %>%
        mutate(sentiment_score = case_when(
                llm_label == "Negative" ~ -1,
                llm_label == "Neutral" ~ 0,
                llm_label == "Positive" ~ 1
        ))

# read metadata
review_df <- read_csv("Data/tidy/gangtok_reviews_3.csv")

# Join sentiments with review_df
review_df <- review_df %>%
        left_join(gangtok_sentiments %>% 
                          select(doc_id, sentiment_score), by = "doc_id")

# -------------- Summary -----------------#
# Total Negative
table(review_df$sentiment_score)

#out
out <- readRDS("Data/MG Road Gangtok/gangtok_stm_out.rds")

set.seed(12345)

stm_model_14 <- stm(
        documents = out$documents,
        vocab = out$vocab,
        data = review_df,
        K = 14
)
plot(stm_model_14)



# Theta Matrix
theta <- as.data.frame(stm_model_14$theta)
dim(theta)

colnames(theta)

# Add topic names
colnames(theta) <- paste0("Topic", 1:14)
# Combine with doc_id so we can join
theta$doc_id <- review_df$doc_id

# Merge sentiment + topics
reg_df <- review_df %>%
        select(doc_id, sentiment_score) %>%
        inner_join(theta, by = "doc_id")

# Regression
lm_model <- lm(sentiment_score ~ ., 
               data = reg_df %>% select(sentiment_score, starts_with("Topic")))
summary(lm_model)

# Visualize Coefficients
library(broom)
library(ggplot2)

coef_df <- broom::tidy(lm_model)

coef_df %>%
        filter(term != "(Intercept)") %>%
        ggplot(aes(x = reorder(term, estimate), y = estimate)) +
        geom_col(fill = "steelblue") +
        geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
        coord_flip() +
        labs(x = "Topics", y = "Effect on Sentiment (beta)",
             title = "Topic effects on sentiment")
# Plot 2
coef_df <- broom::tidy(lm_model)
coef_df %>%
        filter(term != "(Intercept)") %>%
        mutate(significant = p.value < 0.05) %>%
        ggplot(aes(x = reorder(term, estimate), y = estimate, fill = significant)) +
        geom_col() +
        coord_flip() +
        labs(x = "Topics", y = "Effect on Sentiment", 
             title = "Topic–Sentiment Regression Results")


#--------------- Approach 2 non-regression ------------------#
overall_mean <- mean(review_df$sentiment_score, na.rm = TRUE) # We have to do this otherwise all topics positive

topic_sentiment <- theta %>%
        mutate(sentiment_score = review_df$sentiment_score) %>%
        pivot_longer(cols = starts_with("Topic"), names_to = "topic", values_to = "gamma") %>%
        group_by(topic) %>%
        summarise(weighted_sentiment = sum(gamma * (sentiment_score - overall_mean)) / sum(gamma),
                  .groups = "drop")


topic_sentiment

# Visualize
ggplot(topic_sentiment, aes(x = reorder(topic, weighted_sentiment), 
                            y = weighted_sentiment)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(x = "Topic", y = "Average Sentiment",
             title = "Topic positions on sentiment scale")



# Find top 5 positive and negative documents for topic 3
jumbo_df <- review_df %>%
        select(doc_id, Date, Season, Source, Countries, Cities, raw_text) %>% #join reg_df
        left_join(reg_df, by = "doc_id")


neg_docs <- jumbo_df %>%
        filter(sentiment_score == -1) %>%
        arrange(desc(Topic3)) %>%
        slice_head(n = 5) %>%
        select(doc_id, Date, Season, Source, Countries, Cities, Topic13, sentiment_score, raw_text)
neg_docs
view(neg_docs)

pos_docs <- jumbo_df %>%
        filter(sentiment_score == 1) %>%
        arrange(desc(Topic13)) %>%
        slice_head(n = 5) %>%
        select(doc_id, Date, Season, Source, Countries, Cities, Topic13, sentiment_score, raw_text)
pos_docs
view(pos_docs)

