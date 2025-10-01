library(tidyverse)
library(stm)


#---------------------------------------------- Shimla ------------------------------------------------#
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

(table(review_df$tourist_origin) / 1360)*100


#------------------------------------- Sentiment Data
folder_path <- "4. Sentiment Analysis/Data/final_sentiments"

# Read Data
gangtok_sentiments <- read_csv(file.path(folder_path, "shimla_gpt_oss20b.csv")) %>%
        mutate(source = "shimla")

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

# Topic Proportions
theta_matrix <- as_tibble(stm_model_21$theta)

colnames(theta_matrix) <- paste0("Topic_", 1:ncol(theta_matrix))

# Combine with review_df
review_df <- bind_cols(review_df, theta_matrix)

# Keep only relevant columns
review_df <- review_df %>% dplyr::select(
        -Date,-Cities,-Countries, -pos_filtered_text, -dfm_text,
        -token_filter_text
        
)

# Add themes
library(readxl)
themes <- read_excel("3. Qualitative Exploration/Codebook-Abstraction-Notes/topic-theme-mapping.xlsx")

# Remove . in TopicID
themes$TopicID <- gsub("\\.", "", themes$TopicID)
themes$TopicID <- str_trim(themes$TopicID)

names(review_df)
head(review_df)

# save
saveRDS(review_df, "Data/r_data/shimla_review_topic_sentiments.rds")

#------------------------------------------------- Topic Sentiments

# Filter Negative
neg_reviews <- review_df %>% filter(llm_label == "Negative")
dim(neg_reviews)        

# Sum topic proportions
neg_topic_sums <- colSums(neg_reviews %>% select(starts_with("Topic_")))
neg_topic_sums <- as_tibble(neg_topic_sums) %>%
        rename(topic = value) %>%
        mutate(TopicID = row_number(),
               TopicID = as.character(TopicID))
neg_topic_sums

# Filter negative reviews
neg_df <- review_df %>% filter(llm_label == "Negative")

# Calculate mean topic proportion across negative reviews
topic_means_neg <- neg_df %>% 
        select(starts_with("Topic_")) %>% 
        summarise(across(everything(), mean))

# Percentage
topic_means_neg_perc <- topic_means_neg %>%
        pivot_longer(everything(), names_to = "Topic", values_to = "Mean_Proportion") %>%
        mutate(Mean_Proportion = Mean_Proportion * 100)

# Plot
library(ggplot2)
library(ggplot2)

ggplot(topic_means_neg_perc, 
       aes(x = reorder(Topic, Mean_Proportion), 
           y = Mean_Proportion)) +
        geom_col(fill = "red") +
        geom_text(aes(label = sprintf("%.1f%%", Mean_Proportion)),   # format as percentages
                  hjust = 0.5, size = 3.5, vjust = -1) +                       # nudge right for visibility
        labs(title = "Mean Topic Proportions in Negative Reviews",
             x = "Topic",
             y = "Mean Proportion (%)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +              # center title
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))     # add space for labels

                                             
#---------------------------------------------------- Combined
library(tidyverse)
library(ggplot2)

# 1) Prepare data: means by sentiment (as percentage)
topic_means_by_sentiment <- review_df %>%
        select(llm_label, starts_with("Topic_")) %>%
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%   # keep only these 3
        group_by(llm_label) %>%
        summarise(across(starts_with("Topic_"), mean, .names = "mean_{col}")) %>%
        ungroup() %>%
        pivot_longer(
                cols = starts_with("mean_Topic_"),
                names_to = "Topic",
                values_to = "Mean_Proportion"
        ) %>%
        mutate(
                Topic = str_replace(Topic, "^mean_", ""),     # tidy name: Topic_1 ...
                Mean_Proportion = Mean_Proportion * 100      # convert to percent
        )

# 2) compute ordering based on Negative mean (so bars are sorted by what matters for Negative)
topic_order <- topic_means_by_sentiment %>%
        filter(llm_label == "Negative") %>%
        arrange(desc(Mean_Proportion)) %>%
        pull(Topic)

topic_means_by_sentiment <- topic_means_by_sentiment %>%
        mutate(Topic = factor(Topic, levels = topic_order))

# 3) Plot: horizontal grouped bars with percent labels
ggplot(topic_means_by_sentiment, 
       aes(x = Topic, y = Mean_Proportion, fill = llm_label)) +
        geom_col(position = position_dodge(width = 0.9), width = 0.8) +
        # labels: move labels slightly outside the bars so they remain readable
        geom_text(aes(label = sprintf("%.1f%%", Mean_Proportion)),
                  position = position_dodge(width = 0.9),
                  hjust = 0.5,    # nudges labels to the right (works with coord_flip)
                  size = 3,
                  vjust = -1) +
        labs(title = "Mean Topic Proportions by Sentiment (Negative / Neutral / Positive)",
             x = "Topic",
             y = "Mean Proportion (%)",
             fill = "Sentiment") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.y = element_text(size = 10)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))   # provide extra space for labels


#----------------------------- Top 5 Topics in Each Category

library(tidyverse)
library(tidytext)   # for reorder_within + scale_x_reordered

# 1. Compute mean proportions by sentiment (percent)
topic_means_by_sentiment <- review_df %>%
        select(llm_label, starts_with("Topic_")) %>%
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%
        group_by(llm_label) %>%
        summarise(across(starts_with("Topic_"), mean)) %>%
        ungroup() %>%
        pivot_longer(cols = starts_with("Topic_"),
                     names_to = "Topic",
                     values_to = "Mean_Proportion") %>%
        mutate(Mean_Proportion = Mean_Proportion * 100)

# 2. Select top 5 topics per sentiment
top5_per_sent <- topic_means_by_sentiment %>%
        group_by(llm_label) %>%
        slice_max(Mean_Proportion, n = 5, with_ties = FALSE) %>%
        ungroup()

# 3. Reorder within each facet (tidytext helper)
top5_per_sent <- top5_per_sent %>%
        mutate(Topic_ordered = reorder_within(Topic, Mean_Proportion, llm_label))

top5_per_sent <- top5_per_sent %>%
        mutate(Topic_ordered = fct_drop(Topic_ordered))


# 4. Plot: faceted horizontal bars (each facet has its own y-order)
ggplot(top5_per_sent, aes(x = Topic_ordered, y = Mean_Proportion, fill = llm_label)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.1f%%", Mean_Proportion)),
                  hjust = -0.05, size = 3) +
        facet_wrap(~ llm_label, scales = "free_y", ncol = 1) +
        scale_x_reordered() +
        labs(title = "Top 5 Topics by Sentiment (Mean Proportion %)",
             x = "Topic",
             y = "Mean Proportion (%)") +
        theme_minimal() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

##############################################################################

# Sentiment Distribution in Seasons
season_sentiment <- review_df %>%
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%
        group_by(Season, llm_label) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(Season) %>%
        mutate(total = sum(count),
               proportion = (count / total) * 100) %>%
        ungroup()
season_sentiment
# Facet Plot
ggplot(season_sentiment, aes(x = Season, y = proportion, fill = llm_label)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = sprintf("%.1f%%", proportion)),
                  position = position_dodge(width = 0.9),
                  vjust = -0.5, size = 3) +
        labs(title = "Sentiment Distribution by Season",
             x = "Season",
             y = "Proportion (%)",
             fill = "Sentiment") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))


# Sentiment Distribution by Tourist Origin
origin_sentiment <- review_df %>%
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%
        group_by(tourist_origin, llm_label) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(tourist_origin) %>%
        mutate(total = sum(count),
               proportion = (count / total) * 100) %>%
        ungroup()
origin_sentiment                                             
# Facet Plot
ggplot(origin_sentiment, aes(x = tourist_origin, y = proportion, fill = llm_label)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = sprintf("%.1f%%", proportion)),
                  position = position_dodge(width = 0.9),
                  vjust = -0.5, size = 3) +
        labs(title = "Sentiment Distribution by Tourist Origin",
             x = "Tourist Origin",
             y = "Proportion (%)",
             fill = "Sentiment") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))


# Sentiment Distribution by Year
year_sentiment <- review_df %>%
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%
        group_by(year, llm_label) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(total = sum(count),
               proportion = (count / total) * 100) %>%
        ungroup()
year_sentiment
# Line Chart
ggplot(year_sentiment, aes(x = year, y = proportion, color = llm_label)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        geom_text(aes(label = sprintf("%.1f%%", proportion)),
                  vjust = -0.5, size = 3) +
        labs(title = "Sentiment Distribution by Year",
             x = "Year",
             y = "Proportion (%)",
             color = "Sentiment") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))


# Sentiment Distribution by Themes
# Add themes
library(readxl)
themes <- read_excel("3. Qualitative Exploration/Codebook-Abstraction-Notes/topic-theme-mapping.xlsx")

# Remove . in TopicID
themes$TopicID <- gsub("\\.", "", themes$TopicID)
themes$TopicID <- str_trim(themes$TopicID)

themes <- themes %>% mutate(Topic = paste0("Topic_", TopicID))

# Keep Shimla Theme Only
themes <- themes %>% filter(Site == "Shimla")

theme_sentiment <- review_df %>%
        pivot_longer(cols = starts_with("Topic_"),
                     names_to = "Topic",
                     values_to = "Proportion") %>%
        left_join(themes, by = c("Topic" = "Topic")) %>%
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%
        group_by(CommonTheme, llm_label) %>%
        summarise(mean_proportion = mean(Proportion) * 100) %>%
        ungroup()
theme_sentiment
# Facet Plot
ggplot(theme_sentiment, aes(x = CommonTheme, y = mean_proportion, fill = llm_label)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = sprintf("%.1f%%", mean_proportion)),
                  position = position_dodge(width = 0.9),
                  vjust = -0.5, size = 3) +
        labs(title = "Mean Topic Proportions by Common Theme and Sentiment",
             x = "Common Theme",
             y = "Mean Proportion (%)",
             fill = "Sentiment") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
