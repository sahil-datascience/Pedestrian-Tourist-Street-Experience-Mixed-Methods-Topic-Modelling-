
library(tidyverse)

# Load Data
shimla <- readRDS("Data/r_data/shimla_review_topic_sentiments.rds") %>%
        mutate(site = "Shimla") 
gangtok <- readRDS("Data/r_data/gangtok_topic_sentiment_data.rds") %>%
        mutate(site = "Gangtok")

# Add themes
library(readxl)
themes <- read_excel("3. Qualitative Exploration/Codebook-Abstraction-Notes/topic-theme-mapping.xlsx")

# Remove . in TopicID
themes$TopicID <- gsub("\\.", "", themes$TopicID)
themes$TopicID <- str_trim(themes$TopicID)
themes <- themes %>% mutate(Topic = paste0("Topic_", TopicID))

shimla_themes <- themes %>% filter(Site == "Shimla")
gangtok_themes <- themes %>% filter(Site == "Gangtok")


head(shimla)
head(gangtok)

# Join
all_docs <- bind_rows(shimla, gangtok)


#--------------------------- All Topics and All Sentiments [For Appendix] -------------------------------------#
# 1) Prepare data: means by sentiment (as percentage)
topic_means_by_sentiment_shimla <- shimla %>%
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
topic_means_by_sentiment_gangtok <- gangtok %>%
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
topic_order_shimla <- topic_means_by_sentiment_shimla %>%
        filter(llm_label == "Negative") %>%
        arrange(desc(Mean_Proportion)) %>%
        pull(Topic)
topic_order_gangtok <- topic_means_by_sentiment_gangtok %>%
        filter(llm_label == "Negative") %>%
        arrange(desc(Mean_Proportion)) %>%
        pull(Topic)

topic_means_by_sentiment_shimla <- topic_means_by_sentiment_shimla %>%
        mutate(Topic = factor(Topic, levels = topic_order))
topic_means_by_sentiment_gangtok <- topic_means_by_sentiment_gangtok %>%
        mutate(Topic = factor(Topic, levels = topic_order))

# 3) Plot: horizontal grouped bars with percent labels
shimla_p <- ggplot(topic_means_by_sentiment_shimla, 
       aes(x = Topic, y = Mean_Proportion, fill = llm_label)) +
        geom_col(position = position_dodge(width = 0.9), width = 0.8) +
        # labels: move labels slightly outside the bars so they remain readable
        geom_text(aes(label = sprintf("%.1f%%", Mean_Proportion)),
                  position = position_dodge(width = 0.9),
                  hjust = 0.5,    # nudges labels to the right (works with coord_flip)
                  size = 3,
                  vjust = -1) +
        labs(title = "A: Shimla",
             x = "Topic",
             y = "Mean Proportion (%)",
             fill = "Sentiment") +
        theme_minimal(base_size = 15, base_family = "Times New Roman") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.y = element_text(size = 10)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))   # provide extra space for labels
shimla_p
gangtok_p <- ggplot(topic_means_by_sentiment_gangtok, 
                    aes(x = Topic, y = Mean_Proportion, fill = llm_label)) +
        geom_col(position = position_dodge(width = 0.9), width = 0.8) +
        # labels: move labels slightly outside the bars so they remain readable
        geom_text(aes(label = sprintf("%.1f%%", Mean_Proportion)),
                  position = position_dodge(width = 0.9),
                  hjust = 0.5,    # nudges labels to the right (works with coord_flip)
                  size = 3,
                  vjust = -1) +
        labs(title = "B: Gangtok",
             x = "Topic",
             y = "Mean Proportion (%)",
             fill = "Sentiment") +
        theme_minimal(base_size = 15, base_family = "Times New Roman") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.y = element_text(size = 10)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))   # provide extra space for labels
gangtok_p

# Combine Plots
library(patchwork)
combined_all <- shimla_p / gangtok_p +
        plot_annotation(title = "Mean Topic Proportions by Sentiment (Negative / Neutral / Positive)",
                        theme = theme(plot.title = element_text(size = 18,
                                                                face = "bold",
                                                                hjust = 0.5))) 
combined_all

# save .svg
ggsave("4. Sentiment Analysis/sentiment_distributions_bothSites_allTopics.svg",
       plot = combined_all, width = 20, height = 10, dpi = 300)

#--------------------------- Top 5 in Sentiment -------------------------------------#
# Topic Distribution in Negative Sentiments
shimla_neg <- shimla %>% filter(llm_label == "Negative")
gangtok_neg <- gangtok %>% filter(llm_label == "Negative")

# Count (%) of Topics in Negative Sentiments
shimla_neg_dist <- shimla_neg %>% 
        select(starts_with("Topic_")) %>% 
        summarise(across(everything(), mean)) %>%
        pivot_longer(everything(), names_to = "Topic", values_to = "Mean_Proportion") %>%
        mutate(TopicName = shimla_themes$TopicName[match(Topic, shimla_themes$Topic)]) %>%
        mutate(Mean_Proportion = Mean_Proportion * 100) %>%
        arrange(desc(Mean_Proportion)) %>%
        head(5)
gangtok_neg_dist <- gangtok_neg %>% 
        select(starts_with("Topic_")) %>% 
        summarise(across(everything(), mean)) %>%
        pivot_longer(everything(), names_to = "Topic", values_to = "Mean_Proportion") %>%
        mutate(TopicName = gangtok_themes$TopicName[match(Topic, gangtok_themes$Topic)]) %>%
        mutate(Mean_Proportion = Mean_Proportion * 100) %>%
        arrange(desc(Mean_Proportion)) %>%
        head(5)

# Plot Pathes
neg_plot_shimla <- ggplot(shimla_neg_dist, aes(x = reorder(TopicName, Mean_Proportion), y = Mean_Proportion)) +
        geom_bar(stat = "identity", fill = "tomato") +
        geom_text(aes(label = sprintf("%.1f%%", Mean_Proportion)),
                  vjust = -0.5, hjust = -0.3, size = 3) +
        labs(title = "A: Shimla",
             x = "Topics",
             y = "Mean Proportion (%)") +
        coord_flip() +
        theme_minimal(base_size = 15, base_family = "Times New Roman") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
neg_plot_gangtok <- ggplot(gangtok_neg_dist, aes(x = reorder(TopicName, Mean_Proportion), y = Mean_Proportion)) +
        geom_bar(stat = "identity", fill = "tomato") +
        geom_text(aes(label = sprintf("%.1f%%", Mean_Proportion)),
                  vjust = -0.5, hjust = -0.3, size = 3) +
        labs(title = "B: Gangtok",
             x = "Topics",
             y = "Mean Proportion (%)") +
        coord_flip() +
        theme_minimal(base_size = 15, base_family = "Times New Roman") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Combine Plots
library(patchwork)
combined_neg_plot <- neg_plot_shimla / neg_plot_gangtok  + 
        plot_annotation(title = "Top 5 Topics in Negative Reviews",
                        theme = theme(plot.title = element_text(size = 16,
                                                                face = "bold",
                                                                hjust = 0.5)))

combined_neg_plot



# Save the combined plot
ggsave("4. Sentiment Analysis/sentiment_distributions_bothSites.svg",
       plot = combined_neg_plot, width = 12, height = 8, dpi = 300)

#------------- Quotes

# Add themes
library(readxl)
themes <- read_excel("3. Qualitative Exploration/Codebook-Abstraction-Notes/topic-theme-mapping.xlsx")

# Remove . in TopicID
themes$TopicID <- gsub("\\.", "", themes$TopicID)
themes$TopicID <- str_trim(themes$TopicID)
# Rename Site to source
themes <- themes %>% rename(source = Site)




# Combine
shimla_neg
shimla_neg_dist$Topic

library(purrr)

# Suppose shimla_neg has topic proportions named Topic_1 ... Topic_21
# Let's say these are your top 5 negative topics (replace with your own list)
top5_topics <- c("Topic_13", "Topic_15", "Topic_4", "Topic_8", "Topic_1")

# Function to extract top 5 reviews for a single topic
get_top_reviews <- function(df, topic_col, n = 5) {
        df %>%
                arrange(desc(.data[[topic_col]])) %>%       # sort by topic proportion
                slice_head(n = n) %>%                       # take top n
                select(doc_id, raw_text, !!sym(topic_col)) %>% # keep relevant cols
                mutate(Topic = topic_col)
}

# Apply function to all top topics and combine results
top_reviews <- map_dfr(top5_topics, ~ get_top_reviews(shimla_neg, .x, n = 5))

# Preview results
print(top_reviews)
# Save to JSON
library(jsonlite)
write_json(top_reviews, "4. Sentiment Analysis/Data/quotes/shimla_top5_negative_reviews.json")

gangtok_neg_dist$Topic
top_5_topics_gangtok <- c("Topic_4", "Topic_5", "Topic_12", "Topic_10", "Topic_3")
top_reviews_gangtok <- map_dfr(top_5_topics_gangtok, ~ get_top_reviews(gangtok_neg, .x, n = 5))
print(top_reviews_gangtok)
# Save to JSON
write_json(top_reviews_gangtok, "4. Sentiment Analysis/Data/quotes/gangtok_top5_negative_reviews.json")

#
#
#
#--------------------------- Seasonal Analysis ---------------------------#
#
#
#

shimla_season <- shimla  %>%
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%
        group_by(Season, llm_label) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(Season) %>%
        mutate(total = sum(count),
               proportion = (count / total) * 100) %>%
        ungroup()
gangtok_season <- gangtok  %>% 
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%
        group_by(Season, llm_label) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(Season) %>%
        mutate(total = sum(count),
               proportion = (count / total) * 100) %>%
        ungroup()

# Plot Line Charts
library(ggrepel)
season_plot_shimla <- ggplot(shimla_season, aes(x = Season, y = proportion, group = llm_label, color = llm_label)) +
        geom_line(size = 1) +
        geom_text_repel(aes(label = sprintf("%.1f%%", proportion)),
                        size = 3,
                        show.legend = FALSE,
                        max.overlaps = 20,      # Increase if many labels overlap
                        nudge_y = 0.5,          # Small upward nudge
                        segment.color = "grey50", 
                        segment.size = 0.3) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Negative" = "tomato", "Neutral" = "gray", "Positive" = "steelblue")) +
        labs(title = "A: Shimla",
             x = "Season",
             y = "Proportion (%)",
             color = "Sentiment") +
        theme_minimal(base_size = 15, base_family = "Times New Roman") + # Remove Legend
        theme(legend.position = "none", # tilt x axis text
              axis.text.x = element_text(angle = 45, hjust = 1))
season_plot_gangtok <- ggplot(gangtok_season, aes(x = Season, y = proportion, group = llm_label, color = llm_label)) +
        geom_line(size = 1) +
        geom_text_repel(aes(label = sprintf("%.1f%%", proportion)),
                        size = 3,
                        show.legend = FALSE,
                        max.overlaps = 20,      # Increase if many labels overlap
                        nudge_y = 0.5,          # Small upward nudge
                        segment.color = "grey50", 
                        segment.size = 0.3) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Negative" = "tomato", "Neutral" = "gray", "Positive" = "steelblue")) +
        labs(title = "B: Gangtok",
             x = "Season",
             y = "Proportion (%)",
             color = "Sentiment") +
        theme_minimal(base_size = 15, base_family = "Times New Roman") +
        theme(legend.position = "none", # tilt x axis text
              axis.text.x = element_text(angle = 45, hjust = 1))

# Combine Plots
combined_season_plot <- (season_plot_shimla | season_plot_gangtok) +
        plot_annotation(title = "Seasonal Sentiment Distribution",
                        theme = theme(plot.title = element_text(size = 18,
                                                                face = "bold",
                                                                hjust = 0.5,
                                                                family = "Times New Roman"))) +
        theme_minimal(base_size = 15, base_family = "Times New Roman") +
        theme(legend.position = "left",
              axis.text.x = element_text(angle = 45, hjust = 1))
combined_season_plot

# Save .svg
ggsave("4. Sentiment Analysis/sentiment_distributions_bothSites_season.svg",
       plot = combined_season_plot, width = 8.5, height = 5, dpi = 300)

#----------------------- Quotes

# Safe function: sample up to n rows per Season x sentiment group
extract_quotes_by_season_sentiment <- function(data, n = 5) {
        
        # Ensure the grouping columns exist
        if (!all(c("Season", "llm_label", "doc_id", "Main_Text") %in% names(data))) {
                stop("Data must contain columns: Season, llm_label, doc_id, Main_Text")
        }
        
        # Use group_by + group_map to sample per-group with a constant n
        sampled_list <- data %>%
                group_by(Season, llm_label) %>%
                group_map(
                        ~ {
                                df <- .x
                                k  <- min(nrow(df), n)       # how many we can sample
                                if (k == 0) {
                                        tibble()                   # return empty tibble if group empty
                                } else {
                                        df %>% slice_sample(n = k) %>%
                                                select(Season, llm_label, doc_id, Main_Text)
                                }
                        },
                        .keep = TRUE
                )
        
        # bind results into one dataframe
        result_df <- bind_rows(sampled_list)
        
        # Optional: keep group order consistent
        result_df %>% arrange(Season, llm_label, doc_id)
}


# Example usage:
shimla_season_quotes <- extract_quotes_by_season_sentiment(shimla, n = 5)
gangtok <- gangtok %>% rename(Main_Text = raw_text)  # rename for consistency
gangtok_season_quotes <- extract_quotes_by_season_sentiment(gangtok, n = 5)

# Save to JSON
write_json(shimla_season_quotes, "4. Sentiment Analysis/Data/quotes/shimla_season_quotes.json")
write_json(gangtok_season_quotes, "4. Sentiment Analysis/Data/quotes/gangtok_season_quotes.json")

view(shimla_season_quotes)
view(gangtok_season_quotes)


#---------------------- Tourist Origins ----------------------#
shimla_origin <- shimla  %>%
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%
        group_by(tourist_origin, llm_label) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(tourist_origin) %>%
        mutate(total = sum(count),
               proportion = (count / total) * 100) %>%
        ungroup()
gangtok_origin <- gangtok  %>% 
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%
        group_by(tourist_origin, llm_label) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(tourist_origin) %>%
        mutate(total = sum(count),
               proportion = (count / total) * 100) %>%
        ungroup()
# Plot Line Charts
origin_plot_shimla <- ggplot(shimla_origin, aes(x = tourist_origin, y = proportion, group = llm_label, color = llm_label)) +
        geom_line(size = 1) +
        geom_text(aes(label = sprintf("%.1f%%", proportion)),
                  position = position_dodge(width = 0.9),
                  vjust = -0.5, size = 3) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Negative" = "tomato", "Neutral" = "gray", "Positive" = "steelblue")) +
        labs(title = "Shimla: Tourist Origin Sentiment Distribution",
             x = "Tourist Origin",
             y = "Proportion (%)",
             color = "Sentiment") +
        theme_minimal()
origin_plot_gangtok <- ggplot(gangtok_origin, aes(x = tourist_origin, y = proportion, group = llm_label, color = llm_label)) +
        geom_line(size = 1) +
        geom_text(aes(label = sprintf("%.1f%%", proportion)),
                  position = position_dodge(width = 0.9),
                  vjust = -0.5, size = 3) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Negative" = "tomato", "Neutral" = "gray", "Positive" = "steelblue")) +
        labs(title = "Gangtok: Tourist Origin Sentiment Distribution",
             x = "Tourist Origin",
             y = "Proportion (%)",
             color = "Sentiment") +
        theme_minimal()
# Combine Plots
combined_origin_plot <- origin_plot_shimla | origin_plot_gangtok
combined_origin_plot

# Save
ggsave("4. Sentiment Analysis/sentiment_distributions_bothSites_origin.svg",
       plot = combined_origin_plot, width = 8.5, height = 5, dpi = 300)



#------------------------------ Yearly Analysis ------------------------------#
shimla_date <- shimla  %>%
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%
        group_by(year, llm_label) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(total = sum(count),
               proportion = (count / total) * 100) %>%
        ungroup()
gangtok_date <- gangtok  %>% 
        filter(llm_label %in% c("Negative", "Neutral", "Positive")) %>%
        group_by(year, llm_label) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(total = sum(count),
               proportion = (count / total) * 100) %>%
        ungroup()
# Plot Line Charts
date_plot_shimla <- ggplot(shimla_date, aes(x = year, y = proportion, group = llm_label, color = llm_label)) +
        geom_line(size = 1) +
        geom_text(aes(label = sprintf("%.1f%%", proportion)),
                  position = position_dodge(width = 0.9),
                  vjust = -0.5, size = 3) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Negative" = "tomato", "Neutral" = "gray", "Positive" = "steelblue")) +
        labs(title = "Shimla: Yearly Sentiment Distribution",
             x = "Year",
             y = "Proportion (%)",
             color = "Sentiment") +
        theme_minimal()
date_plot_gangtok <- ggplot(gangtok_date, aes(x = year, y = proportion, group = llm_label, color = llm_label)) +
        geom_line(size = 1) +
        geom_text(aes(label = sprintf("%.1f%%", proportion)),
                  position = position_dodge(width = 0.9),
                  vjust = -0.5, size = 3) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Negative" = "tomato", "Neutral" = "gray", "Positive" = "steelblue")) +
        labs(title = "Gangtok: Yearly Sentiment Distribution",
             x = "Year",
             y = "Proportion (%)",
             color = "Sentiment") +
        theme_minimal()
# Combine Plots
combined_date_plot <- date_plot_shimla | date_plot_gangtok
combined_date_plot


#######################################################################
#######################################################################
