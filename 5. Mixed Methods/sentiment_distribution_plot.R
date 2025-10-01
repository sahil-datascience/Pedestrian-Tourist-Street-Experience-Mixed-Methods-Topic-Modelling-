
library(tidyverse)

folder_path <- "4. Sentiment Analysis/Data/final_sentiments"

# Read Data
shimla_sentiments <- read_csv(file.path(folder_path, "shimla_gpt_oss20b.csv")) %>%
        mutate(source = "shimla")
gangtok_sentiments <- read_csv(file.path(folder_path, "gangtok_gpt_oss20b.csv")) %>%
        mutate(source = "gangtok")

# Combine
all_sentiments <- bind_rows(shimla_sentiments, gangtok_sentiments)


# Change plot to normal bars with percentage labels
sentiment_plot2 <- all_sentiments %>%
        group_by(source, llm_label) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(source) %>%
        mutate(proportion = count / sum(count),
               percent_label = paste0(round(proportion * 100, 1), "%")) %>%
        ggplot(aes(x = source, y = count, fill = llm_label)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = percent_label), 
                  position = position_dodge(width = 0.9), 
                  vjust = -0.5, size = 3) +
        labs(title = "Sentiment Distribution by Site",
             x = "Site",
             y = "Count of Sentiments",
             fill = "Sentiment") +
        theme_minimal(base_size = 12) +
        theme(text = element_text(family = "Times New Roman"),
              legend.position = "top",
              plot.title = element_text(face = "bold", family = "Times New Roman", size = 18, hjust = 0.5),
              plot.subtitle = element_text(size = 10, family = "Times New Roman", hjust = 0.5))
print(sentiment_plot2)

# Save .svg
ggsave("Plots/sentiment_distribution_plot.svg", plot = sentiment_plot2, width = 8, height = 5)
