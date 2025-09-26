
library(tidyverse)

# Load Data
shimla_reviews <- read_csv("Data/tidy/shimla_reviews_3.csv") # n = 3652
gangtok_reviews <- read_csv("Data/tidy/gangtok_reviews_3.csv") # n = 1360


#------------------------ Data Preparation -------------------------------------#

# Add nationality column 'Foriegn' or 'Domestic'
shimla_reviews <- shimla_reviews %>% 
        mutate(Nationality = ifelse(Countries == "India", "Domestic", "Foreign"))
gangtok_reviews <- gangtok_reviews %>% 
        mutate(Nationality = ifelse(Countries == "India", "Domestic", "Foreign"))

# Extract Year from Date
shimla_reviews <- shimla_reviews %>% 
        mutate(Year = lubridate::year(lubridate::ymd(Date)))

gangtok_reviews <- gangtok_reviews %>% 
        mutate(Year = lubridate::year(lubridate::ymd(Date)))


# Add initial sentiment column so that our data do not only contain positive reviews

# Using VADER via the vader package
library(vader)
# Function to compute sentiment
compute_sentiment <- function(df, text_col) {
        sentiments <- vader_df(df[[text_col]])
        df <- bind_cols(df, sentiments)
        return(df)
}
# Apply to samples
gangtok_reviews <- compute_sentiment(gangtok_reviews, "raw_text")
shimla_reviews <- compute_sentiment(shimla_reviews, "Main_Text")


# Create & Apply Thresholds (We want labels to match our manual labels)
gangtok_reviews <- gangtok_reviews %>%
        mutate(
                sentiment_rule_based = case_when(
                        compound >= 0.5 ~ "Totally Positive",
                        compound > 0 & compound < 0.5 ~ "Mostly Positive",
                        compound == 0 ~ "Mixed",
                        compound < 0 & compound > -0.5 ~ "Mostly Negative",
                        compound <= -0.5 ~ "Totally Negative"
                )
        )
shimla_reviews <- shimla_reviews %>%
        mutate(
                sentiment_rule_based = case_when(
                        compound >= 0.5 ~ "Totally Positive",
                        compound > 0 & compound < 0.5 ~ "Mostly Positive",
                        compound == 0 ~ "Mixed",
                        compound < 0 & compound > -0.5 ~ "Mostly Negative",
                        compound <= -0.5 ~ "Totally Negative"
                )
        )


# Keep relevant columns only
shimla_reviews <- shimla_reviews %>% 
        select(doc_id, Date, Year, Season, Nationality, Main_Text,
               sentiment_rule_based)

gangtok_reviews <- gangtok_reviews %>% 
        select(doc_id, Date, Year, Season, Nationality, raw_text,
               sentiment_rule_based)



#------------------------ EDA -------------------------------------#
# Plot Years 
p1 <- ggplot(shimla_reviews, aes(x = as.factor(Year))) +
        geom_bar(fill = "steelblue") +
        labs(title = "Shimla Reviews by Year",
             x = "Year",
             y = "Number of Reviews") +
        theme_minimal()
p1
p2 <- ggplot(gangtok_reviews, aes(x = as.factor(Year))) +
        geom_bar(fill = "darkgreen") +
        labs(title = "Gangtok Reviews by Year",
             x = "Year",
             y = "Number of Reviews") +
        theme_minimal()
p2

# Plot National
p3 <- ggplot(shimla_reviews, aes(x = Nationality)) +
        geom_bar(fill = "steelblue") +
        labs(title = "Shimla Reviews by Nationality") +
        theme_minimal()
p3             
p4 <- ggplot(gangtok_reviews, aes(x = Nationality)) +
        geom_bar(fill = "darkgreen") +
        labs(title = "Gangtok Reviews by Nationality") +
        theme_minimal()       
p4

# Plot Sentiment
p5 <- ggplot(shimla_reviews, aes(x = sentiment_rule_based)) +
        geom_bar(fill = "steelblue") +
        labs(title = "Shimla Reviews by Sentiment") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
p5
p6 <- ggplot(gangtok_reviews, aes(x = sentiment_rule_based)) +
        geom_bar(fill = "darkgreen") +
        labs(title = "Gangtok Reviews by Sentiment") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6

#------------------------ Sampling -------------------------------------#
# We will do stratified sampling based on Year, Nationality, and Sentiment

             

set.seed(123) # reproducibility

# Function to draw stratified sample
sample_reviews <- function(df, n_sample) {
        df %>%
                group_by(Year, Nationality, sentiment_rule_based) %>%
                sample_frac(size = n_sample / nrow(df), replace = FALSE) %>%
                ungroup()
}

# Option 1: Fixed sample of 100 per site
#shimla_sample <- sample_reviews(shimla_reviews, 100)
#gangtok_sample <- sample_reviews(gangtok_reviews, 100)

# Option 2: Proportional 10% sample
shimla_sample_10 <- sample_reviews(shimla_reviews, round(0.1 * nrow(shimla_reviews)))
gangtok_sample_10 <- sample_reviews(gangtok_reviews, round(0.1 * nrow(gangtok_reviews)))

# Plot
p7 <- ggplot(shimla_sample_10, aes(x = as.factor(Year))) +
        geom_bar(fill = "steelblue") +
        labs(title = "Shimla Sample Reviews by Year",
             x = "Year",
             y = "Number of Reviews") +
        theme_minimal()
p7
p8 <- ggplot(gangtok_sample_10, aes(x = as.factor(Year))) +
        geom_bar(fill = "darkgreen") +
        labs(title = "Gangtok Sample Reviews by Year",
             x = "Year",
             y = "Number of Reviews") +
        theme_minimal()
p8

# Plot National
p9 <- ggplot(shimla_sample_10, aes(x = Nationality)) +
        geom_bar(fill = "steelblue") +
        labs(title = "Shimla Sample Reviews by Nationality") +
        theme_minimal()
p9
p10 <- ggplot(gangtok_sample_10, aes(x = Nationality)) +
        geom_bar(fill = "darkgreen") +
        labs(title = "Gangtok Sample Reviews by Nationality") +
        theme_minimal()
p10

# Plot Sentiment
p11 <- ggplot(shimla_sample_10, aes(x = sentiment_rule_based)) +
        geom_bar(fill = "steelblue") +
        labs(title = "Shimla Sample Reviews by Sentiment") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
p11
p12 <- ggplot(gangtok_sample_10, aes(x = sentiment_rule_based)) + 
        geom_bar(fill = "darkgreen") +
        labs(title = "Gangtok Sample Reviews by Sentiment") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

#------------------------- Create a Before and After canvas of all plots ------------------#
library(patchwork)

# Summary of plots
#------------------ All Data ------------------#
# p1: Shimla Reviews by Year 
# p2: Gangtok Reviews by Year
# p3: Shimla Reviews by Nationality
# p4: Gangtok Reviews by Nationality
# p5: Shimla Reviews by Sentiment 
# p6: Gangtok Reviews by Sentiment
#------------------ Sampled Data ------------------#
# p7: Shimla Sample Reviews by Year
# p8: Gangtok Sample Reviews by Year
# p9: Shimla Sample Reviews by Nationality
# p10: Gangtok Sample Reviews by Nationality
# p11: Shimla Sample Reviews by Sentiment
# p12: Gangtok Sample Reviews by Sentiment



# Compare Shimla All vs Sample
shimla_compare <- (p1 | p7) / (p3 | p9) / (p5 | p11) +
        plot_annotation(title = "EDA and Sampling of Shimla and Gangtok Reviews",
                        subtitle = "Top 3 rows: Full Data | Bottom 3 rows: Sampled Data",
                        caption = "Data Source: TripAdvisor | Analysis: R (tidyverse, vader, patchwork)") &
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 14, hjust = 0.5),
              plot.caption = element_text(size = 10, hjust = 0.5))
shimla_compare


        # Compare Gangtok All vs Sample
gangtok_compare <- (p2 | p8) / (p4 | p10) / (p6 | p12) +
        plot_annotation(title = "EDA and Sampling of Shimla and Gangtok Reviews",
                        subtitle = "Top 3 rows: Full Data | Bottom 3 rows: Sampled Data",
                        caption = "Data Source: TripAdvisor | Analysis: R (tidyverse, vader, patchwork)") &
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 14, hjust = 0.5),
              plot.caption = element_text(size = 10, hjust = 0.5))
gangtok_compare

# Combine both Shimla and Gangtok comparisons vertically
combined_plot <- shimla_compare | gangtok_compare +
        plot_layout(ncol = 1) +
        plot_annotation(title = "EDA and Sampling of Shimla and Gangtok Reviews",
                        subtitle = "Top 3 rows: Full Data | Bottom 3 rows: Sampled Data",
                        caption = "Data Source: TripAdvisor | Analysis: R (tidyverse, vader, patchwork)") &
        theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 8, hjust = 0.5),
              plot.caption = element_text(size = 6, hjust = 0.5))
combined_plot
# Improve layout and spacing
# Save the combined plot
ggsave("4. Sentiment Analysis/EDA_Sampling_Shimla_Gangtok.svg", width = 16, height = 20)


# Export dataset
folder_path <- "4. Sentiment Analysis"
write_csv(shimla_sample_10, paste0(folder_path, "/Data/shimla_sample_10.csv")) # n = 351
write_csv(gangtok_sample_10, paste0(folder_path, "/Data/gangtok_sample_10.csv")) # n = 127









