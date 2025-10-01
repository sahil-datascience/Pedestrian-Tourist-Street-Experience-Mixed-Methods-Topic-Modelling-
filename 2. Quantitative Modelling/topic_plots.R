
# Load libraries
library(tidyverse)
library(stm)

#----------------------------------------- Shimla -----------------------------------------#
stm_model_21 <- readRDS("Data/r_data/stm_model_k21.rds")

# Topic Names
shimla_topics <- tibble::tibble(
        topic = 1:21,
        label = c(
                "Vibrancy vs Overhype",
                "Relaxation, Views & Atmosphere",
                "Shopping Street with British Vibes",
                "Shimla Mall Road – Hill Station Benchmark",
                "The Ridge – Landmark Gathering Place",
                "Mall Road Hustle & Heritage Charm",
                "Walking Experience (Monkey Nuisance)",
                "The Ridge as Main Attraction",
                "Shopping Experience – Pashmina & Bargaining",
                "Scandal Point & Historic Connection",
                "Spending Time on Mall Road",
                "Pricing, Bargaining & Market Comparisons",
                "Overcrowding & Peak Season Struggles",
                "Pleasant Evenings & Romance",
                "Boutique Shops & Bargaining Culture",
                "British Heritage & Colonial Charm",
                "Ridge – Panoramic Views & Activities",
                "Temporal Changes & Colonial Past",
                "Accessibility, Snowfall & January Events",
                "Temples, Toy Train & Transport",
                "Exploring Shops, Cafes & Landmarks"
        )
) %>%
        mutate(label = paste0(label, " (T", topic, ")"))

# Calculate mean topic proportions
theta_shimla <- data.frame(
        topic = 1:ncol(stm_model_21$theta),
        prevalence = colMeans(stm_model_21$theta)
) %>%
        left_join(shimla_topics, by = "topic") %>%
        mutate(pct = prevalence * 100) %>%                # convert to percent
        arrange(pct) %>%
        mutate(label = factor(label, levels = label))    # preserve order for plotting

# Join with topic labels
theta_shimla <- theta_shimla %>%
        left_join(shimla_topics, by = "topic")

# Plot
shimla_topics <- ggplot(theta_shimla, aes(x = reorder(label, prevalence), y = prevalence)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(x = "Topic",
             y = "Proportion of documents",
             title = "A: Shimla (K 21)") +
        theme_minimal()

shimla_topics
#----------------------------------------- Gangtok -----------------------------------------#
# Load the STM model for Gangtok
stm_model_14 <- readRDS("Data/r_data/stm_model_k14.rds")

# Topic Names

gangtok_topics <- tibble::tibble(
        topic = 1:14,
        label = c(
                "MG Road – Heart of Gangtok",
                "Tibetan & Local Street Food",
                "Handicrafts & Branded Shopping",
                "Leisure, Hangouts & No-Vehicle Zone",
                "Comparisons with Other Mall Roads",
                "Transformation into Pedestrian Open-Mall",
                "Festivals & Must-Visit Experiences",
                "Love for Place & People",
                "Street Design & Aesthetics",
                "Walkability & Accessibility Challenges",
                "Time-Spending & Evening Leisure",
                "Shopping Street with Mixed Stores",
                "Clean but Expensive Market",
                "Vibrant Place with Food Joints"
        )
) %>%
        mutate(label = paste0(label, " (T", topic, ")"))


# Calculate mean topic proportions
theta <- data.frame(
        topic = 1:ncol(stm_model_14$theta),
        prevalence = colMeans(stm_model_14$theta)
)

# Join with topic labels
theta <- theta %>%
        left_join(gangtok_topics, by = "topic")

# Plot
gagntok_topics <- ggplot(theta, aes(x = reorder(label, prevalence), y = prevalence)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(x = "Topic",
             y = "Proportion of documents",
             title = "B: Gangtok (K14)") +
        theme_minimal()

#------------------------------------------ Combine Plots ------------------------------------------#
library(patchwork)
combined_plot <- shimla_topics | gagntok_topics 

combined_plot <- combined_plot +
        plot_annotation(
                title = "Topic Proportions for Shimla and Gangtok",
                subtitle = "Derived from STM topic models and labeled after qualitative exploration",
                theme = theme(
                        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "Times New Roman"),
                        plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5, family = "Times New Roman")
                )
        ) & theme(plot.title = element_text(face = "bold", family = "Times New Roman"),
                plot.subtitle = element_text(face = "italic", family = "Times New Roman"))
combined_plot

# Save the combined plot
ggsave("Plots/output/shimla_gangtok_topic_proportions.svg", combined_plot, width = 11, height = 7, dpi = 300)

