# Load libraries
library(tidyverse)
library(stm)
library(scales)
library(patchwork)

# ---------- Helper function ----------
build_topic_df <- function(stm_model, labels) {
        theta_mat <- stm_model$theta
        mean_prop <- colMeans(theta_mat) # average topic proportion
        tibble(
                topic = seq_along(mean_prop),
                label = labels$label,
                mean_prop = mean_prop
        ) %>%
                arrange(mean_prop) %>%
                mutate(label = factor(label, levels = label)) # preserve order for plotting
}

# ---------- Shimla ----------
stm_model_21 <- readRDS("Data/r_data/stm_model_k21.rds")

shimla_labels <- tibble(
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
) %>% mutate(label = paste0(label, " (T", topic, ")"))

shimla_df <- build_topic_df(stm_model_21, shimla_labels)

shimla_plot <- ggplot(shimla_df, aes(x = reorder(label, mean_prop), y = mean_prop)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(x = "Topic",
             y = "Mean topic proportion",
             title = "A: Shimla (K = 21)") +
        scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
        theme_minimal(base_size = 12) +
        theme(text = element_text(family = "Times New Roman"))

# ---------- Gangtok ----------
stm_model_14 <- readRDS("Data/r_data/stm_model_k14.rds")

gangtok_labels <- tibble(
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
) %>% mutate(label = paste0(label, " (T", topic, ")"))

gangtok_df <- build_topic_df(stm_model_14, gangtok_labels)

gangtok_plot <- ggplot(gangtok_df, aes(x = reorder(label, mean_prop), y = mean_prop)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(x = "Topic",
             y = "Mean topic proportion",
             title = "B: Gangtok (K = 14)") +
        scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
        theme_minimal(base_size = 12) +
        theme(text = element_text(family = "Times New Roman"))

# ---------- Combine ----------
combined_plot <- shimla_plot / gangtok_plot

combined_plot <- combined_plot + plot_layout(heights = c(1.5, 1))

combined_plot <- combined_plot +
        plot_annotation(
                title = "Mean Topic Proportions for Shimla and Gangtok",
                subtitle = "Bars show the average share of review content devoted to each topic (STM outputs)",
                theme = theme(
                        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "Times New Roman"),
                        plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5, family = "Times New Roman")
                )
        )

# Print combined plot
print(combined_plot)

# Save (SVG or PNG)
ggsave("Plots/output/mean_topic_proportions.svg", combined_plot, width = 7, height = 8.5, dpi = 300)
