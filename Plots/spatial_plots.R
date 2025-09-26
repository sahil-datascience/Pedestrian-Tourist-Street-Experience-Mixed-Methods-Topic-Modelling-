# Required packages
library(tidyverse)
library(tidytext)   # provides reorder_within and scale_x_reordered
library(scales)
library(glue)

# -------------------------
# Load data
# -------------------------
shimla <- read_csv("Data/tidy/shimla_reviews_3.csv")
gangtok <- read_csv("Data/tidy/gangtok_reviews_3.csv")

# Select relevant columns
shimla <- shimla %>% select(doc_id, Cities, Countries) %>% mutate(location = "Shimla")
gangtok <- gangtok %>% select(doc_id, Cities, Countries) %>% mutate(location = "Gangtok")

# Compute original N per site (use these N for percentage calculations and labels)
site_N <- bind_rows(shimla, gangtok) %>%
        group_by(location) %>%
        summarise(N = n(), .groups = "drop")

# Ensure ordering: Shimla first, then Gangtok
site_order <- c("Shimla", "Gangtok")

# Combine
combined <- bind_rows(shimla, gangtok)

# -------------------------
# Clean / normalise data
# -------------------------
combined_clean <- combined %>%
        mutate(
                Cities = str_squish(Cities),
                Countries = str_squish(Countries)
        ) %>%
        # remove empty/NA/Unknown placeholders
        filter(!(is.na(Cities) | Cities == "" | Cities == "Unknown"),
               !(is.na(Countries) | Countries == "" | Countries == "Unknown"))

# -------------------------
# Plot function (fixed reorder_within + conditional labels)
# -------------------------
plot_top_origins_with_N <- function(data,
                                    var = c("Cities", "Countries"),
                                    top_n = 12,
                                    merge_other = TRUE,
                                    exclude_domestic_country = TRUE,
                                    pct_inside_threshold = 0.08,  # if pct >= 8% show label inside bar
                                    out_file = "Plots/output/top_origins.svg",
                                    width = 10, height = 7) {
        var <- match.arg(var)
        var_sym <- rlang::sym(var)
        
        # Optionally exclude India for Countries plots
        if (var == "Countries" && exclude_domestic_country) {
                data <- data %>% filter(Countries != "India")
        }
        
        # Base counts by site and origin
        df_counts <- data %>%
                group_by(location, !!var_sym) %>%
                summarise(n = n(), .groups = "drop") %>%
                group_by(location) %>%
                arrange(location, desc(n)) %>%
                mutate(rank = row_number()) %>%
                ungroup()
        
        if (merge_other) {
                df_topflag <- df_counts %>%
                        group_by(location) %>%
                        mutate(is_top = rank <= top_n) %>%
                        ungroup() %>%
                        mutate(name = if_else(is_top, as.character(!!var_sym), "Other"))
                
                df_plot <- df_topflag %>%
                        group_by(location, name) %>%
                        summarise(n = sum(n), .groups = "drop") %>%
                        ungroup()
        } else {
                df_plot <- df_counts %>%
                        filter(rank <= top_n) %>%
                        rename(name = !!var_sym)
        }
        
        # Attach site N for percentage calculation and create location label with N
        df_plot <- df_plot %>%
                left_join(site_N, by = "location") %>%
                mutate(pct = n / N,
                       label_text = sprintf("%d (%.1f%%)", n, pct * 100),
                       location_label = paste0(location, " (N=", N, ")"))
        
        # Build desired labels in order Shimla then Gangtok
        desired_labels <- site_order %>%
                map_chr(~ paste0(.x, " (N=", site_N %>% filter(location == .x) %>% pull(N), ")"))
        
        # Ensure factor ordering for facet labels
        df_plot <- df_plot %>%
                mutate(location_label = factor(location_label, levels = desired_labels))
        
        # reorder_within: correct arg order is reorder_within(x, by, within)
        df_plot <- df_plot %>%
                arrange(location_label, n) %>%
                mutate(ordered_name = reorder_within(name, n, location_label))
        
        # Conditional labels: inside if pct >= threshold, else outside
        df_inside <- df_plot %>% filter(pct >= pct_inside_threshold)
        df_outside <- df_plot %>% filter(pct < pct_inside_threshold)
        
        expand_mult <- 0.30
        
        p <- ggplot(df_plot, aes(x = ordered_name, y = n, fill = location_label)) +
                geom_col(show.legend = FALSE) +
                # inside labels (white)
                geom_text(data = df_inside, aes(label = label_text),
                          color = "white", size = 3.0, family = "Times New Roman",
                          position = position_stack(vjust = 0.5), hjust = 0.5) +
                # outside labels (black) - placed slightly right of bar
                geom_text(data = df_outside, aes(label = label_text),
                          color = "black", size = 3.0, family = "Times New Roman",
                          hjust = -0.02) +
                coord_flip() +
                facet_wrap(~location_label, scales = "free_y", ncol = 1) +
                scale_x_reordered() +
                scale_y_continuous(expand = expansion(mult = c(0, expand_mult))) +
                labs(
                        x = NULL,
                        y = "Number of reviews",
                        title = if_else(var == "Cities", "A. Cities", "B. Countries"),
                        caption = if_else(var == "Countries" & exclude_domestic_country,
                                          "Domestic country (India) excluded from country plot.",
                                          "")
                ) +
                theme_minimal(base_size = 12) +
                theme(
                        text = element_text(family = "Times New Roman"),
                        strip.text = element_text(face = "italic"),
                        axis.text.y = element_text(size = 9),
                        panel.grid.major.y = element_blank()
                )
        
        # Save as SVG
        ggsave(out_file, p, device = "svg", width = width, height = height)
        message("Saved plot to: ", out_file)
        return(p)
}

# -------------------------
# Run plots
# -------------------------
# Top cities (top_n = 12), save as SVG
p_cities <- plot_top_origins_with_N(
        data = combined_clean,
        var = "Cities",
        top_n = 12,
        merge_other = FALSE,
        exclude_domestic_country = FALSE,
        pct_inside_threshold = 0.08,
        out_file = "Plots/output/top_cities_by_site_withN.svg",
        width = 9,
        height = 7
)
p_cities

# Top countries (top_n = 12, exclude India), save as SVG
p_countries <- plot_top_origins_with_N(
        data = combined_clean,
        var = "Countries",
        top_n = 12,
        merge_other = FALSE,
        exclude_domestic_country = TRUE,
        pct_inside_threshold = 0.08,
        out_file = "Plots/output/top_countries_by_site_withN.svg",
        width = 9,
        height = 7
)
p_countries



# Combine
library(patchwork)
p_combined <-   (p_cities | p_countries) 

p_combined <- p_combined +
        plot_annotation(
                title = "Spatial Diversity of Tourists",
                subtitle = "Top 12 cities and countries per site",
                caption = ""
        ) &
        theme(
                plot.title = element_text(family = "Times New Roman", face = "bold", size = 18, hjust = 0.5),
                plot.subtitle = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
                plot.caption = element_text(family = "Times New Roman", size = 8),
                axis.title.x = element_text(family = "Times New Roman", size = 9, color = "black")
        )

p_combined

ggsave("Plots/output/combined_spatial_diversity.svg", p_combined, device = "svg", width = 10, height = 7.5)

                                                   