# plot_theme_prevalence_by_site.R
# Computes theme-level prevalence from STM topic prevalences and plots comparison across sites.
# Assumes:
# - topic-theme mapping file at "3. Qualitative Exploration/Codebook-Abstraction-Notes/topic-theme-mapping.xlsx"
# - STM model files at "Data/r_data/stm_model_k21.rds" (Shimla) and "Data/r_data/stm_model_k14.rds" (Gangtok)
# - Output folder "Plots/" exists (or adjust path)

library(tidyverse)
library(readxl)
library(stm)
library(forcats)
library(scales)

# ---------------------- User paths (change if needed) -------------------------
mapping_path <- "3. Qualitative Exploration/Codebook-Abstraction-Notes/topic-theme-mapping.xlsx"
shimla_model_path <- "Data/r_data/stm_model_k21.rds"
gangtok_model_path <- "Data/r_data/stm_model_k14.rds"
out_plot_path <- "Plots/theme_prevalence_by_site.svg"
# ------------------------------------------------------------------------------

# 1. Read topic -> theme mapping
topic_theme_mapping <- read_xlsx(mapping_path) %>%
        mutate(
                # Normalize TopicID to an integer topic number (extract digits)
                TopicID_num = as.integer(str_extract(as.character(TopicID), "\\d+")),
                CommonTheme = str_squish(as.character(CommonTheme)),
                Site = as.character(Site)
        )

# Quick sanity check: ensure mapping has numeric topic ids
if (any(is.na(topic_theme_mapping$TopicID_num))) {
        warning("Some TopicID entries in mapping could not be parsed to integers. Check TopicID column formatting.")
}

# helper to compute theme prevalences from an stm model object
compute_theme_prevalence <- function(stm_model, site_name, mapping_df) {
        if (is.null(stm_model$theta)) stop("STM model does not contain $theta matrix.")
        # mean topic prevalence (proportion of document mass assigned to each topic)
        topic_means <- colMeans(stm_model$theta)
        topic_df <- tibble(
                topic = seq_along(topic_means),
                prevalence = topic_means
        )
        # join mapping for this site (mapping might have separate rows per site)
        mapping_site <- mapping_df %>% filter(Site == site_name)
        # If mapping has no Site column match, fallback to any mapping (useful if mapping file doesn't use Site)
        if (nrow(mapping_site) == 0) {
                mapping_site <- mapping_df
                message(glue::glue("No rows in topic-theme mapping for site '{site_name}' — using all mapping rows instead."))
        }
        # join by numeric topic id
        joined <- topic_df %>%
                left_join(mapping_site %>% select(TopicID_num, CommonTheme), by = c("topic" = "TopicID_num"))
        
        # if any topics do not have a theme, label as "UNMAPPED"
        joined <- joined %>%
                mutate(CommonTheme = ifelse(is.na(CommonTheme), "UNMAPPED", CommonTheme))
        
        # Aggregate: sum topic prevalences for topics mapping to same theme
        theme_df <- joined %>%
                group_by(CommonTheme) %>%
                summarise(
                        theme_prevalence = sum(prevalence, na.rm = TRUE),
                        n_topics = n(),
                        .groups = "drop"
                ) %>%
                arrange(desc(theme_prevalence)) %>%
                mutate(
                        Site = site_name,
                        theme_prevalence_pct = theme_prevalence * 100   # optional percent
                )
        return(theme_df)
}

# 2. Load models
stm_model_shimla <- readRDS(shimla_model_path)
stm_model_gangtok <- readRDS(gangtok_model_path)

# 3. Compute theme prevalence for each site
theme_shimla <- compute_theme_prevalence(stm_model_shimla, "Shimla", topic_theme_mapping)
theme_gangtok <- compute_theme_prevalence(stm_model_gangtok, "Gangtok", topic_theme_mapping)

# 4. Combine and prepare for plotting
theme_combined <- bind_rows(theme_shimla, theme_gangtok)

# Optionally define theme order by overall prevalence across both sites
theme_order <- theme_combined %>%
        group_by(CommonTheme) %>%
        summarise(total = sum(theme_prevalence), .groups = "drop") %>%
        arrange(desc(total)) %>%
        pull(CommonTheme)

theme_combined <- theme_combined %>%
        mutate(CommonTheme = factor(CommonTheme, levels = theme_order))
theme_combined <- theme_combined %>%
        mutate(Site = forcats::fct_rev(Site))

# 5. Plot: grouped bar chart (themes on x, prevalence on y) with sites side-by-side
p <- ggplot(theme_combined, aes(x = CommonTheme, y = theme_prevalence_pct, fill = Site)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.65) +
        labs(
                x = "Themes",
                y = "Mean Theme Proportion (%)",
                title = "Mean Theme Proportions by Site",
                subtitle = "Computed by summing topic proportions assigned to each theme\n(Mean topic proportions from STM models)"
        ) +
        theme_minimal(base_size = 15, base_family = "Times New Roman") +
        theme(
                legend.position = "top",
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
                axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 15)),  # push away from axis
                axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15)),  # push away from ticks
                axis.text.y  = element_text(size = 12),
                axis.text.x  = element_text(size = 13, angle = 45, hjust = 1),
                plot.margin  = margin(t = 15, r = 20, b = 50, l = 40)  # extra bottom/left spacing
        ) +
        scale_y_continuous(labels = scales::percent_format(scale = 1))

# Print plot
print(p)

# 6. Save
ggsave(out_plot_path, p, width = 12, height = 8, device = "svg")
message("Saved plot to ", out_plot_path)
