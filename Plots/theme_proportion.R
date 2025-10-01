##############################################################################
library(tidyverse)
library(scales)
library(stm)    # ensure stm is loaded for model objects

# ---------- assume these exist from your previous code ----------
# stm_model_21, stm_model_14
# shimla_topics (topic, label with (Tn))
# gangtok_topics (topic, label with (Tn))
# ----------------------------------------------------------------

# --- 1. Define mapping from topic -> common theme (edit as needed) ---
# Suggested theme names: Shopping, Food, Atmosphere, Accessibility, Heritage, TouristPressure, Leisure, Design, Culture, Other

# Shimla mapping (topic numbers -> theme). Edit to match your qualitative coding.
shimla_theme_map <- tibble::tibble(
        topic = 1:21,
        theme = c(
                "Atmosphere",        #1 Vibrancy vs Overhype
                "Atmosphere",        #2 Relaxation, Views & Atmosphere
                "Shopping",          #3 Shopping Street with British Vibes
                "Heritage",          #4 Shimla Mall Road – Hill Station Benchmark
                "Heritage",          #5 The Ridge – Landmark
                "Shopping",          #6 Mall Road Hustle & Heritage Charm
                "Accessibility",     #7 Walking Experience (Monkey Nuisance)
                "Heritage",          #8 The Ridge as Main Attraction
                "Shopping",          #9 Shopping Experience – Pashmina
                "Heritage",          #10 Scandal Point & Historic
                "Leisure",           #11 Spending Time on Mall Road
                "Shopping",          #12 Pricing, Bargaining & Market
                "TouristPressure",   #13 Overcrowding & Peak Season
                "Leisure",           #14 Pleasant Evenings & Romance
                "Shopping",          #15 Boutique Shops & Bargaining
                "Heritage",          #16 British Heritage & Colonial Charm
                "Atmosphere",        #17 Ridge – Panoramic Views
                "Heritage",          #18 Temporal Changes & Colonial Past
                "Accessibility",     #19 Accessibility, Snowfall & Events
                "Culture",           #20 Temples, Toy Train & Transport
                "Shopping"           #21 Exploring Shops, Cafes & Landmarks
        )
)

# Gangtok mapping (edit if needed)
gangtok_theme_map <- tibble::tibble(
        topic = 1:14,
        theme = c(
                "Atmosphere",     #1 MG Road – Heart of Gangtok
                "Food",           #2 Tibetan & Local Street Food
                "Shopping",       #3 Handicrafts & Branded Shopping
                "Leisure",        #4 Leisure, Hangouts & No-Vehicle Zone
                "Comparison",     #5 Comparisons with Other Mall Roads
                "Leisure",        #6 Transformation into Pedestrian Open-Mall
                "Events",         #7 Festivals & Must-Visit Experiences
                "Atmosphere",     #8 Love for Place & People
                "Design",         #9 Street Design & Aesthetics
                "Accessibility",  #10 Walkability & Accessibility Challenges
                "Leisure",        #11 Time-Spending & Evening Leisure
                "Shopping",       #12 Shopping Street with Mixed Stores
                "Shopping",       #13 Clean but Expensive Market
                "Food"            #14 Vibrant Place with Food Joints
        )
)

# --- 2. Compute theme prevalence for Shimla ---
theta_shimla <- data.frame(
        topic = 1:ncol(stm_model_21$theta),
        prevalence = colMeans(stm_model_21$theta)   # proportions summing to ~1
) %>%
        left_join(shimla_theme_map, by = "topic") %>%
        left_join(shimla_topics %>% select(topic, label), by = "topic")  # add label if you want

theme_shimla <- theta_shimla %>%
        group_by(theme) %>%
        summarise(theme_prevalence = sum(prevalence)) %>%
        ungroup() %>%
        mutate(site = "Shimla")

# --- 3. Compute theme prevalence for Gangtok ---
theta_gangtok <- data.frame(
        topic = 1:ncol(stm_model_14$theta),
        prevalence = colMeans(stm_model_14$theta)
) %>%
        left_join(gangtok_theme_map, by = "topic") %>%
        left_join(gangtok_topics %>% select(topic, label), by = "topic")

theme_gangtok <- theta_gangtok %>%
        group_by(theme) %>%
        summarise(theme_prevalence = sum(prevalence)) %>%
        ungroup() %>%
        mutate(site = "Gangtok")

# --- 4. Combine and tidy for plotting ---
theme_compare <- bind_rows(theme_shimla, theme_gangtok)

# Optionally ensure same theme ordering and replace NA themes
theme_compare <- theme_compare %>%
        mutate(theme = if_else(is.na(theme), "Other", theme)) %>%
        # choose a consistent set/order of themes to show (edit to match your interest)
        mutate(theme = factor(theme, levels = c("Shopping","Food","Atmosphere","Heritage",
                                                "Accessibility","Leisure","Design","Events",
                                                "TouristPressure","Culture","Comparison","Other")))

# Replace missing combinations with zero so both sites show all themes
all_themes <- expand.grid(site = unique(theme_compare$site), theme = levels(theme_compare$theme), stringsAsFactors = FALSE)
theme_compare <- all_themes %>%
        as_tibble() %>%
        left_join(theme_compare, by = c("site","theme")) %>%
        mutate(theme_prevalence = replace_na(theme_prevalence, 0))

# --- 5. Plot: grouped bar chart (Shimla vs Gangtok) ---
palette_site <- c("Shimla" = "#2c7fb8", "Gangtok" = "#f03b20")

p_themes <- ggplot(theme_compare, aes(x = theme, y = theme_prevalence, fill = site)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
        scale_fill_manual(values = palette_site) +
        labs(x = "Theme", y = "Proportion of documents (theme share)", 
             title = "Comparative Theme Prevalence: Shimla vs Gangtok",
             subtitle = "Theme prevalence derived from STM topic proportions (summed by theme)",
             fill = "Site") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              plot.title = element_text(face = "bold"))

p_themes

# Save figure
ggsave("Plots/output/theme_prevalence_shimla_gangtok.png", p_themes, width = 10, height = 5, dpi = 300)


# --- 6. OPTIONAL: stacked bar chart (showing composition within each site) ---
p_stacked <- ggplot(theme_compare, aes(x = site, y = theme_prevalence, fill = theme)) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        labs(x = "", y = "Share of themes (by site)", title = "Theme Composition by Site") +
        theme_minimal() +
        theme(legend.position = "right")

p_stacked
ggsave("Plots/output/theme_composition_stacked.png", p_stacked, width = 8, height = 5, dpi = 300)

