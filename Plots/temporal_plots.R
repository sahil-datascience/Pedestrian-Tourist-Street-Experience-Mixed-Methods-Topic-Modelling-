
library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)


# Import data
shimla <- read_csv("Data/tidy/shimla_reviews_3.csv")
gangtok <- read_csv("Data/tidy/gangtok_reviews_3.csv")

# Select Relevant Columns
shimla <- shimla %>%
  select(doc_id, Date, Season) %>%
  mutate(location = "Shimla")
gangtok <- gangtok %>% 
  select(doc_id, Date, Season) %>%
  mutate(location = "Gangtok")

# Date format: "2022-12-15"
# Season format: "Autumn (Sep-Nov)","Summer (Jun-Aug)","Winter (Dec-Feb)","Spring (March-May)"

# Combine
all_reviews <- bind_rows(shimla, gangtok)

# Make sure Date is a Date object (adjust format if needed)
all_reviews <- all_reviews %>%
        mutate(Date = as_date(Date))    # if Date is "YYYY-MM-DD"


############################################

# -------------------------
#  Yearly % (line chart)
# -------------------------
year_summary <- all_reviews %>%
        filter(!is.na(Date)) %>%
        mutate(year = year(Date)) %>%
        group_by(location, year) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(location) %>%
        mutate(pct = n / sum(n) * 100) %>%
        ungroup()

# optional: you can limit labels to values above a small pct threshold to avoid clutter
# year_summary <- year_summary %>% filter(pct >= 1)   # keep only >=1% (adjust as needed)

p_year <- ggplot(year_summary, aes(x = year, y = pct, color = location, group = location)) +
        geom_line(size = 1.1) +
        geom_point(size = 2) +
        # use geom_text_repel to avoid overlapping labels
        geom_text_repel(aes(label = sprintf("%0.01f%%", pct)),
                        nudge_y = 5,                         # slight upward nudge (adjust)
                        direction = "x",                       # repel in y-direction primarily
                        segment.size = 0.25,                   # leader line thickness
                        segment.alpha = 0.6,
                        size = 3.0,
                        family = "Times New Roman",
                        box.padding = 0.35,
                        point.padding = 0.4,
                        max.overlaps = 20,
                        show.legend = FALSE) +
        scale_x_continuous(breaks = sort(unique(year_summary$year))) +
        scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.12))) +
        labs(x = "Year", y = "Percent of reviews",
             title = "Temporal (Yearly) Distribution of Reviews",
             subtitle = "Percent of each site's reviews per year",
             color = "Site") +
        theme_minimal(base_size = 12) +
        theme(text = element_text(family = "Times New Roman"),
              legend.position = "top",
              plot.title = element_text(face = "bold", family = "Times New Roman", size = 14, hjust = 0.5),
              plot.subtitle = element_text(size = 10, family = "Times New Roman", hjust = 0.5))

p_year

# Save as SVG (or PNG if you prefer)
svg_file <- "Plots/output/yearly_distribution.svg"
ggsave(svg_file, p_year, device = "svg", width = 8, height = 4, dpi = 300)

