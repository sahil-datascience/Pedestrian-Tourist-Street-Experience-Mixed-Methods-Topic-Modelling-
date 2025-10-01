

########################### This is the final script used.

library(tidyverse)
library(readxl)

file_path <- "3. Qualitative Exploration/Codebook-Abstraction-Notes/topic-theme-mapping.xlsx"

#----------------------------- Manual Exploration -------------------------------#
topic_theme_mapping <- read_xlsx(file_path) %>%
        mutate(
                Site = as.factor(Site),
                TopicID = as.character(TopicID),
                TopicName = factor(TopicName),
                CommonTheme = factor(CommonTheme)
        )
topic_theme_mapping

themes <- table(topic_theme_mapping$CommonTheme)
names(themes)

topic_theme_mapping %>%
        filter(Site == "Gangtok" & CommonTheme == names(themes)[7])

#----------------------------- Function to save as .svg -------------------------------#

convert_gt_to_svg <- function(gt_tbl, out_svg, 
                              out_pdf = sub("\\.svg$", ".pdf", out_svg),
                              width = 11, height = 8.5) {
        message("Saving gt table as PDF: ", out_pdf)
        gtsave(gt_tbl, out_pdf, vwidth = width, vheight = height)
        
        # converter lookup
        inkscape_path <- Sys.which("inkscape")
        
        if (!nzchar(inkscape_path)) stop("Inkscape not found in PATH")
        
        message("Converting PDF to SVG with Inkscape...")
        system2(inkscape_path, args = c(shQuote(out_pdf), "--export-filename", shQuote(out_svg)))
        
        if (file.exists(out_svg)) {
                message("SVG saved at: ", normalizePath(out_svg))
        } else {
                stop("Conversion ran but SVG not created.")
        }
}


#--------------------------- Topic ID and Topic Name Mapping Table ---------------------------#

library(dplyr)
library(stringr)
library(gt)

# helper to extract numeric id from strings like "1.", " 10.", "2. "
extract_num <- function(x) {
        x %>% 
                str_trim() %>% 
                str_extract("\\d+") %>%    # extract the first run of digits
                as.integer()
}

# prepare shimla with numeric id
shimla_tbl <- df %>%
        filter(Site == "Shimla") %>%
        select(TopicID, TopicName) %>%
        mutate(id = extract_num(TopicID)) %>%
        arrange(id) %>%
        select(id, TopicName)     # id is numeric

# prepare gangtok with numeric id
gangtok_tbl <- df %>%
        filter(Site == "Gangtok") %>%
        select(TopicID, TopicName) %>%
        mutate(id = extract_num(TopicID)) %>%
        arrange(id) %>%
        select(id, TopicName)

# create a master vector of all IDs that appear in either site, sorted numerically
all_ids <- sort(unique(c(shimla_tbl$id, gangtok_tbl$id)), na.last = NA)

# master tibble to align both sites by numeric topic id
master <- tibble(id = all_ids) %>%
        left_join(shimla_tbl %>% rename(Shimla_Name = TopicName), by = "id") %>%
        left_join(gangtok_tbl %>% rename(Gangtok_Name = TopicName), by = "id")

# prepare display columns: T prefix for IDs, blanks instead of NA
master_display <- master %>%
        mutate(
                Shimla_ID = ifelse(is.na(Shimla_Name), "", paste0("**T", id, "**")),
                Gangtok_ID = ifelse(is.na(Gangtok_Name), "", paste0("**T", id, "**")),
                Shimla_Name = ifelse(is.na(Shimla_Name), "", Shimla_Name),
                Gangtok_Name = ifelse(is.na(Gangtok_Name), "", Gangtok_Name)
        ) %>%
        select(Shimla_ID, Shimla_Name, Gangtok_ID, Gangtok_Name)

# build gt table
gt_tbl <- master_display %>%
        gt() %>%
        tab_spanner(label = md("**A. Shimla**"), columns = c(Shimla_ID, Shimla_Name)) %>%
        tab_spanner(label = md("**B. Gangtok**"), columns = c(Gangtok_ID, Gangtok_Name)) %>%
        cols_label(
                Shimla_ID   = md("**Topic ID**"),
                Shimla_Name = md("**Topic Name**"),
                Gangtok_ID  = md("**Topic ID**"),
                Gangtok_Name= md("**Topic Name**")
        ) %>%
        tab_header(title = md("**Topic ID ↔ Topic Name**")) %>%
        fmt_markdown(columns = everything()) %>%   # render the markdown
        opt_table_font(font = list(google_font("Times New Roman"), default_fonts()))


gt_tbl


# save
# Example with your gt table object
#convert_gt_to_svg(gt_tbl, out_svg = "Plots/topicID_name_mapping.svg", width = 9, height = 7)
gtsave(gt_tbl, "Plots/topicID_name_mapping.png")

#---------------------------------------- Theme - Topic Mapping ------------------------------------------------#

library(dplyr)
library(stringr)
library(tidyr)
library(gt)

# 0) Clean + stable theme order (reuse your approach)
df2 <- df %>%
        mutate(
                TopicID_chr   = str_replace_all(as.character(TopicID), "\\s+|\\.", ""),  # e.g., "10"
                TopicName     = str_squish(TopicName),
                CommonTheme   = str_squish(CommonTheme)
        )

if (!inherits(df2$CommonTheme, "factor")) {
        theme_order <- df2 %>% count(CommonTheme, sort = TRUE) %>% pull(CommonTheme)
        df2$CommonTheme <- factor(df2$CommonTheme, levels = theme_order)
}

# helper: build per-site columns (IDs & Names collapsed with <br/>)
build_site_cols <- function(data, site_name) {
        data %>%
                filter(Site == site_name) %>%
                arrange(CommonTheme, as.numeric(TopicID_chr)) %>%
                group_by(CommonTheme) %>%
                summarise(
                        !!paste0(site_name, "_IDs")   := paste0("T", TopicID_chr, collapse = "<br/>"),
                        !!paste0(site_name, "_Names") := paste(TopicName, collapse = "<br/>"),
                        .groups = "drop"
                )
}

shimla <- build_site_cols(df2, "Shimla")
gangtok <- build_site_cols(df2, "Gangtok")

tbl <- full_join(shimla, gangtok, by = "CommonTheme") %>%
        mutate(across(-CommonTheme, ~ tidyr::replace_na(., ""))) %>%
        # make Themes bold (content)
        mutate(CommonTheme = paste0("**", as.character(CommonTheme), "**"))

# ---- GT table ----
gt_tbl <- tbl %>%
        gt() %>%
        # Site spanners (bold)
        tab_spanner(label = md("**A. Shimla**"),  columns = c(Shimla_IDs, Shimla_Names)) %>%
        tab_spanner(label = md("**B. Gangtok**"), columns = c(Gangtok_IDs, Gangtok_Names)) %>%
        # Column labels (bold)
        cols_label(
                CommonTheme   = md("**Theme**"),
                Shimla_IDs    = md("**Topic ID**"),
                Shimla_Names  = md("**Topic Name**"),
                Gangtok_IDs   = md("**Topic ID**"),
                Gangtok_Names = md("**Topic Name**")
        ) %>%
        # Title (bold)
        tab_header(title = md("**Theme → Topic Mapping**")) %>%
        # Render markdown and <br/> in cells
        fmt_markdown(columns = everything()) %>%
        # Font
        opt_table_font(font = list(google_font("Times New Roman"), default_fonts()))

gt_tbl

# Save
#convert_gt_to_svg(gt_tbl, "Plots/theme_topic_mapping.svg", width = 10, height = 8)

gtsave(gt_tbl, "Plots/theme_topic_mapping.png")

