

### Load Libraries
library(tidyverse)

###
### Import Data
###
gangtok_reviews <- read_csv("Data/raw/MG_Road_Gangtok.csv")

names(gangtok_reviews) # Restart Here

# Mutate Document ID
gangtok_reviews <- gangtok_reviews %>%
        mutate(doc_id = paste0('text', seq(1, nrow(gangtok_reviews))))


###
### Pre - Processing
###

#Add Source
gangtok_reviews$Source <- "MG Road"


# Merge Tabline with Review Text
gangtok_reviews$raw_text <- paste(gangtok_reviews$Tagline, gangtok_reviews$review_text)

# Remove Duplicate
dim(gangtok_reviews)
gangtok_reviews <- gangtok_reviews %>% distinct(review_text, .keep_all = TRUE)
dim(gangtok_reviews) # No Duplicate Removed


# Keep only relevant columns
gangtok_reviews <- gangtok_reviews %>%
        select(doc_id, Date, Source, Tourist_Home_location, raw_text)

head(gangtok_reviews)

##############################################################
################# Temporal Variable ##########################
##############################################################


#------------------------- Format date variable -----------------------#

gangtok_reviews$Date <- str_extract(gangtok_reviews$Date, "\\d{1,2} \\w+ \\d{4}")
gangtok_reviews$Date <- lubridate::dmy(gangtok_reviews$Date)

#------------------------- Include Seasons ----------------------------#
# Extract month and year
gangtok_reviews <- gangtok_reviews %>%
        mutate(Month = format(Date, "%m"),
               Year = format(Date, "%Y"))

# Create a new column for season
gangtok_reviews <- gangtok_reviews %>%
        mutate(Season = case_when(
                Month %in% c("12", "01", "02") ~ "Winter (Dec-Feb)",
                Month %in% c("03", "04", "05") ~ "Spring (March-May)",
                Month %in% c("06", "07", "08") ~ "Summer (Jun-Aug)",
                Month %in% c("09", "10", "11") ~ "Autumn (Sep-Nov)"
        ))

# Aggregate data by season and year

seasonal_counts <- gangtok_reviews %>%
        group_by(Season, Year) %>%
        summarise(Count = n(), .groups = 'drop')

#-------------------------- Summary ---------------------------#
table(gangtok_reviews$Season)

# Review Data
gangtok_reviews <- gangtok_reviews %>%
        select(doc_id, Source, Date, Season,Tourist_Home_location, raw_text)
## Save Data
write_csv(gangtok_reviews, "Data/tidy/gangtok_reviews_1.csv") # n = 1385


