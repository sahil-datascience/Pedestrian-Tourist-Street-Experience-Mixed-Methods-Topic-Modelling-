

######################################################################
############### Data Import and Pre-Processing (Temporal) ############
######################################################################

### Load Libraries
library(tidyverse)

###
### Import Data
###

mall_reviews <- read_csv("Data/raw/mall_reviews.csv")
ridge_reviews <- read_csv("Data/raw/ridge_reviews.csv")



###
### Pre - Processing
###

#Add Source
mall_reviews$Source <- "Mall"
ridge_reviews$Source <- "Ridge"

#Merge Data
all_reviews <- rbind(mall_reviews, ridge_reviews) # n = 1357

# Mutate Doc ID
all_reviews <- all_reviews %>%
        mutate(doc_id = paste0('text', seq(1, nrow(all_reviews))))


# Merge Tabline with Review Text
all_reviews$Main_Text <- paste(all_reviews$Scrape_Tagline, all_reviews$Scrape_Review_Text)

# Remove Duplicate
#dim(all_reviews)
all_reviews <- all_reviews %>% distinct(Main_Text, .keep_all = TRUE)
dim(all_reviews) # 1 Duplicate Removed

names(all_reviews)
# Keep only relevant columns
all_reviews <- all_reviews %>%
        select(doc_id, Date, Source, Tourist_Home_Location, Main_Text)


##############################################################
################# Temporal Variable ##########################
##############################################################


#Format date variable 
all_reviews$Date <- str_extract(all_reviews$Date, "\\d{1,2} \\w+ \\d{4}")
all_reviews$Date <- lubridate::dmy(all_reviews$Date)

#Include Seasons

# Extract month and year
reviews_seasonal <- all_reviews %>%
        mutate(Month = format(Date, "%m"),
               Year = format(Date, "%Y"))

# Create a new column for season
reviews_seasonal <- reviews_seasonal %>%
        mutate(Season = case_when(
                Month %in% c("12", "01", "02") ~ "Winter (Dec-Feb)",
                Month %in% c("03", "04", "05") ~ "Spring (March-May)",
                Month %in% c("06", "07", "08") ~ "Summer (Jun-Aug)",
                Month %in% c("09", "10", "11") ~ "Autumn (Sep-Nov)"
        ))

# Aggregate data by season and year

seasonal_counts <- reviews_seasonal %>%
        group_by(Season, Year) %>%
        summarise(Count = n(), .groups = 'drop')


# Add a Season Column to all_reviews
all_reviews <- all_reviews %>%
        mutate(Month = format(Date, "%m"),
               Year = format(Date, "%Y")) %>%
        mutate(Season = case_when(
                Month %in% c("12", "01", "02") ~ "Winter (Dec-Feb)",
                Month %in% c("03", "04", "05") ~ "Spring (March-May)",
                Month %in% c("06", "07", "08") ~ "Summer (Jun-Aug)",
                Month %in% c("09", "10", "11") ~ "Autumn (Sep-Nov)"
        )) %>%
        select(-Month, -Year)



# Keep only relevant columns
all_reviews <- all_reviews %>%
        select(doc_id, Source, Date, Season, Tourist_Home_Location, Main_Text)

## Save
## Save Data
write_csv(all_reviews, "Data/tidy/shimla_reviews_1.csv")

