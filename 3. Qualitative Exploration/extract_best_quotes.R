


library(tidyverse)
gangtok_quotes <- read_csv("3. Qualitative Exploration/Data/stm_14.csv")
shimla_quotes <- read_csv("3. Qualitative Exploration/Data/stm_21.csv")

names(gangtok_quotes)

# Select cols
gangtok_quotes <- gangtok_quotes %>%
        select(TopicID, RepDocRank, RepDocText) %>%
        mutate(source = "Gangtok")
shimla_quotes <- shimla_quotes %>% 
        select(TopicID, RepDocRank, RepDocText) %>%
        mutate(source = "Shimla")

# Combine
all_quotes <- bind_rows(gangtok_quotes, shimla_quotes)




# Remove NAs in RepDocText
all_quotes <- all_quotes %>%
        filter(!is.na(RepDocText))

# Save
write_csv(all_quotes, "3. Qualitative Exploration/Data/best_quotes_combined.csv")


# ------------------------------------------------------------------------- #
 
# add DocID using tidy data
shimla <- read_csv("Data/tidy/shimla_reviews_3.csv")
gangtok <- read_csv("Data/tidy/gangtok_reviews_3.csv")


# Use fussy-mathcing to link RepDocText (all_quotes) to Main_Text (shimla, gangtok) to get doc_id
library(fuzzyjoin)
# Shimla
shimla_quotes <- stringdist_left_join(all_quotes %>% filter(source == "Shimla"),
                                      shimla %>% select(doc_id, Main_Text),
                                      by = c("RepDocText" = "Main_Text"),
                                      max_dist = 5, distance_col = "dist") %>%
        group_by(TopicID, RepDocRank, RepDocText) %>%
        slice_min(order_by = dist, n = 1) %>%
        ungroup() %>%
        select(-dist) 


shimla_quotes <- shimla_quotes %>%
        select(source, TopicID, doc_id, RepDocText)
#Gangtok
gangtok_quotes <- stringdist_left_join(all_quotes %>% filter(source == "Gangtok"),
                                           gangtok %>% select(doc_id, raw_text),
                                           by = c("RepDocText" = "raw_text"),
                                           max_dist = 5, distance_col = "dist") %>%
        group_by(TopicID, RepDocRank, RepDocText) %>%
        slice_min(order_by = dist, n = 1) %>%
        ungroup() %>%
        select(-dist)
gangtok_quotes <- gangtok_quotes %>%
        select(source, TopicID, doc_id, RepDocText)
 
# combines
all_quotes <- # Combine
        all_quotes <- bind_rows(gangtok_quotes, shimla_quotes)


# Save
write_csv(all_quotes, "3. Qualitative Exploration/Data/best_quotes_combined.csv")



#-------------------------------------------------------------------------------#
# Add themes
library(readxl)
themes <- read_excel("3. Qualitative Exploration/Codebook-Abstraction-Notes/topic-theme-mapping.xlsx")

# Remove . in TopicID
themes$TopicID <- gsub("\\.", "", themes$TopicID)
themes$TopicID <- str_trim(themes$TopicID)
# Remove spaces


# Rename Site to source
themes <- themes %>% rename(source = Site)


# Join by source and Topic ID
# Change TopicID in all_quotes to character
all_quotes$TopicID <- as.character(all_quotes$TopicID)


all_quotes <-  all_quotes %>%
        left_join(
                themes %>% select(source, TopicID, TopicName, CommonTheme),
                by = c("source", "TopicID")
        )


# Rearrange columns
all_quotes <- all_quotes %>%
        select(source, TopicID, TopicName, CommonTheme, doc_id, RepDocText)


# Save
write_csv(all_quotes, "3. Qualitative Exploration/Data/best_quotes_combined.csv")


#-----------------#
unique(all_quotes$CommonTheme)

library(jsonlite)


# Create separate datasets for all themes

# "Place Identity & Atmosphere"
place_identity_atmosphere <- all_quotes %>% filter(CommonTheme == "Place Identity & Atmosphere") %>% 
        select(-CommonTheme)
write_json(place_identity_atmosphere, "3. Qualitative Exploration/Data/themes_rep_docs/place_identity_atmosphere.json")

# Shopping & Markets
shopping_markets <- all_quotes %>% filter(CommonTheme == "Shopping & Markets") %>%
        select(-CommonTheme)
#view(shopping_markets)
# Save in json fromat
write_json(shopping_markets, "3. Qualitative Exploration/Data/themes_rep_docs/shopping_markets.json")

# Walking Experience & Accessibility
walking_accessibility <- all_quotes %>% filter(CommonTheme == unique(all_quotes$CommonTheme)[5]) %>% 
        select(-CommonTheme)
write_json(walking_accessibility, "3. Qualitative Exploration/Data/themes_rep_docs/walking_accessibility.json")

# "Heritage, History & Identity" 
heritage_history_identity <- all_quotes %>% filter(CommonTheme == unique(all_quotes$CommonTheme)[4]) %>% 
        select(-CommonTheme)
write_json(heritage_history_identity, "3. Qualitative Exploration/Data/themes_rep_docs/heritage_history_identity.json")

# Tourism Actitvities & Events
tourism_activities_events <- all_quotes %>% filter(CommonTheme == unique(all_quotes$CommonTheme)[6]) %>% 
        select(-CommonTheme)
write_json(tourism_activities_events, "3. Qualitative Exploration/Data/themes_rep_docs/tourism_activities_events.json")

# Tourist Pressure & Overcrowding
tourist_pressure_overcrowding <- all_quotes %>% filter(CommonTheme == unique(all_quotes$CommonTheme)[7]) %>% 
        select(-CommonTheme)
write_json(tourist_pressure_overcrowding, "3. Qualitative Exploration/Data/themes_rep_docs/tourist_pressure_overcrowding.json")

# "Food & Culinary Experience"
place_culinary_exp <- all_quotes %>% filter(CommonTheme == unique(all_quotes$CommonTheme)[2]) %>% 
        select(-CommonTheme)
write_json(place_culinary_exp, "3. Qualitative Exploration/Data/themes_rep_docs/food_culinary_experience.json")
