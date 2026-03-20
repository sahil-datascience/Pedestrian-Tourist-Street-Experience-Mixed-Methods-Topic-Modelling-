
# Export negative reviews for qualitative coding
library(tidyverse)

# Read Data
gangtok_sentiments <- read_csv("4. Sentiment Analysis/Data/final_sentiments/gangtok_gpt_oss20b.csv") %>%
  filter(llm_label == "Negative")
shimla_sentiments <- read_csv("4. Sentiment Analysis/Data/final_sentiments/shimla_gpt_oss20b.csv") %>%
  filter(llm_label == "Negative")

# Export negative reviews as xlsx files
library(writexl)

# Path
folder_path <- file.path("4. Sentiment Analysis/Data/Qualitative Analysis")

# Export Gangtok Negative Reviews
write_xlsx(gangtok_sentiments, path = file.path(folder_path, "gangtok_negative_reviews.xlsx"))
# Export Shimla Negative Reviews
write_xlsx(shimla_sentiments, path = file.path(folder_path, "shimla_negative_reviews.xlsx"))

######################### Topic-Stratified Sampling for Qualitative Coding #########################

# Load Data
shimla <- readRDS("Data/r_data/shimla_review_topic_sentiments.rds") %>%
        mutate(site = "Shimla") %>% filter(llm_label == "Negative") 
gangtok <- readRDS("Data/r_data/gangtok_topic_sentiment_data.rds") %>%
        mutate(site = "Gangtok") %>% filter(llm_label == "Negative")



library(dplyr)
library(tidyr)

#####
#### Gangtok Negative Reviews - Topic Analysis
#####

# 1. Identify which columns are Topic columns (Topic_1 to Topic_14)
topic_cols <- grep("^Topic_", names(gangtok), value = TRUE)

# 2. Calculate the total weight for each topic to find the "predominant" ones
topic_summary <- gangtok %>%
  summarise(across(all_of(topic_cols), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Topic", values_to = "Total_Weight") %>%
  arrange(desc(Total_Weight))

# Get the names of the top 5 topics
top_5_topics <- head(topic_summary$Topic, 5)

print("The top 5 predominant topics are:")
print(top_5_topics)

# 3. Create a function or a loop to see the top 3 documents for each top topic
# This helps you understand the context of the negativity.
top_documents_analysis <- lapply(top_5_topics, function(topic_name) {
  gangtok %>%
    select(doc_id, raw_text, !!sym(topic_name)) %>%
    arrange(desc(!!sym(topic_name))) %>%
    head(20) %>% # Change Value here
    mutate(Topic_Identified = topic_name)
})

# Combine into one readable table
analysis_results <- bind_rows(top_documents_analysis)

# View the results
print(analysis_results)

library(jsonlite)

# Function to transform data frame to JSON
export_to_json <- function(data, file_name = "output.json", save_to_disk = TRUE) {
        
        # Convert the data frame to a JSON string
        # 'pretty = TRUE' adds indentation and line breaks
        json_data <- jsonlite::toJSON(data, pretty = TRUE)
        
        if (save_to_disk) {
                # Write the string to a file
                write(json_data, file_name)
                message(paste("Success: Data exported to", file_name))
        }
        
        # Return the JSON string in case you want to use it in the R console
        return(json_data)
}

# Example Usage:
# Using the 'analysis_results' table we created in the previous step
json_output <- export_to_json(analysis_results, "top_negative_topics.json")


#####
#### Shimla Negative Reviews - Topic Analysis
#####

# 1. Identify which columns are Topic columns (Topic_1 to Topic_21)
topic_cols_shimla <- grep("^Topic_", names(shimla), value = TRUE)
# 2. Calculate the total weight for each topic to find the "predominant" ones
topic_summary_shimla <- shimla %>%
  summarise(across(all_of(topic_cols_shimla), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Topic", values_to = "Total_Weight") %>%
  arrange(desc(Total_Weight))
# Get the names of the top 5 topics
top_5_topics_shimla <- head(topic_summary_shimla$Topic, 5)
print("The top 5 predominant topics in Shimla are:")

print(top_5_topics_shimla)
# 3. Create a function or a loop to see the top 3 documents for each top topic
# This helps you understand the context of the negativity.
top_documents_analysis_shimla <- lapply(top_5_topics_shimla, function(topic_name) {
  shimla %>%
    select(doc_id, Main_Text, !!sym(topic_name)) %>%
    arrange(desc(!!sym(topic_name))) %>%
    head(20) %>% # Change Value here
    mutate(Topic_Identified = topic_name)
})
# Combine into one readable table
analysis_results_shimla <- bind_rows(top_documents_analysis_shimla)
# View the results
print(analysis_results_shimla)
# Export to JSON
json_output_shimla <- export_to_json(analysis_results_shimla, "top_negative_topics_shimla.json")
