
# Load libraries
library(tidyverse)
library(stm)

# ## Load Data

# read metadata
review_df <- read_csv("Data/tidy/shimla_reviews_3.csv")

#out
# read document feature matrix
dfm <- readRDS("Data/r_data/shimla_dfm.rds")


# Quick Pre-Processing
# Filter empty or too small documents
dfm_empty <- dfm[rowSums(dfm) < 3, ] 
dfm_empty

# Remove dfm_empty from dfm
dfm <- dfm[rowSums(dfm) >= 3, ]

# Remove dfm_empty from reviews
review_df <- review_df %>% filter(!doc_id %in% rownames(dfm_empty))

# Add Visitor Type for Meta Data
review_df <- review_df %>% 
        mutate(visitor_type = ifelse(Countries == "India", "Domestic", "International"),
               year = lubridate::year(Date))
# Missing as "Unknown"
review_df$visitor_type <- ifelse(is.na(review_df$visitor_type), "Unknown", review_df$visitor_type)

review_df
# Start Topic Modeling

# Convert dfm to stm format
out <- convert(dfm, to = "stm")
# Save
saveRDS(out, "Data/r_data/shimla_stm_out.rds")



## Find K Method
## Find K
# Find K using seachK()
#k_set <- c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
#k_set <- c(15:35)

#searchK <- searchK(out$documents, out$vocab,
#                   K = k_set, data = review_df,
#                   seed = 12345)
#searchK
# Save searchK results
#saveRDS(searchK, "2. Quantitative Modelling/searchK_Data/Shimla_searchK_[5-80].rds")
#saveRDS(searchK, "2. Quantitative Modelling/searchK_Data/Shimla_searchK_[15-35].rds")


# Load searchK results
searchK <- readRDS("2. Quantitative Modelling/searchK_Data/Shimla_searchK_[5-80].rds")
#searchK <- readRDS(searchK, "Data/Shimla/searchK_[15-35].rds")

searchK
plot(searchK)

# Plot searchK results

#plot indicators for optimum number of topics, takes good time!  
library(ggplot2)
library(tidyr)
library(purrr)

searchK$results %>% 
        pivot_longer(
                cols = -K,
                names_to = "metric",
                values_to = "value") %>% 
        filter(metric %in% 
                       c("lbound", "exclus", 
                         "residual", "semcoh")) %>% 
        mutate(value = map_dbl(value, 1)) %>% 
        mutate(K = map_dbl(K, 1)) %>% 
        ggplot(aes(x = K,
                   y = value,
                   color = metric)) + 
        geom_point() +   
        geom_vline(xintercept = 21, linetype = 2, color = "red") +
        facet_wrap( ~ metric, scales = "free_y") +
        labs(x = "K", y = "Value", color = "Metric",
             title = "Find K Metrics for Shimla",
             subtitle = "Selected K 21 (Dotted Red Line)") + 
        theme(text = element_text("Times New Roman"),
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"))

# save the plot
ggsave("2. Quantitative Modelling/searchK_Data/shimla_searchK_plot.svg", width = 10, height = 6)

#### Candidate K values: 20, 21
# Fit STM model
set.seed(12345)
names(review_df)


stm_model_21 <- stm(
        documents = out$documents,
        vocab = out$vocab,
        data = review_df,
        K = 21)
plot(stm_model_21, main = "STM Model for Shimla (K 21)")

# Save the model
saveRDS(stm_model_21, "Data/r_data/stm_model_k21.rds")




### Selected Model 21



### Find Thoughts



sentences <- findThoughts(stm_model_20, texts = review_df$Main_Text,
                          topics = 1,
                          n = 5)
sentences$docs[[1]]


   
## Topic Correlations

# Hierarchical Correlations
library(stmCorrViz)
stmCorrViz(stm_model_21, file_out = "Data/model_selection/stm_corr_viz.html",
           documents_raw = review_df$Main_Text,
           title = paste0("Topic", 1:21))



