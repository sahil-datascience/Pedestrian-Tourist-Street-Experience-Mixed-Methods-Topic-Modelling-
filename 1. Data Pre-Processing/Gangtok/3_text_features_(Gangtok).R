

# Text Pre-Processing

##############################################################
################# Main Text Variable #########################
##############################################################


######################################################################
########## Exploratory Data Analysis of Textual Content ##############
######################################################################

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(textstem) 
library(spacyr)

############################## Get Data ##############################
# Load all reviews
gangtok_reviews <- read_csv("Data/tidy/gangtok_reviews_2.csv")

gangtok_reviews

########## Create a corpus from the reviews ############################

corp <- corpus(gangtok_reviews, text_field = "raw_text")

tok <- quanteda::tokens(corp,
                        remove_url     = TRUE,    # http, www
                        remove_separators = TRUE, # extra spaces / tabs
                        remove_punct   = TRUE,    # [[:punct:]]
                        remove_numbers = TRUE,    # \d+
                        remove_symbols = TRUE,    # #, @, < >, emojis, etc.
                        split_hyphens  = TRUE) %>% # split hyphenated words 
        tokens_tolower()  # Convert to lower case

# Handles Tokesn with dot in the middle (e.g, "price.there")
tok <- tokens_replace(tok, pattern = "\\.", replacement = " ", valuetype = "regex")

# Remove tokens involving numbers
tok <- tokens_remove(tok, pattern = "\\d+", valuetype = "regex")

# Remove # and @ mentions
tok <- tokens_remove(tok, pattern = c("#", "@"), valuetype = "fixed")

##################### POS Tagging ##########################

# Convert tokens to a corpus
char_vec <- sapply(tok, function(x) paste(x, collapse = " "))

corpus <- corpus(char_vec)


spacy_initialize(model = "en_core_web_sm")

# POS tagging
pos_tags <- spacy_parse(corpus, pos = TRUE,
                        tag = TRUE, entity = TRUE)

##### Explore POS Tags #####
# Plot top 10 lemma in each pos tag
pos_data <- pos_tags %>% 
        filter(!is.na(pos)) %>% 
        select(doc_id, lemma, pos) %>% 
        mutate(pos = as.factor(pos))

# Frequency of lemmas
lemma_freq <- pos_data %>% 
        group_by(lemma, pos) %>% 
        summarise(count = n()) %>% 
        arrange(desc(count))
# View top 10 lemmas in each POS tag
lemma_top10 <- lemma_freq %>% 
        group_by(pos) %>% 
        top_n(10, count) %>% 
        arrange(pos, desc(count))
# View the top 10 lemmas in each POS tag
lemma_top10
# Plot top 10 lemmas in each POS tag
lemma_top10 %>% 
        ggplot(aes(x = reorder(lemma, count), y = count, fill = pos)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ pos, scales = "free_y") +
        coord_flip() +
        labs(title = "Top 10 Lemmas in Each POS Tag", x = "Lemma", y = "Count") +
        theme_minimal()


# Which POS tags to preserve?
# Keep only NOUN, PROPN, VERB, ADJ
# Optional: ADV
# Remove: DET, PRON, AUX, CCONJ, SCONJ, PUNCT, SYM, NUM, etc.



filtered_pos <- pos_tags %>% 
        filter(pos %in% c("NOUN", "PROPN", "VERB", "ADJ"))

# 
paste("Total terms before filtering:", dim(pos_tags)[1])
paste("Terms removed:",dim(pos_tags)[1] - dim(filtered_pos)[1])
paste("Remaining:", dim(filtered_pos)[1])
paste("Unique terms after filtering:", length(unique(filtered_pos$token)))


# Back to data frame
pos_filtered_text <- filtered_pos %>% 
        group_by(doc_id) %>% 
        summarize(pos_filtered_text = paste(token, collapse = " "))

# Mutate with reviews by doc id
gangtok_reviews <- gangtok_reviews %>%
        right_join(pos_filtered_text, by = "doc_id")
gangtok_reviews



#-------- -----------------#
# Create corpus
corpus_from_filtered_pos <- corpus(gangtok_reviews, text_field = "pos_filtered_text")


# Convert corpus to tokens
tok <- quanteda::tokens(corpus_from_filtered_pos)

########## Lemmatisation [not stemming] ##########################

tok <- as.tokens(lapply(tok, lemmatize_words))

########## Remove Stopwords ##########################

tok <- tokens_remove(tok, pattern = stopwords("english"))




############### Advanced Cleaning: Remove Domain Specific Stopwords ##########################

# Top Words
top_words <- textstat_frequency(dfm(tok), n = 100)


#domain_stopwords <- c("mall", "shimla")

#tok <- tokens_remove(tok, pattern =  c(domain_stopwords))


########### Idenitfy Collocations ###################################

collocs <- textstat_collocations(tok, size = 2:3, min_count = 5)

# View collocations
#view(collocs)     # Sort Variable to See Quality

filtered_collocs <- collocs %>% 
        filter(count > 4, count_nested > 4, lambda > 2, z > 1 )
#view(filtered_collocs)

unique(filtered_collocs$collocation)


# Keep only high association scores

remove_collocs <-  c("station india", "love one" ,"hang even",
                     "late even", "get kind", "road know",
                     "road various", "r gangtok",  "even light"
)


# Filter collocations with lambda > 4 and not in generic_collocs
collocs_keep <- filtered_collocs %>% 
        filter(lambda > 4 & !(collocation %in% c(remove_collocs)))


#collocs_keep$collocation

#view(collocs_keep)

# Example: merge similar collocations
merge_map <- c(
        "mg marg" = "mg road",
        "g marg" = "mg road",
        "g road" = "mg road",
        "gandhi marg" = "mg road",
        "gandhi road" = "mg road",
        "family friend" = "friend family",
        "lal bazar" = "lal market",
        
        
        ""
)

# Apply replacement
collocs_keep$collocation <- plyr::mapvalues(
        collocs_keep$collocation,
        from = names(merge_map),
        to = merge_map,
        warn_missing = FALSE
)



#collocs_keep$collocation
#view(collocs_keep)

# Compound them into single tokens
tok <- tokens_compound(tok, pattern = collocs_keep)


# Save tokens
saveRDS(tok, "Data/r_data/gangtok_tokens.rds")



# convert token back into character vector, doc id
data_frame_tokens <- data.frame(doc_id = names(tok),
                                token_filter_text = sapply(tok, function(x) paste(x, collapse = " ")),
                                stringsAsFactors = FALSE)

# Mutate with reviews by doc id
reviews <- gangtok_reviews %>%
        right_join(data_frame_tokens, by = "doc_id")


# save
write_csv(reviews, "Data/tidy/gangtok_reviews_3.csv")

# View final reviews data
#reviews %>% select(doc_id, Main_Text, pos_filtered_text, token_filter_text) %>% # filter empty row
#        filter(nchar(token_filter_text) > 0) 

######################################################################
###################### Feature Engineering ###########################
######################################################################

#### Document Feature Matrix (DFM) ####################
# Load necessary libraries
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(stm)


############################## Get Data ##############################
# Load tokens
tokens <- readRDS("Data/r_data/gangtok_tokens.rds")


##############################################################
# Create Document Feature Matrix (DFM)
dfm <- dfm(tokens)  
dfm


#################### Iterative Cleaning #########################
# Remove sparse terms
dfm <- dfm_trim(dfm, min_termfreq = 2, termfreq_type = "count")
# Remove terms that appear in less than 10 documents
dfm <- dfm_trim(dfm, min_docfreq = 1, docfreq_type = "count")
# Remove terms that appear in more than 50% of the documents
dfm <- dfm_trim(dfm, max_docfreq = 0.95, docfreq_type = "prop")


# Remove terms that are too short
dfm <- dfm_keep(dfm, min_nchar = 3) 

# Remove terms that are too long
dfm <- dfm_keep(dfm, max_nchar = 15)

dfm


################### Exploratory Analysis ######################
# Explore Tokens
# Frequency
top_20 <- textstat_frequency(dfm, n = 50)  
top_20


last_20 <- textstat_frequency(dfm) %>% arrange(desc(rank)) 
head(last_20, 25)
#################################################################

# Remove Specific Tokens

#oo_frequent <- c("place", "shop", "road", "good", "walk", "visit",
#                  "nice", "ridge", "view", "time", "enjoy", "lot", 
#                  "great", "see", "take", "even", "get", "many",
#                  "much")


#misspled_tokens <- c("atma", "bcz", "rds", "lyk", "abt", # Optional: Correct Spellings
#                     "luv", "spl")



#dfm <- dfm_remove(dfm, pattern = c(too_frequent, misspled_tokens))

#################################################################


# Save the document-feature matrix
saveRDS(dfm, "Data/r_data/gangtok_dfm.rds")

# Read the document-feature matrix
dfm <- readRDS("Data/r_data/gangtok_dfm.rds")

dim(dfm)



################ Save this upadated text structure to original data frame #####################

# convert dfm and add to reviews

mat <- as.matrix(dfm)

dim(mat)

# Reconstruct text
reconstructed <- apply(mat, 1, function(x) {
        words <- rep(colnames(mat), times = x)
        paste(words, collapse = " ")
})

# create data frame two colums, doc_id and text
reconstructed <- data.frame(doc_id = rownames(mat), dfm_text = reconstructed,
                            stringsAsFactors = FALSE)
dim(reconstructed)
names(reconstructed)
head(reconstructed)

# read reviews
gangtok_reviews <- read_csv("Data/tidy/gangtok_reviews_3.csv")

# mutate reconstructed to reviews by doc_id
gangtok_reviews <- gangtok_reviews %>% right_join(reconstructed, by = "doc_id")

gangtok_reviews
# export
write_csv(gangtok_reviews, "Data/tidy/gangtok_reviews_3.csv")


dim(gangtok_reviews)
head(gangtok_reviews)

gangtok_reviews %>% select(doc_id, raw_text, token_filter_text, dfm_text) %>% view()

# Observations
# - I have removed very less features, no matter how frequent and general they are
#   for tourism experience they may be important.



