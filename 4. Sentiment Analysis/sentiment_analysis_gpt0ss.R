
#-------------- ---------- Sentiment Analysis -------------------------------#

library(tidyverse)


# Import Labeled Sample
labeled_samples <- read_csv("Scripts/6. Sentiment Analysis/Data/sentiment_labeled_3labels.csv")
#Total = 351 + 127 = 478
dim(labeled_samples)
#view(labeled_samples)
# Bifurcate by Source
shimla_sample <- labeled_samples %>% filter(source == "shimla") %>% # n = 351
        select(doc_id, raw_text, human_label) %>% 
        rename(sentiment_label = human_label)

gangtok_sample <- labeled_samples %>% filter(source == "gangtok") %>% # n = 127
        select(doc_id, raw_text, human_label) %>% 
        rename(sentiment_label = human_label)

# Import all data
shimla_reviews <- read_csv("Data/Shimla/shimla_reviews.csv") # n = 3652
gangtok_reviews <- read_csv("Data/MG Road Gangtok/gangtok_reviews.csv") # n = 1360

# Remove sampled already labeled
shimla_reviews <- shimla_reviews %>% 
        filter(!doc_id %in% shimla_sample$doc_id) # n = 3301
gangtok_reviews <- gangtok_reviews %>% 
        filter(!doc_id %in% gangtok_sample$doc_id) # n = 1233

# Keep only required columns
shimla_reviews_sentiment <- shimla_reviews %>% select(doc_id, Main_Text) 
gangtok_reviews_sentiment <- gangtok_reviews %>% select(doc_id, raw_text)


#---------------------------- LLM Labeling with Ollamar -------------------------------#
# Batch LLM labeling script for Shimla & Gangtok
# Paste this into your script after you build few_shot_prompt & escape_text as in your code.

library(tidyverse)
library(glue)
library(ollamar)
library(progressr)
library(jsonlite)
library(stringr)
library(purrr)

test_connection()
list_models()

# --------- CONFIG ----------
batch_size <- 32       # tune: 8, 16, 32
max_retries <- 3
retry_wait <- 2        # seconds (exponential backoff used)
model_name <- "gpt-oss:20b"
print_responses <- TRUE    # set TRUE to print raw responses to console
# ---------------------------

# Safety: require few_shot_prompt and escape_text exist; otherwise stop
if (!exists("few_shot_prompt")) stop("few_shot_prompt not found. Please define it as in your earlier code.")
if (!exists("escape_text")) {
        escape_text <- function(x) {
                x <- gsub("\\\\", "\\\\\\\\", x)
                x <- gsub("\n", " ", x); x <- gsub("\r", " ", x)
                x <- gsub("'", "\\\\'", x); x <- gsub('"', '\\"', x)
                trimws(as.character(x))
        }
}

# Helper: build batch prompt with explicit JSON markers
build_batch_prompt_safe <- function(few_shot_prompt, batch_df) {
        reviews_lines <- map2_chr(as.character(batch_df$doc_id), batch_df$raw_text, function(id, txt) {
                safe <- escape_text(txt)
                paste0("ID: ", id, "\nReview: \"", safe, "\"")
        })
        reviews_block <- paste(reviews_lines, collapse = "\n\n")
        
        instruction <- "
Now classify each review above on the 3-level scale (Positive, Neutral, Negative).
Return ONLY a single JSON array between the markers JSON_START and JSON_END.
The array must contain objects with keys: doc_id (string) and label (one of Positive, Neutral, Negative).
Do NOT return any other text before JSON_START or after JSON_END.
Example:
JSON_START
[{\"doc_id\":\"1\",\"label\":\"Positive\"}, {\"doc_id\":\"2\",\"label\":\"Neutral\"}]
JSON_END
"
        
        paste0(few_shot_prompt, "\n\nReviews:\n\n", reviews_block, "\n\n", instruction)
}

# General batch runner: takes a tibble with doc_id & raw_text (and optionally human label) and returns annotated tibble
run_label_batches <- function(data_df, prefix = "dataset", save_basename = "labels") {
        stopifnot(all(c("doc_id", "raw_text") %in% names(data_df)))
        # build batches
        batches <- data_df %>%
                mutate(.idx = row_number()) %>%
                group_by(batch = ((.idx - 1) %/% batch_size) + 1) %>%
                group_split()
        
        handlers(global = TRUE)
        all_results <- list()
        raw_responses <- vector("list", length(batches))
        
        labeled_tbl <- with_progress({
                p <- progressor(along = batches)
                for (i in seq_along(batches)) {
                        batch_df <- batches[[i]] %>% select(doc_id, raw_text)
                        prompt_text <- build_batch_prompt_safe(few_shot_prompt, batch_df)
                        messages <- create_messages(create_message(prompt_text, role = "system"))
                        
                        resp_text <- NULL
                        attempt <- 1
                        # exponential backoff retries
                        while (attempt <= max_retries) {
                                try({
                                        resp_text <- chat(model_name, messages, output = "text", stream = FALSE)
                                }, silent = TRUE)
                                if (!is.null(resp_text) && nzchar(resp_text)) break
                                Sys.sleep(retry_wait * attempt)
                                attempt <- attempt + 1
                        }
                        if (is.null(resp_text)) resp_text <- ""
                        
                        # Save raw response to file
                        raw_file <- file.path(save_raw_dir, paste0(prefix, "_batch_", i, ".txt"))
                        writeLines(resp_text, raw_file)
                        raw_responses[[i]] <- raw_file
                        
                        if (print_responses) {
                                cat("\n----", prefix, "batch", i, "raw (first 500 chars) ----\n")
                                cat(substr(resp_text, 1, 500), "\n")
                                cat("---- end ----\n")
                        }
                        
                        # ---------------- Parsing fallbacks ----------------
                        parsed_df <- NULL
                        
                        # 1) Strict JSON_START / JSON_END extraction
                        if (str_detect(resp_text, "JSON_START")) {
                                jstart <- str_locate(resp_text, "JSON_START")[1,2] + 1
                                jend_pos <- str_locate(resp_text, "JSON_END")
                                if (!any(is.na(jend_pos))) {
                                        jend <- jend_pos[1,1] - 1
                                        json_str <- str_sub(resp_text, jstart, jend) %>% str_trim()
                                        try({ parsed_df <- as_tibble(fromJSON(json_str, simplifyVector = TRUE)) }, silent = TRUE)
                                }
                        }
                        
                        # 2) Looser: first JSON array in the text
                        if (is.null(parsed_df) || nrow(parsed_df) == 0) {
                                json_start <- regexpr("\\[\\s*\\{", resp_text)
                                if (json_start[1] != -1) {
                                        json_str2 <- substr(resp_text, json_start[1], nchar(resp_text))
                                        br_positions <- gregexpr("\\]", json_str2)[[1]]
                                        if (length(br_positions) && br_positions[1] != -1) {
                                                last_br <- max(br_positions)
                                                json_str2 <- substr(json_str2, 1, last_br)
                                                try({ parsed_df <- as_tibble(fromJSON(json_str2, simplifyVector = TRUE)) }, silent = TRUE)
                                        }
                                }
                        }
                        
                        # 3) Regex match repeated {"doc_id":...,"label":...}
                        if (is.null(parsed_df) || nrow(parsed_df) == 0) {
                                pairs <- str_match_all(resp_text,
                                                       "\\{\\s*\"?doc_id\"?\\s*[:=]\\s*\"?([^\",}]+)\"?\\s*,\\s*\"?label\"?\\s*[:=]\\s*\"?([^\"}]+)\"?\\s*\\}")
                                if (length(pairs[[1]]) > 0) {
                                        df <- tibble(doc_id = pairs[[1]][,2], label = pairs[[1]][,3])
                                        parsed_df <- df %>% rename(llm_label = label)
                                }
                        }
                        
                        # 4) Proximity heuristic
                        if (is.null(parsed_df) || nrow(parsed_df) == 0) {
                                found <- map2_chr(as.character(batch_df$doc_id), batch_df$raw_text, function(id, txt) {
                                        pattern <- paste0("\\b", id, "\\b")
                                        pos <- str_locate_all(resp_text, regex(pattern, ignore_case = TRUE))[[1]]
                                        lab <- NA_character_
                                        if (is.matrix(pos) && nrow(pos) > 0) {
                                                start <- max(1, pos[1,1] - 50)
                                                end <- min(nchar(resp_text), pos[1,2] + 200)
                                                window <- str_sub(resp_text, start, end)
                                                lab_match <- str_match(window, "(Positive|Neutral|Negative)")
                                                if (!is.na(lab_match[1,2])) lab <- lab_match[1,2]
                                        }
                                        lab
                                })
                                if (any(!is.na(found))) {
                                        parsed_df <- tibble(doc_id = as.character(batch_df$doc_id), llm_label = found)
                                        parsed_df <- parsed_df %>% filter(!is.na(llm_label))
                                }
                        }
                        
                        # 5) Sequential label extraction fallback
                        if (is.null(parsed_df) || nrow(parsed_df) == 0) {
                                labels_seq <- str_extract_all(resp_text, "\\b(Positive|Neutral|Negative)\\b")[[1]]
                                if (length(labels_seq) >= 1) {
                                        n_assign <- min(length(labels_seq), nrow(batch_df))
                                        parsed_df <- tibble(doc_id = as.character(batch_df$doc_id)[1:n_assign], llm_label = labels_seq[1:n_assign])
                                }
                        }
                        
                        # Finalize batch result
                        if (is.null(parsed_df) || nrow(parsed_df) == 0) {
                                batch_res <- tibble(doc_id = as.character(batch_df$doc_id), llm_label = NA_character_)
                        } else {
                                if ("label" %in% names(parsed_df)) parsed_df <- parsed_df %>% rename(llm_label = label)
                                parsed_df <- parsed_df %>% mutate(doc_id = as.character(doc_id), llm_label = as.character(llm_label))
                                parsed_df <- parsed_df %>% group_by(doc_id) %>% slice_head(n = 1) %>% ungroup()
                                missing <- setdiff(as.character(batch_df$doc_id), parsed_df$doc_id)
                                if (length(missing) > 0) {
                                        parsed_df <- bind_rows(parsed_df, tibble(doc_id = missing, llm_label = NA_character_))
                                }
                                batch_res <- parsed_df %>% select(doc_id, llm_label)
                        }
                        
                        # Attach batch index + raw file reference
                        all_results[[i]] <- batch_res %>% mutate(batch = i, raw_file = raw_file)
                        p()
                } # end for batches
                
                bind_rows(all_results) %>%
                        left_join(data_df %>% select(doc_id, everything()), by = "doc_id")
        }) # end with_progress
        
        # Save summary CSV for this run
        out_csv <- file.path(dirname(save_raw_dir), paste0(save_basename, "_", prefix, ".csv"))
        write_csv(labeled_tbl, out_csv)
        
        message(glue("Saved labeled table for {prefix} to: {out_csv}"))
        message(glue("Raw responses saved in: {save_raw_dir} (files named {prefix}_batch_<i>.txt)"))
        return(labeled_tbl)
}

# -------------------- RUN for Shimla & Gangtok --------------------

# Prepare Shimla and Gangtok dataframes (match your earlier names)
# Expect shimla_reviews_sentiment has columns doc_id & Main_Text
shimla_input <- shimla_reviews_sentiment %>% transmute(doc_id = as.character(doc_id), raw_text = as.character(Main_Text))
gangtok_input <- gangtok_reviews_sentiment %>% transmute(doc_id = as.character(doc_id), raw_text = as.character(raw_text))

# Run (these will create CSV outputs and raw files)
shimla_labeled <- run_label_batches(shimla_input, prefix = "shimla", save_basename = "ollamar_batch_labels")
gangtok_labeled <- run_label_batches(gangtok_input, prefix = "gangtok", save_basename = "ollamar_batch_labels")

# Quick checks
cat("Shimla label counts:\n"); print(table(shimla_labeled$llm_label, useNA = "always"))
cat("Gangtok label counts:\n"); print(table(gangtok_labeled$llm_label, useNA = "always"))

table(shimla_labeled$llm_label)
unique(shimla_labeled$llm_label)

unique(gangtok_labeled$llm_label)
table(gangtok_labeled$llm_label)

#view

shimla_reviews_sentiment_labeled <- shimla_reviews_sentiment %>%
        left_join(shimla_labeled %>% select(doc_id, llm_label), by = "doc_id") 
dim(shimla_reviews_sentiment)
dim(shimla_reviews_sentiment_labeled)
table(shimla_reviews_sentiment_labeled$llm_label)

#-------------------------- Fix NA Values if any ----------------------------#
# Filter NA if needed
shimla_labeled_na <- shimla_reviews_sentiment_labeled %>% filter(is.na(llm_label))
# Run again
shimla_na_labeled <- run_label_batches(
        shimla_labeled_na %>%  
                transmute(doc_id = as.character(doc_id),
                          raw_text = as.character(Main_Text)),
        prefix = "shimla_na", save_basename = "ollamar_batch_labels")

# Check

# Replace values in original
shimla_reviews_sentiment_labeled <- shimla_reviews_sentiment_labeled %>%
        rows_update(shimla_na_labeled %>% select(doc_id, llm_label), by = "doc_id")

unique(shimla_reviews_sentiment_labeled$llm_label) # Make sure no more NA

# Join Shimla Sample
shimla_reviews_sentiment_labeled <- bind_rows(
        shimla_reviews_sentiment_labeled %>% select(doc_id, raw_text = Main_Text, llm_label),
        shimla_sample %>% select(doc_id, raw_text, sentiment_label) %>% rename(llm_label = sentiment_label)
)
dim(shimla_reviews_sentiment_labeled) # Should be 3652 now

# Save
write_csv(shimla_reviews_sentiment_labeled, "Scripts/6. Sentiment Analysis/Data/final_sentiments/shimla_gpt_oss20b.csv")


#gangtok
gangtok_reviews_sentiment_labeled <- gangtok_reviews_sentiment %>%
        left_join(gangtok_labeled %>% select(doc_id, llm_label), by = "doc_id")
# Filter if any NA
gangtok_labeled_na <- gangtok_reviews_sentiment_labeled %>% filter(is.na(llm_label))
# Run again
gangtok_na_labeled <- run_label_batches(
        gangtok_labeled_na %>%  
                transmute(doc_id = as.character(doc_id),
                          raw_text = as.character(raw_text)),
        prefix = "gangtok_na", save_basename = "ollamar_batch_labels")
# Replace Values in Original
gangtok_reviews_sentiment_labeled <- gangtok_reviews_sentiment_labeled %>%
        rows_update(gangtok_na_labeled %>% select(doc_id, llm_label), by = "doc_id")

unique(gangtok_reviews_sentiment_labeled$llm_label) # Make sure no more NA
dim(gangtok_reviews_sentiment_labeled) # Should be 1233 now
# Join Gangtok Sample
gangtok_reviews_sentiment_labeled <- bind_rows(
        gangtok_reviews_sentiment_labeled %>% select(doc_id, raw_text, llm_label),
        gangtok_sample %>% select(doc_id, raw_text, sentiment_label) %>% rename(llm_label = sentiment_label)
)
dim(gangtok_reviews_sentiment_labeled) # Should be 1360 now

# Save
write_csv(gangtok_reviews_sentiment_labeled, "Scripts/6. Sentiment Analysis/Data/final_sentiments/gangtok_gpt_oss20b.csv")
