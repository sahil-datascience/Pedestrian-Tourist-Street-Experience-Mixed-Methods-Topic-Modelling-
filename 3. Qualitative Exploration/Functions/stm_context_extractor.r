#' Robust STM Topic Extractor
#'
#' Extracts topic keywords (prob, frex, lift, score) and representative documents
#' for each topic from an stm model, with a single call to labelTopics and
#' robust fallbacks using findThoughts and theta-based selection.
#'
#' @param stm_model An fitted `stm` model object.
#' @param review_df A data.frame containing at least a character column `Main_Text`.
#' @param n_topics Integer: number of topics to extract (default: 73).
#' @param n_keywords Integer: number of keywords to extract per topic (default: 15).
#' @param n_docs Integer: number of representative documents per topic (default: 5).
#' @param csv_out Optional path to write a CSV file. If NULL, no file is written.
#' @param verbose Logical: print progress messages (default TRUE).
#' @return A data.frame with one row per (Topic x RepDocRank) containing keywords and texts.
#' @examples
#' \dontrun{
#' library(stm)
#' # assume stm_model and review_df exist
#' df <- stm_topic_extractor(stm_model, review_df, n_topics = 20, csv_out = "topics.csv")
#' }
#' @importFrom stm labelTopics findThoughts
#' @importFrom pbapply pblapply
#' @importFrom dplyr %>% mutate across everything
#' @importFrom stringr str_trim str_squish
#' @importFrom readr write_csv
#' @export
stm_context_extractor <- function(
  stm_model,
  review_df,
  n_topics = 73,
  n_keywords = 15,
  n_docs = 5,
  csv_out = NULL,
  verbose = TRUE
){
  # ---- sanity checks ----
  if (missing(stm_model) || is.null(stm_model)) stop("stm_model is required.")
  if (missing(review_df) || !("Main_Text" %in% names(review_df))) stop("review_df with column 'Main_Text' is required.")
  if (!is.character(review_df$Main_Text)) stop("review_df$Main_Text must be a character vector.")
  if (!requireNamespace("stm", quietly = TRUE)) stop("Package 'stm' is required but not installed.")

  if (verbose) message("Calling labelTopics() once for all topics...")
  lab_all <- tryCatch(stm::labelTopics(stm_model, n = n_keywords), error = function(e) NULL)
  if (is.null(lab_all)) stop("labelTopics failed on stm_model. Check model object.")

  get_lab_row <- function(lab_obj, field, k, n = n_keywords) {
    if (is.null(lab_obj) || is.null(lab_obj[[field]])) return(character(0))
    x <- lab_obj[[field]]
    if (is.matrix(x) && nrow(x) >= k) {
      out <- as.character(x[k, , drop = TRUE])
    } else if (is.list(x) && length(x) >= k && is.character(x[[k]])) {
      out <- as.character(x[[k]])
    } else if (is.character(x) && length(x) >= n) {
      out <- as.character(x[1:min(length(x), n)])
    } else {
      out <- character(0)
    }
    out <- out[!is.na(out)]
    out <- stringr::str_trim(out)
    out[out != ""] %>% head(n)
  }

  if (verbose) message("Extracting per-topic keywords and representative docs...")
  topic_rows <- pbapply::pblapply(seq_len(n_topics), function(k) {
    prob_kw  <- get_lab_row(lab_all, "prob",  k, n_keywords)
    frex_kw  <- get_lab_row(lab_all, "frex",  k, n_keywords)
    lift_kw  <- get_lab_row(lab_all, "lift",  k, n_keywords)
    score_kw <- get_lab_row(lab_all, "score", k, n_keywords)

    th <- tryCatch(stm::findThoughts(stm_model, texts = review_df$Main_Text, topics = k, n = n_docs), error = function(e) NULL)
    docs <- character(0)
    if (!is.null(th)) {
      if (!is.null(th$docs) && length(th$docs) >= 1 && is.character(th$docs[[1]])) {
        docs <- th$docs[[1]]
      } else if (is.list(th) && length(th) >= 1 && is.character(th[[1]])) {
        docs <- th[[1]]
      } else if (is.character(th)) {
        docs <- th
      }
    }

    # fallback: top documents by theta
    if (length(docs) < n_docs && !is.null(stm_model$theta) && ncol(stm_model$theta) >= k) {
      idx <- order(stm_model$theta[, k], decreasing = TRUE)
      chosen_idx <- idx[1:min(length(idx), n_docs)]
      docs_by_theta <- review_df$Main_Text[chosen_idx]
      docs_unique <- unique(c(docs, docs_by_theta))
      docs <- docs_unique[1:min(length(docs_unique), n_docs)]
    }

    if (length(docs) < n_docs) docs <- c(docs, rep("", n_docs - length(docs)))
    if (length(docs) > n_docs) docs <- docs[1:n_docs]

    d <- data.frame(
      TopicID = as.character(k),
      RepDocRank = seq_len(n_docs),
      RepDocText = docs,
      ProbKeywords = paste(prob_kw, collapse = ", "),
      FREXKeywords = paste(frex_kw, collapse = ", "),
      LiftKeywords = paste(lift_kw, collapse = ", "),
      ScoreKeywords = paste(score_kw, collapse = ", "),
      stringsAsFactors = FALSE
    )
    d
  }, cl = NULL)

  topic_df <- do.call(rbind, topic_rows)

  # tidy up - convert NAs to empty strings and squish keyword fields
  topic_df[] <- lapply(topic_df, function(x) ifelse(is.na(x), "", as.character(x)))
  topic_df$ProbKeywords    <- stringr::str_squish(topic_df$ProbKeywords)
  topic_df$FREXKeywords    <- stringr::str_squish(topic_df$FREXKeywords)
  topic_df$LiftKeywords    <- stringr::str_squish(topic_df$LiftKeywords)
  topic_df$ScoreKeywords   <- stringr::str_squish(topic_df$ScoreKeywords)

  if (!is.null(csv_out)) {
    readr::write_csv(topic_df, csv_out)
    if (verbose) message("Saved CSV: ", normalizePath(csv_out))
  }

  return(topic_df)
}

# End of file
