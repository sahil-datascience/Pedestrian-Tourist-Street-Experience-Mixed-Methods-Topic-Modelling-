# build_stm_html_gradient_badge.R
# Function version of your script (keeps original structure, fixes HTML string concat bug)
context_in_HTML <- function(
    csv_in,
    html_out,
    COLOR_MAP = list(
      Prob  = c(31L, 119L, 180L),
      FREX  = c(44L, 160L, 44L),
      Lift  = c(214L, 39L, 40L),
      Score = c(255L, 127L, 14L)
    ),
    OVERLAP_FIXED_COLOR = NULL,
    FONT_FAMILY = "Times New Roman",
    FONT_SIZE_PX = 14L,
    show_badge = TRUE,
    verbose = TRUE
) {
  ## dependencies
  if (!requireNamespace("readr", quietly = TRUE)) stop("readr required")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("stringr required")
  if (!requireNamespace("pbapply", quietly = TRUE)) stop("pbapply required")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("htmltools required")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("tibble required")
  
  if (missing(csv_in) || missing(html_out)) stop("csv_in and html_out are required")
  if (!file.exists(csv_in)) stop("csv_in not found: ", csv_in)
  
  # read csv
  topic_df <- readr::read_csv(csv_in, col_types = readr::cols(.default = "c")) %>%
    dplyr::mutate(RepDocRank = ifelse("RepDocRank" %in% names(.), as.integer(RepDocRank), NA_integer_))
  
  topic_df <- topic_df %>%
    dplyr::group_by(TopicID) %>%
    dplyr::mutate(RepDocRank = ifelse(is.na(RepDocRank), dplyr::row_number(), RepDocRank)) %>%
    dplyr::ungroup()
  
  split_keywords <- function(x) {
    if (is.na(x) || x == "") return(character(0))
    parts <- stringr::str_split(x, "[,;|]")[[1]]
    parts <- stringr::str_trim(parts)
    parts <- parts[parts != ""]
    parts <- stringr::str_squish(parts)
    tolower(parts)
  }
  
  rgb_to_hex <- function(rgb) {
    sprintf("#%02x%02x%02x", as.integer(rgb[1]), as.integer(rgb[2]), as.integer(rgb[3]))
  }
  
  # find_substring_spans: includes underscore-split units (minimal change)
  find_substring_spans <- function(text, kw_by_metric) {
    spans <- NULL
    txt <- as.character(text)
    if (is.na(txt) || nchar(txt) == 0) return(tibble::tibble())
    for (metric in names(kw_by_metric)) {
      kws <- kw_by_metric[[metric]]
      if (length(kws) == 0) next
      for (kw in kws) {
        if (is.na(kw) || stringr::str_trim(kw) == "") next
        units <- c(as.character(kw))
        if (grepl("_", kw, fixed = TRUE)) {
          tokens <- unlist(stringr::str_split(as.character(kw), "_", simplify = FALSE))
          tokens <- tokens[tokens != ""]
          if (length(tokens) > 0) units <- unique(c(units, tokens))
        }
        for (unit in units) {
          esk <- stringr::str_replace_all(unit, "([\\\\^$.|?*+()\\\\[\\\\]{}\\\\\\\\])", "\\\\\\1")
          pat <- paste0("(?i)(?=(", esk, "))")
          g <- gregexpr(pat, txt, perl = TRUE)[[1]]
          if (length(g) && g[1] != -1) {
            for (s in as.integer(g)) {
              e <- s + nchar(unit) - 1
              if (e > nchar(txt)) e <- nchar(txt)
              matched_sub <- substr(txt, s, e)
              spans <- rbind(spans, data.frame(
                start = s, end = e, metric = metric,
                matched_text = matched_sub, keyword = kw, stringsAsFactors = FALSE
              ))
            }
          }
        }
      }
    }
    if (is.null(spans)) return(tibble::tibble())
    dplyr::as_tibble(spans)
  }
  
  merge_overlapping_spans <- function(spans_df) {
    if (nrow(spans_df) == 0) return(tibble::tibble())
    spans_df <- spans_df %>% dplyr::arrange(start, dplyr::desc(end - start))
    merged <- list()
    cur_s <- NULL; cur_e <- NULL; cur_metrics <- NULL; cur_subs <- NULL
    for (i in seq_len(nrow(spans_df))) {
      s <- spans_df$start[i]; e <- spans_df$end[i]
      met <- spans_df$metric[i]; sub <- spans_df$matched_text[i]
      if (is.null(cur_s)) {
        cur_s <- s; cur_e <- e; cur_metrics <- c(met); cur_subs <- c(sub)
      } else {
        if (s <= cur_e) {
          cur_e <- max(cur_e, e)
          cur_metrics <- unique(c(cur_metrics, met))
          cur_subs <- unique(c(cur_subs, sub))
        } else {
          merged[[length(merged) + 1]] <- list(start = cur_s, end = cur_e, metrics = cur_metrics, subs = cur_subs)
          cur_s <- s; cur_e <- e; cur_metrics <- c(met); cur_subs <- c(sub)
        }
      }
    }
    if (!is.null(cur_s)) merged[[length(merged) + 1]] <- list(start = cur_s, end = cur_e, metrics = cur_metrics, subs = cur_subs)
    merged_df <- do.call(rbind, lapply(merged, function(x) {
      data.frame(start = x$start, end = x$end,
                 metrics = I(list(x$metrics)), subs = I(list(x$subs)), stringsAsFactors = FALSE)
    }))
    tibble::as_tibble(merged_df)
  }
  
  metric_abbrev <- function(metric) {
    switch(metric,
           "Prob"  = "P",
           "FREX"  = "F",
           "Lift"  = "L",
           "Score" = "S",
           substr(metric, 1, 1))
  }
  
  build_gradient_css <- function(metrics, color_map) {
    if (length(metrics) == 0) return(NULL)
    cols <- lapply(metrics, function(m) color_map[[m]])
    hexs <- vapply(cols, function(rgb) rgb_to_hex(rgb), FUN.VALUE = character(1))
    n <- length(hexs)
    segs <- seq(0, 100, length.out = n + 1)
    pieces <- character(0)
    for (i in seq_len(n)) {
      start <- floor(segs[i])
      end   <- ceiling(segs[i+1])
      pieces <- c(pieces, paste0(hexs[i], " ", start, "%,", hexs[i], " ", end, "%"))
    }
    gradient <- paste(pieces, collapse = ", ")
    sprintf("linear-gradient(90deg, %s)", gradient)
  }
  
  # REPLACED: use htmltools::tags for safe HTML building (avoids parse errors)
  highlight_text_with_merged <- function(txt, merged_df, color_map_local, overlap_fixed_color_local, show_badge_local = TRUE) {
    if (nrow(merged_df) == 0) return(htmltools::htmlEscape(txt))
    out_parts <- list()
    last <- 1
    full_len <- nchar(txt)
    
    for (i in seq_len(nrow(merged_df))) {
      s <- merged_df$start[i]; e <- merged_df$end[i]
      if (last <= s - 1) {
        out_parts <- c(out_parts, htmltools::htmlEscape(substr(txt, last, s - 1)))
      }
      metrics <- unlist(merged_df$metrics[[i]])
      metrics <- unique(metrics)
      snippet <- substr(txt, s, e)
      
      if (!is.null(overlap_fixed_color_local) && length(metrics) > 1) {
        hex <- rgb_to_hex(overlap_fixed_color_local)
        span_style <- paste0("background:", hex, "; color:#000; padding:0 3px; border-radius:2px;")
        span_tag <- htmltools::tags$span(htmltools::htmlEscape(snippet),
                                         title = paste(metrics, collapse = ", "),
                                         class = "kw",
                                         style = span_style)
      } else {
        gradient_css <- build_gradient_css(metrics, color_map_local)
        span_style <- paste0("background:", gradient_css, "; color:#000; padding:0 3px; border-radius:2px; white-space:nowrap;")
        span_tag <- htmltools::tags$span(htmltools::htmlEscape(snippet),
                                         title = paste(metrics, collapse = ", "),
                                         class = "kw",
                                         style = span_style)
      }
      
      if (isTRUE(show_badge_local)) {
        badge_txt <- paste0("(", paste0(vapply(metrics, metric_abbrev, FUN.VALUE = character(1)), collapse = ""), ")")
        badge_tag <- htmltools::tags$sup(htmltools::htmlEscape(badge_txt),
                                         style = "font-size:70%; color:#444; margin-left:3px;")
        out_parts <- c(out_parts, as.character(span_tag), as.character(badge_tag))
      } else {
        out_parts <- c(out_parts, as.character(span_tag))
      }
      
      last <- e + 1
    }
    
    if (last <= full_len) out_parts <- c(out_parts, htmltools::htmlEscape(substr(txt, last, full_len)))
    paste(out_parts, collapse = "")
  }
  
  # Build per-topic metadata
  topic_ids <- unique(topic_df$TopicID) %>% sort()
  topic_meta <- lapply(topic_ids, function(tid) {
    row <- topic_df %>% dplyr::filter(TopicID == tid) %>% dplyr::slice(1)
    kw_prob  <- split_keywords(row$ProbKeywords)
    kw_frex  <- split_keywords(row$FREXKeywords)
    kw_lift  <- split_keywords(row$LiftKeywords)
    kw_score <- split_keywords(row$ScoreKeywords)
    list(kw_by_metric = list(Prob = kw_prob, FREX = kw_frex, Lift = kw_lift, Score = kw_score))
  })
  names(topic_meta) <- topic_ids
  
  # Build HTML
  if (verbose) cat("Building HTML with split-gradient + badge highlighting (underscore tokens treated as separate keys)...\n")
  html_parts <- list()
  css <- sprintf("
  <style>
    body { font-family: '%s', serif; margin: 18px; font-size: %dpx; line-height:1.45; }
    .container { max-width: 1200px; margin-right: 320px; }
    .toc { position: fixed; right: 10px; top: 10px; width: 300px; background: #fafafa; border:1px solid #ddd; padding:10px; max-height:90vh; overflow:auto; font-size:13px; }
    .topic { margin-bottom: 28px; padding-bottom: 6px; border-bottom: 1px solid #eee; }
    .topic-title { font-size: 18px; margin-bottom: 6px; }
    .keywords { font-size: 13px; margin-bottom: 6px; white-space: pre-wrap; }
    .repdoc { margin: 8px 0; padding: 8px; background: #fff; border-left: 3px solid #f9f9f9; }
    .kw { padding: 0 3px; border-radius: 2px; }
    a.toclink { text-decoration: none; color: #333; display: block; padding: 4px 0; }
    .legend { margin-bottom: 8px; font-size:13px; }
    .legend .item { display:inline-block; margin-right:10px; vertical-align:middle; }
    .legend .dot { display:inline-block; width:12px; height:12px; border-radius:2px; margin-right:4px; vertical-align:middle; }
    .guide { margin: 15px 0; font-size:13px; line-height:1.5; background:#f8f8f8; padding:10px; border:1px solid #ddd; border-radius:4px; }
  </style>", FONT_FAMILY, FONT_SIZE_PX)
  html_parts <- c(html_parts, "<!doctype html><html><head><meta charset='utf-8'><title>STM Topics Annotated</title>", css, "</head><body>")
  html_parts <- c(html_parts, "<h1>STM Topics — Annotated (Gradient + Badge)</h1>")
  
  guide_block <- "
  <div class='guide'>
    <h3>Legend – Keyword Feature Categories</h3>
    <ul>
      <li><span style='color:#1f77b4; font-weight:bold;'>Prob (P)</span>: Highest probability words. Most frequent in the topic, but can include general/common terms.</li>
      <li><span style='color:#2ca02c; font-weight:bold;'>FREX (F)</span>: FREX (Frequency–Exclusivity) words. Balance frequency with exclusivity; often the most interpretable.</li>
      <li><span style='color:#d62728; font-weight:bold;'>Lift (L)</span>: Words disproportionately more frequent in this topic than in the corpus overall; highlight unique vocabulary.</li>
      <li><span style='color:#ff7f0e; font-weight:bold;'>Score (S)</span>: Words maximizing a likelihood-based scoring rule; often rare but informative terms.</li>
    </ul>
    <p><strong>Badges:</strong> After each highlighted word, a badge like <code>(PF)</code> or <code>(PLS)</code> shows which feature categories that word belongs to.</p>
    <p><strong>Underscore keywords:</strong> Keywords with underscores (e.g. <code>vehicle_allow</code>) have been split into their component words (<code>vehicle</code> and <code>allow</code>) for highlighting.</p>
  </div>
  "
  html_parts <- c(html_parts, guide_block)
  
  legend_items <- sapply(names(COLOR_MAP), function(m) {
    hex <- rgb_to_hex(COLOR_MAP[[m]])
    sprintf('<span class="item"><span class="dot" style="background:%s;"></span><strong>%s</strong> (%s)</span>', hex, m, metric_abbrev(m))
  }, USE.NAMES = FALSE)
  html_parts <- c(html_parts, sprintf("<div class='legend'>Legend: %s</div>", paste(legend_items, collapse = " ")))
  
  html_parts <- c(html_parts, "<div class='container'><div id='topics'>")
  toc_links <- character(0)
  
  pbapply::pboptions(type = "timer")
  topic_list <- as.list(topic_ids)
  
  res <- pbapply::pblapply(seq_along(topic_list), function(idx) {
    tid <- topic_list[[idx]]
    kw_by_metric <- topic_meta[[as.character(tid)]]$kw_by_metric
    header_html <- paste0("<div id='topic-", htmltools::htmlEscape(as.character(tid)), "' class='topic'>",
                          "<div class='topic-title'>Topic ", htmltools::htmlEscape(as.character(tid)), "</div>")
    kw_lines <- sapply(names(kw_by_metric), function(metric) {
      col_hex <- rgb_to_hex(COLOR_MAP[[metric]])
      words <- paste(kw_by_metric[[metric]], collapse = ", ")
      sprintf("<div class='keywords'><span style='color:%s; font-weight:700;'>%s:</span> %s</div>", col_hex, metric, htmltools::htmlEscape(words))
    }, USE.NAMES = FALSE)
    content <- c(header_html, kw_lines)
    rows <- topic_df %>% dplyr::filter(TopicID == tid) %>% dplyr::arrange(RepDocRank)
    for (r in seq_len(nrow(rows))) {
      raw <- rows$RepDocText[r]
      if (is.na(raw) || raw == "") {
        content <- c(content, "<div class='repdoc'></div>")
        next
      }
      spans_df <- find_substring_spans(raw, kw_by_metric)
      if (nrow(spans_df) == 0) {
        content <- c(content, sprintf("<div class='repdoc'>%s</div>", htmltools::htmlEscape(raw)))
        next
      }
      merged_df <- merge_overlapping_spans(spans_df)
      highlighted <- highlight_text_with_merged(raw, merged_df, COLOR_MAP, OVERLAP_FIXED_COLOR, show_badge)
      content <- c(content, sprintf("<div class='repdoc'>%s</div>", highlighted))
    }
    content <- c(content, "</div>")
    toc <- sprintf("<a class='toclink' href='#topic-%s'>Topic %s</a>", htmltools::htmlEscape(as.character(tid)), htmltools::htmlEscape(as.character(tid)))
    list(content = paste(content, collapse = "\n"), toc = toc)
  }, cl = NULL)
  
  for (i in seq_along(res)) {
    html_parts <- c(html_parts, res[[i]]$content)
    toc_links <- c(toc_links, res[[i]]$toc)
  }
  
  html_parts <- c(html_parts, "</div>")
  html_parts <- c(html_parts, "<div class='toc'><h3>Topics</h3>", toc_links, "</div>")
  html_parts <- c(html_parts, "</div></body></html>")
  
  html_out_text <- as.character(unlist(html_parts))
  
  if (verbose) cat("Writing HTML to:", html_out, "\n")
  writeLines(html_out_text, con = html_out, useBytes = TRUE)
  if (verbose) cat("Done. File saved at:", normalizePath(html_out), "\n")
  
  invisible(html_out)
}
