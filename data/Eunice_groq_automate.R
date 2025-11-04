library(dplyr)
library(purrr)
library(httr2)
library(jsonlite)
library(stringr)
library(rvest)

`%||%` <- function(x, y) if (is.null(x)) y else x

GROQ_API_KEY <- Sys.getenv("GROQ_API_KEY")

VALID_TOPICS <- c(
  "Arts & Culture", "Business", "Crime & Public Safety", "Education",
  "Food & Restaurants", "Health & Environment", "Housing", "Immigration",
  "Politics", "Social Movements", "Sports & Recreation", "Transportation & Infrastructure"
)

clean_html_content <- function(html_text) {
  tryCatch({
    cleaned <- read_html(html_text) %>%
      html_text2()
    cleaned <- str_squish(cleaned)
    substr(cleaned, 1, 2000)
  }, error = function(e) {
    cleaned <- gsub("<[^>]+>", " ", html_text)
    cleaned <- str_squish(cleaned)
    substr(cleaned, 1, 2000)
  })
}

classify_article_topic_groq <- function(text, api_key, max_retries = 5) {
  
  clean_text <- clean_html_content(text)
  topic_list <- paste("-", VALID_TOPICS, collapse = "\n")
  
  prompt <- paste(
    "Classify this Chicago news article into ONE of these topics:",
    topic_list,
    "\n",
    "Analyze the article carefully and provide an HONEST confidence score between 0.5 and 1.0.",
    "- Use 0.95-1.0 only if you're very certain",
    "- Use 0.8-0.9 if fairly confident",
    "- Use 0.6-0.75 if somewhat uncertain",
    "- Use 0.5-0.6 if it's ambiguous",
    "\n",
    "Return ONLY valid JSON with this exact format:",
    '{"topic": "Topic Name", "confidence": 0.85}',
    "\n",
    "The topic must be EXACTLY one of the options listed above.",
    "\n--- ARTICLE START ---\n",
    clean_text,
    "\n--- ARTICLE END ---"
  )
  
  for (attempt in 1:max_retries) {
    result <- tryCatch({
      response <- request("https://api.groq.com/openai/v1/chat/completions") %>%
        req_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ) %>%
        req_body_json(list(
          model = "llama-3.1-8b-instant",
          messages = list(
            list(role = "user", content = prompt)
          ),
          temperature = 0.5
        )) %>%
        req_timeout(60) %>%
        req_perform() %>%
        resp_body_json()
      
      raw <- response$choices[[1]]$message$content
      json_str <- stringr::str_extract(raw, "\\{[\\s\\S]*\\}")
      if (is.na(json_str)) {
        stop("Model did not return valid JSON")
      }
      
      parsed <- jsonlite::fromJSON(json_str, simplifyVector = TRUE)
      topic <- parsed$topic %||% NA_character_
      
      if (!is.na(topic)) {
        match_idx <- which(tolower(VALID_TOPICS) == tolower(topic))
        if (length(match_idx) > 0) {
          topic <- VALID_TOPICS[match_idx[1]]
        } else {
          match_idx <- which(sapply(VALID_TOPICS, function(t) {
            grepl(tolower(topic), tolower(t)) || grepl(tolower(t), tolower(topic))
          }))
          if (length(match_idx) > 0) {
            topic <- VALID_TOPICS[match_idx[1]]
            cat("  Partial match found:", topic, "\n")
          } else {
            cat("  Invalid topic returned:", topic, "\n")
            topic <- NA_character_
          }
        }
      }
      
      list(
        topic = topic,
        confidence = as.numeric(parsed$confidence %||% NA_real_),
        success = TRUE
      )
      
    }, error = function(e) {
      if (grepl("429", e$message)) {
        if (attempt < max_retries) {
          wait_time <- min(2^attempt * 10, 120)
          cat("  ⚠ Rate limit hit. Waiting", wait_time, "seconds before retry", attempt, "...\n")
          Sys.sleep(wait_time)
          NULL
        } else {
          cat("  ✗ Rate limit - all retries exhausted\n")
          list(topic = NA_character_, confidence = NA_real_, success = FALSE, rate_limited = TRUE)
        }
      } else {
        if (attempt < max_retries) {
          cat("  Retry", attempt, "failed:", e$message, "\n")
          Sys.sleep(2)
          NULL
        } else {
          cat("  All retries failed:", e$message, "\n")
          list(topic = NA_character_, confidence = NA_real_, success = FALSE, rate_limited = FALSE)
        }
      }
    })
    
    if (!is.null(result)) return(result)
  }
  
  list(topic = NA_character_, confidence = NA_real_, success = FALSE, rate_limited = FALSE)
}

# AUTO-RESTART: This function automatically pauses and resumes
process_with_auto_restart <- function(total_articles = NULL,
                                      api_key,
                                      checkpoint_file = "data/groq_classification_checkpoint.rds",
                                      batch_size = 10,
                                      max_consecutive_failures = 3,
                                      pause_duration_minutes = 10) {
  
  # Load all articles once
  all_articles_full <- readRDS("data/api_scrape.rds") %>%
    mutate(original_row_id = row_number()) %>%
    select(original_row_id, content.rendered)
  
  if (!is.null(total_articles)) {
    all_articles_full <- all_articles_full %>%
      slice_head(n = total_articles)
  }
  
  total_to_process <- nrow(all_articles_full)
  
  # Keep looping until all articles are processed
  repeat {
    # Check current progress
    if (file.exists(checkpoint_file)) {
      results <- readRDS(checkpoint_file)
      processed_ids <- results$row_id
      remaining_articles <- all_articles_full %>%
        filter(!original_row_id %in% processed_ids)
      
      cat("\n", rep("=", 60), "\n")
      cat("PROGRESS CHECK\n")
      cat(rep("=", 60), "\n")
      cat("✓ Processed:", length(processed_ids), "/", total_to_process, "articles\n")
      cat("⏳ Remaining:", nrow(remaining_articles), "articles\n")
      cat(rep("=", 60), "\n\n")
      
      if (nrow(remaining_articles) == 0) {
        cat("🎉 ALL ARTICLES PROCESSED!\n")
        return(results)
      }
      
    } else {
      results <- tibble(
        row_id = integer(),
        content.rendered = character(),
        topic_tag = character(),
        topic_confidence = numeric()
      )
      remaining_articles <- all_articles_full
      cat("Starting fresh. Total articles:", total_to_process, "\n\n")
    }
    
    consecutive_failures <- 0
    articles_processed_this_round <- 0
    
    # Process articles until we hit rate limit
    for (i in 1:nrow(remaining_articles)) {
      article <- remaining_articles[i, ]
      progress_num <- nrow(all_articles_full) - nrow(remaining_articles) + i
      
      cat("\n=== Article", progress_num, "of", total_to_process, "(ID:", article$original_row_id, ") ===\n")
      
      classification <- classify_article_topic_groq(
        article$content.rendered, 
        api_key = api_key
      )
      
      # Check for rate limiting
      if (!is.null(classification$rate_limited) && classification$rate_limited) {
        consecutive_failures <- consecutive_failures + 1
        cat("⚠ Consecutive failures:", consecutive_failures, "/", max_consecutive_failures, "\n")
        
        if (consecutive_failures >= max_consecutive_failures) {
          cat("\n🛑 RATE LIMIT DETECTED\n")
          cat("Processed", articles_processed_this_round, "articles this round\n")
          cat("⏸ Auto-pausing for", pause_duration_minutes, "minutes...\n")
          cat("Progress is saved. Will automatically resume.\n\n")
          
          # Save current progress
          saveRDS(results, checkpoint_file)
          
          # Wait for rate limit to reset
          for (min in pause_duration_minutes:1) {
            cat("\rResuming in", min, "minutes...  ")
            Sys.sleep(60)
          }
          cat("\n\n🔄 RESUMING...\n\n")
          
          # Break out of inner loop to restart with updated remaining_articles
          break
        }
      } else if (classification$success) {
        consecutive_failures <- 0
        articles_processed_this_round <- articles_processed_this_round + 1
      }
      
      # Add result
      new_row <- tibble(
        row_id = article$original_row_id,
        content.rendered = article$content.rendered,
        topic_tag = classification$topic,
        topic_confidence = classification$confidence
      )
      
      results <- bind_rows(results, new_row)
      
      # Save checkpoint
      if (i %% batch_size == 0 || i == nrow(remaining_articles)) {
        saveRDS(results, checkpoint_file)
        cat("✓ Checkpoint saved (", nrow(results), "total)\n")
      }
      
      # Rate limiting pause
      if (i < nrow(remaining_articles)) {
        Sys.sleep(3)
      }
    }
    
    # If we completed all remaining articles without hitting rate limit, we're done!
    if (articles_processed_this_round == nrow(remaining_articles)) {
      cat("\n🎉 ALL ARTICLES PROCESSED!\n")
      return(results)
    }
  }
}

# Function to retry failed articles
retry_failed_articles <- function(checkpoint_file, api_key) {
  
  if (!file.exists(checkpoint_file)) {
    cat("No checkpoint file found!\n")
    return(NULL)
  }
  
  results <- readRDS(checkpoint_file)
  failed_indices <- which(is.na(results$topic_tag))
  
  if (length(failed_indices) == 0) {
    cat("✓ No failed articles to retry!\n")
    return(results)
  }
  
  cat("Found", length(failed_indices), "failed articles to retry\n")
  cat("Failed article IDs:", results$row_id[failed_indices], "\n\n")
  
  for (idx in failed_indices) {
    cat("Retrying article row_id", results$row_id[idx], "(", which(failed_indices == idx), "of", length(failed_indices), ")\n")
    
    classification <- classify_article_topic_groq(
      results$content.rendered[idx],
      api_key = api_key
    )
    
    results$topic_tag[idx] <- classification$topic
    results$topic_confidence[idx] <- classification$confidence
    
    saveRDS(results, checkpoint_file)
    cat("✓ Saved\n")
    
    Sys.sleep(3)
  }
  
  cat("\n✓ Retry complete!\n")
  return(results)
}

# ============================================
# MAIN USAGE - JUST RUN THIS!
# ============================================

cat("🚀 Starting AUTO-RESTART processing\n")
cat("This will automatically pause and resume when hitting rate limits.\n")
cat("You can leave this running - it will handle everything!\n\n")

# Process all articles with auto-restart
results <- process_with_auto_restart(
  total_articles = 28812,
  api_key = GROQ_API_KEY,
  checkpoint_file = "data/groq_classification_checkpoint.rds",
  batch_size = 10,
  max_consecutive_failures = 3,
  pause_duration_minutes = 0.083  # Wait 10 minutes when hitting rate limit
)

#############################Ran here

results <- readRDS("data/groq_classification_checkpoint.rds")

# View the data table
View(results)

# After main processing, retry any failed articles
cat("\n=== Retrying failed articles ===\n")
results <- retry_failed_articles(
  checkpoint_file = "data/groq_classification_checkpoint.rds",
  api_key = GROQ_API_KEY
)

# Final summary
cat("\n", rep("=", 60), "\n")
cat("FINAL RESULTS\n")
cat(rep("=", 60), "\n")
cat("Total processed:", nrow(results), "articles\n\n")
cat("Classification Summary:\n")
print(table(results$topic_tag, useNA = "always"))

cat("\nConfidence Score Distribution:\n")
print(summary(results$topic_confidence))

# Save final results
saveRDS(results, "data/articles_with_topics_final.rds")
cat("\n✓ Saved final results to: data/articles_with_topics_final.rds\n")