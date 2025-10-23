library(dplyr)
library(purrr)
library(httr2)
library(jsonlite)
library(stringr)
library(rvest)

# Helper function
`%||%` <- function(x, y) if (is.null(x)) y else x

# Your existing ollama_generate function
if (!exists("ollama_generate")) {
  ollama_generate <- function(prompt, model = "llama3:8b", timeout = 180) {
    request("http://127.0.0.1:11434/api/generate") |>
      req_body_json(list(model = model, prompt = prompt, stream = FALSE)) |>
      req_timeout(timeout) |>
      req_perform() |>
      resp_body_json() |>
      (\(x) x$response)()
  }
}

# IMPROVED: Clean HTML and extract plain text
clean_html_content <- function(html_text) {
  tryCatch({
    # Parse HTML and extract text
    cleaned <- read_html(html_text) %>%
      html_text2()
    
    # Remove extra whitespace
    cleaned <- str_squish(cleaned)
    
    # Limit to first 2000 characters for efficiency
    substr(cleaned, 1, 2000)
  }, error = function(e) {
    # Fallback: just clean the raw text
    cleaned <- gsub("<[^>]+>", " ", html_text)
    cleaned <- str_squish(cleaned)
    substr(cleaned, 1, 2000)
  })
}

# IMPROVED: Classification function with retry logic
classify_article_topic <- function(text, model = "llama3:8b", max_retries = 2) {
  sys <- paste(
    "You are a news article classifier.",
    "Classify this Chicago news article into ONE of these topics:",
    "- Arts & culture",
    "- Food & restaurants",
    "- Immigration",
    "- Politics",
    "- Health",
    "",
    "Return ONLY valid JSON. No code fences, no prose.",
    "Schema:",
    "{",
    '  "topic": string,',
    '  "confidence": number',
    "}",
    "",
    "The 'topic' field must be one of the five options above, exactly as written.",
    sep = "\n"
  )
  
  # Clean the text first
  clean_text <- clean_html_content(text)
  
  prompt <- paste(sys,
                  "\n--- ARTICLE START ---\n",
                  clean_text,
                  "\n--- ARTICLE END ---\n")
  
  for (attempt in 1:max_retries) {
    result <- tryCatch({
      raw <- ollama_generate(prompt, model = model)
      
      # Extract JSON from response
      json_str <- stringr::str_extract(raw, "\\{[\\s\\S]*\\}\\s*$")
      if (is.na(json_str)) {
        stop("Model did not return valid JSON")
      }
      
      parsed <- jsonlite::fromJSON(json_str, simplifyVector = TRUE)
      
      # Validate and clean the topic
      valid_topics <- c("Arts & culture", "Food & restaurants", 
                        "Immigration", "Politics", "Health")
      
      topic <- parsed$topic %||% NA_character_
      
      # Try to match to valid topics (case-insensitive)
      if (!is.na(topic)) {
        match_idx <- which(tolower(valid_topics) == tolower(topic))
        if (length(match_idx) > 0) {
          topic <- valid_topics[match_idx[1]]
        } else {
          stop(paste("Invalid topic returned:", topic))
        }
      }
      
      list(
        topic = topic,
        confidence = as.numeric(parsed$confidence %||% NA_real_),
        success = TRUE
      )
    }, error = function(e) {
      if (attempt < max_retries) {
        cat("  Retry", attempt, "failed:", e$message, "\n")
        Sys.sleep(2)  # Wait before retry
        NULL
      } else {
        list(topic = NA_character_, confidence = NA_real_, success = FALSE)
      }
    })
    
    if (!is.null(result)) return(result)
  }
  
  list(topic = NA_character_, confidence = NA_real_, success = FALSE)
}

# IMPROVED: Process with checkpointing and rate limiting
process_articles_with_checkpoints <- function(articles_df, 
                                              checkpoint_file = "data/classification_checkpoint.rds",
                                              batch_size = 5) {
  
  # Check if checkpoint exists
  if (file.exists(checkpoint_file)) {
    cat("Found existing checkpoint. Loading...\n")
    results <- readRDS(checkpoint_file)
    start_idx <- nrow(results) + 1
    cat("Resuming from article", start_idx, "\n")
  } else {
    results <- tibble(
      row_id = integer(),
      content.rendered = character(),
      topic_tag = character(),
      topic_confidence = numeric()
    )
    start_idx <- 1
  }
  
  # Add row IDs to track progress
  articles_df <- articles_df %>%
    mutate(row_id = row_number())
  
  total <- nrow(articles_df)
  
  # Process remaining articles
  for (i in start_idx:total) {
    cat("\n=== Processing article", i, "of", total, "===\n")
    
    article <- articles_df[i, ]
    
    # Classify
    classification <- classify_article_topic(article$content.rendered)
    
    # Add result
    new_row <- tibble(
      row_id = article$row_id,
      content.rendered = article$content.rendered,
      topic_tag = classification$topic,
      topic_confidence = classification$confidence
    )
    
    results <- bind_rows(results, new_row)
    
    # Save checkpoint after each batch
    if (i %% batch_size == 0 || i == total) {
      saveRDS(results, checkpoint_file)
      cat("Checkpoint saved at article", i, "\n")
    }
    
    # Rate limiting: pause between requests
    if (i < total) {
      cat("Pausing 2 seconds...\n")
      Sys.sleep(2)
    }
    
    # Optional: Garbage collection every 10 articles
    if (i %% 10 == 0) {
      gc()
    }
  }
  
  cat("\n=== Processing complete! ===\n")
  return(results)
}

# Load your data
all_articles <- readRDS("data/api_scrape.rds") %>% 
  slice_head(n = 20) %>%
  select(content.rendered)

# Process with improvements
results <- process_articles_with_checkpoints(
  all_articles, 
  checkpoint_file = "data/classification_checkpoint.rds",
  batch_size = 5  # Save progress every 5 articles
)

# View results
cat("\nClassification Summary:\n")
print(table(results$topic_tag, useNA = "always"))

cat("\nSample Results:\n")
print(results %>% 
        select(topic_tag, topic_confidence) %>%
        head(10))

# Save final results
saveRDS(results, "data/articles_with_topics_final.rds")

# Clean up checkpoint file
if (file.exists("data/classification_checkpoint.rds")) {
  file.remove("data/classification_checkpoint.rds")
  cat("\nCheckpoint file removed.\n")
}