# ADD ARTICLE TAGS WITH AI ----

# load packages ----
library(dplyr)
library(purrr)
library(httr2)
library(jsonlite)
library(stringr)
library(rvest)
library(here)

`%||%` <- function(x, y) if (is.null(x)) y else x

# constants ----
openai_api_key <- Sys.getenv("OPEN_AI_KEY")

valid_topics <- c(
  "Arts & Culture", "Business", "Crime & Public Safety", "Education",
  "Food & Restaurants", "Health & Environment", "Housing", "Immigration",
  "Politics", "Social Movements", "Sports & Recreation", "Transportation & Infrastructure"
)

# helper functions ----
clean_html_content <- function(html_text) {
  tryCatch({
    cleaned <- read_html(html_text) %>% html_text2()
    cleaned <- str_squish(cleaned)
    substr(cleaned, 1, 2000)
  }, error = function(e) {
    cleaned <- gsub("<[^>]+>", " ", html_text)
    cleaned <- str_squish(cleaned)
    substr(cleaned, 1, 2000)
  })
}

classify_article <- function(text, api_key, max_retries = 5) {
  clean_text <- clean_html_content(text)
  topic_list <- paste("-", valid_topics, collapse = "\n")
  prompt <- paste(
    "classify this chicago news article into one of these topics:",
    topic_list,
    "\nreturn only json:",
    '{"topic": "Topic Name", "confidence": 0.85}',
    "\n--- article start ---\n",
    clean_text,
    "\n--- article end ---"
  )
  
  for (attempt in 1:max_retries) {
    result <- tryCatch({
      resp <- request("https://api.openai.com/v1/chat/completions") %>%
        req_headers(Authorization = paste("Bearer", api_key),
                    `Content-Type` = "application/json") %>%
        req_body_json(list(
          model = "gpt-4o-mini",
          messages = list(list(role = "user", content = prompt)),
          temperature = 0.5
        )) %>%
        req_timeout(60) %>%
        req_perform() %>%
        resp_body_json()
      
      raw <- resp$choices[[1]]$message$content
      json_str <- str_extract(raw, "\\{[\\s\\S]*\\}")
      parsed <- fromJSON(json_str)
      topic <- parsed$topic %||% NA_character_
      if (!topic %in% valid_topics) topic <- NA_character_
      
      list(topic = topic, confidence = as.numeric(parsed$confidence %||% NA_real_), success = TRUE)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(result)) return(result)
    Sys.sleep(2)
  }
  list(topic = NA_character_, confidence = NA_real_, success = FALSE)
}

# auto-restart processing ----
process_articles <- function(articles_file = "data/api_scrape.rds",
                             checkpoint_file = "data/my_openai_checkpoint.rds",
                             batch_size = 10,
                             pause_minutes = 1) {
  
  all_articles <- readRDS(articles_file) %>%
    mutate(row_id = row_number()) %>%
    select(id, row_id, content.rendered)
  
  results <- if (file.exists(checkpoint_file)) readRDS(checkpoint_file) else tibble(
    id = character(), row_id = integer(), content.rendered = character(),
    topic_tag = character(), topic_confidence = numeric()
  )
  
  remaining <- anti_join(all_articles, results, by = "row_id")
  total <- nrow(all_articles)
  
  while (nrow(remaining) > 0) {
    for (i in seq_len(nrow(remaining))) {
      art <- remaining[i, ]
      cat("\nfetching article", art$row_id, "of", total, "\n")
      
      res <- classify_article(art$content.rendered, openai_api_key)
      results <- bind_rows(results, tibble(
        id = art$id, row_id = art$row_id, content.rendered = art$content.rendered,
        topic_tag = res$topic, topic_confidence = res$confidence
      ))
      
      if (i %% batch_size == 0 || i == nrow(remaining)) {
        saveRDS(results, checkpoint_file)
        cat("✓ checkpoint saved (", nrow(results), "total)\n")
      }
      Sys.sleep(0.5)
    }
    
    remaining <- anti_join(all_articles, results, by = "row_id")
    if (nrow(remaining) > 0) {
      cat("\npausing for", pause_minutes, "minute(s) to avoid rate limits...\n")
      Sys.sleep(pause_minutes * 60)
    }
  }
  cat("\n🎉 all articles processed!\n")
  results
}

# call function ----
final_results <- process_articles(
  articles_file = "data/api_scrape.rds",
  checkpoint_file = "data/my_openai_checkpoint.rds",
  batch_size = 10,
  pause_minutes = 1
)

saveRDS(final_results, here("data", "articles_with_topics_final.rds"))
cat("✓ final results saved!\n")