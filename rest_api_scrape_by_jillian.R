# SIMPLE WORDPRESS REST API SCRAPER ----
library(httr)
library(jsonlite)
library(dplyr)
library(here)
# /wp-json/wp/v2/posts
# scrape the REST API ----
get_all_posts <- function(base_url, per_page = 100, max_pages = 1000, start_page = 1) {
  all_posts <- list()
  
  for(page in start_page:max_pages) {
    url <- paste0(base_url, "/wp-json/wp/v2/posts?per_page=", per_page, "&page=", page)
    
    # timeout: 20 minutes
    res <- GET(url, timeout(1200))
    
    # stop if request fails
    if(status_code(res) != 200) break
    
    posts <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
    
    # stop if no posts returned
    if(length(posts) == 0) break
    
    # convert all columns to character to avoid binding issues
    posts[] <- lapply(posts, as.character)
    
    all_posts[[page]] <- posts
    cat("Fetched page", page, "with", nrow(posts), "posts\n")
    
    # save point every 10 pages
    if(page %% 10 == 0) {
      temp_df <- bind_rows(all_posts)
      saveRDS(temp_df, here("data", paste0("checkpoint_page_", page, ".rds")))
      cat("Saved checkpoint at page", page, "\n")
    }
  }
  
  bind_rows(all_posts)
}

# call fxn ----
all_posts_df <- get_all_posts("https://blockclubchicago.org")
saveRDS(all_posts_df, here("data", "api_scrape.rds"))

all_posts_df <- readRDS(here("data", "api_scrape.rds"))
