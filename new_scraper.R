# WORDPRESS REST API SCRAPER ----
# Automatically does full scrape first time, then incremental updates
# load packages ----
library(httr)
library(jsonlite)
library(dplyr)

# Use here() if available, otherwise use relative paths
if(require(here, quietly = TRUE)) {
  data_dir <- here("data")
} else {
  data_dir <- "data"
}

# create data directory if it doesn't exist
if(!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

# FULL SCRAPE FUNCTION (for first run) ----
get_all_posts <- function(base_url, per_page = 100, max_pages = 1000, start_page = 1) {
  all_posts <- list()
  
  for(page in start_page:max_pages) {
    url <- paste0(base_url, "/wp-json/wp/v2/posts?per_page=", per_page, "&page=", page)
    
    # timeout: 20 minutes
    res <- GET(url, timeout(1200))
    
    # stop if request fails
    if(status_code(res) != 200) {
      cat("Stopped at page", page, "- status code:", status_code(res), "\n")
      break
    }
    
    posts <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
    
    # stop if no posts returned
    if(length(posts) == 0) break
    
    # convert all columns to character to avoid binding issues
    posts[] <- lapply(posts, as.character)
    
    all_posts[[page]] <- posts
    cat("Fetched page", page, "with", nrow(posts), "posts\n")
    
    # save checkpoint every 10 pages
    if(page %% 10 == 0) {
      temp_df <- bind_rows(all_posts)
      saveRDS(temp_df, file.path(data_dir, paste0("checkpoint_page_", page, ".rds")))
      cat("Saved checkpoint at page", page, "\n")
    }
  }
  
  bind_rows(all_posts)
}

# INCREMENTAL SCRAPE FUNCTION (for updates) ----
get_new_posts <- function(base_url, existing_posts, per_page = 100, max_pages = 100) {
  
  # get the most recent post ID to know where to stop
  latest_id <- max(as.numeric(existing_posts$id), na.rm = TRUE)
  cat("Latest existing post ID:", latest_id, "\n")
  
  new_posts <- list()
  found_existing <- FALSE
  
  for(page in 1:max_pages) {
    url <- paste0(base_url, "/wp-json/wp/v2/posts?per_page=", per_page, "&page=", page)
    
    # timeout: 20 minutes
    res <- GET(url, timeout(1200))
    
    # stop if request fails
    if(status_code(res) != 200) {
      cat("Stopped at page", page, "- status code:", status_code(res), "\n")
      break
    }
    
    posts <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
    
    # stop if no posts returned
    if(length(posts) == 0) {
      cat("No more posts found at page", page, "\n")
      break
    }
    
    # convert all columns to character
    posts[] <- lapply(posts, as.character)
    
    # check if we've reached posts we already have
    post_ids <- as.numeric(posts$id)
    new_mask <- post_ids > latest_id
    
    if(any(!new_mask)) {
      # we've hit posts we already have
      found_existing <- TRUE
      if(any(new_mask)) {
        # keep only the new ones from this page
        posts <- posts[new_mask, ]
        new_posts[[page]] <- posts
        cat("Page", page, "- found", sum(new_mask), "new posts, reached existing content\n")
      } else {
        cat("Page", page, "- all posts already exist, stopping\n")
      }
      break
    }
    
    new_posts[[page]] <- posts
    cat("Fetched page", page, "with", nrow(posts), "NEW posts\n")
  }
  
  if(length(new_posts) > 0) {
    return(bind_rows(new_posts))
  } else {
    return(NULL)
  }
}

# MAIN SCRAPING LOGIC ----
scrape_wordpress <- function(base_url) {
  data_file <- file.path(data_dir, "api_scrape.rds")
  
  # Check if we have existing data
  if(file.exists(data_file)) {
    cat("\n=== INCREMENTAL UPDATE MODE ===\n")
    existing_posts <- readRDS(data_file)
    cat("Found", nrow(existing_posts), "existing posts\n\n")
    
    # Get only new posts
    new_posts <- get_new_posts(base_url, existing_posts)
    
    if(!is.null(new_posts)) {
      # Combine with existing
      combined_df <- bind_rows(new_posts, existing_posts)
      
      # Remove any duplicates by id
      combined_df <- combined_df %>% 
        distinct(id, .keep_all = TRUE) %>%
        arrange(desc(as.numeric(id)))
      
      # Save updated data
      saveRDS(combined_df, data_file)
      
      cat("\n=== UPDATE SUMMARY ===\n")
      cat("New posts added:", nrow(new_posts), "\n")
      cat("Total posts now:", nrow(combined_df), "\n")
      
      return(combined_df)
    } else {
      cat("\nNo new posts found. Data unchanged.\n")
      return(existing_posts)
    }
    
  } else {
    cat("\n=== FULL SCRAPE MODE (First Run) ===\n")
    cat("No existing data found. Starting full scrape...\n\n")
    
    # Do full scrape
    all_posts_df <- get_all_posts(base_url)
    
    # Save data
    saveRDS(all_posts_df, data_file)
    
    cat("\n=== SCRAPE COMPLETE ===\n")
    cat("Total posts scraped:", nrow(all_posts_df), "\n")
    cat("Data saved to:", data_file, "\n")
    
    return(all_posts_df)
  }
}

# RUN THE SCRAPER ----
all_posts_df <- scrape_wordpress("https://blockclubchicago.org")

# Optional: view the data
cat("\nFirst few posts:\n")
print(head(all_posts_df %>% select(id, title.rendered, date), 10))