# ROUGH DRAFT WEB SCRAPE

# load packages
library(rvest)
library(dplyr)
library(tidygeocoder)
library(purrr)
library(stringr)
library(tidyverse)
library(tidyr)

# scrape single article (scrape all is recursive) ----
scrape_single_article <- function(article_url) {
  
  cat("Scraping article:", article_url, "\n")
  
  # add a lil delay for the server
  Sys.sleep(1)
  
  tryCatch({
    # read the article
    article_page <- read_html(article_url)
    
    # extract detailed article information
    headline <- article_page |>
      html_elements("h1.entry-title, .entry-header h1, h1") |>
      html_text(trim = TRUE) |>
      first()
    
    # full article text (multiple paragraphs)
    article_text <- article_page |>
      html_elements(".entry-content p, .post-content p, .content p") |>
      html_text(trim = TRUE) |>
      paste(collapse = " ")
    
    # author information
    author <- article_page |>
      html_elements(".author-name, .byline, .entry-author, .author") |>
      html_text(trim = TRUE) |>
      first()
    
    # publication date
    pub_date <- article_page |>
      html_elements(".entry-date, .published, time, .post-date") |>
      html_text(trim = TRUE) |>
      first()
    
    # categories/neighborhoods
    categories <- article_page |>
      html_elements(".cat-links a, .categories a") |>
      html_text(trim = TRUE) |>
      paste(collapse = ", ")
    
    # tags
    tags <- article_page |>
      html_elements(".tags a, .post-tags a, .tag-links a") |>
      html_text(trim = TRUE) |>
      paste(collapse = ", ")
    
    # featured image
    featured_image <- article_page |>
      html_elements(".featured-image img, .post-thumbnail img, .wp-post-image") |>
      html_attr("src") |>
      first()
    
    # meta description
    meta_description <- article_page |>
      html_elements("meta[name='description']") |>
      html_attr("content") |>
      first()
    
    # word count
    word_count <- str_count(article_text, "\\S+")
    
    return(tibble(
      url = article_url,
      headline = ifelse(length(headline) == 0 || is.na(headline), NA, headline),
      author = ifelse(length(author) == 0 || is.na(author), NA, author),
      pub_date = ifelse(length(pub_date) == 0 || is.na(pub_date), NA, pub_date),
      categories = ifelse(categories == "" || is.na(categories), NA, categories),
      tags = ifelse(tags == "" || is.na(tags), NA, tags),
      article_text = ifelse(article_text == "" || is.na(article_text), NA, article_text),
      word_count = word_count,
      featured_image = ifelse(length(featured_image) == 0 || is.na(featured_image), NA, featured_image),
      meta_description = ifelse(length(meta_description) == 0 || is.na(meta_description), NA, meta_description),
      scraped_at = Sys.time()
    ))
    
  }, error = function(e) {
    cat("Error scraping", article_url, ":", e$message, "\n")
    return(tibble(
      url = article_url,
      headline = NA,
      author = NA,
      pub_date = NA,
      categories = NA,
      tags = NA,
      article_text = NA,
      word_count = NA,
      featured_image = NA,
      meta_description = NA,
      scraped_at = Sys.time(),
      error = e$message
    ))
  })
}

# scrape all articles ----
scrape_all_articles <- function(homepage_url = "https://blockclubchicago.org/", max_articles = NULL) {
  
  cat("=== STEP 1: Getting article links from homepage ===\n")
  
  # get all article links from homepage
  homepage <- read_html(homepage_url)
  
  article_links <- homepage |>
    html_elements("article") |>
    map_chr(~ {
      .x |>
        html_elements(".entry-title a, h2 a, h1 a") |>
        html_attr("href") |>
        first()
    }) |>
    na.omit() |>
    unique()
  
  cat("Found", length(article_links), "article links on homepage\n")
  
  # limit for testing if specified
  if (!is.null(max_articles)) {
    article_links <- head(article_links, max_articles)
    cat("Limited to", length(article_links), "articles for testing\n")
  }
  
  cat("\n=== STEP 2: Scraping full content for each article ===\n")
  
  # progress tracking
  total_articles <- length(article_links)
  
  # scrape each article and combine into one dataframe
  all_articles <- map_dfr(seq_along(article_links), ~ {
    i <- .x
    url <- article_links[i]
    
    cat("Progress:", i, "/", total_articles, "-", url, "\n")
    
    # scrape this article
    article_data <- scrape_single_article(url)
    
    # add progress info
    article_data$article_number <- i
    article_data$total_articles <- total_articles
    
    return(article_data)
  })
  
  cat("\n=== SCRAPING COMPLETE ===\n")
  cat("Successfully scraped", nrow(all_articles), "articles\n")
  cat("Total words across all articles:", sum(all_articles$word_count, na.rm = TRUE), "\n")
  
  return(all_articles)
}

# add geocoding ----
add_geocoding <- function(articles_df) {
  
  cat("\n=== STEP 3: Adding geocoding for neighborhoods ===\n")
  
  # extract unique neighborhoods from categories
  unique_neighborhoods <- articles_df |>
    filter(!is.na(categories)) |>
    separate_rows(categories, sep = ", ") |>
    mutate(categories = str_trim(categories)) |>
    filter(categories != "" & !is.na(categories)) |>
    distinct(categories) |>
    pull(categories)
  
  cat("Found", length(unique_neighborhoods), "unique neighborhoods to geocode\n")
  
  # create geocoding lookup table
  neighborhood_coords <- tibble(neighborhood = unique_neighborhoods) |>
    mutate(
      # add "Chicago, IL" to improve geocoding accuracy
      full_address = paste(neighborhood, "Chicago, IL"),
      # clean up some common neighborhood name issues
      full_address = case_when(
        str_detect(full_address, "Downtown") ~ "Downtown Chicago, IL",
        str_detect(full_address, "Loop") ~ "Chicago Loop, IL", 
        str_detect(full_address, "West Town") ~ "West Town Chicago, IL",
        str_detect(full_address, "South Loop") ~ "South Loop Chicago, IL",
        TRUE ~ full_address
      )
    )
  
  cat("Starting geocoding process...\n")
  
  # geocode with error handling
  neighborhood_coords <- neighborhood_coords |>
    mutate(
      geocode_attempt = map(full_address, ~ {
        cat("Geocoding:", .x, "\n")
        Sys.sleep(0.5)
        
        tryCatch({
          result <- geocode(.x, method = 'osm', lat = latitude, long = longitude)
          return(result)
        }, error = function(e) {
          cat("  Failed:", e$message, "\n")
          return(tibble(latitude = NA, longitude = NA))
        })
      })
    ) |>
    unnest(geocode_attempt) |>
    select(neighborhood, latitude, longitude, full_address)
  
  # show geocoding results
  successful_geocodes <- sum(!is.na(neighborhood_coords$latitude))
  cat("Successfully geocoded", successful_geocodes, "out of", nrow(neighborhood_coords), "neighborhoods\n")
  
  # join coordinates back to articles
  articles_with_coords <- articles_df |>
    # create a row for each article-neighborhood combination
    separate_rows(categories, sep = ", ") |>
    mutate(categories = str_trim(categories)) |>
    filter(!is.na(categories) & categories != "") |>
    # join with coordinates
    left_join(neighborhood_coords, by = c("categories" = "neighborhood")) |>
    # rename for clarity
    rename(
      neighborhood = categories,
      neighborhood_lat = latitude,
      neighborhood_lng = longitude,
      neighborhood_geocoded_address = full_address
    )
  
  cat("Final dataset has", nrow(articles_with_coords), "rows (article-neighborhood combinations)\n")
  
  return(list(
    articles_with_coords = articles_with_coords,
    neighborhood_lookup = neighborhood_coords
  ))
}

# complete the scrape ----
run_complete_scrape <- function(max_articles = NULL, save_file = "blockclub_all_articles.csv", include_geocoding = TRUE) {
  
  cat("🗞️ STARTING BLOCK CLUB CHICAGO COMPLETE SCRAPE 🗞️\n\n")
  
  # scrape all articles
  all_articles <- scrape_all_articles(max_articles = max_articles)
  
  # add geocoding if requested
  if (include_geocoding) {
    geocoding_results <- add_geocoding(all_articles)
    articles_final <- geocoding_results$articles_with_coords
    neighborhood_lookup <- geocoding_results$neighborhood_lookup
  } else {
    articles_final <- all_articles
    neighborhood_lookup <- NULL
  }
  
  # create output directory if needed
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # save the complete dataset
  full_path <- file.path("data", save_file)
  write_csv(articles_final, full_path)
  
  cat("\n✅ COMPLETE! Saved", nrow(articles_final), "records to:", full_path, "\n")
  
  # save neighborhood lookup table separately
  if (!is.null(neighborhood_lookup)) {
    lookup_path <- file.path("data", "neighborhood_coordinates.csv")
    write_csv(neighborhood_lookup, lookup_path)
    cat("📍 Saved neighborhood coordinates to:", lookup_path, "\n")
  }
  
  # show summary stats
  cat("\n=== SUMMARY STATISTICS ===\n")
  if (include_geocoding) {
    original_articles <- length(unique(articles_final$url))
    cat("Original articles:", original_articles, "\n")
    cat("Article-neighborhood combinations:", nrow(articles_final), "\n")
    cat("Records with coordinates:", sum(!is.na(articles_final$neighborhood_lat)), "\n")
    cat("Unique neighborhoods:", length(unique(articles_final$neighborhood)), "\n")
  } else {
    cat("Total articles:", nrow(articles_final), "\n")
  }
  
  cat("Articles with full text:", sum(!is.na(articles_final$article_text)), "\n")
  cat("Articles with authors:", sum(!is.na(articles_final$author)), "\n")
  cat("Average words per article:", round(mean(articles_final$word_count, na.rm = TRUE)), "\n")
  
  # show neighborhood distribution
  if (include_geocoding) {
    neighborhood_counts <- articles_final |>
      count(neighborhood, sort = TRUE)
    
    cat("\nTop 10 neighborhoods by article count:\n")
    print(head(neighborhood_counts, 10))
    
    # show geocoding success rate
    geocoded_neighborhoods <- articles_final |>
      distinct(neighborhood, neighborhood_lat) |>
      summarise(
        total = n(),
        geocoded = sum(!is.na(neighborhood_lat)),
        success_rate = round(geocoded/total * 100, 1)
      )
    
    cat("\nGeocoding success rate:", geocoded_neighborhoods$success_rate, "%\n")
  }
  
  return(articles_final)
}

# call fxn ----
cat("=== TESTING WITH 5 ARTICLES + GEOCODING ===\n")
test_articles <- run_complete_scrape(
  max_articles = 5, 
  save_file = "test_articles_with_coords.csv",
  include_geocoding = TRUE
)

# all_articles <- run_complete_scrape(
#   save_file = "blockclub_complete_dataset_with_coords.csv",
#   include_geocoding = TRUE
# )