# ROUGH DRAFT WEB SCRAPE ----

# load packages ----
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
# Enhanced add_geocoding function ----
add_geocoding <- function(articles_df) {
  
  cat("\n=== STEP 3: Adding geocoding for neighborhoods ===\n")
  
  # create a list of known Chicago neighborhoods for matching
  chicago_neighborhoods <- c(
    "Albany Park", "Andersonville", "Archer Heights", "Armour Square", "Ashburn",
    "Auburn Gresham", "Austin", "Avalon Park", "Avondale", "Belmont Cragin",
    "Beverly", "Bridgeport", "Brighton Park", "Bronzeville", "Bucktown",
    "Burnside", "Calumet Heights", "Chatham", "Chinatown", "Clearing",
    "Douglas", "Dunning", "East Garfield Park", "East Side", "Edgewater",
    "Edison Park", "Englewood", "Forest Glen", "Fuller Park", "Gage Park",
    "Garfield Ridge", "Grand Boulevard", "Greater Grand Crossing", "Hegewisch",
    "Hermosa", "Humboldt Park", "Hyde Park", "Irving Park", "Jefferson Park",
    "Kenwood", "Lake View", "Lincoln Park", "Lincoln Square", "Little Village",
    "Logan Square", "Loop", "Lower West Side", "McKinley Park", "Montclare",
    "Morgan Park", "Mount Greenwood", "Near North Side", "Near South Side",
    "Near West Side", "New City", "North Center", "North Lawndale", "North Park",
    "Norwood Park", "Oakland", "O'Hare", "Pilsen", "Portage Park", "Pullman",
    "Riverdale", "Rogers Park", "Roseland", "South Chicago", "South Deering",
    "South Lawndale", "South Loop", "South Shore", "Uptown", "Washington Heights",
    "Washington Park", "West Elsdon", "West Englewood", "West Garfield Park",
    "West Lawn", "West Pullman", "West Ridge", "West Town", "Woodlawn"
  )
  
  # extract neighborhoods from article text and keep categories separate
  articles_with_neighborhoods <- articles_df |>
    mutate(
      # extract first word/phrase from article text (up to first dash or punctuation)
      first_word = str_extract(article_text, "^[A-Z][A-Za-z\\s]+?(?=\\s—|\\s-|\\.|,|\\s\\()"),
      first_word = str_trim(first_word),
      # use first word if it exists, otherwise use categories
      neighborhood = ifelse(!is.na(first_word) & first_word != "", 
                            first_word, 
                            categories)
    ) |>
    # keep original categories column separate
    select(url, headline, author, pub_date, categories, neighborhood, first_word, everything())
  
  # extract unique neighborhoods for geocoding (from the neighborhood column only)
  unique_neighborhoods <- articles_with_neighborhoods |>
    filter(!is.na(neighborhood) & neighborhood != "") |>
    separate_rows(neighborhood, sep = ", ") |>
    mutate(neighborhood = str_trim(neighborhood)) |>
    filter(neighborhood != "") |>
    distinct(neighborhood) |>
    pull(neighborhood)
  
  cat("Found", length(unique_neighborhoods), "unique neighborhoods to geocode\n")
  cat("Neighborhoods found:", paste(unique_neighborhoods, collapse = ", "), "\n")
  
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
  
  # Fixed geocoding approach - batch geocode the entire dataframe
  tryCatch({
    neighborhood_coords <- neighborhood_coords |>
      geocode(full_address, method = 'osm')
  }, error = function(e) {
    cat("Batch geocoding failed, trying individual geocoding...\n")
    
    # Initialize lat and long columns
    neighborhood_coords$lat <- NA_real_
    neighborhood_coords$long <- NA_real_
    
    # Geocode each address individually
    for(i in 1:nrow(neighborhood_coords)) {
      address <- neighborhood_coords$full_address[i]
      cat("Geocoding:", address, "\n")
      Sys.sleep(0.5)
      
      tryCatch({
        # Create a single-row dataframe for geo()
        temp_df <- data.frame(address = address)
        result <- geo(temp_df, address = address, method = 'osm')
        if(nrow(result) > 0 && !is.na(result$lat[1])) {
          neighborhood_coords$lat[i] <- result$lat[1]
          neighborhood_coords$long[i] <- result$long[1]
        }
      }, error = function(e) {
        cat("  Failed:", e$message, "\n")
      })
    }
  })
  
  # show geocoding results
  successful_geocodes <- sum(!is.na(neighborhood_coords$lat))
  cat("Successfully geocoded", successful_geocodes, "out of", nrow(neighborhood_coords), "neighborhoods\n")
  
  # join coordinates back to articles
  articles_with_coords <- articles_with_neighborhoods |>
    # Use detected neighborhood if it exists and matches, otherwise use original categories
    mutate(
      final_neighborhood = ifelse(!is.na(first_word) & first_word %in% chicago_neighborhoods, 
                                  first_word,  # Use only the detected neighborhood
                                  neighborhood) # Use original categories (may have multiple)
    ) |>
    # Now separate rows only if using original categories (which may have multiple neighborhoods)
    separate_rows(final_neighborhood, sep = ", ") |>
    mutate(final_neighborhood = str_trim(final_neighborhood)) |>
    filter(!is.na(final_neighborhood) & final_neighborhood != "") |>
    # join with coordinates using the final neighborhood
    left_join(neighborhood_coords, by = c("final_neighborhood" = "neighborhood")) |>
    # remove the original neighborhood column and rename final_neighborhood
    select(-neighborhood) |>
    rename(
      neighborhood = final_neighborhood,
      neighborhood_lat = lat,
      neighborhood_lng = long,
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