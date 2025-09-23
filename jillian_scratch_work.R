library(rvest)
library(dplyr)
library(tidygeocoder)
library(purrr)
library(stringr)
library(tidyverse)
library(tidyr)

# URL of the newspaper site
url <- "https://blockclubchicago.org/"

# read the HTML content
webpage <- read_html(url)

# extract article headlines ---
# inspect the site and update the CSS selector accordingly
headlines <- webpage |> 
  html_elements(".entry-title, .post-title, h2 a") |> 
  html_text(trim = TRUE)

# extract article links ----
links <- webpage %>%
  html_elements(".entry-title a, .post-title a, h2 a") %>%  # same links as headlines
  html_attr("href")

# extract authors ----
authors <- webpage %>%
  html_elements(".byline") %>%  # example CSS selectors
  html_text(trim = TRUE)

# extract publication dates ----
# dates <- webpage %>%
#   html_elements(".published, .entry-date") %>%  # example CSS selectors
#   html_text(trim = TRUE)

neighborhoods <- webpage %>%
  html_elements(".entry-header a[rel='category tag'], .cat-links a[rel='category tag']") %>%
  html_text(trim = TRUE)

# combine into a dataframe ----
articles <- tibble(
  author = authors,
  neighborhoods = neighborhoods
)

# view ----
print(articles)

# extract all data within article containers ----
# Try to find the container that holds each complete article
articles_method1 <- webpage %>%
  html_elements("article, .post, .entry") %>%  # common article container selectors
  map_dfr(~ {
    article_node <- .x
    
    # Extract headline from within this article
    headline <- article_node %>%
      html_elements(".entry-title, .post-title, h1, h2, h3") %>%
      html_text(trim = TRUE) %>%
      first() # take first match
    
    # Extract link from within this article
    link <- article_node %>%
      html_elements("a") %>%
      html_attr("href") %>%
      first() # take first link
    
    # Extract category/neighborhood from within this article
    category <- article_node %>%
      html_elements("a[rel='category tag'], .category, .cat-links a") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = ", ") # join multiple categories
    
    # Extract author from within this article
    author <- article_node %>%
      html_elements(".byline, .author, .by-author") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Return as tibble row
    tibble(
      headline = ifelse(length(headline) == 0, NA, headline),
      link = ifelse(length(link) == 0, NA, link),
      category = ifelse(category == "", NA, category),
      author = ifelse(length(author) == 0, NA, author)
    )
  })

print("Method 1 - Article container approach:")
print(articles_method1)

all_categories <- webpage %>%
  html_elements("a[rel='category tag']") %>%
  html_text(trim = TRUE)

library(rvest)
library(dplyr)

# URL of the newspaper site
url <- "https://blockclubchicago.org/"

# Read the HTML content
webpage <- read_html(url)

library(rvest)
library(dplyr)
library(purrr)

# URL of the newspaper site
url <- "https://blockclubchicago.org/"

# Read the HTML content
webpage <- read_html(url)

# Method 1: Use the .cat-links class (WordPress standard)
cat("=== METHOD 1: Using .cat-links ===\n")
categories_method1 <- webpage %>%
  html_elements(".cat-links a") %>%
  html_text(trim = TRUE)

cat("Categories found with .cat-links a:", length(categories_method1), "\n")
print(head(categories_method1, 10))

# Method 2: Extract from article containers with categories
cat("\n=== METHOD 2: Article-based extraction ===\n")

# Find article containers and extract data from each
articles <- webpage %>%
  html_elements("article") %>%
  map_dfr(~ {
    article_node <- .x
    
    # Extract headline
    headline <- article_node %>%
      html_elements(".entry-title a, h2 a, h1 a") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Extract link
    link <- article_node %>%
      html_elements(".entry-title a, h2 a, h1 a") %>%
      html_attr("href") %>%
      first()
    
    # Extract category using .cat-links
    category <- article_node %>%
      html_elements(".cat-links a") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = ", ")
    
    # Extract author
    author <- article_node %>%
      html_elements(".byline, .author, .entry-author") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Extract date if available
    date <- article_node %>%
      html_elements(".entry-date, .published, time") %>%
      html_text(trim = TRUE) %>%
      first()
    
    tibble(
      headline = ifelse(length(headline) == 0 || is.na(headline), NA, headline),
      link = ifelse(length(link) == 0 || is.na(link), NA, link),
      category = ifelse(category == "", NA, category),
      author = ifelse(length(author) == 0 || is.na(author), NA, author),
      date = ifelse(length(date) == 0 || is.na(date), NA, date)
    )
  })

cat("Articles found:", nrow(articles), "\n")
print(articles)

# Method 3: Alternative approach - look for category in post classes
cat("\n=== METHOD 3: Using post classes ===\n")

articles_alt <- webpage %>%
  html_elements("article[class*='category-'], .post[class*='category-']") %>%
  map_dfr(~ {
    article_node <- .x
    
    # Extract category from the article's class attribute
    article_classes <- article_node %>% html_attr("class")
    category_classes <- str_extract_all(article_classes, "category-[a-z0-9-]+")[[1]]
    category_from_class <- category_classes %>%
      str_replace("category-", "") %>%
      str_replace_all("-", " ") %>%
      str_to_title() %>%
      paste(collapse = ", ")
    
    # Extract other data
    headline <- article_node %>%
      html_elements(".entry-title a, h2 a, h1 a") %>%
      html_text(trim = TRUE) %>%
      first()
    
    link <- article_node %>%
      html_elements(".entry-title a, h2 a, h1 a") %>%
      html_attr("href") %>%
      first()
    
    # Try .cat-links first, then fall back to class-based category
    category_from_links <- article_node %>%
      html_elements(".cat-links a") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = ", ")
    
    category <- ifelse(category_from_links != "", category_from_links, category_from_class)
    
    tibble(
      headline = ifelse(length(headline) == 0 || is.na(headline), NA, headline),
      link = ifelse(length(link) == 0 || is.na(link), NA, link),
      category = ifelse(category == "", NA, category),
      category_method = ifelse(category_from_links != "", "cat-links", "class-based")
    )
  })

cat("Articles found with class-based categories:", nrow(articles_alt), "\n")
print(articles_alt)

# Method 4: Direct category link extraction
cat("\n=== METHOD 4: Direct category links ===\n")
category_links <- webpage %>%
  html_elements("a[href*='/category/']") %>%
  map_dfr(~ {
    link_node <- .x
    category_name <- link_node %>% html_text(trim = TRUE)
    category_url <- link_node %>% html_attr("href")
    
    tibble(
      category = category_name,
      category_url = category_url
    )
  }) %>%
  distinct() %>%
  filter(category != "" & !is.na(category))

cat("Unique category links found:", nrow(category_links), "\n")
print(category_links)

# Summary
cat("\n=== SUMMARY ===\n")
cat("Choose the method that works best:\n")
cat("Method 1 (.cat-links): ", length(categories_method1), " categories\n")
cat("Method 2 (article extraction): ", nrow(articles), " articles with categories\n")
cat("Method 3 (class-based): ", nrow(articles_alt), " articles with categories\n")
cat("Method 4 (direct links): ", nrow(category_links), " unique category links\n")


##############

#' Scrape Block Club Chicago articles with categories/neighborhoods
#' @param url The URL to scrape (defaults to homepage)
#' @return A tibble with article data including neighborhoods/categories
scrape_blockclub_chicago <- function(url = "https://blockclubchicago.org/") {
  
  cat("Scraping Block Club Chicago from:", url, "\n")
  
  # Read the HTML content
  webpage <- read_html(url)
  
  # Extract articles with associated category data
  articles <- webpage %>%
    html_elements("article") %>%
    map_dfr(~ {
      article_node <- .x
      
      # Extract headline
      headline <- article_node %>%
        html_elements(".entry-title a, h2 a, h1 a") %>%
        html_text(trim = TRUE) %>%
        first()
      
      # Extract link
      link <- article_node %>%
        html_elements(".entry-title a, h2 a, h1 a") %>%
        html_attr("href") %>%
        first()
      
      # Extract category/neighborhood using .cat-links (WordPress standard)
      neighborhoods <- article_node %>%
        html_elements(".cat-links a") %>%
        html_text(trim = TRUE) %>%
        paste(collapse = ", ")
      
      # Extract author
      author <- article_node %>%
        html_elements(".byline, .author, .entry-author, .author-name") %>%
        html_text(trim = TRUE) %>%
        first()
      
      # Extract date if available
      date <- article_node %>%
        html_elements(".entry-date, .published, time") %>%
        html_text(trim = TRUE) %>%
        first()
      
      # Extract excerpt if available
      excerpt <- article_node %>%
        html_elements(".entry-summary, .excerpt, p") %>%
        html_text(trim = TRUE) %>%
        first()
      
      # Clean up and return
      tibble(
        headline = ifelse(length(headline) == 0 || is.na(headline), NA, headline),
        link = ifelse(length(link) == 0 || is.na(link), NA, link),
        neighborhoods = ifelse(neighborhoods == "" || is.na(neighborhoods), NA, neighborhoods),
        author = ifelse(length(author) == 0 || is.na(author), NA, author),
        date = ifelse(length(date) == 0 || is.na(date), NA, date),
        excerpt = ifelse(length(excerpt) == 0 || is.na(excerpt), NA, excerpt)
      )
    }) %>%
    # Remove rows with no headline (likely not actual articles)
    filter(!is.na(headline)) %>%
    # Remove duplicates
    distinct(headline, .keep_all = TRUE)
  
  cat("Found", nrow(articles), "articles\n")
  cat("Articles with neighborhoods:", sum(!is.na(articles$neighborhoods)), "\n")
  
  return(articles)
}

# Run the scraper
articles_df <- scrape_blockclub_chicago()

# Display results
print(articles_df)

# Show neighborhood distribution
cat("\n=== NEIGHBORHOOD BREAKDOWN ===\n")
neighborhood_counts <- articles_df %>%
  filter(!is.na(neighborhoods)) %>%
  separate_rows(neighborhoods, sep = ", ") %>%
  count(neighborhoods, sort = TRUE)

print(neighborhood_counts)

# Clean the data further if needed
articles_clean <- articles_df %>%
  # Remove articles without neighborhoods if you only want neighborhood-specific content
  filter(!is.na(neighborhoods)) %>%
  # Clean up neighborhoods column - split multiple neighborhoods into separate rows if needed
  separate_rows(neighborhoods, sep = ", ") %>%
  # Clean neighborhood names
  mutate(
    neighborhoods = str_trim(neighborhoods),
    neighborhoods = str_to_title(neighborhoods)
  ) %>%
  # Remove any empty neighborhood values
  filter(neighborhoods != "" & !is.na(neighborhoods))

cat("\n=== CLEANED DATA (one row per article-neighborhood combination) ===\n")
print(articles_clean)

# Optional: Add geocoding if you want coordinates for neighborhoods
# Uncomment the following lines if you want to geocode neighborhoods

cat("\n=== ADDING GEOCODING ===\n")
neighborhoods_unique <- unique(articles_clean$neighborhoods)

# Geocode neighborhoods (add "Chicago, IL" to improve accuracy)
neighborhoods_geocoded <- tibble(neighborhoods = neighborhoods_unique) %>%
  mutate(full_address = paste(neighborhoods, "Chicago, IL")) %>%
  geocode(full_address, method = 'osm', lat = latitude, long = longitude) %>%
  select(neighborhoods, latitude, longitude)

# Join back to main data
articles_with_coords <- articles_clean %>%
  left_join(neighborhoods_geocoded, by = "neighborhoods")

print(articles_with_coords)

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Total articles scraped:", nrow(articles_df), "\n")
cat("Articles with neighborhood tags:", sum(!is.na(articles_df$neighborhoods)), "\n")
cat("Articles with author info:", sum(!is.na(articles_df$author)), "\n")
cat("Articles with dates:", sum(!is.na(articles_df$date)), "\n")
cat("Unique neighborhoods mentioned:", length(unique(articles_clean$neighborhoods)), "\n")

# Show sample of final data
cat("\n=== SAMPLE OF FINAL DATA ===\n")
print(head(articles_clean, 10))
articles_with_coords

write_csv(articles_with_coords, "blockclub_articles.csv")


########### ---- streamlined working ----
library(rvest)
library(dplyr)
library(purrr)
library(readr)

#' Scrape full content from a single Block Club Chicago article
#' @param article_url URL of the individual article
#' @return A tibble with full article data
scrape_single_article <- function(article_url) {
  
  cat("Scraping article:", article_url, "\n")
  
  # Add delay to be respectful to the server
  Sys.sleep(1)
  
  tryCatch({
    # Read the article page
    article_page <- read_html(article_url)
    
    # Extract detailed article information
    headline <- article_page %>%
      html_elements("h1.entry-title, .entry-header h1, h1") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Full article text (multiple paragraphs)
    article_text <- article_page %>%
      html_elements(".entry-content p, .post-content p, .content p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " ")
    
    # Author information
    author <- article_page %>%
      html_elements(".author-name, .byline, .entry-author, .author") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Publication date
    pub_date <- article_page %>%
      html_elements(".entry-date, .published, time, .post-date") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Categories/neighborhoods
    categories <- article_page %>%
      html_elements(".cat-links a, .categories a") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = ", ")
    
    # Tags
    tags <- article_page %>%
      html_elements(".tags a, .post-tags a, .tag-links a") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = ", ")
    
    # Featured image
    featured_image <- article_page %>%
      html_elements(".featured-image img, .post-thumbnail img, .wp-post-image") %>%
      html_attr("src") %>%
      first()
    
    # Meta description
    meta_description <- article_page %>%
      html_elements("meta[name='description']") %>%
      html_attr("content") %>%
      first()
    
    # Word count
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

#' Scrape homepage then get full content for all articles
#' @param homepage_url The homepage URL
#' @param max_articles Maximum number of articles to scrape (for testing)
#' @return A tibble with full article data
scrape_all_articles <- function(homepage_url = "https://blockclubchicago.org/", max_articles = NULL) {
  
  cat("=== STEP 1: Getting article links from homepage ===\n")
  
  # Get all article links from homepage
  homepage <- read_html(homepage_url)
  
  article_links <- homepage %>%
    html_elements("article") %>%
    map_chr(~ {
      .x %>%
        html_elements(".entry-title a, h2 a, h1 a") %>%
        html_attr("href") %>%
        first()
    }) %>%
    na.omit() %>%
    unique()
  
  cat("Found", length(article_links), "article links on homepage\n")
  
  # Limit for testing if specified
  if (!is.null(max_articles)) {
    article_links <- head(article_links, max_articles)
    cat("Limited to", length(article_links), "articles for testing\n")
  }
  
  cat("\n=== STEP 2: Scraping full content for each article ===\n")
  
  # Progress tracking
  total_articles <- length(article_links)
  
  # Scrape each article and combine into one dataframe
  all_articles <- map_dfr(seq_along(article_links), ~ {
    i <- .x
    url <- article_links[i]
    
    cat("Progress:", i, "/", total_articles, "-", url, "\n")
    
    # Scrape this article
    article_data <- scrape_single_article(url)
    
    # Add progress info
    article_data$article_number <- i
    article_data$total_articles <- total_articles
    
    return(article_data)
  })
  
  cat("\n=== SCRAPING COMPLETE ===\n")
  cat("Successfully scraped", nrow(all_articles), "articles\n")
  cat("Total words across all articles:", sum(all_articles$word_count, na.rm = TRUE), "\n")
  
  return(all_articles)
}

#' Main function to run the complete scraping process
run_complete_scrape <- function(max_articles = NULL, save_file = "blockclub_all_articles.csv") {
  
  cat("🗞️ STARTING BLOCK CLUB CHICAGO COMPLETE SCRAPE 🗞️\n\n")
  
  # Scrape all articles
  all_articles <- scrape_all_articles(max_articles = max_articles)
  
  # Create output directory if needed
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # Save the complete dataset
  full_path <- file.path("data", save_file)
  write_csv(all_articles, full_path)
  
  cat("\n✅ COMPLETE! Saved", nrow(all_articles), "articles to:", full_path, "\n")
  
  # Show summary stats
  cat("\n=== SUMMARY STATISTICS ===\n")
  cat("Total articles:", nrow(all_articles), "\n")
  cat("Articles with full text:", sum(!is.na(all_articles$article_text)), "\n")
  cat("Articles with authors:", sum(!is.na(all_articles$author)), "\n")
  cat("Articles with categories:", sum(!is.na(all_articles$categories)), "\n")
  cat("Average words per article:", round(mean(all_articles$word_count, na.rm = TRUE)), "\n")
  cat("Date range:", min(all_articles$scraped_at, na.rm = TRUE), "to", max(all_articles$scraped_at, na.rm = TRUE), "\n")
  
  # Show unique categories
  all_categories <- all_articles %>%
    filter(!is.na(categories)) %>%
    separate_rows(categories, sep = ", ") %>%
    count(categories, sort = TRUE)
  
  cat("\nTop 10 neighborhoods/categories:\n")
  print(head(all_categories, 10))
  
  return(all_articles)
}

# === USAGE EXAMPLES ===

# Test with just 5 articles first
cat("=== TESTING WITH 5 ARTICLES ===\n")
test_articles <- run_complete_scrape(max_articles = 5, save_file = "test_articles.csv")

# When ready, scrape everything (this will take a while!)
# cat("=== SCRAPING ALL ARTICLES (THIS WILL TAKE TIME!) ===\n")
# all_articles <- run_complete_scrape(save_file = "blockclub_complete_dataset.csv")

cat("\n🎉 Ready to scrape! Uncomment the last lines to scrape all articles.\n")
# Scrape ALL articles (this will take time!)
#all_articles <- run_complete_scrape(save_file = "blockclub_complete_dataset.csv")

# Test with just 5 articles
test_articles <- run_complete_scrape(max_articles = 5, save_file = "test_articles.csv")



#---- stream --
# with geocoding

library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(tidygeocoder)
library(stringr)

#' Scrape full content from a single Block Club Chicago article
#' @param article_url URL of the individual article
#' @return A tibble with full article data
scrape_single_article <- function(article_url) {
  
  cat("Scraping article:", article_url, "\n")
  
  # Add delay to be respectful to the server
  Sys.sleep(1)
  
  tryCatch({
    # Read the article page
    article_page <- read_html(article_url)
    
    # Extract detailed article information
    headline <- article_page %>%
      html_elements("h1.entry-title, .entry-header h1, h1") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Full article text (multiple paragraphs)
    article_text <- article_page %>%
      html_elements(".entry-content p, .post-content p, .content p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " ")
    
    # Author information
    author <- article_page %>%
      html_elements(".author-name, .byline, .entry-author, .author") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Publication date
    pub_date <- article_page %>%
      html_elements(".entry-date, .published, time, .post-date") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Categories/neighborhoods
    categories <- article_page %>%
      html_elements(".cat-links a, .categories a") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = ", ")
    
    # Tags
    tags <- article_page %>%
      html_elements(".tags a, .post-tags a, .tag-links a") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = ", ")
    
    # Featured image
    featured_image <- article_page %>%
      html_elements(".featured-image img, .post-thumbnail img, .wp-post-image") %>%
      html_attr("src") %>%
      first()
    
    # Meta description
    meta_description <- article_page %>%
      html_elements("meta[name='description']") %>%
      html_attr("content") %>%
      first()
    
    # Word count
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

#' Scrape homepage then get full content for all articles
#' @param homepage_url The homepage URL
#' @param max_articles Maximum number of articles to scrape (for testing)
#' @return A tibble with full article data
scrape_all_articles <- function(homepage_url = "https://blockclubchicago.org/", max_articles = NULL) {
  
  cat("=== STEP 1: Getting article links from homepage ===\n")
  
  # Get all article links from homepage
  homepage <- read_html(homepage_url)
  
  article_links <- homepage %>%
    html_elements("article") %>%
    map_chr(~ {
      .x %>%
        html_elements(".entry-title a, h2 a, h1 a") %>%
        html_attr("href") %>%
        first()
    }) %>%
    na.omit() %>%
    unique()
  
  cat("Found", length(article_links), "article links on homepage\n")
  
  # Limit for testing if specified
  if (!is.null(max_articles)) {
    article_links <- head(article_links, max_articles)
    cat("Limited to", length(article_links), "articles for testing\n")
  }
  
  cat("\n=== STEP 2: Scraping full content for each article ===\n")
  
  # Progress tracking
  total_articles <- length(article_links)
  
  # Scrape each article and combine into one dataframe
  all_articles <- map_dfr(seq_along(article_links), ~ {
    i <- .x
    url <- article_links[i]
    
    cat("Progress:", i, "/", total_articles, "-", url, "\n")
    
    # Scrape this article
    article_data <- scrape_single_article(url)
    
    # Add progress info
    article_data$article_number <- i
    article_data$total_articles <- total_articles
    
    return(article_data)
  })
  
  cat("\n=== SCRAPING COMPLETE ===\n")
  cat("Successfully scraped", nrow(all_articles), "articles\n")
  cat("Total words across all articles:", sum(all_articles$word_count, na.rm = TRUE), "\n")
  
  return(all_articles)
}

#' Geocode neighborhoods and add coordinates
#' @param articles_df Dataframe with articles
#' @return Dataframe with added latitude/longitude columns
add_geocoding <- function(articles_df) {
  
  cat("\n=== STEP 3: Adding geocoding for neighborhoods ===\n")
  
  # Extract unique neighborhoods from categories
  unique_neighborhoods <- articles_df %>%
    filter(!is.na(categories)) %>%
    separate_rows(categories, sep = ", ") %>%
    mutate(categories = str_trim(categories)) %>%
    filter(categories != "" & !is.na(categories)) %>%
    distinct(categories) %>%
    pull(categories)
  
  cat("Found", length(unique_neighborhoods), "unique neighborhoods to geocode\n")
  
  # Create geocoding lookup table
  neighborhood_coords <- tibble(neighborhood = unique_neighborhoods) %>%
    mutate(
      # Add "Chicago, IL" to improve geocoding accuracy
      full_address = paste(neighborhood, "Chicago, IL"),
      # Clean up some common neighborhood name issues
      full_address = case_when(
        str_detect(full_address, "Downtown") ~ "Downtown Chicago, IL",
        str_detect(full_address, "Loop") ~ "Chicago Loop, IL", 
        str_detect(full_address, "West Town") ~ "West Town Chicago, IL",
        str_detect(full_address, "South Loop") ~ "South Loop Chicago, IL",
        TRUE ~ full_address
      )
    )
  
  cat("Starting geocoding process...\n")
  
  # Geocode with error handling
  neighborhood_coords <- neighborhood_coords %>%
    mutate(
      geocode_attempt = map(full_address, ~ {
        cat("Geocoding:", .x, "\n")
        Sys.sleep(0.5)  # Be respectful to geocoding service
        
        tryCatch({
          result <- geocode(.x, method = 'osm', lat = latitude, long = longitude)
          return(result)
        }, error = function(e) {
          cat("  Failed:", e$message, "\n")
          return(tibble(latitude = NA, longitude = NA))
        })
      })
    ) %>%
    unnest(geocode_attempt) %>%
    select(neighborhood, latitude, longitude, full_address)
  
  # Show geocoding results
  successful_geocodes <- sum(!is.na(neighborhood_coords$latitude))
  cat("Successfully geocoded", successful_geocodes, "out of", nrow(neighborhood_coords), "neighborhoods\n")
  
  # Join coordinates back to articles
  articles_with_coords <- articles_df %>%
    # Create a row for each article-neighborhood combination
    separate_rows(categories, sep = ", ") %>%
    mutate(categories = str_trim(categories)) %>%
    filter(!is.na(categories) & categories != "") %>%
    # Join with coordinates
    left_join(neighborhood_coords, by = c("categories" = "neighborhood")) %>%
    # Rename for clarity
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

#' Main function to run the complete scraping process with geocoding
run_complete_scrape <- function(max_articles = NULL, save_file = "blockclub_all_articles.csv", include_geocoding = TRUE) {
  
  cat("🗞️ STARTING BLOCK CLUB CHICAGO COMPLETE SCRAPE 🗞️\n\n")
  
  # Scrape all articles
  all_articles <- scrape_all_articles(max_articles = max_articles)
  
  # Add geocoding if requested
  if (include_geocoding) {
    geocoding_results <- add_geocoding(all_articles)
    articles_final <- geocoding_results$articles_with_coords
    neighborhood_lookup <- geocoding_results$neighborhood_lookup
  } else {
    articles_final <- all_articles
    neighborhood_lookup <- NULL
  }
  
  # Create output directory if needed
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # Save the complete dataset
  full_path <- file.path("data", save_file)
  write_csv(articles_final, full_path)
  
  cat("\n✅ COMPLETE! Saved", nrow(articles_final), "records to:", full_path, "\n")
  
  # Save neighborhood lookup table separately
  if (!is.null(neighborhood_lookup)) {
    lookup_path <- file.path("data", "neighborhood_coordinates.csv")
    write_csv(neighborhood_lookup, lookup_path)
    cat("📍 Saved neighborhood coordinates to:", lookup_path, "\n")
  }
  
  # Show summary stats
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
  
  # Show neighborhood distribution
  if (include_geocoding) {
    neighborhood_counts <- articles_final %>%
      count(neighborhood, sort = TRUE)
    
    cat("\nTop 10 neighborhoods by article count:\n")
    print(head(neighborhood_counts, 10))
    
    # Show geocoding success rate
    geocoded_neighborhoods <- articles_final %>%
      distinct(neighborhood, neighborhood_lat) %>%
      summarise(
        total = n(),
        geocoded = sum(!is.na(neighborhood_lat)),
        success_rate = round(geocoded/total * 100, 1)
      )
    
    cat("\nGeocoding success rate:", geocoded_neighborhoods$success_rate, "%\n")
  }
  
  return(articles_final)
}

# === USAGE EXAMPLES ===

# Test with just 5 articles first (with geocoding)
cat("=== TESTING WITH 5 ARTICLES + GEOCODING ===\n")
test_articles <- run_complete_scrape(
  max_articles = 5, 
  save_file = "test_articles_with_coords.csv",
  include_geocoding = TRUE
)

# Test without geocoding (faster)
# test_articles_no_geo <- run_complete_scrape(
#   max_articles = 5, 
#   save_file = "test_articles_no_coords.csv",
#   include_geocoding = FALSE
# )

# When ready, scrape everything with geocoding (this will take a LONG time!)
# cat("=== SCRAPING ALL ARTICLES + GEOCODING (THIS WILL TAKE A LONG TIME!) ===\n")
# all_articles <- run_complete_scrape(
#   save_file = "blockclub_complete_dataset_with_coords.csv",
#   include_geocoding = TRUE
# )

# Or scrape everything without geocoding (faster)
# all_articles_no_geo <- run_complete_scrape(
#   save_file = "blockclub_complete_dataset.csv",
#   include_geocoding = FALSE
# )

cat("\n🎉 Ready to scrape!\n")
cat("💡 TIP: Test with 5 articles first, then run the full scrape.\n")
cat("📍 Geocoding adds coordinates but makes it much slower.\n")

# === BONUS: Quick geocoding check function ===
check_geocoding_sample <- function() {
  cat("=== TESTING GEOCODING ON SAMPLE NEIGHBORHOODS ===\n")
  
  sample_neighborhoods <- c("Lincoln Park", "Wicker Park", "Logan Square", "Humboldt Park")
  
  test_coords <- tibble(neighborhood = sample_neighborhoods) %>%
    mutate(full_address = paste(neighborhood, "Chicago, IL")) %>%
    geocode(full_address, method = 'osm', lat = latitude, long = longitude)
  
  print(test_coords)
  
  if (any(!is.na(test_coords$latitude))) {
    cat("✅ Geocoding is working!\n")
  } else {
    cat("❌ Geocoding might have issues. Check your internet connection.\n")
  }
}

# Uncomment to test geocoding
check_geocoding_sample()


library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

#' Scrape full content from a single Block Club Chicago article
#' @param article_url URL of the individual article
#' @return A tibble with full article data
scrape_single_article <- function(article_url) {
  
  # Add delay to be respectful to the server
  Sys.sleep(1)
  
  tryCatch({
    # Read the article page
    article_page <- read_html(article_url)
    
    # Extract detailed article information
    headline <- article_page %>%
      html_elements("h1.entry-title, .entry-header h1, h1") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Full article text (multiple paragraphs)
    article_text <- article_page %>%
      html_elements(".entry-content p, .post-content p, .content p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " ")
    
    # Author information
    author <- article_page %>%
      html_elements(".author-name, .byline, .entry-author, .author") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Publication date
    pub_date <- article_page %>%
      html_elements(".entry-date, .published, time, .post-date") %>%
      html_text(trim = TRUE) %>%
      first()
    
    # Categories/neighborhoods using the working method
    neighborhoods <- article_page %>%
      html_elements(".cat-links a") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = ", ")
    
    # Tags using the working method  
    tags <- article_page %>%
      html_elements("a[href*='/tag/']") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = ", ")
    
    # Featured image
    featured_image <- article_page %>%
      html_elements(".featured-image img, .post-thumbnail img, .wp-post-image") %>%
      html_attr("src") %>%
      first()
    
    # Meta description
    meta_description <- article_page %>%
      html_elements("meta[name='description']") %>%
      html_attr("content") %>%
      first()
    
    # Word count
    word_count <- str_count(article_text, "\\S+")
    
    return(tibble(
      url = article_url,
      headline = ifelse(length(headline) == 0 || is.na(headline), NA, headline),
      author = ifelse(length(author) == 0 || is.na(author), NA, author),
      pub_date = ifelse(length(pub_date) == 0 || is.na(pub_date), NA, pub_date),
      neighborhoods = ifelse(neighborhoods == "" || is.na(neighborhoods), NA, neighborhoods),
      tags = ifelse(tags == "" || is.na(tags), NA, tags),
      article_text = ifelse(article_text == "" || is.na(article_text), NA, article_text),
      word_count = word_count,
      featured_image = ifelse(length(featured_image) == 0 || is.na(featured_image), NA, featured_image),
      meta_description = ifelse(length(meta_description) == 0 || is.na(meta_description), NA, meta_description),
      scraped_at = Sys.time()
    ))
    
  }, error = function(e) {
    return(tibble(
      url = article_url,
      headline = NA,
      author = NA,
      pub_date = NA,
      neighborhoods = NA,
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

#' Get article links using the working method from earlier
#' @param homepage_url The homepage URL
#' @param max_articles Maximum number of articles to get
#' @return Vector of article URLs
get_article_links <- function(homepage_url = "https://blockclubchicago.org/", max_articles = NULL) {
  
  homepage <- read_html(homepage_url)
  
  # Use the working method from Method 2
  article_links <- homepage %>%
    html_elements("article") %>%
    map_chr(~ {
      .x %>%
        html_elements(".entry-title a, h2 a, h1 a") %>%
        html_attr("href") %>%
        first()
    }) %>%
    na.omit() %>%
    unique()
  
  if (!is.null(max_articles)) {
    article_links <- head(article_links, max_articles)
  }
  
  return(article_links)
}

# Hardcoded coordinates for Chicago neighborhoods (no API calls)
get_chicago_neighborhood_coords <- function(neighborhood) {
  coords_lookup <- tribble(
    ~neighborhood, ~latitude, ~longitude,
    "Downtown", 41.8781, -87.6298,
    "Loop", 41.8781, -87.6298,
    "Lincoln Park", 41.9242, -87.6369,
    "Wicker Park", 41.9073, -87.6776,
    "Logan Square", 41.9294, -87.7073,
    "Humboldt Park", 41.9058, -87.7012,
    "West Town", 41.8955, -87.6731,
    "South Loop", 41.8708, -87.6252,
    "Lakeview", 41.9403, -87.6464,
    "Lincoln Square", 41.9684, -87.6890,
    "Hyde Park", 41.7940, -87.5940,
    "Pilsen", 41.8554, -87.6598,
    "Little Village", 41.8554, -87.7075,
    "Chinatown", 41.8526, -87.6323,
    "Bronzeville", 41.8206, -87.6186,
    "Austin", 41.8986, -87.7636,
    "Garfield Park", 41.8803, -87.7178,
    "North Lawndale", 41.8602, -87.7155,
    "Englewood", 41.7791, -87.6443,
    "Woodlawn", 41.7809, -87.5957,
    "Chatham", 41.7321, -87.6062,
    "Roseland", 41.6932, -87.6142,
    "Auburn Gresham", 41.7440, -87.6544,
    "Back of the Yards", 41.8140, -87.6564,
    "Brighton Park", 41.8174, -87.6950,
    "Uptown", 41.9659, -87.6501,
    "Edgewater", 41.9806, -87.6667,
    "Rogers Park", 42.0084, -87.6761,
    "Albany Park", 41.9689, -87.7139,
    "Irving Park", 41.9536, -87.7240,
    "Portage Park", 41.9536, -87.7610,
    "Jefferson Park", 41.9708, -87.7608,
    "Norwood Park", 41.9889, -87.7869,
    "Near North Side", 41.8955, -87.6236,
    "Near West Side", 41.8708, -87.6567,
    "Armour Square", 41.8390, -87.6317,
    "Douglas", 41.8206, -87.6086,
    "Oakland", 41.8117, -87.5985,
    "Grand Boulevard", 41.8117, -87.6186,
    "Washington Park", 41.7940, -87.6186,
    "Fuller Park", 41.8068, -87.6291,
    "Greater Grand Crossing", 41.7654, -87.6062,
    "Kenwood", 41.8068, -87.5957,
    "Washington Heights", 41.7206, -87.6544,
    "Mount Greenwood", 41.6932, -87.7075,
    "Morgan Park", 41.6932, -87.6734,
    "Beverly", 41.7206, -87.6734,
    "Ashburn", 41.7440, -87.7075,
    "West Lawn", 41.7673, -87.7240,
    "Chicago Lawn", 41.7673, -87.6950,
    "West Elsdon", 41.7940, -87.7240,
    "Gage Park", 41.7940, -87.6950,
    "Clearing", 41.7854, -87.7610,
    "West Garfield Park", 41.8803, -87.7377,
    "East Garfield Park", 41.8803, -87.6950,
    "Near South Side", 41.8390, -87.6236,
    "Citywide", 41.8781, -87.6298,
    "Arts & Culture", 41.8781, -87.6298,  # Use downtown for general categories
    "Belmont Cragin", 41.9289, -87.7665,
    "Hermosa", 41.9075, -87.7261,
    "Bridgeport", 41.8317, -87.6353,
    "McKinley Park", 41.8268, -87.6679,
    "Gage Park", 41.7940, -87.6950,
    "Brighton Park", 41.8174, -87.6950,
    "Avondale", 41.9389, -87.7098,
    "Bucktown", 41.9181, -87.6789,
    "Old Town", 41.9105, -87.6331,
    "Wrigleyville", 41.9484, -87.6553,
    "Portage Park", 41.9536, -87.7610,
    "Belmont Cragin", 41.9289, -87.7665,
    "Hermosa", 41.9075, -87.7261,
    "North Center", 41.9478, -87.6789,
    "Irving Park", 41.9536, -87.7240,
    "Pullman", 41.7098, -87.6089,
    "South Shore", 41.7575, -87.5707,
    "East Side", 41.7098, -87.5440,
    "South Chicago", 41.7398, -87.5598
  )
  
  result <- coords_lookup %>%
    filter(str_to_lower(neighborhood) == str_to_lower(!!neighborhood)) %>%
    select(latitude, longitude)
  
  if (nrow(result) == 0) {
    return(tibble(latitude = NA, longitude = NA))
  }
  
  return(result)
}

#' Add coordinates using only hardcoded Chicago neighborhood data
#' @param articles_df Dataframe with articles and neighborhoods
#' @return Dataframe with coordinates added but keeping one row per article
add_geocoding_to_articles <- function(articles_df) {
  
  # Step 1: Get unique neighborhoods from the neighborhoods column
  unique_neighborhoods <- articles_df %>%
    filter(!is.na(neighborhoods)) %>%
    separate_rows(neighborhoods, sep = ", ") %>%
    mutate(neighborhoods = str_trim(neighborhoods)) %>%
    filter(neighborhoods != "" & !is.na(neighborhoods)) %>%
    distinct(neighborhoods) %>%
    pull(neighborhoods)
  
  if (length(unique_neighborhoods) == 0) {
    # If no neighborhoods found, add empty coordinate columns
    articles_df$neighborhood_lat <- NA
    articles_df$neighborhood_lng <- NA
    return(articles_df)
  }
  
  cat("Found", length(unique_neighborhoods), "unique neighborhoods to geocode\n")
  
  # Step 2: Create neighborhood lookup with hardcoded coordinates only
  neighborhood_coords <- tibble(neighborhood = unique_neighborhoods) %>%
    mutate(
      coords = map(neighborhood, ~ {
        cat("Getting coords for:", .x, "\n")
        
        # Use hardcoded Chicago neighborhood coordinates
        chicago_coords <- get_chicago_neighborhood_coords(.x)
        if (nrow(chicago_coords) > 0 && !is.na(chicago_coords$latitude[1])) {
          cat("  ✓ Found hardcoded coords\n")
          return(chicago_coords)
        }
        
        cat("  ✗ No coords available\n")
        return(tibble(latitude = NA, longitude = NA))
      })
    ) %>%
    unnest(coords) %>%
    select(neighborhood, latitude, longitude)
  
  successful_geocodes <- sum(!is.na(neighborhood_coords$latitude))
  cat("Successfully found coords for", successful_geocodes, "out of", nrow(neighborhood_coords), "neighborhoods\n")
  
  # Step 3: Join coordinates back to articles (using first neighborhood for coordinates)
  articles_with_coords <- articles_df %>%
    mutate(
      first_neighborhood = str_extract(neighborhoods, "^[^,]+") %>% str_trim()
    ) %>%
    left_join(neighborhood_coords, by = c("first_neighborhood" = "neighborhood")) %>%
    select(-first_neighborhood) %>%
    rename(
      neighborhood_lat = latitude,
      neighborhood_lng = longitude
    )
  
  return(articles_with_coords)
}

#' Complete scraping function that returns exact column structure requested
#' @param max_articles Maximum number of articles to scrape
#' @return Clean dataframe with exact columns requested
scrape_blockclub_complete <- function(max_articles = NULL) {
  
  # Step 1: Get article links from homepage
  article_links <- get_article_links(max_articles = max_articles)
  
  # Step 2: Scrape each article individually
  all_articles <- map_dfr(article_links, scrape_single_article)
  
  # Step 3: Add hardcoded coordinates
  articles_with_coords <- add_geocoding_to_articles(all_articles)
  
  # Step 4: Ensure exact column order and names
  final_df <- articles_with_coords %>%
    select(
      url,
      headline, 
      author,
      pub_date,
      neighborhoods,
      tags,
      article_text,
      word_count,
      featured_image,
      meta_description,
      scraped_at,
      neighborhood_lat,
      neighborhood_lng
    )
  
  return(final_df)
}

# Test with 5 articles
test_data <- scrape_blockclub_complete(max_articles = 5)
print(colnames(test_data))
print(test_data)

write_csv(test_data, "test_data_sample.csv")