# CLEANING THE DATA ----
# packages ----
library(tidyverse)
library(here)
library(conflicted)

# conflicts ----
conflicts_prefer(dplyr::filter)

# read in data ----
api <- readRDS(here("data/api_scrape.rds"))
census <- read_csv(here("data/ACS_5_Year_Data_by_Community_Area_20251007.csv"))

api_clean <- api |>
  select(
    id, date, slug, status, link, author, 
    categories, tags, credibility_indicators, 
    title.rendered, content.rendered, excerpt.rendered,
    slp_primary_category.name, parsely.meta.articleSection,
    parsely.meta.keywords, 
    "yoast_head_json.twitter_misc.Est. reading time"
  ) |>
  mutate(
    sub_community = content.rendered |>
      str_remove_all("<[^>]+>") |>   
      str_extract("^[^—-]+") |>    
      str_trim()                    
  )

chi_communities <- c(
  "Rogers Park", "West Ridge", "Uptown", "Lincoln Square", 
  "North Center", "Lake View", "Lincoln Park", "Near North Side", 
  "Edison Park", "Norwood Park", "Jefferson Park", "Forest Glen", 
  "North Park", "Albany Park", "Portage Park", "Irving Park", 
  "Dunning", "Montclare", "Belmont Cragin", "Hermosa", 
  "Avondale", "Logan Square", "Humboldt Park", "West Town", 
  "Austin", "West Garfield Park", "East Garfield Park", "Near West Side",
  "North Lawndale", "South Lawndale", "Lower West Side", "Loop", 
  "Near South Side", "Armour Square", "Douglas", "Oakland", 
  "Fuller Park", "Grand Boulevard", "Kenwood", "Washington Park", 
  "Hyde Park", "Woodlawn", "South Shore", "Chatham", "Avalon Park",
  "South Chicago", "Burnside", "Calumet Heights", "Roseland", 
  "Pullman", "South Deering", "East Side", "West Pullman", 
  "Riverdale", "Hegewisch", "Garfield Ridge", "Archer Heights", 
  "Brighton Park", "McKinley Park", "Bridgeport", "New City", 
  "West Elsdon", "Gage Park", "Clearing", "West Lawn", "Chicago Lawn", 
  "West Englewood", "Englewood", "Greater Grand Crossing", 
  "Ashburn", "Auburn Gresham", "Beverly", "Washington Heights", 
  "Mount Greenwood", "Morgan Park", "Jeffery Manor", "East Morgan Park", 
  "West Morgan Park"
)

api_clean <- api_clean |>
  rowwise() |>
  mutate(
    community = case_when(
      sub_community %in% chi_communities ~ sub_community,
      TRUE ~ str_trim(str_extract(parsely.meta.articleSection, "^[^,]+"))
    )
  ) |>
  ungroup()
  

dim(api_clean)
# 28K rows
