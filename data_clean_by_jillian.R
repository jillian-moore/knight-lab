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

# flattened neighborhood mapping ----
neighborhood_mapping <- tribble(
  ~neighborhood, ~chi_community,
  "Austin", "Austin",
  "Austin", "West Garfield Park",
  "Austin", "East Garfield Park",
  "Broadview", "Austin",
  "Garfield Park", "West Garfield Park",
  "Garfield Park", "East Garfield Park",
  "North Lawndale", "North Lawndale",
  "Belmont Cragin", "Belmont Cragin",
  "Hermosa", "Hermosa",
  "Beverly", "Beverly",
  "Mount Greenwood", "Mount Greenwood",
  "Morgan Park", "Morgan Park",
  "Bridgeport", "Bridgeport",
  "Chinatown", "Armour Square",
  "McKinley Park", "McKinley Park",
  "Gage Park", "Gage Park",
  "Brighton Park", "Brighton Park",
  "Bronzeville", "Kenwood",
  "Bronzeville", "Grand Boulevard",
  "Bronzeville", "Washington Park",
  "Near South Side", "Near South Side",
  "Downtown", "Loop",
  "Loop", "Loop",
  "River North", "Near North Side",
  "Gold Coast", "Near North Side",
  "South Loop", "Near South Side",
  "South Loop", "Loop",
  "West Loop", "Near West Side",
  "Edgebrook", "Edgebrook",
  "Edison Park", "Edison Park",
  "Sauganash", "Sauganash",
  "Englewood", "Englewood",
  "Chatham", "Chatham",
  "Auburn Gresham", "Auburn Gresham",
  "Ashburn", "Ashburn",
  "Hyde Park", "Hyde Park",
  "Woodlawn", "Woodlawn",
  "South Shore", "South Shore",
  "Jefferson Park", "Jefferson Park",
  "Portage Park", "Portage Park",
  "Norwood Park", "Norwood Park",
  "O’Hare", "Other",
  "Lakeview", "Lake View",
  "Wrigleyville", "Lake View",
  "Northalsted", "Lake View",
  "Roscoe Village", "North Center",
  "Lincoln Park", "Lincoln Park",
  "Old Town", "Lincoln Park",
  "Lincoln Square", "Lincoln Square",
  "North Center", "North Center",
  "Irving Park", "Irving Park",
  "Albany Park", "Albany Park",
  "Ravenswood", "Lincoln Square",
  "Ravenswood", "North Center",
  "Logan Square", "Logan Square",
  "Humboldt Park", "Humboldt Park",
  "Avondale", "Avondale",
  "Midway", "Other",
  "West Lawn", "West Lawn",
  "Pilsen", "Lower West Side",
  "Little Village", "South Lawndale",
  "Back of the Yards", "Back of the Yards",
  "Roseland", "Roseland",
  "Pullman", "Pullman",
  "South Chicago", "South Chicago",
  "East Side", "East Side",
  "Hegewisch", "Hegewisch",
  "Uptown", "Uptown",
  "Edgewater", "Uptown",
  "Rogers Park", "Rogers Park",
  "Andersonville", "Lincoln Square",
  "Wicker Park", "West Town",
  "Bucktown", "West Town",
  "West Town", "West Town"
) |>
  mutate(
    neighborhood = str_to_lower(neighborhood),
    chi_community = str_to_lower(chi_community)
  )

api_clean <- api_clean |>
  mutate(
    sub_community_lower = str_to_lower(sub_community),
    # Extract fallback from parsely metadata
    fallback_community = str_trim(str_extract(slp_primary_category.name, "^[^,]+")),
    fallback_community = if_else(is.na(fallback_community) | fallback_community == "", 
                                 "Other", 
                                 fallback_community)
  ) |>
  # Join with mapping table
  left_join(
    neighborhood_mapping |> 
      group_by(neighborhood) |> 
      slice(1) |>  # Take first mapping if multiple exist
      ungroup(),
    by = c("sub_community_lower" = "neighborhood")
  ) |>
  # Use mapped value or fallback
  mutate(
    community = coalesce(chi_community, fallback_community)
  ) |>
  # Clean up helper columns
  select(-sub_community_lower, -fallback_community, -chi_community)


dim(api_clean)
# 28K rows
