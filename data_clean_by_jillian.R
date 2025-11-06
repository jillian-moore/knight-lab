# CLEANING THE DATA ----

# packages ----
library(tidyverse)
library(here)
library(conflicted)
library(sf)

# conflicts ----
conflicts_prefer(dplyr::filter)

# read in all data ----
api <- readRDS(here("data/api_scrape.rds"))

census <- read_csv(here("data/ACS_5_Year_Data_by_Community_Area_20251007.csv")) |> 
  janitor::clean_names()

chi_boundaries <- read_csv(here("data/chi_boundaries.csv")) |> 
  janitor::clean_names()

# census clean ----
census_clean <- census |> 
  mutate(
    age_0_17 = male_0_to_17 + female_0_to_17,
    age_18_24 = male_18_to_24 + female_18_to_24,
    age_25_34 = male_25_to_34 + female_25_to_34,
    age_35_49 = male_35_to_49 + female_35_to_49,
    age_50_64 = male_50_to_64 + female_50_to_64,
    age_65_plus = male_65 + female_65
  ) |> 
  rename("community" = "community_area")

# shapefile clean ----
if (!"the_geom" %in% names(chi_boundaries)) stop("❌ chi_boundaries.csv must include 'the_geom' column.")

# api clean ----
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

# flattened neighborhood mapping
neighborhood_mapping <- tribble(
  ~sub_community, ~chi_community,
  
  # Citywide
  "Citywide", "Chicago",
  "Chicago", "Chicago",
  
  # West Side
  "Austin", "Austin",
  "Austin", "West Garfield Park",
  "Austin", "East Garfield Park",
  "Broadview", "Austin",
  "Garfield Park", "West Garfield Park",
  "Garfield Park", "East Garfield Park",
  "North Lawndale", "North Lawndale",
  "South Lawndale", "South Lawndale",
  "Little Village", "South Lawndale",
  "West Lawn", "West Lawn",
  "West Loop", "Near West Side",
  "Near West Side", "Near West Side",
  "Humboldt Park", "Humboldt Park",
  "West Town", "West Town",
  "Wicker Park", "West Town",
  "Bucktown", "West Town",
  "Lower West Side", "Lower West Side",
  "Pilsen", "Lower West Side",
  
  # North Side
  "Rogers Park", "Rogers Park",
  "Edgewater", "Edgewater",
  "Uptown", "Uptown",
  "Bowmanville", "Lincoln Square",
  "Lincoln Square", "Lincoln Square",
  "Andersonville", "Lincoln Square",
  "Ravenswood", "Lincoln Square",
  "North Center", "North Center",
  "Ravenswood", "North Center",
  "Roscoe Village", "North Center",
  "Lake View", "Lake View",
  "Lakeview", "Lake View",
  "Wrigleyville", "Lake View",
  "Northalsted", "Lake View",
  "Lincoln Park", "Lincoln Park",
  "Old Town", "Lincoln Park",
  "Near North Side", "Near North Side",
  "River North", "Near North Side",
  "Gold Coast", "Near North Side",
  "Edison Park", "Edison Park",
  "Norwood Park", "Norwood Park",
  "Jefferson Park", "Jefferson Park",
  "Forest Glen", "Forest Glen",
  "North Park", "North Park",
  "Albany Park", "Albany Park",
  "Portage Park", "Portage Park",
  "Irving Park", "Irving Park",
  "Dunning", "Dunning",
  "Montclare", "Montclare",
  "Belmont Cragin", "Belmont Cragin",
  "Hermosa", "Hermosa",
  "Avondale", "Avondale",
  "Logan Square", "Logan Square",
  
  # Northwest Side
  "O'Hare", "O'Hare",
  "Edgebrook", "Edgebrook",
  "Sauganash", "Sauganash",
  
  # Central
  "Loop", "Loop",
  "City Hall", "Loop",
  "Downtown", "Loop",
  "South Loop", "Loop",
  "Near South Side", "Near South Side",
  "South Loop", "Near South Side",
  "Armour Square", "Armour Square",
  "Chinatown", "Armour Square",
  "Douglas", "Douglas",
  "Oakland", "Oakland",
  "Fuller Park", "Fuller Park",
  "Grand Boulevard", "Grand Boulevard",
  "Bronzeville", "Grand Boulevard",
  "Kenwood", "Kenwood",
  "Bronzeville", "Kenwood",
  "Washington Park", "Washington Park",
  "Bronzeville", "Washington Park",
  "Hyde Park", "Hyde Park",
  "Woodlawn", "Woodlawn",
  
  # South Side
  "Bridgeport", "Bridgeport",
  "McKinley Park", "McKinley Park",
  "Brighton Park", "Brighton Park",
  "Archer Heights", "Archer Heights",
  "Gage Park", "Gage Park",
  "Clearing", "Clearing",
  "West Elsdon", "West Elsdon",
  "Garfield Ridge", "Garfield Ridge",
  "Back of the Yards", "New City",
  "West Englewood", "West Englewood",
  "Englewood", "Englewood",
  "Greater Grand Crossing", "Greater Grand Crossing",
  "Ashburn", "Ashburn",
  "Auburn Gresham", "Auburn Gresham",
  "Beverly", "Beverly",
  "Washington Heights", "Washington Heights",
  "Mount Greenwood", "Mount Greenwood",
  "Morgan Park", "Morgan Park",
  "Chatham", "Chatham",
  "Avalon Park", "Avalon Park",
  "South Shore", "South Shore",
  "South Chicago", "South Chicago",
  "Burnside", "Burnside",
  "Calumet Heights", "Calumet Heights",
  "Roseland", "Roseland",
  "Pullman", "Pullman",
  "South Deering", "South Deering",
  "East Side", "East Side",
  "West Pullman", "West Pullman",
  "Riverdale", "Riverdale",
  "Hegewisch", "Hegewisch"
  ) |>
  mutate(
    sub_community = str_to_lower(str_trim(sub_community)), 
    chi_community = str_to_lower(str_trim(chi_community))  
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

# match neighborhoods row by row using our mapping
api_clean <- api_clean |>
  rowwise() |>
  mutate(
    sub_community = str_to_lower(str_trim(sub_community)),
    article_section_split = str_split(parsely.meta.articleSection, ",\\s*"),
    primary_category_split = str_split(slp_primary_category.name, ",\\s*")
  ) |>
  rowwise() |>
  mutate(
    matched_neighborhoods = list({
      # empty vector to store matched neighborhoods
      matches <- character()
      
      # does sub_community field match a neighborhood?
      if (!is.na(sub_community) && sub_community != "") {
        current_sub <- sub_community
        
        # look up sub_community in neighborhood_mapping
        sub_matches <- neighborhood_mapping |>
          filter(sub_community == current_sub | chi_community == current_sub) |>
          pull(chi_community) |>
          unique()
        
        # if matches found, ONLY use these
        if (length(sub_matches) > 0) {
          matches <- sub_matches
        }
      }
      
      # if no sub_community match, check ALL items in article sections
      if (length(matches) == 0 && length(article_section_split) > 0 && !is.null(article_section_split[[1]])) {
        sections_clean <- str_to_lower(str_trim(article_section_split[[1]]))
        sections_clean <- sections_clean[!is.na(sections_clean) & sections_clean != ""]
        
        # loop through EACH section and collect ALL matches
        for (section in sections_clean) {
          section_matches <- neighborhood_mapping |>
            filter(sub_community == section | chi_community == section) |>
            pull(chi_community) |>
            unique()
          matches <- c(matches, section_matches)
        }
      }
      
      # if still no matches, check ALL items in primary category
      if (length(matches) == 0 && length(primary_category_split) > 0 && !is.null(primary_category_split[[1]])) {
        categories_clean <- str_to_lower(str_trim(primary_category_split[[1]]))
        categories_clean <- categories_clean[!is.na(categories_clean) & categories_clean != ""]
        
        # loop through EACH category and collect ALL matches
        for (category in categories_clean) {
          category_matches <- neighborhood_mapping |>
            filter(sub_community == category | chi_community == category) |>
            pull(chi_community) |>
            unique()
          matches <- c(matches, category_matches)
        }
      }
      
      # return all unique matches found
      unique(matches)
    })
  ) |>
  ungroup()

# add separate neighborhood columns
api_clean <- api_clean |>
  mutate(
    neighborhood1 = map_chr(matched_neighborhoods, ~{if(length(.x) >= 1) .x[1] else NA_character_}),
    neighborhood2 = map_chr(matched_neighborhoods, ~{if(length(.x) >= 2) .x[2] else NA_character_}),
    neighborhood3 = map_chr(matched_neighborhoods, ~{if(length(.x) >= 3) .x[3] else NA_character_})
  ) |> 
  mutate(neighborhood1 = replace_na(neighborhood1, "chicago"))

# add topics (at random) ----
# selected topics
topics <- c(
  "Arts & Culture", "Business", "Crime & Public Safety", "Education",
  "Food & Restaurants", "Health & Environment", "Housing", "Immigration",
  "Politics", "Social Movements", "Sports & Recreation", "Transportation & Infrastructure"
)

api_clean <- api_clean |> 
  mutate(
    random_topic = sample(topics, size = n(), replace = TRUE)
  )

# merge files ----
# shapefile and census merge
chi_boundaries_clean <- chi_boundaries |> 
  select(community, the_geom, area_num_1, shape_area, shape_len) |> 
  left_join(census_clean, by = "community") |> 
  mutate(community = str_to_lower(community))

# pivot api longer and prepare date
api_long <- api_clean |> 
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    year_month = floor_date(date, "month")
  ) |> 
  pivot_longer(
    cols = starts_with("neighborhood"),
    names_to = "neighborhood_col",
    values_to = "community"
  ) |> 
  filter(!is.na(community) & community != "") |> 
  select(-neighborhood_col)

# keep individual article rows with dates
api_detail <- api_long |> 
  select(community, random_topic, date, year, month, year_month)

# get date range for slider
date_range <- api_long |> 
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE)
  )

# aggregate api data by community, topic, and date
api_summary <- api_long |> 
  group_by(community, random_topic, date) |> 
  summarise(article_count = n(), .groups = "drop")

# get total articles by community with date info
api_community_totals <- api_long |> 
  group_by(community) |> 
  summarise(
    total_articles = n(),
    .groups = "drop"
  )

# merge with spatial data
full_data <- chi_boundaries_clean |> 
  left_join(api_community_totals, by = "community") |> 
  left_join(api_summary, by = "community", relationship = "many-to-many")

# clean up and handle NAs
full_data <- full_data |> 
  select(-starts_with("female"), -starts_with("male")) |> 
  select(-ends_with(".y")) |> 
  rename_with(~str_remove(., "\\.x$"), ends_with(".x")) |> 
  mutate(
    total_articles = replace_na(total_articles, 0),
    article_count = replace_na(article_count, 0),
    random_topic = replace_na(random_topic, "No Coverage")
  )

# add per person calculations
full_data <- full_data |>
  mutate(
    # Overall articles per person
    articles_per_person = if_else(total_population > 0, 
                                  total_articles / total_population, 
                                  0),
    articles_per_1000 = if_else(total_population > 0, 
                                (total_articles / total_population) * 1000, 
                                0),
    
    # Articles per 1,000 by TOTAL POPULATION
    topic_articles_per_0_17 = if_else(age_0_17 > 0, 
                                      article_count / age_0_17, 
                                      0),
    topic_articles_per_18_24 = if_else(age_18_24 > 0, 
                                       article_count / age_18_24, 
                                       0),
    topic_articles_per_25_34 = if_else(age_25_34 > 0, 
                                       article_count / age_25_34, 
                                       0),
    topic_articles_per_35_49 = if_else(age_35_49 > 0, 
                                       article_count / age_35_49, 
                                       0),
    topic_articles_per_50_64 = if_else(age_50_64 > 0, 
                                       article_count / age_50_64, 
                                       0),
    topic_articles_per_65_plus = if_else(age_65_plus > 0, 
                                         article_count / age_65_plus, 
                                         0),
    
    # Articles per 1,000 by RACE/ETHNICITY
    articles_per_white = if_else(white > 0, 
                                 article_count / white * 1000, 
                                 0),
    articles_per_black = if_else(black_or_african_american > 0, 
                                 article_count / black_or_african_american * 1000, 
                                 0),
    articles_per_asian = if_else(asian > 0, 
                                 article_count / asian * 1000, 
                                 0),
    articles_per_native_american = if_else(american_indian_or_alaska_native > 0, 
                                           article_count / american_indian_or_alaska_native * 1000, 
                                           0),
    articles_per_pacific_islander = if_else(native_hawaiian_or_pacific_islander > 0, 
                                            article_count / native_hawaiian_or_pacific_islander * 1000, 
                                            0),
    articles_per_other_race = if_else(other_race > 0, 
                                      article_count / other_race * 1000, 
                                      0),
    articles_per_multiracial = if_else(multiracial > 0, 
                                       article_count / multiracial * 1000, 
                                       0),
    articles_per_hispanic = if_else(hispanic_or_latino > 0, 
                                    article_count / hispanic_or_latino * 1000, 
                                    0),
    articles_per_white_non_hispanic = if_else(white_not_hispanic_or_latino > 0, 
                                              article_count / white_not_hispanic_or_latino * 1000, 
                                              0),
    
    # Articles per 1,000 by INCOME bracket
    articles_per_under_25k = if_else(under_25_000 > 0, 
                                     article_count / under_25_000 * 1000, 
                                     0),
    articles_per_25k_to_50k = if_else(x25_000_to_49_999 > 0, 
                                      article_count / x25_000_to_49_999 * 1000, 
                                      0),
    articles_per_50k_to_75k = if_else(x50_000_to_74_999 > 0, 
                                      article_count / x50_000_to_74_999 * 1000, 
                                      0),
    articles_per_75k_to_125k = if_else(x75_000_to_125_000 > 0, 
                                       article_count / x75_000_to_125_000 * 1000, 
                                       0),
    articles_per_over_125k = if_else(x125_000 > 0, 
                                     article_count / x125_000 * 1000, 
                                     0)
  )

# prepare data for shiny app ----

# create chi_boundaries_sf (spatial data with census info)
chi_boundaries_sf <- chi_boundaries_clean |> 
  st_as_sf(wkt = "the_geom", crs = 4326) |> 
  select(community, total_population, white, black_or_african_american, 
         american_indian_or_alaska_native, asian, native_hawaiian_or_pacific_islander,
         other_race, multiracial, hispanic_or_latino,
         under_25_000, x25_000_to_49_999, x50_000_to_74_999, 
         x75_000_to_125_000, x125_000,
         age_0_17, age_18_24, age_25_34, age_35_49, age_50_64, age_65_plus,
         the_geom) |> 
  distinct(community, .keep_all = TRUE)

# create article_data (long format with one row per article-community)
article_data <- api_detail |> 
  rename(
    article_date = date,
    topic_match = random_topic
  ) |> 
  select(community, topic_match, article_date, year, month, year_month)

# 3. create topics vector
topics <- c(
  "Arts & Culture", "Business", "Crime & Public Safety", "Education",
  "Food & Restaurants", "Health & Environment", "Housing", "Immigration",
  "Politics", "Social Movements", "Sports & Recreation", "Transportation & Infrastructure"
)

# name demographic vars
demo_choices <- c(
  "None" = "None",
  "Total Population" = "total_population",
  "White" = "white",
  "Black/African American" = "black_or_african_american",
  "Asian" = "asian",
  "Native American" = "american_indian_or_alaska_native",
  "Pacific Islander" = "native_hawaiian_or_pacific_islander",
  "Other Race" = "other_race",
  "Multiracial" = "multiracial",
  "Hispanic/Latino" = "hispanic_or_latino",
  "Income: Under $25k" = "under_25_000",
  "Income: $25k-$50k" = "x25_000_to_49_999",
  "Income: $50k-$75k" = "x50_000_to_74_999",
  "Income: $75k-$125k" = "x75_000_to_125_000",
  "Income: Over $125k" = "x125_000",
  "Age: 0-17" = "age_0_17",
  "Age: 18-24" = "age_18_24",
  "Age: 25-34" = "age_25_34",
  "Age: 35-49" = "age_35_49",
  "Age: 50-64" = "age_50_64",
  "Age: 65+" = "age_65_plus"
)

# get date range
date_range <- api_detail |> 
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE)
  )

# save out ----
save(
  full_data,
  chi_boundaries_sf,
  article_data,
  date_range,
  topics,
  demo_choices,
  file = here("data/full_data.rda")
)