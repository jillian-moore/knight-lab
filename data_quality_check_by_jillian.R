# DATA QUALITY CHECK ----

# load packages ----

# load data ----
source(here("data/full_data.rda"))

# event tag confirmation
# the average accuracy by each topic of the AI, in percent

# census data check
# the total people by community and each demographic
# bar graph

# API data check
# total articles per year

# neighborhood mapping
# make pretty list of my mapping and explain this methodology in plain English
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

