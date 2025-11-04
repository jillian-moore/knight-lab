# MAIN DASHBOARD

# load packages ----
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(stringr)
library(htmltools)
library(here)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)

# source data clean ----
load(here("data/full_data.rda"))

# source modules ----
source(here("modules/module1.R"))
source(here("modules/module2.R"))

# PREPARE SPATIAL DATA (load once at startup for caching efficiency) ----
chi_boundaries_sf <- chi_boundaries_clean %>%
  mutate(
    the_geom_parsed = lapply(the_geom, function(wkt) {
      tryCatch(st_as_sfc(wkt, crs = 4326), error = function(e) NULL)
    })
  ) %>%
  filter(!sapply(the_geom_parsed, is.null)) %>%
  mutate(the_geom = st_sfc(do.call(c, the_geom_parsed), crs = 4326)) %>%
  select(-the_geom_parsed) %>%
  st_as_sf(sf_column_name = "the_geom")

# PREPARE ARTICLE DATA ----
article_data <- api_detail %>%
  rename(topic_match = random_topic, article_date = date)

# DATE RANGE ----
date_range <- list(
  min_date = date_range$min_date,
  max_date = date_range$max_date
)

# TOPIC CHOICES ----
topic_choices <- topics

# DEMOGRAPHICS CHOICES ----
demo_choices <- c(
  "None" = "None",
  "White" = "white",
  "Black or African American" = "black_or_african_american",
  "American Indian or Alaska Native" = "american_indian_or_alaska_native",
  "Asian" = "asian",
  "Native Hawaiian or Pacific Islander" = "native_hawaiian_or_pacific_islander",
  "Other Race" = "other_race",
  "Multiracial" = "multiracial",
  "Hispanic or Latino" = "hispanic_or_latino",
  "Under $25,000" = "under_25_000",
  "$25,000 to $49,999" = "x25_000_to_49_999",
  "$50,000 to $74,999" = "x50_000_to_74_999",
  "$75,000 to $125,000" = "x75_000_to_125_000",
  "$125,000 +" = "x125_000_plus",
  "0 to 17" = "age_0_17",
  "18 to 24" = "age_18_24",
  "25 to 34" = "age_25_34",
  "35 to 49" = "age_35_49",
  "50 to 64" = "age_50_64",
  "65+" = "age_65_plus"
)

# UI ----
ui <- navbarPage(
  title = "Chicago Community Analytics Dashboard",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  tabPanel(
    "Map Explorer",
    icon = icon("map"),
    mapExplorerUI("map_tab")
  ),
  
  tabPanel(
    "Community Comparison",
    icon = icon("chart-line"),
    communityComparisonUI("comparison_tab")
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # Map Explorer Module
  mapExplorerServer(
    "map_tab",
    chi_boundaries_sf = chi_boundaries_sf,
    article_data = article_data,
    date_range = date_range,
    topics = topic_choices,
    demo_choices = demo_choices
  )
  
  # Community Comparison Module
  communityComparisonServer(
    "comparison_tab",
    full_data = full_data
  )
}

# RUN APP ----
# For local network access, use:
# shinyApp(ui, server, options = list(host = "0.0.0.0", port = 3838))

# For local only:
shinyApp(ui, server)