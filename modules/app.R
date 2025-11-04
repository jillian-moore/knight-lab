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