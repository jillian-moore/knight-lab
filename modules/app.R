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
  title = div(
    style = "display: flex; align-items: center; gap: 10px;",
    tags$img(src = "lnllogotransparent", height = "40px"),
    "Chicago Community Analytics"
  ),
  id = "main_navbar",
  windowTitle = "Chicago Community Analytics Dashboard",
  theme = bslib::bs_theme(
    version = 5,
    bg = "#f1f3f2",
    fg = "#333333",
    primary = "#dd5600",
    secondary = "#00bf7d",
    base_font = bslib::font_google("Lato"),
    heading_font = bslib::font_google("Crimson Text")
  ),
  
  tabPanel(
    "Map Explorer",
    icon = icon("map"),
    value = "map_tab",
    mapExplorerUI("map_tab")
  ),
  
  tabPanel(
    "Community Comparison",
    icon = icon("chart-line"),
    value = "comparison_tab",
    communityComparisonUI("comparison_tab")
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # Reactive value to store selected community from map click
  selected_ward1 <- reactiveVal(NULL)
  
  # Map Explorer Module with compare callback
  mapExplorerServer(
    "map_tab",
    chi_boundaries_sf = chi_boundaries_sf,
    article_data = article_data,
    date_range = date_range,
    topics = topic_choices,
    demo_choices = demo_choices,
    on_compare_click = function(community_name) {
      # Store the clicked community
      selected_ward1(community_name)
      
      # Switch to comparison tab
      updateNavbarPage(session, "main_navbar", selected = "comparison_tab")
    }
  )
  
  # Community Comparison Module
  communityComparisonServer(
    "comparison_tab",
    full_data = full_data,
    selected_ward1 = selected_ward1
  )
}

# RUN APP ----
shinyApp(ui, server)