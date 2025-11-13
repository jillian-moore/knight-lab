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
library(bslib)
library(fontawesome)
library(DT)  # Added for dataTableOutput

logo_base64 <- base64enc::base64encode(here("www/lnllogowhiterectangle.jpeg"))

# source data clean ----
load(here("data/full_data.rda"))

# source modules ----
source(here("modules/module1.R"))
source(here("modules/module2.R"))
source(here("modules/module3.R"))  # Added third module

# UI ----
ui <- fluidPage(
  
  # MAIN TITLE BELOW LOGO
  div(
    style = "text-align: center; margin-top: 75px; margin-bottom: 15px;",
    h1("Chicago Community Analytics", 
       style = "font-family: 'Crimson Text', serif; font-weight: 700; font-size: 72px; color: #dd5600;"),
    h4("Explore Chicago neighborhoods news coverage and demographics", 
       style = "font-family: 'Lato', sans-serif; font-weight: 400; color: #333333;")
  ),
  
  # NAVBAR WITH TABS
  navbarPage(
    title = NULL,
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
    
    # Add custom CSS to ensure images load
    tags$head(
      tags$style(HTML("
        .navbar-brand img {
          display: inline-block;
          vertical-align: middle;
        }
      "))
    ),
    
    # Dropdown menu for visualizations
    navbarMenu(
      "Tabs",
      icon = icon("chart-bar"),
      
      tabPanel(
        "Map Visualization",
        icon = icon("map"),
        value = "map_tab",
        mapExplorerUI("map_tab")
      ),
      
      tabPanel(
        "Ward Comparison",
        icon = icon("chart-line"),
        value = "comparison_tab",
        communityComparisonUI("comparison_tab")
      ),
      
      tabPanel(
        "Data Quality",
        icon = icon("clipboard-check"),
        value = "quality_tab",
        dataQualityUI("quality_tab")
      )
    )
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
  
  # Data Quality Module
  dataQualityServer(
    "quality_tab",
    chi_boundaries_sf = chi_boundaries_sf,
    article_data = article_data
  )
}

# RUN APP ----
shinyApp(ui, server)