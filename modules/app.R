# MAIN DASHBOARD ----

# Load packages ----
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
library(DT)
library(base64enc)
conflicts_prefer(shiny::dataTableOutput)
conflicts_prefer(shiny::renderDataTable)

# Encode logo ----
logo_base64 <- base64enc::base64encode(here("www/lnllogowhiterectangle.jpeg"))

# Load data ----
load(here("data/full_data.rda"))

# Source modules ----
source(here("modules/module1.R"))   # mapExplorerUI / Server
source(here("modules/module2.R"))   # communityComparisonUI / Server
source(here("modules/module3.R"))   # dataQualityUI / Server
source(here("modules/module4.R"))   # contextTabUI / Server

# UI ----
ui <- fluidPage(
  
  # Global custom CSS ----
  tags$head(
    tags$style(HTML("
      .navbar-brand img {
        display: inline-block;
        vertical-align: middle;
        height: 40px;
      }
    "))
  ),
  
  # NAVBAR WITH TABS ----
  navbarPage(
    title = NULL,
    id = "main_navbar",
    windowTitle = "Chicago Community Analytics Dashboard",
    theme = bslib::bs_theme(
      version = 5,
      bg = "#f1f3f2",
      fg = "#333333",
      primary = "#dd5600",
      secondary = "#ffa914",
      base_font = bslib::font_google("Lato"),
      heading_font = bslib::font_google("Crimson Text")
    ),
    
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
        icon = icon("table"),
        value = "data_quality_tab",
        dataQualityUI("data_quality_tab")
      ),
      
      tabPanel(
        "About / Context",
        icon = icon("info-circle"),
        value = "context_tab",
        contextTabUI("context_tab")
      )
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # Reactive value for selected ward
  selected_ward1 <- reactiveVal(NULL)
  
  # Map Explorer Module
  mapExplorerServer(
    "map_tab",
    chi_boundaries_sf = chi_boundaries_sf,
    article_data = article_data,
    date_range = date_range,
    topics = topic_choices,
    demo_choices = demo_choices,
    on_compare_click = function(community_name) {
      selected_ward1(community_name)
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
    "data_quality_tab",
    chi_boundaries_sf = chi_boundaries_sf,
    article_data = article_data
  )
  
  # Context / About Tab Module
  contextTabServer("context_tab")
  
  # Optional: Suppress legacy DT warnings
  options(shiny.legacy.datatable = FALSE)
}

# RUN APP ----
shinyApp(ui, server)
