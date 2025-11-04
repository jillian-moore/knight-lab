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
library(DT)  # For data tables in quality module
conflicts_prefer(DT::dataTableOutput)
conflicts_prefer(shiny::renderDataTable)
# source data clean ----
load(here("data/full_data.rda"))

# source modules ----
source(here("modules/module1.R"))
source(here("modules/module2.R"))
source(here("modules/module3.R"))

# UI ----
ui <- navbarPage(
  title = "Chicago Community Analytics Dashboard",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  id = "main_navbar",
  
  # Custom CSS for better navbar styling
  header = tags$head(
    tags$style(HTML("
      .navbar-brand {
        font-weight: 600;
        font-size: 20px;
      }
      .navbar {
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .nav-pills .nav-link {
        border-radius: 6px;
        margin: 0 4px;
      }
      .btn-nav-custom {
        margin: 8px 4px;
        border-radius: 6px;
        font-weight: 500;
        transition: all 0.2s;
      }
      .btn-nav-custom:hover {
        transform: translateY(-1px);
        box-shadow: 0 2px 8px rgba(0,0,0,0.15);
      }
    "))
  ),
  
  tabPanel(
    "Map Explorer",
    icon = icon("map"),
    value = "map_tab",
    
    # Navigation buttons at the top
    fluidRow(
      column(12,
             div(style = "text-align: right; padding: 10px 15px; background: #f8f9fa; border-bottom: 1px solid #dee2e6;",
                 actionButton(
                   "goto_comparison",
                   "Community Comparison →",
                   icon = icon("chart-line"),
                   class = "btn btn-primary btn-nav-custom"
                 ),
                 actionButton(
                   "goto_quality",
                   "Data Quality →",
                   icon = icon("check-circle"),
                   class = "btn btn-info btn-nav-custom"
                 )
             )
      )
    ),
    
    mapExplorerUI("map_explorer")
  ),
  
  tabPanel(
    "Community Comparison",
    icon = icon("chart-line"),
    value = "comparison_tab",
    
    # Navigation buttons at the top
    fluidRow(
      column(12,
             div(style = "text-align: left; padding: 10px 15px; background: #f8f9fa; border-bottom: 1px solid #dee2e6;",
                 actionButton(
                   "goto_map",
                   "← Map Explorer",
                   icon = icon("map"),
                   class = "btn btn-secondary btn-nav-custom"
                 ),
                 actionButton(
                   "goto_quality_from_comparison",
                   "Data Quality →",
                   icon = icon("check-circle"),
                   class = "btn btn-info btn-nav-custom"
                 )
             )
      )
    ),
    
    communityComparisonUI("comparison")
  ),
  
  tabPanel(
    "Data Quality",
    icon = icon("check-circle"),
    value = "quality_tab",
    
    # Navigation buttons at the top
    fluidRow(
      column(12,
             div(style = "text-align: left; padding: 10px 15px; background: #f8f9fa; border-bottom: 1px solid #dee2e6;",
                 actionButton(
                   "goto_map_from_quality",
                   "← Map Explorer",
                   icon = icon("map"),
                   class = "btn btn-secondary btn-nav-custom"
                 ),
                 actionButton(
                   "goto_comparison_from_quality",
                   "Community Comparison",
                   icon = icon("chart-line"),
                   class = "btn btn-primary btn-nav-custom"
                 )
             )
      )
    ),
    
    dataQualityUI("quality")
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # Navigation observers
  # From Map Explorer
  observeEvent(input$goto_comparison, {
    updateNavbarPage(session, "main_navbar", selected = "comparison_tab")
  })
  
  observeEvent(input$goto_quality, {
    updateNavbarPage(session, "main_navbar", selected = "quality_tab")
  })
  
  # From Community Comparison
  observeEvent(input$goto_map, {
    updateNavbarPage(session, "main_navbar", selected = "map_tab")
  })
  
  observeEvent(input$goto_quality_from_comparison, {
    updateNavbarPage(session, "main_navbar", selected = "quality_tab")
  })
  
  # From Data Quality
  observeEvent(input$goto_map_from_quality, {
    updateNavbarPage(session, "main_navbar", selected = "map_tab")
  })
  
  observeEvent(input$goto_comparison_from_quality, {
    updateNavbarPage(session, "main_navbar", selected = "comparison_tab")
  })
  
  # Map Explorer Module
  mapExplorerServer(
    "map_explorer",
    chi_boundaries_sf = chi_boundaries_sf,
    article_data = article_data,
    date_range = date_range,
    topics = topic_choices,
    demo_choices = demo_choices
  )
  
  # Community Comparison Module
  communityComparisonServer(
    "comparison",
    full_data = full_data
  )
  
  # Data Quality Module
  dataQualityServer(
    "quality",
    chi_boundaries_sf = chi_boundaries_sf,
    article_data = article_data
  )
}

# RUN APP ----
# For local network access, use:
# shinyApp(ui, server, options = list(host = "0.0.0.0", port = 3838))
# For local only:
shinyApp(ui, server)