library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(stringr)
library(scales)
library(lubridate)
library(here)

# Load data
load(here("data/full_data.rda"))

# Source module
source("R/map_module.R")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Chicago News Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map Visualization", tabName = "map", icon = icon("map")),
      menuItem("Data Tables", tabName = "tables", icon = icon("table")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              h2("Topic & Demographic Coverage Map"),
              mapModuleUI("main_map")
      ),
      tabItem(tabName = "tables",
              h2("Data Explorer")
              # Add other content here
      ),
      tabItem(tabName = "analytics",
              h2("Coverage Analytics")
              # Add other content here
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive data source
  chicago_data <- reactive({
    full_data
  })
  
  # Call map module
  mapModuleServer("main_map", chicago_data)
}

shinyApp(ui, server)