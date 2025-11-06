# global.R ----
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(sf)
library(tidyr)
library(conflicted)

# Handle conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(shinydashboard::box)
conflicts_prefer(shiny::observe)

# Load your data ----
# articles <- read_csv("your_data.csv") |>
#   mutate(publication_date = ymd_hms(publication_date))

# Example data structure (replace with your actual data)
set.seed(123)
articles <- data.frame(
  id = 1:500,
  publication_date = seq(as.POSIXct("2023-01-01"), as.POSIXct("2025-12-31"), length.out = 500),
  category = sample(c("Politics", "Economy", "Health", "Education", "Environment"), 500, replace = TRUE),
  region = sample(c("Northeast", "Southeast", "Midwest", "Southwest", "West"), 500, replace = TRUE),
  census_age_group = sample(c("18-25", "26-35", "36-45", "46-55", "56+"), 500, replace = TRUE),
  census_income = sample(c("Low", "Middle", "High"), 500, replace = TRUE),
  latitude = runif(500, 25, 49),
  longitude = runif(500, -125, -65),
  title = paste("Article", 1:500),
  stringsAsFactors = FALSE
)

# Get date range
min_date <- as.Date(min(articles$publication_date, na.rm = TRUE))
max_date <- as.Date(max(articles$publication_date, na.rm = TRUE))

# Custom theme colors
primary_color <- "#2c3e50"
secondary_color <- "#3498db"
accent_color <- "#e74c3c"

# ============================================================================
# ui.R ----
dashboardPage(
  
  # Header with custom height and logo
  dashboardHeader(
    title = span(
      "Your Dashboard Title",
      style = "font-size: 24px; font-weight: bold;"
    ),
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      style = "padding: 10px;",
      tags$img(
        src = "logo.png",  # Place logo.png in www/ folder
        height = "40px",
        style = "margin-top: 5px;"
      )
    )
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Article Analysis", tabName = "analysis", icon = icon("newspaper")),
      menuItem("Geographic Distribution", tabName = "map", icon = icon("map-marked-alt")),
      menuItem("Census Crosstabs", tabName = "crosstabs", icon = icon("table")),
      menuItem("How to Use", tabName = "howto", icon = icon("question-circle")),
      menuItem("Context & About", tabName = "context", icon = icon("info-circle"))
    ),
    
    hr(style = "border-color: #34495e;"),
    
    # Global Filters
    div(
      style = "padding: 15px;",
      
      h4("Filters", style = "color: white; margin-top: 0;"),
      
      dateRangeInput(
        "dateRange",
        "Publication Date Range:",
        start = min_date,
        end = max_date,
        min = min_date,
        max = max_date,
        width = "100%"
      ),
      
      selectInput(
        "filterCategory",
        "Category:",
        choices = c("All" = "", sort(unique(articles$category))),
        multiple = TRUE,
        width = "100%"
      ),
      
      selectInput(
        "filterRegion",
        "Region:",
        choices = c("All" = "", sort(unique(articles$region))),
        multiple = TRUE,
        width = "100%"
      ),
      
      selectInput(
        "filterAgeGroup",
        "Census Age Group:",
        choices = c("All" = "", sort(unique(articles$census_age_group))),
        multiple = TRUE,
        width = "100%"
      ),
      
      selectInput(
        "filterIncome",
        "Census Income Level:",
        choices = c("All" = "", sort(unique(articles$census_income))),
        multiple = TRUE,
        width = "100%"
      ),
      
      actionButton(
        "resetFilters",
        "Reset All Filters",
        icon = icon("redo"),
        class = "btn-warning",
        style = "width: 100%; margin-top: 10px;"
      )
    )
  ),
  
  # Body
  dashboardBody(
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        /* Header styling */
        .main-header .logo {
          height: 60px;
          font-size: 24px;
        }
        .main-header .navbar {
          height: 60px;
          min-height: 60px;
        }
        .content-wrapper {
          margin-top: 60px;
        }
        .main-header .sidebar-toggle {
          height: 60px;
        }
        
        /* Color scheme */
        .skin-blue .main-header .navbar {
          background-color: #2c3e50;
        }
        .skin-blue .main-header .logo {
          background-color: #34495e;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #2c3e50;
        }
        
        /* Box styling */
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .box-header {
          border-radius: 8px 8px 0 0;
        }
        
        /* Button styling */
        .btn {
          border-radius: 4px;
          font-weight: 500;
        }
        
        /* Value boxes */
        .small-box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        /* Sidebar */
        .sidebar-menu > li > a {
          font-size: 15px;
        }
        
        /* Info box styling */
        .info-box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      
      # Overview Tab ----
      tabItem(
        tabName = "overview",
        
        h2("Dashboard Overview", style = "margin-top: 0; color: #2c3e50;"),
        
        # Key Metrics
        fluidRow(
          valueBoxOutput("totalArticles", width = 3),
          valueBoxOutput("dateRangeBox", width = 3),
          valueBoxOutput("categoriesBox", width = 3),
          valueBoxOutput("regionsBox", width = 3)
        ),
        
        # Main visualizations
        fluidRow(
          box(
            title = "Articles Over Time",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("timeSeriesPlot", height = "350px")
          ),
          
          box(
            title = "Category Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("categoryPie", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Cumulative Publications",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("cumulativePlot", height = "300px")
          )
        )
      ),
      
      # Article Analysis Tab ----
      tabItem(
        tabName = "analysis",
        
        h2("Article Analysis", style = "margin-top: 0; color: #2c3e50;"),
        
        fluidRow(
          box(
            title = "Articles by Category and Region",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("categoryRegionPlot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Monthly Publication Trends",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("monthlyTrends", height = "350px")
          ),
          
          box(
            title = "Recent Articles",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DT::dataTableOutput("recentArticles")
          )
        )
      ),
      
      # Map Tab ----
      tabItem(
        tabName = "map",
        
        h2("Geographic Distribution", style = "margin-top: 0; color: #2c3e50;"),
        
        fluidRow(
          box(
            title = "Article Locations",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = 650,
            leafletOutput("articleMap", height = "580px")
          )
        ),
        
        fluidRow(
          box(
            title = "Regional Summary",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("regionalSummary")
          )
        )
      ),
      
      # Census Crosstabs Tab ----
      tabItem(
        tabName = "crosstabs",
        
        h2("Census Data Crosstabs", style = "margin-top: 0; color: #2c3e50;"),
        
        fluidRow(
          box(
            title = "Select Variables for Crosstab",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                6,
                selectInput(
                  "crosstab_row",
                  "Row Variable:",
                  choices = c(
                    "Category" = "category",
                    "Region" = "region",
                    "Age Group" = "census_age_group",
                    "Income Level" = "census_income"
                  ),
                  selected = "category"
                )
              ),
              column(
                6,
                selectInput(
                  "crosstab_col",
                  "Column Variable:",
                  choices = c(
                    "Category" = "category",
                    "Region" = "region",
                    "Age Group" = "census_age_group",
                    "Income Level" = "census_income"
                  ),
                  selected = "census_age_group"
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Crosstab Heatmap",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("crosstabHeatmap", height = "450px")
          ),
          
          box(
            title = "Crosstab Table",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            DT::dataTableOutput("crosstabTable")
          )
        )
      ),
      
      # How to Use Tab ----
      tabItem(
        tabName = "howto",
        
        h2("How to Use This Dashboard", style = "margin-top: 0; color: #2c3e50;"),
        
        fluidRow(
          box(
            width = 12,
            status = "info",
            
            h3(icon("filter"), " Using Filters"),
            p("The sidebar on the left contains global filters that apply to all visualizations:"),
            tags$ul(
              tags$li(strong("Date Range:"), "Select start and end dates to filter articles by publication date."),
              tags$li(strong("Category:"), "Filter articles by one or more categories."),
              tags$li(strong("Region:"), "Filter articles by geographic region."),
              tags$li(strong("Census Variables:"), "Filter by age group and income level."),
              tags$li(strong("Reset Button:"), "Click to clear all filters and return to the full dataset.")
            ),
            
            hr(),
            
            h3(icon("chart-line"), " Navigating Tabs"),
            tags$ul(
              tags$li(strong("Overview:"), "See key metrics and trends at a glance."),
              tags$li(strong("Article Analysis:"), "Explore detailed breakdowns by category and region."),
              tags$li(strong("Geographic Distribution:"), "View articles on an interactive map."),
              tags$li(strong("Census Crosstabs:"), "Analyze relationships between demographic variables."),
              tags$li(strong("Context & About:"), "Learn more about the data sources and methodology.")
            ),
            
            hr(),
            
            h3(icon("mouse-pointer"), " Interactive Features"),
            p("Most visualizations are interactive:"),
            tags$ul(
              tags$li("Hover over charts to see detailed values"),
              tags$li("Click and drag to zoom into specific areas"),
              tags$li("Double-click to reset the view"),
              tags$li("Click on legend items to show/hide categories"),
              tags$li("Click on map markers to see article details")
            )
          )
        )
      ),
      
      # Context Tab ----
      tabItem(
        tabName = "context",
        
        h2("Context & About", style = "margin-top: 0; color: #2c3e50;"),
        
        fluidRow(
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "About This Dashboard",
            
            h3("Project Overview"),
            p("Describe your project here. What is the purpose of this dashboard? 
              What questions does it help answer? Who is the intended audience?"),
            
            h3("Data Sources"),
            p("Provide information about your data sources:"),
            tags$ul(
              tags$li("Article data: [Your source]"),
              tags$li("Census data: [Your source]"),
              tags$li("Geographic data: [Your source]"),
              tags$li("Last updated: [Date]")
            ),
            
            h3("Methodology"),
            p("Explain any data processing, categorization, or analysis methods used."),
            
            h3("Contact & Credits"),
            p("Dashboard created by: [Your Name/Organization]"),
            p("For questions or feedback: [Contact Information]")
          ),
          
          box(
            width = 4,
            status = "info",
            solidHeader = TRUE,
            title = "Quick Stats",
            
            infoBoxOutput("totalArticlesInfo", width = 12),
            infoBoxOutput("dateRangeInfo", width = 12),
            infoBoxOutput("categoriesInfo", width = 12),
            
            hr(),
            
            h4("Data Quality"),
            p(textOutput("dataQualityText"))
          )
        )
      )
    )
  )
)