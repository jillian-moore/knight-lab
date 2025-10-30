# PRIMARY DATA VIZ ----
# packages ----
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(scales)
library(RColorBrewer)
library(lubridate)
library(here)
library(tidyr)
library(janitor)
library(htmltools)

# source data cleaning script ----
source(here("data_clean_by_jillian.R"))

# canonical topics ----
topic_cols <- c(
  "arts_and_culture",
  "business",
  "crime_public_safety",
  "education",
  "food_restaurants",
  "health_environment",
  "housing",
  "immigration",
  "politics",
  "social_movements",
  "sports_recreation",
  "transportation_infrastructure"
)

# get article data with dates ----
if (!exists("article_topic_data")) {
  stop("❌ article_topic_data not found. Check data_clean_by_jillian.R creates this object.")
}

# determine min/max dates safely
date_min <- min(article_topic_data$article_date, na.rm = TRUE)
date_max <- max(article_topic_data$article_date, na.rm = TRUE)

# demographics ----
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
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .title-panel { 
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .title-panel h2 { margin: 0; font-weight: 300; }
      .title-panel p { margin: 5px 0 0 0; opacity: 0.9; font-size: 14px; }
      .control-section {
        background: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        margin-bottom: 15px;
      }
      .control-section h4 {
        margin-top: 0;
        color: #667eea;
        font-size: 16px;
        font-weight: 600;
        border-bottom: 2px solid #f0f0f0;
        padding-bottom: 8px;
      }
      .mode-badge {
        display: inline-block;
        padding: 4px 10px;
        border-radius: 12px;
        font-size: 11px;
        font-weight: 600;
        margin-left: 10px;
      }
      .real-mode { background: #d4edda; color: #155724; }
    "))
  ),
  
  div(class = "title-panel",
      h2("Chicago Community Coverage Explorer",
         span(class = "mode-badge real-mode", "LIVE")),
      p("Visualizing Block Club Chicago article topics and census demographics across 77 neighborhoods")
  ),
  
  fluidRow(
    column(4,
           div(class = "control-section",
               h4("📰 Topic Selection"),
               selectInput("blue_var", NULL,
                           choices = c("None", setNames(topic_cols, str_to_title(gsub("_", " ", topic_cols)))), 
                           selected = "None")
           ),
           
           div(class = "control-section",
               h4("📊 Demographics"),
               selectInput("demo_var", NULL,
                           choices = demo_choices, 
                           selected = "None")
           ),
           
           div(class = "control-section",
               h4("📅 Time Period"),
               sliderInput("month_slider", NULL,
                           min = date_min,
                           max = date_max,
                           value = date_max,
                           step = 31,
                           animate = animationOptions(interval = 2000, loop = TRUE))
           )
    ),
    
    column(8,
           leafletOutput("map", height = "750px"),
           tags$div(style = "text-align: center; margin-top: 15px; font-size: 13px; color: #999;",
                    paste0("Data: ", format(date_min, "%b %Y"), " - ", format(date_max, "%b %Y")),
                    " | Census: 2020-2024"
           )
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  filtered_topic_data <- reactive({
    df <- article_topic_data
    if (!is.null(input$month_slider)) {
      df <- df |> filter(article_date <= input$month_slider)
    }
    
    if (!is.null(input$blue_var) && input$blue_var != "None") {
      df <- df |> filter(topic_match == input$blue_var)
    }
    
    topic_summary <- df |>
      group_by(community) |>
      summarise(article_count = n(), .groups = "drop")
    
    topic_summary$article_count <- replace_na(topic_summary$article_count, 0)
    topic_summary
  })
  
  output$map <- renderLeaflet({
    leaflet(chi_boundaries) |> 
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -87.65, lat = 41.84, zoom = 10)
  })
  
  observe({
    map_data <- chi_boundaries
    topic_data <- filtered_topic_data()
    
    if(nrow(topic_data) > 0) {
      map_data <- map_data |> left_join(topic_data, by = "community")
    }
    
    map_data$article_count <- replace_na(map_data$article_count, 0)
    n <- nrow(map_data)
    
    # BLUE opacity
    blue_opacity <- rep(0, n)
    if (!is.null(input$blue_var) && input$blue_var != "None") {
      max_articles <- max(map_data$article_count, na.rm = TRUE)
      if (!is.infinite(max_articles) && max_articles > 0) {
        blue_opacity <- map_data$article_count / max_articles
      }
    }
    
    # YELLOW opacity
    yellow_opacity <- rep(0, n)
    if (!is.null(input$demo_var) && input$demo_var != "None" && input$demo_var %in% names(map_data)) {
      demo_vals <- suppressWarnings(as.numeric(map_data[[input$demo_var]]))
      demo_vals[is.na(demo_vals)] <- 0
      max_demo <- max(demo_vals, na.rm = TRUE)
      if (!is.infinite(max_demo) && max_demo > 0) {
        yellow_opacity <- demo_vals / max_demo
      }
    }
    
    # Determine fill colors
    fill_colors <- rep("#D9D9D9", n)
    fill_opacity <- rep(0.3, n)
    
    if (!is.null(input$blue_var) && input$blue_var != "None" && (is.null(input$demo_var) || input$demo_var == "None")) {
      fill_colors <- rep("#0066CC", n)
      fill_opacity <- pmax(blue_opacity * 0.85, 0.1)
    } else if ((is.null(input$blue_var) || input$blue_var == "None") && !is.null(input$demo_var) && input$demo_var != "None") {
      fill_colors <- rep("#FFD700", n)
      fill_opacity <- pmax(yellow_opacity * 0.85, 0.1)
    } else if (!is.null(input$blue_var) && input$blue_var != "None" && !is.null(input$demo_var) && input$demo_var != "None") {
      fill_colors <- sapply(1:n, function(i) {
        blue_val <- blue_opacity[i]
        yellow_val <- yellow_opacity[i]
        if(blue_val == 0 && yellow_val == 0) return("#D9D9D9")
        total <- blue_val + yellow_val
        blue_weight <- blue_val / total
        yellow_weight <- yellow_val / total
        if(blue_weight > yellow_weight){
          green_base_rgb <- c(64, 176, 80)
          blue_green_rgb <- c(32, 160, 192)
          shift_strength <- (blue_weight - 0.5) * 2
          blended_rgb <- green_base_rgb * (1 - shift_strength) + blue_green_rgb * shift_strength
        } else {
          green_base_rgb <- c(64, 176, 80)
          yellow_green_rgb <- c(144, 192, 64)
          shift_strength <- (yellow_weight - 0.5) * 2
          blended_rgb <- green_base_rgb * (1 - shift_strength) + yellow_green_rgb * shift_strength
        }
        rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3], maxColorValue = 255)
      })
      fill_opacity <- pmax(pmax(blue_opacity, yellow_opacity) * 0.85, 0.1)
    }
    
    leafletProxy("map", data = map_data) |>
      clearShapes() |>
      addPolygons(
        fillColor = fill_colors,
        color = "#555",
        weight = 1,
        fillOpacity = fill_opacity,
        label = ~lapply(paste0(
          "<b>", str_to_title(community), "</b><br>",
          "📰 Articles: ", article_count, "<br>",
          "📊 Demographic: ",
          ifelse(is.null(input$demo_var) || input$demo_var == "None", "None",
                 ifelse(input$demo_var %in% names(map_data),
                        format(replace_na(map_data[[input$demo_var]],0), big.mark = ","), "N/A"))
        ), htmltools::HTML),
        labelOptions = labelOptions(direction = "auto", textsize = "12px", sticky = TRUE)
      )
  })
}

# RUN APP ----
shinyApp(ui, server)
