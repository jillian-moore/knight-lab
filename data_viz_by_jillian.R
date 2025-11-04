library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(stringr)
library(scales)
library(lubridate)
library(here)
library(geojsonio)  # for converting WKT to GeoJSON

# Load data
load(here("data/full_data.rda"))

# Get topics
topics <- full_data |> 
  filter(!is.na(random_topic) & random_topic != "No Coverage") |> 
  pull(random_topic) |> 
  unique() |> 
  sort()

# Helper function
safe_rescale <- function(x, to = c(0, 1)) {
  if (all(is.na(x)) || length(unique(x[!is.na(x)])) <= 1) {
    return(rep(0.5, length(x)))
  }
  scales::rescale(x, to = to, from = range(x, na.rm = TRUE, finite = TRUE))
}

# Demographic choices
demo_choices <- c(
  "None" = "None",
  "White" = "white",
  "Black or African American" = "black_or_african_american",
  "Asian" = "asian",
  "Hispanic or Latino" = "hispanic_or_latino",
  "Under $25,000" = "under_25_000",
  "$25,000 to $49,999" = "x25_000_to_49_999",
  "$50,000 to $74,999" = "x50_000_to_74_999",
  "$75,000 to $125,000" = "x75_000_to_125_000"
)

# Get date range
date_range <- full_data |> 
  summarise(min = min(date, na.rm = TRUE),
            max = max(date, na.rm = TRUE))

# Helper: Convert WKT to lat/lng coordinates for leaflet
wkt_to_coords <- function(wkt_text) {
  # Extract coordinates from MULTIPOLYGON or POLYGON WKT
  coords_str <- gsub(".*\\(\\(\\((.*)\\)\\)\\).*", "\\1", wkt_text)
  coords_pairs <- strsplit(coords_str, ", ")[[1]]
  
  coords_list <- lapply(coords_pairs, function(pair) {
    vals <- as.numeric(strsplit(pair, " ")[[1]])
    c(vals[2], vals[1])  # Leaflet wants [lat, lng]
  })
  
  list(coords_list)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Chicago News"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                column(4,
                       selectInput("topic_var", "Select Topic:",
                                   choices = c("None" = "None", setNames(topics, topics)),
                                   selected = "None")
                ),
                column(4,
                       selectInput("demo_var", "Select Demographic:",
                                   choices = demo_choices,
                                   selected = "None")
                ),
                column(4,
                       sliderInput("date_slider", "Articles Published Up To:",
                                   min = date_range$min,
                                   max = date_range$max,
                                   value = date_range$max,
                                   timeFormat = "%b %Y"),
                       checkboxInput("show_all_dates", "Show All Time Periods", value = TRUE)
                )
              ),
              fluidRow(
                column(12,
                       leafletOutput("map", height = "600px")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Prepare aggregated data (NO SF)
  map_data <- reactive({
    df <- full_data
    
    # Filter by date
    if (!input$show_all_dates && "date" %in% names(df)) {
      df <- df |> filter(date <= input$date_slider)
    }
    
    # Filter by topic if selected
    if (!is.null(input$topic_var) && input$topic_var != "None") {
      df <- df |> filter(random_topic == input$topic_var)
    }
    
    # Aggregate counts
    article_counts <- df |> 
      group_by(community, random_topic) |> 
      summarise(topic_articles = n(), .groups = "drop")
    
    total_counts <- df |> 
      group_by(community) |> 
      summarise(total_articles = n(), .groups = "drop")
    
    # Get unique community with demographics and geometry
    demo_cols <- names(df)[names(df) %in% demo_choices[demo_choices != "None"]]
    
    base_map <- df |> 
      select(community, the_geom, all_of(demo_cols)) |> 
      distinct(community, .keep_all = TRUE)
    
    # Join the counts back
    result <- base_map |> 
      left_join(article_counts, by = "community") |> 
      left_join(total_counts, by = "community") |> 
      mutate(
        topic_articles = replace_na(topic_articles, 0),
        total_articles = replace_na(total_articles, 0)
      )
    
    result
  })
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      setView(lng = -87.65, lat = 41.84, zoom = 10)
  })
  
  # Update map
  observe({
    df <- map_data()
    
    if (nrow(df) == 0) return()
    
    # Calculate intensities
    topic_intensity <- if (input$topic_var != "None" && "topic_articles" %in% names(df)) {
      safe_rescale(df$topic_articles)
    } else {
      rep(0, nrow(df))
    }
    
    demo_intensity <- if (input$demo_var != "None" && input$demo_var %in% names(df)) {
      demo_vals <- suppressWarnings(as.numeric(df[[input$demo_var]]))
      demo_vals[is.na(demo_vals)] <- 0
      safe_rescale(demo_vals)
    } else {
      rep(0, nrow(df))
    }
    
    # Create color gradients
    topic_gradient <- colorRampPalette(c("#DEEBF7", "#08519C"))(100)
    demo_gradient <- colorRampPalette(c("#FFF7BC", "#D95F0E"))(100)
    
    # Blend colors
    fill_colors <- sapply(1:nrow(df), function(i) {
      base_rgb <- c(1, 1, 1)
      
      if (topic_intensity[i] > 0) {
        topic_color <- topic_gradient[as.integer(topic_intensity[i] * 99) + 1]
        topic_rgb <- col2rgb(topic_color) / 255
        base_rgb <- (1 - topic_intensity[i]) * base_rgb + topic_intensity[i] * topic_rgb
      }
      
      if (demo_intensity[i] > 0) {
        demo_color <- demo_gradient[as.integer(demo_intensity[i] * 99) + 1]
        demo_rgb <- col2rgb(demo_color) / 255
        base_rgb <- (1 - demo_intensity[i]) * base_rgb + demo_intensity[i] * demo_rgb
      }
      
      rgb(base_rgb[1], base_rgb[2], base_rgb[3])
    })
    
    fill_opacity <- ifelse(topic_intensity > 0 | demo_intensity > 0, 0.7, 0.3)
    
    # Clear existing shapes
    proxy <- leafletProxy("map") |> clearShapes()
    
    # Add polygons from WKT text
    for (i in 1:nrow(df)) {
      # Parse WKT to coordinates
      wkt <- df$the_geom[i]
      
      # Simple regex to extract coordinates (works for most cases)
      coords_str <- gsub("MULTIPOLYGON \\(\\(\\((.*)\\)\\)\\)", "\\1", wkt)
      coords_pairs <- strsplit(coords_str, ", ")[[1]]
      
      coords_matrix <- do.call(rbind, lapply(coords_pairs, function(pair) {
        vals <- as.numeric(strsplit(pair, " ")[[1]])
        c(vals[2], vals[1])  # [lat, lng]
      }))
      
      proxy <- proxy |> 
        addPolygons(
          lng = coords_matrix[, 2],
          lat = coords_matrix[, 1],
          fillColor = fill_colors[i],
          fillOpacity = fill_opacity[i],
          color = "#555",
          weight = 1,
          label = htmltools::HTML(paste0(
            "<b>", str_to_title(df$community[i]), "</b><br>",
            if_else(input$topic_var != "None" & !is.na(df$random_topic[i]),
                    paste0("📰 ", df$random_topic[i], ": ", df$topic_articles[i], " articles<br>"),
                    ""),
            "Total Articles: ", df$total_articles[i]
          ))
        )
    }
  })
}

shinyApp(ui, server)