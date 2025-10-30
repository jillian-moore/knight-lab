# PRIMARY DATA VIZ ----
# packages ----
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(stringr)
library(htmltools)
library(here)
library(tidyr)

# source data cleaning script ----
source("data_clean_by_jillian.R")

# prepare spatial data from cleaning script ----
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

# use article data from cleaning script
article_data <- api_detail %>%
  rename(topic_match = random_topic, article_date = date)

date_min <- date_range$min_date
date_max <- date_range$max_date

# topic choices (from cleaning script's topics vector)
topic_choices <- c("None", topics)

# demographics choices
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
      body { 
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background: #f5f7fa;
      }
      .title-panel { 
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 25px 30px;
        border-radius: 10px;
        margin-bottom: 25px;
        box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      }
      .title-panel h2 { 
        margin: 0; 
        font-weight: 300;
        font-size: 28px;
        letter-spacing: -0.5px;
      }
      .title-panel p { 
        margin: 8px 0 0 0; 
        opacity: 0.95; 
        font-size: 15px; 
      }
      .control-section {
        background: white;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 18px;
        border: 1px solid #e8ebf0;
      }
      .control-section h4 {
        margin-top: 0;
        color: #667eea;
        font-size: 15px;
        font-weight: 600;
        border-bottom: 2px solid #f0f2f5;
        padding-bottom: 10px;
        margin-bottom: 15px;
        letter-spacing: 0.3px;
      }
      .mode-badge {
        display: inline-block;
        padding: 5px 12px;
        border-radius: 14px;
        font-size: 11px;
        font-weight: 700;
        margin-left: 12px;
        letter-spacing: 0.5px;
      }
      .real-mode { 
        background: #d4edda; 
        color: #155724;
        border: 1px solid #c3e6cb;
      }
      .shiny-input-radiogroup {
        margin-top: 5px;
      }
      .radio label {
        padding-left: 5px;
        font-weight: 400;
      }
      .leaflet-popup-content {
        font-size: 13px;
      }
    "))
  ),
  
  div(class = "title-panel",
      h2("Chicago Community Coverage Explorer",
         span(class = "mode-badge real-mode", "LIVE DATA")),
      p("Visualizing Block Club Chicago article topics and census demographics across 77 neighborhoods")
  ),
  
  fluidRow(
    column(4,
           div(class = "control-section",
               h4("📰 Topic Selection"),
               selectInput("blue_var", NULL,
                           choices = topic_choices, 
                           selected = "None")
           ),
           
           div(class = "control-section",
               h4("📊 Demographics"),
               selectInput("demo_var", NULL,
                           choices = demo_choices, 
                           selected = "None")
           ),
           
           div(class = "control-section",
               h4("📏 Metric Type"),
               radioButtons("metric_type", NULL,
                            choices = c("Total Articles" = "total",
                                        "Articles per 1,000 People" = "per_capita"),
                            selected = "total")
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
           tags$div(style = "text-align: center; margin-top: 15px; font-size: 13px; color: #6c757d;",
                    paste0("📅 Data Range: ", format(date_min, "%b %Y"), " - ", format(date_max, "%b %Y")),
                    " • 📊 Census: ACS 2020-2024"
           )
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # Get base census data (one row per community)
  base_census_data <- reactive({
    chi_boundaries_sf %>%
      select(community, total_population, white, black_or_african_american, 
             american_indian_or_alaska_native, asian, native_hawaiian_or_pacific_islander,
             other_race, multiracial, hispanic_or_latino,
             under_25_000, x25_000_to_49_999, x50_000_to_74_999, 
             x75_000_to_125_000, x125_000,
             age_0_17, age_18_24, age_25_34, age_35_49, age_50_64, age_65_plus) %>%
      st_drop_geometry() %>%
      distinct(community, .keep_all = TRUE)
  })
  
  # Filter articles based on date and topic
  filtered_topic_data <- reactive({
    df <- article_data
    
    # Filter by date (all articles UP TO the selected date)
    if (!is.null(input$month_slider)) {
      df <- df %>% filter(article_date <= input$month_slider)
    }
    
    # Filter by topic if selected
    if (!is.null(input$blue_var) && input$blue_var != "None") {
      df <- df %>% filter(topic_match == input$blue_var)
    }
    
    # Count articles per community
    topic_summary <- df %>%
      group_by(community) %>%
      summarise(article_count = n(), .groups = "drop")
    
    # Ensure all communities are represented (even with 0 articles)
    all_communities <- data.frame(
      community = unique(chi_boundaries_sf$community),
      stringsAsFactors = FALSE
    )
    
    topic_summary <- all_communities %>%
      left_join(topic_summary, by = "community") %>%
      mutate(article_count = replace_na(article_count, 0))
    
    topic_summary
  })
  
  output$map <- renderLeaflet({
    leaflet(chi_boundaries_sf) |> 
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -87.65, lat = 41.84, zoom = 10)
  })
  
  observe({
    # Start with spatial data
    map_data <- chi_boundaries_sf
    
    # Get filtered article counts
    topic_data <- filtered_topic_data()
    
    # Get base census data
    census_data <- base_census_data()
    
    # Join article counts
    map_data <- map_data %>%
      left_join(topic_data, by = "community")
    
    # Join census data (to ensure we have clean demographic data)
    map_data <- map_data %>%
      select(-any_of(names(census_data)[-1])) %>%  # Remove duplicate demo columns
      left_join(census_data, by = "community")
    
    map_data$article_count <- replace_na(map_data$article_count, 0)
    
    # Calculate display value (total or per capita)
    if (input$metric_type == "per_capita") {
      map_data <- map_data %>%
        mutate(
          display_value = if_else(total_population > 0, 
                                  (article_count / total_population) * 1000, 
                                  0)
        )
    } else {
      map_data <- map_data %>%
        mutate(display_value = article_count)
    }
    
    n <- nrow(map_data)
    
    # BLUE intensity (based on article counts/metrics) - using sqrt scaling for better visual differentiation
    blue_intensity <- rep(0, n)
    if (!is.null(input$blue_var) && input$blue_var != "None") {
      max_val <- max(map_data$display_value, na.rm = TRUE)
      if (!is.infinite(max_val) && max_val > 0) {
        # Square root scaling for better mid-range differentiation
        normalized_vals <- map_data$display_value / max_val
        blue_intensity <- sqrt(normalized_vals)  # More gradual progression
      }
    }
    
    # YELLOW intensity (based on demographics) - using sqrt scaling
    yellow_intensity <- rep(0, n)
    if (!is.null(input$demo_var) && input$demo_var != "None" && input$demo_var %in% names(map_data)) {
      demo_vals <- suppressWarnings(as.numeric(map_data[[input$demo_var]]))
      demo_vals[is.na(demo_vals)] <- 0
      max_demo <- max(demo_vals, na.rm = TRUE)
      if (!is.infinite(max_demo) && max_demo > 0) {
        normalized_demo <- demo_vals / max_demo
        yellow_intensity <- sqrt(normalized_demo)
      }
    }
    
    # Determine fill colors with better color gradients
    fill_colors <- rep("#E8E8E8", n)
    
    if (!is.null(input$blue_var) && input$blue_var != "None" && 
        (is.null(input$demo_var) || input$demo_var == "None")) {
      # Only topic selected - blue gradient from light to dark
      fill_colors <- sapply(1:n, function(i) {
        intensity <- blue_intensity[i]
        if (intensity == 0) return("#E8E8E8")
        # Gradient from light blue (#B3D9FF) to dark blue (#0033A0)
        r <- round(179 + (0 - 179) * intensity)
        g <- round(217 + (51 - 217) * intensity)
        b <- round(255 + (160 - 255) * intensity)
        rgb(r, g, b, maxColorValue = 255)
      })
    } else if ((is.null(input$blue_var) || input$blue_var == "None") && 
               !is.null(input$demo_var) && input$demo_var != "None") {
      # Only demographic selected - yellow/orange gradient
      fill_colors <- sapply(1:n, function(i) {
        intensity <- yellow_intensity[i]
        if (intensity == 0) return("#E8E8E8")
        # Gradient from light yellow (#FFF4CC) to dark orange (#CC6600)
        r <- round(255 + (204 - 255) * intensity)
        g <- round(244 + (102 - 244) * intensity)
        b <- round(204 + (0 - 204) * intensity)
        rgb(r, g, b, maxColorValue = 255)
      })
    } else if (!is.null(input$blue_var) && input$blue_var != "None" && 
               !is.null(input$demo_var) && input$demo_var != "None") {
      # Both selected - sophisticated green blend
      fill_colors <- sapply(1:n, function(i) {
        blue_val <- blue_intensity[i]
        yellow_val <- yellow_intensity[i]
        if(blue_val == 0 && yellow_val == 0) return("#E8E8E8")
        
        # Calculate average intensity
        avg_intensity <- (blue_val + yellow_val) / 2
        
        # Determine color based on which is dominant
        total <- blue_val + yellow_val
        blue_weight <- blue_val / total
        
        if(blue_weight > 0.6) {
          # More blue - blue-green gradient
          # From light teal to dark blue-green
          r <- round(102 + (0 - 102) * avg_intensity)
          g <- round(204 + (128 - 204) * avg_intensity)
          b <- round(204 + (128 - 204) * avg_intensity)
        } else if(blue_weight < 0.4) {
          # More yellow - yellow-green gradient
          # From light lime to olive green
          r <- round(204 + (128 - 204) * avg_intensity)
          g <- round(255 + (160 - 255) * avg_intensity)
          b <- round(102 + (64 - 102) * avg_intensity)
        } else {
          # Balanced - true green gradient
          # From light green to forest green
          r <- round(144 + (34 - 144) * avg_intensity)
          g <- round(238 + (139 - 238) * avg_intensity)
          b <- round(144 + (34 - 144) * avg_intensity)
        }
        rgb(r, g, b, maxColorValue = 255)
      })
    }
    
    # Create labels based on mode and selections
    labels <- lapply(1:n, function(i) {
      row_data <- map_data[i, ]
      
      # Base label with community name
      label_text <- paste0("<b style='font-size: 14px;'>", str_to_title(row_data$community), "</b><br>")
      
      # Add metric info
      if (input$metric_type == "per_capita") {
        label_text <- paste0(
          label_text,
          "<span style='color: #667eea;'>📰 Articles per 1,000: </span>", 
          "<b>", round(row_data$display_value, 2), "</b><br>"
        )
        # Don't show redundant demographic count in per capita mode
      } else {
        # Total articles mode
        label_text <- paste0(
          label_text,
          "<span style='color: #667eea;'>📰 Total Articles: </span>", 
          "<b>", row_data$article_count, "</b><br>"
        )
        
        # Add demographic info if selected
        if (!is.null(input$demo_var) && input$demo_var != "None" && 
            input$demo_var %in% names(row_data)) {
          demo_value <- row_data[[input$demo_var]]
          demo_name <- names(demo_choices)[demo_choices == input$demo_var]
          label_text <- paste0(
            label_text,
            "<span style='color: #f39c12;'>📊 ", demo_name, ": </span>",
            "<b>", format(replace_na(demo_value, 0), big.mark = ","), "</b>"
          )
        }
      }
      
      HTML(label_text)
    })
    
    leafletProxy("map", data = map_data) |>
      clearShapes() |>
      addPolygons(
        fillColor = fill_colors,
        color = "#666",
        weight = 1.2,
        opacity = 0.8,
        fillOpacity = 0.75,
        label = labels,
        labelOptions = labelOptions(
          direction = "auto", 
          textsize = "13px", 
          sticky = TRUE,
          style = list(
            "padding" = "8px 12px",
            "border-radius" = "6px",
            "box-shadow" = "0 3px 10px rgba(0,0,0,0.2)"
          )
        ),
        highlightOptions = highlightOptions(
          weight = 2.5,
          color = "#000",
          opacity = 1,
          fillOpacity = 0.85,
          bringToFront = TRUE
        )
      )
  })
}

# RUN APP ----
shinyApp(ui, server)