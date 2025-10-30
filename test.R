# PRIMARY DATA VIZ ----
# packages ----
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(stringr)
library(htmltools)
library(here)

# source data cleaning script ----
source("data_clean_by_jillian.R")

# convert full_data to sf object
chi_boundaries_sf <- full_data %>%
  filter(!is.na(the_geom)) %>%
  distinct(community, .keep_all = TRUE) %>%
  mutate(
    the_geom_parsed = lapply(the_geom, function(wkt) {
      tryCatch(st_as_sfc(wkt, crs = 4326), error = function(e) NULL)
    })
  ) %>%
  filter(!sapply(the_geom_parsed, is.null)) %>%
  mutate(the_geom = st_sfc(do.call(c, the_geom_parsed), crs = 4326)) %>%
  select(-the_geom_parsed) %>%
  st_as_sf(sf_column_name = "the_geom")

# get date range
date_range_data <- full_data %>%
  summarise(min_date = min(date, na.rm = TRUE),
            max_date = max(date, na.rm = TRUE))

date_min <- date_range_data$min_date
date_max <- date_range_data$max_date

# topic choices
topic_choices <- c("None", sort(unique(full_data$random_topic[full_data$random_topic != "No Coverage"])))

# demographics choices
demo_choices <- c(
  "None" = "None",
  "Age 0-17" = "age_0_17",
  "Age 18-24" = "age_18_24",
  "Age 25-34" = "age_25_34",
  "Age 35-49" = "age_35_49",
  "Age 50-64" = "age_50_64",
  "Age 65+" = "age_65_plus",
  "White" = "white",
  "Black or African American" = "black_or_african_american",
  "American Indian or Alaska Native" = "american_indian_or_alaska_native",
  "Asian" = "asian",
  "Native Hawaiian or Pacific Islander" = "native_hawaiian_or_pacific_islander",
  "Other Race" = "other_race",
  "Multiracial" = "multiracial",
  "Hispanic or Latino" = "hispanic_or_latino",
  "White Non-Hispanic" = "white_not_hispanic_or_latino",
  "Income: Under $25,000" = "under_25_000",
  "Income: $25,000 to $49,999" = "x25_000_to_49_999",
  "Income: $50,000 to $74,999" = "x50_000_to_74_999",
  "Income: $75,000 to $125,000" = "x75_000_to_125_000",
  "Income: $125,000+" = "x125_000"
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
      .legend-box {
        background: white;
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .legend-item {
        display: flex;
        align-items: center;
        margin-bottom: 8px;
      }
      .legend-color {
        width: 30px;
        height: 20px;
        margin-right: 10px;
        border-radius: 3px;
        border: 1px solid #ddd;
      }
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
           ),
           
           div(class = "legend-box",
               h4("Color Legend"),
               div(class = "legend-item",
                   div(class = "legend-color", style = "background-color: #FFD700;"),
                   span("Demographics (Yellow)")
               ),
               div(class = "legend-item",
                   div(class = "legend-color", style = "background-color: #0066CC;"),
                   span("Topic Coverage (Blue)")
               ),
               div(class = "legend-item",
                   div(class = "legend-color", style = "background: linear-gradient(to right, #FFD700 0%, #0066CC 100%);"),
                   span("Combined (Blue over Yellow)")
               ),
               p(style = "font-size: 11px; color: #666; margin-top: 10px;",
                 "Darker = Higher values. Blue coverage layered over yellow demographics.")
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
  
  filtered_data <- reactive({
    # Filter by date
    df <- full_data %>% filter(date <= input$month_slider)
    
    # Filter by topic if not "None"
    if (!is.null(input$blue_var) && input$blue_var != "None") {
      df <- df %>% filter(random_topic == input$blue_var)
    }
    
    # Aggregate by community - sum article_count
    df_agg <- df %>%
      group_by(community) %>%
      summarise(
        article_count = sum(article_count, na.rm = TRUE),
        total_population = first(total_population),
        age_0_17 = first(age_0_17),
        age_18_24 = first(age_18_24),
        age_25_34 = first(age_25_34),
        age_35_49 = first(age_35_49),
        age_50_64 = first(age_50_64),
        age_65_plus = first(age_65_plus),
        white = first(white),
        black_or_african_american = first(black_or_african_american),
        american_indian_or_alaska_native = first(american_indian_or_alaska_native),
        asian = first(asian),
        native_hawaiian_or_pacific_islander = first(native_hawaiian_or_pacific_islander),
        other_race = first(other_race),
        multiracial = first(multiracial),
        hispanic_or_latino = first(hispanic_or_latino),
        white_not_hispanic_or_latino = first(white_not_hispanic_or_latino),
        under_25_000 = first(under_25_000),
        x25_000_to_49_999 = first(x25_000_to_49_999),
        x50_000_to_74_999 = first(x50_000_to_74_999),
        x75_000_to_125_000 = first(x75_000_to_125_000),
        x125_000 = first(x125_000),
        .groups = "drop"
      )
    
    df_agg$article_count <- replace_na(df_agg$article_count, 0)
    df_agg
  })
  
  output$map <- renderLeaflet({
    leaflet(chi_boundaries_sf) |> 
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -87.65, lat = 41.84, zoom = 10)
  })
  
  observe({
    map_data <- chi_boundaries_sf %>%
      left_join(filtered_data(), by = "community")
    
    map_data$article_count <- replace_na(map_data$article_count, 0)
    
    # Calculate display value based on metric type
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
    
    # BLUE opacity (topic coverage)
    blue_opacity <- rep(0, n)
    topic_selected <- !is.null(input$blue_var) && input$blue_var != "None"
    
    if (topic_selected) {
      max_val <- max(map_data$display_value, na.rm = TRUE)
      if (!is.infinite(max_val) && max_val > 0) {
        blue_opacity <- pmin(map_data$display_value / max_val, 1)
        blue_opacity[is.na(blue_opacity)] <- 0
      }
    }
    
    # YELLOW opacity (demographics)
    yellow_opacity <- rep(0, n)
    demo_selected <- !is.null(input$demo_var) && input$demo_var != "None" && input$demo_var %in% names(map_data)
    
    if (demo_selected) {
      demo_vals <- suppressWarnings(as.numeric(map_data[[input$demo_var]]))
      demo_vals[is.na(demo_vals)] <- 0
      max_demo <- max(demo_vals, na.rm = TRUE)
      if (!is.infinite(max_demo) && max_demo > 0) {
        yellow_opacity <- pmin(demo_vals / max_demo, 1)
        yellow_opacity[is.na(yellow_opacity)] <- 0
      }
    }
    
    # Determine fill colors - BLUE OVER YELLOW
    fill_colors <- rep("#D9D9D9", n)
    fill_opacity <- rep(0.3, n)
    
    if (topic_selected && demo_selected) {
      # Both selected - blue layered over yellow
      fill_colors <- sapply(1:n, function(i) {
        y_strength <- yellow_opacity[i]
        b_strength <- blue_opacity[i]
        
        if (y_strength == 0 && b_strength == 0) return("#D9D9D9")
        
        # Yellow as base layer
        yellow_rgb <- c(255, 215, 0)
        blue_rgb <- c(0, 102, 204)
        
        # Yellow base
        base_color <- yellow_rgb * y_strength + c(217, 217, 217) * (1 - y_strength)
        
        # Blue overlaid on top
        final_color <- blue_rgb * b_strength + base_color * (1 - b_strength)
        
        rgb(final_color[1], final_color[2], final_color[3], maxColorValue = 255)
      })
      fill_opacity <- pmax(pmax(blue_opacity, yellow_opacity) * 0.9, 0.15)
      
    } else if (topic_selected) {
      # Only topic selected - use blue
      fill_colors <- rep("#0066CC", n)
      fill_opacity <- pmax(blue_opacity * 0.9, 0.15)
      
    } else if (demo_selected) {
      # Only demographic selected - use yellow
      fill_colors <- rep("#FFD700", n)
      fill_opacity <- pmax(yellow_opacity * 0.9, 0.15)
    }
    
    # Create labels
    metric_label <- if(input$metric_type == "per_capita") {
      "Articles per 1,000"
    } else {
      "Total Articles"
    }
    
    demo_value <- if(demo_selected) {
      format(replace_na(map_data[[input$demo_var]], 0), big.mark = ",")
    } else {
      "None selected"
    }
    
    demo_name <- if(demo_selected) {
      names(demo_choices)[demo_choices == input$demo_var]
    } else {
      "Demographic"
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
          "📰 ", metric_label, ": ", round(display_value, 2), "<br>",
          "📊 ", demo_name, ": ", demo_value
        ), htmltools::HTML),
        labelOptions = labelOptions(direction = "auto", textsize = "12px", sticky = TRUE),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      )
  })
}

# RUN APP ----
shinyApp(ui, server)