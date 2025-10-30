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
library(ggplot2)

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
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
        background: #fafbfc;
        color: #24292e;
      }
      .title-panel { 
        background: linear-gradient(135deg, #4f46e5 0%, #7c3aed 100%);
        color: white;
        padding: 32px 40px;
        border-radius: 12px;
        margin-bottom: 28px;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
      }
      .title-panel h2 { 
        margin: 0; 
        font-weight: 600;
        font-size: 32px;
        letter-spacing: -0.02em;
      }
      .title-panel p { 
        margin: 10px 0 0 0; 
        opacity: 0.95; 
        font-size: 16px;
        font-weight: 300;
      }
      .control-section {
        background: white;
        padding: 24px;
        border-radius: 12px;
        box-shadow: 0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06);
        margin-bottom: 20px;
        border: 1px solid #e1e4e8;
      }
      .control-section h4 {
        margin: 0 0 16px 0;
        color: #4f46e5;
        font-size: 14px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.05em;
      }
      .mode-badge {
        display: inline-block;
        padding: 6px 14px;
        border-radius: 16px;
        font-size: 12px;
        font-weight: 600;
        margin-left: 12px;
        letter-spacing: 0.02em;
        background: rgba(255, 255, 255, 0.2);
        border: 1px solid rgba(255, 255, 255, 0.3);
      }
      .btn-primary {
        background: linear-gradient(135deg, #4f46e5 0%, #7c3aed 100%);
        border: none;
        border-radius: 8px;
        padding: 10px 20px;
        font-weight: 600;
        transition: transform 0.2s, box-shadow 0.2s;
      }
      .btn-primary:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(79, 70, 229, 0.4);
      }
      .modal-header {
        background: linear-gradient(135deg, #4f46e5 0%, #7c3aed 100%);
        color: white;
        border: none;
        padding: 24px 32px;
      }
      .modal-body {
        padding: 32px;
      }
      .modal-xl {
        max-width: 1400px;
      }
      .comparison-container {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 32px;
      }
      .community-card {
        border: 1px solid #e1e4e8;
        border-radius: 12px;
        padding: 28px;
        background: #fafbfc;
      }
      .community-card h3 {
        color: #24292e;
        margin: 0 0 24px 0;
        font-size: 24px;
        font-weight: 600;
        padding-bottom: 16px;
        border-bottom: 2px solid #e1e4e8;
      }
      .shape-container {
        background: white;
        border: 1px solid #e1e4e8;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 24px;
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 200px;
      }
      .shape-svg {
        max-width: 100%;
        height: auto;
      }
      .stats-grid {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 16px;
        margin-bottom: 24px;
      }
      .stat-card {
        background: white;
        border: 1px solid #e1e4e8;
        border-radius: 8px;
        padding: 16px;
      }
      .stat-label {
        font-size: 12px;
        font-weight: 600;
        color: #6b7280;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        margin-bottom: 8px;
      }
      .stat-value {
        font-size: 24px;
        font-weight: 700;
        color: #24292e;
      }
      .chart-container {
        background: white;
        border: 1px solid #e1e4e8;
        border-radius: 8px;
        padding: 20px;
        margin-top: 16px;
      }
      .chart-title {
        font-size: 14px;
        font-weight: 600;
        color: #24292e;
        margin-bottom: 16px;
      }
      .leaflet-popup-content-wrapper {
        border-radius: 12px;
        box-shadow: 0 10px 25px rgba(0, 0, 0, 0.2);
      }
      .popup-header {
        font-size: 18px;
        font-weight: 600;
        color: #4f46e5;
        margin-bottom: 16px;
        padding-bottom: 12px;
        border-bottom: 2px solid #e1e4e8;
      }
      .popup-stat {
        display: flex;
        justify-content: space-between;
        padding: 8px 0;
        border-bottom: 1px solid #f3f4f6;
      }
      .popup-label {
        font-weight: 500;
        color: #6b7280;
      }
      .popup-value {
        font-weight: 600;
        color: #24292e;
      }
    "))
  ),
  
  # Comparison Modal
  tags$div(
    id = "comparisonModal",
    class = "modal fade",
    tabindex = "-1",
    tags$div(
      class = "modal-dialog modal-xl",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h4(class = "modal-title", "Community Comparison"),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            style = "color: white; opacity: 1; font-size: 32px; font-weight: 300;",
            HTML("&times;")
          )
        ),
        tags$div(
          class = "modal-body",
          uiOutput("comparison_content")
        )
      )
    )
  ),
  
  tags$script(HTML("
    $(document).ready(function() {
      Shiny.addCustomMessageHandler('showComparison', function(message) {
        $('#comparisonModal').modal('show');
      });
    });
  ")),
  
  div(class = "title-panel",
      h2("Chicago Community Coverage Explorer",
         span(class = "mode-badge", "LIVE DATA")),
      p("Analyzing Block Club Chicago article distribution and demographics across 77 neighborhoods")
  ),
  
  fluidRow(
    column(4,
           div(class = "control-section",
               h4("Topic Selection"),
               selectInput("blue_var", NULL,
                           choices = topic_choices, 
                           selected = "None")
           ),
           
           div(class = "control-section",
               h4("Demographics"),
               selectInput("demo_var", NULL,
                           choices = demo_choices, 
                           selected = "None")
           ),
           
           div(class = "control-section",
               h4("Metric Type"),
               radioButtons("metric_type", NULL,
                            choices = c("Total Articles" = "total",
                                        "Articles per 1,000 People" = "per_capita"),
                            selected = "total")
           ),
           
           div(class = "control-section",
               h4("Time Period"),
               sliderInput("month_slider", NULL,
                           min = date_min,
                           max = date_max,
                           value = date_max,
                           step = 31,
                           animate = animationOptions(interval = 2000, loop = TRUE))
           ),
           
           div(class = "control-section",
               h4("Compare Communities"),
               selectInput("compare_area1", "Community 1:",
                           choices = c("Select..." = "", sort(str_to_title(unique(chi_boundaries_sf$community)))),
                           selected = ""),
               selectInput("compare_area2", "Community 2:",
                           choices = c("Select..." = "", sort(str_to_title(unique(chi_boundaries_sf$community)))),
                           selected = ""),
               actionButton("compare_btn", "Compare", 
                            class = "btn-primary",
                            style = "width: 100%; margin-top: 12px;")
           )
    ),
    
    column(8,
           leafletOutput("map", height = "750px"),
           tags$div(style = "text-align: center; margin-top: 20px; font-size: 13px; color: #6b7280; font-weight: 500;",
                    paste0("Data Range: ", format(date_min, "%b %Y"), " - ", format(date_max, "%b %Y")),
                    " | Census: ACS 2020-2024"
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
    
    if (!is.null(input$month_slider)) {
      df <- df %>% filter(article_date <= input$month_slider)
    }
    
    if (!is.null(input$blue_var) && input$blue_var != "None") {
      df <- df %>% filter(topic_match == input$blue_var)
    }
    
    topic_summary <- df %>%
      group_by(community) %>%
      summarise(article_count = n(), .groups = "drop")
    
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
    map_data <- chi_boundaries_sf
    topic_data <- filtered_topic_data()
    census_data <- base_census_data()
    
    map_data <- map_data %>%
      left_join(topic_data, by = "community") %>%
      select(-any_of(names(census_data)[-1])) %>%
      left_join(census_data, by = "community")
    
    map_data$article_count <- replace_na(map_data$article_count, 0)
    
    if (input$metric_type == "per_capita") {
      map_data <- map_data %>%
        mutate(display_value = if_else(total_population > 0, 
                                       (article_count / total_population) * 1000, 0))
    } else {
      map_data <- map_data %>%
        mutate(display_value = article_count)
    }
    
    n <- nrow(map_data)
    
    blue_intensity <- rep(0, n)
    if (!is.null(input$blue_var) && input$blue_var != "None") {
      max_val <- max(map_data$display_value, na.rm = TRUE)
      if (!is.infinite(max_val) && max_val > 0) {
        normalized_vals <- map_data$display_value / max_val
        blue_intensity <- sqrt(normalized_vals)
      }
    }
    
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
    
    fill_colors <- rep("#E8E8E8", n)
    
    if (!is.null(input$blue_var) && input$blue_var != "None" && 
        (is.null(input$demo_var) || input$demo_var == "None")) {
      fill_colors <- sapply(1:n, function(i) {
        intensity <- blue_intensity[i]
        if (intensity == 0) return("#E8E8E8")
        r <- round(179 + (0 - 179) * intensity)
        g <- round(217 + (51 - 217) * intensity)
        b <- round(255 + (160 - 255) * intensity)
        rgb(r, g, b, maxColorValue = 255)
      })
    } else if ((is.null(input$blue_var) || input$blue_var == "None") && 
               !is.null(input$demo_var) && input$demo_var != "None") {
      fill_colors <- sapply(1:n, function(i) {
        intensity <- yellow_intensity[i]
        if (intensity == 0) return("#E8E8E8")
        r <- round(255 + (204 - 255) * intensity)
        g <- round(244 + (102 - 244) * intensity)
        b <- round(204 + (0 - 204) * intensity)
        rgb(r, g, b, maxColorValue = 255)
      })
    } else if (!is.null(input$blue_var) && input$blue_var != "None" && 
               !is.null(input$demo_var) && input$demo_var != "None") {
      fill_colors <- sapply(1:n, function(i) {
        blue_val <- blue_intensity[i]
        yellow_val <- yellow_intensity[i]
        if(blue_val == 0 && yellow_val == 0) return("#E8E8E8")
        
        avg_intensity <- (blue_val + yellow_val) / 2
        total <- blue_val + yellow_val
        blue_weight <- blue_val / total
        
        if(blue_weight > 0.6) {
          r <- round(102 + (0 - 102) * avg_intensity)
          g <- round(204 + (128 - 204) * avg_intensity)
          b <- round(204 + (128 - 204) * avg_intensity)
        } else if(blue_weight < 0.4) {
          r <- round(204 + (128 - 204) * avg_intensity)
          g <- round(255 + (160 - 255) * avg_intensity)
          b <- round(102 + (64 - 102) * avg_intensity)
        } else {
          r <- round(144 + (34 - 144) * avg_intensity)
          g <- round(238 + (139 - 238) * avg_intensity)
          b <- round(144 + (34 - 144) * avg_intensity)
        }
        rgb(r, g, b, maxColorValue = 255)
      })
    }
    
    # Simple hover labels
    labels <- lapply(1:n, function(i) {
      row_data <- map_data[i, ]
      label_text <- paste0("<b>", str_to_title(row_data$community), "</b><br>")
      
      if (input$metric_type == "per_capita") {
        label_text <- paste0(label_text, "Articles per 1,000: ", round(row_data$display_value, 2))
      } else {
        label_text <- paste0(label_text, "Total Articles: ", row_data$article_count)
        if (!is.null(input$demo_var) && input$demo_var != "None" && input$demo_var %in% names(row_data)) {
          demo_value <- row_data[[input$demo_var]]
          demo_name <- names(demo_choices)[demo_choices == input$demo_var]
          label_text <- paste0(label_text, "<br>", demo_name, ": ", format(replace_na(demo_value, 0), big.mark = ","))
        }
      }
      HTML(label_text)
    })
    
    # Create click popups with detailed stats
    popups <- lapply(1:n, function(i) {
      row_data <- map_data[i, ]
      
      popup_html <- paste0(
        "<div class='popup-header'>", str_to_title(row_data$community), "</div>",
        "<div class='popup-stat'>",
        "<span class='popup-label'>Total Articles</span>",
        "<span class='popup-value'>", row_data$article_count, "</span></div>",
        "<div class='popup-stat'>",
        "<span class='popup-label'>Articles per 1,000</span>",
        "<span class='popup-value'>", round(row_data$display_value, 2), "</span></div>",
        "<div class='popup-stat'>",
        "<span class='popup-label'>Population</span>",
        "<span class='popup-value'>", format(row_data$total_population, big.mark = ","), "</span></div>",
        "<div class='popup-stat'>",
        "<span class='popup-label'>White</span>",
        "<span class='popup-value'>", format(row_data$white, big.mark = ","), "</span></div>",
        "<div class='popup-stat'>",
        "<span class='popup-label'>Black/African American</span>",
        "<span class='popup-value'>", format(row_data$black_or_african_american, big.mark = ","), "</span></div>",
        "<div class='popup-stat'>",
        "<span class='popup-label'>Hispanic/Latino</span>",
        "<span class='popup-value'>", format(row_data$hispanic_or_latino, big.mark = ","), "</span></div>"
      )
      
      HTML(popup_html)
    })
    
    leafletProxy("map", data = map_data) |>
      clearShapes() |>
      addPolygons(
        fillColor = fill_colors,
        color = "#666",
        weight = 1,
        opacity = 0.8,
        fillOpacity = 0.75,
        label = labels,
        popup = popups,
        labelOptions = labelOptions(
          direction = "auto", 
          textsize = "13px", 
          sticky = TRUE,
          style = list("padding" = "8px 12px", "border-radius" = "8px")
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000",
          opacity = 1,
          fillOpacity = 0.85,
          bringToFront = TRUE
        )
      )
  })
  
  # Comparison feature
  observeEvent(input$compare_btn, {
    req(input$compare_area1, input$compare_area2)
    
    if (input$compare_area1 == "" || input$compare_area2 == "") {
      showNotification("Please select two communities to compare", type = "warning")
      return()
    }
    
    if (input$compare_area1 == input$compare_area2) {
      showNotification("Please select two different communities", type = "warning")
      return()
    }
    
    session$sendCustomMessage("showComparison", list())
  })
  
  output$comparison_content <- renderUI({
    req(input$compare_area1, input$compare_area2)
    
    census_data <- base_census_data()
    article_data_filtered <- filtered_topic_data()
    
    get_community_data <- function(community_name, color) {
      community_lower <- tolower(community_name)
      census_row <- census_data %>% filter(community == community_lower)
      article_row <- article_data_filtered %>% filter(community == community_lower)
      article_count <- if(nrow(article_row) > 0) article_row$article_count else 0
      
      # Get shape for SVG
      shape <- chi_boundaries_sf %>% filter(community == community_lower)
      coords <- st_coordinates(shape)[,1:2]
      x_range <- range(coords[,1])
      y_range <- range(coords[,2])
      
      # Normalize coordinates for SVG
      width <- 200
      height <- 200
      normalized_coords <- data.frame(
        x = (coords[,1] - x_range[1]) / (x_range[2] - x_range[1]) * width,
        y = height - ((coords[,2] - y_range[1]) / (y_range[2] - y_range[1]) * height)
      )
      
      polygon_points <- paste(apply(normalized_coords, 1, function(row) paste(row[1], row[2], sep=",")), collapse=" ")
      
      # Demographics data for chart
      demo_data <- data.frame(
        category = c("White", "Black", "Asian", "Hispanic", "Other"),
        value = c(
          census_row$white,
          census_row$black_or_african_american,
          census_row$asian,
          census_row$hispanic_or_latino,
          census_row$other_race + census_row$multiracial + census_row$native_hawaiian_or_pacific_islander + census_row$american_indian_or_alaska_native
        )
      )
      demo_data$percentage <- round(demo_data$value / census_row$total_population * 100, 1)
      
      list(
        name = community_name,
        color = color,
        svg_path = polygon_points,
        articles = article_count,
        articles_per_1000 = if(census_row$total_population > 0) {
          round((article_count / census_row$total_population) * 1000, 2)
        } else 0,
        total_pop = format(census_row$total_population, big.mark = ","),
        demo_data = demo_data
      )
    }
    
    comm1 <- get_community_data(input$compare_area1, "#4f46e5")
    comm2 <- get_community_data(input$compare_area2, "#7c3aed")
    
    tagList(
      div(class = "comparison-container",
          # Community 1
          div(class = "community-card",
              h3(comm1$name),
              div(class = "shape-container",
                  HTML(paste0('<svg class="shape-svg" viewBox="0 0 200 200" width="200" height="200">
                    <polygon points="', comm1$svg_path, '" fill="', comm1$color, '" stroke="#333" stroke-width="2" opacity="0.7"/>
                  </svg>'))
              ),
              div(class = "stats-grid",
                  div(class = "stat-card",
                      div(class = "stat-label", "Total Articles"),
                      div(class = "stat-value", comm1$articles)
                  ),
                  div(class = "stat-card",
                      div(class = "stat-label", "Per 1,000 People"),
                      div(class = "stat-value", comm1$articles_per_1000)
                  ),
                  div(class = "stat-card",
                      div(class = "stat-label", "Population"),
                      div(class = "stat-value", comm1$total_pop)
                  )
              ),
              div(class = "chart-container",
                  div(class = "chart-title", "Demographics"),
                  plotOutput("demo_chart1", height = "250px")
              )
          ),
          # Community 2
          div(class = "community-card",
              h3(comm2$name),
              div(class = "shape-container",
                  HTML(paste0('<svg class="shape-svg" viewBox="0 0 200 200" width="200" height="200">
                    <polygon points="', comm2$svg_path, '" fill="', comm2$color, '" stroke="#333" stroke-width="2" opacity="0.7"/>
                  </svg>'))
              ),
              div(class = "stats-grid",
                  div(class = "stat-card",
                      div(class = "stat-label", "Total Articles"),
                      div(class = "stat-value", comm2$articles)
                  ),
                  div(class = "stat-card",
                      div(class = "stat-label", "Per 1,000 People"),
                      div(class = "stat-value", comm2$articles_per_1000)
                  ),
                  div(class = "stat-card",
                      div(class = "stat-label", "Population"),
                      div(class = "stat-value", comm2$total_pop)
                  )
              ),
              div(class = "chart-container",
                  div(class = "chart-title", "Demographics"),
                  plotOutput("demo_chart2", height = "250px")
              )
          )
      )
    )
  })
  
  # Demographics charts
  output$demo_chart1 <- renderPlot({
    req(input$compare_area1)
    
    census_data <- base_census_data()
    community_lower <- tolower(input$compare_area1)
    census_row <- census_data %>% filter(community == community_lower)
    
    demo_data <- data.frame(
      category = c("White", "Black", "Asian", "Hispanic", "Other"),
      value = c(
        census_row$white,
        census_row$black_or_african_american,
        census_row$asian,
        census_row$hispanic_or_latino,
        census_row$other_race + census_row$multiracial + census_row$native_hawaiian_or_pacific_islander + census_row$american_indian_or_alaska_native
      )
    )
    demo_data$percentage <- round(demo_data$value / census_row$total_population * 100, 1)
    demo_data <- demo_data %>% arrange(desc(percentage))
    
    ggplot(demo_data, aes(x = reorder(category, percentage), y = percentage)) +
      geom_col(fill = "#4f46e5", alpha = 0.8) +
      coord_flip() +
      labs(x = NULL, y = "Percentage (%)") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 11, color = "#24292e"),
        axis.title = element_text(size = 12, face = "bold", color = "#6b7280"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      )
  })
  
  output$demo_chart2 <- renderPlot({
    req(input$compare_area2)
    
    census_data <- base_census_data()
    community_lower <- tolower(input$compare_area2)
    census_row <- census_data %>% filter(community == community_lower)
    
    demo_data <- data.frame(
      category = c("White", "Black", "Asian", "Hispanic", "Other"),
      value = c(
        census_row$white,
        census_row$black_or_african_american,
        census_row$asian,
        census_row$hispanic_or_latino,
        census_row$other_race + census_row$multiracial + census_row$native_hawaiian_or_pacific_islander + census_row$american_indian_or_alaska_native
      )
    )
    demo_data$percentage <- round(demo_data$value / census_row$total_population * 100, 1)
    demo_data <- demo_data %>% arrange(desc(percentage))
    
    ggplot(demo_data, aes(x = reorder(category, percentage), y = percentage)) +
      geom_col(fill = "#7c3aed", alpha = 0.8) +
      coord_flip() +
      labs(x = NULL, y = "Percentage (%)") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 11, color = "#24292e"),
        axis.title = element_text(size = 12, face = "bold", color = "#6b7280"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      )
  })
}

# RUN APP ----
shinyApp(ui, server)