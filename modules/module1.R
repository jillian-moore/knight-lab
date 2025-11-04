# DATA VIZ - MODULE 1

# UI ----
mapExplorerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
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
        .leaflet-popup-content-wrapper {
          border-radius: 16px;
          box-shadow: 0 20px 40px rgba(0, 0, 0, 0.15);
          padding: 0;
          overflow: hidden;
          min-width: 340px;
        }
        .leaflet-popup-content {
          margin: 0;
          width: auto !important;
        }
        .popup-header {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 20px 24px;
          font-size: 20px;
          font-weight: 700;
          letter-spacing: -0.02em;
        }
        .popup-body {
          padding: 20px 24px;
          background: white;
        }
        .popup-section {
          margin-bottom: 20px;
        }
        .popup-section:last-child {
          margin-bottom: 0;
        }
        .popup-section-title {
          font-size: 11px;
          font-weight: 700;
          color: #6b7280;
          text-transform: uppercase;
          letter-spacing: 0.1em;
          margin-bottom: 12px;
          padding-bottom: 8px;
          border-bottom: 2px solid #e5e7eb;
        }
        .popup-stat-row {
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 10px 0;
          border-bottom: 1px solid #f3f4f6;
        }
        .popup-stat-row:last-child {
          border-bottom: none;
        }
        .popup-stat-label {
          font-size: 13px;
          font-weight: 500;
          color: #374151;
        }
        .popup-stat-value {
          font-size: 15px;
          font-weight: 700;
          color: #667eea;
        }
        .popup-highlight {
          background: linear-gradient(135deg, #fef3c7 0%, #fde68a 100%);
          padding: 12px 16px;
          border-radius: 8px;
          margin-top: 12px;
          border-left: 4px solid #f59e0b;
        }
        .popup-highlight-label {
          font-size: 11px;
          font-weight: 700;
          color: #92400e;
          text-transform: uppercase;
          letter-spacing: 0.05em;
          margin-bottom: 4px;
        }
        .popup-highlight-value {
          font-size: 16px;
          font-weight: 700;
          color: #b45309;
        }
        .popup-compare-btn {
          width: 100%;
          margin-top: 16px;
          padding: 12px;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border: none;
          border-radius: 8px;
          font-weight: 600;
          font-size: 14px;
          cursor: pointer;
          text-align: center;
          transition: transform 0.2s, box-shadow 0.2s;
        }
        .popup-compare-btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 16px rgba(102, 126, 234, 0.4);
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
                 selectInput(ns("blue_var"), NULL,
                             choices = NULL, 
                             selected = "None")
             ),
             
             div(class = "control-section",
                 h4("📊 Demographics"),
                 selectInput(ns("demo_var"), NULL,
                             choices = NULL, 
                             selected = "None")
             ),
             
             div(class = "control-section",
                 h4("📏 Metric Type"),
                 radioButtons(ns("metric_type"), NULL,
                              choices = c("Total Articles" = "total",
                                          "Articles per 1,000 People" = "per_capita"),
                              selected = "total")
             ),
             
             div(class = "control-section",
                 h4("📅 Time Period"),
                 sliderInput(ns("date_range_slider"), NULL,
                             min = as.Date("2020-01-01"),
                             max = Sys.Date(),
                             value = c(as.Date("2020-01-01"), Sys.Date()),
                             step = 31,
                             animate = animationOptions(interval = 2000, loop = TRUE))
             )
      ),
      
      column(8,
             leafletOutput(ns("map"), height = "750px"),
             tags$div(style = "text-align: center; margin-top: 15px; font-size: 13px; color: #6c757d;",
                      textOutput(ns("date_range_text"))
             )
      )
    )
  )
}

# server ----
mapExplorerServer <- function(id, chi_boundaries_sf, article_data, date_range, 
                              topics, demo_choices, on_compare_click) {
  moduleServer(id, function(input, output, session) {
    
    # Helper function to find predominant category
    find_predominant <- function(values, labels) {
      if (all(is.na(values)) || all(values == 0)) {
        return(list(label = "N/A", value = 0))
      }
      max_idx <- which.max(values)
      list(label = labels[max_idx], value = values[max_idx])
    }
    
    # update choices on module initialization
    observe({
      updateSelectInput(session, "blue_var", 
                        choices = c("None", topics), 
                        selected = "None")
      updateSelectInput(session, "demo_var", 
                        choices = demo_choices, 
                        selected = "None")
      updateSliderInput(session, "date_range_slider",
                        min = date_range$min_date,
                        max = date_range$max_date,
                        value = c(date_range$min_date, date_range$max_date))
    })
    
    # base census data 
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
    }) %>% bindCache("base_census_data")
    
    # filter articles based on date range and topic
    filtered_topic_data <- reactive({
      req(input$date_range_slider, input$blue_var)
      
      df <- article_data
      
      # filter by date range
      df <- df %>% 
        filter(article_date >= input$date_range_slider[1],
               article_date <= input$date_range_slider[2])
      
      # filter by topic if selected
      if (input$blue_var != "None") {
        df <- df %>% filter(topic_match == input$blue_var)
      }
      
      # count articles per community
      topic_summary <- df %>%
        group_by(community) %>%
        summarise(article_count = n(), .groups = "drop")
      
      # ensure all communities are represented
      all_communities <- data.frame(
        community = unique(chi_boundaries_sf$community),
        stringsAsFactors = FALSE
      )
      
      topic_summary <- all_communities %>%
        left_join(topic_summary, by = "community") %>%
        mutate(article_count = replace_na(article_count, 0))
      
      topic_summary
    }) %>% bindCache(input$date_range_slider, input$blue_var)
    
    # initialize map once
    output$map <- renderLeaflet({
      leaflet(chi_boundaries_sf) |> 
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = -87.65, lat = 41.84, zoom = 10)
    })
    
    # update map polygons
    observe({
      map_data <- chi_boundaries_sf
      topic_data <- filtered_topic_data()
      census_data <- base_census_data()
      
      # join data
      map_data <- map_data %>%
        left_join(topic_data, by = "community") %>%
        select(-any_of(names(census_data)[-1])) %>%
        left_join(census_data, by = "community")
      
      map_data$article_count <- replace_na(map_data$article_count, 0)
      
      # calculate display value
      if (input$metric_type == "per_capita") {
        map_data <- map_data %>%
          mutate(display_value = if_else(total_population > 0, 
                                         (article_count / total_population) * 1000, 
                                         0))
      } else {
        map_data <- map_data %>%
          mutate(display_value = article_count)
      }
      
      n <- nrow(map_data)
      
      # blue intensity (articles)
      blue_intensity <- rep(0, n)
      if (input$blue_var != "None") {
        max_val <- max(map_data$display_value, na.rm = TRUE)
        if (!is.infinite(max_val) && max_val > 0) {
          normalized_vals <- map_data$display_value / max_val
          blue_intensity <- sqrt(normalized_vals)
        }
      }
      
      # yellow intensity (demographics)
      yellow_intensity <- rep(0, n)
      if (input$demo_var != "None" && input$demo_var %in% names(map_data)) {
        demo_vals <- suppressWarnings(as.numeric(map_data[[input$demo_var]]))
        demo_vals[is.na(demo_vals)] <- 0
        max_demo <- max(demo_vals, na.rm = TRUE)
        if (!is.infinite(max_demo) && max_demo > 0) {
          normalized_demo <- demo_vals / max_demo
          yellow_intensity <- sqrt(normalized_demo)
        }
      }
      
      # determine colors
      fill_colors <- rep("#E8E8E8", n)
      
      if (input$blue_var != "None" && (input$demo_var == "None")) {
        # blue gradient only
        fill_colors <- sapply(1:n, function(i) {
          intensity <- blue_intensity[i]
          if (intensity == 0) return("#E8E8E8")
          r <- round(179 + (0 - 179) * intensity)
          g <- round(217 + (51 - 217) * intensity)
          b <- round(255 + (160 - 255) * intensity)
          rgb(r, g, b, maxColorValue = 255)
        })
      } else if (input$blue_var == "None" && input$demo_var != "None") {
        # yellow gradient only
        fill_colors <- sapply(1:n, function(i) {
          intensity <- yellow_intensity[i]
          if (intensity == 0) return("#E8E8E8")
          r <- round(255 + (204 - 255) * intensity)
          g <- round(244 + (102 - 244) * intensity)
          b <- round(204 + (0 - 204) * intensity)
          rgb(r, g, b, maxColorValue = 255)
        })
      } else if (input$blue_var != "None" && input$demo_var != "None") {
        # green blend
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
      
      # create hover labels
      labels <- lapply(1:n, function(i) {
        row_data <- map_data[i, ]
        label_text <- paste0("<b style='font-size: 14px;'>", str_to_title(row_data$community), "</b><br>")
        
        if (input$metric_type == "per_capita") {
          label_text <- paste0(
            label_text,
            "<span style='color: #667eea;'>📰 Articles per 1,000: </span>", 
            "<b>", round(row_data$display_value, 2), "</b><br>"
          )
        } else {
          label_text <- paste0(
            label_text,
            "<span style='color: #667eea;'>📰 Total Articles: </span>", 
            "<b>", row_data$article_count, "</b><br>"
          )
          
          if (input$demo_var != "None" && input$demo_var %in% names(row_data)) {
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
      
      # create enhanced click popups with "Compare with Others" button
      popups <- lapply(1:n, function(i) {
        row_data <- map_data[i, ]
        community_name <- row_data$community
        
        # Find predominant categories
        race_values <- c(
          row_data$white, 
          row_data$black_or_african_american, 
          row_data$asian, 
          row_data$hispanic_or_latino,
          row_data$other_race + row_data$multiracial + 
            row_data$native_hawaiian_or_pacific_islander + 
            row_data$american_indian_or_alaska_native
        )
        race_labels <- c("White", "Black/African American", "Asian", "Hispanic/Latino", "Other/Multiracial")
        predominant_race <- find_predominant(race_values, race_labels)
        
        age_values <- c(
          row_data$age_0_17,
          row_data$age_18_24,
          row_data$age_25_34,
          row_data$age_35_49,
          row_data$age_50_64,
          row_data$age_65_plus
        )
        age_labels <- c("0-17 years", "18-24 years", "25-34 years", "35-49 years", "50-64 years", "65+ years")
        predominant_age <- find_predominant(age_values, age_labels)
        
        income_values <- c(
          row_data$under_25_000,
          row_data$x25_000_to_49_999,
          row_data$x50_000_to_74_999,
          row_data$x75_000_to_125_000,
          row_data$x125_000
        )
        income_labels <- c("Under $25K", "$25K-$50K", "$50K-$75K", "$75K-$125K", "$125K+")
        predominant_income <- find_predominant(income_values, income_labels)
        
        # Create unique button ID for this popup
        btn_id <- paste0("compare_btn_", gsub("[^a-z0-9]", "_", tolower(community_name)))
        
        popup_html <- paste0(
          "<div class='popup-header'>", str_to_title(community_name), "</div>",
          "<div class='popup-body'>",
          "<div class='popup-section'>",
          "<div class='popup-section-title'>Coverage Metrics</div>",
          "<div class='popup-stat-row'>",
          "<span class='popup-stat-label'>Total Articles</span>",
          "<span class='popup-stat-value'>", row_data$article_count, "</span></div>",
          "<div class='popup-stat-row'>",
          "<span class='popup-stat-label'>Per 1,000 People</span>",
          "<span class='popup-stat-value'>", 
          round((row_data$article_count / row_data$total_population) * 1000, 2), 
          "</span></div>",
          "</div>",
          "<div class='popup-section'>",
          "<div class='popup-section-title'>Demographics</div>",
          "<div class='popup-stat-row'>",
          "<span class='popup-stat-label'>Total Population</span>",
          "<span class='popup-stat-value'>", format(row_data$total_population, big.mark = ","), "</span></div>",
          "</div>",
          "<div class='popup-highlight'>",
          "<div class='popup-highlight-label'>Predominant Race</div>",
          "<div class='popup-highlight-value'>", predominant_race$label, " (", 
          format(predominant_race$value, big.mark = ","), ")</div>",
          "</div>",
          "<div class='popup-highlight'>",
          "<div class='popup-highlight-label'>Predominant Age Group</div>",
          "<div class='popup-highlight-value'>", predominant_age$label, " (", 
          format(predominant_age$value, big.mark = ","), ")</div>",
          "</div>",
          "<div class='popup-highlight'>",
          "<div class='popup-highlight-label'>Predominant Income</div>",
          "<div class='popup-highlight-value'>", predominant_income$label, " (", 
          format(predominant_income$value, big.mark = ","), ")</div>",
          "</div>",
          "<button class='popup-compare-btn' onclick='Shiny.setInputValue(\"map_tab-popup_compare_click\", \"", 
          community_name, "\", {priority: \"event\"})'>",
          "🔍 Compare with Others",
          "</button>",
          "</div>"
        )
        
        HTML(popup_html)
      })
      
      leafletProxy("map", session, data = map_data) |>
        clearShapes() |>
        addPolygons(
          fillColor = fill_colors,
          color = "#666",
          weight = 1.2,
          opacity = 0.8,
          fillOpacity = 0.75,
          label = labels,
          popup = popups,
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
    
    # Handle popup compare button clicks
    observeEvent(input$popup_compare_click, {
      req(input$popup_compare_click)
      if (!is.null(on_compare_click)) {
        on_compare_click(input$popup_compare_click)
      }
    })
    
    # date range text
    output$date_range_text <- renderText({
      req(input$date_range_slider)
      paste0("📅 Selected Period: ", format(input$date_range_slider[1], "%b %Y"), 
             " - ", format(input$date_range_slider[2], "%b %Y"),
             " • 📊 Census: ACS 2020-2024")
    })
  })
}