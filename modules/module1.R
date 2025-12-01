# DATA VIZ - MODULE 1 ----

logo_base64 <- base64enc::base64encode(here("www/lnllogowhiterectangle.jpeg"))

# UI ----
mapExplorerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Lato:wght@400;700&family=Crimson+Text:wght@700&display=swap"),
      tags$style(HTML("
        body { 
          font-family: 'Lato', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
          background: #f1f3f2;
          color: #333333;
        }
        .title-panel { 
          background: linear-gradient(135deg, #dd5600 0%, #c24c00 100%);
          color: white;
          padding: 20px 35px;
          border-radius: 0;
          margin-bottom: 25px;
          box-shadow: 0 4px 12px rgba(221, 86, 0, 0.25);
          display: flex;
          align-items: center;
          gap: 20px;
        }
        .title-text {
          flex: 1;
        }
        .title-panel h2 { 
          margin: 0; 
          font-family: 'Crimson Text', serif;
          font-weight: 700;
          font-size: 32px;
          letter-spacing: 0;
        }
        .title-panel p { 
          margin: 8px 0 0 0; 
          opacity: 0.95; 
          font-size: 15px;
          font-weight: 400;
          font-family: 'Lato', sans-serif;
        }
        .control-section {
          background: white;
          padding: 22px;
          border-radius: 0;
          box-shadow: 0 2px 4px rgba(0,0,0,0.08);
          margin-bottom: 18px;
          border: 1px solid #c9ccc8;
        }
        .control-section h4 {
          margin-top: 0;
          color: #dd5600;
          font-size: 14px;
          font-weight: 700;
          font-family: 'Lato', sans-serif;
          border-bottom: 2px solid #f1f3f2;
          padding-bottom: 10px;
          margin-bottom: 15px;
          letter-spacing: 0.5px;
          text-transform: uppercase;
        }
        .citywide-toggle-container {
          display: flex;
          align-items: center;
          gap: 12px;
          padding: 12px;
          background: #fff8e1;
          border-radius: 0;
          border-left: 4px solid #eec200;
        }
        .citywide-toggle-container label {
          margin: 0;
          font-size: 13px;
          font-weight: 600;
          color: #333333;
          font-family: 'Lato', sans-serif;
        }
        .mode-badge {
          display: inline-block;
          padding: 6px 14px;
          border-radius: 3px;
          font-size: 11px;
          font-weight: 700;
          margin-left: 12px;
          letter-spacing: 0.5px;
          font-family: 'Lato', sans-serif;
        }
        .real-mode { 
          background: #00bf7d; 
          color: white;
          border: none;
        }
        .leaflet-popup-content-wrapper {
          border-radius: 0;
          box-shadow: 0 20px 40px rgba(0, 0, 0, 0.15);
          padding: 0;
          overflow: hidden;
          min-width: 340px;
        }
        .leaflet-popup-content {
          margin: 0;
          width: auto !important;
          font-family: 'Lato', sans-serif;
        }
        .popup-header {
          background: linear-gradient(135deg, #dd5600 0%, #c24c00 100%);
          color: white;
          padding: 20px 24px;
          font-size: 22px;
          font-weight: 700;
          font-family: 'Crimson Text', serif;
          letter-spacing: 0;
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
          color: #666666;
          text-transform: uppercase;
          letter-spacing: 0.1em;
          margin-bottom: 12px;
          padding-bottom: 8px;
          border-bottom: 2px solid #f1f3f2;
          font-family: 'Lato', sans-serif;
        }
        .popup-stat-row {
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 10px 0;
          border-bottom: 1px solid #f1f3f2;
        }
        .popup-stat-row:last-child {
          border-bottom: none;
        }
        .popup-stat-label {
          font-size: 13px;
          font-weight: 400;
          color: #666666;
          font-family: 'Lato', sans-serif;
        }
        .popup-stat-value {
          font-size: 16px;
          font-weight: 700;
          color: #dd5600;
          font-family: 'Lato', sans-serif;
        }
        .popup-highlight {
          background: #fff8e1;
          padding: 12px 16px;
          border-radius: 0;
          margin-top: 12px;
          border-left: 4px solid #eec200;
        }
        .popup-highlight-label {
          font-size: 11px;
          font-weight: 700;
          color: #666666;
          text-transform: uppercase;
          letter-spacing: 0.05em;
          margin-bottom: 4px;
          font-family: 'Lato', sans-serif;
        }
        .popup-highlight-value {
          font-size: 16px;
          font-weight: 700;
          color: #333333;
          font-family: 'Crimson Text', serif;
        }
        .popup-compare-btn {
          width: 100%;
          margin-top: 16px;
          padding: 12px;
          background: #dd5600;
          color: white;
          border: none;
          border-radius: 0;
          font-weight: 700;
          font-size: 14px;
          cursor: pointer;
          text-align: center;
          transition: background 0.3s;
          font-family: 'Lato', sans-serif;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        .popup-compare-btn:hover {
          background: #c24c00;
        }
        .map-legend {
          position: absolute;
          top: 10px;
          right: 10px;
          background: white;
          padding: 15px;
          border-radius: 0;
          box-shadow: 0 2px 8px rgba(0,0,0,0.15);
          border: 1px solid #c9ccc8;
          z-index: 1000;
          max-width: 280px;
          font-family: 'Lato', sans-serif;
        }
        .legend-title {
          font-size: 12px;
          font-weight: 700;
          color: #dd5600;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          margin-bottom: 12px;
          border-bottom: 2px solid #f1f3f2;
          padding-bottom: 6px;
        }
        .legend-gradient {
          height: 20px;
          background: linear-gradient(to right, #eec200, #00bf7d, #007993);
          margin: 10px 0;
          border: 1px solid #c9ccc8;
        }
        .legend-labels {
          display: flex;
          justify-content: space-between;
          font-size: 10px;
          color: #666666;
          margin-bottom: 8px;
        }
        .legend-description {
          font-size: 11px;
          color: #666666;
          line-height: 1.5;
          margin-top: 10px;
        }
        .legend-note {
          font-size: 10px;
          color: #999999;
          font-style: italic;
          margin-top: 8px;
        }
      "))
    ),
    
    # title panel with logo
    div(class = "title-panel",
        tags$img(src = sprintf("data:image/jpeg;base64,%s", logo_base64), style = "height: 60px; width: auto;"),
        div(class = "title-text",
            h2("Chicago News Lens"),
            p("Explore news coverage patterns across Chicago community areas")
        )
    ),
    
    fluidRow(
      column(4,
             div(class = "control-section",
                 h4("Topic Selection"),
                 selectInput(ns("blue_var"), NULL,
                             choices = NULL, 
                             selected = "None")
             ),
             
             div(class = "control-section",
                 h4("Demographics"),
                 selectInput(ns("demo_var"), NULL,
                             choices = NULL, 
                             selected = "None")
             ),
             
             div(class = "control-section",
                 h4("Metric Type"),
                 radioButtons(ns("metric_type"), NULL,
                              choices = c("Total Articles" = "total",
                                          "Articles per 1,000 People" = "per_capita"),
                              selected = "total")
             ),
             
             div(class = "control-section",
                 h4("Coverage Scope"),
                 div(class = "citywide-toggle-container",
                     checkboxInput(ns("include_citywide"), 
                                   "Include citywide articles in neighborhood counts",
                                   value = TRUE)
                 )
             ),
             
             div(class = "control-section",
                 h4("Time Period"),
                 sliderInput(ns("date_range_slider"), NULL,
                             min = as.Date("2020-01-01"),
                             max = Sys.Date(),
                             value = c(as.Date("2020-01-01"), Sys.Date()),
                             step = 31)
             )
      ),
      
      column(8,
             div(style = "position: relative;",
                 leafletOutput(ns("map"), height = "750px"),
                 # Legend overlay
                 uiOutput(ns("map_legend"))
             ),
             tags$div(style = "text-align: center; margin-top: 15px; font-size: 13px; color: #666666; font-weight: 400; font-family: 'Lato', sans-serif;",
                      textOutput(ns("date_range_text"))
             )
      )
    )
  )
}

# SERVER ----
mapExplorerServer <- function(id, chi_boundaries_sf, article_data, date_range, 
                              topics, demo_choices, on_compare_click) {
  moduleServer(id, function(input, output, session) {
    
    # helper function to find predominant category
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
    
    # filter articles based on date range, topic, and citywide toggle
    filtered_topic_data <- reactive({
      req(input$date_range_slider, input$blue_var)
      
      # use TRUE as default if include_citywide is not yet initialized
      include_citywide <- isTRUE(input$include_citywide)
      
      df <- article_data
      
      # filter by date range
      df <- df %>% 
        filter(article_date >= input$date_range_slider[1],
               article_date <= input$date_range_slider[2])
      
      # filter by topic if selected
      if (input$blue_var != "None") {
        df <- df %>% filter(topic_match == input$blue_var)
      }
      
      # handle citywide toggle
      if (include_citywide) {
        # distribute citywide articles to ALL neighborhoods
        citywide_articles <- df %>% 
          filter(community == "chicago")
        
        neighborhood_articles <- df %>% 
          filter(community != "chicago")
        
        if (nrow(citywide_articles) > 0) {
          # get all unique neighborhoods
          all_neighborhoods <- unique(chi_boundaries_sf$community)
          
          # replicate citywide articles for each neighborhood
          citywide_distributed <- citywide_articles %>%
            slice(rep(1:n(), each = length(all_neighborhoods))) %>%
            mutate(community = rep(all_neighborhoods, times = nrow(citywide_articles)))
          
          # combine with neighborhood-specific articles
          df <- bind_rows(neighborhood_articles, citywide_distributed)
        } else {
          df <- neighborhood_articles
        }
      } else {
        # exclude citywide articles completely
        df <- df %>% filter(community != "chicago")
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
    }) %>% bindCache(input$date_range_slider, input$blue_var, input$include_citywide)
    
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
      
      # blue intensity (articles) - Using complementary cyan/blue
      blue_intensity <- rep(0, n)
      if (input$blue_var != "None") {
        max_val <- max(map_data$display_value, na.rm = TRUE)
        if (!is.infinite(max_val) && max_val > 0) {
          normalized_vals <- map_data$display_value / max_val
          blue_intensity <- sqrt(normalized_vals)
        }
      }
      
      # yellow intensity (demographics) - Using tertiary yellow
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
      
      # determine colors - Orange Line palette
      fill_colors <- rep("#f1f3f2", n)  # color-tertiary-light for no data
      
      # CITYWIDE TOGGLE ONLY - show blue when everything else is "None"
      if (input$blue_var == "None" && input$demo_var == "None" && isTRUE(input$include_citywide)) {
        # Blue gradient for citywide-only view
        citywide_max <- max(map_data$article_count, na.rm = TRUE)
        if (!is.infinite(citywide_max) && citywide_max > 0) {
          fill_colors <- sapply(1:n, function(i) {
            if (map_data$article_count[i] == 0) return("#f1f3f2")
            intensity <- sqrt(map_data$article_count[i] / citywide_max)
            # From light cyan to dark teal
            r <- round(173 + (0 - 173) * intensity)
            g <- round(216 + (121 - 216) * intensity)
            b <- round(230 + (147 - 230) * intensity)
            rgb(r, g, b, maxColorValue = 255)
          })
        }
      } else if (input$blue_var != "None" && (input$demo_var == "None")) {
        # blue gradient: complementary light to dark
        fill_colors <- sapply(1:n, function(i) {
          intensity <- blue_intensity[i]
          if (intensity == 0) return("#f1f3f2")
          # From light cyan to dark teal (complementary to orange)
          r <- round(173 + (0 - 173) * intensity)
          g <- round(216 + (121 - 216) * intensity)
          b <- round(230 + (147 - 230) * intensity)
          rgb(r, g, b, maxColorValue = 255)
        })
      } else if (input$blue_var == "None" && input$demo_var != "None") {
        # yellow gradient: tertiary light to dark
        fill_colors <- sapply(1:n, function(i) {
          intensity <- yellow_intensity[i]
          if (intensity == 0) return("#f1f3f2")
          # From #f1ece4 to #eec200 (tertiary colors)
          r <- round(241 + (238 - 241) * intensity)
          g <- round(236 + (194 - 236) * intensity)
          b <- round(228 + (0 - 228) * intensity)
          rgb(r, g, b, maxColorValue = 255)
        })
      } else if (input$blue_var != "None" && input$demo_var != "None") {
        # Green blend when both selected (secondary color)
        fill_colors <- sapply(1:n, function(i) {
          blue_val <- blue_intensity[i]
          yellow_val <- yellow_intensity[i]
          if(blue_val == 0 && yellow_val == 0) return("#f1f3f2")
          
          avg_intensity <- (blue_val + yellow_val) / 2
          total <- blue_val + yellow_val
          blue_weight <- blue_val / total
          
          if(blue_weight > 0.6) {
            # more blue - cyan tones
            r <- round(173 + (0 - 173) * avg_intensity)
            g <- round(216 + (150 - 216) * avg_intensity)
            b <- round(230 + (136 - 230) * avg_intensity)
          } else if(blue_weight < 0.4) {
            # more yellow - warm green
            r <- round(200 + (139 - 200) * avg_intensity)
            g <- round(220 + (195 - 220) * avg_intensity)
            b <- round(150 + (74 - 150) * avg_intensity)
          } else {
            # balanced - secondary green (#00bf7d)
            r <- round(180 + (0 - 180) * avg_intensity)
            g <- round(220 + (191 - 220) * avg_intensity)
            b <- round(200 + (125 - 200) * avg_intensity)
          }
          rgb(r, g, b, maxColorValue = 255)
        })
      }
      
      # create hover labels
      labels <- lapply(1:n, function(i) {
        row_data <- map_data[i, ]
        label_text <- paste0("<b style='font-size: 15px; font-family: \"Crimson Text\", serif;'>", 
                             str_to_title(row_data$community), "</b><br>")
        
        if (input$metric_type == "per_capita") {
          label_text <- paste0(
            label_text,
            "<span style='color: #dd5600; font-family: \"Lato\", sans-serif;'>📰 Articles per 1,000: </span>", 
            "<b>", round(row_data$display_value, 2), "</b><br>"
          )
        } else {
          label_text <- paste0(
            label_text,
            "<span style='color: #dd5600; font-family: \"Lato\", sans-serif;'>📰 Total Articles: </span>", 
            "<b>", row_data$article_count, "</b><br>"
          )
          
          if (input$demo_var != "None" && input$demo_var %in% names(row_data)) {
            demo_value <- row_data[[input$demo_var]]
            demo_name <- names(demo_choices)[demo_choices == input$demo_var]
            label_text <- paste0(
              label_text,
              "<span style='color: #eec200; font-family: \"Lato\", sans-serif;'>📊 ", demo_name, ": </span>",
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
        
        # find predominant categories
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
          color = "#666666",
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
              "border-radius" = "0",
              "box-shadow" = "0 3px 10px rgba(0,0,0,0.2)",
              "font-family" = "'Lato', sans-serif"
            )
          ),
          highlightOptions = highlightOptions(
            weight = 2.5,
            color = "#333333",
            opacity = 1,
            fillOpacity = 0.85,
            bringToFront = TRUE
          )
        )
    })
    
    # handle popup compare button clicks
    observeEvent(input$popup_compare_click, {
      req(input$popup_compare_click)
      if (!is.null(on_compare_click)) {
        on_compare_click(input$popup_compare_click)
      }
    })
    
    # render dynamic legend based on selections
    output$map_legend <- renderUI({
      # only show legend when both topic and demographic are selected
      if (input$blue_var != "None" && input$demo_var != "None") {
        div(class = "map-legend",
            div(class = "legend-title", "Coverage Ratio"),
            div(class = "legend-gradient"),
            div(class = "legend-labels",
                span("More Demographics"),
                span("Balanced"),
                span("More Coverage")
            ),
            div(class = "legend-description",
                HTML("<b>Yellow-green:</b> Higher demographic proportion<br>
                     <b>Blue-green:</b> Higher article coverage<br>
                     <b>Green:</b> Balanced ratio")
            ),
            div(class = "legend-note",
                "Color intensity shows relative proportions"
            )
        )
      }
    })
    
    # date range text with citywide indicator
    output$date_range_text <- renderText({
      req(input$date_range_slider)
      citywide_text <- if(isTRUE(input$include_citywide)) "includes citywide" else "neighborhood-only"
      paste0("Selected Period: ", format(input$date_range_slider[1], "%b %Y"), 
             " - ", format(input$date_range_slider[2], "%b %Y"),
             "Census: ACS 2020-2024 • Coverage: ", citywide_text)
    })
  })
}