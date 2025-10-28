# Map Module UI
mapModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("topic_var"), "Select Topic:",
                         choices = NULL, selected = NULL),
             colourpicker::colourInput(ns("topic_color_start"), "Topic Start Color:", "#DEEBF7"),
             colourpicker::colourInput(ns("topic_color_end"), "Topic End Color:", "#08519C")
      ),
      column(3,
             selectInput(ns("demo_var"), "Select Demographic:",
                         choices = NULL, selected = NULL),
             colourpicker::colourInput(ns("demo_color_start"), "Demo Start Color:", "#FFF7BC"),
             colourpicker::colourInput(ns("demo_color_end"), "Demo End Color:", "#D95F0E")
      ),
      column(6,
             sliderInput(ns("date_slider"), "Articles Published Up To:",
                         min = as.Date("2020-01-01"),
                         max = Sys.Date(),
                         value = Sys.Date(),
                         timeFormat = "%b %Y",
                         animate = animationOptions(interval = 1000, loop = FALSE)),
             checkboxInput(ns("show_all_dates"), "Show All Time Periods", value = TRUE)
      )
    ),
    fluidRow(
      column(12,
             leafletOutput(ns("map"), height = "600px")
      )
    )
  )
}

# Map Module Server
mapModuleServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Extract topics and demographics from data
    observe({
      req(data())
      
      topics <- data() |> 
        filter(!is.na(random_topic) & random_topic != "No Coverage") |> 
        pull(random_topic) |> 
        unique() |> 
        sort()
      
      demo_choices <- c(
        "None" = "None",
        "White" = "white",
        "Black or African American" = "black_or_african_american",
        "Asian" = "asian",
        "Hispanic or Latino" = "hispanic_or_latino",
        "Under $25,000" = "under_25_000",
        "$25,000 to $49,999" = "x25_000_to_49_999",
        "$50,000 to $74,999" = "x50_000_to_74_999",
        "$75,000 to $125,000" = "x75_000_to_125_000",
        "$125,000+" = "x125_000_plus",
        "Age 0-17" = "age_0_17",
        "Age 18-24" = "age_18_24",
        "Age 25-34" = "age_25_34",
        "Age 35-49" = "age_35_49",
        "Age 50-64" = "age_50_64",
        "Age 65+" = "age_65_plus"
      )
      
      updateSelectInput(session, "topic_var", 
                        choices = c("None" = "None", setNames(topics, topics)),
                        selected = "None")
      updateSelectInput(session, "demo_var", 
                        choices = demo_choices,
                        selected = "None")
      
      # Update date slider range
      date_range <- data() |> 
        summarise(min = min(date, na.rm = TRUE),
                  max = max(date, na.rm = TRUE))
      
      updateSliderInput(session, "date_slider",
                        min = date_range$min,
                        max = date_range$max,
                        value = date_range$max)
    })
    
    # Helper function for rescaling
    safe_rescale <- function(x, to = c(0, 1)) {
      if (all(is.na(x)) || length(unique(x[!is.na(x)])) <= 1) {
        return(rep(0.5, length(x)))
      }
      scales::rescale(x, to = to, from = range(x, na.rm = TRUE, finite = TRUE))
    }
    
    # Reactive: filtered and aggregated data
    map_data <- reactive({
      req(data())
      df <- data()
      
      # Filter by date if not showing all
      if (!input$show_all_dates) {
        df <- df |> filter(date <= input$date_slider)
      }
      
      # Aggregate: cumulative articles by community and topic up to date
      df_agg <- df |> 
        group_by(community) |> 
        summarise(
          total_articles = n(),
          # Keep first geometry and demographics
          the_geom = first(the_geom),
          across(matches("^(white|black|asian|hispanic|under_|x\\d|age_)"), first),
          .groups = "drop"
        )
      
      # Add topic-specific counts
      topic_counts <- df |> 
        group_by(community, random_topic) |> 
        summarise(topic_articles = n(), .groups = "drop")
      
      df_agg <- df_agg |> 
        left_join(topic_counts, by = "community", relationship = "many-to-many")
      
      # Ensure sf object
      if (!inherits(df_agg, "sf")) {
        df_agg <- st_as_sf(df_agg, sf_column_name = "the_geom", crs = 4326)
      }
      
      df_agg
    })
    
    # Initialize map
    output$map <- renderLeaflet({
      leaflet() |> 
        addProviderTiles(providers$CartoDB.Positron) |> 
        setView(lng = -87.65, lat = 41.84, zoom = 10)
    })
    
    # Update map with layered colors
    observe({
      req(map_data())
      df <- map_data()
      
      # Filter to selected topic if chosen
      if (input$topic_var != "None") {
        df <- df |> filter(random_topic == input$topic_var)
      }
      
      if (nrow(df) == 0) return()
      
      # Calculate topic intensity (0-1)
      if (input$topic_var != "None" && "topic_articles" %in% names(df)) {
        topic_intensity <- safe_rescale(df$topic_articles)
      } else {
        topic_intensity <- rep(0, nrow(df))
      }
      
      # Calculate demographic intensity (0-1)
      if (input$demo_var != "None" && input$demo_var %in% names(df)) {
        demo_vals <- suppressWarnings(as.numeric(df[[input$demo_var]]))
        demo_vals[is.na(demo_vals)] <- 0
        demo_intensity <- safe_rescale(demo_vals)
      } else {
        demo_intensity <- rep(0, nrow(df))
      }
      
      # Create color gradients
      topic_gradient <- colorRampPalette(c(input$topic_color_start, input$topic_color_end))(100)
      demo_gradient <- colorRampPalette(c(input$demo_color_start, input$demo_color_end))(100)
      
      # Blend colors using opacity layering
      fill_colors <- sapply(1:nrow(df), function(i) {
        # Start with white base
        base_rgb <- c(1, 1, 1)
        
        # Layer topic color with its intensity as opacity
        if (topic_intensity[i] > 0) {
          topic_color <- topic_gradient[as.integer(topic_intensity[i] * 99) + 1]
          topic_rgb <- col2rgb(topic_color) / 255
          base_rgb <- (1 - topic_intensity[i]) * base_rgb + topic_intensity[i] * topic_rgb
        }
        
        # Layer demographic color with its intensity as opacity
        if (demo_intensity[i] > 0) {
          demo_color <- demo_gradient[as.integer(demo_intensity[i] * 99) + 1]
          demo_rgb <- col2rgb(demo_color) / 255
          base_rgb <- (1 - demo_intensity[i]) * base_rgb + demo_intensity[i] * demo_rgb
        }
        
        rgb(base_rgb[1], base_rgb[2], base_rgb[3])
      })
      
      # Overall opacity based on whether anything is selected
      fill_opacity <- ifelse(topic_intensity > 0 | demo_intensity > 0, 0.7, 0.3)
      
      # Update map
      leafletProxy("map", data = df, session = session) |> 
        clearShapes() |> 
        addPolygons(
          fillColor = fill_colors,
          fillOpacity = fill_opacity,
          color = "#555",
          weight = 1,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = ~paste0(
            "<b>", str_to_title(community), "</b><br>",
            if_else(input$topic_var != "None",
                    paste0("📰 ", input$topic_var, ": ", topic_articles, " articles<br>"),
                    ""),
            if_else(input$demo_var != "None",
                    paste0("👥 ", names(which(c(
                      "None" = "None",
                      "White" = "white",
                      "Black or African American" = "black_or_african_american",
                      "Asian" = "asian",
                      "Hispanic or Latino" = "hispanic_or_latino"
                    ) == input$demo_var)), ": ", 
                    round(as.numeric(.data[[input$demo_var]]), 1), "%"),
                    ""),
            "<br>Total Articles: ", total_articles
          ) |> lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          )
        )
    })
    
    return(map_data)
  })
}