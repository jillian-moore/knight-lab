# WARD COMPARISON - MODULE 2

# UI ----
communityComparisonUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Crimson+Text:wght@400;600;700&family=Lato:wght@300;400;700&display=swap');
        
        .title-panel { 
          background: linear-gradient(135deg, #dd5600 0%, #c24c00 100%);
          color: white;
          padding: 20px 25px;
          border-radius: 0;
          margin-bottom: 20px;
          box-shadow: 0 4px 12px rgba(221, 86, 0, 0.25);
          display: flex;
          align-items: center;
          gap: 20px;
        }
        .title-logo {
          height: 70px;
          width: auto;
        }
        .title-text {
          flex: 1;
        }
        .title-panel h2 { 
          margin: 0; 
          font-family: 'Crimson Text', serif;
          font-weight: 700; 
          font-size: 28px;
        }
        .title-panel p { 
          margin: 6px 0 0 0; 
          opacity: 0.95; 
          font-size: 14px; 
          font-weight: 400;
          font-family: 'Lato', sans-serif;
        }
        .control-section {
          background: white;
          padding: 20px;
          border-radius: 0;
          box-shadow: 0 2px 4px rgba(0,0,0,0.08);
          margin-bottom: 15px;
          border: 1px solid #c9ccc8;
        }
        .control-section h3 {
          margin-top: 0;
          color: #dd5600;
          font-size: 18px;
          font-weight: 700;
          font-family: 'Lato', sans-serif;
          border-bottom: 2px solid #f1f3f2;
          padding-bottom: 8px;
        }
        .control-section h4 {
          margin-top: 0;
          color: #dd5600;
          font-size: 16px;
          font-weight: 700;
          font-family: 'Lato', sans-serif;
          border-bottom: 2px solid #f1f3f2;
          padding-bottom: 8px;
          margin-bottom: 15px;
        }
        .community-selector-card {
          background: white;
          border-radius: 0;
          padding: 20px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.08);
          border: 1px solid #c9ccc8;
        }
        .community-selector-header {
          font-size: 13px;
          font-weight: 700;
          color: #dd5600;
          text-transform: uppercase;
          letter-spacing: 0.05em;
          margin-bottom: 12px;
          font-family: 'Lato', sans-serif;
        }
        .stat-item {
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 12px 16px;
          margin-bottom: 10px;
          background: #f1f3f2;
          border-radius: 0;
          border-left: 4px solid #dd5600;
        }
        .stat-item-label {
          font-size: 12px;
          font-weight: 700;
          color: #666666;
          text-transform: uppercase;
          letter-spacing: 0.05em;
          font-family: 'Lato', sans-serif;
        }
        .stat-item-value {
          font-size: 18px;
          font-weight: 700;
          color: #333333;
          font-family: 'Crimson Text', serif;
        }
        .predominant-box {
          background: #fff8e1;
          padding: 12px 16px;
          border-radius: 0;
          margin-bottom: 10px;
          border-left: 4px solid #eec200;
        }
        .predominant-title {
          font-size: 10px;
          font-weight: 700;
          color: #666666;
          text-transform: uppercase;
          letter-spacing: 0.05em;
          margin-bottom: 5px;
          font-family: 'Lato', sans-serif;
        }
        .predominant-value {
          font-size: 15px;
          font-weight: 700;
          color: #333333;
          font-family: 'Crimson Text', serif;
        }
        .demographic-selector {
          margin-bottom: 15px;
        }
        .demographic-selector label {
          font-family: 'Lato', sans-serif;
          font-weight: 400;
          color:
          #666666;
        }
      "))
    ),
    
    div(class = "title-panel",
        tags$img(src = "lnllogotransparent.png", class = "title-logo", alt = "Logo"),
        div(class = "title-text",
            h2("Chicago Community Area Comparison"),
            p("Compare news coverage and demographics between two Chicago neighborhoods")
        )
    ),
    
    # Community selectors with stat cards
    fluidRow(
      column(6,
             div(class = "community-selector-card",
                 div(class = "community-selector-header", "📍 First Community Area"),
                 selectInput(ns("ward1"), NULL,
                             choices = NULL,
                             selected = NULL),
                 uiOutput(ns("stat_card1"))
             )
      ),
      column(6,
             div(class = "community-selector-card",
                 div(class = "community-selector-header", "📍 Second Community Area"),
                 selectInput(ns("ward2"), NULL,
                             choices = NULL,
                             selected = NULL),
                 uiOutput(ns("stat_card2"))
             )
      )
    ),
    
    # Charts section
    fluidRow(
      column(6,
             div(class = "control-section",
                 h4("Articles per Person Over Time"),
                 plotOutput(ns("line_chart"), height = "350px")
             )
      ),
      column(6,
             div(class = "control-section",
                 h4("Population Distribution Comparison"),
                 div(class = "demographic-selector",
                     radioButtons(ns("pyramid_type"), "Select Demographic:",
                                  choices = c("Age" = "age", "Race" = "race", "Income" = "income"),
                                  selected = "age",
                                  inline = TRUE)
                 ),
                 plotOutput(ns("population_pyramid"), height = "350px")
             )
      )
    ),
    
    fluidRow(
      column(12,
             div(class = "control-section",
                 h4("Topics by Year"),
                 plotOutput(ns("stacked_bar"), height = "400px")
             )
      )
    )
  )
}

# server ----
communityComparisonServer <- function(id, full_data, selected_ward1 = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # Helper function to find predominant category
    find_predominant <- function(values, labels) {
      if (all(is.na(values)) || all(values == 0)) {
        return(list(label = "N/A", value = 0))
      }
      max_idx <- which.max(values)
      list(label = labels[max_idx], value = values[max_idx])
    }
    
    # initialize community choices
    observe({
      community_choices <- sort(unique(full_data$community))
      names(community_choices) <- str_to_title(community_choices)
      
      # Check if ward1 is pre-selected from map click
      ward1_default <- if (!is.null(selected_ward1()) && selected_ward1() != "") {
        selected_ward1()
      } else {
        community_choices[1]
      }
      
      updateSelectInput(session, "ward1", 
                        choices = community_choices,
                        selected = ward1_default)
      updateSelectInput(session, "ward2", 
                        choices = community_choices,
                        selected = community_choices[min(2, length(community_choices))])
    })
    
    # reactive data for selected wards
    ward_data <- reactive({
      req(input$ward1, input$ward2)
      
      full_data %>%
        filter(community %in% c(input$ward1, input$ward2)) %>%
        mutate(
          community_label = str_to_title(community),
          year = year(date),
          year_month = floor_date(date, "month")
        )
    }) %>% bindCache(input$ward1, input$ward2)
    
    # Get census data for selected communities
    census_summary <- reactive({
      req(ward_data())
      
      ward_data() %>%
        group_by(community, community_label) %>%
        summarise(
          total_population = first(total_population),
          white = first(white),
          black_or_african_american = first(black_or_african_american),
          asian = first(asian),
          hispanic_or_latino = first(hispanic_or_latino),
          other_race = first(other_race),
          multiracial = first(multiracial),
          native_hawaiian_or_pacific_islander = first(native_hawaiian_or_pacific_islander),
          american_indian_or_alaska_native = first(american_indian_or_alaska_native),
          age_0_17 = first(age_0_17),
          age_18_24 = first(age_18_24),
          age_25_34 = first(age_25_34),
          age_35_49 = first(age_35_49),
          age_50_64 = first(age_50_64),
          age_65_plus = first(age_65_plus),
          under_25_000 = first(under_25_000),
          x25_000_to_49_999 = first(x25_000_to_49_999),
          x50_000_to_74_999 = first(x50_000_to_74_999),
          x75_000_to_125_000 = first(x75_000_to_125_000),
          x125_000 = first(x125_000),
          total_articles = sum(article_count, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        # Maintain selection order
        mutate(order = case_when(
          community == input$ward1 ~ 1,
          community == input$ward2 ~ 2,
          TRUE ~ 3
        )) %>%
        arrange(order)
    }) %>% bindCache(input$ward1, input$ward2)
    
    # Create stat card for community 1
    output$stat_card1 <- renderUI({
      req(census_summary())
      stats <- census_summary()
      
      if (nrow(stats) < 1) return(NULL)
      stats_row <- stats[1, ]
      
      # Find predominant categories
      race_values <- c(
        stats_row$white, 
        stats_row$black_or_african_american, 
        stats_row$asian, 
        stats_row$hispanic_or_latino,
        stats_row$other_race + stats_row$multiracial + 
          stats_row$native_hawaiian_or_pacific_islander + 
          stats_row$american_indian_or_alaska_native
      )
      race_labels <- c("White", "Black/African American", "Asian", "Hispanic/Latino", "Other/Multiracial")
      predominant_race <- find_predominant(race_values, race_labels)
      
      age_values <- c(
        stats_row$age_0_17,
        stats_row$age_18_24,
        stats_row$age_25_34,
        stats_row$age_35_49,
        stats_row$age_50_64,
        stats_row$age_65_plus
      )
      age_labels <- c("0-17 years", "18-24 years", "25-34 years", "35-49 years", "50-64 years", "65+ years")
      predominant_age <- find_predominant(age_values, age_labels)
      
      income_values <- c(
        stats_row$under_25_000,
        stats_row$x25_000_to_49_999,
        stats_row$x50_000_to_74_999,
        stats_row$x75_000_to_125_000,
        stats_row$x125_000
      )
      income_labels <- c("Under $25K", "$25K-$50K", "$50K-$75K", "$75K-$125K", "$125K+")
      predominant_income <- find_predominant(income_values, income_labels)
      
      tagList(
        div(class = "stat-item",
            span(class = "stat-item-label", "Total Articles"),
            span(class = "stat-item-value", format(stats_row$total_articles, big.mark = ","))
        ),
        div(class = "stat-item",
            span(class = "stat-item-label", "Total Population"),
            span(class = "stat-item-value", format(stats_row$total_population, big.mark = ","))
        ),
        div(class = "predominant-box",
            div(class = "predominant-title", "Predominant Age"),
            div(class = "predominant-value", 
                paste0(predominant_age$label, " (", format(predominant_age$value, big.mark = ","), ")"))
        ),
        div(class = "predominant-box",
            div(class = "predominant-title", "Predominant Race"),
            div(class = "predominant-value", 
                paste0(predominant_race$label, " (", format(predominant_race$value, big.mark = ","), ")"))
        ),
        div(class = "predominant-box",
            div(class = "predominant-title", "Predominant Income"),
            div(class = "predominant-value", 
                paste0(predominant_income$label, " (", format(predominant_income$value, big.mark = ","), ")"))
        )
      )
    })
    
    # Create stat card for community 2
    output$stat_card2 <- renderUI({
      req(census_summary())
      stats <- census_summary()
      
      if (nrow(stats) < 2) return(NULL)
      stats_row <- stats[2, ]
      
      # Find predominant categories
      race_values <- c(
        stats_row$white, 
        stats_row$black_or_african_american, 
        stats_row$asian, 
        stats_row$hispanic_or_latino,
        stats_row$other_race + stats_row$multiracial + 
          stats_row$native_hawaiian_or_pacific_islander + 
          stats_row$american_indian_or_alaska_native
      )
      race_labels <- c("White", "Black/African American", "Asian", "Hispanic/Latino", "Other/Multiracial")
      predominant_race <- find_predominant(race_values, race_labels)
      
      age_values <- c(
        stats_row$age_0_17,
        stats_row$age_18_24,
        stats_row$age_25_34,
        stats_row$age_35_49,
        stats_row$age_50_64,
        stats_row$age_65_plus
      )
      age_labels <- c("0-17 years", "18-24 years", "25-34 years", "35-49 years", "50-64 years", "65+ years")
      predominant_age <- find_predominant(age_values, age_labels)
      
      income_values <- c(
        stats_row$under_25_000,
        stats_row$x25_000_to_49_999,
        stats_row$x50_000_to_74_999,
        stats_row$x75_000_to_125_000,
        stats_row$x125_000
      )
      income_labels <- c("Under $25K", "$25K-$50K", "$50K-$75K", "$75K-$125K", "$125K+")
      predominant_income <- find_predominant(income_values, income_labels)
      
      tagList(
        div(class = "stat-item",
            span(class = "stat-item-label", "Total Articles"),
            span(class = "stat-item-value", format(stats_row$total_articles, big.mark = ","))
        ),
        div(class = "stat-item",
            span(class = "stat-item-label", "Total Population"),
            span(class = "stat-item-value", format(stats_row$total_population, big.mark = ","))
        ),
        div(class = "predominant-box",
            div(class = "predominant-title", "Predominant Age"),
            div(class = "predominant-value", 
                paste0(predominant_age$label, " (", format(predominant_age$value, big.mark = ","), ")"))
        ),
        div(class = "predominant-box",
            div(class = "predominant-title", "Predominant Race"),
            div(class = "predominant-value", 
                paste0(predominant_race$label, " (", format(predominant_race$value, big.mark = ","), ")"))
        ),
        div(class = "predominant-box",
            div(class = "predominant-title", "Predominant Income"),
            div(class = "predominant-value", 
                paste0(predominant_income$label, " (", format(predominant_income$value, big.mark = ","), ")"))
        )
      )
    })
    
    # line chart data
    line_data <- reactive({
      req(ward_data())
      
      ward_data() %>%
        group_by(community_label, year_month) %>%
        summarise(
          articles = sum(article_count, na.rm = TRUE),
          population = first(total_population),
          articles_per_1000 = (articles / population) * 1000,
          .groups = "drop"
        )
    }) %>% bindCache(input$ward1, input$ward2)
    
    output$line_chart <- renderPlot({
      req(line_data())
      
      ggplot(line_data(), aes(x = year_month, y = articles_per_1000, 
                              color = community_label, group = community_label)) +
        geom_line(size = 1.5) +
        geom_point(size = 2.5) +
        scale_color_manual(values = c("#dd5600", "#00bf7d")) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
        labs(
          x = NULL,
          y = "Articles per 1,000 People",
          color = "Community Area"
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          text = element_text(family = "sans", color = "#333333"),
          plot.title = element_text(family = "serif", face = "bold", size = 16),
          legend.position = "top",
          legend.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, color = "#666666"),
          axis.text.y = element_text(color = "#666666"),
          axis.title.y = element_text(color = "#333333", face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#f1f3f2")
        )
    }) %>% bindCache(input$ward1, input$ward2)
    
    # Population pyramid data
    pyramid_data <- reactive({
      req(census_summary(), input$pyramid_type)
      
      stats <- census_summary()
      
      if (input$pyramid_type == "age") {
        # Age distribution
        data.frame(
          community = rep(stats$community_label, each = 6),
          category = rep(c("0-17", "18-24", "25-34", "35-49", "50-64", "65+"), times = nrow(stats)),
          value = c(
            stats$age_0_17[1], stats$age_18_24[1], stats$age_25_34[1], 
            stats$age_35_49[1], stats$age_50_64[1], stats$age_65_plus[1],
            if(nrow(stats) > 1) c(stats$age_0_17[2], stats$age_18_24[2], stats$age_25_34[2], 
                                  stats$age_35_49[2], stats$age_50_64[2], stats$age_65_plus[2]) else rep(0, 6)
          ),
          stringsAsFactors = FALSE
        )
      } else if (input$pyramid_type == "race") {
        # Race distribution
        data.frame(
          community = rep(stats$community_label, each = 5),
          category = rep(c("White", "Black", "Asian", "Hispanic", "Other"), times = nrow(stats)),
          value = c(
            stats$white[1], stats$black_or_african_american[1], stats$asian[1], 
            stats$hispanic_or_latino[1], 
            stats$other_race[1] + stats$multiracial[1] + stats$native_hawaiian_or_pacific_islander[1] + stats$american_indian_or_alaska_native[1],
            if(nrow(stats) > 1) c(
              stats$white[2], stats$black_or_african_american[2], stats$asian[2], 
              stats$hispanic_or_latino[2], 
              stats$other_race[2] + stats$multiracial[2] + stats$native_hawaiian_or_pacific_islander[2] + stats$american_indian_or_alaska_native[2]
            ) else rep(0, 5)
          ),
          stringsAsFactors = FALSE
        )
      } else {
        # Income distribution
        data.frame(
          community = rep(stats$community_label, each = 5),
          category = rep(c("Under $25K", "$25K-$50K", "$50K-$75K", "$75K-$125K", "$125K+"), times = nrow(stats)),
          value = c(
            stats$under_25_000[1], stats$x25_000_to_49_999[1], stats$x50_000_to_74_999[1], 
            stats$x75_000_to_125_000[1], stats$x125_000[1],
            if(nrow(stats) > 1) c(
              stats$under_25_000[2], stats$x25_000_to_49_999[2], stats$x50_000_to_74_999[2], 
              stats$x75_000_to_125_000[2], stats$x125_000[2]
            ) else rep(0, 5)
          ),
          stringsAsFactors = FALSE
        )
      }
    }) %>% bindCache(input$ward1, input$ward2, input$pyramid_type)
    
    output$population_pyramid <- renderPlot({
      req(pyramid_data())
      
      pyr_data <- pyramid_data()
      
      # Get the two community names in order
      communities <- unique(pyr_data$community)
      
      # Make first community negative (left), second positive (right)
      pyr_data <- pyr_data %>%
        mutate(
          plot_value = ifelse(community == communities[1], -abs(value), abs(value))
        )
      
      # Find max value for x-axis limits
      max_val <- max(abs(pyr_data$value), na.rm = TRUE)
      
      ggplot(pyr_data, aes(x = plot_value, y = category, fill = community)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("#dd5600", "#00bf7d")) +
        scale_x_continuous(
          labels = function(x) format(abs(x), big.mark = ","),
          limits = c(-max_val * 1.1, max_val * 1.1)
        ) +
        labs(
          x = "Population",
          y = NULL,
          fill = "Community"
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          text = element_text(family = "sans", color = "#333333"),
          legend.position = "top",
          legend.text = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "#f1f3f2"),
          axis.text.y = element_text(size = 12, face = "bold", color = "#333333"),
          axis.text.x = element_text(color = "#666666"),
          axis.title.x = element_text(color = "#333333", face = "bold")
        ) +
        geom_vline(xintercept = 0, color = "#666666", size = 0.5)
    }) %>% bindCache(input$ward1, input$ward2, input$pyramid_type)
    
    # stacked bar data
    bar_data <- reactive({
      req(ward_data())
      
      ward_data() %>%
        filter(random_topic != "No Coverage") %>%
        group_by(community_label, year, random_topic) %>%
        summarise(total = sum(article_count, na.rm = TRUE), .groups = "drop")
    }) %>% bindCache(input$ward1, input$ward2)
    
    output$stacked_bar <- renderPlot({
      req(bar_data())
      
      ggplot(bar_data(), aes(x = factor(year), y = total, fill = random_topic)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap(~ community_label, ncol = 2) +
        scale_fill_manual(values = c("#dd5600", "#00bf7d", "#eec200", "#5a3825", 
                                     "#6633cc", "#c9ccc8", "#666666", "#333333",
                                     "#f1ece4", "#ada8a0", "#ff8800", "#2196f3")) +
        labs(
          x = "Year",
          y = "Number of Articles",
          fill = "Topic"
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          text = element_text(family = "sans", color = "#333333"),
          legend.position = "bottom",
          legend.text = element_text(size = 11),
          strip.text = element_text(size = 14, face = "bold", family = "serif", color = "#dd5600"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#f1f3f2"),
          axis.text = element_text(color = "#666666"),
          axis.title = element_text(color = "#333333", face = "bold")
        ) +
        guides(fill = guide_legend(nrow = 2))
    }) %>% bindCache(input$ward1, input$ward2)
  })
}