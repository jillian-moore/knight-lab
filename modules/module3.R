# DATA QUALITY CHECK MODULE ----

# UI ----
dataQualityUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
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
        .control-section h3, .control-section h4 {
          margin-top: 0;
          color: #dd5600;
          font-weight: 700;
          font-family: 'Lato', sans-serif;
          border-bottom: 2px solid #f1f3f2;
          padding-bottom: 8px;
        }
        .control-section h3 {
          font-size: 18px;
        }
        .control-section h4 {
          font-size: 16px;
          margin-bottom: 15px;
        }
        .metric-card {
          background: white;
          border-radius: 0;
          padding: 20px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.08);
          border: 1px solid #c9ccc8;
          border-left: 4px solid #dd5600;
          margin-bottom: 15px;
          transition: all 0.3s ease;
          text-align: center;
        }
        .metric-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.12);
        }
        .metric-value {
          font-size: 32px;
          font-weight: 700;
          color: #dd5600;
          margin: 5px 0;
          font-family: 'Crimson Text', serif;
        }
        .metric-label {
          font-size: 12px;
          font-weight: 700;
          color: #666666;
          text-transform: uppercase;
          letter-spacing: 0.05em;
          font-family: 'Lato', sans-serif;
        }
        .section-card {
          background: white;
          border-radius: 0;
          padding: 25px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.08);
          border: 1px solid #c9ccc8;
          margin-bottom: 20px;
        }
        .section-card h3 {
          color: #dd5600;
          font-size: 18px;
          font-weight: 700;
          font-family: 'Lato', sans-serif;
          margin-top: 0;
          margin-bottom: 20px;
          border-bottom: 2px solid #f1f3f2;
          padding-bottom: 10px;
        }
        .plot-container {
          background: #f1f3f2;
          border-radius: 0;
          padding: 15px;
          margin-top: 15px;
        }
        .nav-pills > li > a {
          border-radius: 0;
          font-family: 'Lato', sans-serif;
          font-weight: 600;
        }
        .nav-pills > li.active > a,
        .nav-pills > li.active > a:hover,
        .nav-pills > li.active > a:focus {
          background-color: #dd5600;
        }
        .methodology-box {
          background: #f1f3f2;
          padding: 20px;
          border-radius: 0;
          font-family: 'Courier New', monospace;
          font-size: 13px;
          line-height: 1.8;
          border-left: 4px solid #dd5600;
          white-space: pre-wrap;
        }
      "))
    ),
    
    # Title panel
    div(class = "title-panel",
        div(class = "title-text",
            h2("Data Quality Dashboard"),
            p("Comprehensive validation and analysis of Chicago community data")
        )
    ),
    
    # Summary Metrics Row
    fluidRow(
      column(3,
             div(class = "metric-card",
                 div(class = "metric-label", "Total Articles"),
                 div(class = "metric-value", textOutput(ns("total_articles")))
             )
      ),
      column(3,
             div(class = "metric-card",
                 div(class = "metric-label", "Communities Covered"),
                 div(class = "metric-value", textOutput(ns("total_communities")))
             )
      ),
      column(3,
             div(class = "metric-card",
                 div(class = "metric-label", "Total Population"),
                 div(class = "metric-value", textOutput(ns("total_population")))
             )
      ),
      column(3,
             div(class = "metric-card",
                 div(class = "metric-label", "Topics Tracked"),
                 div(class = "metric-value", textOutput(ns("topics_tracked")))
             )
      )
    ),
    
    # Tab Panel for Different Sections
    tabsetPanel(
      type = "pills",
      
      # Topic Distribution Tab
      tabPanel(
        "Topic Distribution",
        icon = icon("tags"),
        br(),
        fluidRow(
          column(12,
                 div(class = "section-card",
                     h3("Article Distribution by Topic"),
                     div(class = "plot-container",
                         plotOutput(ns("topic_plot"), height = "500px")
                     ),
                     br(),
                     dataTableOutput(ns("topic_table"))
                 )
          )
        )
      ),
      
      # Census Demographics Tab
      tabPanel(
        "Demographics",
        icon = icon("users"),
        br(),
        fluidRow(
          column(6,
                 div(class = "section-card",
                     h3("Race/Ethnicity Distribution"),
                     div(class = "plot-container",
                         plotOutput(ns("race_plot"), height = "400px")
                     )
                 )
          ),
          column(6,
                 div(class = "section-card",
                     h3("Age Distribution"),
                     div(class = "plot-container",
                         plotOutput(ns("age_plot"), height = "400px")
                     )
                 )
          )
        ),
        fluidRow(
          column(12,
                 div(class = "section-card",
                     h3("Income Distribution"),
                     div(class = "plot-container",
                         plotOutput(ns("income_plot"), height = "400px")
                     )
                 )
          )
        )
      ),
      
      # Publication Trends Tab
      tabPanel(
        "Publication Trends",
        icon = icon("chart-line"),
        br(),
        fluidRow(
          column(12,
                 div(class = "section-card",
                     h3("Article Publication Timeline"),
                     div(class = "plot-container",
                         plotOutput(ns("timeline_plot"), height = "400px")
                     )
                 )
          )
        ),
        fluidRow(
          column(12,
                 div(class = "section-card",
                     h3("Articles by Year"),
                     div(class = "plot-container",
                         plotOutput(ns("year_plot"), height = "400px")
                     ),
                     br(),
                     dataTableOutput(ns("year_table"))
                 )
          )
        )
      ),
      
      # Neighborhood Coverage Tab
      tabPanel(
        "Neighborhood Coverage",
        icon = icon("map-marked-alt"),
        br(),
        fluidRow(
          column(12,
                 div(class = "section-card",
                     h3("Top 20 Neighborhoods by Article Coverage"),
                     div(class = "plot-container",
                         plotOutput(ns("neighborhood_plot"), height = "500px")
                     )
                 )
          )
        ),
        fluidRow(
          column(6,
                 div(class = "section-card",
                     h3("Top Covered Neighborhoods"),
                     dataTableOutput(ns("top_neighborhoods_table"))
                 )
          ),
          column(6,
                 div(class = "section-card",
                     h3("Low Coverage Neighborhoods"),
                     p("Communities with fewer than 10 articles:", 
                       style = "color: #666666; font-size: 14px; font-family: 'Lato', sans-serif;"),
                     dataTableOutput(ns("low_coverage_table"))
                 )
          )
        )
      ),
      
      # Methodology Tab
      tabPanel(
        "Methodology",
        icon = icon("book"),
        br(),
        fluidRow(
          column(12,
                 div(class = "section-card",
                     h3("Neighborhood Mapping Methodology"),
                     div(class = "methodology-box",
                         verbatimTextOutput(ns("methodology_text"))
                     )
                 )
          )
        )
      )
    )
  )
}

# SERVER ----
dataQualityServer <- function(id, chi_boundaries_sf, article_data) {
  moduleServer(id, function(input, output, session) {
    
    # Prepare census summary data
    census_summary <- reactive({
      chi_boundaries_sf %>%
        st_drop_geometry() %>%
        select(community, total_population, 
               white, black_or_african_american, asian, hispanic_or_latino,
               age_0_17, age_18_24, age_25_34, age_35_49, age_50_64, age_65_plus,
               under_25_000, x25_000_to_49_999, x50_000_to_74_999, 
               x75_000_to_125_000, x125_000) %>%
        distinct(community, .keep_all = TRUE)
    })
    
    # Topic counts
    topic_counts <- reactive({
      article_data %>%
        count(topic_match, name = "article_count") %>%
        arrange(desc(article_count)) %>%
        mutate(
          percentage = (article_count / sum(article_count)) * 100,
          percentage_label = paste0(round(percentage, 1), "%")
        )
    })
    
    # Mapping summary
    mapping_summary <- reactive({
      article_data %>%
        group_by(community) %>%
        summarise(article_count = n(), .groups = "drop") %>%
        arrange(desc(article_count))
    })
    
    # Summary Metrics
    output$total_articles <- renderText({
      format(nrow(article_data), big.mark = ",")
    })
    
    output$total_communities <- renderText({
      nrow(mapping_summary())
    })
    
    output$total_population <- renderText({
      format(sum(census_summary()$total_population, na.rm = TRUE), big.mark = ",")
    })
    
    output$topics_tracked <- renderText({
      length(unique(article_data$topic_match))
    })
    
    # Topic Distribution Plot
    output$topic_plot <- renderPlot({
      ggplot(topic_counts(), aes(x = reorder(topic_match, article_count), y = article_count)) +
        geom_col(fill = "#dd5600", alpha = 0.9) +
        geom_text(aes(label = paste0(article_count, "\n(", percentage_label, ")")), 
                  hjust = -0.1, size = 3.5, family = "sans", color = "#333333") +
        coord_flip() +
        labs(
          title = "Article Distribution by Topic",
          subtitle = paste0("Total Articles: ", format(sum(topic_counts()$article_count), big.mark = ",")),
          x = NULL,
          y = "Number of Articles"
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          text = element_text(family = "sans", color = "#333333"),
          plot.title = element_text(family = "serif", face = "bold", size = 16, color = "#333333"),
          plot.subtitle = element_text(size = 12, color = "#666666", family = "sans"),
          axis.text = element_text(size = 11, color = "#666666"),
          axis.title = element_text(color = "#333333", face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#f1f3f2")
        ) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))
    })
    
    output$topic_table <- renderDataTable({
      topic_counts() %>%
        select(Topic = topic_match, 
               `Article Count` = article_count, 
               Percentage = percentage_label) %>%
        datatable(
          options = list(pageLength = 12, dom = 't'),
          rownames = FALSE
        )
    })
    
    # Race/Ethnicity Plot
    output$race_plot <- renderPlot({
      race_totals <- census_summary() %>%
        summarise(
          White = sum(white, na.rm = TRUE),
          Black = sum(black_or_african_american, na.rm = TRUE),
          Asian = sum(asian, na.rm = TRUE),
          Hispanic = sum(hispanic_or_latino, na.rm = TRUE)
        ) %>%
        pivot_longer(everything(), names_to = "race_ethnicity", values_to = "population") %>%
        mutate(
          percentage = (population / sum(population)) * 100,
          percentage_label = paste0(round(percentage, 1), "%")
        )
      
      ggplot(race_totals, aes(x = reorder(race_ethnicity, population), y = population)) +
        geom_col(fill = "#dd5600", alpha = 0.9) +
        geom_text(aes(label = paste0(comma(population), "\n(", percentage_label, ")")), 
                  hjust = -0.1, size = 4, family = "sans", color = "#333333") +
        coord_flip() +
        labs(
          title = "Chicago Population by Race/Ethnicity",
          x = NULL,
          y = "Population"
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          text = element_text(family = "sans", color = "#333333"),
          plot.title = element_text(family = "serif", face = "bold", size = 16, color = "#333333"),
          axis.text = element_text(size = 11, color = "#666666"),
          axis.title = element_text(color = "#333333", face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#f1f3f2")
        ) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))
    })
    
    # Age Distribution Plot
    output$age_plot <- renderPlot({
      age_totals <- census_summary() %>%
        summarise(
          `0-17` = sum(age_0_17, na.rm = TRUE),
          `18-24` = sum(age_18_24, na.rm = TRUE),
          `25-34` = sum(age_25_34, na.rm = TRUE),
          `35-49` = sum(age_35_49, na.rm = TRUE),
          `50-64` = sum(age_50_64, na.rm = TRUE),
          `65+` = sum(age_65_plus, na.rm = TRUE)
        ) %>%
        pivot_longer(everything(), names_to = "age_group", values_to = "population") %>%
        mutate(
          percentage = (population / sum(population)) * 100,
          percentage_label = paste0(round(percentage, 1), "%")
        )
      
      ggplot(age_totals, aes(x = age_group, y = population)) +
        geom_col(fill = "#ffa914", alpha = 0.9) +
        geom_text(aes(label = paste0(comma(population), "\n(", percentage_label, ")")), 
                  vjust = -0.5, size = 3.5, family = "sans", color = "#333333") +
        labs(
          title = "Chicago Population by Age Group",
          x = "Age Group",
          y = "Population"
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          text = element_text(family = "sans", color = "#333333"),
          plot.title = element_text(family = "serif", face = "bold", size = 16, color = "#333333"),
          axis.text = element_text(size = 11, color = "#666666"),
          axis.title = element_text(color = "#333333", face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#f1f3f2")
        ) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))
    })
    
    # Income Distribution Plot
    output$income_plot <- renderPlot({
      income_totals <- census_summary() %>%
        summarise(
          `Under $25k` = sum(under_25_000, na.rm = TRUE),
          `$25k-$50k` = sum(x25_000_to_49_999, na.rm = TRUE),
          `$50k-$75k` = sum(x50_000_to_74_999, na.rm = TRUE),
          `$75k-$125k` = sum(x75_000_to_125_000, na.rm = TRUE),
          `$125k+` = sum(x125_000, na.rm = TRUE)
        ) %>%
        pivot_longer(everything(), names_to = "income_bracket", values_to = "households") %>%
        mutate(
          percentage = (households / sum(households)) * 100,
          percentage_label = paste0(round(percentage, 1), "%"),
          income_bracket = factor(income_bracket, 
                                  levels = c("Under $25k", "$25k-$50k", "$50k-$75k", 
                                             "$75k-$125k", "$125k+"))
        )
      
      ggplot(income_totals, aes(x = income_bracket, y = households)) +
        geom_col(fill = "#eec200", alpha = 0.9) +
        geom_text(aes(label = paste0(comma(households), "\n(", percentage_label, ")")), 
                  vjust = -0.5, size = 3.5, family = "sans", color = "#333333") +
        labs(
          title = "Chicago Households by Income Bracket",
          x = "Income Bracket",
          y = "Number of Households"
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          text = element_text(family = "sans", color = "#333333"),
          plot.title = element_text(family = "serif", face = "bold", size = 16, color = "#333333"),
          axis.text = element_text(size = 11, color = "#666666"),
          axis.text.x = element_text(angle = 15, hjust = 1),
          axis.title = element_text(color = "#333333", face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#f1f3f2")
        ) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))
    })
    
    # Timeline Plot
    output$timeline_plot <- renderPlot({
      articles_by_month <- article_data %>%
        mutate(year_month = floor_date(article_date, "month")) %>%
        count(year_month, name = "article_count") %>%
        arrange(year_month)
      
      ggplot(articles_by_month, aes(x = year_month, y = article_count)) +
        geom_line(color = "#dd5600", size = 1.5) +
        geom_point(color = "#dd5600", size = 2.5) +
        labs(
          title = "Article Publication Trend Over Time",
          x = "Date",
          y = "Articles Published"
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          text = element_text(family = "sans", color = "#333333"),
          plot.title = element_text(family = "serif", face = "bold", size = 16, color = "#333333"),
          axis.text = element_text(size = 11, color = "#666666"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(color = "#333333", face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#f1f3f2")
        ) +
        scale_y_continuous(labels = comma) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b %Y")
    })
    
    # Year Plot
    output$year_plot <- renderPlot({
      articles_by_year <- article_data %>%
        mutate(year = year(article_date)) %>%
        count(year, name = "article_count") %>%
        arrange(year)
      
      ggplot(articles_by_year, aes(x = factor(year), y = article_count)) +
        geom_col(fill = "#ffa914", alpha = 0.9) +
        geom_text(aes(label = comma(article_count)), vjust = -0.5, size = 4, 
                  family = "sans", color = "#333333") +
        labs(
          title = "Articles Published by Year",
          x = "Year",
          y = "Number of Articles"
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          text = element_text(family = "sans", color = "#333333"),
          plot.title = element_text(family = "serif", face = "bold", size = 16, color = "#333333"),
          axis.text = element_text(size = 11, color = "#666666"),
          axis.title = element_text(color = "#333333", face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#f1f3f2")
        ) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))
    })
    
    output$year_table <- renderDataTable({
      article_data %>%
        mutate(year = year(article_date)) %>%
        count(year, name = "article_count") %>%
        arrange(desc(year)) %>%
        datatable(
          options = list(pageLength = 10, dom = 't'),
          rownames = FALSE,
          colnames = c("Year", "Article Count")
        )
    })
    
    # Neighborhood Coverage Plot
    output$neighborhood_plot <- renderPlot({
      mapping_summary() %>%
        top_n(20, article_count) %>%
        ggplot(aes(x = reorder(community, article_count), y = article_count)) +
        geom_col(fill = "#dd5600", alpha = 0.9) +
        geom_text(aes(label = comma(article_count)), hjust = -0.1, size = 3, 
                  family = "sans", color = "#333333") +
        coord_flip() +
        labs(
          title = "Top 20 Neighborhoods by Article Coverage",
          x = NULL,
          y = "Number of Articles"
        ) +
        theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          text = element_text(family = "sans", color = "#333333"),
          plot.title = element_text(family = "serif", face = "bold", size = 16, color = "#333333"),
          axis.text = element_text(size = 10, color = "#666666"),
          axis.title = element_text(color = "#333333", face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#f1f3f2")
        ) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))
    })
    
    output$top_neighborhoods_table <- renderDataTable({
      mapping_summary() %>%
        head(15) %>%
        mutate(community = str_to_title(community)) %>%
        datatable(
          options = list(pageLength = 15, dom = 't'),
          rownames = FALSE,
          colnames = c("Community", "Article Count")
        )
    })
    
    output$low_coverage_table <- renderDataTable({
      mapping_summary() %>%
        filter(article_count < 10) %>%
        mutate(community = str_to_title(community)) %>%
        datatable(
          options = list(pageLength = 10, dom = 't'),
          rownames = FALSE,
          colnames = c("Community", "Article Count")
        )
    })
    
    # Methodology Text
    output$methodology_text <- renderText({
      paste(
        "NEIGHBORHOOD MAPPING METHODOLOGY",
        "=================================",
        "",
        "This analysis maps Block Club Chicago articles to Chicago's 77 official",
        "community areas using a hierarchical matching system:",
        "",
        "1. PRIMARY MATCHING (Sub-community field):",
        "   - Extract lead text before em-dash/hyphen in article content",
        "   - Match against neighborhood mapping table",
        "   - If match found, use ONLY this neighborhood",
        "",
        "2. SECONDARY MATCHING (Article Sections):",
        "   - If no primary match, parse parsely.meta.articleSection field",
        "   - Check each section against mapping table",
        "   - Collect ALL matching neighborhoods",
        "",
        "3. TERTIARY MATCHING (Primary Category):",
        "   - If still no match, parse slp_primary_category.name field",
        "   - Check each category against mapping table",
        "   - Collect ALL matching neighborhoods",
        "",
        "4. DEFAULT ASSIGNMENT:",
        "   - If no matches found, assign to 'chicago' (citywide)",
        "",
        "MULTI-NEIGHBORHOOD ARTICLES:",
        "- Articles can be assigned to multiple neighborhoods (up to 3)",
        "- This captures stories that span multiple communities",
        "- Each assignment is stored in separate columns",
        "",
        paste0("DATA QUALITY SUMMARY:"),
        paste0("- Total neighborhoods mapped: ", nrow(mapping_summary())),
        paste0("- Total articles: ", format(nrow(article_data), big.mark = ",")),
        paste0("- Avg articles per neighborhood: ", round(mean(mapping_summary()$article_count))),
        sep = "\n"
      )
    })
  })
}