# WARD COMPARISON

# UI ----
communityComparisonUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
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
        .control-section h3 {
          margin-top: 0;
          color: #667eea;
          font-size: 18px;
          font-weight: 600;
          border-bottom: 2px solid #f0f0f0;
          padding-bottom: 8px;
        }
        .control-section h4 {
          margin-top: 0;
          color: #667eea;
          font-size: 16px;
          font-weight: 600;
          border-bottom: 2px solid #f0f0f0;
          padding-bottom: 8px;
        }
      "))
    ),
    
    div(class = "title-panel",
        h2("Chicago Community Area Comparison"),
        p("Compare news coverage and demographics between two Chicago neighborhoods")
    ),
    
    fluidRow(
      column(6,
             div(class = "control-section",
                 h4("📍 Select First Community Area"),
                 selectInput(ns("ward1"), NULL,
                             choices = NULL,
                             selected = NULL)
             )
      ),
      column(6,
             div(class = "control-section",
                 h4("📍 Select Second Community Area"),
                 selectInput(ns("ward2"), NULL,
                             choices = NULL,
                             selected = NULL)
             )
      )
    ),
    
    fluidRow(
      column(12,
             div(class = "control-section",
                 h3("Summary Statistics"),
                 tableOutput(ns("summary_table"))
             )
      )
    ),
    
    fluidRow(
      column(6,
             div(class = "control-section",
                 h4("Articles per Person Over Time"),
                 plotOutput(ns("line_chart"), height = "350px")
             )
      ),
      column(6,
             div(class = "control-section",
                 h4("Article Topics Distribution"),
                 plotOutput(ns("pie_chart"), height = "350px")
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
communityComparisonServer <- function(id, full_data) {
  moduleServer(id, function(input, output, session) {
    
    # initialize community choices
    observe({
      community_choices <- sort(unique(full_data$community))
      names(community_choices) <- str_to_title(community_choices)
      
      updateSelectInput(session, "ward1", 
                        choices = community_choices,
                        selected = community_choices[1])
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
    
    # summary statistics table
    summary_stats <- reactive({
      req(ward_data())
      
      ward_data() %>%
        group_by(community_label) %>%
        summarise(
          `Total Articles` = sum(article_count, na.rm = TRUE),
          `Population` = format(first(total_population), big.mark = ","),
          `Articles per 1,000` = round(sum(article_count, na.rm = TRUE) / first(total_population) * 1000, 2),
          `Most Common Topic` = {
            topic_counts <- table(random_topic[random_topic != "No Coverage"])
            if(length(topic_counts) > 0) names(which.max(topic_counts)) else "N/A"
          },
          `Date Range` = paste(format(min(date), "%b %Y"), "-", format(max(date), "%b %Y")),
          .groups = "drop"
        ) %>%
        rename(`Community Area` = community_label)
    }) %>% bindCache(input$ward1, input$ward2)
    
    output$summary_table <- renderTable({
      summary_stats()
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
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
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("#667eea", "#f093fb")) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
        labs(
          x = NULL,
          y = "Articles per 1,000 People",
          color = "Community Area"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank()
        )
    }) %>% bindCache(input$ward1, input$ward2)
    
    # pie chart data
    pie_data <- reactive({
      req(ward_data())
      
      ward_data() %>%
        filter(random_topic != "No Coverage") %>%
        group_by(community_label, random_topic) %>%
        summarise(total = sum(article_count, na.rm = TRUE), .groups = "drop") %>%
        group_by(community_label) %>%
        mutate(
          percentage = total / sum(total) * 100,
          label = paste0(random_topic, "\n", round(percentage, 1), "%")
        )
    }) %>% bindCache(input$ward1, input$ward2)
    
    output$pie_chart <- renderPlot({
      req(pie_data())
      
      ggplot(pie_data(), aes(x = "", y = total, fill = random_topic)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        facet_wrap(~ community_label) +
        scale_fill_brewer(palette = "Set3") +
        labs(fill = "Topic") +
        theme_void(base_size = 11) +
        theme(
          legend.position = "right",
          strip.text = element_text(size = 12, face = "bold")
        )
    }) %>% bindCache(input$ward1, input$ward2)
    
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
        scale_fill_brewer(palette = "Set3") +
        labs(
          x = "Year",
          y = "Number of Articles",
          fill = "Topic"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          strip.text = element_text(size = 13, face = "bold"),
          panel.grid.minor = element_blank()
        ) +
        guides(fill = guide_legend(nrow = 2))
    }) %>% bindCache(input$ward1, input$ward2)
  })
}