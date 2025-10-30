# ward_comparison_module.R
# Module for comparing two community areas (wards) with multiple visualizations

# UI Module ----
wardComparisonUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(6,
             div(class = "control-section",
                 h4("📍 Select First Community Area"),
                 selectInput(ns("ward1"), NULL,
                             choices = NULL,  # Will be updated in server
                             selected = NULL)
             )
      ),
      column(6,
             div(class = "control-section",
                 h4("📍 Select Second Community Area"),
                 selectInput(ns("ward2"), NULL,
                             choices = NULL,  # Will be updated in server
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

# Server Module ----
wardComparisonServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get unique communities from data
    observe({
      communities <- sort(unique(data$community))
      community_labels <- str_to_title(communities)
      names(communities) <- community_labels
      
      updateSelectInput(session, "ward1", 
                        choices = communities,
                        selected = communities[1])
      updateSelectInput(session, "ward2", 
                        choices = communities,
                        selected = communities[2])
    })
    
    # Reactive data for selected wards
    ward_data <- reactive({
      req(input$ward1, input$ward2)
      
      data %>%
        filter(community %in% c(input$ward1, input$ward2)) %>%
        mutate(
          community_label = str_to_title(community),
          year = lubridate::year(date),
          year_month = lubridate::floor_date(date, "month")
        )
    })
    
    # Summary Statistics Table ----
    output$summary_table <- renderTable({
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
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Line Chart: Articles per Person Over Time ----
    output$line_chart <- renderPlot({
      req(ward_data())
      
      line_data <- ward_data() %>%
        group_by(community_label, year_month) %>%
        summarise(
          articles = sum(article_count, na.rm = TRUE),
          population = first(total_population),
          articles_per_1000 = (articles / population) * 1000,
          .groups = "drop"
        )
      
      ggplot(line_data, aes(x = year_month, y = articles_per_1000, 
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
    })
    
    # Pie Chart: Topic Distribution ----
    output$pie_chart <- renderPlot({
      req(ward_data())
      
      pie_data <- ward_data() %>%
        filter(random_topic != "No Coverage") %>%
        group_by(community_label, random_topic) %>%
        summarise(total = sum(article_count, na.rm = TRUE), .groups = "drop") %>%
        group_by(community_label) %>%
        mutate(
          percentage = total / sum(total) * 100,
          label = paste0(random_topic, "\n", round(percentage, 1), "%")
        )
      
      ggplot(pie_data, aes(x = "", y = total, fill = random_topic)) +
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
    })
    
    # Stacked Bar Chart: Topics by Year ----
    output$stacked_bar <- renderPlot({
      req(ward_data())
      
      bar_data <- ward_data() %>%
        filter(random_topic != "No Coverage") %>%
        group_by(community_label, year, random_topic) %>%
        summarise(total = sum(article_count, na.rm = TRUE), .groups = "drop")
      
      ggplot(bar_data, aes(x = factor(year), y = total, fill = random_topic)) +
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
    })
  })
}

# Example usage in main app:
# In UI:
#   tabPanel("Compare Areas", wardComparisonUI("ward_compare"))
#
# In Server:
#   wardComparisonServer("ward_compare", data = full_data)
