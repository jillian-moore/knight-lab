# load packages ----
library(shiny)
library(shinydashboard)
library(here)
library(tidyverse)
library(conflicted)
library(palmerpenguins)

# handle conflicts
tidymodels_prefer()
conflicts_prefer(shinydashboard::box)
conflicts_prefer(shiny::observe)

# ============================================================================
# server.R ----
function(input, output, session) {
  
  # reactive filtered data based on date range
  filtered_penguins <- reactive({
    req(input$startDate, input$endDate)
    penguins |>
      dplyr::filter(observation_date >= input$startDate, 
                    observation_date <= input$endDate)
  })
  
  # cumulative plot
  output$cumulative_plot <- renderPlotly({
    data_for_plot <- filtered_penguins() |>
      arrange(observation_date) |>
      group_by(observation_date) |>
      summarise(count = n(), .groups = 'drop') |>
      mutate(cumulative_count = cumsum(count))
    
    p <- ggplot(data_for_plot, aes(x = observation_date, y = cumulative_count)) +
      geom_col(fill = "steelblue") +
      labs(x = "Date", y = "Cumulative Count", title = "Cumulative Penguin Observations") +
      theme_minimal()
    
    suppressWarnings(ggplotly(p))
  })
  
  # overview plots
  output$species_island_plot <- renderPlotly({
    p <- ggplot(filtered_penguins(), aes(x = island, fill = species)) +
      geom_bar(position = "dodge") +
      labs(x = "Island", y = "Count", fill = "Species") +
      theme_minimal()
    
    suppressWarnings(ggplotly(p))
  })
  
  output$bill_plot <- renderPlotly({
    p <- ggplot(filtered_penguins(), aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
      geom_point() +
      labs(x = "Bill Length (mm)", y = "Bill Depth (mm)", color = "Species") +
      theme_minimal()
    
    suppressWarnings(ggplotly(p))
  })
  
  output$mass_plot <- renderPlotly({
    data_to_plot <- if (input$mass_species == "All") {
      filtered_penguins()
    } else {
      filtered_penguins() |> filter(species == input$mass_species)
    }
    
    p <- ggplot(data_to_plot, aes(x = species, y = body_mass_g, fill = species)) +
      geom_boxplot() +
      labs(x = "Species", y = "Body Mass (g)", fill = "Species") +
      theme_minimal()
    
    suppressWarnings(ggplotly(p))
  })
  
  # penguin finder
  output$penguin_results <- DT::renderDataTable({
    
    input$find_penguins
    
    isolate({
      filtered <- filtered_penguins()
      
      if (input$filter_species != "") {
        filtered <- filtered |> filter(species == input$filter_species)
      }
      
      if (input$filter_island != "") {
        filtered <- filtered |> filter(island == input$filter_island)
      }
      
      if (input$filter_sex != "") {
        filtered <- filtered |> filter(sex == input$filter_sex)
      }
      
      filtered <- filtered |>
        filter(!is.na(body_mass_g)) |>
        filter(body_mass_g >= input$filter_mass_min & body_mass_g <= input$filter_mass_max)
      
      if (nrow(filtered) > 0) {
        filtered |>
          select(species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, sex, observation_date) |>
          head(20)
      } else {
        data.frame(Message = "No penguins found with your criteria.")
      }
    })
  }, 
  options = list(pageLength = 10, scrollX = TRUE),
  server = FALSE
  )
}