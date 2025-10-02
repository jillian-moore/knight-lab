# libraries ----
library(shiny)
library(tidyverse)

# load packages
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(stringr)
library(here)
library(tidymodels)
library(conflicted)
library(penguins)

# handle conflicts
tidymodels_prefer()
conflicts_prefer(shinydashboard::box)
conflicts_prefer(shiny::observe)

# source functions
source(here("r-scripts/helper_functions.R"))

# load data 
load(here("data/animals.rda"))

# clean data
animals <- animals |> 
  janitor::clean_names() |> 
  mutate(
    age_years = age_upon_intake / 52,
    age_years = round(age_years, digits = 2)
  ) |> 
  slice_sample(n = 10000)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Austin Animal Center"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Pet Matcher", tabName = "matcher", icon = icon("heart")),
      menuItem("Additional Information", tabName = "additional", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # overview tab
      tabItem(
        tabName = "overview",
        
        # welcome and data source side by side
        fluidRow(
          box(
            width = 6,
            status = "info",
            solidHeader = TRUE,
            title = "Welcome!",
            p("This dashboard helps staff and volunteers make data-informed decisions at the Austin Animal Shelter. The Austin Animal Center is the largest no-kill municipal animal shelter in the United States, 
        serving the City of Austin and Travis County, Texas. We encourage you to read our urgent action items below, browse the animals available for adoption, and find a pet match that fits the needs of your family."),
            
            div(
              style = "margin-top: 20px; display: flex; justify-content: space-between; align-items: center;",
              
              # left-aligned logo
              img(
                src = "logo.png", 
                alt = "Austin Animal Center Logo",
                style = "max-height: 330px; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);"
              ),
              
              # right-aligned image
              img(
                src = "dog.png", 
                alt = "Dog",
                style = "max-height: 330px; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);"
              )
            )
          ),
          
          box(
            width = 6,
            status = "info",
            solidHeader = TRUE,
            title = tags$span(
              icon("database"), 
              " Data Source"
            ),
            
            div(
              style = "padding: 15px;",
              
              # introduction
              p(
                style = "font-size: 16px; margin-bottom: 20px; color: #2c3e50;",
                "This application uses official data collected by the Austin Animal Center and published on data.gov. 
           The datasets provide comprehensive information about animal intakes and outcomes at Austin's municipal animal shelter."
              ),
              
              # dataset citations
              div(
                style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #17a2b8;",
                
                h4(
                  style = "color: #17a2b8; margin-top: 0; margin-bottom: 15px;",
                  icon("file-text"), " Dataset Citations"
                ),
                
                # first citation
                div(
                  style = "margin-bottom: 15px; padding: 10px; background-color: white; border-radius: 5px;",
                  strong("Austin Animal Center Outcomes:"), br(),
                  em("City of Austin. (2025, Feb. 25). Austin Animal Center Outcomes. Data.gov."), br(),
                  tags$a(
                    href = "https://catalog.data.gov/dataset/austin-animal-center-outcomes",
                    target = "_blank",
                    style = "color: #17a2b8; text-decoration: none;",
                    icon("external-link-alt"), " View Dataset"
                  )
                ),
                
                # second citation
                div(
                  style = "padding: 10px; background-color: white; border-radius: 5px;",
                  strong("Austin Animal Center Intakes:"), br(),
                  em("City of Austin. (2025, Feb. 25). Austin Animal Center Intakes. Data.gov."), br(),
                  tags$a(
                    href = "https://catalog.data.gov/dataset/austin-animal-center-intakes",
                    target = "_blank",
                    style = "color: #17a2b8; text-decoration: none;",
                    icon("external-link-alt"), " View Dataset"
                  )
                )
              ),
              
              # additional info
              div(
                style = "margin-top: 20px; padding: 15px; background-color: #e8f4f8; border-radius: 8px;",
                p(
                  style = "margin: 0; color: #2c3e50; font-size: 14px;",
                  icon("info-circle"), 
                  "Dog and cat breeds are placed into representative groups using functions written by dashboard maintenance. For example, the maine coon and ragdoll breeds are grouped into the Persian longhair category, and Australian shepherds and bearded collies are grouped into the herding category."
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = HTML('<span style="color:white;">Volunteers needed to care for stray animals<br><small style="color:white;"><i>Strays are more likely to enter the shelter injured or sick. Donating money and/or time is the best way to help us save strays!</i></small></span>'),
            status = "primary", solidHeader = TRUE,
            width = 6, height = 500,
            plotlyOutput("intake_plot")
          ),
          box(
            title = HTML('<span style="color:white;">Foster homes needed for springtime kittens<br><small style="color:white;"><i>Kittens are born seasonally, unlike other animals. We especially appreciate those looking to adopt and/or foster cats in the springtime!</i></small></span>'),
            status = "primary", solidHeader = TRUE,
            width = 6, height = 500,
            plotlyOutput("birth_month_plot")
          )
        ),
        
        fluidRow(
          box(
            title = HTML('<span style="color:white;">Breed Group Distribution<br><small style="color:white;"><i>Dogs in the sporting breed group and domestic shorthair cats are common at the Austin Animal Shelter.</i></small></span>'), 
            status = "primary", solidHeader = TRUE,
            width = 12, height = 500,
            fluidRow(
              column(12,
                     radioButtons("breed_type", "Select Animal Type:",
                                  choices = list("Dogs" = "dog", "Cats" = "cat"),
                                  selected = "dog",
                                  inline = TRUE)
              )
            ),
            plotlyOutput("breed_group_plot")
          )
        )
      ),
      
      # pet matcher tab
      tabItem(
        tabName = "matcher",
        fluidRow(
          shinydashboard::box(
            title = "Find Your Perfect Match!", status = "success", solidHeader = TRUE,
            width = 4,
            selectInput("match_type", "Animal Type:",
                        choices = c("Any" = "", "Dog", "Cat")),
            
            conditionalPanel(
              condition = "input.match_type == 'Dog'",
              selectInput("match_breed_group", "Dog Breed Group:",
                          choices = c("Any" = "", 
                                      "Herding" = "herding", 
                                      "Hound" = "hound", 
                                      "Non-Sporting" = "non_sporting", 
                                      "Sporting" = "sporting", 
                                      "Terrier" = "terrier", 
                                      "Toy" = "toy", 
                                      "Working" = "working"))
            ),
            
            conditionalPanel(
              condition = "input.match_type == 'Cat'",
              selectInput("match_cat_breed_group", "Cat Breed Group:",
                          choices = c("Any" = "", 
                                      "Oriental" = "oriental",
                                      "Persian/Longhair" = "persian_longhair", 
                                      "American/European" = "american_european",
                                      "Rex/Hairless" = "rex_hairless",
                                      "Wild/Hybrid" = "wild_hybrid",
                                      "Bobtail/Tailless" = "bobtail_tailless",
                                      "Specialty" = "specialty",
                                      "Domestic" = "domestic"))
            ),
            
            selectInput("match_sex", "Sex:",
                        choices = c("All" = "",
                                    "Neutered Male" = "Neutered Male",
                                    "Neutered Female" = "Neutered Female", 
                                    "Intact Male" = "Intact Male",
                                    "Intact Female" = "Intact Female"),
                        selected = ""),
            
            numericInput("match_age_min", "Minimum Age (years):", value = 0, min = 0, max = 20),
            numericInput("match_age_max", "Maximum Age (years):", value = 5, min = 0, max = 20),
            
            actionButton("find_matches", "Find My Match!", class = "btn-success", style = "width: 100%; margin-top: 10px;")
          ),
          
          shinydashboard::box(
            title = "Your Matches", status = "success", solidHeader = TRUE,
            width = 8,
            DT::dataTableOutput("match_results")
          )
        )
      ),
      
      # additional information tab
      tabItem(
        tabName = "additional",
        fluidRow(
          column(
            width = 8,
            # first insight box
            box(
              width = 12,
              status = "info",
              solidHeader = TRUE,
              title = tags$span(
                icon("chart-line"), 
                "Intake Condition by Intake Type Visualization"
              ),
              p("The core insight the intake condition by intake type visualization communicates is that stray animals arrive at the shelter in significantly worse health conditions than those surrendered by owners or transferred from other facilities. This critical information helps volunteers and staff understand that stray animals require immediate medical attention and more intensive care resources, making them priority cases for veterinary intervention and specialized foster care.")
            ),
            
            # second insight box
            box(
              width = 12,
              status = "info",
              solidHeader = TRUE,
              title = tags$span(
                icon("calendar"), 
                "Animal Type by Birth Month Visualization"
              ),
              p("The core insight the birth month by animal type visualization communicates is that cats experience a distinct seasonal breeding pattern with peak births occurring in spring and early summer months, while dogs maintain more consistent birth rates throughout the year. This knowledge allows the shelter to prepare for the annual 'kitten season' by recruiting additional foster families, scheduling spay/neuter clinics, and allocating extra resources during these predictable influx periods.")
            ),
            
            # third insight box
            box(
              width = 12,
              status = "info",
              solidHeader = TRUE,
              title = tags$span(
                icon("paw"), 
                " Breed Group Distribution Visualization"
              ),
              p("The core insight the breed group distribution with dog/cat widget communicates is that sporting and herding dog breeds, along with domestic shorthair cats, represent the largest populations at the shelter. This information helps potential adopters understand the types of animals most in need of homes, while also indicating that active families with yards might find excellent matches among the abundant sporting and herding breeds that require regular exercise and mental stimulation.")
            ),
            
            # fourth insight box
            box(
              width = 12,
              status = "info",
              solidHeader = TRUE,
              title = tags$span(
                icon("search"), 
                "Pet Matcher"
              ),
              p("The core insight the Pet Match feature with dropdown, type, and search widgets communicates is that personalized matching significantly improves adoption outcomes by allowing families to find animals that truly fit their lifestyle, housing situation, and preferences. This data-driven approach transforms the adoption process from browsing random listings to discovering compatible companions, ultimately reducing return rates and creating more successful long-term placements.")
            )
          ),
          
          column(
            width = 4,
            box(
              width = 12,
              status = "info",
              solidHeader = FALSE,
              title = NULL,
              div(
                style = "text-align: center; padding: 20px;",
                img(
                  src = "cat.png", 
                  alt = "Cat",
                  style = "max-width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);"
                )
              )
            )
          )
        ) 
      ) 
    )  
  )
)

# server
server <- function(input, output, session) {
  
  # add breed groups to the data
  animals$dog_breed_group <- sapply(animals$breed, replace_with_group)
  animals$cat_breed_group <- sapply(animals$breed, replace_with_cat_group)
  animals$dog_breed_group_clean <- gsub("_.*", "", animals$dog_breed_group)
  animals$cat_breed_group_clean <- gsub("_.*", "", animals$cat_breed_group)
  
  # update sex choices based on available data
  observe({
    # update sex choices for pet matcher
    sex_choices <- c("All" = "", sort(unique(animals$sex_upon_outcome[!is.na(animals$sex_upon_outcome)])))
    updateSelectInput(session, "match_sex", choices = sex_choices)
  })
  
  # overview plots
  output$intake_plot <- renderPlotly({
    animal_counts1 <- animals |>
      count(intake_type) |>
      arrange(desc(n))
    
    animals_for_plot <- animals |>
      mutate(intake_type = factor(intake_type, levels = animal_counts1$intake_type))
    
    p <- ggplot(animals_for_plot, aes(x = intake_type, fill = intake_condition)) +
      geom_bar(position = "stack") +
      labs(
        title = "Relationship Between Intake Type and Intake Condition",
        x = "Intake Type",
        y = "Animal Count",
        fill = "Intake Condition"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.title.x = element_text(size = 10), 
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 10), 
        plot.title = element_text(size = 10),   
        legend.title = element_text(size = 8),  
        legend.text = element_text(size = 7),   
        legend.key.size = unit(0.7, "lines"),
        legend.spacing = unit(0.5, "lines")
      )
    
    ggplotly(p)
  })
  
  output$birth_month_plot <- renderPlotly({
    animal_counts <- animals |>
      count(animal_type) |>
      arrange(desc(n))
    
    month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    animals_for_plot <- animals |>
      mutate(
        animal_type = factor(animal_type, levels = animal_counts$animal_type),
        birth_month = factor(birth_month, levels = month_levels)
      )
    
    p <- ggplot(animals_for_plot, aes(x = animal_type, fill = birth_month)) +
      geom_bar(position = "stack") +
      labs(
        title = "Relationship Between Animal Type and Birth Month",
        x = "Animal Type",
        y = "Animal Count", 
        fill = "Birth Month"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.title.x = element_text(size = 10), 
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 10), 
        plot.title = element_text(size = 10),   
        legend.title = element_text(size = 8),  
        legend.text = element_text(size = 7),   
        legend.key.size = unit(0.7, "lines"),
        legend.spacing = unit(0.5, "lines")
      )
    
    ggplotly(p)
  })
  
  output$breed_group_plot <- renderPlotly({
    if (input$breed_type == "dog") {
      breed_data <- animals |>
        filter(animal_type == "Dog", dog_breed_group_clean != "unknown") |>
        group_by(dog_breed_group_clean) |>
        summarise(count = n(), .groups = 'drop') |>
        arrange(desc(count))
      
      p <- ggplot(breed_data, aes(x = reorder(dog_breed_group_clean, count), y = count)) +
        geom_bar(stat = "identity", fill = "magenta") +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Dog Breed Group Distribution", 
          x = "Breed Group", 
          y = "Count"
        ) +
        theme(
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title = element_text(size = 9),
          plot.title = element_text(size = 10)
        )
    } else {
      breed_data <- animals |>
        filter(animal_type == "Cat", cat_breed_group_clean != "unknown") |>
        group_by(cat_breed_group_clean) |>
        summarise(count = n(), .groups = 'drop') |>
        arrange(desc(count))
      
      p <- ggplot(breed_data, aes(x = reorder(cat_breed_group_clean, count), y = count)) +
        geom_bar(stat = "identity", fill = "turquoise") +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Cat Breed Group Distribution", 
          x = "Breed Group", 
          y = "Count"
        ) +
        theme(
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title = element_text(size = 9),
          plot.title = element_text(size = 10)
        )
    }
    
    ggplotly(p)
  })
  
  # pet matcher
  output$match_results <- DT::renderDataTable({
    
    input$find_matches
    
    isolate({
      matched_animals <- animals
      
      # apply animal type filter
      if (!is.null(input$match_type) && input$match_type != "") {
        matched_animals <- matched_animals |>
          filter(animal_type == input$match_type)
      }
      
      # apply sex filter
      if (!is.null(input$match_sex) && input$match_sex != "") {
        matched_animals <- matched_animals |>
          filter(sex_upon_outcome == input$match_sex)
      }
      
      # apply age filter (using age_years now)
      matched_animals <- matched_animals |>
        filter(!is.na(age_years)) |>
        filter(age_years >= input$match_age_min & age_years <= input$match_age_max)
      
      # apply breed group filter for dogs
      if (input$match_type == "Dog" && !is.null(input$match_breed_group) && input$match_breed_group != "") {
        matched_animals <- matched_animals |>
          filter(!is.na(dog_breed_group_clean)) |>
          filter(dog_breed_group_clean == input$match_breed_group)
      }
      
      # apply breed group filter for cats
      if (input$match_type == "Cat" && !is.null(input$match_cat_breed_group) && input$match_cat_breed_group != "") {
        matched_animals <- matched_animals |>
          filter(!is.na(cat_breed_group_clean)) |>
          filter(cat_breed_group_clean == input$match_cat_breed_group)
      }
      
      # select and format results
      if (nrow(matched_animals) > 0) {
        result_data <- matched_animals |>
          select(name, animal_type, breed, color, age_years, birth_month, sex_upon_outcome) |>
          head(20) |>
          mutate(
            name = as.character(name),
            animal_type = as.character(animal_type),
            breed = as.character(breed),
            color = as.character(color),
            age_years = round(as.numeric(age_years), 2),
            birth_month = as.character(birth_month),
            sex_upon_outcome = as.character(sex_upon_outcome)
          )
        
        # clean up column names for display
        colnames(result_data) <- c("Name", "Type", "Breed", "Color", "Age (years)", "Birth Month", "Sex")
        
        return(result_data)
      } else {
        # return a simple data frame for "no matches"
        return(data.frame(
          Message = "No matches found with your criteria. Try adjusting your preferences!",
          stringsAsFactors = FALSE
        ))
      }
    })
  }, 
  options = list(
    pageLength = 10, 
    scrollX = TRUE,
    autoWidth = TRUE,
    columnDefs = list(list(width = '100px', targets = "_all"))
  ),
  server = FALSE
  )
  
}

# run the app
shinyApp(ui = ui, server = server)