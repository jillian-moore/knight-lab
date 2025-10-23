# PRIMARY DATA VIZ ----

# packages ----
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(scales)
library(RColorBrewer)
library(lubridate)
library(here)
library(tidyr)

# load data ----
source("data_clean_by_jillian.R")

chi_boundaries <- read_csv(here("data/chi_boundaries.csv")) |> 
  janitor::clean_names()

census_raw <- read_csv(here("data/ACS_5_Year_Data_by_Community_Area_20251007.csv")) |> 
  janitor::clean_names()

# clean data ----
census <- census_raw |>
  mutate(
    age_0_17 = male_0_to_17 + female_0_to_17,
    age_18_24 = male_18_to_24 + female_18_to_24,
    age_25_34 = male_25_to_34 + female_25_to_34,
    age_35_49 = male_35_to_49 + female_35_to_49,
    age_50_64 = male_50_to_64 + female_50_to_64,
    age_65_plus = male_65 + female_65
  ) |> 
  rename("community" = "community_area")

# prepare shape ----
if (!"the_geom" %in% names(chi_boundaries)) stop("❌ chi_boundaries.csv must include 'the_geom' column.")
chi_boundaries <- st_as_sf(chi_boundaries, wkt = "the_geom", crs = 4326)

# combine census and shapefile ----
chi_boundaries <- chi_boundaries |>
  left_join(census, by = "community")

# pivot longer ----
api_long <- api_clean |>
  pivot_longer(
    cols = starts_with("neighborhood"), 
    names_to = "neighborhood_col",
    values_to = "community"
  ) |>
  filter(!is.na(community) & community != "") |>
  select(-neighborhood_col)

# selected topics ----
topics <- c(
  "Arts & Culture",
  "Business",
  "Crime & Public Safety",
  "Education",
  "Food & Restaurants",
  "Health & Environment",
  "Housing",
  "Immigration",
  "Politics",
  "Social Movements",
  "Sports & Recreation",
  "Transportation & Infrastructure"
)

# aggregate api data by community
api_summary <- api_long |>
  group_by(community) |>
  summarise(
    article_count = n(),
    .groups = "drop"
  )

# merge data ----
full_data <- chi_boundaries |>
  left_join(api_summary, by = "community") |> 
  mutate(
    article_count = replace_na(article_count, 0),
    random_topic = sample(topics, size = n(), replace = TRUE)
  )

# helper function for rescaling ----
safe_rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE)) {
  if (all(is.na(x)) || length(unique(x[!is.na(x)])) <= 1) {
    return(rep(0.5, length(x)))
  }
  scales::rescale(x, to = to, from = from)
}

# demographic titles ----
demo_choices <- c(
  "None" = "None",
  
  # race
  "White" = "white",
  "Black or African American" = "black_or_african_american",
  "American Indian or Alaska Native" = "american_indian_or_alaska_native",
  "Asian" = "asian",
  "Native Hawaiian or Pacific Islander" = "native_hawaiian_or_pacific_islander",
  "Other Race" = "other_race",
  "Multiracial" = "multiracial",
  "Hispanic or Latino" = "hispanic_or_latino",
  
  # income
  "Under $25,000" = "under_25_000",
  "$25,000 to $49,999" = "x25_000_to_49_999",
  "$50,000 to $74,999" = "x50_000_to_74_999",
  "$75,000 to $125,000" = "x75_000_to_125_000",
  "$125,000 +" = "x125_000_plus",
  
  # age
  "0 to 17" = "age_0_17",
  "18 to 24" = "age_18_24",
  "25 to 34" = "age_25_34",
  "35 to 49" = "age_35_49",
  "50 to 64" = "age_50_64",
  "65+" = "age_65_plus"
)

# UI ----
ui <- fluidPage(
  titlePanel("Chicago Neighborhood Map: Topics vs Demographics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("blue_var", "Select Topic (Blue):", 
                  choices = c("None", topics), 
                  selected = "None"),
      selectInput("demo_var", "Select Demographic (Yellow):",
                  choices = demo_choices,
                  selected = "None"),
      sliderInput(
        "month_slider", "Select Month:",
        min = as.Date("2020-01-01"), 
        max = as.Date("2030-12-01"),
        value = as.Date("2020-01-01"),
        step = 31,
        animate = animationOptions(interval = 2000, loop = TRUE)
      ),
      tags$div(style="margin-top:20px;font-size:12px;",
               tags$b("Legend:"), br(),
               tags$span(style="color:blue;", "■ Blue = Topic intensity"), br(),
               tags$span(style="color:gold;", "■ Yellow = Demographic intensity"), br(),
               tags$span(style="color:green;", "■ Green = Overlap intensity"))
    ),
    mainPanel(
      leafletOutput("map", height = "800px"),
      tags$div(style="text-align:center; margin-top:10px; font-size:12px; color:#777;",
               "Concept visualization — Block Club Chicago topics vs demographics")
    )
  )
)

# server ----
server <- function(input, output, session) {
  
  # initial map
  output$map <- renderLeaflet({
    leaflet(full_data) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -87.65, lat = 41.84, zoom = 10)
  })
  
  observe({
    df <- full_data
    n <- nrow(df)
    
    # blue: topic intensity ----
    if (input$blue_var != "None") {
      # use random_topic for demonstration
      topic_palette <- colorRampPalette(brewer.pal(9, "Blues"))(length(topics))
      names(topic_palette) <- topics
      fill_colors <- topic_palette[df$random_topic]
    } else {
      fill_colors <- rep("#D9D9D9", n)
    }
    
    fill_opacity <- rep(0.8, n)
    
    # yellow: demographic intensity ----
    if (input$demo_var != "None" && input$demo_var %in% names(df)) {
      g_vals <- suppressWarnings(as.numeric(df[[input$demo_var]]))
      g_vals[is.na(g_vals)] <- median(g_vals, na.rm = TRUE)
      g_scaled <- safe_rescale(g_vals)
      
      yellow_base <- "#FFD700"
      yellow_intensity <- g_scaled * 0.8 + 0.1
      
      if (input$blue_var != "None") {
        # blend blue + yellow
        blended_colors <- sapply(seq_along(fill_colors), function(i) {
          b_rgb <- col2rgb(fill_colors[i]) / 255
          y_rgb <- col2rgb(yellow_base) / 255
          mixed <- (1 - yellow_intensity[i]) * b_rgb + yellow_intensity[i] * y_rgb
          rgb(mixed[1], mixed[2], mixed[3])
        })
        fill_colors <- blended_colors
        fill_opacity <- rep(0.9, n)
      } else {
        fill_colors <- rep(yellow_base, n)
        fill_opacity <- yellow_intensity
      }
    } else {
      fill_opacity <- ifelse(input$blue_var == "None", 0.6, 0.8)
    }
    
    # render polygons ----
    leafletProxy("map", data = df) |>
      clearShapes() |>
      addPolygons(
        fillColor = fill_colors,
        color = "#555", weight = 1,
        fillOpacity = fill_opacity,
        label = ~paste0("<b>", str_to_title(community), "</b><br>",
                        "🟦 Topic: ", random_topic, "<br>🟨 Demographic: ",
                        ifelse(input$demo_var == "None", "None", 
                               names(demo_choices)[demo_choices == input$demo_var])),
        labelOptions = labelOptions(direction = "auto", textsize = "12px", sticky = TRUE)
      )
  })
}

# run app ----
shinyApp(ui, server)