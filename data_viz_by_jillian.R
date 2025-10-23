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

# merge data ----
full_data <- chi_boundaries |>
  left_join(api_long, by = "community") |> 
  mutate(random_topic = sample(topics, size = n(), replace = TRUE))

# DEMOGRAPHIC CHOICES ----
demo_choices <- c(
  "None" = "None",
  # Race / Ethnicity
  "White" = "white",
  "Black or African American" = "black_or_african_american",
  "American Indian or Alaska Native" = "american_indian_or_alaska_native",
  "Asian" = "asian",
  "Native Hawaiian or Pacific Islander" = "native_hawaiian_or_pacific_islander",
  "Other Race" = "other_race",
  "Multiracial" = "multiracial",
  "Hispanic or Latino" = "hispanic_or_latino",
  # Income
  "Under $25,000" = "under_25_000",
  "$25,000 to $49,999" = "x25_000_to_49_999",
  "$50,000 to $74,999" = "x50_000_to_74_999",
  "$75,000 to $125,000" = "x75_000_to_125_000",
  "$125,000 +" = "x125_000_plus",
  # Age
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
      selectInput("blue_var", "Select Topic (Blue):", choices = c("None", topics), selected = "None"),
      selectInput("demo_var", "Select Demographic (Yellow):",
                  choices = demo_choices,
                  selected = "None"
      ),
      sliderInput(
        "month_slider", "Select Month (simulated):",
        min = 1, max = 12, value = 1, step = 1,
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

# Helper ----
safe_rescale <- function(x) {
  if (all(is.na(x)) || length(unique(na.omit(x))) <= 1) return(rep(0.2, length(x)))
  rescale(x, to = c(0, 1))
}

# Server ----
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(full_data) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -87.65, lat = 41.84, zoom = 10)
  })
  
  observe({
    df <- full_data
    n <- nrow(df)
    
    # ---- Simulated BLUE layer (topic intensity) ----
    set.seed(as.integer(as.numeric(input$month_slider) %% 1e6))
    blue_vals <- runif(n, 0, 1)
    blue_palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)
    blue_colors <- blue_palette[as.numeric(cut(blue_vals, breaks = 100))]
    fill_colors <- rep("#D9D9D9", n)
    fill_opacity <- rep(0.8, n)
    if (input$blue_var != "None") fill_colors <- blue_colors
    
    # ---- YELLOW (Census / demographic) ----
    if (input$demo_var != "None" && input$demo_var %in% names(df)) {
      g_vals <- suppressWarnings(as.numeric(df[[input$demo_var]]))
      g_vals[is.na(g_vals)] <- median(g_vals, na.rm = TRUE)
      g_scaled <- safe_rescale(g_vals)
      
      yellow_base <- "#FFD700"
      yellow_opacity <- g_scaled * 0.8 + 0.1
      
      if (input$blue_var != "None") {
        blended_colors <- mapply(function(b_col, y_op) {
          b_rgb <- col2rgb(b_col)/255
          y_rgb <- col2rgb(yellow_base)/255
          mixed <- (1 - y_op) * b_rgb + y_op * y_rgb
          rgb(mixed[1], mixed[2], mixed[3])
        }, fill_colors, yellow_opacity, SIMPLIFY = TRUE)
        fill_colors <- blended_colors
        fill_opacity <- rep(0.9, n)
      } else {
        fill_colors <- rep(yellow_base, n)
        fill_opacity <- yellow_opacity
      }
    } else {
      fill_opacity <- ifelse(input$blue_var == "None", 0.6, 0.8)
    }
    
    # ---- Render polygons ----
    leafletProxy("map", data = df) |>
      clearShapes() |>
      addPolygons(
        fillColor = fill_colors,
        color = "#555", weight = 1,
        fillOpacity = fill_opacity,
        label = ~paste0("<b>", str_to_title(community), "</b><br>",
                        "🟦 Topic: ", input$blue_var, "<br>🟨 Demographic: ",
                        ifelse(input$demo_var == "None", "None", 
                               names(demo_choices)[demo_choices == input$demo_var])),
        labelOptions = labelOptions(direction = "auto", textsize = "12px", sticky = TRUE)
      )
  })
}

# Run ----
shinyApp(ui, server)
