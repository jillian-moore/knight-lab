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
library(janitor)

# load data ----
source("data_clean_by_jillian.R") # api_clean must be loaded here

chi_boundaries <- read_csv(here("data/chi_boundaries.csv")) |> 
  clean_names()

census_raw <- read_csv(here("data/ACS_5_Year_Data_by_Community_Area_20251007.csv")) |> 
  clean_names()

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
  rename(community = community_area)

# prepare shape ----
if (!"the_geom" %in% names(chi_boundaries)) stop("❌ chi_boundaries.csv must include 'the_geom' column.")

# --- FIX: Safe geometry conversion ---
# Safe WKT parsing
chi_boundaries <- chi_boundaries %>%
  # Convert WKT string to sfc (list of sfg)
  mutate(the_geom_parsed = lapply(the_geom, function(wkt) {
    tryCatch(st_as_sfc(wkt, crs = 4326), error = function(e) NULL)
  })) %>%
  # Remove rows where parsing failed
  filter(!sapply(the_geom_parsed, is.null)) %>%
  # Flatten list-column into sfc
  mutate(the_geom = st_sfc(do.call(c, the_geom_parsed), crs = 4326)) %>%
  select(-the_geom_parsed) %>%
  st_as_sf(sf_column_name = "the_geom")

# combine census and shapefile ----
chi_boundaries <- chi_boundaries |> left_join(census, by = "community")

# process API ----
api_cols <- tolower(names(api_clean))
community_col <- api_cols[grepl("community|neigh", api_cols)][1]
if (is.null(community_col) || is.na(community_col)) stop("❌ API data must include a community or neighborhood column.")

api_long <- api_clean |>
  rename(community = all_of(community_col)) |>
  filter(!is.na(community) & community != "")

# detect topic column
topic_col <- api_cols[grepl("topic", api_cols)][1]
if (!is.null(topic_col) && !is.na(topic_col)) {
  api_long <- api_long |> rename(topic = all_of(topic_col))
} else {
  api_long$topic <- NA
}

# topic categories ----
topics <- c(
  "Arts & Culture", "Business", "Crime & Public Safety", "Education",
  "Food & Restaurants", "Health & Environment", "Housing", "Immigration",
  "Politics", "Social Movements", "Sports & Recreation", "Transportation & Infrastructure"
)

# summarize API by community ----
api_summary <- api_long |>
  group_by(community) |>
  summarise(article_count = n(), .groups = "drop")

# merge all data ----
full_data <- chi_boundaries |>
  left_join(api_summary, by = "community") |>
  mutate(article_count = replace_na(article_count, 0))

# helper function for rescaling ----
safe_rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE)) {
  if (all(is.na(x)) || length(unique(x[!is.na(x)])) <= 1) {
    return(rep(0.5, length(x)))
  }
  scales::rescale(x, to = to, from = from)
}

# extended green palette for overlays
green_palette <- c("#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476",
                   "#41ab5d", "#238b45", "#006d2c", "#00441b")

# demographic categories ----
demo_choices <- c(
  "None" = "None",
  "White" = "white",
  "Black or African American" = "black_or_african_american",
  "American Indian or Alaska Native" = "american_indian_or_alaska_native",
  "Asian" = "asian",
  "Native Hawaiian or Pacific Islander" = "native_hawaiian_or_pacific_islander",
  "Other Race" = "other_race",
  "Multiracial" = "multiracial",
  "Hispanic or Latino" = "hispanic_or_latino",
  "Under $25,000" = "under_25_000",
  "$25,000 to $49,999" = "x25_000_to_49_999",
  "$50,000 to $74,999" = "x50_000_to_74_999",
  "$75,000 to $125,000" = "x75_000_to_125_000",
  "$125,000 +" = "x125_000_plus",
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
                  choices = c("None", topics), selected = "None"),
      selectInput("demo_var", "Select Demographic (Yellow):",
                  choices = demo_choices, selected = "None"),
      sliderInput(
        "month_slider", "Select Month:",
        min = as.Date("2020-01-01"),
        max = as.Date("2030-12-01"),
        value = as.Date("2020-01-01"),
        step = 31,
        animate = animationOptions(interval = 2000, loop = TRUE)
      ),
      tags$div(style = "margin-top:20px;font-size:12px;",
               tags$b("Legend:"), br(),
               tags$span(style = "color:blue;", "■ Blue = Topic intensity"), br(),
               tags$span(style = "color:gold;", "■ Yellow = Demographic intensity"), br(),
               tags$span(style = "color:green;", "■ Green = Overlap intensity"))
    ),
    mainPanel(
      leafletOutput("map", height = "800px"),
      tags$div(style = "text-align:center; margin-top:10px; font-size:12px; color:#777;",
               "Concept visualization — Block Club Chicago topics vs demographics")
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # detect date column automatically
  date_col <- intersect(names(api_long),
                        c("published_at", "pub_date", "date", "created_at", "timestamp"))[1]
  
  # reactive filtering based on month slider
  filtered_data <- reactive({
    df <- api_long
    if (!is.null(date_col) && !is.na(date_col)) {
      df[[date_col]] <- suppressWarnings(as.Date(df[[date_col]]))
      df <- df |> filter(!is.na(.data[[date_col]]) & .data[[date_col]] <= input$month_slider)
    }
    df <- df |>
      group_by(community) |>
      summarise(article_count = n(), .groups = "drop") |>
      right_join(chi_boundaries, by = "community") |>
      mutate(article_count = replace_na(article_count, 0))
    df
  })
  
  # base map
  output$map <- renderLeaflet({
    leaflet(full_data) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -87.65, lat = 41.84, zoom = 10)
  })
  
  # reactive polygon rendering
  observe({
    df <- filtered_data()
    
    # Make sure df is an sf object
    if (!inherits(df, "sf")) {
      df <- st_as_sf(df, sf_column_name = "the_geom", crs = 4326)
    }
    
    n <- nrow(df)
    
    blue_norm <- if (input$blue_var != "None") safe_rescale(df$article_count) else rep(0, n)
    yellow_norm <- if (input$demo_var != "None" && input$demo_var %in% names(df)) {
      vals <- suppressWarnings(as.numeric(df[[input$demo_var]]))
      vals[is.na(vals)] <- median(vals, na.rm = TRUE)
      safe_rescale(vals)
    } else rep(0, n)
    
    fill_colors <- rep("#D9D9D9", n)
    fill_opacity <- rep(0.6, n)
    
    if (input$blue_var != "None" && input$demo_var == "None") {
      blue_palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)
      fill_colors <- blue_palette[ceiling(blue_norm * 99) + 1]
      fill_opacity <- blue_norm * 0.9 + 0.1
    } else if (input$blue_var == "None" && input$demo_var != "None") {
      fill_colors <- rep("#FFD700", n)
      fill_opacity <- yellow_norm * 0.9 + 0.1
    } else if (input$blue_var != "None" && input$demo_var != "None") {
      intensity <- pmin(blue_norm + yellow_norm, 1)
      fill_colors <- colorRampPalette(green_palette)(100)[ceiling(intensity * 99) + 1]
      fill_opacity <- intensity * 0.9 + 0.1
    }
    
    leafletProxy("map", data = df) |>
      clearShapes() |>
      addPolygons(
        fillColor = fill_colors,
        color = "#555", weight = 1,
        fillOpacity = fill_opacity,
        label = ~paste0("<b>", str_to_title(community), "</b><br>",
                        "🟦 Topic count: ", article_count, "<br>🟨 Demographic: ",
                        ifelse(input$demo_var == "None", "None",
                               names(demo_choices)[demo_choices == input$demo_var])),
        labelOptions = labelOptions(direction = "auto", textsize = "12px", sticky = TRUE)
      )
  })
  
}

# RUN APP ----
shinyApp(ui, server)
