# ================================================================
# Chicago Map — Demographics (Yellow from Census) × Topic (Blue Simulated)
# ================================================================

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

# ================================================================
# ---- 1. Load Data ----
# ================================================================
boundaries_path <- here("data", "chi_boundaries.csv")
if (!file.exists(boundaries_path)) boundaries_path <- "~/Downloads/knightlab!/data/chi_boundaries.csv"
chi_boundaries <- read_csv(path.expand(boundaries_path), show_col_types = FALSE)

census_path <- here("data", "ACS_5_Year_Data_by_Community_Area_20251007.csv")
if (!file.exists(census_path)) census_path <- "~/Downloads/knightlab!/data/ACS_5_Year_Data_by_Community_Area_20251007.csv"
census_raw <- read_csv(path.expand(census_path), show_col_types = FALSE)


api <- readRDS(here("data/api_scrape.rds"))



message("✅ Using chi_boundaries from: ", boundaries_path)
message("✅ Using ACS Census file from: ", census_path)

# ================================================================
# ---- 2. Clean Census ----
# ================================================================
col_names <- c(
  "ACS Year","Community Area",
  "Under $25,000","$25,000 to $49,999","$50,000 to $74,999",
  "$75,000 to $125,000","$125,000 +",
  "Male 0 to 17","Male 18 to 24","Male 25 to 34","Male 35 to 49",
  "Male 50 to 64","Male 65+",
  "Female 0 to 17","Female 18 to 24","Female 25 to 34","Female 35 to 49",
  "Female 50 to 64","Female 65 +",
  "Total Population","White","Black or African American","American Indian or Alaska Native",
  "Asian","Native Hawaiian or Pacific Islander","Other Race","Multiracial",
  "White Not Hispanic or Latino","Hispanic or Latino","Record ID"
)
if (ncol(census_raw) == length(col_names)) names(census_raw) <- col_names

# Numeric cleaning — robust
census <- census_raw %>%
  mutate(across(-`Community Area`,
                ~ suppressWarnings(as.numeric(str_replace_all(
                  as.character(.x), "[^0-9\\.\\-]", ""
                ))))) %>%
  mutate(community = str_to_lower(str_trim(`Community Area`)))

# ================================================================
# ---- 3. Prepare Shape ----
# ================================================================
chicago <- chi_boundaries %>%
  mutate(community = str_to_lower(str_trim(COMMUNITY)))

if (!"the_geom" %in% names(chicago)) stop("❌ chi_boundaries.csv must include 'the_geom' column.")
chicago <- st_as_sf(chicago, wkt = "the_geom", crs = 4326)

chicago_data <- chicago %>%
  left_join(census, by = "community")

# ================================================================
# ---- 4. Simulated Blue Data ----
# ================================================================
topics <- c("Immigration","Education","Housing","Crime","Health",
            "Environment","Transportation","Business","Politics","Culture")

# ================================================================
# ---- 5. UI ----
# ================================================================
ui <- fluidPage(
  titlePanel("Chicago Neighborhood Map: Topics vs Demographics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("blue_var", "Select Topic (Blue):", choices = c("None", topics), selected = "None"),
      selectInput("demo_var", "Select Demographic (Yellow):",
                  choices = list(
                    "None" = "None",
                    "Race / Ethnicity" = c(
                      "White", "Black or African American", "American Indian or Alaska Native",
                      "Asian", "Native Hawaiian or Pacific Islander", "Other Race",
                      "Multiracial", "Hispanic or Latino"
                    ),
                    "Income" = c(
                      "Under $25,000", "$25,000 to $49,999", "$50,000 to $74,999",
                      "$75,000 to $125,000", "$125,000 +"
                    ),
                    "Age (Male)" = c(
                      "Male 0 to 17","Male 18 to 24","Male 25 to 34",
                      "Male 35 to 49","Male 50 to 64","Male 65+"
                    ),
                    "Age (Female)" = c(
                      "Female 0 to 17","Female 18 to 24","Female 25 to 34",
                      "Female 35 to 49","Female 50 to 64","Female 65 +"
                    )
                  ),
                  selected = "None"
      ),
      sliderInput(
        "month_slider", "Select Month:",
        min = as.Date("2020-01-01"), max = as.Date("2030-12-01"),
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

# ================================================================
# ---- 6. Helper ----
# ================================================================
safe_rescale <- function(x) {
  if (all(is.na(x)) || length(unique(na.omit(x))) <= 1) return(rep(0.2, length(x)))
  rescale(x, to = c(0, 1))
}

# ================================================================
# ---- 7. Server ----
# ================================================================
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(chicago_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -87.65, lat = 41.84, zoom = 10)
  })
  
  observe({
    df <- chicago_data
    n <- nrow(df)
    
    # ---- Simulated BLUE layer ----
    set.seed(as.integer(as.numeric(input$month_slider) %% 1e6))
    blue_vals <- runif(n, 0, 1)
    blue_palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)
    blue_colors <- blue_palette[as.numeric(cut(blue_vals, breaks = 100))]
    fill_colors <- rep("#D9D9D9", n)
    fill_opacity <- rep(0.8, n)
    if (input$blue_var != "None") fill_colors <- blue_colors
    
    # ---- YELLOW (Census) ----
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
    }
    
    # ---- Render ----
    leafletProxy("map", data = df) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = fill_colors,
        color = "#555", weight = 1,
        fillOpacity = fill_opacity,
        label = ~paste0("<b>", str_to_title(community), "</b><br>",
                        "🟦 ", input$blue_var, "<br>🟨 ", input$demo_var)
      )
  })
}

# ================================================================
# ---- 8. Run ----
# ================================================================
shinyApp(ui, server)
