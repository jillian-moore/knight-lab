# ================================================================
# Chicago Map — Demographics (Green Opacity from Census) × Topic (Blue Simulated)
# ================================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(scales)
library(RColorBrewer)
library(here)

# ================================================================
# ---- 1. Load Data ----
# ================================================================
# Load chi_boundaries.csv from here() or working directory
possible_boundaries <- c(
  here("data", "chi_boundaries.csv"),
  "data/chi_boundaries.csv",
  "chi_boundaries.csv"
)

boundaries_path <- possible_boundaries[file.exists(possible_boundaries)][1]
if (is.na(boundaries_path)) stop("❌ Could not find chi_boundaries.csv in data folder or working directory.")
message("✅ Using chi_boundaries from: ", boundaries_path)

chi_boundaries <- read_csv(boundaries_path, show_col_types = FALSE)

# Directly reference your ACS file
census_path <- "~/Downloads/knightlab!/data/ACS_5_Year_Data_by_Community_Area_20251007.csv"
if (!file.exists(path.expand(census_path))) stop("❌ Could not find ACS Census file at specified path.")
message("✅ Using ACS Census file from: ", census_path)

census_raw <- read_csv(path.expand(census_path), show_col_types = FALSE)

# ================================================================
# ---- 2. Clean & Prepare Census Data ----
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

if (ncol(census_raw) == length(col_names)) {
  names(census_raw) <- col_names
}

census <- census_raw %>%
  mutate(across(-`Community Area`,
                ~ suppressWarnings(as.numeric(str_replace_all(.x, "[^0-9.-]", ""))))) %>%
  mutate(community = str_to_lower(str_trim(`Community Area`)))

# ================================================================
# ---- 3. Prepare Shape Data ----
# ================================================================
chicago <- chi_boundaries %>%
  mutate(community = str_to_lower(str_trim(COMMUNITY)))

if (!"the_geom" %in% names(chicago)) {
  stop("❌ chi_boundaries.csv must include a 'the_geom' column containing WKT geometry.")
}

# Convert to sf
chicago <- st_as_sf(chicago, wkt = "the_geom", crs = 4326)

# Merge
chicago_data <- chicago %>%
  left_join(census, by = "community")

# ================================================================
# ---- 4. Simulated Blue Layer ----
# ================================================================
set.seed(2025)
topics <- c("Immigration","Education","Housing","Crime","Health",
            "Environment","Transportation","Business","Politics","Culture")

random_blue_values <- tibble(
  community = unique(chicago$community),
  blue_value = runif(length(unique(chicago$community)), 0, 1)^2
)

blue_choices <- c("None", topics)

# ================================================================
# ---- 5. UI ----
# ================================================================
ui <- fluidPage(
  titlePanel("Chicago Map — Demographics × Topics"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("blue_var", "Select Topic Category:",
                  choices = blue_choices, selected = "None"),
      
      selectInput(
        "demo_var", "Select Demographic:",
        choices = c(
          "None" = "None",
          "Race / Ethnicity" = c(
            "White",
            "Black or African American",
            "American Indian or Alaska Native",
            "Asian",
            "Native Hawaiian or Pacific Islander",
            "Other Race",
            "Multiracial",
            "Hispanic or Latino"
          ),
          "Income" = c(
            "Under $25,000",
            "$25,000 to $49,999",
            "$50,000 to $74,999",
            "$75,000 to $125,000",
            "$125,000 +"
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
      )
    ),
    mainPanel(leafletOutput("map", height = "800px"))
  )
)

# ================================================================
# ---- 6. Helpers ----
# ================================================================
safe_rescale <- function(x) {
  if (all(is.na(x)) || length(unique(na.omit(x))) <= 1) return(rep(0, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0.5, length(x)))
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
    
    fill_colors <- rep("#D9D9D9", n)
    fill_opacity <- rep(0.8, n)
    
    # ---- BLUE (simulated topic) ----
    blue_vals <- random_blue_values$blue_value[match(df$community, random_blue_values$community)]
    blue_palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)
    blue_colors <- blue_palette[as.numeric(cut(blue_vals, breaks = 100))]
    
    if (input$blue_var != "None") {
      fill_colors <- blue_colors
    }
    
    # ---- GREEN (from Census) ----
    if (!is.null(input$demo_var) && input$demo_var != "None" && input$demo_var %in% names(df)) {
      g_vals <- df[[input$demo_var]]
      
      if (!is.numeric(g_vals)) {
        g_vals <- suppressWarnings(as.numeric(str_replace_all(as.character(g_vals), "[^0-9.-]", "")))
      }
      
      if (!all(is.na(g_vals))) {
        g_scaled <- safe_rescale(g_vals)
        green_base <- "#00FF00"
        green_opacity <- g_scaled * 0.8 + 0.1
        
        if (input$blue_var != "None") {
          blended_colors <- mapply(function(b_col, g_op) {
            b_rgb <- col2rgb(b_col) / 255
            g_rgb <- col2rgb(green_base) / 255
            mixed <- (1 - g_op) * b_rgb + g_op * g_rgb
            rgb(mixed[1], mixed[2], mixed[3])
          }, fill_colors, green_opacity, SIMPLIFY = TRUE)
          
          fill_colors <- blended_colors
          fill_opacity <- rep(0.9, n)
        } else {
          fill_colors <- rep(green_base, n)
          fill_opacity <- green_opacity
        }
      }
    }
    
    # ---- Render ----
    leafletProxy("map", data = df) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = fill_colors,
        color = "#555",
        weight = 1,
        opacity = 1,
        fillOpacity = fill_opacity,
        label = ~paste0("<b>", str_to_title(community), "</b><br>",
                        "🟩 ", input$demo_var, ": ", prettyNum(df[[input$demo_var]], big.mark = ","), "<br>",
                        "🟦 ", input$blue_var)
      )
  })
}

# ================================================================
# ---- 8. Run ----
# ================================================================
shinyApp(ui, server)
