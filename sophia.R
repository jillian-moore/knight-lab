# ================================================================
# Chicago Neighborhood Map - Dual Gradient (Functional + True Variation)
# ================================================================

library(shiny)
library(sf)
library(ggplot2)
library(dplyr)
library(scales)
library(readr)
library(here)
library(stringr)

# ---- 1. Load data ----
chicago <- st_read("~/Downloads/BOUNDARIES.geojson", quiet = TRUE)
census <- read_csv(here("data/ACS_5_Year_Data_by_Community_Area_20251007.csv"))

# ---- 2. Identify community column ----
text_cols_census <- names(census)[sapply(census, is.character)]
comm_col_guess <- text_cols_census[grepl("community|area|neigh", text_cols_census, ignore.case = TRUE)][1]
if (is.na(comm_col_guess)) stop("❌ Could not find community/area column in census data.")

census <- census %>%
  mutate(community = str_to_lower(str_trim(!!sym(comm_col_guess))))

text_cols_geo <- names(chicago)[sapply(chicago, is.character)]
neigh_col <- text_cols_geo[1]
chicago <- chicago %>%
  mutate(community = str_to_lower(str_trim(!!sym(neigh_col))))

# ---- 3. Define topics and demographic variables ----
set.seed(2025)
topics <- c("Immigration", "Education", "Housing", "Crime", "Health",
            "Environment", "Transportation", "Business", "Politics", "Culture")

demo_cols <- names(census)[sapply(census, is.numeric)]
demo_cols <- setdiff(demo_cols, "community")

# ---- 4. Simulate variable topic intensity per neighborhood ----
topic_data <- expand.grid(
  community = unique(chicago$community),
  topic = topics
) %>%
  mutate(
    # topic intensity depends on spatial order — creates regional gradient
    articles = runif(n(), 0, 1) * (rank(community) / length(unique(community))) +
      rnorm(n(), 0, 0.1)
  )

# ---- 5. Blend color function ----
blend_color <- function(x, y) {
  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  
  x <- rescale(x, to = c(0, 1))
  y <- rescale(y, to = c(0, 1))
  
  blend_intensity <- (x + y) / 2
  
  # vivid pastel gradient yellow → green → blue
  col_fun <- colorRamp(c("#FFFF00", "#00FF00", "#0000FF"))
  base_rgb <- col_fun(blend_intensity) / 255
  
  # pastel effect
  pastel_rgb <- base_rgb * 0.9 + 0.1
  rgb(pastel_rgb[, 1], pastel_rgb[, 2], pastel_rgb[, 3])
}

# ================================================================
# ---- 6. Shiny UI ----
# ================================================================
ui <- fluidPage(
  titlePanel("🟩 Chicago Neighborhoods: Topic × Demographic Blend"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_topic", "Select Topic (Blue):",
                  choices = topics, selected = "Housing"),
      selectInput("selected_demo", "Select Demographic (Yellow):",
                  choices = demo_cols, selected = demo_cols[1]),
      br(),
      helpText("🟨 = High demographic | 🟦 = Many topic articles | 🟩 = Both high")
    ),
    
    mainPanel(
      plotOutput("mapPlot", height = "700px")
    )
  )
)

# ================================================================
# ---- 7. Shiny Server ----
# ================================================================
server <- function(input, output, session) {
  output$mapPlot <- renderPlot({
    selected_topic <- input$selected_topic
    selected_demo  <- input$selected_demo
    
    # ---- Filter ----
    topic_sel <- topic_data %>% filter(topic == selected_topic)
    demo_sel  <- census %>% select(community, value = all_of(selected_demo))
    
    # ---- Merge ----
    map_data <- chicago %>%
      left_join(topic_sel, by = "community") %>%
      left_join(demo_sel, by = "community") %>%
      mutate(
        articles = ifelse(is.na(articles), 0, articles),
        value = ifelse(is.na(value), 0, value),
        # Ensure nonconstant variation
        x_scaled = rescale(articles, to = c(0, 1)),
        y_scaled = rescale(value, to = c(0, 1)),
        blend = mapply(blend_color, x_scaled, y_scaled)
      )
    
    # ---- Plot ----
    ggplot(map_data) +
      geom_sf(aes(fill = blend), color = "white", size = 0.25) +
      scale_fill_identity() +
      labs(
        title = paste0("Chicago Neighborhoods — ", selected_topic, " × ", selected_demo),
        subtitle = "🟨 Yellow = Demographic | 🟦 Blue = Topic | 🟩 Green = Overlap",
        caption = "Block Club Chicago (topics simulated) + Census demographics"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 13),
        legend.position = "none"
      )
  })
}

# ================================================================
# ---- 8. Run App ----
# ================================================================
shinyApp(ui, server)
