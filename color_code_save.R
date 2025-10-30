# BLUE opacity
blue_opacity <- rep(0, n)
if (!is.null(input$blue_var) && input$blue_var != "None") {
  max_val <- max(map_data$display_value, na.rm = TRUE)
  if (!is.infinite(max_val) && max_val > 0) {
    blue_opacity <- map_data$display_value / max_val
  }
}

# YELLOW opacity
yellow_opacity <- rep(0, n)
if (!is.null(input$demo_var) && input$demo_var != "None" && input$demo_var %in% names(map_data)) {
  demo_vals <- suppressWarnings(as.numeric(map_data[[input$demo_var]]))
  demo_vals[is.na(demo_vals)] <- 0
  max_demo <- max(demo_vals, na.rm = TRUE)
  if (!is.infinite(max_demo) && max_demo > 0) {
    yellow_opacity <- demo_vals / max_demo
  }
}

# Determine fill colors
fill_colors <- rep("#D9D9D9", n)
fill_opacity <- rep(0.3, n)

if (!is.null(input$blue_var) && input$blue_var != "None" && (is.null(input$demo_var) || input$demo_var == "None")) {
  fill_colors <- rep("#0066CC", n)
  fill_opacity <- pmax(blue_opacity * 0.85, 0.1)
} else if ((is.null(input$blue_var) || input$blue_var == "None") && !is.null(input$demo_var) && input$demo_var != "None") {
  fill_colors <- rep("#FFD700", n)
  fill_opacity <- pmax(yellow_opacity * 0.85, 0.1)
} else if (!is.null(input$blue_var) && input$blue_var != "None" && !is.null(input$demo_var) && input$demo_var != "None") {
  fill_colors <- sapply(1:n, function(i) {
    blue_val <- blue_opacity[i]
    yellow_val <- yellow_opacity[i]
    if(blue_val == 0 && yellow_val == 0) return("#D9D9D9")
    total <- blue_val + yellow_val
    blue_weight <- blue_val / total
    yellow_weight <- yellow_val / total
    if(blue_weight > yellow_weight){
      green_base_rgb <- c(64, 176, 80)
      blue_green_rgb <- c(32, 160, 192)
      shift_strength <- (blue_weight - 0.5) * 2
      blended_rgb <- green_base_rgb * (1 - shift_strength) + blue_green_rgb * shift_strength
    } else {
      green_base_rgb <- c(64, 176, 80)
      yellow_green_rgb <- c(144, 192, 64)
      shift_strength <- (yellow_weight - 0.5) * 2
      blended_rgb <- green_base_rgb * (1 - shift_strength) + yellow_green_rgb * shift_strength
    }
    rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3], maxColorValue = 255)
  })
  fill_opacity <- pmax(pmax(blue_opacity, yellow_opacity) * 0.85, 0.1)
}

# Create labels
metric_label <- if(input$metric_type == "per_capita") {
  "Articles per 1,000"
} else {
  "Articles"
}

leafletProxy("map", data = map_data) |>
  clearShapes() |>
  addPolygons(
    fillColor = fill_colors,
    color = "#555",
    weight = 1,
    fillOpacity = fill_opacity,
    label = ~lapply(paste0(
      "<b>", str_to_title(community), "</b><br>",
      "📰 ", metric_label, ": ", round(display_value, 2), "<br>",
      "📊 Demographic: ",
      ifelse(is.null(input$demo_var) || input$demo_var == "None", "None",
             ifelse(input$demo_var %in% names(map_data),
                    format(replace_na(map_data[[input$demo_var]],0), big.mark = ","), "N/A"))
    ), htmltools::HTML),
    labelOptions = labelOptions(direction = "auto", textsize = "12px", sticky = TRUE)
  )
})
}