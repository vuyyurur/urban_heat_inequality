

# library(shiny)
# library(leaflet)
# library(tidyverse)
# library(sf)
# library(caret)

# # Set working directory
# setwd("~/Desktop/urban_heat_inequality")
# cat("Working directory:", getwd(), "\n")  # Debug

# # Load data and model with error handling
# tryCatch({
#   census_geo <- readRDS("data/census_geo_updated.rds") %>%
#     st_transform(crs = 4326)
#   cat("Loaded census_geo\n")
# }, error = function(e) {
#   stop("Failed to load census_geo_updated.rds: ", e$message)
# })

# tryCatch({
#   rf_model <- readRDS("models/rf_model.rds")
#   cat("Loaded rf_model\n")
# }, error = function(e) {
#   stop("Failed to load rf_model.rds: ", e$message)
# })

# # Prepare prediction data
# pred_data <- census_geo %>%
#   mutate(
#     Median_Income = scale(Median_Income),
#     Tree_Canopy_Pct = scale(Tree_Canopy_Pct),
#     Pop_Density = scale(Pop_Density),
#     Area_km2 = scale(Area_km2),
#     Centroid_X = scale(st_coordinates(st_centroid(geometry))[,1]),
#     Centroid_Y = scale(st_coordinates(st_centroid(geometry))[,2]),
#     Pred_Temp_C = predict(rf_model, .)
#   ) %>%
#   st_drop_geometry()

# # UI
# ui <- fluidPage(
#   titlePanel("Chicago Urban Heat & Inequality Dashboard"),
#   tags$style(HTML("
#     body { font-family: 'Roboto', sans-serif; background-color: #f5f5f5; }
#     .sidebar { background-color: #ffffff; border-radius: 10px; padding: 15px; }
#     .main-panel { background-color: #ffffff; border-radius: 10px; padding: 15px; }
#     h3 { color: #2c3e50; }
#   ")),
#   sidebarLayout(
#     sidebarPanel(
#       class = "sidebar",
#       selectInput("map_var", "Select Map Variable:",
#                   choices = c("Mean Temperature (°C)" = "Mean_Temp_C",
#                               "Tree Canopy (%)" = "Tree_Canopy_Pct",
#                               "Population Density (people/km²)" = "Pop_Density")),
#       h3("About"),
#       p("This app visualizes urban heat disparities across 1,323 Chicago census tracts, using Random Forest predictions (R² 0.798).")
#     ),
#     mainPanel(
#       class = "main-panel",
#       leafletOutput("map", height = 400),
#       plotOutput("pred_plot", height = 300)
#     )
#   )
# )

# # Server
# server <- function(input, output) {
#   output$map <- renderLeaflet({
#     var <- input$map_var
#     pal <- switch(var,
#                   "Mean_Temp_C" = colorNumeric("RdYlBu", domain = census_geo$Mean_Temp_C, reverse = TRUE),
#                   "Tree_Canopy_Pct" = colorNumeric("Greens", domain = census_geo$Tree_Canopy_Pct),
#                   "Pop_Density" = colorNumeric("Purples", domain = census_geo$Pop_Density))
    
#     labels <- sprintf(
#       "<strong>Tract: %s</strong><br/>%s: %.2f",
#       census_geo$GEOID,
#       switch(var, "Mean_Temp_C" = "Temperature (°C)", "Tree_Canopy_Pct" = "Tree Canopy (%)", "Pop_Density" = "Pop. Density (people/km²)"),
#       census_geo[[var]]
#     ) %>% lapply(htmltools::HTML)
    
#     leaflet(census_geo) %>%
#       addTiles() %>%
#       addPolygons(
#         fillColor = ~pal(census_geo[[var]]),
#         weight = 1,
#         opacity = 1,
#         color = "white",
#         fillOpacity = 0.7,
#         label = labels
#       ) %>%
#       addLegend(pal = pal, values = census_geo[[var]], title = var)
#   })
  
#   output$pred_plot <- renderPlot({
#     ggplot(pred_data, aes(x = Mean_Temp_C, y = Pred_Temp_C)) +
#       geom_point(color = "#e74c3c", alpha = 0.6) +
#       geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
#       labs(x = "Actual Temperature (°C)", y = "Predicted Temperature (°C)",
#            title = "Random Forest Predictions (R² = 0.798)") +
#       theme_minimal() +
#       theme(
#         text = element_text(family = "Roboto"),
#         plot.title = element_text(hjust = 0.5, color = "#2c3e50"),
#         panel.background = element_rect(fill = "#f5f5f5"),
#         plot.background = element_rect(fill = "#f5f5f5")
#       )
#   })
# }

# # Run app
# shinyApp(ui, server)



library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(caret)

# Set working directory
setwd("~/Desktop/urban_heat_inequality")
cat("Working directory:", getwd(), "\n")

# Load data and model
tryCatch({
  census_geo <- readRDS("data/census_geo_updated.rds") %>%
    st_transform(crs = 4326)
  cat("Loaded census_geo with", nrow(census_geo), "rows\n")
  cat("Columns:", names(census_geo), "\n")
}, error = function(e) {
  stop("Failed to load census_geo_updated.rds: ", e$message)
})

tryCatch({
  rf_model <- readRDS("models/rf_model.rds")
  cat("Loaded rf_model\n")
  cat("Model predictors:", names(rf_model$trainingData)[-1], "\n")
}, error = function(e) {
  stop("Failed to load rf_model.rds: ", e$message)
})

# Prepare prediction data
tryCatch({
  # Calculate centroids explicitly
  centroids <- st_centroid(census_geo$geometry) %>%
    st_coordinates() %>%
    as.data.frame()
  names(centroids) <- c("Centroid_X", "Centroid_Y")
  cat("Centroids created with", nrow(centroids), "rows\n")
  
  pred_data <- census_geo %>%
    mutate(
      Median_Income = scale(Median_Income),
      Tree_Canopy_Pct = scale(Tree_Canopy_Pct),
      Pop_Density = scale(Pop_Density),
      Area_km2 = scale(Area_km2),
      Centroid_X = scale(centroids$Centroid_X),
      Centroid_Y = scale(centroids$Centroid_Y)
    )
  cat("Columns before prediction:", names(pred_data), "\n")
  
  pred_data <- pred_data %>%
    mutate(Pred_Temp_C = predict(rf_model, .)) %>%
    st_drop_geometry()
  cat("Prediction data created with", nrow(pred_data), "rows\n")
}, error = function(e) {
  stop("Failed to create pred_data: ", e$message)
})

# UI
ui <- fluidPage(
  titlePanel("Chicago Urban Heat & Inequality Dashboard"),
  tags$style(HTML("
    body { font-family: 'Roboto', sans-serif; background-color: #f5f5f5; }
    .sidebar { background-color: #ffffff; border-radius: 10px; padding: 15px; }
    .main-panel { background-color: #ffffff; border-radius: 10px; padding: 15px; }
    h3 { color: #2c3e50; }
  ")),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      selectInput("map_var", "Select Map Variable:",
                  choices = c("Mean Temperature (°C)" = "Mean_Temp_C",
                              "Tree Canopy (%)" = "Tree_Canopy_Pct",
                              "Population Density (people/km²)" = "Pop_Density")),
      h3("About"),
      p("This app visualizes urban heat disparities across 1,323 Chicago census tracts, using Random Forest predictions (R² 0.798).")
    ),
    mainPanel(
      class = "main-panel",
      leafletOutput("map", height = 400),
      plotOutput("pred_plot", height = 300)
    )
  )
)

# Server
server <- function(input, output) {
  output$map <- renderLeaflet({
    var <- input$map_var
    pal <- switch(var,
                  "Mean_Temp_C" = colorNumeric("RdYlBu", domain = census_geo$Mean_Temp_C, reverse = TRUE),
                  "Tree_Canopy_Pct" = colorNumeric("Greens", domain = census_geo$Tree_Canopy_Pct),
                  "Pop_Density" = colorNumeric("Purples", domain = census_geo$Pop_Density))
    
    labels <- sprintf(
      "<strong>Tract: %s</strong><br/>%s: %.2f",
      census_geo$GEOID,
      switch(var, "Mean_Temp_C" = "Temperature (°C)", "Tree_Canopy_Pct" = "Tree Canopy (%)", "Pop_Density" = "Pop. Density (people/km²)"),
      census_geo[[var]]
    ) %>% lapply(htmltools::HTML)
    
    leaflet(census_geo) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(census_geo[[var]]),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        label = labels
      ) %>%
      addLegend(pal = pal, values = census_geo[[var]], title = var)
  })
  
  output$pred_plot <- renderPlot({
    ggplot(pred_data, aes(x = Mean_Temp_C, y = Pred_Temp_C)) +
      geom_point(color = "#e74c3c", alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
      labs(x = "Actual Temperature (°C)", y = "Predicted Temperature (°C)",
           title = "Random Forest Predictions (R² = 0.798)") +
      theme_minimal() +
      theme(
        text = element_text(family = "Roboto"),
        plot.title = element_text(hjust = 0.5, color = "#2c3e50"),
        panel.background = element_rect(fill = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5")
      )
  })
}

# Run app
shinyApp(ui, server)