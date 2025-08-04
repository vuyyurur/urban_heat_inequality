# # Load packages
# library(tidyverse)
# library(sf)

# # Load preprocessed data
# census_geo <- readRDS("data/census_geo.rds")
# print("Loaded census_geo:")
# print(head(census_geo))

# # Correlation test
# cor_test <- cor.test(census_geo$Mean_Temp_C, census_geo$Median_Income)
# print("Correlation test:")
# print(cor_test)

# # Linear regression
# lm_summary <- summary(lm(Mean_Temp_C ~ Median_Income, data = census_geo))
# print("Linear regression summary:")
# print(lm_summary)

# # Set graphics device for macOS stability
# if (Sys.info()["sysname"] == "Darwin") {
#   quartz(width = 8, height = 6)
# }

# # Scatterplot: Temperature vs. Income
# scatter_plot <- ggplot(census_geo, aes(x = Median_Income, y = Mean_Temp_C)) +
#   geom_point(alpha = 0.5, color = "lightblue") +
#   geom_smooth(method = "lm", color = "blue") +
#   labs(title = "Temperature vs. Median Income in Chicago",
#        x = "Median Household Income ($)",
#        y = "Mean Temperature (°C)") +
#   theme_dark() +
#   theme(
#     plot.title = element_text(color = "white"),
#     axis.title = element_text(color = "white"),
#     axis.text = element_text(color = "white"),
#     plot.background = element_rect(fill = "black"),
#     panel.background = element_rect(fill = "black")
#   )
# print("Generating scatterplot...")
# print(scatter_plot)
# ggsave("plots/scatter_temp_income.png", plot = scatter_plot, width = 8, height = 6)

# # Heat Map: Temperature distribution
# heat_map <- ggplot(census_geo) +
#   geom_sf(aes(fill = Mean_Temp_C), color = NA) +
#   scale_fill_gradient(low = "blue", high = "red", name = "Temperature (°C)") +
#   labs(title = "Chicago Heat Map (July 1, 2020)") +
#   theme_dark() +
#   theme(
#     plot.title = element_text(color = "white"),
#     axis.title = element_text(color = "white"),
#     axis.text = element_text(color = "white"),
#     legend.text = element_text(color = "white"),
#     legend.title = element_text(color = "white"),
#     legend.background = element_rect(fill = "black"),
#     plot.background = element_rect(fill = "black"),
#     panel.background = element_rect(fill = "black")
#   )
# print("Generating heat map...")
# print(heat_map)
# ggsave("plots/heat_map.png", plot = heat_map, width = 8, height = 8)

# # Income Map: Income distribution
# income_map <- ggplot(census_geo) +
#   geom_sf(aes(fill = Median_Income), color = NA) +
#   scale_fill_gradient(low = "green", high = "yellow", name = "Income ($)") +
#   labs(title = "Chicago Median Income Map") +
#   theme_dark() +
#   theme(
#     plot.title = element_text(color = "white"),
#     axis.title = element_text(color = "white"),
#     axis.text = element_text(color = "white"),
#     legend.text = element_text(color = "white"),
#     legend.title = element_text(color = "white"),
#     legend.background = element_rect(fill = "black"),
#     plot.background = element_rect(fill = "black"),
#     panel.background = element_rect(fill = "black")
#   )
# print("Generating income map...")
# print(income_map)
# ggsave("plots/income_map.png", plot = income_map, width = 8, height = 8)

# # Histogram: Median Income
# income_hist <- ggplot(census_geo, aes(x = Median_Income)) +
#   geom_histogram(fill = "green", bins = 30) +
#   labs(title = "Distribution of Median Household Income", x = "Income ($)", y = "Count") +
#   theme_dark() +
#   theme(
#     plot.title = element_text(color = "white"),
#     axis.title = element_text(color = "white"),
#     axis.text = element_text(color = "white"),
#     plot.background = element_rect(fill = "black"),
#     panel.background = element_rect(fill = "black")
#   )
# print("Generating income histogram...")
# print(income_hist)
# ggsave("plots/income_histogram.png", plot = income_hist, width = 8, height = 6)

# # Histogram: Temperature
# temp_hist <- ggplot(census_geo, aes(x = Mean_Temp_C)) +
#   geom_histogram(fill = "red", bins = 30) +
#   labs(title = "Distribution of Mean Temperature (July 1, 2020)", x = "Temperature (°C)", y = "Count") +
#   theme_dark() +
#   theme(
#     plot.title = element_text(color = "white"),
#     axis.title = element_text(color = "white"),
#     axis.text = element_text(color = "white"),
#     plot.background = element_rect(fill = "black"),
#     panel.background = element_rect(fill = "black")
#   )
# print("Generating temperature histogram...")
# print(temp_hist)
# ggsave("plots/temp_histogram.png", plot = temp_hist, width = 8, height = 6)

# print("EDA complete. Plots saved in plots/")

# Load packages
library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)

# Load data
census_geo <- readRDS("data/census_geo_updated.rds")
cat("Loaded census_geo:\n")
print(head(census_geo))

# Summary statistics
cat("Summary statistics:\n")
summary_stats <- census_geo %>%
  st_drop_geometry() %>%
  select(Mean_Temp_C, Median_Income, Tree_Canopy_Pct, Pop_Density, Area_km2) %>%
  summary()
print(summary_stats)

# Correlation matrix
cat("Correlation matrix:\n")
cor_matrix <- census_geo %>%
  st_drop_geometry() %>%
  select(Mean_Temp_C, Median_Income, Tree_Canopy_Pct, Pop_Density, Area_km2) %>%
  cor(use = "complete.obs")
print(cor_matrix)

# Scatterplot: Mean_Temp_C vs. features
scatter_temp_income <- ggplot(census_geo, aes(x = Median_Income, y = Mean_Temp_C)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Temperature vs. Median Income",
    x = "Median Income ($)",
    y = "Mean Temperature (°C)"
  ) +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
scatter_temp_canopy <- ggplot(census_geo, aes(x = Tree_Canopy_Pct, y = Mean_Temp_C)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Temperature vs. Tree Canopy Cover",
    x = "Tree Canopy Cover (%)",
    y = "Mean Temperature (°C)"
  ) +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
scatter_temp_density <- ggplot(census_geo, aes(x = Pop_Density, y = Mean_Temp_C)) +
  geom_point(color = "purple", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Temperature vs. Population Density",
    x = "Population Density (people/km²)",
    y = "Mean Temperature (°C)"
  ) +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
cat("Generating scatterplots...\n")
png("plots/scatter_temp_income.png", width = 6, height = 4, units = "in", res = 300)
print(scatter_temp_income)
dev.off()
png("plots/scatter_temp_canopy.png", width = 6, height = 4, units = "in", res = 300)
print(scatter_temp_canopy)
dev.off()
png("plots/scatter_temp_density.png", width = 6, height = 4, units = "in", res = 300)
print(scatter_temp_density)
dev.off()

# Histograms
hist_temp <- ggplot(census_geo, aes(x = Mean_Temp_C)) +
  geom_histogram(fill = "blue", bins = 30) +
  labs(
    title = "Distribution of Mean Temperature",
    x = "Mean Temperature (°C)",
    y = "Count"
  ) +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
hist_income <- ggplot(census_geo, aes(x = Median_Income)) +
  geom_histogram(fill = "red", bins = 30) +
  labs(
    title = "Distribution of Median Income",
    x = "Median Income ($)",
    y = "Count"
  ) +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
hist_canopy <- ggplot(census_geo, aes(x = Tree_Canopy_Pct)) +
  geom_histogram(fill = "green", bins = 30) +
  labs(
    title = "Distribution of Tree Canopy Cover",
    x = "Tree Canopy Cover (%)",
    y = "Count"
  ) +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
hist_density <- ggplot(census_geo, aes(x = Pop_Density)) +
  geom_histogram(fill = "purple", bins = 30) +
  labs(
    title = "Distribution of Population Density",
    x = "Population Density (people/km²)",
    y = "Count"
  ) +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
cat("Generating histograms...\n")
png("plots/hist_temp.png", width = 6, height = 4, units = "in", res = 300)
print(hist_temp)
dev.off()
png("plots/hist_income.png", width = 6, height = 4, units = "in", res = 300)
print(hist_income)
dev.off()
png("plots/hist_canopy.png", width = 6, height = 4, units = "in", res = 300)
print(hist_canopy)
dev.off()
png("plots/hist_density.png", width = 6, height = 4, units = "in", res = 300)
print(hist_density)
dev.off()

# Transform to long-lat (EPSG:4326) for leaflet
census_geo_ll <- st_transform(census_geo, crs = 4326)

# Maps
map_temp <- leaflet(census_geo_ll) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorNumeric("RdYlBu", Mean_Temp_C, reverse = TRUE)(Mean_Temp_C),
    fillOpacity = 0.7,
    weight = 1,
    color = "white",
    popup = ~paste("Tract:", NAME.x, "<br>Temperature (°C):", round(Mean_Temp_C, 2))
  ) %>%
  addLegend(pal = colorNumeric("RdYlBu", census_geo_ll$Mean_Temp_C, reverse = TRUE), values = census_geo_ll$Mean_Temp_C, title = "Temperature (°C)")
map_canopy <- leaflet(census_geo_ll) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorNumeric("Greens", Tree_Canopy_Pct)(Tree_Canopy_Pct),
    fillOpacity = 0.7,
    weight = 1,
    color = "white",
    popup = ~paste("Tract:", NAME.x, "<br>Tree Canopy (%):", round(Tree_Canopy_Pct, 2))
  ) %>%
  addLegend(pal = colorNumeric("Greens", census_geo_ll$Tree_Canopy_Pct), values = census_geo_ll$Tree_Canopy_Pct, title = "Tree Canopy (%)")
map_density <- leaflet(census_geo_ll) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorNumeric("Purples", Pop_Density)(Pop_Density),
    fillOpacity = 0.7,
    weight = 1,
    color = "white",
    popup = ~paste("Tract:", NAME.x, "<br>Pop Density (people/km²):", round(Pop_Density, 2))
  ) %>%
  addLegend(pal = colorNumeric("Purples", census_geo_ll$Pop_Density), values = census_geo_ll$Pop_Density, title = "Pop Density (people/km²)")
cat("Generating maps...\n")
htmlwidgets::saveWidget(map_temp, "plots/map_temp.html", selfcontained = FALSE)
htmlwidgets::saveWidget(map_canopy, "plots/map_canopy.html", selfcontained = FALSE)
htmlwidgets::saveWidget(map_density, "plots/map_density.html", selfcontained = FALSE)

cat("EDA complete. Plots saved to plots/\n")