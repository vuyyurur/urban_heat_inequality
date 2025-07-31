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

# print("EDA complete. Plots saved in plots/")

# Load packages
library(tidyverse)
library(sf)

# Load preprocessed data
census_geo <- readRDS("data/census_geo.rds")
print("Loaded census_geo:")
print(head(census_geo))

# Correlation test
cor_test <- cor.test(census_geo$Mean_Temp_C, census_geo$Median_Income)
print("Correlation test:")
print(cor_test)

# Linear regression
lm_summary <- summary(lm(Mean_Temp_C ~ Median_Income, data = census_geo))
print("Linear regression summary:")
print(lm_summary)

# Set graphics device for macOS stability
if (Sys.info()["sysname"] == "Darwin") {
  quartz(width = 8, height = 6)
}

# Scatterplot: Temperature vs. Income
scatter_plot <- ggplot(census_geo, aes(x = Median_Income, y = Mean_Temp_C)) +
  geom_point(alpha = 0.5, color = "lightblue") +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Temperature vs. Median Income in Chicago",
       x = "Median Household Income ($)",
       y = "Mean Temperature (°C)") +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
print("Generating scatterplot...")
print(scatter_plot)
ggsave("plots/scatter_temp_income.png", plot = scatter_plot, width = 8, height = 6)

# Heat Map: Temperature distribution
heat_map <- ggplot(census_geo) +
  geom_sf(aes(fill = Mean_Temp_C), color = NA) +
  scale_fill_gradient(low = "blue", high = "red", name = "Temperature (°C)") +
  labs(title = "Chicago Heat Map (July 1, 2020)") +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
print("Generating heat map...")
print(heat_map)
ggsave("plots/heat_map.png", plot = heat_map, width = 8, height = 8)

# Income Map: Income distribution
income_map <- ggplot(census_geo) +
  geom_sf(aes(fill = Median_Income), color = NA) +
  scale_fill_gradient(low = "green", high = "yellow", name = "Income ($)") +
  labs(title = "Chicago Median Income Map") +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
print("Generating income map...")
print(income_map)
ggsave("plots/income_map.png", plot = income_map, width = 8, height = 8)

# Histogram: Median Income
income_hist <- ggplot(census_geo, aes(x = Median_Income)) +
  geom_histogram(fill = "green", bins = 30) +
  labs(title = "Distribution of Median Household Income", x = "Income ($)", y = "Count") +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
print("Generating income histogram...")
print(income_hist)
ggsave("plots/income_histogram.png", plot = income_hist, width = 8, height = 6)

# Histogram: Temperature
temp_hist <- ggplot(census_geo, aes(x = Mean_Temp_C)) +
  geom_histogram(fill = "red", bins = 30) +
  labs(title = "Distribution of Mean Temperature (July 1, 2020)", x = "Temperature (°C)", y = "Count") +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
print("Generating temperature histogram...")
print(temp_hist)
ggsave("plots/temp_histogram.png", plot = temp_hist, width = 8, height = 6)

print("EDA complete. Plots saved in plots/")