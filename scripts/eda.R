# Load packages
library(tidyverse)
library(sf)

# Load preprocessed data
census_geo <- readRDS("data/census_geo.rds")

# Correlation test
cor_test <- cor.test(census_geo$Mean_Temp_C, census_geo$Median_Income)
print(cor_test)

# Scatterplot: Temperature vs. Income
ggplot(census_geo, aes(x = Median_Income, y = Mean_Temp_C)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Temperature vs. Median Income in Chicago",
       x = "Median Household Income ($)",
       y = "Mean Temperature (°C)") +
  theme_minimal()
ggsave("plots/scatter_temp_income.png", width = 8, height = 6)

# Map: Temperature distribution
ggplot(census_geo) +
  geom_sf(aes(fill = Mean_Temp_C), color = NA) +
  scale_fill_gradient(low = "blue", high = "red", name = "Temperature (°C)") +
  labs(title = "Chicago Heat Map (July 1, 2020)") +
  theme_minimal()
ggsave("plots/heat_map.png", width = 8, height = 8)

# Map: Income distribution
ggplot(census_geo) +
  geom_sf(aes(fill = Median_Income), color = NA) +
  scale_fill_gradient(low = "green", high = "yellow", name = "Income ($)") +
  labs(title = "Chicago Median Income Map") +
  theme_minimal()
ggsave("plots/income_map.png", width = 8, height = 8)