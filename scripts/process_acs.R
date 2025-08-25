# Load packages
library(tidycensus)
library(sf)
library(tidyverse)

census_api_key("65c37b4e94ff65d638945ad14e7f68c181247320")  

# Load current dataset
census_geo <- readRDS("data/census_geo_updated.rds")

pop_data <- get_acs(
  geography = "tract",
  variables = "B01003_001E", 
  state = "IL",
  county = "Cook",
  year = 2021,
  geometry = FALSE
)

pop_data <- pop_data %>%
  select(GEOID, Population = estimate)

# Merge with census_geo and calculate population density
census_geo <- census_geo %>%
  mutate(Area_km2 = as.numeric(st_area(geometry) / 1e6)) %>%  # Area in km²
  left_join(pop_data, by = "GEOID") %>%
  mutate(Pop_Density = Population / Area_km2)

census_geo$Population[is.na(census_geo$Population)] <- median(census_geo$Population, na.rm = TRUE)
census_geo$Pop_Density[is.na(census_geo$Pop_Density)] <- median(census_geo$Pop_Density, na.rm = TRUE)

saveRDS(census_geo, "data/census_geo_updated.rds")
cat("Saved updated dataset to data/census_geo_updated.rds\n")

cat("Summary of Population:\n")
print(summary(census_geo$Population))
cat("Summary of Pop_Density:\n")
print(summary(census_geo$Pop_Density))