# Load packages
library(tidyverse)
library(sf)
library(raster)

# Load datasets
census <- read_csv("data/census_income.csv")
chicago_tracts <- st_read("data/chicago_tracts.shp")
modis_temp <- raster("data/modis_temp.tif")

# Check input data
print("Census rows:")
print(nrow(census))
print("Tract rows:")
print(nrow(chicago_tracts))
print("Sample Tract vs GEOID:")
print(head(census$Tract))
print(head(chicago_tracts$GEOID))

# Create GEOID in census
census <- census %>% mutate(GEOID = paste0(state, county, Tract))

# Verify GEOID match
print("Sample census GEOID:")
print(head(census$GEOID))

# Ensure CRS alignment
modis_crs <- crs(modis_temp)
chicago_tracts <- st_transform(chicago_tracts, modis_crs)

# Join
census_geo <- chicago_tracts %>% left_join(census, by = "GEOID")
print("Rows after join:")
print(nrow(census_geo))

# Aggregate raster to reduce memory usage
modis_temp_agg <- aggregate(modis_temp, fact = 2, fun = mean)  # 2x2 pixel aggregation
print("Aggregated raster extent:")
print(extent(modis_temp_agg))

# Extract mean temperature
census_geo$Mean_Temp <- extract(modis_temp_agg, census_geo, fun = mean, na.rm = TRUE)

# Clean data
census_geo <- census_geo %>% 
  filter(!is.na(Median_Income), Median_Income > 0, !is.na(Mean_Temp))

# Convert to Celsius
census_geo$Mean_Temp_C <- census_geo$Mean_Temp - 273.15

# Verify
print("Final rows:")
print(nrow(census_geo))
print(summary(census_geo$Median_Income))
print(summary(census_geo$Mean_Temp_C))

# Save
saveRDS(census_geo, "data/census_geo.rds")
print("Saved to data/census_geo.rds")