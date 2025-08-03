# Load packages
library(terra)
library(sf)
library(tidyverse)

# Load data
census_geo <- readRDS("data/census_geo.rds")
nlcd <- rast("data/nlcd_2019_tree_canopy_cover_conus.tif")  # Update path if different

# Transform census_geo to match NLCD CRS
census_geo <- st_transform(census_geo, crs(nlcd))

# Extract mean tree canopy cover per tract
tree_canopy <- terra::extract(nlcd, census_geo, fun = "mean", na.rm = TRUE)
census_geo$Tree_Canopy_Pct <- tree_canopy[, 2]  # Second column contains means

# Handle missing values (if any)
census_geo$Tree_Canopy_Pct[is.na(census_geo$Tree_Canopy_Pct)] <- mean(census_geo$Tree_Canopy_Pct, na.rm = TRUE)

# Save updated dataset
saveRDS(census_geo, "data/census_geo_updated.rds")
cat("Saved updated dataset to data/census_geo_updated.rds\n")

# Verify
cat("Summary of Tree_Canopy_Pct:\n")
print(summary(census_geo$Tree_Canopy_Pct))