library(terra)
library(sf)
library(tidyverse)

census_geo <- readRDS("data/census_geo.rds")
nlcd <- rast("data/nlcd_2019_tree_canopy_cover_conus.tif") 

census_geo <- st_transform(census_geo, crs(nlcd))

# Extract mean tree canopy cover per tract
tree_canopy <- terra::extract(nlcd, census_geo, fun = "mean", na.rm = TRUE)
census_geo$Tree_Canopy_Pct <- tree_canopy[, 2]  # Second column contains means

census_geo$Tree_Canopy_Pct[is.na(census_geo$Tree_Canopy_Pct)] <- mean(census_geo$Tree_Canopy_Pct, na.rm = TRUE)

saveRDS(census_geo, "data/census_geo_updated.rds")
cat("Saved updated dataset to data/census_geo_updated.rds\n")

cat("Summary of Tree_Canopy_Pct:\n")
print(summary(census_geo$Tree_Canopy_Pct))