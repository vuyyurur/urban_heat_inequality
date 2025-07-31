library(sf)
# Load Illinois tracts
il_tracts <- st_read("data/tl_2020_17_tract.shp")
# Filter for Cook County (FIPS: 031)
chicago_tracts <- il_tracts[il_tracts$COUNTYFP == "031", ]
# Save
st_write(chicago_tracts, "data/chicago_tracts.shp")