library(sf)
il_tracts <- st_read("data/tl_2020_17_tract.shp")
chicago_tracts <- il_tracts[il_tracts$COUNTYFP == "031", ]
st_write(chicago_tracts, "data/chicago_tracts.shp")