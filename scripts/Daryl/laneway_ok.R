
library(sf)

# Download to a temporary file
tmp_geojson1 <- tempfile(fileext = ".geojson")
download.file(
  "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/property-parcel-polygons/exports/geojson",
  tmp_geojson1
)
tmp_geojson2 <- tempfile(fileext = ".geojson")
download.file(
  "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/lanes/exports/geojson",
  tmp_geojson2
)

parcel <- read_sf(tmp_geojson1) %>%
  st_transform(crs=32610)
lanes <- read_sf(tmp_geojson2) %>%
  st_transform(crs=32610)

lanes_buff <- st_buffer(lanes, 4)

parcel$laneok1 <- ifelse(st_intersects(parcel, lanes_buff) %>% lengths>0, 1, 0)
