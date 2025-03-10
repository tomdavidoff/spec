# getLanewaySpatial.R
# R to do spatial analysis on RS-1 lots
# Tom Davidoff
# March 8, 2025

library(sf)
library(data.table)

dP <- read_sf("data/raw/property-parcel-polygons.geojson")
dS <- read_sf("data/raw/lanes.geojson")
dSbuff <- st_buffer(dS, 4)
dP$laneok <- st_intersects(dP,dSbuff)
df <- data.table(cbind(laneok=1*as.vector(lengths(dP$laneok)>0),streetNumber=as.vector(dP$civic_number),streetName=as.vector(dP$streetname)))
fwrite(df,"data/derived/lanewaySpatial.csv")