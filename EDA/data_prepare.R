library(tidyverse)
library(raster)
library(rgdal)
library(geojsonio)

kc_cr_station_2040 <- shapefile("./data/Leibb/2040CommuterRailStations/2040_CR_station.shp")
kc_cr_station_2040@data <- kc_cr_station_2040@data %>%
  select(EditNotes)

colnames(kc_cr_station_2040@data) <- c("Station Name")

writeOGR(kc_cr_station_2040, dsn = "./data/kc_cr_station_2040", layer = "kc_cr_station_2040", driver = "GeoJSON")



kc_lr_station_2040 <- shapefile("./data/Leibb/2040LightRailStations/2040_LR_station.shp")
kc_lr_station_2040@data <- kc_lr_station_2040@data %>%
  select(EditNotes)

colnames(kc_lr_station_2040@data) <- c("Station Name")

writeOGR(kc_lr_station_2040, dsn = "./data/kc_lr_station_2040", layer = "kc_lr_station_2040", driver = "GeoJSON")




kc_tl_2040 <- shapefile("./data/Leibb/2040TransitLines/emme_tlines.shp")
kc_tl_2040@data <- kc_tl_2040@data %>%
  select(DESC)

colnames(kc_tl_2040@data) <- c("Transit Line Name")

writeOGR(kc_tl_2040, dsn = "./data/kc_tl_2040", layer = "kc_tl_2040", driver = "GeoJSON")