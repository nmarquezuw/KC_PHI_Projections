rm(list = ls())
library(tidyverse)
library(sf)
library(raster)
library(rgdal)
library(geojsonio)
library(raster)
source("./00_utils.R")

hp_proj <- read.csv(
    file = "./data/tract_race_projections.csv",
    colClasses=c("GEOID" = "character")
)

hp_proj <- hp_proj %>%
    filter(
        Year %in% seq(2000,2045,5)
    ) %>%
    select(-Type)

write.csv(hp_proj, file = "./data/tract_age5_race_sex_proj_2020_2045.csv", row.names = FALSE)

download_kc_public_clinics_spdf <- function() {
    # Public Health Clinics data source from King County GIS Open Data API
    # https://gis-kingcounty.opendata.arcgis.com/datasets/public-health-clinics-ph-clinics-point?geometry=-124.257%2C47.186%2C-120.241%2C47.835
    tf <- tempfile(fileext = ".json")
    temp <- readLines("https://gisdata.kingcounty.gov/arcgis/rest/services/OpenDataPortal/pubsafe___base/MapServer/178/query?where=1%3D1&outFields=NAME,ADDRESS,ZIPCODE&outSR=4326&f=json")
    write(temp, file = tf)
    readOGR(tf)
}

# kc_public_clinics <- download_kc_public_clinics_spdf()
# 
# writeOGR(kc_public_clinics, dsn = "./data/kc_public_clinics.json", layer = "kc_public_clinics", driver="GeoJSON")


download_kc_schools_spdf <- function() {
    # School Sites in King County / schsite point data source from King County GIS Open Data API
    # https://gis-kingcounty.opendata.arcgis.com/datasets/school-sites-in-king-county-schsite-point?geometry=-123.799%2C47.157%2C-120.017%2C47.807
    tf <- tempfile(fileext = ".json")
    temp <- readLines("https://gisdata.kingcounty.gov/arcgis/rest/services/OpenDataPortal/admin___base/MapServer/107/query?where=1%3D1&outFields=CODE,NAME,ADDRESS,ZIPCODE,DISTRICT&outSR=4326&f=json")
    write(temp, file = tf)
    spdf <- readOGR(tf)
    spdf@data <- spdf@data %>%
        mutate(
            CODE = case_when(
                CODE==660 ~ "School - Elementary",
                CODE==661 ~ "School - Junior High or Middle",
                CODE==662 ~ "School - High",
                CODE==663 ~ "School - College or University",
                CODE==664 ~ "School - Alternative",
                CODE==665 ~ "School - Other facility",
                CODE==666 ~ "School - K thru 12"
            )
        )
    spdf
}

kc_schools <- download_kc_schools_spdf()

writeOGR(kc_schools, dsn = "./data/kc_schools.json", layer = "kc_schools", driver="GeoJSON")




library(tibble)
library(DT)
library(dplyr)
library(tidygeocoder)

#----------WIC-------
wic <- read.csv(
    "./data/wic.csv",
    header = FALSE
)

colnames(wic) <- c("Name", "Address")

wic <- wic %>%
    geocode(
        address = Address,
        method = "osm"
    )

wic_spdf <- SpatialPointsDataFrame(
    coords = wic[,c("long", "lat")],
    data = wic[,c("Name", "Address")],
    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
)

writeOGR(wic_spdf, dsn = "./data/kc_wic.json", layer = "wic_spdf", driver="GeoJSON")

#----------CHC-------
chc <- read.csv(
    "./data/kc_chc.csv",
    header = FALSE
)

colnames(chc) <- c("Name", "Address")

chc <- chc %>%
    geocode(
        address = Address,
        method = "osm"
    )

chc_spdf <- SpatialPointsDataFrame(
    coords = chc[,c("long", "lat")],
    data = chc[,c("Name", "Address")],
    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
)

writeOGR(chc_spdf, dsn = "./data/kc_chc.json", layer = "chc_spdf", driver="GeoJSON")


kc_hra <- shapefile("./data/hra_2010block/HRA_2010Block_Clip.shp") 

colnames(kc_hra@data)[2] = "GEOID"

kc_hra@data <- kc_hra@data %>%
    dplyr::select(GEOID)

writeOGR(kc_hra, dsn = "./data/kc_hra.json", layer = "kc_hra", driver="GeoJSON")


hra_proj <- read.csv(
    file = "./data/hra_race_projections.csv",
    colClasses=c(
        "Age5" = "factor",
        "value" = "numeric"
    )
)

colnames(hra_proj)[2] <- "GEOID"

write.csv(hra_proj, file = "./data/HRA_age5_race_sex_proj_2000_2045.csv", row.names = FALSE)

