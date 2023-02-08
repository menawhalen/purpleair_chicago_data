library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(sf)
library(lubridate)

## now try to get it for full list
sensor_ids <- c(168725, 124183, 41581, 120369)

## base url and my api key never change
base <- 'https://api.purpleair.com/v1/sensors/'
info_key <- '?api_key=4660F889-5645-11ED-B5AA-42010A800006'
sensor_info <- NULL

## for loop length of all the sensor numbers I collected
for (i in sensor_ids){
  API_URL <- paste0(base, i, info_key)
  raw_data <- GET(API_URL)
  sensor_info <- bind_rows(sensor_info, 
                           data.frame(
    id = fromJSON(rawToChar(raw_data$content), flatten = TRUE)$sensor$sensor_index,
    name = fromJSON(rawToChar(raw_data$content), flatten = TRUE)$sensor$name,
    location_type = fromJSON(rawToChar(raw_data$content), flatten = TRUE)$sensor$location_type,
    latitude = fromJSON(rawToChar(raw_data$content), flatten = TRUE)$sensor$latitude,
    longitude = fromJSON(rawToChar(raw_data$content), flatten = TRUE)$sensor$longitude,
    private = fromJSON(rawToChar(raw_data$content), flatten = TRUE)$sensor$private,
    date_created = fromJSON(rawToChar(raw_data$content), flatten = TRUE)$sensor$date_created,
    last_seen = fromJSON(rawToChar(raw_data$content), flatten = TRUE)$sensor$last_seen
  ))
}

# store info as date time
sensor_info$date_created <- as_datetime(sensor_info$date_created)
sensor_info$last_seen <- as_datetime(sensor_info$last_seen)


comm_ar <- ("<path to chicago comunity area shp file>") %>%
  st_read(quiet = TRUE)

sensor_info <- sensor_info %>% 
  filter(location_type == 0) %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = st_crs(comm_ar))

ggplot(comm_ar) +
  geom_sf() +
  geom_sf(data = sensor_info)




