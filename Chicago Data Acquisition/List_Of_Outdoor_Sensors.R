library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(sf)
library(lubridate)

sensorids <- read_xlsx("sensor_ids.xlsx")

## now try to get it for full list
sensor_ids <- as.vector(sensorids$sensor_id)

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


# filter to only outdoor sensors
outdoor_sensors <- sensor_info %>%
  filter(location_type == 0)

# now filter to sensors that were active for all of 2022
outdoor_2022_sensors <- outdoor_sensors %>%
  filter(date_created < '2022-01-01 00:00:00')

# get list of outdoor sensors that were active 2022
vec <- as.data.frame(outdoor_2022_sensors$id)

#124737 removed from november
#124679 removed from november
#77067 removed from november
