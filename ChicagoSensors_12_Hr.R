library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(httpcode)
library(tcltk)
library(readxl)

### old inputs
#' Argment 	Description
#' sensorIndex 	The sensor index found in the url (?select=sensor_index) of a selected sensor in the purpleair maps purpleair map.
#' apiReadKey 	PurpleAir API read key with access to historical data. See PurpleAir Community website for more information.
#' startTimeStamp 	The beginning date in the format "YYYY-MM-DD HH:mm:ss".
#' endTimeStamp 	The end date in the format "YYYY-MM-DD" HH:mm:ss.
#' average 	The desired average in minutes, one of the following: "0" (real-time), "10", "30", "60", "360" (6 hour), "1440" (1 day).
#' fields 	The "Fields" parameter specifies which 'sensor data fields' to include in the response.

# read in sensor ids and store as vector 
sensorids <- read_xlsx("sensor_ids.xlsx")
sensor_ids <- as.vector(sensorids$sensor_id)

api_key <- '4660F889-5645-11ED-B5AA-42010A800006'
fields=c("pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b")


start <- "2023-01-01 00:00:00"
end <- "2023-01-01 06:00:00"

# Set Time Stamp
t_dif <- as.POSIXct(end, tz="UTC") - as.POSIXct(start, tz="UTC")

### takes start and end time to break up if it is over 2 days and separates it into 2 section days
if (t_dif <= as.difftime(48, units = 'hours') ) {
  ##if less than two days make dates
  start_timestamps <- as.POSIXct(start, tz="UTC")
  end_timestamps   <- as.POSIXct(end, tz="UTC") 
} else {
  ## if over 2 days create seq to start every 2 days
  start_timestamps <- seq(from=as.POSIXct(start, tz="UTC")
                          ,to=as.POSIXct(end, tz="UTC"),by="2 days")
  ## and sequence that ends 2 days (down to the second) after the start date sequence
  end_timestamps   <- seq(from=as.POSIXct(start, tz="UTC") + as.difftime(172799, units = 'secs')
                          ,to=as.POSIXct(end, tz="UTC"),by="2 days")
}




# create empty dataframes for loops
r_for <- data.frame()
r_final <- data.frame()

# loop through each item in sensor vector
for (i in 1:length(sensor_ids)){
  r_dataframe <- data.frame()
  
  # this needs to updated with each item in sensor ids
  URLbase <- paste0('https://api.purpleair.com/v1/sensors/',sensor_ids[i], '/history') 
  
  for (j in 1:length(start_timestamps)) {
    # Set variables
    queryList = list(
      start_timestamp = as.character(as.integer(as.POSIXct(start_timestamps[j], tz="UTC"))),
      end_timestamp = as.character(as.integer(as.POSIXct(end_timestamps[j], tz="UTC"))),
      average = 0,
      fields = fields)
    
    # GET PurpleAir sensor history data
    r_temp <- httr::GET(
      URLbase,
      query = queryList,
      config = add_headers("X-API-Key" = api_key)
    )
    
    
    # Structurized data in form of R vectors and lists
    r_parsed <- fromJSON(content(r_temp, as="text"))
    
    # Data frame from JSON data
    r_dataframe <- as.data.frame(r_parsed$data)
    
    # assign column names
    names(r_dataframe) <- r_parsed$fields
    
    
    # Convert datetime format
    r_dataframe$time_stamp <- as.integer(r_dataframe$time_stamp)
    r_dataframe$time_stamp <- as.POSIXct(r_dataframe$time_stamp, origin="1970-01-01", tz="UTC")
    
    ## Order by date
    r_dataframe <- r_dataframe[order(r_dataframe$time_stamp),]
    
    r_for <- rbind(r_for, r_dataframe)
    
  }
  
  # Add sensor index
 if (nrow(r_for) != 0) {
  r_for$sensor_id   <- sensor_ids[i]
  }
  
  # Set final request data frame
  r_final <- rbind(r_final, r_for)
  r_for <- data.frame()
  
  
}

# how many unique IDs are showing up
unique(r_final$sensor_id)

# 15


