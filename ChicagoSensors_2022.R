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
sensorids <- read_xlsx("working_sensor_ids.xlsx")
sensor_ids <- as.vector(sensorids$sensor_id)[2]

api_key <- '4660F889-5645-11ED-B5AA-42010A800006'
fields=c("pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b, pm2.5_cf_1, pm10.0_cf_1, humidity, temperature, pressure, rssi")


# create a vector of all dates, in unix time, in the interval that we are looking for, in this case all days of 2022
dates <- seq(1640995200, 1672531200, by=86400)

### Create start and end vectors for looping through time interval

# create start and end date vectors
start_vector <- dates[seq(1, length(dates), 5)]

# create end vector (this is 1 second less than every date in start vector except for 1st item)
end_vector <- c()
for (i in 2:length(start_vector)){
  end_vector[i] <- start_vector[i] - 1
}

# remove last item of start vector and 1st item of end vector
start_vec <- start_vector[-74]
end_vec <- end_vector[-1]

# check dates in data frame to see that everything is correct
#df<- data.frame(start_vec,end_vec)

###create loop for 5 (full) day intervals of 2022 and store data in data frame


# create empty dataframes for loops
r_for <- data.frame()
r_final <- data.frame()
r_chicago <- data.frame()

for (i in 1:length(start_vec)){
  
  start <- start_vec[i]
  end <- end_vec[i]
  
  # Set Time Stamp
  t_dif <- as.POSIXct(end, origin="1970-01-01", tz="UTC") - as.POSIXct(start, origin="1970-01-01", tz="UTC")
  
  ### takes start and end time to break up if it is over 2 days and separates it into 2 section days
  if (t_dif <= as.difftime(48, units = 'hours') ) {
    ##if less than two days make dates
    start_timestamps <- as.POSIXct(start, origin="1970-01-01", tz="UTC")
    end_timestamps   <- as.POSIXct(end, origin="1970-01-01", tz="UTC") 
  } else {
    ## if over 2 days create seq to start every 2 days
    start_timestamps <- seq(from=as.POSIXct(start, origin="1970-01-01", tz="UTC")
                            ,to=as.POSIXct(end, origin="1970-01-01", tz="UTC"),by="2 days")
    ## and sequence that ends 2 days (down to the second) after the start date sequence
    end_timestamps   <- seq(from=as.POSIXct(start, origin="1970-01-01", tz="UTC") + as.difftime(172799, units = 'secs')
                            ,to=as.POSIXct(end, origin="1970-01-01", tz="UTC"),by="2 days")
  }
  
  
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
  # store in dataframe
  r_chicago <- rbind(r_chicago, r_final)
  f_final <- data.frame()
}  


# how many unique IDs are showing up
unique(chicago_2022$sensor_id)
