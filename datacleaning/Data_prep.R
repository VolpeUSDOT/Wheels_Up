# Read in ASQP data for 2015-2017
# Fix the comma issue in ORIGIN_CITY_NAME and DEST_CITY_NAME, which causes typical comma-separated value parsers to fail

library(tidyverse)

codeloc = ifelse(grep('Flynn', normalizePath('~/')),
                 "~/Documents/git/Wheels_Up",
                 "Erika_put_your_path_here/Wheels_Up")
sharedloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Data"

avail_data = dir(file.path(sharedloc, 'Fixlines'))

# Fix comma issue by skipping header row, then add state to follow in ORIGIN_CITY_NAME and DEST_CITY_NAME
d_15 <- readr::read_csv(file.path(sharedloc, avail_data[grep('2015', avail_data)]),
                        col_names = F, skip = 1)

head_15 <- readr::read_csv(file.path(sharedloc, avail_data[grep('2015', avail_data)]),
                           col_names = F, n_max = 1)

head_15 <- as.character(head_15[1,])
paste(head_15, collapse = "', '")

# Manually add ORIGIN_STATE_NAME_1 and DEST_STATE_NAME_1
names(d_15) = c('ID', 'SRC_FILE', 'YEAR', 'QUARTER', 'MONTH', 'DAY_OF_MONTH', 'DAY_OF_WEEK', 'FLIGHT_DATE', 'UNIQUE_CARRIER', 'AIRLINE_ID', 'CARRIER', 'TAIL_NUM', 'FLIGHT_NUM', 'ORIGIN', 'ORIGIN_CITY_NAME', 
                'ORIGIN_STATE_NAME_1',
                'ORIGIN_STATE', 'ORIGIN_STATE_FIPS', 'ORIGIN_STATE_NAME', 'ORIGIN_WAC', 'DEST', 'DEST_CITY_NAME',
                'DEST_STATE_NAME_1',
                'DEST_STATE', 'DEST_STATE_FIPS', 'DEST_STATE_NAME', 'DEST_WAC', 'CRS_DEP_TIME_HR', 'CRS_DEP_TIME_MIN', 'DEP_TIME_HR', 'DEP_TIME_MIN', 'DEP_DELAY', 'DEP_DELAY_MINS', 'DEP_DELAY_15', 'DEP_DELAY_GRPS', 'DEP_TIME_BLK', 'TAXI_OUT', 'WHEELS_OFF', 'WHEELS_ON', 'TAXI_IN', 'CRS_ARR_TIME_HR', 'CRS_ARR_TIME_MIN', 'ARR_TIME_HR', 'ARR_TIME_MIN', 'ARR_DELAY', 'ARR_DELAY_MINS', 'ARR_DELAY_15', 'ARR_DELAY_GRPS', 'ARR_TIME_BLK', 'CANCELLED', 'CANCELLATION_CODE', 'DIVERTED', 'CRS_ELAPSED_TIME', 'ACTUAL_ELAPSED_TIME', 'AIR_TIME', 'FLIGHTS', 'DISTANCE', 'DISTANCE_GRP', 'CARRIER_DELAY', 'WEATHER_DELAY', 'NAS_DELAY', 'SECURITY_DELAY', 'LATE_AIRCRAFT_DELAY')

# Need time zones to correctly format departure and arrival times. 
# Will then need to correct arrival date for overnight flights. Will have to check for arrival time < departure time and increment the date by one day.  

# Columns to drop
to_drop = c('ORIGIN_STATE_NAME_1', 'DEST_STATE_NAME_1', 'TAIL_NUM', 'UNIQUE_CARRIER', 'SRC_FILE', 'ID',
            'ORIGIN_STATE_NAME', 'DEST_STATE_NAME')

d_15 = d_15[!names(d_15) %in% to_drop] %>%
  mutate(DAY_OF_WEEK = as.factor(DAY_OF_WEEK),
         FLIGHT_DATE = as.Date(FLIGHT_DATE),
         AIRLINE_ID = as.factor(AIRLINE_ID),
         CARRIIER = as.factor(CARRIER),
         FLIGHT_NUM = as.factor(FLIGHT_NUM),
         ACTUAL_ELAPSED_TIME = as.numeric(ACTUAL_ELAPSED_TIME),
         AIR_TIME = as.numeric(AIR_TIME),
         ORIGIN = as.factor(ORIGIN),
         ORIGIN_CITY_NAME = as.factor(ORIGIN_CITY_NAME),
         ORIGIN_STATE = as.factor(ORIGIN_STATE),         
         ORIGIN_STATE_FIPS = as.factor(ORIGIN_STATE_FIPS),         
         ORIGIN_WAC = as.factor(ORIGIN_WAC),         
         DEST = as.factor(DEST),
         DEST_CITY_NAME = as.factor(DEST_CITY_NAME),
         DEST_STATE = as.factor(DEST_STATE),         
         DEST_STATE_FIPS = as.factor(DEST_STATE_FIPS),         
         DEST_WAC = as.factor(DEST_WAC),          
         CANCELLATION_CODE = as.factor(CANCELLATION_CODE)
         # Dep_time = as.POSIXct(paste(FLIGHT_DATE, CRS_DEP_TIME_HR, CRS_DEP_TIME_MIN), 
         #                     format = "%Y-%m-%d %H %M", tz = ...),
         # Arr_time = as.POSIXct(paste(FLIGHT_DATE, CRS_ARR_TIME_HR, CRS_ARR_TIME_MIN), 
         #                     format = "%Y-%m-%d %H %M", tz = ....)
  )

save(list = 'd_15', file = file.path(sharedloc, 'ASQP_2015.RData'))
