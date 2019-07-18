# Read in ASQP data for 2015-2018, Plus available months from 2019
# Fixes the comma issue in ORIGIN_CITY_NAME and DEST_CITY_NAME, which causes typical comma-separated value parsers to fail
# Saves results on shared drive as .RData files

# Setup ----
library(tidyverse)

# devtools::install_github("gadenbuie/regexplain")
codeloc = ifelse(grepl('Flynn', normalizePath('~/')),
                 "~/git/Wheels_Up",
                 "~/GitHub/Wheels_Up")
sharedloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Data"
# sharedloc = 'C:/Users/Daniel.Flynn/Desktop'

avail_data = dir(sharedloc)[grep('^ASQP.\\d{4}.csv$', dir(sharedloc))] # Match with regular expresion to only get csv files

new_data = dir(sharedloc)[grep('On_Time_Reporting_Carrier_On_Time_Performance_\\(\\w*\\)_\\d{4}_\\d{1,2}', dir(sharedloc))] # Newly available data, Jan-April 2019.

# Function ----

# Fix comma issue by skipping header row, then add state to follow in ORIGIN_CITY_NAME and DEST_CITY_NAME

read_ASQP <- function(datafile){
  
  data_file <- readr::read_csv(datafile,
                          col_names = F, skip = 1)
  
  head_file <- readr::read_csv(datafile,
                             col_names = F, n_max = 1)

  head_file <- as.character(head_file[1,])
  
  if(length(head_file) < ncol(data_file)){
    # paste(head_file, collapse = "', '")
    
    # Manually add ORIGIN_STATE_NAME_1 and DEST_STATE_NAME_1
    names(data_file) = c('ID', 'SRC_FILE', 'YEAR', 'QUARTER', 'MONTH', 'DAY_OF_MONTH', 'DAY_OF_WEEK', 'FLIGHT_DATE', 'UNIQUE_CARRIER', 'AIRLINE_ID', 'CARRIER', 'TAIL_NUM', 'FLIGHT_NUM', 'ORIGIN', 'ORIGIN_CITY_NAME', 
                    'ORIGIN_STATE_NAME_1',
                    'ORIGIN_STATE', 'ORIGIN_STATE_FIPS', 'ORIGIN_STATE_NAME', 'ORIGIN_WAC', 'DEST', 'DEST_CITY_NAME',
                    'DEST_STATE_NAME_1',
                    'DEST_STATE', 'DEST_STATE_FIPS', 'DEST_STATE_NAME', 'DEST_WAC', 'CRS_DEP_TIME_HR', 'CRS_DEP_TIME_MIN', 'DEP_TIME_HR', 'DEP_TIME_MIN', 'DEP_DELAY', 'DEP_DELAY_MINS', 'DEP_DELAY_15', 'DEP_DELAY_GRPS', 'DEP_TIME_BLK', 'TAXI_OUT', 'WHEELS_OFF', 'WHEELS_ON', 'TAXI_IN', 'CRS_ARR_TIME_HR', 'CRS_ARR_TIME_MIN', 'ARR_TIME_HR', 'ARR_TIME_MIN', 'ARR_DELAY', 'ARR_DELAY_MINS', 'ARR_DELAY_15', 'ARR_DELAY_GRPS', 'ARR_TIME_BLK', 'CANCELLED', 'CANCELLATION_CODE', 'DIVERTED', 'CRS_ELAPSED_TIME', 'ACTUAL_ELAPSED_TIME', 'AIR_TIME', 'FLIGHTS', 'DISTANCE', 'DISTANCE_GRP', 'CARRIER_DELAY', 'WEATHER_DELAY', 'NAS_DELAY', 'SECURITY_DELAY', 'LATE_AIRCRAFT_DELAY')
  } else {
    names(data_file) = head_file
  }
  # Need time zones to correctly format departure and arrival times. 
  # Will then need to correct arrival date for overnight flights. Will have to check for arrival time < departure time and increment the date by one day.  
  
  # Columns to drop
  to_drop = c('ORIGIN_STATE_NAME_1', 'DEST_STATE_NAME_1',
              'ORIGIN_WAC', 'ORIGIN_STATE', 'ORIGIN_STATE_FIPS',
              'TAIL_NUM', 'UNIQUE_CARRIER', 'SRC_FILE', 'ID', 'FLIGHTS',
              'QUARTER', 'DAY_OF_MONTH',
              'ORIGIN_STATE_NAME', 'DEST_STATE_NAME',
              'DEST_WAC', 'DEST_STATE', 'DEST_STATE_FIPS')
  
  # Drop columns and format
  data_file = data_file[!names(data_file) %in% to_drop] %>%
    mutate(DAY_OF_WEEK = as.factor(DAY_OF_WEEK),
           FLIGHT_DATE = as.Date(FLIGHT_DATE),
           AIRLINE_ID = as.factor(AIRLINE_ID),
           CARRIER = as.factor(CARRIER),
           FLIGHT_NUM = as.factor(FLIGHT_NUM),
           ACTUAL_ELAPSED_TIME = as.numeric(ACTUAL_ELAPSED_TIME),
           AIR_TIME = as.numeric(AIR_TIME),
           ORIGIN = as.factor(ORIGIN),
           ORIGIN_CITY_NAME = as.factor(ORIGIN_CITY_NAME),
           DEST = as.factor(DEST),
           DEST_CITY_NAME = as.factor(DEST_CITY_NAME),
           CANCELLATION_CODE = as.factor(CANCELLATION_CODE)
           # Dep_time = as.POSIXct(paste(FLIGHT_DATE, CRS_DEP_TIME_HR, CRS_DEP_TIME_MIN), 
           #                     format = "%Y-%m-%d %H %M", tz = ...),
           # Arr_time = as.POSIXct(paste(FLIGHT_DATE, CRS_ARR_TIME_HR, CRS_ARR_TIME_MIN), 
           #                     format = "%Y-%m-%d %H %M", tz = ....)
    )
} # end read_ASQP function

# Apply to data ----
d_15 <- read_ASQP(file.path(sharedloc, avail_data[grep('2015', avail_data)]))
d_16 <- read_ASQP(file.path(sharedloc, avail_data[grep('2016', avail_data)]))
d_17 <- read_ASQP(file.path(sharedloc, avail_data[grep('2017', avail_data)]))
d_18 <- read_ASQP(file.path(sharedloc, avail_data[grep('2018', avail_data)]))

save(list = 'd_15', file = file.path(sharedloc, 'ASQP_2015.RData'))
save(list = 'd_16', file = file.path(sharedloc, 'ASQP_2016.RData'))
save(list = 'd_17', file = file.path(sharedloc, 'ASQP_2017.RData'))
save(list = 'd_18', file = file.path(sharedloc, 'ASQP_2018.RData'))

# Read new data, from 'Prezipped File' format via Transtats
keepvars = names(d_18)

d_19_1 <- read_csv(file.path(sharedloc, new_data[grep('2019_1', new_data)]))
d_19_2 <- read_csv(file.path(sharedloc, new_data[grep('2019_2', new_data)]))
d_19_3 <- read_csv(file.path(sharedloc, new_data[grep('2019_3', new_data)]))
d_19_4 <- read_csv(file.path(sharedloc, new_data[grep('2019_4', new_data)]))

# Change naming convention to match already prepared data: All caps, with underscore to separate names

nameconv <- function(oldnames){
  # gsub: Find single uppercase letters followed by lowercase letters, replace with the same thing preceded by an underscore 
  nn <- gsub('(.)([A-Z]{1}[a-z])', '\\1_\\2', oldnames)
  # convert to uppercase
  nn <- toupper(nn)
  # eliminate duplicate underscores
  nn <- sub('__', '_', nn)
  # Fix Day of week and day of month
  nn <- gsub('DAYOF', 'DAY_OF', nn)
  nn <- gsub('OFWEEK', 'OF_WEEK', nn)
  # Change Reporting airline to carrier to match 
  nn[nn == 'REPORTING_AIRLINE'] = 'CARRIER'
  nn
}
  
names(d_19_1) <- nameconv(names(d_19_1))
names(d_19_2) <- nameconv(names(d_19_2))
names(d_19_3) <- nameconv(names(d_19_3))
names(d_19_4) <- nameconv(names(d_19_4))

# Keep only which we used in in other prepped data. One difference: CRS, DEP and ARR times in other prepped data are split to _HR and _MIN, while new data keeps together

keepnames = c(names(d_18), 'CRS_DEP_TIME', 'CRS_ARR_TIME', 'DEP_TIME', 'ARR_TIME')

d_19_1 <- d_19_1 %>%
  select(names(d_19_1)[names(d_19_1) %in% keepnames])

d_19_2 <- d_19_2 %>%
  select(names(d_19_2)[names(d_19_2) %in% keepnames])

d_19_3 <- d_19_3 %>%
  select(names(d_19_3)[names(d_19_3) %in% keepnames])

d_19_4 <- d_19_4 %>%
  select(names(d_19_4)[names(d_19_4) %in% keepnames])

save(list = c('d_19_1', 'd_19_2','d_19_3', 'd_19_4'),
     file = file.path(sharedloc, 'ASQP_2019.RData'))


# Scan across years ----
# Extract carriers and airports, make tables showing which years they appear in and how frequently

carr_yr = d_15 %>%
  group_by(CARRIER) %>%
  summarize(count_15 = n())

carr_yr = full_join(carr_yr, d_16 %>%
                      group_by(CARRIER) %>%
                      summarize(count_16 = n())
                    )

carr_yr = full_join(carr_yr, d_17 %>%
                      group_by(CARRIER) %>%
                      summarize(count_17 = n())
                    )

carr_yr = full_join(carr_yr, d_18 %>%
                      group_by(CARRIER) %>%
                      summarize(count_18 = n())
)


od_yr = d_15 %>%
  group_by(ORIGIN, DEST) %>%
  summarize(count_15 = n())

od_yr = full_join(od_yr, d_16 %>%
                    group_by(ORIGIN, DEST) %>%
                    summarize(count_16 = n())
                  )

od_yr = full_join(od_yr, d_17 %>%
                      group_by(ORIGIN, DEST) %>%
                      summarize(count_17 = n())
                  )

od_yr = full_join(od_yr, d_18 %>%
                    group_by(ORIGIN, DEST) %>%
                    summarize(count_18 = n())
)

any_od_na = od_yr %>% filter(is.na(count_15) | is.na(count_16) | is.na(count_17))
any_od_na %>% filter(is.na(count_15))
any_od_na %>% filter(is.na(count_16))
any_od_na %>% filter(is.na(count_17))

all(unique(od_yr$DEST) %in% unique(od_yr$ORIGIN)) # not all airports present in both origin and destination
dest_not_in_origin = unique(od_yr$DEST)[!unique(od_yr$DEST) %in% unique(od_yr$ORIGIN)]  
origin_not_in_dest = unique(od_yr$ORIGIN)[!unique(od_yr$ORIGIN) %in% unique(od_yr$DEST)]

od_yr %>% filter(DEST %in% dest_not_in_origin)
od_yr %>% filter(ORIGIN %in% origin_not_in_dest)

save(list = c('carr_yr', 'od_yr'),
     file = file.path(sharedloc, 'Cross-year_Summary.RData'))


# Summary of April 2019 data for Recent Reportable Flights ----

d_19_4 <- d_19_4 %>% 
  mutate(OD = paste(Origin, Dest))

d_19_4 %>%
  summarize(nflights = format(length(Year), big.mark = ','),
            nOD = length(unique(OD)),
            nCarrier = length(unique(Reporting_Airline)),
            pct_Canc = sum(Cancelled == 1 | Diverted == 1)/length(Year),
            pct_6to9_dep = sum(DepTimeBlk == '0600-0659' |
                                 DepTimeBlk == '0700-0759' |
                                 DepTimeBlk == '0800-0859') / length(Year),
            pct_6to7_dep = sum(DepTimeBlk == '0600-0659') / length(Year)
            )

# nflights  nOD  nCarrier   pct_Canc pct_6to9_dep pct_6to7_dep
# 612,023   5550       17 0.02614771    0.2088974   0.07490568
