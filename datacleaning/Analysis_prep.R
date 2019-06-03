# Prep script for Air Time models
# Creates full cross year data set, as well as training/test data set

# Setup ----
# Check to see if working on EC2 instance or local
if(Sys.getenv('USER')=='rstudio'){
  codeloc = '~/Wheels_Up/Users/Daniel.Flynn/Documents/git/Wheels_Up'  
  sharedloc = "~/Data"
  resultsloc = "~/results"
} else { 
  
  codeloc = ifelse(grepl('Flynn', normalizePath('~/')),
                   "~/git/Wheels_Up",
                   "~/GitHub/Wheels_Up")
  sharedloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Data"
  resultsloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Results"
  
}

library(tidyverse) # if this fails, run install.packages('tidyverse')
# Analysis prep  ----

# subset to only flights with available air time, and create unique origin-destination pair variable

# Full year model, by carrier
# 12 carriers to assess across the four years -- in order of size   
use_carriers = c('AA', 'AS', 'B6', 'DL', 'EV', 'F9', 'HA', 'NK', 'OO', 'UA', 'VX', 'WN')

if(!file.exists(file.path(sharedloc, 'ASQP_2015-2018.RData'))){
  
  load(file.path(sharedloc, 'ASQP_2018.RData'))
  load(file.path(sharedloc, 'ASQP_2017.RData'))
  load(file.path(sharedloc, 'ASQP_2016.RData'))
  load(file.path(sharedloc, 'ASQP_2015.RData'))
  
  d_crossyear <- full_join(d_18, d_17)
  d_crossyear <- full_join(d_crossyear, d_16)
  d_crossyear <- full_join(d_crossyear, d_15)
  
  d_crossyear <- d_crossyear %>%
    filter(!is.na(AIR_TIME) & CARRIER %in% use_carriers) %>%
    mutate(O_D = paste(ORIGIN, DEST, sep = "-"),
           O_D_18 = ifelse(YEAR == '2018', paste(ORIGIN, DEST, sep = "-"), NA),
           YEAR = as.factor(YEAR)) %>%
    filter(O_D %in% O_D_18) %>%
    select(YEAR, MONTH, DAY_OF_WEEK, CARRIER, ORIGIN, DEST, O_D,
           DEP_TIME_HR, DEP_TIME_MIN, DEP_TIME_BLK,
           ARR_TIME_HR, ARR_TIME_MIN, ARR_TIME_BLK,
           CANCELLED, CANCELLATION_CODE, DIVERTED,
           AIR_TIME)
  
  # Only keep O_D from 2018
  # format(object.size(d_crossyear), 'Gb')
  rm(d_15, d_16, d_17, d_18); gc()
  system('free -g')# Print free memory in Gb
  
  # Save full cross year for easier setup, and also save test/trainign validation set
  save('d_crossyear', file = file.path(sharedloc, 'ASQP_2015-2018.RData'))
  
  # Stratify random sampling to ensure all Carrier x O_D are represented in training set
  d_s <- d_crossyear %>%
    mutate(rowN = 1:nrow(d_crossyear),
           c_od = paste(CARRIER, O_D)) %>%
    select(rowN, c_od, CARRIER, O_D)
  
  samprow <- vector()
  
  for(i in unique(d_s$c_od)){
    d <- d_s %>%
      filter(c_od == i)
    
    # Is it divisible by 5? Eliminates any carrier x o_d combinations between 0 and 4 flights across all 4 years. 
    if(nrow(d) / 5 > 0){
      samprow_c_od <- sample(d$rowN, nrow(d)*.2, replace = F)
      samprow <- c(samprow, samprow_c_od)
    } 
    
  }
  
  # samprow <- sample(1:nrow(d_crossyear), nrow(d_crossyear)*.2, replace = F) For unstratified sampling
  
  d_crossyear_validate <- d_crossyear[samprow,]
  d_crossyear_train <- d_crossyear[!rownames(d_crossyear) %in% samprow,]
  
  # ensure sampling number adds up
  stopifnot(nrow(d_crossyear_validate) == length(samprow))
  stopifnot(nrow(d_crossyear_validate) + nrow(d_crossyear_train) == nrow(d_crossyear))
  
  save(list = c('d_crossyear_validate', 'd_crossyear_train', 'samprow'), file = file.path(sharedloc, 'ASQP_2015-2018_train.RData'))
  
  # Prep 2019 test data
  load(file.path(sharedloc, 'ASQP_2019.RData'))
  
  d_19_1 = d_19_1 %>%
    select(names(d_19_1)[names(d_19_1) %in% names(d_19_2)])
  
  d_19 = full_join(d_19_1, d_19_2)
  
  d_19 <- d_19 %>%
    filter(!is.na(AIR_TIME) & CARRIER %in% use_carriers) %>%
    mutate(O_D = paste(ORIGIN, DEST, sep = "-"))
  
  save(list = c('d_19'), file = file.path(sharedloc, 'ASQP_2019_validate.RData'))
  
}