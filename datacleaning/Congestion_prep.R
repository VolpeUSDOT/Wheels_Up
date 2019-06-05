# Prepare congestion values for Air Time models
# By year, for each airport, calculate number of flights arriving or departing by hour of day, day of week, month of year.
# One file across years. File will be at most 320 airports x 4 years x 12 months x 7 days x 24 hours = 2.5 M rows. Will have one origin and one departure file.

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
library(doParallel)
library(foreach)
# Congestion prep  ----

if(!file.exists(file.path(sharedloc, 'Congestion_2015-2018.RData'))){
  # Data will already by subset to airports used by focal carriers (don't include airports not served by at least one focal carriers, each year)
  
  if(!file.exists(file.path(sharedloc, 'ASQP_2015-2018.RData'))){
    source(file.path(codeloc, 'datacleaning', 'Analysis_prep.R'))
  } else {
    load(file.path(sharedloc, 'ASQP_2015-2018.RData'))
  }
  
  airports = sort(unique(c(d_crossyear$ORIGIN, d_crossyear$DEST))); cat(length(airports), 'Airports \n')
  # Prepare congestion file if not completed already
  
  dy_o <- d_crossyear %>% 
    group_by(ORIGIN, YEAR, MONTH, DAY_OF_WEEK, DEP_TIME_BLK) %>%
    summarize(N_flights = n())  
  
  # Create relative congestion within an airport across year
  dy_ox <- dy_o %>%
    group_by(ORIGIN, YEAR) %>%
    summarize(N_flights_max = max(N_flights))
  
  dy_o <- left_join(dy_o, dy_ox, by = c('ORIGIN', 'YEAR')) %>%
    mutate(Relative_Congestion = N_flights / N_flights_max) %>%
    select(-N_flights_max)
  
  assign('Congestion_Origin', dy_o); rm(dy_o, dy_ox)

  dy_d <- d_crossyear %>% 
    group_by(DEST, YEAR, MONTH, DAY_OF_WEEK, ARR_TIME_BLK) %>%
    summarize(N_flights = n())  
  
  # Create relative congestion within an airport across year
  dy_dx <- dy_d %>%
    group_by(DEST, YEAR) %>%
    summarize(N_flights_max = max(N_flights))
  
  dy_d <- left_join(dy_d, dy_dx, by = c('DEST', 'YEAR')) %>%
    mutate(Relative_Congestion = N_flights / N_flights_max) %>%
    select(-N_flights_max)
  
  assign('Congestion_Destination', dy_d); rm(dy_d, dy_dx)
  
  cong_files = ls()[grep('Congestion_', ls())]
  
  save(list = cong_files,
       file = file.path(sharedloc, 'Congestion_2015-2018.RData'))
} else {
  load(file.path(sharedloc, 'Congestion_2015-2018.RData'))
}

# <><><> Some plotting 

NOTRUN = F
if(NOTRUN){
  # Probably too much detail. Shows distrubtion shift by time of day at least.
  ggplot(Congestion_Destination) + 
    geom_histogram(aes(x = Relative_Congestion, group = as.factor(MONTH), fill = as.factor(MONTH))) + 
    facet_wrap(~ARR_TIME_BLK) 
  
  ggplot(Congestion_Origin) + 
    geom_histogram(aes(x = Relative_Congestion, group = as.factor(MONTH), fill = as.factor(MONTH))) + 
    facet_wrap(~DEP_TIME_BLK) 
  
  # Too much...
  ggplot(Congestion_Destination) + 
    geom_violin(aes(y = Relative_Congestion, 
                    x = ARR_TIME_BLK)) +
    facet_wrap(~MONTH) 
  
  # Better
  ggplot(Congestion_Destination) + 
    geom_boxplot(aes(y = Relative_Congestion, 
                    x = ARR_TIME_BLK)) 

  ggplot(Congestion_Origin) + 
    geom_boxplot(aes(y = Relative_Congestion, 
                     x = DEP_TIME_BLK)) 
  
  # Not much variation by dow
  ggplot(Congestion_Origin) + 
    geom_boxplot(aes(y = Relative_Congestion, 
                     x = DEP_TIME_BLK)) +
    facet_wrap(~DAY_OF_WEEK)
  
  
}
