#Extended Forbes 2019 Figure

# Set up ----
library(tidyverse)
library(gridExtra) # for grid.arrange of multiple ggplots

codeloc = ifelse(grep('Shiu', normalizePath('~/')),
                 "~/git/Wheels_Up",
                 "~/GitHub/Wheels_Up/figures")
sharedloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Data"
figloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Figures"
avail_data = dir(sharedloc)
avail_data = avail_data[grep('^ASQP.\\d{4}.RData$', avail_data)] # Match with regular expresion to only prepared .RData files

if(length(avail_data) == 0) {
  source(file.path(codeloc, 'datacleaning', 'Data_prep.R'))
} else {
  for(x in avail_data){
    load(file.path(sharedloc, x))
  }
  load(file.path(sharedloc, 'Cross-year_Summary.RData'))
}
#Stack change in average time needed taxi in, air Time, taxi Out. Also extract scheduled time and actual time
#"TAXI_IN", "TAXI_OUT", "AIR_TIME", ?? "CRS_ELAPSED_TIME", "ACTUAL_ELAPSED_TIME"??