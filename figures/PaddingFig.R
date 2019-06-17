#Extended Forbes 2019 Figure

# Set up ----
library(tidyverse)
library(gridExtra) # for grid.arrange of multiple ggplots
library(ggplot2)
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

#Bind 3 months of 2019 data together
d_19 = rbind(d_19_1,d_19_2,d_19_3)

#Filter out carriers added in 2018
use_carriers = c('AA', 'AS', 'B6', 'DL', 'EV', 'F9', 'HA', 'NK', 'OO', 'UA', 'VX', 'WN')
d_18 = filter(d_18,CARRIER %in% use_carriers)
d_19 = filter(d_19, CARRIER %in% use_carriers)

#Reference values to calculate change in 
#Values are estimated from the 2015 values in the Forbes figure
taxi_out_ref = mean(d_15[["TAXI_OUT"]], na.rm=TRUE)-2.5
air_time_ref = mean(d_15[["AIR_TIME"]], na.rm=TRUE)-.7
taxi_in_ref = mean(d_15[["TAXI_IN"]], na.rm=TRUE)-2.3
crs_ref = mean(d_15[["CRS_ELAPSED_TIME"]], na.rm=TRUE)-8.8
total_ref = mean(d_15[["ACTUAL_ELAPSED_TIME"]], na.rm=TRUE)-5.5

#Empty vectors to fill with mean values
added_time = vector()
actual_time = vector()
pred_time = vector()

#Year and type labels for each averaged data point and line plot points
year = rep(2015:2019, each = 3)
type = rep(c("Taxi Out","Air Time","Taxi In"), times = 5)
year_simple = 2015:2019

#Iterate over all the available data and create the annual mean.
#Append values to vectors added_time for the area plot, and pred_time/actual_time for the line plots
all_data = mget(paste("d_", 15:19, sep = ""), envir=.GlobalEnv)
for(d in all_data){
  #Annual mean of values
  taxi_out = mean(d[["TAXI_OUT"]], na.rm = TRUE) - taxi_out_ref
  air_time = mean(d[["AIR_TIME"]], na.rm = TRUE) - air_time_ref
  taxi_in = mean(d[["TAXI_IN"]], na.rm = TRUE) - taxi_in_ref
  crs = mean(d[["CRS_ELAPSED_TIME"]], na.rm=TRUE) - crs_ref
  actual = mean(d[["ACTUAL_ELAPSED_TIME"]], na.rm=TRUE) - total_ref
    
  #Placing mean values into appropriate vectors
  added_time = c(added_time, c(taxi_out,air_time,taxi_in))
  pred_time = c(pred_time, crs)
  actual_time = c(actual_time,actual)
}

#Order the data to match Forbes 2019 figure
added_time_df = data.frame(type, year, added_time)
added_time_df$type = factor(added_time_df$type, levels = levels(added_time_df$type)[c(2,1,3)])
line_plot_df = data.frame(year_simple,pred_time, actual_time)
#Create Plot
ggplot()+
  geom_area(data = added_time_df, mapping = aes(x = year,y = added_time,
                                                fill = type))+
  scale_fill_manual(values=c("#fff549","#994646","#68824b"))+
  labs(fill = "")+
  geom_line(data = line_plot_df, mapping = aes(x=year_simple, y = pred_time,color = "Scheduled Time"), size=1)+
  geom_line(data = line_plot_df, mapping = aes(x=year_simple, y = actual_time, color = "Actual Time"),size = 1)+
  scale_color_manual(values = c("#000000","#f7b93d"), name = "")+
  ggtitle("Decomposition of Changes in Actual Flight Time (Year 2014 = 0)")+
  xlab("Year")+
  ylab("mins")