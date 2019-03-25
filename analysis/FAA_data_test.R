# Test of ASPQ data for Bill

# Setup ----
library(tidyverse) # if this fails, run install.packages('tidyverse')

sharedloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Data"

load(file.path(sharedloc, 'ASQP_2018.RData'))

# Quick review  ----

# data are all in one huge data frame, called d_18. We can make different summary tables and plots from this data set. Here are some examples. See many more examples on https://www.tidyverse.org/

# Extract carriers and airports, make tables showing which years they appear in and how frequently

carr_yr = d_18 %>%
  group_by(CARRIER) %>%
  summarize(count_18 = n())

carr_yr # will produce a table, 18 rows x 2 columns

od_yr = d_18 %>%
  group_by(ORIGIN, DEST) %>%
  summarize(count_18 = n())


# Make some plots of AIR_TIME distributions by carrier and origin

at_carrier <- d_18 %>%
  group_by(CARRIER, ORIGIN) %>%
  summarize(n_flights = n(),
            mean_air_time = mean(as.numeric(AIR_TIME), na.rm=T),
            sd_air_time = sd(as.numeric(AIR_TIME), na.rm=T))


# Try one carrier, Southwest

ggplot(at_carrier %>% filter(CARRIER == 'WN')) + 
  geom_histogram(aes(x = mean_air_time))

# Plot of mean vs sd: longer flights are more highly variable; can have high variability even with a large number of flights
ggplot(at_carrier %>% filter(CARRIER == 'WN')) + 
  geom_point(aes(x = mean_air_time,
                 y = n_flights, 
                 size = sd_air_time),
             pch = 21)


# Different kind of plot: width is the number of flights (by origin) with that value of mean air time. Long skinny 'violin' for Hawiian is because it has relatively few flights, and most of them are very long. Short squat violin are for carriers which have many short flights. 
ggplot(at_carrier) + 
  geom_violin(aes(x = CARRIER,
                 y = mean_air_time,
                 fill = n_flights))


