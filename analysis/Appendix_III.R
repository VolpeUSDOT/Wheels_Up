# Read in prepared results files, and make Appendix III tables for predictability of flights by carriers for internal (2015-2018) and external (2019) validation.

# Setup ----
library(tidyverse)
library(knitr)
library(kableExtra)

codeloc = ifelse(grepl('Flynn', normalizePath('~/')),
                 "~/git/Wheels_Up",
                 "~/GitHub/Wheels_Up")
sharedloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Data"
resultsloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Results"

# Read results

load(file.path(resultsloc, 'All_Results.RData'))

load(file.path(resultsloc, 'Flight_Level_Ensemble.RData'))

# Summarize by carrier from flight level ---- 
d_fl_ensemble_Internal = d_fl_ensemble_Internal %>%
  mutate(Est_AIR_TIME = ifelse(Est_AIR_TIME < 1e3, Est_AIR_TIME, NA),
       abs_err = abs(Est_AIR_TIME - AIR_TIME),
       pct_err = abs_err / AIR_TIME) 

int_carrier = d_fl_ensemble_Internal %>%
  group_by(CARRIER) %>%
  summarize(N_flight_segments = length(unique(O_D)), # Number of O_D pairs
            N_flights_estimated = sum(!is.na(Est_AIR_TIME)), # Number of individual flights estimated
            MAE = round(mean(abs_err, na.rm = T), 2),
            pct_in_10min = round(100 * sum(abs_err <= 10 & !is.na(abs_err)) / N_flights_estimated, 2),
            pct_in_10pct = round(100 * sum(pct_err <= .1 & !is.na(pct_err)) / N_flights_estimated, 2))

int_carrier %>%
  mutate(N_flight_segments = format(as.numeric(N_flight_segments), big.mark = ','),
         N_flights_estimated = format(as.numeric(N_flights_estimated), big.mark = ','))


kable(int_carrier,
      format.args = list(big.mark = ','),
      caption = "Summary of flight predictability by carrier, 2015-2018 with internal validation. MAE: Mean absolute error in minutes." ) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))

# 2019

d_fl_ensemble_2019 = d_fl_ensemble_2019 %>%
  mutate(Est_AIR_TIME = ifelse(Est_AIR_TIME < 1e3, Est_AIR_TIME, NA),
         abs_err = abs(Est_AIR_TIME - AIR_TIME),
         pct_err = abs_err / AIR_TIME) 

ext_carrier = d_fl_ensemble_2019 %>%
  group_by(CARRIER) %>%
  summarize(N_flight_segments = length(unique(O_D)), # Number of O_D pairs
            N_flights_estimated = sum(!is.na(Est_AIR_TIME)), # Number of individual flights estimated
            MAE = round(mean(abs_err, na.rm = T), 2),
            pct_in_10min = round(100 * sum(abs_err <= 10 & !is.na(abs_err)) / N_flights_estimated, 2),
            pct_in_10pct = round(100 * sum(pct_err <= .1 & !is.na(pct_err)) / N_flights_estimated, 2))

ext_carrier %>%
  mutate(N_flight_segments = format(as.numeric(N_flight_segments), big.mark = ','),
         N_flights_estimated = format(as.numeric(N_flights_estimated), big.mark = ','))


kable(ext_carrier,
      format.args = list(big.mark = ','),
      caption = "Summary of flight predictability by carrier, valided on 2019 data. MAE: Mean absolute error in minutes." ) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))


