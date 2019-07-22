# Figure to support the conclusion that models can project airborne time within 10% “weeks to months in advance.” The report would be more convincing with a graph that shows error on one axis and length of forecast. The report needs to be clear if its findings counter the expectation that the level of error to be higher for a short term forecast than a forecast months in advance.

# Set up ----
library(tidyverse)
library(gridExtra)

codeloc = ifelse(file.exists('~/git/Wheels_Up'),
                 "~/git/Wheels_Up",
                 "~/GitHub/Wheels_Up/")

sharedloc = "~/Temp_Working_Docs/Results" 
# "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Data"
resultsloc = "~/Temp_Working_Docs/Results"
# "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Results"
figloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Figures"
load(file.path(resultsloc, 'All_Results.RData')) # Created by analysis/Results_prep.R
load(file.path(resultsloc, 'Flight_Level_Ensemble.RData')) # For summaries of most and least predictible airports and other summary statistics
load(file.path(sharedloc, 'ASQP_2019_to04_validate.RData'))

# Prepare error by time
# Model results did not save exact date and time of departure, only variables used in the models, day of week and departure time block. Join back with original data to get date at least.

d_19_match = d_19 %>%
  filter(O_D %in% unique(d_fl_ensemble_2019$O_D) &
           CARRIER %in% unique(d_fl_ensemble_2019$CARRIER)) %>%
  mutate(MONTH = formatC(MONTH, width = 2, flag = '0'),
         key = paste(CARRIER, MONTH, DAY_OF_WEEK, O_D, AIR_TIME, sep = "_"))

d_fl_ensemble_2019_match = d_fl_ensemble_2019 %>%
  filter(!is.na(Est_AIR_TIME) & Est_AIR_TIME < 1e4) %>%
  mutate(pct_Err = 100*( abs(Est_AIR_TIME - AIR_TIME) / AIR_TIME),
         key = paste(CARRIER, MONTH, DAY_OF_WEEK, O_D, AIR_TIME, sep = "_"))


d_fl_19 = left_join(d_fl_ensemble_2019_match %>%
                      filter(!duplicated(key)),
                    d_19_match %>%
                      filter(!duplicated(key)),
                    by = c('key'))

# Summarize by date ----

by_date = d_fl_19 %>%
  filter(!is.na(FLIGHT_DATE)) %>%
  group_by(FLIGHT_DATE) %>%
  summarize(mean_AT = mean(AIR_TIME.x),
            med_AT = median(AIR_TIME.x),
            mean_Err = mean(abs(Est_AIR_TIME - AIR_TIME.x)),
            med_Err = median(abs(Est_AIR_TIME - AIR_TIME.x)),
            mean_pct_Err = mean(pct_Err),
            med_pct_Err = median(pct_Err),
            n_flights = n())

mean_Err_plot = ggplot(by_date, 
       aes(x = FLIGHT_DATE, y = mean_AT, 
           ymin = mean_AT - mean_Err, ymax = mean_AT + mean_Err)) + 
  geom_line(size = 1) + 
  geom_ribbon(alpha = 0.2, fill = 'midnightblue') + 
  theme_bw() +
  xlab('Flight date in 2019') + ylab('Mean airborne time (min)') +
  geom_vline(xintercept = seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-05-01"), by = "1 months"),color = "grey20", size = 0.3)
  
# Pct err plot
pct_Err_plot = ggplot(by_date, 
       aes(x = FLIGHT_DATE, y = mean_pct_Err)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab(NULL) + ylab('Mean percent error \n (% of airborne time)') +
  geom_vline(xintercept = seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-05-01"), by = "1 months"),color = "grey20", size = 0.3)

stacked_err = grid.arrange(pct_Err_plot, 
                           mean_Err_plot, 
                           nrow = 2,
                           heights = c(3, 5))
ggsave(stacked_err, 
       file = file.path(figloc, 'Pred_err_2019.jpg'),
       width = 8, height = 4.5)


# by median (tighter) ----
med_Err_plot = ggplot(by_date, 
                      aes(x = FLIGHT_DATE, y = med_AT, 
                          ymin = med_AT - med_Err, ymax = med_AT + med_Err)) + 
  geom_line(size = 1) + 
  geom_ribbon(alpha = 0.2, fill = 'midnightblue') + 
  theme_bw() +
  xlab('Flight date in 2019') + ylab('Median airborne time (min)') +
  geom_vline(xintercept = seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-05-01"), by = "1 months"),color = "grey20", size = 0.3)


# Pct err plot
pct_Err_plot = ggplot(by_date, 
                      aes(x = FLIGHT_DATE, y = med_pct_Err)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab(NULL) + ylab('Median percent error \n (% of airborne time)') +
  geom_vline(xintercept = seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-05-01"), by = "1 months"),color = "grey20", size = 0.3)

stacked_err_med = grid.arrange(pct_Err_plot, 
                           med_Err_plot, 
                           nrow = 2,
                           heights = c(3, 5))
ggsave(stacked_err_med, 
       file = file.path(figloc, 'Pred_err_med_2019.jpg'),
       width = 8, height = 4.5)
