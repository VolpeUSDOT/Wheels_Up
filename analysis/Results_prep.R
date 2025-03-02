# Results prep
# 1. Prepare single csv files for each analysis, summarizing at carrier or O-D level
# 2. Prepare ensemble model file at O-D level
# 3. Using best fit models for each O-D, prepare flight level prep

# There are three sets of models: base regression models, regression + congestion variables, XGBoost models (which include congestion)
# For each set, there is two kinds of validation: internal (80/20 split by carrier/O-D for training and test 2015-2018) and 2019 (train on all 2015-2018, test on Jan/Feb 2019).

# Setup ----
library(tidyverse)

codeloc = ifelse(grepl('Flynn', normalizePath('~/')),
                 "~/git/Wheels_Up",
                 "~/GitHub/Wheels_Up")
sharedloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Data"
resultsloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Results"

# Function to prepare model results

prep_res <- function(Analysis, within = F, outname = NULL, 
                     valid_type = c('Internal', '2019'),
                     assign_pref = 'd_across_'){
  
  Aname = paste0(Analysis, "_")
  if(is.null(outname)) { outname = Aname }
  
  if(within == T){
    for(within_carr_valid in valid_type){
      ols_res <- dir(file.path(resultsloc, paste0(Aname, within_carr_valid)))
      
      fn = file.path(resultsloc, paste0(Aname, within_carr_valid, '.csv'))
      if(!file.exists(fn)){
        d <- vector()
        for(i in 1:length(ols_res)){
          dx <- read.csv(file.path(resultsloc, paste0(Aname, within_carr_valid), ols_res[i]))
          carrier = substr(ols_res[i], 1, 2)
          dx <- data.frame(carrier, dx)
          dx <- dx %>%
            filter(O_D != '') %>%
            mutate(Origin = substr(O_D, 1, 3),
                   Destination = substr(O_D, 5, 7))
          
          d <- rbind(d, dx)
        }
        
        assign(paste0(assign_pref, within_carr_valid), d, envir = globalenv() )
        
        write.csv(d, file = fn, row.names = F)
      } else {
        assign(paste0(assign_pref, within_carr_valid), read.csv(fn), envir = globalenv() )
      }
    } # End loop within carriers
    
  } else { # end check for within carrier (1Carrier analyses)
    
    for(across_carr_valid in valid_type){
      ols_res <- dir(file.path(resultsloc, paste0(Aname, across_carr_valid)))
      
      fn = file.path(resultsloc, paste0(Aname, across_carr_valid, '.csv'))
      if(!file.exists(fn)){
        d <- read.csv(file.path(resultsloc, paste0(Aname, across_carr_valid), paste0(outname, across_carr_valid, '.csv')))
        d <- d %>%
          filter(O_D != '') %>%
          mutate(Origin = substr(O_D, 1, 3),
                 Destination = substr(O_D, 5, 7))
        
        assign(paste0(assign_pref, across_carr_valid), d, envir = globalenv() )
        
        write.csv(d, file = fn, row.names = F)
      } else {
        assign(paste0(assign_pref, across_carr_valid), read.csv(fn), envir = globalenv() )
      }
    } # end loop across carriers
    
  } # End else for across carriers (1O-D analysises)
}

# Prep results ----
# Base models: Regression 

prep_res(Analysis = '1Carrier_Crossyear_Validate', within = T, assign_pref = 'd_within_')

prep_res(Analysis = '1O-D_Crossyear2_Validate', assign_pref = 'd_across_')

# Regression + Congestion variables 

prep_res(Analysis = '1O-D_Congestion2_Validate', assign_pref = 'd_across_cong_')


# XGBoost 

prep_res(Analysis = '1O-D_Congestion_xgB_Validate', assign_pref = 'd_across_xgb_')

# Create ensemble for across-carrier ----

across_analyses_2019 = ls()[grep('d_across\\w+2019', ls())]
print(across_analyses_2019) # Three sets of models

across_analyses_Internal = ls()[grep('d_across\\w+Internal', ls())]
print(across_analyses_Internal) # Three sets of models


# Loop over results O-D and find best one; use that row for ensemble
combo_names <- c(unique(c(names(d_across_2019), names(d_across_xgb_2019))), 'Best_Model')
res = matrix(data = '', nrow = 1, ncol = length(combo_names))
colnames(res) = combo_names
res = as.data.frame(res)
res = res %>% 
  mutate_if(is.factor, as.character)

for(i in 1:nrow(d_across_2019)){
  # i = 1
  od = d_across_2019[i, 'O_D']
  MAE_compare = which.min(c(d_across_2019 %>% filter(O_D == od) %>% select(MAE),
                            d_across_cong_2019 %>% filter(O_D == od) %>% select(MAE),
                            d_across_xgb_2019 %>% filter(O_D == od) %>% select(MAE) ))
  
  if(length(MAE_compare) > 0){
    od_best <- get(c('d_across_2019', 'd_across_cong_2019', 'd_across_xgb_2019')[MAE_compare]) %>% filter(O_D == od)
    Best_Model = c('Base_regression', 'Regression_congestion', 'XGBoost')[MAE_compare]
  } else {
    od_best <-  d_across_2019 %>% filter(O_D == od) 
    Best_Model = 'Insufficient_data'
  }
  od_best = od_best %>% 
    mutate_all(as.character)
  
  od_best$Best_Model = Best_Model
  
  res <- suppressMessages( full_join(res, od_best) )
  
  if(i %% 100 == 0) cat(i, as.character(od), ' . ')
}

d_ensemble_2019 <- res %>% filter(!is.na(O_D) & O_D != '') %>%
  mutate_at(c('O_D', 'top_sig_coef', 'Model', 'Origin', 'Destination', 'top_feature', 'Pred_vars', 'Best_Model'), as.factor) %>%
  mutate_if(is.character, as.numeric)

write.csv(d_ensemble_2019, file = file.path(resultsloc, 'Air_Models_2019_Ensemble.csv'), row.names = F)

# Repeat for internal validation

combo_names <- c(unique(c(names(d_across_Internal), names(d_across_xgb_Internal))), 'Best_Model')
res = matrix(data = '', nrow = 1, ncol = length(combo_names))
colnames(res) = combo_names
res = as.data.frame(res)
res = res %>% 
  mutate_if(is.factor, as.character)

for(i in 1:nrow(d_across_Internal)){
  # i = 1
  od = d_across_Internal[i, 'O_D']
  MAE_compare = which.min(c(d_across_Internal %>% filter(O_D == od) %>% select(MAE),
                            d_across_cong_Internal %>% filter(O_D == od) %>% select(MAE),
                            d_across_xgb_Internal %>% filter(O_D == od) %>% select(MAE) ))
  
  if(length(MAE_compare) > 0){
    od_best <- get(c('d_across_Internal', 'd_across_cong_Internal', 'd_across_xgb_Internal')[MAE_compare]) %>% filter(O_D == od)
    Best_Model = c('Base_regression', 'Regression_congestion', 'XGBoost')[MAE_compare]
  } else {
    od_best <-  d_across_Internal %>% filter(O_D == od) 
    Best_Model = 'Insufficient_data'
  }
  od_best = od_best %>% 
    mutate_all(as.character)
  
  od_best$Best_Model = Best_Model
  
  res <- suppressMessages( full_join(res, od_best) )
  
  if(i %% 100 == 0) cat(i, as.character(od), ' . ')
}

d_ensemble_Internal <- res %>% filter(!is.na(O_D) & O_D != '') %>%
  mutate_at(c('O_D', 'top_sig_coef', 'Model', 'Origin', 'Destination', 'top_feature', 'Pred_vars', 'Best_Model'), as.factor) %>%
  mutate_if(is.character, as.numeric)

write.csv(d_ensemble_Internal, file = file.path(resultsloc, 'Air_Models_Internal_Ensemble.csv'), row.names = F)

save(list = ls()[grep('^d_', ls())],
     file = file.path(resultsloc, 'All_Results.RData'))

# Create ensemble flight-level data ----

# load(file.path(resultsloc, 'All_Results.RData'))

# Use the d_ensemble data frames to identify which model results to pull from. Then use the Flight_Level data frames to pull the individual flights out
# Some individual flight rows were garbled when writing out in parallel to csv file. Here removing them by filtering on use_carriers

use_carriers = unique(d_within_2019$carrier)

prep_fl <- function(Analysis, 
                     valid_type = c('Internal', '2019'),
                     assign_pref = 'd_fl_'){
  
  Aname = paste0(Analysis, "_")
  
  for(across_carr_valid in valid_type){
    ols_res <- dir(file.path(resultsloc, paste0(Aname, across_carr_valid)))
    
    d <- readr::read_csv(file.path(resultsloc, paste0(Aname, across_carr_valid), paste0('Flight_Level_', Aname, across_carr_valid, '.csv')),
                         skip_empty_rows = T)

    d <- d %>%
        filter(O_D != '' & CARRIER %in% use_carriers) %>%
        mutate(Origin = substr(O_D, 1, 3),
               Destination = substr(O_D, 5, 7))
      
    assign(paste0(assign_pref, across_carr_valid), d, envir = globalenv() )
  } # End validation type loop 
}


# Base 
prep_fl(Analysis = '1O-D_Crossyear2_Validate', assign_pref = 'd_fl_base_')

# Regression + Congestion variables 
prep_fl(Analysis = '1O-D_Congestion2_Validate', assign_pref = 'd_fl_cong_')

# XGBoost 
prep_fl(Analysis = '1O-D_Congestion_xgB_Validate', assign_pref = 'd_fl_xgb_')


# Now look at every O-D in the ensemble. Pull together the flight level data for the matching best model
# Base_regression, Regression_congestion, or XGBoost
# Fix base regression once those are complete 2019-06-10

d_fl_ensemble_2019 = d_fl_ensemble_Internal = vector()

for(i in 1:nrow(d_ensemble_2019)){
  # i = 1
  od = d_ensemble_2019[i, 'O_D']
  
  Best_2019 = d_ensemble_2019[i, 'Best_Model']
  
  if(Best_2019 == 'Base_regression'){
    d_fl_ensemble_2019 = rbind(d_fl_ensemble_2019,  d_fl_base_2019 %>% filter(O_D == od))
  }
  
  if(Best_2019 == 'Regression_congestion'){
    d_fl_ensemble_2019 = rbind(d_fl_ensemble_2019,  d_fl_cong_2019 %>% filter(O_D == od))
  }
  
  if(Best_2019 == 'XGBoost'){
    d_fl_ensemble_2019 = rbind(d_fl_ensemble_2019,  d_fl_xgb_2019 %>% filter(O_D == od))
  }
  
  Best_Internal = d_ensemble_Internal[i, 'Best_Model']
  
  if(Best_Internal == 'Base_regression'){
    d_fl_ensemble_Internal = rbind(d_fl_ensemble_Internal,  d_fl_base_Internal %>% filter(O_D == od))
  }
  
  if(Best_Internal == 'Regression_congestion'){
    d_fl_ensemble_Internal = rbind(d_fl_ensemble_Internal,  d_fl_cong_Internal %>% filter(O_D == od))
  }
  
  if(Best_Internal == 'XGBoost'){
    d_fl_ensemble_Internal = rbind(d_fl_ensemble_Internal,  d_fl_xgb_Internal %>% filter(O_D == od))
  }

  if(i %% 100 == 0) cat(i, as.character(od), ' . ')
}

save(list = c('d_fl_ensemble_2019', 'd_fl_ensemble_Internal'),
     file = file.path(resultsloc, 'Flight_Level_Ensemble.RData'))

write.csv(d_fl_ensemble_2019, file = file.path(resultsloc, 'Flight_Level_Ensemble_2019.csv'), row.names = F)
write.csv(d_fl_ensemble_Internal, file = file.path(resultsloc, 'Flight_Level_Ensemble_Internal.csv'), row.names = F)

# TODO (maybe):
# - Get CRS, actual gate to gate, delay variables, and cancellation variables from original ASQP data for each flight 

# load(file.path(resultsloc, 'Flight_Level_Ensemble.RData'))
