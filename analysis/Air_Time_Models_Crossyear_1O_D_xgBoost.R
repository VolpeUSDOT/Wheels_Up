# Modeling air time with xgBoost instead of linear regression
# OLS regression by O_D and Carrier
# For each unique O_D, fit a model with Carrier as a variable
# With congestion at origin and destination 
# Also saving observed and predicted flight times to flat files for each analysis. 

# Setup ----
# Check to see if working on EC2 instance or local
if(Sys.getenv('USER')=='rstudio'){
  codeloc = '~/Wheels_Up/Users/Daniel.Flynn/Documents/git/Wheels_Up'  
  sharedloc = "~/data"
  resultsloc = "~/results"
} else { 
  
  codeloc = ifelse(grepl('Flynn', normalizePath('~/')),
                   "~/git/Wheels_Up",
                   "~/GitHub/Wheels_Up")
  sharedloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Data"
  resultsloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Results"
  
}

setwd(codeloc)

# <<><<><<> START LOOP OVER VALIDATION OPTIONS
VALIDATION_opts = c('Internal', '2019') # VALIDATION ='Internal' # Two options for validation. One, use a training/test dataset and validate internally 
for(VALIDATION in VALIDATION_opts){
  # <<><<><<>
  
  Analysis = paste0('1O-D_Congestion_xgB_Validate_', VALIDATION)
  
  saveloc = file.path(resultsloc, Analysis); system(paste('mkdir -p', saveloc))
  system(paste('mkdir -p', file.path(saveloc, 'Models'))) # Directory for xgboost feature importance / gain values
  
  library(tidyverse) 
  library(foreach)
  library(doParallel)
  library(xgboost)
  
  # Check to see that prepped data is available. Will load individual years and create test/training data sets if necessary.
  source(file.path(codeloc, 'datacleaning', 'Analysis_prep.R'))
  # Load congestion data or prepare if not completed yet
  source(file.path(codeloc, 'datacleaning', 'Congestion_prep.R'))
  
  if(VALIDATION == 'Internal'){
    load(file.path(sharedloc, 'ASQP_2015-2018_train.RData')) # For internal. Includes both d_crossyear_train and d_crossyear_validate
    d_train <- d_crossyear_train # rename data frames
    d_test <- d_crossyear_validate
    rm(d_crossyear_train, d_crossyear_validate)
  }
  if(VALIDATION == '2019'){
    load(file.path(sharedloc, 'ASQP_2015-2018.RData'))
    load(file.path(sharedloc, 'ASQP_2019_validate.RData')) 
    d_train <- d_crossyear # rename data frames
    d_test <- d_19
    rm(d_crossyear, d_19)
  }
  
  # Join with congestion data ----
  # For each flight, four variables are added: N_flights and Relative_Congestion for the origin airport, and N_flights and Relative_Congestion for the destination airport. Congestion metrics are specific to that Year, month, day of week, and hour of day. 
  # For 2019 test data, use the 2018 congestion metrics. Relative_Congestion is the percent congestion for that airport relative to its maximum flights over a year, so we need a complete year of data to calculate.
  
  d_train <- d_train %>% 
    left_join(Congestion_Origin, by = c('ORIGIN', 'YEAR', 'MONTH', 'DAY_OF_WEEK', 'DEP_TIME_BLK')) %>%
    rename(ORIGIN_REL_CONG = Relative_Congestion,
           ORIGIN_N_flights = N_flights)
  d_train <- d_train %>% 
    left_join(Congestion_Destination, by = c('DEST', 'YEAR', 'MONTH', 'DAY_OF_WEEK', 'ARR_TIME_BLK')) %>%
    rename(DEST_REL_CONG = Relative_Congestion,
           DEST_N_flights = N_flights)
  
  # For 2019 validation, we will use the year coefficients and congestion values for 2018. 
  # Simplest way to do this is to update the d_test file with 2018 as the year.
  if(VALIDATION == '2019') {
    d_test$YEAR = '2018'
    d_test$YEAR <- as.factor(d_test$YEAR)
    d_test$DAY_OF_WEEK <- as.factor(d_test$DAY_OF_WEEK)
    
  }
  d_test <- d_test %>% 
    left_join(Congestion_Origin, by = c('ORIGIN', 'YEAR', 'MONTH', 'DAY_OF_WEEK', 'DEP_TIME_BLK')) %>%
    rename(ORIGIN_REL_CONG = Relative_Congestion,
           ORIGIN_N_flights = N_flights)
  
  d_test <- d_test %>% 
    left_join(Congestion_Destination, by = c('DEST', 'YEAR', 'MONTH', 'DAY_OF_WEEK', 'ARR_TIME_BLK')) %>%
    rename(DEST_REL_CONG = Relative_Congestion,
           DEST_N_flights = N_flights)
  
  # Funtion to match factors level between training and validation data sets. Add empty factor levels if necessary 
  levadd <- function(factor_var, test, train){
    class(test) = 'data.frame'
    class(train) = 'data.frame'
    tlev <- levels(test[,factor_var])
    addlev <- levels(train[,factor_var])[!levels(train[,factor_var]) %in% tlev]
    levels(test[,factor_var]) = c(levels(test[,factor_var]), addlev)
    test[,factor_var]
  }
  
  # Loop over O_D ----
  
  O_D = unique(d_train$O_D) # 5,101 models
  starttime <- Sys.time()
  
  avail.cores <- parallel::detectCores()
  simul.models = 16 # range from 1 to max of available cores. At 1, will use all cores for each model individually (non-parallel)
  cl <- makeCluster(avail.cores/simul.models) # For xgBoost, try nthreads = 4 for each individual model, parallelize 
  registerDoParallel(cl)
  
  # xgBoost notes:
  # - objective will be reg:linear
  # - eval_metric will be mae
  xgb_params <- list(
    booster = "gbtree", 
    objective = "reg:linear", 
    eval_metric = "mae",
    eta = 0.3,                          # default 0.3
    gamma = 0,                          # default 0 
    max_depth = 15,                      # default 6, consider increasing
    min_child_weight = 1, 
    subsample = 1, 
    colsample_bytree = 1,
    nrounds = 100,                       # Consider decreasing if taking too long 
    nthreads = avail.cores/simul.models) # Appears that multithreading does not benefit much, so run on one thread
  
  # Test after getting dtrain set inside loop for one example od
  # system.time(
  # m_xgb_fit <- xgboost(dtrain, params = xgb_params,
  #                      nrounds = xgb_params$nrounds)
  # )
  
  if(!file.exists(file.path(saveloc, "1O_D_log.txt"))){
    writeLines(c(""), file.path(saveloc, "1O_D_log.txt"))
    # Summary table
    write.table(data.frame(O_D = '', N = '', N_carriers = '', Obs_Mean = '',
                           Obs_SD = '', 
                           N_valid = '', Obs_Mean_valid = '', Obs_SD_valid = '',
                           r2 = '', RMSE = '', MAE = '',
                           top_feature = '',
                           top_feature_gain = '',
                           Pred_vars = ''), 
                row.names = F, sep = ',',
                file.path(saveloc, paste0(Analysis, ".csv")))    
    # Flight-level data. Observed, predicted, values for variables
    write.table(data.frame(CARRIER = '', 
                           AIR_TIME = '', 
                           YEAR = '', 
                           MONTH = '', 
                           DAY_OF_WEEK = '', 
                           DEP_TIME_BLK = '',
                           ARR_TIME_BLK = '',
                           O_D = '', 
                           ORIGIN_REL_CONG = '', 
                           DEST_REL_CONG = '', 
                           Est_AIR_TIME = ''), 
                row.names = F, sep = ',',
                file.path(saveloc, paste0('Flight_Level_', Analysis, ".csv")))    
    
  } else {
    # Skip completed models
    completed <- read.csv(file.path(saveloc, paste0(Analysis, ".csv")), header = T)
    
    O_D <- O_D[!O_D %in% as.character(completed$O_D)]  
  }
  
  foreach(od = O_D, .packages = 'dplyr') %dopar% {
    
    # for(od in O_D[4555:4572]){ # Manual loop for debugging
    #   cat(od, '\n')
    # od = as.character(O_D[1])
    freem = paste(system("free -g | awk '{print $7}'", intern = T), collapse = '')
    
    write.table(data.frame(Time = as.character(Sys.time()), Completed = as.character(od), Free = freem),
                row.names = F,
                col.names = F,
                quote = F,
                file = file.path(saveloc, '1O_D_log.txt'),
                append = T)
    
    d_od = d_train %>% filter(O_D == od)
    d_v_od = d_test %>% filter(O_D == od)
    
    # Drop unused levels from existing factors and create factor variables from others
    d_od = d_od %>%
      mutate(CARRIER = as.factor(as.character(CARRIER)),
             O_D = as.character(O_D),
             YEAR = as.factor(as.character(YEAR)),
             MONTH = as.factor(formatC(MONTH, width = 2, flag = '0')),
             DAY_OF_WEEK = as.factor(as.character(DAY_OF_WEEK)),
             DEP_TIME_BLK = as.factor(as.character(DEP_TIME_BLK))) %>%
      select(CARRIER, AIR_TIME, YEAR, MONTH, DAY_OF_WEEK, DEP_TIME_BLK, ARR_TIME_BLK, O_D, ORIGIN_REL_CONG, DEST_REL_CONG)
    
    d_v_od = d_v_od %>%
      mutate(CARRIER = as.factor(as.character(CARRIER)),
             O_D = as.character(O_D),
             YEAR = as.factor(as.character(YEAR)),
             MONTH = as.factor(formatC(MONTH, width = 2, flag = '0')),
             DAY_OF_WEEK = as.factor(as.character(DAY_OF_WEEK)),
             DEP_TIME_BLK = as.factor(as.character(DEP_TIME_BLK))) %>%
      select(CARRIER, AIR_TIME, YEAR, MONTH, DAY_OF_WEEK, DEP_TIME_BLK, ARR_TIME_BLK, O_D, ORIGIN_REL_CONG, DEST_REL_CONG)
    
    # Can only use validation data for observations with factor levels also present in training data. 
    # If all levels present in validataion data are also present in training data, no change is made.
    # If a levels is present in a validation data factor variable which is not in trainign, it is removed from the validation data. Also need to remove the level, so recreate that factor variable
    vars = sapply(d_od, class)
    factorvars = names(vars[which(vars == 'factor')])
    
    for(i in factorvars) { 
      with_added_levs = levadd(factor_var = i, d_od, d_v_od) 
      zerolevs <- which(table(with_added_levs) == 0)
      # Drop any observations in validation set which are in these
      dvi = as.character(unclass(d_v_od[,i])[[1]]) %in% names(zerolevs)
      d_v_od = d_v_od[!dvi,]
    }
    
    # Drop unused levels from validation data, after removing values which are not presnt in trainign data
    d_v_od = d_v_od %>%
      mutate_at(factorvars, function(x) as.factor(as.character(x)))
    
    # Now, look to see if there are factor levels present in training whcih are not in validation. Add empty levels to the validation data variable. Otherwise, prediction fails (requires same set of levels for factors)
    for(i in factorvars) { 
      d_v_od[,i] = levadd(factor_var = i, d_v_od, d_od) 
    }
    
    pred_vars = c('CARRIER', 'YEAR', 'MONTH', 'DAY_OF_WEEK', 'DEP_TIME_BLK', 'ORIGIN_REL_CONG', 'DEST_REL_CONG')
    pred_vars = pred_vars[apply(d_od[pred_vars], MARGIN = 2, function(x) length(unique(x)) > 1)]
    
    stopifnot(identical(sort(levels(d_od$DEP_TIME_BLK)), sort(levels(d_v_od$DEP_TIME_BLK))))
    if(!identical(sort(levels(d_od$DEP_TIME_BLK)), sort(levels(d_v_od$DEP_TIME_BLK)))){
      cat('Check factor levels in', od, '\n')}
    
    
    if(length(pred_vars) > 0){
      
      # For xgboost, create model matrix with dummy variables for each level of each factor variable. + 0 Eliminates intercept
      d_od_train <- model.matrix(~ 0 + .  , data = d_od[pred_vars])
      d_od_label <- d_od$AIR_TIME
      dtrain = xgb.DMatrix(data = d_od_train, label = d_od_label)
      
      d_v_od_test <- model.matrix(~ 0 + .  , data = d_v_od[pred_vars])
      # Order identically to d_od_train
      d_v_od_test <- d_v_od_test[,colnames(d_od_train)]
      stopifnot(identical(colnames(d_od_train), colnames(d_v_od_test)))
      d_v_od_label <- d_v_od$AIR_TIME
      
      dtest = xgb.DMatrix(data = d_v_od_test, label = d_v_od_label)    
      
      m_xgb_fit <- xgboost(dtrain, params = xgb_params, 
                           nrounds = xgb_params$nrounds,
                           verbose = 0)
      
      # Get feature importance. Gain is the measure of importance
      imp <- xgb.importance(model = m_xgb_fit)
      write.csv(imp, file = file.path(saveloc, 'Models', paste0(od, '.csv')))
      top_feature = as.character(imp[1,'Feature'])
      top_feature_gain = as.character(round(imp[1,'Gain'], 5))
      
      # Get pseudo r2 by predicting on data set
      predy <- predict(m_xgb_fit, dtrain)
      r2 = summary(lm(predy ~ d_od$AIR_TIME))$r.squared
      
      # If have at least one validation flight for this carrier, calculate diagnostics.
      if(nrow(d_v_od) > 1){
        # Calculate Root mean squared error, and also Mean absolute error. RMSE is more indicative of large outliers.
        predx <- predict(m_xgb_fit, dtest)
        
        RMSE = sqrt( sum( ( predx - d_v_od$AIR_TIME ) ^ 2 ) / length(predx) )
        MAE =  mean ( abs( predx - d_v_od$AIR_TIME ) )
        
        # Save prediction values to flight level output
        d_v_od$Est_AIR_TIME = predx
        
        write.table(d_v_od,
                    row.names = F,
                    col.names = F,
                    file.path(saveloc, paste0('Flight_Level_', Analysis, ".csv")),
                    sep = ',',
                    append = T)
        
      } else {
        RMSE = MAE = NA
      }
      
      sumvals <- data.frame(O_D = od, N = nrow(d_od),
                            N_Carriers = length(unique(d_od$CARRIER)),
                            Obs_Mean = mean(d_od$AIR_TIME),
                            Obs_SD = sd(d_od$AIR_TIME),
                            N_valid = nrow(d_v_od), Obs_Mean_valid = mean(d_v_od$AIR_TIME), Obs_SD_valid = sd(d_v_od$AIR_TIME),
                            r2, 
                            RMSE,
                            MAE,
                            top_feature,
                            top_feature_gain,
                            Pred_vars = paste(pred_vars, collapse = " + "))
      
    } else {
      sumvals <- data.frame(O_D = od, N = nrow(d_od),
                            N_Carriers = length(unique(d_od$CARRIER)),
                            Obs_Mean = mean(d_od$AIR_TIME),
                            Obs_SD = sd(d_od$AIR_TIME),
                            N_valid = nrow(d_v_od), Obs_Mean_valid = mean(d_v_od$AIR_TIME), Obs_SD_valid = sd(d_v_od$AIR_TIME),
                            r2 = NA, 
                            RMSE = NA,
                            MAE = NA,
                            top_feature = NA,
                            top_feature_gain = NA,
                            Pred_vars = NA)
    }
    write.table(sumvals,
                row.names = F,
                col.names = F,
                file.path(saveloc, paste0(Analysis, ".csv")),
                sep = ',',
                append = T)
    
    rm(d_od, d_v_od, m_xgb_fit); gc()
    # mod
  } # end dopar loop
  
  stopCluster(cl); rm(cl, d_train, d_test, VALIDATION); gc()
  
  endtime = Sys.time() - starttime
  elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))
  cat('\n Completed all O-D pairs in', elapsed_time, '\n')
  
} # End loop over validation options ----