# Modeling air time
# OLS regression by O_D and Carrier
# For each unique O_D, fit a model with Carrier as a variable
# Making congestion at origin and destination an optional variable 
# Also saving observed and predicted flight times, and all model coefficients, to flat files for each analysis. 

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
CONGESTION = F # Set to T to include congestion variables, F to exclude (use schedule variables only)
VALIDATION_opts = c('Internal', '2019') # VALIDATION = 'Internal' # Two options for validation. One, use a training/test dataset and validate internally 
for(VALIDATION in VALIDATION_opts){
  # <<><<><<>
  
  Analysis = paste0('1O-D_Crossyear2_Validate_', VALIDATION)
  
  saveloc = file.path(resultsloc, Analysis); system(paste('mkdir -p', saveloc))
  system(paste('mkdir -p', file.path(saveloc, 'Models'))) # Directory for model coefficients
  
  library(tidyverse) 
  library(foreach)
  library(doParallel)
  
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
           ORIGIN_N_flights = N_flights) %>%
    filter(!is.na(ORIGIN_REL_CONG))
  d_train <- d_train %>% 
    left_join(Congestion_Destination, by = c('DEST', 'YEAR', 'MONTH', 'DAY_OF_WEEK', 'ARR_TIME_BLK')) %>%
    rename(DEST_REL_CONG = Relative_Congestion,
           DEST_N_flights = N_flights) %>%
    filter(!is.na(DEST_REL_CONG))
  
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
           ORIGIN_N_flights = N_flights) %>%
    filter(!is.na(ORIGIN_REL_CONG))
  
  d_test <- d_test %>% 
    left_join(Congestion_Destination, by = c('DEST', 'YEAR', 'MONTH', 'DAY_OF_WEEK', 'ARR_TIME_BLK')) %>%
    rename(DEST_REL_CONG = Relative_Congestion,
           DEST_N_flights = N_flights) %>%
    filter(!is.na(DEST_REL_CONG))
  
  # Function to save a minimal model object for predict()
  cleanModel1 = function(cm) {
    cm$y = NULL
    cm$model = NULL
    cm$residuals = NULL
    cm$fitted.values = NULL
    cm$effects = NULL
    cm$qr = NULL
    cm$linear.predictors = NULL
    cm$weights = NULL
    cm$prior.weights = NULL
    cm$data = NULL
    cm
  }
  # Funtion to match factors levesl between training and validation data sets. Add empty factor levels if necessary 
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
  
  # Start parallel loop over O_D pairs ----
  # Consider forking instead
  # cl <- parallel::makeForkCluster(2)
  
  avail.cores <- parallel::detectCores()
  cl <- makeCluster(avail.cores) 
  registerDoParallel(cl)
  
  if(!file.exists(file.path(saveloc, "1O_D_log.txt"))){
    writeLines(c(""), file.path(saveloc, "1O_D_log.txt"))
    # Summary table
    write.table(data.frame(O_D = '', N = '', N_carriers = '', Obs_Mean = '',
                           Obs_SD = '', 
                           N_valid = '', Obs_Mean_valid = '', Obs_SD_valid = '',
                           r2 = '', RMSE = '', MAE = '',
                           top_sig_coef = '', top_sig_coef_eff = '', Yr = '', Model = ''
    ), 
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
  # mod_list = list()
  
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
    
    # Drop unused levels
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
    
    # Add empty factor levels to validation sample if necessary. Otherwise, prediction fails (requires same set of levels for factors)
    vars = sapply(d_v_od, class)
    factorvars = names(vars[which(vars == 'factor')])
    
    for(i in factorvars) { 
      d_v_od[,i] = levadd(factor_var = i, d_v_od, d_od) 
    }
    
    # Separating out congestion variables; do not include these in the 2x interaction terms, since this leads to overfitting for many flights
    cong_vars = c('ORIGIN_REL_CONG', 'DEST_REL_CONG')
    
    pred_vars = c('CARRIER', 'YEAR', 'MONTH', 'DAY_OF_WEEK', 'DEP_TIME_BLK')
    pred_vars = pred_vars[apply(d_od[pred_vars], MARGIN = 2, function(x) length(unique(x)) > 1)]
    
    pred_vars_noyr = c('CARRIER', 'MONTH', 'DAY_OF_WEEK', 'DEP_TIME_BLK')
    pred_vars_noyr = pred_vars_noyr[apply(d_od[pred_vars_noyr], MARGIN = 2, function(x) length(unique(x)) > 1)]
    
    # Conversely, can only use validation data for observations with factor levels also present in training data. 
    # If all levels present in validataion data are also present in training data, no change is made
    vars = sapply(d_od, class)
    factorvars = names(vars[which(vars == 'factor')])
    
    for(i in factorvars) { 
      d_od[,i] = levadd(factor_var = i, d_od, d_v_od) 
      zerolevs <- which(table(d_od[,i]) == 0)
      # Drop any observations in validation set which are in these
      dvi = as.character(unclass(d_v_od[,i])[[1]]) %in% names(zerolevs)
      d_v_od = d_v_od[!dvi,]
    }
    
    if(length(pred_vars) > 0){
      if(CONGESTION){
        use_formula = as.formula(paste0('AIR_TIME ~ (', paste(pred_vars, collapse = ' + '), ')^2 + (',
                                        paste(cong_vars, collapse = ' + '), ')^2'))
      } else {
        use_formula = as.formula(paste0('AIR_TIME ~ (', paste(pred_vars, collapse = ' + '), ')^2'))
      }
      
      m_ols_fit_interax <- lm(use_formula, data = d_od)
      
      if(CONGESTION){
        use_formula = as.formula(paste0('AIR_TIME ~ ', paste(c(pred_vars, cong_vars), collapse = ' + ')))
      } else {
        use_formula = as.formula(paste0('AIR_TIME ~ ', paste(pred_vars, collapse = ' + ')))
      }
      
      m_ols_fit_no_interax <- lm(use_formula, data = d_od)
      
      # Eliminate any interactions or variables which are not important
      # summary.aov(m_ols_fit_interax)
      # m_ols_step <- step(m_ols_fit_interax, direction = 'both')
      # summary.aov(m_ols_step)
      
      # Assess models without year
      if(length(pred_vars_noyr) > 0){
        if(CONGESTION){
          use_formula_noyr = as.formula(paste0('AIR_TIME ~ (', paste(pred_vars_noyr, collapse = ' + '), ')^2 + (',
                                               paste(cong_vars, collapse = ' + '), ')^2'))
        } else {
          use_formula_noyr = as.formula(paste0('AIR_TIME ~ (', paste(pred_vars_noyr, collapse = ' + '), ')^2'))
        }
        
        m_ols_fit_noyr <- lm(use_formula_noyr, data = d_od)
        
        if(CONGESTION){
          use_formula_noyr = as.formula(paste0('AIR_TIME ~ ', paste(c(pred_vars_noyr, cong_vars), collapse = ' + ')))
        } else {
          use_formula_noyr = as.formula(paste0('AIR_TIME ~ ', paste(pred_vars_noyr, collapse = ' + ')))
        }
        
        m_ols_fit_noyr_no_interax <- lm(use_formula_noyr, data = d_od)
        
        # m_ols_step_noyr <- step(m_ols_fit_noyr, direction = 'both')
        # summary.aov(m_ols_step_noyr)
        
        aic_compare <- AIC(m_ols_fit_interax, m_ols_fit_noyr, m_ols_fit_no_interax, m_ols_fit_noyr_no_interax)
        
        m_ols_best_fit <- get(rownames(aic_compare)[which.min(aic_compare$AIC)])
        
      } else {
        aic_compare <- AIC(m_ols_fit_interax, m_ols_fit_no_interax)
        
        m_ols_best_fit <- get(rownames(aic_compare)[which.min(aic_compare$AIC)])
        
      }
      
      # Summarize
      sm <- summary(m_ols_best_fit)
      smc <- as.data.frame(sm$coefficients)
      
      # Output coefficients 
      write.csv(smc, file = file.path(saveloc, 'Models', paste0(od, '.csv')))
      
      sig_coef <- smc[smc[,4] <= 0.05,]
      num_coef <- nrow(smc)
      
      if('(Intercept)' %in% rownames(sig_coef)){
        sig_coef_noint  <- sig_coef[-grep('(Intercept)', rownames(sig_coef)),]
      } else {
        sig_coef_noint <- sig_coef
      }
      num_sig_coef <- nrow(sig_coef_noint)
      top_sig_coef <- ifelse(num_sig_coef > 0, 
                             rownames(sig_coef_noint[which.max(abs(sig_coef_noint[,1])),]),
                             NA)
      top_sig_coef_eff <- ifelse(num_sig_coef > 0, 
                                 sig_coef_noint[which.max(abs(sig_coef_noint[,1])),'Estimate'],
                                 NA)
      # If have at least one validation flight for this carrier, calculate diagnostics.
      if(nrow(d_v_od) > 1){
        # Calculate Root mean squared error, and also Mean absolute error. RMSE is more indicative of large outliers.
        predx <- predict(m_ols_best_fit, d_v_od)
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
      
      # Check if year is included in best model
      Yr = sum(grepl('YEAR', attr(terms(m_ols_best_fit), 'predvars'))) == 1
      
      sumvals <- data.frame(O_D = od, N = nrow(d_od),
                            N_Carriers = length(unique(d_od$CARRIER)),
                            Obs_Mean = mean(d_od$AIR_TIME),
                            Obs_SD = sd(d_od$AIR_TIME),
                            N_valid = nrow(d_v_od), Obs_Mean_valid = mean(d_v_od$AIR_TIME), Obs_SD_valid = sd(d_v_od$AIR_TIME),
                            r2 = sm$r.squared, 
                            RMSE,
                            MAE,
                            top_sig_coef,
                            top_sig_coef_eff,
                            Yr,
                            Model = as.character(terms(m_ols_best_fit))[3])
      
    } else {
      sumvals <- data.frame(O_D = od, N = nrow(d_od),
                            N_Carriers = length(unique(d_od$CARRIER)),
                            Obs_Mean = mean(d_od$AIR_TIME),
                            Obs_SD = sd(d_od$AIR_TIME),
                            N_valid = nrow(d_v_od), Obs_Mean_valid = mean(d_v_od$AIR_TIME), Obs_SD_valid = sd(d_v_od$AIR_TIME),
                            r2 = NA, 
                            RMSE = NA,
                            MAE = NA,
                            top_sig_coef = NA,
                            top_sig_coef_eff = NA,
                            Yr = NA,
                            Model = NA)
      m_ols_best_fit = NA
    }
    write.table(sumvals,
                row.names = F,
                col.names = F,
                file.path(saveloc, paste0(Analysis, ".csv")),
                sep = ',',
                append = T)
    
    # m_ols_best_fit # Too large: 
    # Previously, was trying to save minimal output for predict: summary, summary.aov, and clean model for predict()
    # list(summary = summary(m_ols_best_fit),
    #      summary.aov = summary.aov(m_ols_best_fit),
    #      mod = cleanModel1(m_ols_best_fit))
    
    # mod = cleanModel1(m_ols_best_fit) # Even more minimal, just for predict(). Still runs out of RAM. So now we do all validation/predition inside the model run loop.
    
    rm(d_od, d_v_od, m_ols_best_fit, m_ols_fit_interax, m_ols_fit_noyr); gc()
    # mod
  } # end dopar loop
  
  stopCluster(cl); rm(cl, d_train, d_test, VALIDATION); gc()
  
  endtime = Sys.time() - starttime
  elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))
  cat('\n Completed all O-D pairs in', elapsed_time, '\n')
  
} # End loop over validation options ----