# Modeling air time
# Full year, one carrier at a time. more chains and iterations

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
stan_models <- file.path(codeloc, 'analysis', 'Stan_Models')

library(tidyverse) # if this fails, run install.packages('tidyverse')
library(foreach)
library(doParallel)

# Analysis prep  ----

# subset to only flights with available air time, create a log-transformed air time variable, and create unique origin-destination pair variable

# Full year model, by carrier
# 12 carriers to assess across the four years 
use_carriers = c('AA', 'AS', 'B6', 'DL', 'EV', 'F9', 'HA', 'NK', 'OO', 'UA', 'VX', 'WN')
SUBSAMPLE = 0

# Loop over carriers ----

for(carrier in use_carriers){ 
  # carrier = use_carriers[2]
  
  load(file.path(sharedloc, 'ASQP_2018.RData'))
  
  d_samp = d_18 %>%
    filter(!is.na(AIR_TIME) & CARRIER == carrier) %>%
    mutate(O_D = paste(ORIGIN, DEST, sep = "-"))

  # Subsample to test
  if(SUBSAMPLE > 0){
  d_samp = d_samp %>%
    sample_n(size = nrow(d_samp)*SUBSAMPLE)
  }
 
  # Drop unused levels
  d_samp = d_samp %>%
    mutate(CARRIER = as.factor(as.character(CARRIER)),
           O_D = as.factor(as.character(O_D)),
           MONTH = as.factor(formatC(MONTH, width = 2, flag = '0'))) %>%
    select(AIR_TIME, MONTH, DAY_OF_WEEK, DEP_TIME_BLK, O_D)
  
  # OLS regression, multicore over O_D pairs: Within carrier model, no interactions
  rm(d_18); gc()
  system('free -g')# Print free memory in Gb
  
  starttime <- Sys.time()
  
  # Start parallel loop over O_D pairs ----
  avail.cores <- parallel::detectCores()
  cl <- makeCluster(avail.cores) 
  registerDoParallel(cl)
  
  foreach(od = unique(d_samp$O_D), .packages = 'dplyr') %dopar% {
    # od = as.character(d_samp$O_D[1])
    d_od = d_samp %>% filter(O_D == od)
    
    m_ols_fit <- lm(AIR_TIME ~ MONTH + DAY_OF_WEEK + DEP_TIME_BLK,
                     data = d_od
    )
    
    m_ols_fit_interax <- lm(AIR_TIME ~ MONTH * DAY_OF_WEEK * DEP_TIME_BLK,
                    data = d_od
    )
    
    # m_ols_all_fit <- lm(AIR_TIME ~ MONTH + DAY_OF_WEEK + DEP_TIME_BLK + O_D,
    #                 data = d_samp
    # )
    # 
    # m_ols_all_fit_interax <- lm(AIR_TIME ~ MONTH * DAY_OF_WEEK + DEP_TIME_BLK + O_D +
    #                       MONTH:O_D + DAY_OF_WEEK:O_D,
    #                     data = d_samp
    #)
    
    # Predict
    pred_data <- data.frame(MONTH = as.factor(rep('07', 3)), 
                            DAY_OF_WEEK = as.factor(c(2,3,4)),
                            DEP_TIME_BLK = as.factor('0800-0859'),
                            O_D = as.factor('DCA-SEA'))
    
    #predict(m_ols_all_fit, pred_data)
    predict(m_ols_fit, pred_data)
    predict(m_ols_fit_interax, pred_data)
    
    d_od %>%
      filter(MONTH == '07' &
             DAY_OF_WEEK %in% c(2, 3, 4) &
             DEP_TIME_BLK == '0800-0859')  %>%
      summarize(mean(AIR_TIME, na.rm = T))
    
  }

  endtime = Sys.time() - starttime
  elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))
  summary_m03_mcmc <- summary(m03_mcmc_fit)
  
  summary_m03_mcmc <- data.frame(Parameter = c('alpha', predictor_levels, 'sigma', paste('Flight', 1:nrow(X)), 'lp'),
                                 summary_m03_mcmc)
  
  save(list = c('m03_mcmc_fit', 'elapsed_time', 'summary_m03_mcmc', 'predictor_levels'), 
       file = file.path(resultsloc, paste0('M03_MCMC_', carrier,'_Full_Fitted.RData')))
  
  # Diagnostics
  table(summary_m03_mcmc$Rhat >= 1.1)
  table(summary_m03_mcmc$n_eff <= ( chain_num * (iter - warmup) ) / 10 )

  rm(m03_mcmc_fit, summary_m03_mcmc, AT_Data_ls); gc() # Force memory cleanup
  
  cat(rep('<<>>', 20), '\n Completed', carrier, 'in', elapsed_time, '\n\n\n')
} # End carrier loop ----
