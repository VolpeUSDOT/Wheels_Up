# Modeling air time
# OLS regression by carrier and O_D

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
  # carrier = use_carriers[7]
  
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
  
  mod_list = list()
  O_D = unique(d_samp$O_D)
  
  writeLines(c(""), file.path(resultsloc, paste(carrier, "log.txt", sep = "_")))    
  write.table(data.frame(O_D = '', N = '', Obs_Mean = '',
                         Obs_SD = '', r2 = '', 
                         top_sig_coef = '', top_sig_coef_eff = ''), 
              row.names = F, sep = ',',
              #quote = F,
              file.path(resultsloc, paste(carrier, "OLS_Summary.csv", sep = "_")))    
  
  mod_list <- foreach(od = O_D, .packages = 'dplyr') %dopar% {
    write.table(data.frame(Time = as.character(Sys.time()), Completed = as.character(od)),
                row.names = F,
                col.names = F,
                quote = F,
                file = file.path(resultsloc, paste(carrier, "log.txt", sep = "_")),
                append = T)
    # od = as.character(O_D[2])
    d_od = d_samp %>% filter(O_D == od)
    
    pred_vars = c('MONTH', 'DAY_OF_WEEK', 'DEP_TIME_BLK')
    
    pred_vars = pred_vars[apply(d_od[pred_vars], MARGIN = 2, function(x) length(unique(x)) > 1)]
    
    if(length(pred_vars) > 0){
      use_formula = as.formula(paste('AIR_TIME ~', paste(pred_vars, collapse = ' * ')))
      
      m_ols_fit_3interax <- lm(use_formula, data = d_od)
      
      use_formula_2x = as.formula(paste0('AIR_TIME ~ (', paste(pred_vars, collapse = ' + '), ')^2'))
      m_ols_fit_2interax <- lm(use_formula_2x, data = d_od)
      
      aic_compare <- AIC(m_ols_fit_3interax, m_ols_fit_2interax)
      
      m_ols_fit_interax <- get(c('m_ols_fit_3interax', 'm_ols_fit_2interax')[which.min(aic_compare$AIC)])
      
      # Summarize
      sm <- summary(m_ols_fit_interax)
      smc <- as.data.frame(sm$coefficients)
      sig_coef <- smc[smc[,4] <= 0.05,]
      num_coef <- nrow(smc)
      sig_coef_noint <- sig_coef[-grep('(Intercept)', rownames(sig_coef)),]
      num_sig_coef <- nrow(sig_coef_noint)
      top_sig_coef <- ifelse(num_sig_coef > 0, 
                             rownames(sig_coef_noint[which.max(abs(sig_coef_noint[,1])),]),
                             NA)
      top_sig_coef_eff <- ifelse(num_sig_coef > 0, 
                                 sig_coef_noint[which.max(abs(sig_coef_noint[,1])),'Estimate'],
                                 NA)
      
      sumvals <- data.frame(O_D = od, N = nrow(d_od),
                            Obs_Mean = mean(d_od$AIR_TIME),
                            Obs_SD = sd(d_od$AIR_TIME),
                            r2 = sm$r.squared, 
                            top_sig_coef,
                            top_sig_coef_eff)
      
    } else {
      sumvals <- data.frame(O_D = od, N = nrow(d_od),
                            Obs_Mean = mean(d_od$AIR_TIME),
                            Obs_SD = sd(d_od$AIR_TIME),
                            r2 = NA, 
                            top_sig_coef = NA,
                            top_sig_coef_eff = NA)
      m_ols_fit_interax = NA
    }
    write.table(sumvals,
                row.names = F,
                col.names = F,
                file = file.path(resultsloc, paste(carrier, "OLS_Summary.csv", sep = "_")),
                sep = ',',
                append = T)
    m_ols_fit_interax 
  } # end dopar loop
  stopCluster(cl); gc()
  
  endtime = Sys.time() - starttime
  elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))
  
  save(list = c('mod_list', 'elapsed_time', 'O_D'), 
       file = file.path(resultsloc, paste0('OLS_interax_', carrier,'_Full_Fitted.RData')))
  
  cat(rep('<<>>', 20), '\n Completed', carrier, 'in', elapsed_time, '\n\n\n')
} # End carrier loop ----
