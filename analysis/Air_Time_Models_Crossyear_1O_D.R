# Modeling air time
# OLS regression by O_D and Carrier
# For each unique O_D, fit a model with Carrier as a variable

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
Analysis = 'OLS_Crossyear_1O_D'
saveloc = file.path(resultsloc, Analysis); system(paste('mkdir -p', saveloc))

library(tidyverse) # if this fails, run install.packages('tidyverse')
library(foreach)
library(doParallel)

# Analysis prep  ----

# subset to only flights with available air time, and create unique origin-destination pair variable

# Full year model, by carrier
# 12 carriers to assess across the four years -- in order of size   
# Only keep O_D which were also present in 2018

use_carriers = c('AA', 'AS', 'B6', 'DL', 'EV', 'F9', 'HA', 'NK', 'OO', 'UA', 'VX', 'WN')

load(file.path(sharedloc, 'ASQP_2018.RData'))
load(file.path(sharedloc, 'ASQP_2017.RData'))
load(file.path(sharedloc, 'ASQP_2016.RData'))
load(file.path(sharedloc, 'ASQP_2015.RData'))

d_crossyear <- full_join(d_18, d_17)
d_crossyear <- full_join(d_crossyear, d_16)
d_crossyear <- full_join(d_crossyear, d_15)

d_crossyear <- d_crossyear %>%
  filter(!is.na(AIR_TIME) & CARRIER %in% use_carriers) %>%
  mutate(O_D = paste(ORIGIN, DEST, sep = "-"),
         O_D_18 = ifelse(YEAR == '2018', paste(ORIGIN, DEST, sep = "-"), NA),
         YEAR = as.factor(YEAR)) %>%
  filter(O_D %in% O_D_18) %>%
  select(YEAR, MONTH, DAY_OF_WEEK, CARRIER, ORIGIN, DEST, O_D,
         DEP_TIME_HR, DEP_TIME_MIN, DEP_TIME_BLK,
         ARR_TIME_HR, ARR_TIME_MIN, ARR_TIME_BLK,
         CANCELLED, CANCELLATION_CODE, DIVERTED,
         AIR_TIME)

# format(object.size(d_crossyear), 'Gb')
rm(d_15, d_16, d_17, d_18); gc()
system("free -g | awk '{print $6}'") # Print available memory in Gb

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

# Loop over O_D ----

O_D = unique(d_crossyear$O_D) # 5,101 models
starttime <- Sys.time()

# Start parallel loop over O_D pairs ----
# Consider forking instead
# cl <- parallel::makeForkCluster(2)

avail.cores <- parallel::detectCores()
cl <- makeCluster(avail.cores) 
registerDoParallel(cl)

if(!file.exists(file.path(saveloc, "1O_D_log.txt"))){
  writeLines(c(""), file.path(saveloc, "1O_D_log.txt"))    
  write.table(data.frame(O_D = '', N = '', 
                         N_Carriers = '',
                         Obs_Mean = '',
                         Obs_SD = '', r2 = '', 
                         top_sig_coef = '', top_sig_coef_eff = '', Yr = ''), 
              row.names = F, sep = ',',
              file = file.path(saveloc, "1O_D_Crossyear_OLS_Summary.csv") )
  
} else {
  # Skip completed models
  completed <- read.table(file.path(saveloc, '1O_D_log.txt'), header = F)
  O_D <- O_D[!O_D %in% completed[,3]]  
}
# mod_list = list()

foreach(od = O_D, .packages = 'dplyr') %dopar% {
  # od = as.character(O_D[1])
  freem = paste(system("free -g | awk '{print $7}'", intern = T), collapse = '')
  
  write.table(data.frame(Time = as.character(Sys.time()), Completed = as.character(od), Free = freem),
              row.names = F,
              col.names = F,
              quote = F,
              file = file.path(saveloc, '1O_D_log.txt'),
              append = T)
  
  d_samp = d_crossyear %>%
    filter(O_D == od)
  
  # Drop unused levels
  d_samp = d_samp %>%
    mutate(CARRIER = as.factor(as.character(CARRIER)),
           O_D = as.factor(as.character(O_D)),
           MONTH = as.factor(formatC(MONTH, width = 2, flag = '0'))) %>%
    select(CARRIER, AIR_TIME, YEAR, MONTH, DAY_OF_WEEK, DEP_TIME_BLK)
  
  pred_vars = c('CARRIER', 'YEAR', 'MONTH', 'DAY_OF_WEEK', 'DEP_TIME_BLK')
  pred_vars = pred_vars[apply(d_samp[pred_vars], MARGIN = 2, function(x) length(unique(x)) > 1)]
  
  pred_vars_noyr = c('CARRIER', 'MONTH', 'DAY_OF_WEEK', 'DEP_TIME_BLK')
  pred_vars_noyr = pred_vars_noyr[apply(d_samp[pred_vars_noyr], MARGIN = 2, function(x) length(unique(x)) > 1)]
  
  if(length(pred_vars) > 0){
    use_formula = as.formula(paste0('AIR_TIME ~ (', paste(pred_vars, collapse = ' + '), ')^2'))
    
    m_ols_fit_interax <- lm(use_formula, data = d_samp)
    
    # Eliminate any interactions or variables which are not important
    # summary.aov(m_ols_fit_interax)
    # m_ols_step <- step(m_ols_fit_interax, direction = 'both')
    # summary.aov(m_ols_step)
    
    # Assess models without year
    if(length(pred_vars_noyr) > 0){
      use_formula_noyr = as.formula(paste0('AIR_TIME ~ (', paste(pred_vars_noyr, collapse = ' + '), ')^2'))
      m_ols_fit_noyr <- lm(use_formula_noyr, data = d_samp)
      # m_ols_step_noyr <- step(m_ols_fit_noyr, direction = 'both')
      # summary.aov(m_ols_step_noyr)
      
      aic_compare <- AIC(m_ols_fit_interax, m_ols_fit_noyr)
      
      m_ols_best_fit <- get(c('m_ols_fit_interax', 'm_ols_fit_noyr')[which.min(aic_compare$AIC)])
      
    } else {
      m_ols_best_fit <- m_ols_fit_interax
    }
    
    # Summarize
    sm <- summary(m_ols_best_fit)
    smc <- as.data.frame(sm$coefficients)
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
    
    # Check if year is included in best model
    Yr = sum(grepl('YEAR', attr(terms(m_ols_best_fit), 'predvars'))) == 1
    
    sumvals <- data.frame(O_D = od, N = nrow(d_samp),
                          N_Carriers = length(unique(d_samp$CARRIER)),
                          Obs_Mean = mean(d_samp$AIR_TIME),
                          Obs_SD = sd(d_samp$AIR_TIME),
                          r2 = sm$r.squared, 
                          top_sig_coef,
                          top_sig_coef_eff,
                          Yr)
    
  } else {
    sumvals <- data.frame(O_D = od, N = nrow(d_samp),
                          N_Carriers = length(unique(d_samp$CARRIER)),
                          Obs_Mean = mean(d_samp$AIR_TIME),
                          Obs_SD = sd(d_samp$AIR_TIME),
                          r2 = NA, 
                          top_sig_coef = NA,
                          top_sig_coef_eff = NA,
                          Yr = NA)
    m_ols_best_fit = NA
  }
  write.table(sumvals,
              row.names = F,
              col.names = F,
              file = file.path(saveloc, "1O_D_Crossyear_OLS_Summary.csv"),
              sep = ',',
              append = T)
  
  # m_ols_best_fit # Too large: 
  # Save minimal output for predict: summary, summary.aov, and clean model for predict()
  # list(summary = summary(m_ols_best_fit),
  #      summary.aov = summary.aov(m_ols_best_fit),
  #      mod = cleanModel1(m_ols_best_fit))
  
  # mod = cleanModel1(m_ols_best_fit) # Even more minimal, just for predict()
  
  rm(d_samp, m_ols_best_fit, m_ols_fit_interax, m_ols_fit_noyr); gc()
  # mod
} # end dopar loop

stopCluster(cl); rm(cl, d_samp); gc()

endtime = Sys.time() - starttime
elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))

# cat('\n Saving', carrier, '\n')
# 
# save(list = c('mod_list', 'elapsed_time', 'O_D'), 
#      file = file.path(saveloc, paste0('OLS_interax_', carrier,'_Crossyear_Fitted.RData')))
