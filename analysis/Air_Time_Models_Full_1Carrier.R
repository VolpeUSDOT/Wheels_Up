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
library(rstan) # for Bayesian multilevel models using Stan
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

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
  
  # Model 03: Within carrier model, no interactions
  
  CompiledModel03 <- stan_model(file.path(stan_models, 'Air_Time_M03.stan')) # Will read compiled model if available
  
  # Set up data
  # M03:  5 categorical predictors leading to 913 dummy variables in a model matrix 
  # lAIR_TIME ~ CARRIER + O_D + MONTH + DAY_OF_WEEK + DEP_TIME_BLK
  X = model.matrix(~ -1 + O_D + MONTH + DAY_OF_WEEK + DEP_TIME_BLK,
                   d_samp)
  y = as.vector(d_samp$AIR_TIME)
  
  AT_Data_ls = list(N = nrow(X),
                    y = y,
                    K = ncol(X),
                    X = X,
                    scale_beta = apply(X, 2, sd) * sd(y) * 2.5, 
                    scale_alpha = sd(y) * 10, # Broad prior for overall intercept
                    loc_sigma = sd(y),
                    use_y_rep = F,
                    use_log_lik = F)
  
  predictor_levels = colnames(X)
  
  rm(d_18, d_samp, X); gc()
  system('free -g')# Print free memory in Gb
  
  chain_num = 2
  iter = 300
  warmup = 150
  
  starttime <- Sys.time()
  
  m03_mcmc_fit <- sampling(CompiledModel03,
                           data = AT_Data_ls,
                           chains = chain_num,
                           iter = iter,
                           warmup = warmup,
                           sample_file = paste0('analysis/Stan_Models/Intermediate_M03_MCMC_', carrier,'.csv'))
  
  endtime = Sys.time() - starttime
  elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))
  summary_m03_mcmc <- summary(m03_mcmc_fit)$summary
  
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
