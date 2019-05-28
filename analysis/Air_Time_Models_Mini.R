# Modeling air time
# Modifying to work on one carrier at a time now

# TODO: 
# - save run time
# - Save model diagnostics
# - Consider O_D within Carrier random effects

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

load(file.path(sharedloc, 'ASQP_2018.RData'))

# Analysis prep  ----

# subset to only flights with available air time, create a log-transformed air time variable, and create unique origin-destination pair variable
d_18 = d_18 %>%
  filter(!is.na(AIR_TIME)) %>%
  mutate(lAIR_TIME = log(AIR_TIME),
         O_D = paste(ORIGIN, DEST, sep = "-"))

# Simplest model. Take a small subset at first
# 12 carriers to assess across the four years 
use_carriers = c('AA', 'AS', 'B6', 'DL', 'EV', 'F9', 'HA', 'NK', 'OO', 'UA', 'VX', 'WN')


# <<>><<>>
carrier = use_carriers[2] # set up for looping over carreirs
# <<>><<>>

d_samp = d_18 %>% 
  filter(CARRIER == carrier)
d_samp = d_samp %>%
  sample_n(size = nrow(d_samp)/100)

# Drop unused levels
d_samp = d_samp %>%
  mutate(CARRIER = as.factor(as.character(CARRIER)),
         O_D = as.factor(as.character(O_D)),
         MONTH = as.factor(formatC(MONTH, width = 2, flag = '0'))) %>%
  select(AIR_TIME, MONTH, DAY_OF_WEEK, CARRIER, O_D, DEP_TIME_BLK)


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

# With VB
starttime <- Sys.time()

m03_vb_fit <- vb(CompiledModel03,
                 data = AT_Data_ls)

endtime = Sys.time() - starttime
elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))
summary_m03_vb <- summary(m03_vb_fit)$summary

predictor_levels = colnames(X)
summary_m03_vb <- data.frame(Parameter = c('alpha', predictor_levels, 'sigma', paste('Flight', 1:nrow(X)), 'lp'),
                             summary_m03_vb)


save(list = c('m03_vb_fit', 'elapsed_time', 'summary_m03_vb', 'predictor_levels'), 
     file = file.path(resultsloc, paste0('M03_VB_', carrier,'_Fitted.RData')))

par_extract <- extract(m03_vb_fit) 

# par_df <- as.data.frame(m03_vb_fit) # Extract posterior samples for every parameter and every flight
# launch_shinystan(m03_vb_fit)

# Example visualization: DL SFO-JFK Monday Eve flights vs VX SFO-JFK Monday Eve flights
# Can do this on local 

inits_vb <- list(alpha = mean(par_extract$alpha),
                 beta = colMeans(par_extract$beta),
                 mu = colMeans(par_extract$mu),
                 sigma = mean(par_extract$sigma)
)

# With MCMC, using VB outputs as initial values

chain_num = 6
iter = 200
warmup = 100

init_ls = list()
for(l in 1:chain_num){
  init_ls[[l]] = inits_vb
}

starttime <- Sys.time()

m03_mcmc_fit <- sampling(CompiledModel03,
                         data = AT_Data_ls,
                         chains = chain_num,
                         iter = iter,
                         warmup = warmup,
                         init = init_ls,
                         sample_file = paste0('analysis/Stan_Models/Intermediate_M03_MCMC_', carrier,'.csv'))

endtime = Sys.time() - starttime
elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))
summary_m03_mcmc <- summary(m03_mcmc_fit)$summary

predictor_levels = colnames(X)
summary_m03_mcmc <- data.frame(Parameter = c('alpha', predictor_levels, 'sigma', paste('Flight', 1:nrow(X)), 'lp'),
                             summary_m03_mcmc)

save(list = c('m03_mcmc_fit', 'elapsed_time', 'summary_m03_mcmc', 'predictor_levels'), 
     file = file.path(resultsloc, paste0('M03_MCMC_', carrier,'_Fitted.RData')))

# Diagnostics
table(summary_m03_mcmc$Rhat >= 1.1)
table(summary_m03_mcmc$n_eff <= ( chain_num * (iter - warmup) ) / 10 )

# With MCMC, without initial values -- 35 minutes instead of 41 for WN. 18 min each for AA, so similar or even faster without initial values from VB.

chain_num = 6
iter = 200
warmup = 100

starttime <- Sys.time()

m03_mcmc_fit <- sampling(CompiledModel03,
                         data = AT_Data_ls,
                         chains = chain_num,
                         iter = iter,
                         warmup = warmup,
#                         init = init_ls,
                         sample_file = paste0('analysis/Stan_Models/Intermediate_M03_MCMC_', carrier,'.csv'))

endtime = Sys.time() - starttime
elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))
summary_m03_mcmc <- summary(m03_mcmc_fit)$summary

predictor_levels = colnames(X)
summary_m03_mcmc <- data.frame(Parameter = c('alpha', predictor_levels, 'sigma', paste('Flight', 1:nrow(X)), 'lp'),
                               summary_m03_mcmc)

save(list = c('m03_mcmc_fit', 'elapsed_time', 'summary_m03_mcmc', 'predictor_levels'), 
     file = file.path(resultsloc, paste0('M03_MCMC_', carrier,'_NoInits_Fitted.RData')))


# Testing gradient ----

m03_mcmc_fit <- sampling(CompiledModel03,
                         data = AT_Data_ls,
                         chains = chain_num,
                         iter = iter,
                         test_grad =T)
# M03, with interactions ----

# Set up data with interactions -- fails, cannot allocate memory

X = model.matrix(~ -1 # + O_D + MONTH + DAY_OF_WEEK + DEP_TIME_BLK 
                     + O_D:MONTH + O_D:DAY_OF_WEEK + O_D:DEP_TIME_BLK
                 , d_samp)
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

chain_num = 6
iter = 200
warmup = 100

starttime <- Sys.time()

m03_mcmc_fit <- sampling(CompiledModel03,
                         data = AT_Data_ls,
                         chains = chain_num,
                         iter = iter,
                         warmup = warmup,
                         #                         init = init_ls,
                         sample_file = paste0('analysis/Stan_Models/Intermediate_M03_MCMC_Interax_', carrier,'.csv'))

endtime = Sys.time() - starttime
elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))
summary_m03_mcmc <- summary(m03_mcmc_fit)$summary

predictor_levels = colnames(X)
summary_m03_mcmc <- data.frame(Parameter = c('alpha', predictor_levels, 'sigma', paste('Flight', 1:nrow(X)), 'lp'),
                               summary_m03_mcmc)

save(list = c('m03_mcmc_fit', 'elapsed_time', 'summary_m03_mcmc', 'predictor_levels'), 
     file = file.path(resultsloc, paste0('M03_MCMC_', carrier,'_NoInits_Interax_Fitted.RData')))

# Old, across multiple carriers ----

MULTICARRIER = F

if(!MULTICARRIER){
# Model 01: Carrier as fixed effect, no interactions ----

CompiledModel01 <- stan_model(file.path(stan_models, 'Air_Time_M01.stan')) # Will read compiled model if available

# Set up data
# M01:  5 categorical predictors leading to 913 dummy variables in a model matrix 
# lAIR_TIME ~ CARRIER + O_D + MONTH + DAY_OF_WEEK + DEP_TIME_BLK
X = model.matrix(~ -1 + CARRIER + O_D + MONTH + DAY_OF_WEEK + DEP_TIME_BLK,
                 d_samp)
y = as.vector(d_samp$lAIR_TIME)

AT_Data_ls = list(N = nrow(X),
                  y = y,
                  K = ncol(X),
                  X = X,
                  scale_beta = apply(X, 2, sd) * sd(y) * 2.5, 
                  scale_alpha = sd(y) * 10, # Broad prior for overall intercept
                  loc_sigma = sd(y),
                  use_y_rep = F,
                  use_log_lik = F)

# With VB
starttime <- Sys.time()

m01_vb_fit <- vb(CompiledModel01,
                 data = AT_Data_ls)

endtime = Sys.time() - starttime
elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))
summary_m01_vb <- summary(m01_vb_fit)$summary

save(c('m01_vb_fit', 'elapsed_time'), file = file.path(resultsloc, 'M01_VB_Fitted.RData'))

# Plot results...
colnames(X)
par_extract <- extract(m01_vb_fit) 
beta_extract <- as.data.frame(par_extract$beta)
names(beta_extract) = colnames(X)

par_df <- as.data.frame(m01_vb_fit) # Extract posterior samples for every parameter and every flight
# launch_shinystan(m01_vb_fit)

# Example visualization: DL SFO-JFK Monday Eve flights vs VX SFO-JFK Monday Eve flights
# Can do this on local 

inits_vb <- list(alpha = mean(par_extract$alpha),
                 beta = colMeans(par_extract$beta),
                 mu = colMeans(par_extract$mu),
                 sigma = mean(par_extract$sigma)
                 )

# With MCMC, using VB outputs as initial values

chain_num = 6
init_ls = list()
for(l in 1:chain_num){
  init_ls[[l]] = inits_vb
}

starttime <- Sys.time()

m01_mcmc_fit <- sampling(CompiledModel01,
                       data = AT_Data_ls,
                       chains = chain_num,
                       iter = 200,
                       warmup = 100,
                       init = init_ls,
                       sample_file = 'analysis/Stan_Models/Intermediate_TEST_m01_mcmc_fit.csv')

endtime = Sys.time() - starttime
elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))

save(c('m01_mcmc_fit', 'elapsed_time'), file = file.path(resultsloc, 'M01_MCMC_Fitted.RData'))


# Model 02: Carrier as random effect ----

CompiledModel02 <- stan_model(file.path(stan_models, 'Air_Time_M02.stan')) # Will read compiled model if available

# Set up data
# M02:  1 grouping variable and 4 categorical predictors leading to 913 dummy variables in a model matrix 
# lAIR_TIME ~ CARRIER + O_D + MONTH + DAY_OF_WEEK + DEP_TIME_BLK
X = model.matrix(~ O_D + MONTH + DAY_OF_WEEK + DEP_TIME_BLK,
                 d_samp)
y = as.vector(d_samp$lAIR_TIME)
Carrier = as.numeric(d_samp$CARRIER)
  
AT_Data_02_ls = list(N = nrow(X),
                     N_carrier = length(unique(Carrier)),
                     Carrier = Carrier,
                     y = y,
                     K = ncol(X),
                     X = X,
                     loc_sigma = sd(y))

# With VB

starttime <- Sys.time()

m02_vb_fit <- vb(CompiledModel02,
                 data = AT_Data_02_ls)

summary_m02_vb <- summary(m02_vb_fit)$summary

endtime = Sys.time() - starttime
elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))

save(c('m02_vb_fit', 'elapsed_time'), file = file.path(resultsloc, 'M02_VB_Fitted.RData'))

par_extract <- extract(m02_vb_fit) 
beta_extract <- as.data.frame(par_extract$beta) # Now with a separate beta for each carrier, pre-pended with 1, 2... to indicate carrier 1, 2...

par_df <- as.data.frame(m02_vb_fit) # Extract posterior samples for every parameter and every flight
# launch_shinystan(m02_vb_fit)

# Find out what parameters we want to extract for initial values to pass to MCMC
# N_carrier * K beta, N_carrier * K beta_raw, K gamma, 1 sigma, K tau
pnames <- table(gsub('\\[.+', '', names(par_df)))

# Beta and Beta_raw need to be processed into N_carrier rows and K columns. Output is an array.
b_inits <- apply(par_extract$beta, MARGIN = c(2,3), FUN = mean)
b_raw_inits <- apply(par_extract$beta_raw, MARGIN = c(2,3), FUN = mean)

inits_vb <- list(gamma = colMeans(par_extract$gamma),
                 tau = colMeans(par_extract$tau),
                 beta = b_inits,
                 beta_raw = b_raw_inits,
                 sigma = mean(par_extract$sigma)
                 )

# With MCMC, using VB outputs as initial values

chain_num = 6
init_ls = list()
for(l in 1:chain_num){
  init_ls[[l]] = inits_vb
}

starttime <- Sys.time()

m02_mcmc_fit <- sampling(CompiledModel02,
                         data = AT_Data_02_ls,
                         chains = chain_num,
                         iter = 200,
                         warmup = 100,
                         init = init_ls,
                         sample_file = 'analysis/Stan_Models/Intermediate_m02_mcmc_fit.csv')

endtime = Sys.time() - starttime
elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))

save(c('m02_mcmc_fit', 'elapsed_time'), file = file.path(resultsloc, 'M02_MCMC_Fitted.RData'))

} # End if multicarrier