# Modeling air time
# Full year, two carriers, more chains and iterations

# TODO: 
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

# Full year of data for two carriers
d_samp = d_18 %>% 
  filter(CARRIER == 'DL' | CARRIER == 'VX')

# Drop unused levels
d_samp = d_samp %>%
  mutate(CARRIER = as.factor(as.character(CARRIER)),
         O_D = as.factor(as.character(O_D)),
         MONTH = as.factor(formatC(MONTH, width = 2, flag = '0'))) %>%
  select(lAIR_TIME, MONTH, DAY_OF_WEEK, CARRIER, O_D, DEP_TIME_BLK)


# # Model 02: Carrier as random effect ----

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

save(c('m02_vb_fit', 'elapsed_time'), file = file.path(resultsloc, 'M02__Full2C_VB_Fitted.RData'))

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
                         iter = 500,
                         warmup = 150,
                         init = init_ls,
                         sample_file = 'analysis/Stan_Models/Intermediate__Full2C_m02_mcmc_fit.csv')

endtime = Sys.time() - starttime
elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))

save(c('m02_mcmc_fit', 'elapsed_time'), file = file.path(resultsloc, 'M02__Full2C_MCMC_Fitted.RData'))
