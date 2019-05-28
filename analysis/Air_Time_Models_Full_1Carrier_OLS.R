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
  
  mod_list = list()
  O_D = unique(d_samp$O_D)
  
  writeLines(c(""), file.path(resultsloc, paste(carrier, "log.txt", sep = "_")))    
  
  mod_list <- foreach(od = O_D, .packages = 'dplyr') %dopar% {
    
    # od = as.character(d_samp$O_D[44])
    d_od = d_samp %>% filter(O_D == od)
    
    pred_vars = c('MONTH', 'DAY_OF_WEEK', 'DEP_TIME_BLK')
    
    pred_vars = pred_vars[apply(d_od[pred_vars], MARGIN = 2, function(x) length(unique(x)) > 1)]
    
    use_formula = as.formula(paste('AIR_TIME ~', paste(pred_vars, collapse = ' * ')))
    
    m_ols_fit_interax <- lm(use_formula, data = d_od)
    
    # writeLines(paste(as.character(Sys.time()), as.character(od), " completed"), 
    #            con = file.path(resultsloc, paste(carrier, "log.txt", sep = "_")),
    #            sep = '\n')
    write.table(data.frame(Time = as.character(Sys.time()), Completed = as.character(od)),
                row.names = F,
                col.names = F,
                quote = F,
                file = file.path(resultsloc, paste(carrier, "log.txt", sep = "_")),
                append = T)
    
    m_ols_fit_interax 
  }

  stopCluster(cl); gc()
  
  endtime = Sys.time() - starttime
  elapsed_time = paste(round(endtime, 2), attr(endtime, 'units'))

  save(list = c('mod_list', 'elapsed_time', 'O_D'), 
       file = file.path(resultsloc, paste0('OLS_interax_', carrier,'_Full_Fitted.RData')))
  
  rm(m_ols_fit_interax, mod_list); gc() # Force memory cleanup
  
  cat(rep('<<>>', 20), '\n Completed', carrier, 'in', elapsed_time, '\n\n\n')
} # End carrier loop ----
