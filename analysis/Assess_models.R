# Assess model diagnostics on local

# Setup ----
library(tidyverse)
library(shinystan)

code_loc = '~/git/Wheels_Up'

sharedloc = '//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Code/'
sharedloc = '~/Wheels_Up_Temp'
resultsloc = file.path(sharedloc, 'home/rstudio/results')

dir(resultsloc)

# Model 01 Assess ---

load(file.path(resultsloc, 'M01_VB_Fitted.RData'))
load(file.path(resultsloc, 'M01_MCMC_Fitted.RData'))

launch_shinystan(m01_vb_fit)
launch_shinystan(m01_mcmc_fit)

# Easier to save parameter names from model fitting; can recreated by carrying out the same data prep steps here, if needed.

# Model 02 Assess ---

load(file.path(resultsloc, 'M02_VB_Fitted.RData'))
load(file.path(resultsloc, 'M02_MCMC_Fitted.RData'))

launch_shinystan(m02_vb_fit)
launch_shinystan(m02_mcmc_fit) # Many parameters poorly converged; need to look at model specification again. 
