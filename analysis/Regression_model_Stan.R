# Stan regression model structures

# Setup ----
codeloc = ifelse(grepl('Flynn', normalizePath('~/')),
                 "~/git/Wheels_Up",
                 "~/GitHub/Wheels_Up")

setwd(codeloc)
source(file.path(codeloc, 'utility', 'get_packages.R'))

library(tidyverse) # if this fails, run install.packages('tidyverse')
library(lme4) # Mixed effect multilevel models
library(brms) # for Bayesian multilevel models using Stan
library(sjPlot) # for some nice model output tables


sharedloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Data"

load(file.path(sharedloc, 'ASQP_2018.RData'))

# Analysis prep  ----

# subset to only flights with available air time, and create a log-transformed air time variable
d_18 = d_18 %>%
  filter(!is.na(AIR_TIME)) %>%
  mutate(lAIR_TIME = log(AIR_TIME))

# Modeling in Stan with BRMS ----
# https://paul-buerkner.github.io/brms/

# Simplest model. Take a small subset at first
d_samp = d_18 %>% sample_n(size = nrow(d_18)/100)

# 47 sec
system.time(
  m1 <- lm(lAIR_TIME ~ CARRIER + MONTH + DAY_OF_WEEK + ORIGIN + DEST,
         data = d_samp)
  )

# 256 sec
system.time(
  # Simple hierarchircal model by carrier as random intercept
  m2 <- lmer(lAIR_TIME ~ MONTH + DAY_OF_WEEK + ORIGIN + DEST +
               (1 | CARRIER),
             data = d_samp)
)


# Simple Bayesian linear
# gradient evaluation 0.578 sec
system.time(
  m3 <- brm(lAIR_TIME ~ MONTH + DAY_OF_WEEK + ORIGIN + DEST + CARRIER,
            data = d_samp, chains = 2, cores = 2)
)

save(list = c('m1', 'm2', 'm3'),
     file = file.path(sharedloc, paste0('Models_', Sys.Date(), '.RData')))
# Options to probably include: make sure save_ranef = T
# inits: inits	
#   inits can be a list of lists containing the initial values, 
#   or a function (or function name) generating initial values. 
#   The latter options are mainly implemented for internal testing.
# seed = 1234 # use the same seed to make outputs reproducible
# Give some priors based on lm fits

marginal_effects(m3)

# plot marginal effects for each predictor
plot(marginal_effects(m3), ask = FALSE)

# investigate model fit
loo(m3)
pp_check(m3)


