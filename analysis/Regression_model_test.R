# Small tests of regression model structures

# Setup ----
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

p1 <- ggplot(d_18, aes(x = AIR_TIME)) +
  geom_histogram(aes(y=..density..)) 

p1 # skew right

p1 + scale_x_continuous(trans = 'log') # log normal should work nicely.

p2 <- ggplot(d_18, aes(x = lAIR_TIME)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1) 
p2 + stat_function(fun = dnorm, args = list(mean = mean(d_18$lAIR_TIME),
                                         sd = sd(d_18$lAIR_TIME)))

# Simplest model. Take a small subset at first
d_samp = d_18 %>% sample_n(size = nrow(d_18)/100)

m1 <- lm(lAIR_TIME ~ CARRIER + MONTH + DAY_OF_WEEK + ORIGIN + DEST,
         data = d_samp)

op = par(no.readonly = T)

par(mfrow = c(1,2))
plot(m1, which = 1:2) # One cluster of short flights poorly represented by this model, but overall very good already.
summary(m1)$adj.r.squared

par(op)

tab_model(m1)

# Simple hierarchircal model by carrier as random intercept
m2 <- lmer(lAIR_TIME ~ MONTH + DAY_OF_WEEK + ORIGIN + DEST +
             (1 | CARRIER),
         data = d_samp)

summary(m2)
tab_model(m2)

plot(resid(m2) ~ fitted(m2)) # same cluster of poor fit for a few flights, otherwise good

AIC(m1, m2) # not gaining anything with this model

# Simple Bayesian multilevel

m3 <- brm(lAIR_TIME ~ MONTH + DAY_OF_WEEK + ORIGIN + DEST + (1|CARRIER),
          data = d_samp)

marginal_effects(m3)

bprior1 <- prior(student_t(5,0,10), class = b) + prior(cauchy(0,2), class = sd)
fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient), 
            data = epilepsy, family = poisson(), prior = bprior1)
