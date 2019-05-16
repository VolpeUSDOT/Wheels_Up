library(brms)
bprior1 <- prior(student_t(5,0,10), class = b) + 
  prior(cauchy(0,2), class = sd)
fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
            data = epilepsy, family = poisson(), prior = bprior1)

summary(fit1)


