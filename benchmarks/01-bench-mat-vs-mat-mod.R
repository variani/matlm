### inc
library(microbenchmark)
library(ggplot2)

theme_set(theme_minimal())

### simulate data
N <- 1e2
M <- 5e2

simdat <- matlm_sim_randpred(seed = 1, N = N, M = M)

system.time(assoc_matlm <- matlm(simdat$form, simdat$dat, pred = simdat$pred))

system.time(assoc_matlm_mod <- matlm_mod(simdat$form, simdat$dat, pred = simdat$pred))
