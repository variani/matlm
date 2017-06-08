context("matlm")

test_that("matlm: basics", {
  N <- 50
  M <- 5
  
  simdat <- matlm_sim_randpred(seed = 1, N = N, M = M) 
  
  mod <- matlm(simdat$form, simdat$dat, pred = simdat$pred)
  
})
