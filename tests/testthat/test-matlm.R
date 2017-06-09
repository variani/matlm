context("matlm")

test_that("matlm: basics", {
  N <- 1000
  M <- 5
  
  simdat <- matlm_sim_randpred(seed = 10, N = N, M = M) 
  
  assoc <- matlm(simdat$form, simdat$dat, pred = simdat$pred)
  assoc_mod <- matlm_mod(simdat$form, simdat$dat, pred = simdat$pred)
  
  expect_equal(assoc_mod$tab$pval, assoc$tab$pval, tol = 1e-3)
})
