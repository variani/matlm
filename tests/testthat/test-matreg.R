context("matreg")

test_that("matreg0", {
  N <- 100
  M <- 5
  
  simdat <- matlm_sim_randpred(seed = 10, N = N, M = M) 
  
  assoc <- matreg0(simdat$dat$y, simdat$pred)
  assoc_mod <- matlm_mod(simdat$form, simdat$dat, pred = simdat$pred)
  
  expect_equal(assoc_mod$tab$pval, assoc$pval, tol = 1e-3)
})

