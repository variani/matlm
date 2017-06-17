context("perm")

test_that("margial", {
  stopifnot(require(magrittr))
  stopifnot(require(dplyr))    
  
  seed <- 1
  N <- 50
  M <- 10
  L <- 10
  
  rho <- 0.9
  
  # unrelated
  simdat <- matlm_sim_randpred(seed = seed, N = N, M = M) 
  assoc <- matlm(simdat$form, simdat$dat, pred = simdat$pred, num_perm = L)

  min_pvals <- group_by(assoc$ptab, perm) %>% summarize(pval_min = min(pval)) %$% pval_min
  min_pval <- min(assoc$tab$pval)

  pval_perm_unrel <- (sum(min_pvals < min_pval) + 1) / (L + 1)

  # related
  simdat <- matlm_sim_randpred(seed = seed, N = N, M = M, rho = rho) 
  assoc <- matlm(simdat$form, simdat$dat, pred = simdat$pred, num_perm = L)

  min_pvals <- group_by(assoc$ptab, perm) %>% summarize(pval_min = min(pval)) %$% pval_min
  min_pval <- min(assoc$tab$pval)

  pval_perm_rel <- (sum(min_pvals < min_pval) + 1) / (L + 1)

  expect_true(pval_perm_unrel < pval_perm_rel)
})

