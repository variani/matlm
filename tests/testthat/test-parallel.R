context("parallel")

test_that("2 cores (matrix)", {
  N <- 10
  M <- 5
  
  simdat <- matlm_sim_randpred(seed = 1, N = N, M = M) 
  
  assoc1 <- matlm(simdat$form, simdat$dat, pred = simdat$pred, num_batches = 2)
  assoc2 <- matlm(simdat$form, simdat$dat, pred = simdat$pred, num_batches = 2, cores = 2)
 
  expect_equal(assoc1$tab$pval, assoc2$tab$pval, tol = 1e-10)
})

test_that("2 cores (bigmemory)", {
  N <- 10
  M <- 5
  
  simdat <- matlm_sim_randpred(seed = 1, N = N, M = M) 

  pred <- simdat$pred
  bpred <- as.big.matrix(pred)
    
  assoc1 <- matlm(simdat$form, simdat$dat, pred = bpred, num_batches = 2)
  #assoc2 <- matlm(simdat$form, simdat$dat, pred = bpred, num_batches = 2, cores = 2)
 
  #expect_equal(assoc1$tab$pval, assoc2$tab$pval, tol = 1e-10)
})
