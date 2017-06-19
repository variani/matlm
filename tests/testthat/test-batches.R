context("batches")

test_that("many batches (matrix)", {
  N <- 10
  M <- 5
  
  simdat <- matlm_sim_randpred(seed = 1, N = N, M = M) 
  
  assoc1 <- matlm(simdat$form, simdat$dat, pred = simdat$pred, num_batches = 1)
  assoc2 <- matlm(simdat$form, simdat$dat, pred = simdat$pred, num_batches = 2)
  assoc3 <- matlm(simdat$form, simdat$dat, pred = simdat$pred, num_batches = 3)  
 
  expect_equal(assoc1$tab$pval, assoc2$tab$pval, tol = 1e-10)
  expect_equal(assoc1$tab$pval, assoc3$tab$pval, tol = 1e-10)
  
  # `num_batches = M`
  assocM <- matlm(simdat$form, simdat$dat, pred = simdat$pred, num_batches = M)
  expect_equal(assoc1$tab$pval, assocM$tab$pval, tol = 1e-10)  

  # `num_batches > M`  
  expect_error(matlm(simdat$form, simdat$dat, pred = simdat$pred, num_batches = M + 1))   
})

test_that("many batches (big matrix)", {
  N <- 10
  M <- 5
  
  simdat <- matlm_sim_randpred(seed = 1, N = N, M = M) 

  pred <- simdat$pred
  bpred <- as.big.matrix(pred, backingfile = "pred.bin", descriptorfile = "pred.desc")
  
  assoc1 <- matlm(simdat$form, simdat$dat, pred = bpred, num_batches = 1)
  assoc2 <- matlm(simdat$form, simdat$dat, pred = bpred, num_batches = 2)
  assoc3 <- matlm(simdat$form, simdat$dat, pred = bpred, num_batches = 3)  
 
  expect_equal(assoc1$tab$pval, assoc2$tab$pval, tol = 1e-10)
  expect_equal(assoc1$tab$pval, assoc3$tab$pval, tol = 1e-10)
  
  # `num_batches = M`
  assocM <- matlm(simdat$form, simdat$dat, pred = bpred, num_batches = M)
  expect_equal(assoc1$tab$pval, assocM$tab$pval, tol = 1e-10)  

  # `num_batches > M`  
  expect_error(matlm(simdat$form, simdat$dat, pred = bpred, num_batches = M + 1))  
})
