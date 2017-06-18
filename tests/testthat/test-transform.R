context("transform")

test_that("margial", {
  seed <- 1
  N <- 500
  M <- 5
  
  # run
  simdat <- matlm_sim_randpred(seed = seed, N = N, M = M) 
  
  weights <- sample(1:3, N, replace = TRUE)
  varcov <- diag(1/weights)
  transform <- diag(sqrt(weights))
    
  assoc1 <- matlm(simdat$form, simdat$dat, pred = simdat$pred)
  assoc2 <- matlm(simdat$form, simdat$dat, pred = simdat$pred, transform = transform)
  assoc3 <- matlm_mod(simdat$form, simdat$dat, weights = weights, pred = simdat$pred)  
  
  expect_true(all(assoc1$tab$pval != assoc2$tab$pval))
  expect_equal(assoc2$tab$pval, assoc3$tab$pval, tolerance = 1e-3)
})

test_that("interaction", {
  seed <- 1
  N <- 500
  M <- 5
  
  # run
  simdat <- matlm_sim_randpred(seed = seed, N = N, M = M) 
  
  weights <- sample(1:3, N, replace = TRUE)
  varcov <- diag(1/weights)
  transform <- diag(sqrt(weights))
    
  assoc1 <- matlm(simdat$form_int, simdat$dat, pred = simdat$pred, int = "d")
  assoc2 <- matlm(simdat$form_int, simdat$dat, pred = simdat$pred, int = "d", transform = transform)
  assoc3 <- matlm_mod(simdat$form_int, simdat$dat, weights = weights, pred = simdat$pred, int = "d")  
  
  expect_true(all(assoc1$tab$pval != assoc2$tab$pval))
  expect_equal(assoc2$tab$pval, assoc3$tab$pval, tolerance = 1e-3)
})
