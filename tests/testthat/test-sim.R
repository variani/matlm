context("sim")

test_that("correlated predictors", {
  rho <- 0.9
  N <- 1000
  M <- 6

  C <- matrix(rho, M, M)
  diag(C) <- 1

  simdat <- matlm_sim_randpred(seed = 1, N = N, M = M, rho = rho)
 
  C_data <- simdat$pred %>% cor %>% round(1)
  
  expect_equal(C, C_data)
})

