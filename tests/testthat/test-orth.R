context("orth")

test_that("orth: y", {
  N <- 50
  
  # simulate data  
  set.seed(1)
  y <- rnorm(N)

  c1 <- rep(1, N)
  c2 <- rbinom(N, 1, 0.5)
  C <- cbind(c1, c2)
  
  # orth. on c1
  y_orth1 <- matlm_orth(c1, y)

  # orth. on C = [c1, c2]
  y_orth2 <- matlm_orth(C, y)
    
  # expect
  expect_true(abs(crossprod(y_orth1, c1)) < 1e-10)
})
