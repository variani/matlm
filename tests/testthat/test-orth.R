context("orth")

test_that("matlm_orth", {
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
  expect_true(abs(crossprod(y_orth2, c1)) < 1e-10)
  expect_true(abs(crossprod(y_orth2, c1)) < 1e-10)    
})

test_that("matlm_orth on Xi", {
  N <- 10
  M <- 5
  K <- 3

  # data
  set.seed(1)
  y <- rnorm(N)
  X <- matrix(rnorm(N*M), N, M)
  Xi <- matrix(rnorm(N*M), N, M)

  C <- matrix(rbinom(N*K, 1, 0.5), N, K)
  C[, 1] <- 1

  # orth. `Xi`
  Xi_orth <- matlm_orth(C, X, Xi)

  # expect
  expect_true(all(abs(crossprod(C, Xi_orth)) < 1e-10))  
})

