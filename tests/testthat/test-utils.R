context("utils")

test_that("matlm_center", {
  N <- 10
  
  set.seed(1)
  means <- rnorm(N * N) %>% matrix(N, N) %>% matlm_center %>% apply(2, mean)
  
  expect_equal(means, rep(0, N)) 
})

test_that("matlm_scale", {
  N <- 10
  
  set.seed(1)
  means <- rnorm(N * N) %>% matrix(N, N) %>% matlm_scale %>% apply(2, mean)
  sds <- rnorm(N * N) %>% matrix(N, N) %>% matlm_scale %>% apply(2, sd)
  
  expect_equal(means, rep(0, N)) 
  expect_equal(sds, rep(1, N)) 
})
