context("ids/ind")

test_that("ind: sim. data", {
  N <- 5
  ind <- matlm_sim_randpred(seed = 1, N = N, M = 1) %>% 
    with(matlm_ind(form, dat))
  
  expect_true(length(ind) == N)
})
