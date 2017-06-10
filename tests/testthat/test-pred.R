context("pred")

test_that("matlmPredMat: matrix", {
  nrow <- 2
  ncol <- 2
  
  mat <- matrix(2, nrow = nrow, ncol = ncol)
  pred <- matlm_pred(mat)
  
  expect_true(all(c("matlmPredMat", "matlmPred") %in% class(pred)))
  
  expect_true(pred_nrow(pred) == nrow)
  expect_true(pred_ncol(pred) == ncol)
})

test_that("matlmPredMat: big.matrix", {
  nrow <- 2
  ncol <- 2
  
  mat <- matrix(2, nrow = nrow, ncol = ncol)
  bmat <- as.big.matrix(mat)
  
  pred <- matlm_pred(bmat)
  
  expect_true(all(c("matlmPredBigMat", "matlmPred") %in% class(pred)))
  
  expect_true(pred_nrow(pred) == nrow)
  expect_true(pred_ncol(pred) == ncol)
})

test_that("matlmPredMat: big.matrix.descriptor", {
  nrow <- 2
  ncol <- 2
  
  mat <- matrix(2, nrow = nrow, ncol = ncol)
  bmat <- as.big.matrix(mat)
  bdesc <- describe(bmat)  
  
  pred <- matlm_pred(bdesc)
  
  expect_true(all(c("matlmPredBigMat", "matlmPred") %in% class(pred)))
  
  expect_true(pred_nrow(pred) == nrow)
  expect_true(pred_ncol(pred) == ncol)
})

