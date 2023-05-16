context("pred")

test_that("matlmPredMat: matrix", {
  stopifnot(require(bigmemory))
  
  nrow <- 2
  ncol <- 2
  
  mat <- matrix(2, nrow = nrow, ncol = ncol)
  pred <- matlm_pred(mat)
  
  expect_true(all(c("matlmPredMat", "matlmPred") %in% class(pred)))
  
  expect_true(pred_nrow(pred) == nrow)
  expect_true(pred_ncol(pred) == ncol)
})

test_that("matlmPredBigMat: big.matrix", {
  stopifnot(require(bigmemory))
  
  nrow <- 2
  ncol <- 2
  
  mat <- matrix(2, nrow = nrow, ncol = ncol)
  bmat <- as.big.matrix(mat)
  
  pred <- matlm_pred(bmat)
  
  expect_true(all(c("matlmPredBigMat", "matlmPred") %in% class(pred)))
  
  expect_true(pred_nrow(pred) == nrow)
  expect_true(pred_ncol(pred) == ncol)
})

test_that("matlmPredBigMat: big.matrix.descriptor", {
  stopifnot(require(bigmemory))
  
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

test_that("matlmPredMat: bigmemory files", {
  stopifnot(require(bigmemory))
  
  nrow <- 2
  ncol <- 2
  
  mat <- matrix(2, nrow = nrow, ncol = ncol)
  bmat <- as.big.matrix(mat, backingfile = "pred.bin", descriptorfile = "pred.desc")
      
  pred <- matlm_pred(bmat)
  
  expect_true(all(c("matlmPredBigMat", "matlmPred") %in% class(pred)))
  
  expect_true(pred_nrow(pred) == nrow)
  expect_true(pred_ncol(pred) == ncol)
})

test_that("matlmPredMat: external bigmemory files", {
  stopifnot(require(bigmemory))
  
  N <- 100
  M <- 5
  
  simdat <- matlm_sim_randpred(seed = 10, N = N, M = M)
  
  bmat0 <- as.big.matrix(simdat$pred, backingfile = "pred.bin", 
    descriptorfile = "pred.desc", binary = TRUE)
  
  desc <- readRDS("pred.desc")   
  bmat <- attach.big.matrix(desc)
  
  assoc1 <- matlm(simdat$form, simdat$dat, pred = bmat, num_batches = 2)
  #assoc2 <- matlm(simdat$form, simdat$dat, pred = bmat, num_batches = 2, cores = 2)
})

test_that("matlmPredMat: matrix", {
  stopifnot(require(bigmemory))
  
  nrow <- 2
  ncol <- 10
  batch_size <- 3
  
  mat <- matrix(2, nrow = nrow, ncol = ncol)
  pred1 <- matlm_pred(mat, num_batches = 3)
  pred2 <- matlm_pred(mat, batch_size = 3)
  
  expect_true(pred1$num_batches == 3)
  expect_true(pred2$num_batches == 4)
})


test_that("matlmPredBigMat: matlm", {
  stopifnot(require(bigmemory))
  
  N <- 100
  M <- 5
  
  simdat <- matlm_sim_randpred(seed = 10, N = N, M = M) 
  
  bmat <- as.big.matrix(simdat$pred)
  pred <- matlm_pred(bmat)
  
  #assoc <- matlm(simdat$form, simdat$dat, pred = pred)
})

