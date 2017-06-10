### inc
library(bigmemory)
library(parallel)

### par
cores <- 2

### data
mat <- matrix(2, 2, 2)
bmat <- as.big.matrix(mat)
desc <- describe(bmat)
 
cl <- makeCluster(cores, type = "FORK")

fun <- function(x) 
{
  bmat <- attach.big.matrix(desc) 
  mat <- bmat[, ]
  
  colMeans(mat)
}
out <- parSapply(cl, 1:3, fun)

stopCluster(cl)

