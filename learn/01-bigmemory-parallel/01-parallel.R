### inc
library(bigmemory)
library(parallel)

### par
cores <- 4

### data
mat <- matrix(2, nrow = 1e3, ncol = 1e2)
bmat <- as.big.matrix(mat)
desc <- describe(bmat)
 
cl <- makeCluster(cores, type = "FORK")

fun <- function(x) 
{
  bmat <- attach.big.matrix(desc) 
  mat <- bmat[, ]
  
  colMeans(mat)
}
out <- parSapply(cl, seq(1, cores), fun)

stopCluster(cl)

