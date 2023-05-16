# @ https://stat.ethz.ch/pipermail/r-help/2007-April/128925.html

### par
seed <- 1

N <- 1000
M <- 6

rho <- 0.9

### define cov. matrix
C <- matrix(rho, M, M)
diag(C) <- 1

ch <- chol(C)

### simulate data
set.seed(seed)
X <- matrix(rnorm(N * M), nrow = N, ncol = M)
#X <- scale(X)

cor(X) %>% round(2)
cor(X %*% ch) %>% round(2)
