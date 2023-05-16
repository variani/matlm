# @ https://stat.ethz.ch/pipermail/r-help/2007-April/128925.html

### par
seed <- 1

N <- 1000
M <- 6

### simulate data
set.seed(seed)
pvar1 <- c(1, rep(0, N - 1))

C <- diag(M)
C[1, 2:M] <- 0.9
C[2:M, 1] <- 0.9

ch <- chol(C, pivot = TRUE)

X <- cbind(pvar1, 
  matrix(rnorm(N * (M - 1)), nrow = N, ncol = M - 1))
X <- scale(X)

X <- X %*% ch

cor(X) %>% round(1)

# find the current correlation matrix
c1 <- var(x1234)

# cholesky decomposition to get independence
chol1 <- solve(chol(c1))

newx <-  x1234 %*% chol1 

# check that we have independence and x1 unchanged
zapsmall(cor(newx))
all.equal( x1234[,1], newx[,1] )

# create new correlation structure (zeros can be replaced with other r
vals)
newc <- matrix( 
c(1  , 0.4, 0.5, 0.6, 
  0.4, 1  , 0  , 0  ,
  0.5, 0  , 1  , 0  ,
  0.6, 0  , 0  , 1  ), ncol=4 )

# check that it is positive definite
eigen(newc)

chol2 <- chol(newc)

finalx <- newx %*% chol2 * sd(x1) + mean(x1)

