# @ https://stat.ethz.ch/pipermail/r-help/2007-April/128925.html

### par
seed <- 1

N <- 1000
M <- 6

### simulate data
set.seed(seed)
X <- matrix(rnorm(N * M), nrow = N, ncol = M)
X <- scale(X)

mod <- prcomp(X, center = FALSE, scale = FALSE)
L <- mod$x
V <- mod$rotation

V1 <- V[, 1, drop = FALSE]
X2 <- X + 10 * (X %*% V1) %*% t(V1)

X2 <- scale(X2)
mod2 <- prcomp(X2, center = FALSE, scale = FALSE)

### 
L <- diag(M)
U <- matrix(rnorm(M * N), N, M)
S <- sqrt(L * (N - 1))
V <- diag(M)

X <- U %*% S %*% t(V)

V1 <- c(1, rep(0, M - 1))
X2 <- X + 10 * (X %*% V1) %*% t(V1)

