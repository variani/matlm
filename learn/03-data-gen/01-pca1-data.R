### 
# Generat N (1,000) data samples  on M (10) variables such that
# - X_{1,000 x 10} is a data matrix
# - C_{10 x 10} = V L V' is a PCA on covariance matrix (data in X is centered)
# - Then, X = U S V' is a SVD, where L = S^2 / (N - 1)
#
# For data generation:
# - S_{10 x 10} = D is a diagonal matrix with d_1 >> d_i (i != 1)
# - V_{10 x 10} = I (orthogonal matrix)
# - U_{1,000 x 10| is a matrix of random numbers
#
# Refs.
# - https://stats.stackexchange.com/a/134283

### inc
library(pls)

### par
seed <- 1
N <- 1000
M <- 10

ratio <- 10

### data generation
set.seed(seed)

L <- diag(M)
L[1, 2:M] <- 0.9
L[2:M, 1] <- 0.9
#L[i] <- ratio * M

U <- matrix(rnorm(M * N), N, M)
S <- sqrt(L * (N - 1))
S <- S / max(diag(S))
V <- diag(M)

X <- U %*% S %*% t(V)

### PCA model
mod <- prcomp(X, center = TRUE, scale = FALSE)

pc1 <- mod$rotation[, 1]
pvar1 <- V[, 1]
cor(pc1, pvar1)

x1 <- mod$x %*% mod$rotation[, 1]

round(mod$sdev^2 / sum(mod$sdev^2), 2)

#scoreplot(mod)
