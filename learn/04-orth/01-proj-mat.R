### par
N <- 10
M <- 5
K <- 3

### data
set.seed(1)
y <- rnorm(N)
X <- matrix(rnorm(N*M), N, M)

C <- matrix(rbinom(N*K, 1, 0.5), N, K)
C[, 1] <- 1

### orth. by `matlm_orth` function
X_orth <- matlm_orth(C, X)

# check 
crossprod(C, X_orth) %>% round(2)

Q <- qr.Q(qr(C)) 

# projection: x_z = z (z'x) / (z'z)
prod_Q <- apply(Q * Q, 2, sum)
proj <- Q %*% crossprod(Q, X)
#proj <- Q %*% diag(1 / prod_Q) %*% crossprod(Q, X)
X_orth <- X - proj

# check 
crossprod(C, X_orth) %>% round(2)


