### par
N <- 10
M <- 5
K <- 3

### data
set.seed(1)
y <- rnorm(N)
X <- matrix(rnorm(N*M), N, M)
Xi <- matrix(rnorm(N*M), N, M)

C <- matrix(rbinom(N*K, 1, 0.5), N, K)
C[, 1] <- 1

### orth. `X`
Q <- qr.Q(qr(C)) 
X_orth <- X - matlm_proj(Q, X, normalized = TRUE)

# check 
crossprod(C, X_orth) %>% round(2)

### orth. `Xi`
Xi_proj_Q <- matlm_proj(Q, Xi, normalized = TRUE)

norms <- apply(X_orth * X_orth, 2, sum)
prod <- apply(X_orth * Xi, 2, sum)
proj <- X_orth %*% diag(prod/norms) 
Xi_proj_X <- proj
#Xi_proj_X <- matlm_proj(X_orth, Xi, normalized = FALSE)

Xi_orth_Q <- Xi - Xi_proj_Q
Xi_orth <- Xi - Xi_proj_Q - Xi_proj_X

# check 
crossprod(C, Xi_orth_Q) %>% round(2)
crossprod(C, Xi_orth) %>% round(2)

crossprod(X_orth, Xi_orth) %>% diag %>% round(2)

### orth. `y`
Y <- tcrossprod(y_orth_Q, rep(1, M))

Y_proj_Q <- matlm_proj(Q, Y, normalized = TRUE)
Y_proj_X <- matlm_proj(X_orth, Y, normalized = FALSE)

Y_orth <- Y - Y_proj_Q - Y_proj_X

# check 
crossprod(C, Y_orth) %>% round(2)
crossprod(X_orth, Y_orth) %>% round(2)

crossprod(Xi_orth[, 1])


stop()

y_proj_Q <- matlm_proj(Q, y, normalized = TRUE)

prod <- as.numeric(crossprod(y, X_orth))
norms <- apply(X_orth * X_orth, 2, sum)
prod_normalized <- prod / norms
y_proj_X <- tcrossprod(y, prod_normalized) # [y * prod[1], y * prod[2], ...]

y_orth_Q <- y - y_proj_Q
y_orth <- tcrossprod(y_orth_Q, rep(1, M)) - y_proj_X

# check 
crossprod(C, Xi_orth) %>% round(2)

crossprod(Xi_orth[, 1])

