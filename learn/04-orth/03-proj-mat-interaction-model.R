### par
N <- 1000
M <- 5
K <- 3

### data
simdat <- matlm_sim_randpred(seed = 1, N = N, M = M) 

y <- simdat$dat$y
X <- simdat$pred

d <- as.numeric(simdat$dat$d) - 1
C <- model.matrix(simdat$form_int, simdat$dat)
Xi <- diag(d) %*% X

### model
X0_orth <- matlm_orth(C, X)
X_orth <- matlm_orth(C, X, Xi)
crossprod(X_orth, cbind(C, X)) %>% round(2)

X_sc <- matlm_scale(X_orth)

Y <- tcrossprod(y, rep(1, M))
Y_orth <- matlm_orth(C, X, Y)
crossprod(Y_orth, cbind(C, X)) %>% round(2)

Y_sc <- matlm_scale(Y_orth)        

### p-values
r <- sapply(seq(1, ncol(Y_sc)), function(i) crossprod(X_sc[, i], Y_sc[, i]) / (N - 1))
s <- r * sqrt((N - 2) / (1 - r*r))
s2 <- s^2
pvals <- pchisq(s2, df = 1, lower = FALSE)

print(s)
print(pvals)

### assoc_mod
assoc <- matlm_mod(simdat$form_int, simdat$dat, pred = simdat$pred, int = "d")
all.equal(assoc$tab$pval, pvals)

stop()

### another way of computing int.
tab <- sapply(1:M, function(i) {
  dat <- cbind(simdat$dat, x = X[, i], dx = Xi[, i])
  mod1 <- lm(y ~ x*d, dat)
  mod2 <- lm(y ~ x + d + dx, dat)
  
  c(coefficients(summary(mod1))[c(2, 4), 4],
   coefficients(summary(mod2))[c(2, 4), 4])[c(1, 3, 2, 4)]
})

assoc2 <- matlm(form, dat, pred = Xi)
assoc3 <- matlm_mod(form, dat, pred = Xi)

print(assoc$tab$pval)
print(assoc2$tab$pval)
print(assoc3$tab$pval)

all.equal(assoc$tab$pval, assoc2$tab$pval)
all.equal(assoc2$tab$pval, assoc3$tab$pval)
