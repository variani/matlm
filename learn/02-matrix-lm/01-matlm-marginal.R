### inc
library(ggplot2)
library(dplyr)

### par
seed <- 1
N <- 20 # 20, 1000
M <- 100

### simulate data
simdat <- matlm_sim_randpred(seed = seed, N = N, M = M) 

y <- simdat$dat$y
X <- simdat$pred
N <- length(y)

### compute via matrix operations
y_c <- scale(y, scale = FALSE)
X_c <- scale(X, scale = FALSE)

sd_y <- sd(y_c)
sd_X <- apply(X, 2, sd)

r <- as.numeric(crossprod(X_c, y_c) / N) / (sd_y * sd_X)
s <- r * sqrt((N - 2) / (1 - r*r))
# s <_ r * sqrt(N)
s2 <- s^2

pvals_mat <- pchisq(s2, df = 1, lower = FALSE)
    
### compute via `lm`
out <- sapply(1:ncol(X), function(i) {
  x <- X[, i]
  
  dat <- data.frame(y = y, x = x)
  mod <- lm(y ~ x, dat)
  
  estimate <- as.numeric(coef(mod)["x"])
  se2 <- vcov(mod)["x", "x"]
    
  s <- estimate / sqrt(se2)
  s2 <- s^2
  
  pval <- pchisq(s2, 1, lower.tail = FALSE)
  
  list(s = s, pval = pval)
}, simplify = FALSE)

pvals_lm <- sapply(out, function(x) x$pval)

#### check
all(abs(pvals_mat - pvals_lm) < 1e-3)

tab <- data_frame(pval_mat = pvals_mat, pval_lm = pvals_lm)

p <- ggplot(tab, aes(pval_lm, pval_mat)) + geom_point() + 
  geom_abline(linetype = 3)+ coord_equal()
