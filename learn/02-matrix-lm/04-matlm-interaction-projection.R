### inc
library(ggplot2)
library(dplyr)

theme_set(theme_light())

### par
seed <- 1
N <- 1000
M <- 100

### simulate data
simdat <- matlm_sim_randpred(seed = seed, N = N, M = M) 

y <- simdat$dat$y
X <- simdat$pred
N <- length(y)

z <- c(rep(0, N/2), rep(1, N/2))

y <- y #+ 2 * z

### compute via matrix operations
y_c <- scale(y, scale = FALSE)
X_c <- scale(X, scale = FALSE)
z_c <- scale(z, scale = FALSE)

Xi <- X_c * matrix(z_c, nrow = nrow(z_c), ncol = ncol(X))
Xi_c <- scale(Xi, scale = FALSE)

### project out z
y_z <- matlm_orth(z, y_c)
X_z <- matlm_orth(z, X_c)
Xi_z <- matlm_orth(z, Xi_c)

Xi_orth <- matlm_orth(X_z, Xi_z)
y_orth <- y_z # !NB! to be changed

sd_y <- sd(y_orth)
sd_Xi <- apply(Xi_orth, 2, sd)

r <- as.numeric(crossprod(Xi_orth, y_orth) / N)  / (sd_y * sd_Xi)
s <- sqrt(N) * r
s2 <- s^2

pvals_mat <- pchisq(s2, df = 1, lower = FALSE)
    
### compute via `lm`
out <- sapply(1:ncol(X), function(i) {
  x <- X[, i]
  
  dat <- data.frame(y = y, x = x, z = z)
  mod <- lm(y ~ x*z, dat)
  
  estimate <- as.numeric(coef(mod)["x:z"])
  se2 <- vcov(mod)["x:z", "x:z"]
    
  s <- estimate / sqrt(se2)
  s2 <- s^2
  
  pval <- pchisq(s2, 1, lower.tail = FALSE)
  
  list(s = s, pval = pval)
}, simplify = FALSE)

pvals_lm <- sapply(out, function(x) x$pval)

#### check
all(abs(pvals_mat - pvals_lm) < 1e-3)
mean(abs(pvals_mat - pvals_lm))

tab <- data_frame(pval_mat = pvals_mat, pval_lm = pvals_lm)

p <- ggplot(tab, aes(pval_lm, pval_mat)) + geom_point() + 
  geom_abline(linetype = 3)+ coord_equal()
