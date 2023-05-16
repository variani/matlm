### inc
library(gridExtra)

library(corpcor)

library(devtools)
load_all("~/git/variani/qq/")
load_all("~/git/variani/bigcov/")

### par
rho <- 0.9

seed <- 1
N <- 2000 # 500 2000
M <- 2000

### simulate data
simdat0 <- matlm_sim_randpred(seed = seed, N = N, M = M) # independent predictors
simdat <- matlm_sim_randpred(seed = seed, N = N, M = M, rho = rho) # corr. predictors

### run assoc. analysis
assoc0 <- with(simdat0, matlm(form, dat, pred = pred))
assoc <- with(simdat, matlm(form, dat, pred = pred))

### additional analysis (try to correct correlated statistics)
C <- matrix(rho, M, M)
diag(C) <- 1

zscores_c_true <- as.numeric(assoc$tab$zscore %*% solve(chol(C)))
pvals_c_true <- pchisq(zscores_c_true^2, 1, lower.tail = FALSE)

C_est <- bigcov(simdat$pred, center = TRUE, scale = TRUE, num_splits = 5) 
C_est <- corpcor::make.positive.definite(C_est)
zscores_c_est <- as.numeric(assoc$tab$zscore %*% solve(chol(C_est)))
pvals_c_est <- pchisq(zscores_c_est^2, 1, lower.tail = FALSE)


### plot
grid.arrange(
  qq_plot(assoc0$tab$pval, title = "Independent predictors"),
  qq_plot(assoc$tab$pval, title = paste0("Correlated predictors (rho ", rho, ")")),
  qq_plot(pvals_c_true, title = "Correction (true C)"),
  qq_plot(pvals_c_est, title = "Correction (estimated C)"),  
  ncol = 2)
