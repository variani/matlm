### inc
library(ggplot2)
library(dplyr)

### par
seed <- 1
N <- 1000
M <- 5

b <- 5

### simulate data
simdat <- matlm_sim_randpred(seed = seed, N = N, M = M) 

y <- simdat$dat$y
X <- simdat$pred
N <- length(y)


x <- X[, 1]
  
dat <- data.frame(y = y, x = x)
dat <- within(dat, {
  y2 <- y + b * x
  y2 <- scale(y2)
})

mod1 <- lm(y ~ x, dat)
estimate1 <- as.numeric(coef(mod1)["x"])
se21 <- vcov(mod1)["x", "x"]

mod2 <- lm(y2 ~ x, dat)
estimate2 <- as.numeric(coef(mod2)["x"])
se22 <- vcov(mod2)["x", "x"]
  
