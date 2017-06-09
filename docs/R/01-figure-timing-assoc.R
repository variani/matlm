### inc
library(microbenchmark)
library(ggplot2)

theme_set(theme_minimal())

### simulate data
N <- 1e3
M <- 5e3

simdat <- matlm_sim_randpred(seed = 1, N = N, M = M)

### run benchmarks
out <- microbenchmark(
  "matlm" = matlm(simdat$form, simdat$dat, pred = simdat$pred),
  "lm" = matlm_mod(simdat$form, simdat$dat, pred = simdat$pred),
  times = 5)
  
### plot
title <- paste(N, "individuals,", M, "(null) predictors")
subtitle <- "Timing: a second by matlm vs. one order of magnitude more by lm" 

p <- autoplot(out) + labs(title = title, subtitle = subtitle) +
  theme(text = element_text(size = 20))

png("timing-matlm-vs-lm.png", width = 2*480, height = 480)
print(p)
dev.off()
