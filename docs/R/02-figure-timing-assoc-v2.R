### inc
library(microbenchmark)
library(ggplot2)

theme_set(theme_light())

### simulate data
N <- 1e3
M <- 5e3

simdat <- matlm_sim_randpred(seed = 1, N = N, M = M)

### run benchmarks
out_marginal <- microbenchmark(
  "matlm" = matlm(simdat$form, simdat$dat, pred = simdat$pred),
  "lm" = matlm_mod(simdat$form, simdat$dat, pred = simdat$pred),
  times = 5)

out_interaction <- microbenchmark(
  "matlm" = matlm(simdat$form, simdat$dat, pred = simdat$pred, int = "d"),
  "lm" = matlm_mod(simdat$form, simdat$dat, pred = simdat$pred, int = "d"),
  times = 5)

tab <- rbind(mutate(as_data_frame(out_marginal), analysis = "marginal"),
    mutate(as_data_frame(out_interaction), analysis = "interaction")) %>%
  mutate(time = time / 1e9,
    analysis = analysis %>% factor %>% relevel(ref = "marginal")) 

### plot
title <- paste(N, "individuals,", M, "null predictors")
subtitle <- "Timing: 1s by matlm vs. 10s by lm" 

p <- ggplot(tab, aes(expr, time)) + geom_violin() + 
  facet_wrap(~ analysis, ncol = 1) + coord_flip() +
  labs(title = title, subtitle = subtitle, x = "time [sec]", y = "") +
  ylim(c(0, max(tab$time))) + 
  theme(text = element_text(size = 20))

png("timing-matlm-vs-lm-v2.png", width = 1.5*480, height = 480)
print(p)
dev.off()
