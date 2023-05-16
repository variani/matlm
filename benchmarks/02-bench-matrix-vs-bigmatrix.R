### inc
library(bigmemory)

### par
num_batches <- 10
cores <- 2

### simulate data
N <- 5e2
M <- 5e3

simdat <- matlm_sim_randpred(seed = 1, N = N, M = M)

system.time(assoc_matrix <- matlm(simdat$form, simdat$dat, pred = simdat$pred, num_batches = num_batches, cores = cores))

#bpred <- as.big.matrix(simdat$pred)
bpred <- as.big.matrix(simdat$pred, backingfile = "pred.bin", descriptorfile = "pred.desc")

bdesc <- describe(bpred)
system.time(assoc_bigmatrix <- matlm(simdat$form, simdat$dat, pred = bdesc, num_batches = num_batches, cores = cores))
