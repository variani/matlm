#' @export
matlm_sim_randpred <- function(seed, N = 100, M = 500)
{
  if(!missing(seed)) { 
    set.seed(seed)
  }
  
  y <- rnorm(N, mean = 10)
  X <- matrix(rnorm(N * M), nrow = N, ncol = M)
  
  dat <- data_frame(y = y)
  form <- formula(y ~ 1)
  
  out <- list(form = form, dat = dat, pred = X)
  
  return(out)
}
