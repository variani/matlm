#' @export
matlm_sim_randpred <- function(seed, N = 100, M = 500, rho)
{
  # correlated variables
  # - http://r.789695.n4.nabble.com/Random-Normal-Variable-Correlated-to-an-Existing-Binomial-Variable-td3472211.html
  
  if(!missing(seed)) { 
    set.seed(seed)
  }
  
  y <- rnorm(N, mean = 10)
  X <- matrix(rnorm(N * M), nrow = N, ncol = M)
  
  if(!missing(rho)) {
    C <- matrix(rho, M, M)
    diag(C) <- 1

    ch <- chol(C)
    
    X <- X %*% ch 
  }
  
  dat <- data_frame(y = y)
  form <- formula(y ~ 1)
  
  out <- list(form = form, dat = dat, pred = X)
  
  return(out)
}
