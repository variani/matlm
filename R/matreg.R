#' @export
matreg <- function(Y, C0, C, X, Xlist,
  varcov = NULL, transform = NULL, 
  verbose = 0)
{
  ### args
  stopifnot(!missing(Y))
  stopifnot(!missing(X))
  
  missing_C0 <- missing(C0)
  missing_C <- missing(C)
  
  missing_transform <- missing(transform)
  missing_varcov <- missing(varcov)

  missing_Xlist <- missing(Xlist)
  
  ### vars
  N <- NROW(Y)
  M <- NCOL(X)
  
  weighted <- (!missing_transform | !missing_varcov)
  
  # process `C` and `C0`
  if(missing_C0) {
    C0 <- matrix(1, nrow = N, ncol = 1)
  }
  
  if(missing_C) {
    C <- C0
  } else {
    C <- cbind(C0, C)
  }

  # process `X` and `Xlist`
  stopifnot(NROW(X) == N)
  if(is.vector(X)) {
    X <- matrix(X, nrow = N, ncol = 1)
  }
  
  if(missing_Xlist) {
    Xlist <- list(X)
  }
  
  ### rotate data if necessary
  if(weighted) {
    if(verbose > 0) {
      cat(" - transform...\n")
    }
  
    if(missing_transform) {
      stopifnot(!missing_varcov)
      
      nobs_varcov <- nrow(varcov)
      stopifnot(nobs_varcov == N)
      
      # perform EVD on `varcov` to get `transform`
      evd <- eigen(varcov, symmetric = TRUE)
      vectors <- evd$vectors
      values <- evd$values
    
      transform <- vectors %*% diag(1/sqrt(values)) %*% t(vectors) # is symmetric
    } else {
      nobs_transform <- nrow(transform)
      stopifnot(nobs_transform == N)
    }

    # transform
    Y <- crossprod(transform, Y)
    C <- crossprod(transform, C)
    
    Xlist <- lapply(Xlist, function(Xi) crossprod(transform, Xi))
  }
  
  #### compute regression
  if(length(Xlist) == 1) {
    X <- Xlist[[1]]
    
    Y_orth <- matlm_orth(C, Y)
    Y_sc <- matlm_scale(Y_orth) 
  
    X_orth <- matlm_orth(C, X)
    X_sc <- matlm_scale(X_orth)

    r <- apply(X_sc * Y_sc, 2, sum) / (N - 1)
    s <- r * sqrt((N - 2) / (1 - r*r))
    s2 <- s^2

    pvals <- pchisq(s2, df = 1, lower = FALSE)
    
    tab <- data_frame(predictor = seq(1, M), beta = r, zscore = s, pval = pvals) 
  } else {
    stop("length(Xlist)")
  }
  
  ### return
  out <- list(tab = tab)

  return(out)
}

