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

  # process `X`
  stopifnot(NROW(X) == N)
  if(is.vector(X)) {
    X <- matrix(X, nrow = N, ncol = 1)
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
    
    if(!missing_Xlist) {
      Xlist <- lapply(Xlist, function(Xi) crossprod(transform, Xi))
    }
  }
  
  #### compute regression
  if(missing_Xlist) {
    Y_orth <- matlm_orth(C, Y)
    sd_Y_orth <- matlm_sd(Y_orth)
    Y_sc <- matlm_scale(Y_orth, sd_Y_orth) 
  
    X_orth <- matlm_orth(C, X)
    sd_X_orth <- matlm_sd(X_orth)
    X_sc <- matlm_scale(X_orth, sd_X_orth) 
  } else if(length(Xlist) == 2) {
    Y_orth <- matlm_orth(C, Xlist[[1]], Y)
    sd_Y_orth <- matlm_sd(Y_orth)
    Y_sc <- matlm_scale(Y_orth, sd_Y_orth) 
  
    X_orth <- matlm_orth(C, Xlist[[1]], X)
    sd_X_orth <- matlm_sd(X_orth)
    X_sc <- matlm_scale(X_orth, sd_X_orth) 
  } else {
    Y_orth <- matlm_orth_list(C, Xlist, Y)
    sd_Y_orth <- matlm_sd(Y_orth)
    Y_sc <- matlm_scale(Y_orth, sd_Y_orth) 
  
    X_orth <- matlm_orth_list(C, Xlist, X)
    sd_X_orth <- matlm_sd(X_orth)
    X_sc <- matlm_scale(X_orth, sd_X_orth) 
  }

  r <- colSums(X_sc * Y_sc) / (N - 1)
  sigma_sc <- sqrt((1 - r*r) / (N - 2))  
  
  s <- r / sigma_sc
  s2 <- s^2

  pvals <- pchisq(s2, df = 1, lower = FALSE)
  beta <- r * sd_Y_orth / sd_X_orth
  se <- beta / s
  sigma <- sigma_sc * (sd_Y_orth * sqrt(N - 1)) # the correction factor is e'e = sd * (N - 1)
  
  tab <- data_frame(predictor = seq(1, M), 
    beta = beta, se = se, sigma2 = sigma^2,
    zscore = s, pval = pvals) 

  ### return
  out <- list(tab = tab)

  return(out)
}

