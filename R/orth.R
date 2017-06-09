#' @export
matlm_orth <- function(C, X)
{
  # @ https://cran.r-project.org/web/packages/matlib/vignettes/gramreg.html
  
  # arg
  if(is.vector(C)) {
    C <- matrix(C, nrow = length(C), ncol = 1)
  }
    
  # check
  #stopifnot(nrow(X) == nrow(C))
  N <- nrow(C)
  
  # orth.
  Z <- qr.Q(qr(C)) 
  
  X_orth <- X
  for(i in seq(1, ncol(Z))) {
    z <- Z[, i]
    
    X_orth <- X_orth - matlm_orth_proj(z, X_orth)
  }
  
  return(X_orth)
}

#' @export
matlm_orth_proj <- function(z, X) 
{
  crossprod(t(z), crossprod(z, X)) / as.numeric(crossprod(z))
}
