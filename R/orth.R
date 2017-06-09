#' @export
matlm_orth <- function(C, X)
{
  # @ https://cran.r-project.org/web/packages/matlib/vignettes/gramreg.html
  
  # arg
  if(is.vector(C)) {
    C <- matrix(C, nrow = length(C), ncol = 1)
  }
  
  # check
  stopifnot(nrow(C) == nrow(C))
  
  # orth.
  X_orth <- X
  for(i in seq(1, ncol(C))) {
    z <- C[, i]
    
    X_orth <- X_orth - crossprod(t(z), crossprod(z, X_orth)) / as.numeric(crossprod(z))
  }
  
  return(X_orth)
}
