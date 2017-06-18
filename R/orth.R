#' @export
matlm_orth <- function(C, X, Xi)
{
  # @ https://cran.r-project.org/web/packages/matlib/vignettes/gramreg.html

  # args
  missing_Xi <- missing(Xi)
  
  if(is.vector(C)) {
    C <- matrix(C, ncol = 1)
  }
  
  # orth.
  Q <- qr.Q(qr(C)) 
  
  X_orth <- X - matlm_proj(Q, X, normalized = TRUE)
  # `normalized = TRUE`, as Q is orthonormal matrix
  # note that `X_orth` is not normalized
  
  # Xi
  if(!missing_Xi) {
    Xi_proj_Q <- matlm_proj(Q, Xi, normalized = TRUE)
    Xi_proj_X <- matlm_proj_mut(X_orth, Xi)

    Xi_orth <- Xi - Xi_proj_Q - Xi_proj_X
  }

  # return
  if(missing_Xi) {
    return(X_orth)
  } else {
    return(Xi_orth)
  }
}


#' @export
matlm_proj <- function(Q, X, normalized = FALSE) 
{
  # projection: x_z = z (z'x) / (z'z)
  # it follows that (x - x_z) and z are orthogonal, 
  # as z' (x - x_z) = z'x - z'x = 0
  if(normalized) {
    Q %*% crossprod(Q, X)
  } else {
    if(is.vector(Q)) {
      Q %*% crossprod(Q, X) / as.numeric(crossprod(Q))
    } else {
      prod_Q <- apply(Q * Q, 2, sum)
      Q %*% diag(1 / prod_Q) %*% crossprod(Q, X)
    }
  }
}

#' @export
matlm_proj_mut <- function(C, X)
{
  stopifnot(nrow(C) == nrow(X))
  stopifnot(ncol(C) == ncol(X))
  
  norms <- apply(C * C, 2, sum)
  prod <- apply(C * X, 2, sum)
  
  if(ncol(C) == 1) {
    (prod / norms) * C
  } else {
    C %*% diag(prod / norms) 
  }
}
