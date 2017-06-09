#' @export
matlm_center <- function(X)
{
  # @ https://stackoverflow.com/a/12332878
  
  if(is.vector(X)) {
    X - mean(X)
  } else {
    X - tcrossprod(rep(1, nrow(X)), colMeans(X))
  }
}

#' @export
matlm_scale <- function(X)
{
  # @ https://privefl.github.io/blog/(Linear-Algebra)-Do-not-scale-your-matrix/
  
  if(is.vector(X)) {
    (X - mean(X)) / sd(X)
  } else {
    N <- nrow(X)
    M <- ncol(X)

    sd_X <- apply(X, 2, sd)

    (diag(N) - tcrossprod(rep(1, N)) / N) %*% X %*% diag(1 / sd_X, M, M)
  }
}
