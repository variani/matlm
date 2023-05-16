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
matlm_sd <- function(X)
{
  if(is.vector(X)) {
    sd(X)
  } else {
    apply(X, 2, sd)
  }
}

#' @export
matlm_scale <- function(X, sd_X)
{
  # @ https://privefl.github.io/blog/(Linear-Algebra)-Do-not-scale-your-matrix/

  if(missing(sd_X)) {
    sd_X <- matlm_sd(X)
  }

  if(is.vector(X)) {
    (X - mean(X)) / sd_X
  } else if(ncol(X) == 1) {
    (X - colMeans(X)) / sd_X
  } else {
    scale(X, center = colMeans(X), scale = sd_X)
    # N <- nrow(X)
    # M <- ncol(X)

    # (diag(N) - tcrossprod(rep(1, N)) / N) %*% X %*% diag(1 / sd_X, M, M)
  }
}
