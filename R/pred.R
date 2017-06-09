#' S3 class matlmPred.
#' @exportClass matlmPred
#

#' S3 class matlmPredMat.
#' @exportClass matlmPredMat
#

#' S3 class matlmPredBigMat.
#' @exportClass matlmPredBigMat
#

#--------------
# Constructors
#--------------

#' @export
matlm_pred <- function(x, ind = NULL, num_batches = 1, ...)
{
  ### args
  stopifnot(!any(duplicated(ind)))
  
  out <- list(data = x, ind = ind, num_batches = num_batches)
  
  if(class(x) == "matrix") {
    oldClass(out) <- c("matlmPredMat", "matlmPred")
  } else if(class(x) == "big.matrix") {
     oldClass(out) <- c("matlmPredBigMat", "matlmPred")
  } else {
    stop("not supported class")
  }
  
  # colnames
  if(is.null(colnames(out$data))) {
    colnames(out$data) <- as.character(seq(1, pred_ncol(out)))
  } 
  
  # process batches
  beg <- seq(1, pred_ncol(out), length = out$num_batches)
  end <- c(beg[-1], pred_ncol(out))
  
  ### return
  out$beg <- beg
  out$end <- end
  out$complete <- is.null(out$ind) | (length(out$ind) == pred_nrow(out))
  
  return(out)
}

#-----------------
# Export methods
#-----------------

#' @export pred_nrow
pred_nrow <- function(x, ...) UseMethod("pred_nrow")

#' @export pred_ncol
pred_ncol <- function(x, ...) UseMethod("pred_ncol")

#' @export pred_data
pred_data <- function(x, ...) UseMethod("pred_data")

#' @export pred_batch
pred_batch <- function(x, ...) UseMethod("pred_batch")

#--------------
# print, show
#--------------

#' @export print
print.matlmPred <- function(x, ...)
{
  cat(" ", class(x)[1], " (", class(x$data), ")\n", sep = "")

  cat("  - dimensions:", pred_nrow(x), "x", pred_ncol(x), "\n")
}

#--------------
# nrow, ncol
#--------------

#' @export 
pred_nrow.matlmPred <- function(x, ...)
{
  nrow(x$data)
}

#' @export 
pred_ncol.matlmPred <- function(x, ...)
{
  ncol(x$data)
}

#--------------
# pred_data, pred_batch
#--------------

#' @export 
pred_data.matlmPred <- function(x, ind_row, ind_col, ...)
{
  if(x$complete) {
    if(missing(ind_col) & missing(ind_col)) {
      return(x$data[, ])
    } else {
      stop()
    }
  } else {
    stop()
  }
}

#' @export 
pred_batch.matlmPred <- function(x, batch = 1, ...)
{
  if(x$num_batches == 1) {
    stopifnot(batch == 1)
    return(pred_data(x))
  } else {
    stop()
  }  
}
