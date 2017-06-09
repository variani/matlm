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
  
  out <- list(data = x, num_batches = num_batches)
  
  if(class(x) == "matrix") {
    oldClass(out) <- c("matlmPredMat", "matlmPred")
  } else if(class(x) == "big.matrix") {
     oldClass(out) <- c("matlmPredBigMat", "matlmPred")
  } else {
    stop("not supported class")
  }

  stopifnot(num_batches <= pred_ncol(out))

  ### ind
  if(is.null(ind)) {
    ind <- seq(1, pred_nrow(out))
  }
  out$ind <- ind
  out$complete <- (length(ind) == pred_nrow(out))
  
  # colnames
  cnames <- pred_colnames(out)
  if(is.null(cnames)) {
    out$cnames <- as.character(paste0("pred", seq(1, pred_ncol(out))))
    out$null_cnames <- TRUE    
  } else {
    out$cnames <- cnames
    out$null_cnames <- FALSE  
  } 

    
  # process batches
  beg <- seq(1, pred_ncol(out), length = out$num_batches)
  end <- c(beg[-1] - 1, pred_ncol(out))
  stopifnot(all(beg <= pred_ncol(out)))
  stopifnot(all(end <= pred_ncol(out)))
  
  ### return
  out$beg <- beg
  out$end <- end
  
  return(out)
}

#-----------------
# Export methods
#-----------------

#' @export pred_nrow
pred_nrow <- function(x, ...) UseMethod("pred_nrow")

#' @export pred_ncol
pred_ncol <- function(x, ...) UseMethod("pred_ncol")

#' @export pred_colnames
pred_colnames <- function(x, ...) UseMethod("pred_colnames")


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

#' @export 
pred_colnames.matlmPred <- function(x, ...)
{
  colnames(x$data)
}

#--------------
# pred_data, pred_batch
#--------------

#' @export 
pred_data.matlmPred <- function(x, ind_row, ind_col, ...)
{
  # `ind_row` & `ind_col`
  if(missing(ind_row)) {
    ind_row <- x$ind
  }
  if(missing(ind_col)) {
    ind_col <- seq(1, pred_ncol(x))
  }
    
  ### get data matrix `mat`
  mat <- x$data[ind_row, ind_col, drop = FALSE]
  
  ### column names
  if(x$null_cnames) {
    cnames <- x$cnames
    cnames <- cnames[ind_col]
    
    colnames(mat) <- cnames
  }
  
  return(mat)
}

#' @export 
pred_batch.matlmPred <- function(x, batch = 1, ...)
{
  ### arg
  stopifnot(batch <= length(x$beg))
  
  ### column indicies
  ind_col <- seq(x$beg[batch], x$end[batch])
  
  ### get data
  pred_data(x, ind_col = ind_col, ...) 
}
