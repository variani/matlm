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
matlm_pred <- function(x, ind = NULL, num_batches = 1, batch_size = NULL, path_pred = ".", ...)
{
  ### args
  stopifnot(!any(duplicated(ind)))
  
  out <- list(path = path_pred)
  
  if(class(x)[1] == "matrix") {
    out$data <- x

    oldClass(out) <- c("matlmPredMat", "matlmPred")
  } else if(class(x) == "big.matrix") {
    out$data <- describe(x)
    
    oldClass(out) <- c("matlmPredBigMat", "matlmPred")
  } else if(class(x) == "big.matrix.descriptor") {
    out$data <- x
    
    oldClass(out) <- c("matlmPredBigMat", "matlmPred")
  } else {
    stop("not supported class")
  }

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
  if(num_batches > pred_ncol(out)) {
    warning("num_batches > pred_ncol: force num_batches <- pred_ncol")
    num_batches <- pred_ncol(out)
  }
    
  if(is.null(batch_size)) {
    beg <- seq(1, pred_ncol(out), length = num_batches) %>% floor
    end <- c(beg[-1] - 1, pred_ncol(out))
  } else {
    beg <- seq(1, pred_ncol(out), by = batch_size) 
    end <- c(beg[-1] - 1, pred_ncol(out))      
  }
  stopifnot(all(beg <= pred_ncol(out)))
  stopifnot(all(end <= pred_ncol(out)))  
  
  out$num_batches <- length(beg)
  out$batch_size <- batch_size
  out$beg <- beg
  out$end <- end
  
  ### return
  return(out)
}

#-----------------
# Export methods
#-----------------

#' @export pred_batch
pred_attach <- function(x, ...) UseMethod("pred_attach")

#' @export pred_nrow
pred_nrow <- function(x, ...) UseMethod("pred_nrow")

#' @export pred_ncol
pred_ncol <- function(x, ...) UseMethod("pred_ncol")

#' @export pred_colnames
pred_colnames <- function(x, ...) UseMethod("pred_colnames")

#' @export pred_ind
pred_ind <- function(x, ...) UseMethod("pred_ind")

#' @export pred_slice
pred_slice <- function(x, ind_row, ind_col, ...) UseMethod("pred_slice")

#' @export pred_data
pred_data <- function(x, ind_row, ind_col, ...) UseMethod("pred_data")

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
# pred_attach
#--------------

pred_attach.matlmPredBigMat <- function(x, ...) 
{
  stopifnot(class(x$data) == "big.matrix.descriptor")
  bmat <- attach.big.matrix(x$data, path = x$path)
  
  stopifnot(!is.nil(bmat@address))
  
  return(bmat)
}

#--------------
# pred_nrow
#--------------

#' @export 
pred_nrow.matlmPred <- function(x, ...) nrow(x$data)

pred_nrow.matlmPredBigMat <- function(x, ...) pred_attach(x) %>% nrow

#--------------
# pred_ncol
#--------------

#' @export 
pred_ncol.matlmPred <- function(x, ...) ncol(x$data)

pred_ncol.matlmPredBigMat <- function(x, ...) pred_attach(x) %>% ncol

#--------------
# pred_colnames
#--------------

#' @export 
pred_colnames.matlmPred <- function(x, ...) colnames(x$data)

pred_colnames.matlmPredBigMat <- function(x, ...) pred_attach(x) %>% colnames

#--------------
# pred_ind
#--------------

#' @export 
pred_ind.matlmPred <- function(x, ...) x$ind

#--------------
# pred_slice
#--------------

#' @export 
pred_slice.matlmPred <- function(x, ind_row, ind_col, ...) x$data[ind_row, ind_col, drop = FALSE]

pred_slice.matlmPredBigMat <- function(x, ind_row, ind_col, ...) pred_attach(x)[ind_row, ind_col, drop = FALSE]

#--------------------------
# pred_data, pred_batch
#--------------------------

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
  mat <- pred_slice(x, ind_row, ind_col)
  
  ### column names
  if(x$null_cnames) {
    cnames <- x$cnames
    cnames <- cnames[ind_col]
    
    colnames(mat) <- cnames
  }
  
  return(mat)
}

#' @export 
pred_batch.matlmPred <- function(x, batch = 1, ind, ...)
{
  ### arg
  stopifnot(batch <= length(x$beg))
  
  ### column indicies
  ind_col <- seq(x$beg[batch], x$end[batch])
  
  ### get data
  pred_data(x, ind_col = ind_col, ind_row = ind, ...) 
}
