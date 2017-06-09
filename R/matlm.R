#' @export
matlm <- function(formula, data, ..., 
  varcov = NULL, transform = NULL,
  pred, int,
  num_batches = 1,
  verbose = 0)
{
  ### call
  mc <- match.call()
  env <- parent.frame(1)
  
  ### args
  missing_transform <- missing(transform)
  missing_varcov <- missing(varcov)
  
  ### convert `data` to data.fame
  # - data_frame has no row names
  data <- as.data.frame(data)
  
  ### ind
  if(verbose > 0) {
    cat(" - computing indices...\n")
  }
  ind <- matlm_ind(formula, data, ...)

  nobs_data <- nrow(data)
  nobs_model <- length(ind)
  nobs_omit <- nobs_data - nobs_model
  if(verbose > 1) {
    cat("  -- nobs_data", nobs_data, "/ nobs_model", nobs_model, 
      "/ nobs_omit", nobs_omit, "\n")
  }    
  
  ### initialize `pred`
  if(verbose > 0) {
    cat(" - creating `matlmPred`...\n")
  }  
  pred <- matlm_pred(pred, ind, num_batches)
  stopifnot(pred_nrow(pred) == nobs_data)
  
  
  ### extract model/response matrices
  y <- model.extract(model.frame(formula, data), "response")
  C <- model.matrix(formula, data)

  stopifnot(nrow(C) == nobs_model)
  stopifnot(length(y) == nobs_model)
    
  ### test multiple predictors one by one
  y_orth <- matlm_orth(C, y)
  
  for(batch in seq(1, num_batches)) {
    X <- pred_batch(pred, batch)
    X_orth <- matlm_orth(C, X)
  }
  
  return(invisible())
}
