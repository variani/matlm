#' @export
matlm <- function(formula, data, ..., 
  varcov = NULL, transform = NULL,
  pred, int,
  num_batches = 1,
  cores = 1,
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
  pred <- matlm_pred(pred, ind = ind, num_batches = num_batches)
  stopifnot(pred_nrow(pred) == nobs_data)
  
  
  ### extract model/response matrices
  y <- model.extract(model.frame(formula, data), "response")
  C <- model.matrix(formula, data)
  
  stopifnot(nrow(C) == nobs_model)
  stopifnot(length(y) == nobs_model)
  
  N <- nobs_model
    
  ### test multiple predictors one by one
  y_orth <- matlm_orth(C, y)
  y_sc <- matlm_scale(y_orth)
  
  matlm_batch <- function(batch) 
  {
    X <- pred_batch(pred, batch)
    
    X_orth <- matlm_orth(C, X)
    X_sc <- matlm_scale(X_orth)
    
    r <- as.numeric(crossprod(X_sc, y_sc) / N) 
    s <- sqrt(N) * r
    s2 <- s^2

    pvals <- pchisq(s2, df = 1, lower = FALSE)
    
    list(data_frame(predictor = colnames(X), zscore = s, pval = pvals))
  }
  
  if(cores > 1) {
    cl <- makeCluster(cores, type = "FORK")
    out <- parSapply(cl, seq(1, num_batches), matlm_batch)
    stopCluster(cl)
  } else {
    out <- sapply(seq(1, num_batches), matlm_batch)
  }
  
  tab <- bind_rows(out)
  
  ### return
  out <- list(tab = tab)
  
  return(out)
}

#' @export
matlm_mod <- function(formula, data, ..., 
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

  nobs_data <- nrow(data)
    
  ### initialize `pred`
  if(verbose > 0) {
    cat(" - creating `matlmPred`...\n")
  }  
  pred <- matlm_pred(pred, num_batches = num_batches)
  stopifnot(pred_nrow(pred) == nobs_data)
  
  ### test multiple predictors one by one
  out <- sapply(seq(1, num_batches), function(batch) {
    X <- pred_batch(pred, batch)
    predictors <- colnames(X)
    
    predictors_mod <- paste0("pred", seq(1, ncol(X)))
    colnames(X) <- predictors_mod
    
    data_pred <- cbind(data, X)
        
    out <- sapply(predictors_mod, function(pred) {
      formula_pred <- update(formula, paste(". ~ . +", pred))
      
      mod <- lm(formula_pred, data_pred, ...)
      
      effects <- coef(mod)
      var_pred <- tail(names(effects), 1)
      stopifnot(var_pred == pred)
    
      estimate <- as.numeric(effects[pred])
      se2 <- vcov(mod)[pred, pred]
    
      zscore <- estimate / sqrt(se2)
    
      stat <- estimate * estimate / se2
      pval <- pchisq(stat, 1, lower.tail = FALSE)
      
      list(data_frame(predictor = pred, zscore = zscore, pval = pval))
    })
    
    list(bind_rows(out))
  })
  tab <- bind_rows(out)
  
  ### return
  out <- list(tab = tab)
  
  return(out)
}
