#' @export
matlm <- function(formula, data, ..., 
  varcov = NULL, transform = NULL,
  pred, int,
  num_batches = 1,
  num_perm = 0, seed_perm,
  cores = 1,
  verbose = 0)
{
  ### call
  mc <- match.call()
  env <- parent.frame(1)
  
  ### args
  missing_transform <- missing(transform)
  missing_varcov <- missing(varcov)
  missing_seed_perm <- missing(seed_perm)
  
  ### convert `data` to data.fame
  # - data_frame has no row names
  data <- as.data.frame(data)
  
  ### vars
  model <- ifelse(missing(int), "marginal", "interaction")  

  permutation <- ifelse(num_perm > 0, "perm_x", "none")
  
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
  if(verbose > 0) {
    cat(" - model matrices & response...\n")
  }  
  y <- model.extract(model.frame(formula, data), "response")
  C <- model.matrix(formula, data)
  if(model == "interaction") {
    d <- data[ind, int]
  }
  
  stopifnot(nrow(C) == nobs_model)
  stopifnot(length(y) == nobs_model)
  
  N <- nobs_model
  
  ### test multiple predictors one by one
  y_orth <- matlm_orth(C, y)
  y_sc <- matlm_scale(y_orth)
  
  ### local functions, further used by sapply/parSapply
  matlm_batch_marginal <- function(batch, ind)
  {
    X <- pred_batch(pred, batch, ind)
    
    # compute `X_sc`
    X_orth <- matlm_orth(C, X)
    X_sc <- matlm_scale(X_orth)
    
    r <- as.numeric(crossprod(X_sc, y_sc) / (N - 1))
    s <- r * sqrt((N - 2) / (1 - r*r))
    s2 <- s^2
    
    pvals <- pchisq(s2, df = 1, lower = FALSE)
    
    gc()
    
    list(data_frame(predictor = colnames(X), zscore = s, pval = pvals))  
  }
  
  matlm_batch_interaction <- function(batch, ind) 
  {
    X <- pred_batch(pred, batch, ind)
    M <- ncol(X)
    
    # compute `X_sc`
    Xi <- diag(d) %*% X
    X_orth <- matlm_orth(C, X, Xi)
    X_sc <- matlm_scale(X_orth)

    # compute `Y_sc`
    Y <- tcrossprod(y, rep(1, M))
    Y_orth <- matlm_orth(C, X, Y)
    Y_sc <- matlm_scale(Y_orth)   
        
    # compute test statistics via `r` 
    r <- apply(X_sc * Y_sc, 2, sum) / (N - 1)
    s <- r * sqrt((N - 2) / (1 - r*r))
    s2 <- s^2

    pvals <- pchisq(s2, df = 1, lower = FALSE)
    
    gc()
    
    list(data_frame(predictor = colnames(X), zscore = s, pval = pvals))
  }
  
  ### run in a loop (tab)
  if(verbose > 0) {
    cat(" - computing association `tab`...\n")
  }  

  if(cores > 1) {
    cl <- makeCluster(cores, type = "FORK")
    out <- switch(model,
      "marginal" = parSapply(cl, seq(1, num_batches), matlm_batch_marginal),
      "interaction" = parSapply(cl, seq(1, num_batches), matlm_batch_interaction),
      stop("switch by model (tab)"))
    stopCluster(cl)
  } else {
    out <- switch(model,
      "marginal" = sapply(seq(1, num_batches), matlm_batch_marginal),
      "interaction" = sapply(seq(1, num_batches), matlm_batch_interaction),
    stop("switch by model (tab)"))
  }
  tab <- bind_rows(out)

  ### run in a loop (permutation tab)
  if(verbose > 0) {
    cat(" - computing permutation `ptab`...\n")
  }  
  
  pout <- switch(permutation,
    "none" = NULL,
    "perm_x" = {
      ind <- pred_ind(pred)
    
      L <- num_perm
      N <- length(ind)
      P <- sample(ind, size = N * L, replace = TRUE) %>% 
        matrix(nrow = N, ncol = L)
        
      pout <- sapply(seq(1:L), function(l) {
        if(verbose > 0) {
          cat("  --  permutation", l, "/", L, "\n")
        }  
      
        if(cores > 1) {
          cl <- makeCluster(cores, type = "FORK")
          out <- switch(model,
            "marginal" = parSapply(cl, seq(1, num_batches), matlm_batch_marginal, ind = P[, l]),
            "interaction" = parSapply(cl, seq(1, num_batches), matlm_batch_interaction, ind = P[, l]),
            stop("switch by model (tab)"))
          stopCluster(cl)
        } else {
          out <- switch(model,
            "marginal" = sapply(seq(1, num_batches), matlm_batch_marginal, ind = P[, l]),
            "interaction" = sapply(seq(1, num_batches), matlm_batch_interaction, ind = P[, l]),
          stop("switch by model (tab)"))
        }
        tab <- bind_rows(out)

        tab <- mutate(tab, perm = l) %>%
          select(perm, everything())
          
        return(tab)
      }, simplify = FALSE)
    },
    stop("switch by permutation"))
  ptab <- bind_rows(pout)
  
  ### return
  out <- list(model = model, permutation = permutation, tab = tab, ptab = ptab)
  
  oldClass(out) <- c("matlmList", oldClass(out))
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

  ### vars
  nobs_data <- nrow(data)
    
  model <- ifelse(missing(int), "marginal", "interaction")  
  
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
      formula_pred <- switch(model,
        "marginal" = update(formula, paste(". ~ . +", pred)),
        "interaction" = update(formula, paste(". ~ . +", pred, "+", pred, "*", int)),
        stop("switch formula_pred"))
      
      mod <- lm(formula_pred, data_pred, ...)
      
      effects <- coef(mod)
      var_pred <- tail(names(effects), 1)
      
      ret <- switch(model,
        "marginal" = stopifnot(var_pred == pred),
        "interaction" = stopifnot(var_pred %in% 
          c(paste0(pred, ":", int, "1"), paste0(int, "1:", pred))),
        stop("switch"))
      
      estimate <- as.numeric(effects[var_pred])
      se2 <- vcov(mod)[var_pred, var_pred]
    
      zscore <- estimate / sqrt(se2)
    
      stat <- estimate * estimate / se2
      pval <- pchisq(stat, 1, lower.tail = FALSE)
      
      list(data_frame(predictor = pred, zscore = zscore, pval = pval))
    })
    
    list(bind_rows(out))
  })
  tab <- bind_rows(out)
  
  ### return
  out <- list(model = model, tab = tab)
  
  return(out)
}
