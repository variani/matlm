#' @export
matlm <- function(formula, data, ..., 
  varcov = NULL, transform = NULL,
  pred, int,
  num_batches = 1,
  num_perm = 0, seed_perm,
  cores = 1,
  verbose = 0)
{
  tic.clearlog()
  tic("matlm")
  tic("args")

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

  weighted <- (!missing_transform | !missing_varcov)
  toc(log = TRUE, quiet = TRUE)
  
  ### ind
  
  tic("indices")
  
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
  toc(log = TRUE, quiet = TRUE)
  
  ### compute rotation matrix `transform`
  tic("transform")  
  if(weighted) {
    if(missing_transform) {
      stopifnot(!missing_varcov)
      
      nobs_varcov <- nrow(varcov)
      stopifnot(nobs_varcov == nobs_data)
      if(nobs_omit) {
        varcov <- varcov[ind, ind]
      }
      
      # perform EVD on `varcov` to get `transform`
      evd <- eigen(varcov, symmetric = TRUE)
      vectors <- evd$vectors
      values <- evd$values
    
      transform <- vectors %*% diag(1/sqrt(values)) %*% t(vectors) # is symmetric
    } else {
      nobs_transform <- nrow(transform)
      stopifnot(nobs_transform == nobs_data)
      if(nobs_omit) {
        transform <- transform[ind, ind]            
      }
    }
  }
  toc(log = TRUE, quiet = TRUE)
   
  ### initialize `pred` using `ind`
  tic("pred")
  if(verbose > 0) {
    cat(" - creating `matlmPred`...\n")
  }  
  pred <- matlm_pred(pred, ind = ind, num_batches = num_batches)
  stopifnot(pred_nrow(pred) == nobs_data)
  toc(log = TRUE, quiet = TRUE)
  
  ### extract model/response matrices
  tic("model matrices")
  if(verbose > 0) {
    cat(" - model matrices & response...\n")
  }  
  y <- model.extract(model.frame(formula, data), "response")
  C <- model.matrix(formula, data)
  if(model == "interaction") {
    d <- data[ind, int]
    if(class(d) == "factor") {
      d <- as.numeric(d) - 1
    }
  }
  
  stopifnot(nrow(C) == nobs_model)
  stopifnot(length(y) == nobs_model)
  
  N <- nobs_model
  toc(log = TRUE, quiet = TRUE)
  
  ### apply `transform` if necessary
  tic("apply transform")
  if(weighted) {
    y <- crossprod(transform, y)
    C <- crossprod(transform, C)
  }
  toc(log = TRUE, quiet = TRUE)
  
  ### test multiple predictors one by one
  tic("tests")
  y_orth <- matlm_orth(C, y)
  y_sc <- matlm_scale(y_orth)
  
  ### local functions, further used by sapply/parSapply
  matlm_batch_marginal <- function(batch, ind)
  {
    X <- pred_batch(pred, batch, ind)
    
    # compute `X_sc`
    if(weighted) {
      X <- crossprod(transform, X)
    }
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

    Xi <- diag(d) %*% X
    
    if(weighted) {
      X <- crossprod(transform, X)
      Xi <- crossprod(transform, Xi)
    }
    
    # compute `X_sc`
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

  if(cores > 0) {
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
  toc(log = TRUE, quiet = TRUE)
  
  ### return
  tic("return")
  out <- list(model = model, permutation = permutation, tab = tab, ptab = ptab,
    nobs_data = nobs_data, nobs_model = nobs_model, nobs_omit = nobs_omit,
    npred = pred_ncol(pred),
    cores = cores)
  
  oldClass(out) <- c("matlmResults", "matlmList", oldClass(out))
  toc(log = TRUE, quiet = TRUE)
  toc(log = TRUE, quiet = TRUE) # matlm
  
  # save comp. time
  out$tictoc <- tic.log(format = FALSE)  
  out$tictoc_formated <- tic.log(format = TRUE)
  tic.clearlog()  
  
  tictoc_matlm <- tail(out$tictoc, 1)
  tictoc_elapsed <- tictoc_matlm[[1]]$toc - tictoc_matlm[[1]]$tic
  out$tictoc_elapsed <- unname(tictoc_elapsed)

  return(out)
}

#' @export
matlm_mod <- function(formula, data, weights, ..., 
  varcov = NULL, transform = NULL,
  pred, int,
  num_batches = 1,
  verbose = 0)
{
  ### call
  mc <- match.call()
  env <- parent.frame(1)
  
  ### args
  missing_weights <- missing(weights)
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
      
      if(missing_weights) {
        mod <- lm(formula_pred, data_pred, ...)
      } else {
        stopifnot(!("weights" %in% names(data_pred)))
        data_pred$weights <- weights
        
        mod <- lm(formula_pred, data_pred, weights = weights, ...)
      }
      
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
