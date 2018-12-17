#' @export
matlm <- function(formula, data, ..., 
  varcov = NULL, transform = NULL, 
  ids = NULL,
  pred, int,
  num_batches = 1, batch_size = NULL,
  path_pred = ".",
  num_perm = 0, seed_perm,
  returnRespOrth = FALSE, returnPredOrth = FALSE, returnPredOrthSc = FALSE,
  stats_full = FALSE,
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

  missing_ids <- missing(ids)  

  missing_seed_perm <- missing(seed_perm)

  stopifnot(class(formula) == "formula")
  
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
  ind_model <- matlm_ind(formula, data, ...)

  nobs_data <- nrow(data)
  nobs_model <- length(ind_model)
  nobs_omit <- nobs_data - nobs_model
  if(verbose > 1) {
    cat("  -- nobs_data", nobs_data, "/ nobs_model", nobs_model, 
      "/ nobs_omit", nobs_omit, "\n")
  }    
  toc(log = TRUE, quiet = TRUE)
  
  ### compute rotation matrix `transform`
  tic("transform") 
  
  if(verbose > 0) {
    cat(" - computing transform...\n")
  }
   
  if(weighted) {
    if(missing_transform) {
      stopifnot(!missing_varcov)
      
      nobs_varcov <- nrow(varcov)
      if(nobs_varcov == nobs_data) {
        if(nobs_omit) {
          varcov <- varcov[ind_model, ind_model]
        }
      } else {
        stopifnot(!missing_ids)
        
        ids_model <- ids[ind_model]
        
        stopifnot(!is.null(rownames(varcov)))
        ids_varcov <- rownames(varcov)
        
        #idf <- full_join(
        #  data_frame(id = ids_model, id_model = ids_model), 
        #  data_frame(id = ids_varcov, id_varcov = ids_varcov), by = "id")
        #print(idf %>% filter(is.na(id_model) | is.na(id_varcov)))
        #print(table(ids_model %in% ids_varcov))
        
        stopifnot(all(ids_model %in% ids_varcov))
        ind <- which(ids_varcov %in% ids_model)
        varcov <- varcov[ind, ind]
      }
      
      # perform EVD on `varcov` to get `transform`
      evd <- eigen(varcov, symmetric = TRUE)
      vectors <- evd$vectors
      values <- evd$values
    
      transform <- vectors %*% diag(1/sqrt(values)) %*% t(vectors) # is symmetric
    } else {
      nobs_transform <- nrow(transform)
      
      if(nobs_transform == nobs_data) {
        if(nobs_omit) {
          transform <- transform[ind_model, ind_model]            
        }
      } else {
        stopifnot(!missing_ids)
   
        stopifnot(!is.null(rownames(transform)))
        ids_transform <- rownames(transform)      
           
        ids_model <- ids[ind_model]

        #idf <- full_join(
        #  data_frame(id = ids_model, id_model = ids_model), 
        #  data_frame(id = ids_transform, id_transform = ids_transform), by = "id")
        #print(idf %>% filter(is.na(id_model) | is.na(id_transform)))
        #print(table(ids_model %in% ids_transform))
        
                
        stopifnot(all(ids_model %in% ids_transform))
        ind <- which(ids_transform %in% ids_model)
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
  pred <- matlm_pred(pred, ind = ind_model, 
    num_batches = num_batches, batch_size = batch_size, 
    path_pred = path_pred)
  num_batches <- pred$num_batches
  
  stopifnot(pred_nrow(pred) == nobs_data)
  toc(log = TRUE, quiet = TRUE)
  
  ### extract model/response matrices
  tic("model matrices")
  if(verbose > 0) {
    cat(" - model matrices & response...\n")
  }  
  y <- model.extract(model.frame(formula, data), "response")
  C <- model.matrix(formula, data)

  stopifnot(nrow(C) == nobs_model)
  stopifnot(length(y) == nobs_model)
    
  if(model == "interaction") {
    d_fct <- data[ind_model, int]
    
    stopifnot(class(d_fct) == "factor")
    stopifnot(nlevels(d_fct) == 2)
    
    d <- as.numeric(d_fct) - 1
    
    d_cname <- paste0(int, levels(d_fct)[2])
    stopifnot(d_cname %in% colnames(C))
  }
  
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
  if(returnRespOrth) {
    return(as.numeric(y_orth))
  }
  y_sc <- matlm_scale(y_orth)
  
  ### local functions, further used by sapply/parSapply
  matlm_batch_marginal <- function(batch, ind)
  {
    if(verbose > 1) {
      cat("  --  batch", batch, "/", num_batches, "\n")
    }
    
    X <- pred_batch(pred, batch, ind)
    
    # compute `X_sc`
    if(weighted) {
      X <- crossprod(transform, X)
    }
    X_orth <- matlm_orth(C, X)
    if(returnPredOrth) {
      return(list(X_orth))
    }
    sd_X <- matlm_sd(X_orth)
    X_sc <- matlm_scale(X_orth, sd_X = sd_X)
    if(returnPredOrthSc) {
      return(list(X_sc))
    }   
    
    k <- ncol(C) + 1
    r <- as.numeric(crossprod(X_sc, y_sc) / (N - 1))
    r2 <- r * r
    s <- sqrt((1 - r2) / (N - k))
    z <- r / s
    z2 <- z * z
    
    pvals <- pchisq(z2, df = 1, lower = FALSE)
    
    if(stats_full) {
      se <- s / sd_X
      b <- z * se 
    }
    
    gc()
    
    if(!stats_full) {
      list(data_frame(predictor = colnames(X), zscore = z, pval = pvals))  
    } else {
      list(data_frame(predictor = colnames(X), b = b, se = se, zscore = z, pval = pvals))  
    }    
  }
  
  matlm_batch_interaction <- function(batch, ind) 
  {
    if(verbose > 1) {
      cat("  --  batch", batch, "/", num_batches, "\n")
    }
    
    X <- pred_batch(pred, batch, ind)
    M <- ncol(X)

    Xi <- diag(d) %*% X
    
    if(weighted) {
      X <- crossprod(transform, X)
      Xi <- crossprod(transform, Xi)
    }
    
    # compute `X_sc`
    X_orth <- matlm_orth(C, X, Xi)
    if(returnPredOrth) {
      return(list(X_orth))
    }
    sd_X <- matlm_sd(X_orth)
    X_sc <- matlm_scale(X_orth, sd_X = sd_X)
    if(returnPredOrth) {
      return(list(X_sc))
    }
    
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
      "marginal" = parSapply(cl, seq(1, num_batches), function(b)
        matlm_batch_marginal(b, ind_model)),
      "interaction" = parSapply(cl, seq(1, num_batches), function(b)
        matlm_batch_interaction(b, ind_model)),
      stop("switch by model (tab)"))
    stopCluster(cl)
  } else {
    out <- switch(model,
      "marginal" = sapply(seq(1, num_batches), function(b)
        matlm_batch_marginal(b, ind_model)),
      "interaction" = sapply(seq(1, num_batches), function(b)
        matlm_batch_interaction(b, ind_model)),
    stop("switch by model (tab)"))
  }
  if(returnPredOrth | returnPredOrthSc) {
    mat <- do.call(cbind, out)
    return(mat)
  }
  tab <- bind_rows(out)

  ### run in a loop (permutation tab)
  if(verbose > 0) {
    cat(" - computing permutation `ptab`...\n")
  }  
  
  pout <- switch(permutation,
    "none" = NULL,
    "perm_x" = {
      ind <- ind_model
    
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

  out <- list(formula = formula,model = model, weighted = weighted, 
    nobs_data = nobs_data, nobs_model = nobs_model, nobs_omit = nobs_omit,
    npred = pred_ncol(pred),
    permutation = permutation, tab = tab, ptab = ptab,
    num_batches = num_batches, 
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
  pred, 
  path_pred = ".",
  int,
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
  pred <- matlm_pred(pred, num_batches = num_batches, path_pred = path_pred)
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
