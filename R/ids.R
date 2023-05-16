#' @export
matlm_ind <- function(formula, data, ...)
{
  stopifnot(!is.null(row.names(data)))
  
  mf <- model.frame(formula, data, ...)
  
  stopifnot(!is.null(row.names(mf)))
  ind <- which(row.names(data) %in% attr(mf, "row.names"))

  return(ind)  
}
