#' Package matlm.
#'
#' Linear models in matrix form.
#'
#' @rdname matlmsPackage
#' @name matlmPackage
#' @docType package
#'
#' @import Matrix
#' @import bigmemory
#' @importFrom tibble data_frame as_data_frame 
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#' @importFrom stats lm model.extract model.frame model.matrix 
{}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.matlm <- list(
    # bigmemory
    bigmemory.allow.dimname = TRUE)
    
  #toset <- !(names(op.matlm) %in% names(op))
  #if(any(toset)) {
  #  options(op.matlm[toset])
  #}

  options(bigmemory.allow.dimnames = TRUE)

  invisible()
}
