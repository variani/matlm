.onLoad <- function(libname, pkgname) {
  # @ http://r-pkgs.had.co.nz/r.html
  
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
