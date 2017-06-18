#' S3 class matlmList.
#' @exportClass matlmList
#


#--------------
# print, show
#--------------

#' @export print
print.matlmList <- function(x, ...)
{
  str(x)
}

#--------------
# matlmResults class
#--------------

#' @export print
print.matlmResults <- function(x, ...)
{
  cat(" ", class(x)[1], " (", format(x$nobs_data, big.mark = ","), " samples, ", 
    format(x$npred, big.mark = ","), " predictors)", "\n", sep = "")
  
  if(x$nobs_omit) {
    cat("  --  ", x$nobs_omit, " samples omitted\n")
  }
  
  cat("\n - top results 5 / ", format(nrow(x$tab), big.mark = ",") , ":\n", sep = "")
  x$tab %>% arrange(pval) %>% head(5) %>% as.data.frame %>% print
  
  time_str <- paste0(round(x$tictoc_elapsed, 2), " sec")
  cat("\n - computation time ", time_str, " on ", x$cores, " core(s)\n", sep = "")

}


