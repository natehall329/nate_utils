#' Generate blocked non-NA run number variable using run-length encoding
#'
#' @param x vector of values with NAs interspersed
#'
#' @return blocked vector of "run number"
#' @author Nate Hall
#'

#' @export


gen_run_num <- function(x){
  rl <- rle(is.na(x))
  lst <- split(x, rep(cumsum(c(TRUE, rl$values[-length(rl$values)])), rl$lengths))
  runvec <- c()
  for(i in 1:length(lst)){
    runvec <- c(runvec, rep(i, length(lst[[i]])))
  }
  runvec <- ifelse(is.na(x), NA, runvec)
  return(runvec)
}
