#' Get size of object in R environment
#'
#' @param obj R object


obj_size <- function(obj) {
  return(format(object.size(obj), units = "auto"))
}
