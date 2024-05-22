#' List Data Objects in the Current Environment
#'
#' This function lists all data objects in the current R environment, excluding functions.
#'
#' @return A character vector of the names of data objects in the current environment.
#' @export
#' @examples
#' \dontrun{
#'   # Create some example data objects
#'   x <- 1:10
#'   y <- data.frame(a = 1:5, b = 6:10)
#'   z <- "Hello, world!"
#'
#'   # Print data objects
#'   print(list_data_objects())
#' }
list_data_objects <- function() {
  objs <- ls(envir = .GlobalEnv)
  data_objs <- objs[!sapply(objs, function(x) is.function(get(x, envir = .GlobalEnv)))]
  sizes <- sapply(data_objs, function(x) nate.utils::obj_size(get(x, envir = .GlobalEnv)))
  output <- paste(data_objs, sizes, sep = ": ")
  cat(output, sep = "\n")
}
