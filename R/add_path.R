
#' Add a New Path to the PATH Environment Variable
#'
#' This function takes a path as input and adds it to the current PATH environment variable.
#'
#' @param new_path The path that you want to add to the PATH.
#'
#' @return A character string of the new PATH environment variable.
#' @examples
#' add_path("/usr/local/bin")
#' @export
#'
add_path <- function(new_path) {
  new_PATH <- paste(Sys.getenv("PATH"), new_path, sep = .Platform$path.sep)
  Sys.setenv(PATH = new_PATH)
  return(new_PATH)
}
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/new/path", sep = .Platform$path.sep))
