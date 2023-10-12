#' Find the Project Root
#'
#' This function recursively searches for the project root, defined as the directory
#' containing an .Rproj file. It starts in the current directory and moves up the
#' directory tree until it either finds an .Rproj file or reaches a directory
#' with no parent.
#'
#' @param setwd Logical. change the working directory to project root? Defaults to TRUE.
#'
#' @return The directory path containing an .Rproj file.
#' Throws an error if no .Rproj file is found in the directory tree above the current directory.
#'
#' @examples
#' \dontrun{
#' find_project_root()
#' }
#' @export
#' @import rstudioapi
#'
#' @note Be careful when running this function, as it could end up searching a large
#' portion of your file system if no .Rproj file is found.
#'
# Install the package if not already installed
# install.packages("rstudioapi")

find_project_root <- function(setwd = TRUE) {
  # Check if we're running interactively
  if (interactive()) {
    # If we're running interactively, check if we're in RStudio
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      # If we're in RStudio, get the path of the currently active source file
      filepath <- rstudioapi::getActiveDocumentContext()$path
    } else {
      # If we're not in RStudio, show a warning and default to the current working directory
      warning("Running interactively outside RStudio. Returning the current directory.")
      filepath <- getwd()
    }
  } else {
    # If we're not running interactively, get the path of the currently running script
    filepath <- normalizePath(sys.frame(1)$ofile)
  }

  # Start in the directory of the provided filepath
  path <- dirname(filepath)

  while (TRUE) {
    # Look for a .Rproj file in the current directory
    files <- list.files(path, pattern = "\\.Rproj$", full.names = TRUE)

    if (length(files) > 0) {
      # If found, return the directory containing the .Rproj file
      if(setwd){
        setwd(dirname(files[1]))
      }
      return(dirname(files[1]))
    }

    # If not found, go to the parent directory
    parent <- dirname(path)

    if (parent == path) {
      # If there's no parent directory, stop searching
      stop("No .Rproj file found")
    }

    path <- parent
  }
}
