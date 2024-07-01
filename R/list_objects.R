#' List Data and Function Objects in the Current Environment
#'
#' This function lists all data and function objects in the current R environment.
#' It can sort the data objects by name or size and display their sizes in a readable format.
#'
#' @param sort_by A character string specifying the sort order for data objects. Can be "name" (default) or "size".
#' @param display A character string specifying which objects to display. Can be "both" (default), "data", or "functions".
#' @return Prints the names and sizes of data and function objects in the current environment, and the total size of all data objects.
#' @export
#' @examples
#' \dontrun{
#'   # Create some example data objects and functions
#'   x <- 1:10
#'   y <- data.frame(a = 1:5, b = 6:10)
#'   z <- "Hello, world!"
#'   my_func <- function(x) { x + 1 }
#'
#'   # Print objects sorted by name
#'   list_objects()
#'
#'   # Print data objects sorted by size
#'   list_objects(sort_by = "size")
#'
#'   # Print only data objects sorted by name
#'   list_objects(display = "data")
#'
#'   # Print only functions sorted by name
#'   list_objects(display = "functions")
#' }

list_objects <- function(sort_by = "name", display = "both") {
  library(dplyr)
  library(tibble)
  library(nate.utils)

  objs <- ls(envir = .GlobalEnv)

  if(length(objs) == 0){
    return(cat("Empty Environment\n"))
  }

  functions <- objs[sapply(objs, function(x) is.function(get(x, envir = .GlobalEnv)))]
  data_objs <- objs[!objs %in% functions]

  # Calculate sizes for data objects
  size_df <- tibble(
    name = data_objs,
    size_display = sapply(data_objs, function(x) obj_size(get(x, envir = .GlobalEnv))),
    size_numeric = sapply(data_objs, function(x) object.size(get(x, envir = .GlobalEnv)))
  )

  # Calculate sizes for function objects
  func_size_df <- tibble(
    name = functions,
    size_display = sapply(functions, function(x) obj_size(get(x, envir = .GlobalEnv))),
    size_numeric = sapply(functions, function(x) object.size(get(x, envir = .GlobalEnv)))
  )

  # Sort data objects based on the chosen criteria
  if (sort_by == "size") {
    size_df <- size_df %>% arrange(-size_numeric)
  } else if (sort_by != "name") {
    stop("Invalid sort_by parameter. Use 'size' or 'name'.")
  }

  # Find the length of the longest object name
  max_name_length <- max(nchar(c(size_df$name, func_size_df$name)), nchar("All data objects"), nchar("Functions"))

  # Print functions if requested
  if (display == "both" || display == "functions") {
    cat("\n", strrep(" ", (max_name_length + 14 - nchar("Functions")) %/% 2), "Functions:\n", sep = "")
    cat(strrep("-", max_name_length + 3 + max(nchar(size_df$size_display))), "\n", sep = "")
    if (nrow(func_size_df) > 0) {
      func_output <- sprintf("%-*s | %s", max_name_length, func_size_df$name, func_size_df$size_display)
      cat(func_output, sep = "\n")
      cat(strrep("-", max_name_length + 3 + max(nchar(func_size_df$size_display))), "\n", sep = "")

      # Calculate and print the total size of all function objects
      total_func_size <- sum(func_size_df$size_numeric)
      func_size_display <- case_when(
        total_func_size > 1e12 ~ paste0(round(total_func_size / 1e12, 1), " Tb"),
        total_func_size > 1e9 ~ paste0(round(total_func_size / 1e9, 1), " Gb"),
        total_func_size > 1e6 ~ paste0(round(total_func_size / 1e6, 1), " Mb"),
        total_func_size > 1e3 ~ paste0(round(total_func_size / 1e3, 1), " Kb"),
        TRUE ~ paste0(round(total_func_size, 1), " bytes")
      )
      cat(sprintf("%-*s | %s\n", max_name_length, "All function objects", func_size_display))
    } else {
      cat("No functions found.\n")
    }
  }

  # Print data objects if requested
  if (display == "both" || display == "data") {
    cat("\n", strrep(" ", (max_name_length + 14 - nchar("Data Objects")) %/% 2), "Data Objects:\n", sep = "")
    cat(strrep("-", max_name_length + 3 + max(nchar(size_df$size_display))), "\n", sep = "")
    if (nrow(size_df) > 0) {
      data_output <- sprintf("%-*s | %s", max_name_length, size_df$name, size_df$size_display)
      cat(data_output, sep = "\n")
      cat(strrep("-", max_name_length + 3 + max(nchar(size_df$size_display))), "\n", sep = "")

      # Calculate and print the total size of all data objects
      total_data_size <- sum(size_df$size_numeric)
      data_size_display <- case_when(
        total_data_size > 1e12 ~ paste0(round(total_data_size / 1e12, 1), " Tb"),
        total_data_size > 1e9 ~ paste0(round(total_data_size / 1e9, 1), " Gb"),
        total_data_size > 1e6 ~ paste0(round(total_data_size / 1e6, 1), " Mb"),
        total_data_size > 1e3 ~ paste0(round(total_data_size / 1e3, 1), " Kb"),
        TRUE ~ paste0(round(total_data_size, 1), " bytes")
      )
      cat(sprintf("%-*s | %s\n", max_name_length, "All data objects", data_size_display))
    } else {
      cat("No data objects found.\n")
    }
  }
}
