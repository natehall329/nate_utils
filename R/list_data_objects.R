#' List Data Objects in the Current Environment
#'
#' This function lists all data objects in the current R environment, excluding functions. It can sort the objects by name or size and display their sizes in a readable format.
#'
#' @param sort_by A character string specifying the sort order. Can be "name" (default) or "size".
#' @return Prints the names and sizes of data objects in the current environment, and the total size of all objects.
#' @export
#' @examples
#' \dontrun{
#'   # Create some example data objects
#'   x <- 1:10
#'   y <- data.frame(a = 1:5, b = 6:10)
#'   z <- "Hello, world!"
#'
#'   # Print data objects sorted by name
#'   list_data_objects()
#'
#'   # Print data objects sorted by size
#'   list_data_objects("size")
#' }
list_data_objects <- function(sort_by = "name") {
  objs <- ls(envir = .GlobalEnv)
  data_objs <- objs[!sapply(objs, function(x) is.function(get(x, envir = .GlobalEnv)))]

  size_df <- tibble(name = data_objs,
                    size_display = sapply(data_objs, function(x) nate.utils::obj_size(get(x, envir = .GlobalEnv))),
                    size_numeric = sapply(data_objs, function(x) object.size(get(x, envir = .GlobalEnv)))
  )

  # Sort based on the chosen criteria
  if (sort_by == "size") {
    size_df <- size_df %>% arrange(-size_numeric)
  } else if (sort_by != "name") {
    stop("Invalid sort_by parameter. Use 'size' or 'name'.")
  }

  # Find the length of the longest object name
  max_name_length <- max(nchar(size_df$name), nchar("All data objects"))

  # Use that length to determine the width for alignment
  output <- sprintf("%-*s | %s", max_name_length, size_df$name, size_df$size_display)

  # Print the objects and sizes
  cat(output, sep = "\n")

  # Print a line of dashes of the same width
  cat(paste(rep("-", max_name_length + 3 + max(nchar(size_df$size_display))), collapse = ""), "\n", sep = "")

  # Calculate and print the total size of all R objects
  total_size <- sum(size_df$size_numeric)
  size_display <- case_when(
    total_size > 1e12 ~ paste0(round(total_size / 1e12, 1), " Tb"),
    total_size > 1e9 ~ paste0(round(total_size / 1e9, 1), " Gb"),
    total_size > 1e6 ~ paste0(round(total_size / 1e6, 1), " Mb"),
    total_size > 1e3 ~ paste0(round(total_size / 1e3, 1), " Kb"),
    TRUE ~ paste0(round(total_size, 1), " bytes")
  )

  cat(sprintf("%-*s | %s\n", max_name_length, "All data objects", size_display))
}
