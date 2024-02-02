#' Print a Tree-Like Structure of a List
#'
#' Recursively prints a tree-like structure of a list object with proper
#' indentation to visualize its hierarchy. This function aims to mimic the
#' output of `lobstr::tree` with full values shown and colorized output for
#' the list elements.
#'
#' @param x A list object to be printed as a tree structure.
#' @param indent A string used internally by the function to manage indentation.
#'   It is not intended to be set by the user.
#' @param last A logical value used internally to indicate if the current element
#'   is the last in its sublist. It is not intended to be set by the user.
#'
#' @examples
#' config <- list(
#'   run_behav = TRUE,
#'   paths = list(
#'     raw = list(
#'       behav = "/path/to/behav",
#'       eye = "/path/to/eye"
#'     )
#'   )
#' )
#' print_tree(config)
#'
#' @export
#' @importFrom crayon green

print_tree <- function(x, indent = "", last = TRUE) {
  names_x <- names(x)
  len <- length(x)

  for (i in seq_along(x)) {
    is_last <- (i == len)
    new_indent <- ifelse(is_last, "└─", "├─")
    cat(indent, new_indent, names_x[i], ": ", sep = "")

    if (is.list(x[[i]])) {
      cat("\n")
      new_branch <- ifelse(is_last, "   ", "│  ")
      print_tree(x[[i]], paste0(indent, new_branch), is_last)
    } else {
      cat(crayon::green(as.character(x[[i]])), "\n")
    }
  }

  if (last && indent != "") {
    cat(substr(indent, 1, nchar(indent)-3), "\n", sep = "")
  }
}
