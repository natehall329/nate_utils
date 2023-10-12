#' Close All Sinks and Reset Message Output
#'
#' This function closes all active sink connections and ensures that
#' subsequent outputs are displayed in the console.
#'
#' @export
#' @return NULL invisibly.
#'
#' @examples
#' \dontrun{
#'   sink("output.txt")
#'   cat("This will be written to the file.")
#'   close_all_sinks()
#'   cat("This will be shown in the console.")
#' }
close_all_sinks <- function() {
  while (sink.number() > 0) {
    sink(NULL)
  }
  options(warn = 0)
  options(message = -1)
  invisible(NULL)
}
