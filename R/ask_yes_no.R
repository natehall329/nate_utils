#' Ask a yes/no question with default value
#'
#' This function prompts the user with a yes/no question and handles various inputs.
#' If the user input is not valid, it defaults to the specified default value.
#'
#' @param prompt_message The message to display as the prompt.
#' @param default The default value if the user input is invalid (default: "n").
#' @return A character vector "y" for yes, "n" for no.
#' @export
#' @examples
#' response <- ask_yes_no("Start an interactive slurm job with salloc?", default = "n")
#' cat("User response:", response, "\n")
#'

ask_yes_no <- function(prompt_message, default = "n") {
  ans <- readline(paste0(prompt_message, " (y/n | default: ", default, "): "))

  if (tolower(ans) %in% c("y", "yes")) {
    return("y")
  } else if (tolower(ans) %in% c("n", "no")) {
    return("n")
  } else {
    cat("Only y/n accepted. Set to default: ", default, "\n")
    return(default)
  }
}
