#' Pretty Menu with Default Option
#'
#' Displays a menu with a list of choices and allows the user to make a selection.
#' If the user hits return without making a selection, a default option is chosen.
#'
#' @param choices A character vector containing the menu options.
#' @param title An optional character string specifying the title of the menu. Default is NULL.
#' @param default An integer specifying the default option to select if the user hits return without making a selection. Default is 1.
#'
#' @return The integer index of the selected option.
#'
#' @examples
#' choices <- c("Option 1", "Option 2", "Option 3")
#' selected <- menu_pretty(choices, title = "Please select an option:", default = 2)
#' cat("You selected option", selected, "\n")
#'
#' @export

menu_pretty <- function(choices, title = NULL, default = 1) {
  cat("\n")
  if (!is.null(title)) cat(title, "\n", paste0(rep("-", nchar(choices[1])), collapse = ""), "\n", sep = "")

  # Determine the width for option numbers
  width <- nchar(as.character(length(choices)))

  # Display the choices with aligned colons
  for (i in seq_along(choices)) {
    cat(sprintf(paste0("%", width, "d: %s\n"), i, choices[[i]]))
  }
  cat(paste0(rep("-", nchar(choices[1])), collapse = ""), "\nHit Return for default option [", default, "]\n", sep = "")

  # Get user input
  input <- readline(prompt = "> ")

  # If the input is empty, return the default
  if (input == "") {
    return(default)
  }

  # Convert input to a number and check if it is valid
  input_num <- as.numeric(input)
  if (!is.na(input_num) && input_num >= 1 && input_num <= length(choices)) {
    return(input_num)
  } else {
    cat("Invalid input. Please try again.\n")
    return(menu_pretty(choices, title, default))
  }
}
