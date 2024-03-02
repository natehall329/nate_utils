#' Reorder Data Frames in List by Numeric Name
#'
#' This function takes a list of data frames and reorders them based on the
#' numeric values embedded within their names.
#'
#' @param df_list A list of data frames.
#' @return A list of data frames ordered by the numeric values extracted from their names.
#' @export
#'
#' @examples
#' df1 <- data.frame(x = 1:3)
#' df713 <- data.frame(y = 4:6)
#' df42 <- data.frame(z = 7:9)
#'
#' my_list <- list("42" = df42, "713" = df713, "1" = df1)
#' reordered_list <- reorder_list_numeric(my_list)

reorder_list_numeric <- function(df_list) {
  # Extract numeric parts of the names
  df_names_numeric <- as.numeric(gsub("[^0-9]", "", names(df_list)))

  # Order the data.frames according to the numeric values
  df_list[order(df_names_numeric)]
}
