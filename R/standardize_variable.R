#' Standardize Variable
#'
#' This function standardizes a numeric vector according to the specified method.
#'
#' @description
#' The following methods are supported:
#' \itemize{
#'   \item "center": Simple centering subtracts the mean of the variable from all values. This effectively centers them around zero and can be useful when you want to remove the mean but not change the scale.
#'   \item "zscore": Z-score normalization standardizes a variable to have a mean of 0 and standard deviation of 1. It is useful when comparing variables that have different units or scales, and is often used in regression, PCA, and t-tests.
#'   \item "minmax": Min-Max scaling scales the variable so that its values fall between 0 and 1. It is useful for algorithms that require bounded inputs like neural networks, and for visualization.
#'   \item "maxabs": Max Abs scaling scales each observation by the maximum absolute value of the variable. This is useful for data that's centered but not strictly positive, such as text data represented by word frequencies.
#'   \item "robust": Robust scaling uses the median and the IQR, thus not influenced by outliers. It is ideal for data with outliers or when working with algorithms sensitive to outliers.
#'   \item "quantile": Quantile scaling scales the data between the 1st quartile and the 3rd quartile. Sometimes also referred to as "robust" scaling, it can be useful for data with extreme outliers.
#'   \item "unitvector": Unit Vector transformation scales the variable so that the resulting vector has a length (norm) of 1. This is useful when the scale of the variable is not informative, such as when dealing with text data.
#'   \item "decimalscale": Decimal Scaling moves the decimal point of values based on the maximum absolute value in the dataset, which also rescales the data between -1 and 1.
#'   \item "whitening": Whitening or Sphereing Transform de-correlates the features and scales them to have unit variance.
#'   \item "manhattanscale": Manhattan Scaling scales data based on the L1 norm (sum of absolute values).
#'   \item "rankscale": Rank Scaling replaces each data point with its rank in the sorted list of all data points. It's useful when you want to use the relative position of each data point rather than its exact value.
#' }
#'
#' @param x A numeric vector to be standardized.
#' @param method A string specifying the standardization method to use.
#'
#' @return A numeric vector with the standardized values.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- rnorm(100)
#' standardized_data <- standardize_variable(data, method = "zscore")
#' }
#'
#' @seealso \code{\link{scale}}
#'
#' @references
#' Izenman, Alan Julian. "Modern multivariate statistical techniques: Regression, classification,
#' and manifold learning." (2008).


standardize_variable <- function(x, method = "zscore") {
  # Ensure the method is lowercase for consistency
  method <- tolower(method)

  # Check that x is numeric
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }

  # Perform scaling
  if (method == "center") {
    # Simple Centering
    # This subtracts the mean of the variable from all values, effectively centering them around zero.
    # It can be useful when you want to remove the mean but not change the scale.
    result <- x - mean(x)
  } else if (method == "zscore") {
    # Z-Score normalization
    # This transformation standardizes a variable to have a mean of 0 and standard deviation of 1.
    # It's useful when comparing variables that have different units or scales, and is often used in regression, PCA, and t-tests.
    result <- (x - mean(x)) / sd(x)
  } else if (method == "minmax") {
    # Min-Max scaling
    # This scales the variable so that its values fall between 0 and 1.
    # It can be useful for algorithms that require bounded inputs like neural networks, or in visualization.
    result <- (x - min(x)) / (max(x) - min(x))
  } else if (method == "maxabs") {
    # Max Abs scaling
    # This scales each observation by the maximum absolute value of the variable.
    # Useful for data that's centered but not strictly positive, e.g., text data represented by word frequencies.
    result <- x / max(abs(x))
  } else if (method == "robust") {
    # Robust scaling
    # This uses the median and the IQR, thus not influenced by outliers.
    # Ideal for data with outliers or when working with algorithms sensitive to outliers.
    result <- (x - median(x)) / IQR(x)
  } else if (method == "quantile") {
    # Quantile scaling
    # This scales the data between the 1st quartile and the 3rd quartile.
    # Sometimes also referred to as "robust" scaling, it can be useful for data with extreme outliers.
    qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE, names = FALSE)
    result <- (x - qnt[1]) / (qnt[2] - qnt[1])
  } else if (method == "unitvector") {
    # Unit Vector transformation
    # This scales the variable so that the resulting vector has a length (norm) of 1.
    # Used when the scale of the variable is not informative, such as when dealing with text data.
    result <- x / sqrt(sum(x^2))
  } else if (method == "decimalscale") {
    # Decimal Scaling
    # This transformation moves the decimal point of values based on the maximum absolute value in the dataset.
    # This method also rescales the data between -1 and 1.
    result <- x / 10^max(nchar(floor(abs(x))))
  } else if (method == "whitening") {
    # Whitening or Sphereing Transform
    # This transformation de-correlates the features and scales them to have unit variance.
    result <- prcomp(x, scale. = TRUE)$x
  } else if (method == "manhattanscale") {
    # Manhattan Scaling
    # This method scales data based on the L1 norm (sum of absolute values).
    result <- x / sum(abs(x))
  } else if (method == "rankscale") {
    # Rank Scaling
    # This method replaces each data point with its rank in the sorted list of all data points.
    # It's useful when you want to use the relative position of each data point rather than its exact value.
    result <- rank(x)
  } else {
    stop(paste("Unknown method:", method))
  }

  return(result)
}
