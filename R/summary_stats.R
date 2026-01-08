#' Calculate Summary Statistics for Consumer Data
#'
#' This function calculates descriptive statistics for a numeric variable.
#' It is designed to be transparent and easy to understand for reviewers.
#' All calculations are performed using base R functions with clear,
#' step-by-step operations.
#'
#' @param data A numeric vector containing the data to summarize.
#'   Missing values (NA) are automatically removed before calculations.
#' @param include_all Logical. If TRUE, includes additional statistics
#'   like variance and range. Default is TRUE.
#' @param round_digits Integer. Number of decimal places for rounding results.
#'   Default is 2.
#'
#' @return A named list containing the following statistics:
#'   \itemize{
#'     \item \code{n}: Sample size (number of observations)
#'     \item \code{mean}: Arithmetic mean
#'     \item \code{median}: Median (50th percentile)
#'     \item \code{sd}: Standard deviation
#'     \item \code{min}: Minimum value
#'     \item \code{max}: Maximum value
#'     \item \code{q25}: First quartile (25th percentile)
#'     \item \code{q75}: Third quartile (75th percentile)
#'     \item \code{variance}: Variance (if include_all = TRUE)
#'     \item \code{range}: Range (max - min) (if include_all = TRUE)
#'     \item \code{iqr}: Interquartile range (if include_all = TRUE)
#'   }
#'
#' @examples
#' # Example 1: Basic usage with sample consumer spending data
#' spending <- c(45.2, 67.8, 23.4, 89.1, 34.5, 56.7, 78.9, 12.3)
#' calculate_summary_stats(spending)
#'
#' # Example 2: Data with missing values (automatically handled)
#' satisfaction <- c(7, 8, NA, 6, 9, 7, NA, 8, 7)
#' calculate_summary_stats(satisfaction)
#'
#' # Example 3: With fewer statistics and more decimal places
#' prices <- c(19.99, 24.99, 29.99, 34.99, 39.99)
#' calculate_summary_stats(prices, include_all = FALSE, round_digits = 3)
#'
#' @export
#' @importFrom stats mean sd median IQR quantile var
calculate_summary_stats <- function(data,
                                   include_all = TRUE,
                                   round_digits = 2) {

  # Step 1: Input validation
  # Check if data is numeric - this prevents errors from non-numeric input
  if (!is.numeric(data)) {
    stop("Error: 'data' must be a numeric vector. ",
         "You provided: ", class(data)[1])
  }

  # Step 2: Remove missing values
  # We remove NA values to ensure all calculations work properly
  # We also count how many were removed for transparency
  n_missing <- sum(is.na(data))
  data_clean <- data[!is.na(data)]

  # Check if we have any data left after removing NAs
  if (length(data_clean) == 0) {
    stop("Error: No valid (non-missing) data points found.")
  }

  # Inform user if missing values were removed
  if (n_missing > 0) {
    message("Note: ", n_missing, " missing value(s) removed from calculations.")
  }

  # Step 3: Calculate basic statistics
  # These are the core measures that describe the data

  # Sample size - how many observations we have
  n <- length(data_clean)

  # Mean - the average value
  mean_value <- mean(data_clean)

  # Median - the middle value when data is sorted
  median_value <- median(data_clean)

  # Standard deviation - how spread out the data is
  sd_value <- sd(data_clean)

  # Minimum and maximum - the range boundaries
  min_value <- min(data_clean)
  max_value <- max(data_clean)

  # Quartiles - dividing data into quarters
  q25_value <- quantile(data_clean, 0.25, names = FALSE)
  q75_value <- quantile(data_clean, 0.75, names = FALSE)

  # Step 4: Create results list with core statistics
  # We use a named list so results are clearly labeled
  results <- list(
    n = n,
    mean = round(mean_value, round_digits),
    median = round(median_value, round_digits),
    sd = round(sd_value, round_digits),
    min = round(min_value, round_digits),
    max = round(max_value, round_digits),
    q25 = round(q25_value, round_digits),
    q75 = round(q75_value, round_digits)
  )

  # Step 5: Add additional statistics if requested
  if (include_all) {
    # Variance - squared standard deviation
    variance_value <- var(data_clean)

    # Range - difference between max and min
    range_value <- max_value - min_value

    # IQR - interquartile range (Q3 - Q1)
    iqr_value <- IQR(data_clean)

    # Add these to our results
    results$variance <- round(variance_value, round_digits)
    results$range <- round(range_value, round_digits)
    results$iqr <- round(iqr_value, round_digits)
  }

  # Step 6: Return the results
  # The results are returned as a list that can be easily printed or used
  # in further analysis
  return(results)
}
