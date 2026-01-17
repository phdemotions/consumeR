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
#' @return An analysis_result object containing descriptive statistics, including
#'   sample size, location/dispersion measures, and optional extended metrics.
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
calculate_summary_stats <- function(data,
                                   include_all = TRUE,
                                   round_digits = 2) {

  # ============================================================================
  # INPUT VALIDATION (Standardized Utilities)
  # ============================================================================

  assert_beginner_safe(
    data,
    type = "numeric",
    allow_na = TRUE,
    min_length = 1,
    arg_name = "data",
    context = "summary statistics"
  )

  assert_beginner_safe(
    include_all,
    type = "logical",
    min_length = 1,
    max_length = 1,
    arg_name = "include_all",
    context = "summary statistics"
  )

  assert_beginner_safe(
    round_digits,
    type = "numeric",
    min_length = 1,
    max_length = 1,
    min_value = 0,
    max_value = 10,
    arg_name = "round_digits",
    context = "summary statistics"
  )

  data_sanitized <- data
  data_sanitized[is.nan(data_sanitized)] <- NA
  data_sanitized[is.infinite(data_sanitized)] <- NA

  n_missing <- sum(is.na(data_sanitized))
  data_clean <- data_sanitized[!is.na(data_sanitized)]

  assert_beginner_safe(
    data_clean,
    type = "numeric",
    allow_na = FALSE,
    min_length = 1,
    arg_name = "data",
    context = "summary statistics"
  )

  # ============================================================================
  # CALCULATE STATISTICS (APA 7 compliant)
  # ============================================================================

  # Sample size - how many observations we have
  n <- length(data_clean)

  # Mean - the average value (APA 7: report with M =)
  mean_value <- mean(data_clean)

  # Median - the middle value when data is sorted (Mdn)
  median_value <- median(data_clean)

  # Standard deviation - how spread out the data is (SD)
  # APA 7: Use SD (not s) for sample standard deviation
  sd_value <- if (n == 1) NA else sd(data_clean)

  # Minimum and maximum - the range boundaries
  min_value <- min(data_clean)
  max_value <- max(data_clean)

  # Quartiles - dividing data into quarters (Q1, Q3)
  q25_value <- quantile(data_clean, 0.25, names = FALSE)
  q75_value <- quantile(data_clean, 0.75, names = FALSE)

  # ============================================================================
  # PACKAGE RESULTS
  # ============================================================================

  specific_stats <- list(
    n_missing = n_missing,
    mean = round(mean_value, round_digits),
    median = round(median_value, round_digits),
    sd = if (is.na(sd_value)) NA else round(sd_value, round_digits),
    min = round(min_value, round_digits),
    max = round(max_value, round_digits),
    q25 = round(q25_value, round_digits),
    q75 = round(q75_value, round_digits)
  )

  # Add additional statistics if requested
  if (include_all) {
    # Variance - squared standard deviation
    if (n == 1) {
      variance_value <- NA
    } else {
      variance_value <- var(data_clean)
    }

    # Range - difference between max and min
    range_value <- max_value - min_value

    # IQR - interquartile range (Q3 - Q1)
    iqr_value <- IQR(data_clean)

    specific_stats$variance <- if (is.na(variance_value)) NA else round(variance_value, round_digits)
    specific_stats$range <- round(range_value, round_digits)
    specific_stats$iqr <- round(iqr_value, round_digits)
  }

  interpretation <- if (n == 1) {
    glue::glue(
      "The data contain a single observation (N = {n}) with a value of {specific_stats$mean}."
    )
  } else {
    glue::glue(
      "The data have a mean of {specific_stats$mean} (SD = {specific_stats$sd}, ",
      "Mdn = {specific_stats$median}, N = {n})."
    )
  }

  build_analysis_result(
    test_type = "summary_stats",
    test_name = "Descriptive Statistics",
    core_stats = list(
      n = n,
      p_value = NA_real_
    ),
    specific_stats = specific_stats,
    interpretation = interpretation
  )
}
