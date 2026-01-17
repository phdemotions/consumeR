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
calculate_summary_stats <- function(data,
                                   include_all = TRUE,
                                   round_digits = 2) {

  # ============================================================================
  # COMPREHENSIVE INPUT VALIDATION (Gold Standard)
  # ============================================================================

  # Check 1: Type validation
  if (!is.numeric(data)) {
    rlang::abort(c(
      "Data must be numeric",
      "x" = paste0("You provided: ", class(data)[1], " (", typeof(data), ")"),
      "i" = "Summary statistics require numeric data (numbers)",
      "i" = "Common mistake: Passing character or factor variables",
      ">" = "For character data: Convert with as.numeric()",
      ">" = "For factors: Convert with as.numeric(as.character())",
      ">" = "For data frames: Extract column with data$column_name"
    ))
  }

  # Check 2: Handle empty vector
  if (length(data) == 0) {
    rlang::abort(c(
      "Cannot calculate statistics on empty data",
      "x" = "You provided a vector with 0 elements",
      "i" = "Need at least 1 observation to calculate statistics",
      ">" = "Check: Did you subset incorrectly?",
      ">" = "Check: Is your variable name correct?",
      ">" = "Try: length(your_data) to see how many values you have"
    ))
  }

  # Check 3: Detect NaN values
  if (any(is.nan(data))) {
    nan_count <- sum(is.nan(data))
    rlang::warn(c(
      "NaN (Not a Number) values detected",
      "!" = paste0(nan_count, " NaN value(s) found"),
      "i" = "NaN indicates a calculation error (e.g., 0/0, Inf/Inf)",
      "i" = "These will be treated as missing (NA) and removed",
      ">" = "Check: How was this data created?",
      ">" = "Run: summary(your_data) to inspect"
    ))
    # Convert NaN to NA for consistent handling
    data[is.nan(data)] <- NA
  }

  # Check 4: Detect Infinite values
  if (any(is.infinite(data))) {
    inf_count <- sum(is.infinite(data))
    inf_pos <- sum(data == Inf, na.rm = TRUE)
    inf_neg <- sum(data == -Inf, na.rm = TRUE)
    rlang::warn(c(
      "Infinite values detected",
      "!" = paste0(inf_count, " infinite value(s): ",
                   if (inf_pos > 0) paste0(inf_pos, " Inf"), " ",
                   if (inf_neg > 0) paste0(inf_neg, " -Inf")),
      "i" = "Infinite values will bias statistics (mean, SD, etc.)",
      "i" = "These will be treated as missing (NA) and removed",
      "!" = "Common causes:",
      "  " = "  • Division by zero (x/0 = Inf)",
      "  " = "  • Overflow from very large calculations",
      "  " = "  • Data entry errors",
      ">" = "Inspect: your_data[is.infinite(your_data)]",
      ">" = "Consider: Removing or recoding these values"
    ))
    # Convert Inf to NA
    data[is.infinite(data)] <- NA
  }

  # Step 2: Remove missing values
  n_missing <- sum(is.na(data))
  data_clean <- data[!is.na(data)]

  # Check 5: Handle all-NA data
  if (length(data_clean) == 0) {
    rlang::abort(c(
      "No valid (non-missing) data points found",
      "x" = paste0("All ", length(data), " values are NA, NaN, or Inf"),
      "i" = "Cannot calculate statistics without any valid numbers",
      "!" = "Possible causes:",
      "  " = "  • Data import failed",
      "  " = "  • Wrong column selected",
      "  " = "  • Calculation error created only missing values",
      ">" = "Check: summary(your_original_data)",
      ">" = "Check: str(your_data) to see structure"
    ))
  }

  # Check 6: Warn about single value
  if (length(data_clean) == 1) {
    rlang::warn(c(
      "Only one valid data point",
      "!" = paste0("Value: ", data_clean[1]),
      "i" = "Statistics will be limited with n = 1:",
      "  " = "  • SD and variance cannot be calculated (need n >= 2)",
      "  " = "  • Mean = Median = Min = Max (all equal to the single value)",
      "  " = "  • Quartiles will all equal the value",
      ">" = "Consider: Do you have more data available?",
      ">" = "This is not enough for meaningful statistical analysis"
    ))
  }

  # Check 7: Warn about very small sample
  if (length(data_clean) >= 2 && length(data_clean) < 5) {
    rlang::warn(c(
      "Very small sample size",
      "!" = paste0("n = ", length(data_clean), " (after removing ",
                   n_missing, " missing)"),
      "i" = "Statistics will be unreliable with n < 5",
      "i" = "Confidence intervals will be very wide",
      "i" = "Outliers will have huge influence",
      ">" = "Recommendation: Collect more data before drawing conclusions",
      ">" = "Use with caution in any analysis"
    ))
  }

  # Check 8: Inform about missing values (if moderate amount)
  if (n_missing > 0) {
    pct_missing <- round(100 * n_missing / length(data), 1)

    if (pct_missing > 50) {
      rlang::warn(c(
        "More than half of data is missing",
        "!" = paste0(n_missing, " of ", length(data), " values are NA (",
                     pct_missing, "%)"),
        "i" = "Results based on only ", length(data_clean), " observations",
        "!" = "High missingness may indicate:",
        "  " = "  • Data collection problems",
        "  " = "  • Wrong variable selected",
        "  " = "  • Systematic bias (non-random missingness)",
        ">" = "Investigate: Why is so much data missing?",
        ">" = "Consider: Imputation or collecting more complete data"
      ))
    } else if (pct_missing > 20) {
      rlang::warn(c(
        "Substantial missing data",
        "!" = paste0(n_missing, " of ", length(data), " values are NA (",
                     pct_missing, "%)"),
        "i" = "Statistics computed on ", length(data_clean), " complete cases",
        ">" = "Consider: Investigating pattern of missingness"
      ))
    } else {
      message("Note: ", n_missing, " missing value(s) removed from calculations (",
              pct_missing, "% of data)")
    }
  }

  # Check 9: Detect zero variance (all values identical)
  if (length(unique(data_clean)) == 1) {
    rlang::warn(c(
      "Zero variance: all values are identical",
      "!" = paste0("All ", length(data_clean), " values = ", data_clean[1]),
      "i" = "SD and variance will be 0",
      "i" = "Range and IQR will be 0",
      "!" = "This is unusual and may indicate:",
      "  " = "  • Constant/fixed value (by design)",
      "  " = "  • Rounding error (all rounded to same value)",
      "  " = "  • Data entry error",
      "  " = "  • Wrong variable selected",
      ">" = "Check: Is this expected?",
      ">" = "Statistical tests will fail with zero variance"
    ))
  }

  # Check 10: Validate round_digits parameter
  if (!is.numeric(round_digits) || length(round_digits) != 1 ||
      round_digits < 0 || round_digits > 10) {
    rlang::abort(c(
      "Invalid 'round_digits' parameter",
      "x" = paste0("You provided: ", round_digits),
      "i" = "round_digits must be a single integer between 0 and 10",
      "i" = "APA 7 recommends: 2 decimals for most descriptive statistics",
      ">" = "Example: calculate_summary_stats(data, round_digits = 2)"
    ))
  }

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
  if (n == 1) {
    sd_value <- NA  # Cannot calculate SD with n=1
  } else {
    sd_value <- sd(data_clean)
  }

  # Minimum and maximum - the range boundaries
  min_value <- min(data_clean)
  max_value <- max(data_clean)

  # Quartiles - dividing data into quarters (Q1, Q3)
  q25_value <- quantile(data_clean, 0.25, names = FALSE)
  q75_value <- quantile(data_clean, 0.75, names = FALSE)

  # ============================================================================
  # PACKAGE RESULTS
  # ============================================================================

  # Create results list with core statistics
  # APA 7: Round to 2 decimal places for descriptive statistics
  results <- list(
    n = n,
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

    # Add these to our results
    results$variance <- if (is.na(variance_value)) NA else round(variance_value, round_digits)
    results$range <- round(range_value, round_digits)
    results$iqr <- round(iqr_value, round_digits)
  }

  # Add class for potential print method
  class(results) <- c("summary_stats", "list")

  return(results)
}
