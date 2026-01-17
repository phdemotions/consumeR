#' Utility Functions for consumeR Package
#'
#' @description
#' Core utility functions for validation, formatting, and data manipulation
#' used throughout the consumeR package.
#'
#' @name utilities
NULL


#' Format P-Values for Publication
#'
#' @description
#' Formats p-values according to APA 7th edition style or plain format.
#' Handles edge cases like p < .001 and removes leading zeros as appropriate.
#'
#' @param p Numeric vector of p-values (between 0 and 1)
#' @param digits Integer. Number of decimal places (default 3)
#' @param style Character. Output style: "apa" (default) or "plain"
#'
#' @return Character vector of formatted p-values
#'
#' @details
#' **APA Style (default):**
#' - No leading zero: ".042" not "0.042"
#' - p < .001 for very small values
#' - p = .042 for reportable values
#'
#' **Plain Style:**
#' - Keeps leading zero: "0.042"
#' - p < 0.001 for very small values
#' - p = 0.042 for reportable values
#'
#' @examples
#' # APA style (no leading zero)
#' format_p(0.042)  # "p = .042"
#' format_p(0.0001) # "p < .001"
#' format_p(c(0.042, 0.0001, 0.523))
#'
#' # Plain style (with leading zero)
#' format_p(0.042, style = "plain")  # "p = 0.042"
#'
#' # More decimal places
#' format_p(0.04237, digits = 4)  # "p = .0424"
#'
#' @export
format_p <- function(p, digits = 3, style = c("apa", "plain")) {
  style <- match.arg(style)

  # ============================================================================
  # COMPREHENSIVE INPUT VALIDATION (Gold Standard)
  # ============================================================================

  # Check 1: Type validation
  if (!is.numeric(p)) {
    rlang::abort(c(
      "p-value must be numeric",
      "x" = paste0("You provided: ", class(p)[1], " (", typeof(p), ")"),
      "i" = "p-values are numbers between 0 and 1",
      "i" = "Common mistake: Passing character strings like \"0.05\"",
      ">" = "If p is in a data frame, try: my_data$p_value",
      ">" = "If p is character, convert: as.numeric(p)"
    ))
  }

  # Check 2: Handle empty input
  if (length(p) == 0) {
    rlang::abort(c(
      "Cannot format empty p-value vector",
      "x" = "You provided a vector with 0 elements",
      "i" = "Check: Did you subset incorrectly?",
      ">" = "Example: format_p(0.042) or format_p(c(0.01, 0.05))"
    ))
  }

  # Check 3: Detect and handle NaN (Not a Number)
  if (any(is.nan(p))) {
    nan_positions <- which(is.nan(p))
    rlang::abort(c(
      "NaN (Not a Number) detected in p-values",
      "x" = paste0("Position(s): ", paste(nan_positions, collapse = ", ")),
      "i" = "NaN usually indicates: division by zero or invalid calculation",
      "i" = "This means your statistical test failed silently",
      "!" = "Common causes:",
      "  " = "  • Zero variance in your data (all values identical)",
      "  " = "  • Perfect separation in logistic regression",
      "  " = "  • Invalid input to statistical function",
      ">" = "Run: summary(your_data) to check for problems",
      ">" = "Check: Do all groups have variation? sd(your_variable) > 0?"
    ))
  }

  # Check 4: Detect and handle Infinite values
  if (any(is.infinite(p))) {
    inf_positions <- which(is.infinite(p))
    inf_values <- p[inf_positions]
    rlang::abort(c(
      "Infinite p-values detected",
      "x" = paste0("Position(s): ", paste(inf_positions, collapse = ", "),
                   " = ", paste(inf_values, collapse = ", ")),
      "i" = "p-values must be finite numbers between 0 and 1",
      "!" = "This indicates a serious problem with your statistical test",
      "!" = "Possible causes:",
      "  " = "  • Zero variance (constant variable): sd = 0",
      "  " = "  • Extreme outliers or data errors",
      "  " = "  • Sample size too small (n < 3)",
      "  " = "  • Perfect multicollinearity in regression",
      ">" = "Diagnostics to run:",
      ">" = "  summary(your_data)  # Check for issues",
      ">" = "  apply(your_data, 2, sd, na.rm=TRUE)  # Check variances",
      ">" = "  boxplot(your_data)  # Visual inspection"
    ))
  }

  # Check 5: Range validation (excluding NA which we handle separately)
  invalid_range <- !is.na(p) & (p < 0 | p > 1)
  if (any(invalid_range)) {
    bad_positions <- which(invalid_range)
    bad_values <- p[bad_positions]
    rlang::abort(c(
      "p-values must be between 0 and 1",
      "x" = paste0("Invalid value(s) at position(s) ",
                   paste(bad_positions, collapse = ", "), ": ",
                   paste(round(bad_values, 4), collapse = ", ")),
      "i" = "p-values represent probabilities and must be in [0, 1]",
      "!" = "Values < 0 or > 1 indicate a calculation error",
      ">" = "Check: Is this really a p-value?",
      ">" = "Check: Did you accidentally pass a test statistic instead?"
    ))
  }

  # Check 6: Validate digits parameter
  if (!is.numeric(digits) || length(digits) != 1 || digits < 1 || digits > 10) {
    rlang::abort(c(
      "Invalid 'digits' parameter",
      "x" = paste0("You provided: ", digits),
      "i" = "digits must be a single integer between 1 and 10",
      "i" = "APA 7th edition recommends: 2-3 digits for p-values",
      ">" = "Example: format_p(0.042, digits = 3)"
    ))
  }

  # Check 7: Warn if all values are NA
  if (all(is.na(p))) {
    rlang::warn(c(
      "All p-values are NA (missing)",
      "!" = "Returning NA for all values",
      "i" = "This usually means your statistical test couldn't run",
      ">" = "Check for: missing data, insufficient sample size, or invalid inputs"
    ))
    return(rep(NA_character_, length(p)))
  }

  # ============================================================================
  # APA 7TH EDITION FORMATTING
  # ============================================================================

  # Determine threshold for "p < " reporting (APA 7: typically .001)
  threshold <- 10^(-digits)

  # APA 7 Note: p-values should be reported with 2-3 decimal places
  # and should not be reported as "p = 0.000" or "p = 1.000"
  if (digits > 4 && style == "apa") {
    rlang::warn(c(
      "Unusual precision for APA style",
      "!" = paste0("You requested ", digits, " digits"),
      "i" = "APA 7th edition recommends 2-3 decimal places for p-values",
      "i" = "More precision doesn't add meaningful information",
      ">" = "Consider: digits = 3 (default)"
    ))
  }

  # Format each p-value according to APA 7 standards
  formatted <- vapply(p, function(p_val) {
    # Handle NA values
    if (is.na(p_val)) {
      return(NA_character_)
    }

    # Special case: Exact zero (computationally)
    # APA 7: Never report p = 0.000, always use p < .001
    if (p_val == 0) {
      if (style == "apa") {
        return("p < .001")
      } else {
        return("p < 0.001")
      }
    }

    # Special case: Exact one (p = 1.000)
    # APA 7: This is extremely rare and usually indicates an error
    if (p_val == 1) {
      rlang::warn(c(
        "p-value of exactly 1.000 detected",
        "!" = "This is extremely unusual in real data",
        "i" = "A p-value of 1.0 means absolutely no evidence against the null",
        "i" = "This often indicates a problem with the analysis",
        ">" = "Double-check: your data, test choice, and assumptions"
      ))
      if (style == "apa") {
        return("p = 1.000")
      } else {
        return("p = 1.000")
      }
    }

    # Very small p-values: p < .001 (APA 7 standard)
    if (p_val < threshold) {
      if (style == "apa") {
        # APA 7: Remove leading zero
        threshold_str <- sub("^0", "", sprintf(paste0("%.", digits, "f"), threshold))
        return(paste0("p < ", threshold_str))
      } else {
        return(paste0("p < ", sprintf(paste0("%.", digits, "f"), threshold)))
      }
    }

    # Regular p-values: format with specified precision
    # APA 7: Use "p = " followed by value without leading zero
    p_str <- sprintf(paste0("%.", digits, "f"), p_val)

    if (style == "apa") {
      # APA 7: Remove leading zero (e.g., .042 not 0.042)
      p_str <- sub("^0", "", p_str)
    }

    paste0("p = ", p_str)
  }, character(1))

  formatted
}


#' Format Sample Sizes for Publication
#'
#' @description
#' Formats sample sizes with proper notation (N for total, n for subgroup).
#'
#' @param n Integer or numeric vector of sample sizes
#' @param type Character. "N" for total sample (default) or "n" for subgroup
#' @param include_label Logical. Include "N = " or "n = " prefix? (default TRUE)
#'
#' @return Character vector of formatted sample sizes
#'
#' @examples
#' # Total sample
#' format_n(150)  # "N = 150"
#'
#' # Subgroup sample
#' format_n(45, type = "n")  # "n = 45"
#'
#' # Multiple values
#' format_n(c(100, 50), type = "n")  # c("n = 100", "n = 50")
#'
#' # Without label
#' format_n(150, include_label = FALSE)  # "150"
#'
#' @export
format_n <- function(n, type = c("N", "n"), include_label = TRUE) {
  type <- match.arg(type)

  # ============================================================================
  # COMPREHENSIVE INPUT VALIDATION (Gold Standard)
  # ============================================================================

  # Check 1: Type validation
  if (!is.numeric(n)) {
    rlang::abort(c(
      "Sample size must be numeric",
      "x" = paste0("You provided: ", class(n)[1], " (", typeof(n), ")"),
      "i" = "Sample sizes are counts of observations (whole numbers)",
      "i" = "Common mistake: Passing character strings like \"150\"",
      ">" = "If n is in a data frame, try: nrow(my_data)",
      ">" = "To count observations: length(my_variable)",
      ">" = "If n is character, convert: as.numeric(n)"
    ))
  }

  # Check 2: Handle empty input
  if (length(n) == 0) {
    rlang::abort(c(
      "Cannot format empty sample size vector",
      "x" = "You provided a vector with 0 elements",
      "i" = "Check: Did you subset incorrectly?",
      ">" = "Example: format_n(150) or format_n(c(100, 50))"
    ))
  }

  # Check 3: Detect NaN (Not a Number)
  if (any(is.nan(n))) {
    nan_positions <- which(is.nan(n))
    rlang::abort(c(
      "NaN (Not a Number) detected in sample sizes",
      "x" = paste0("Position(s): ", paste(nan_positions, collapse = ", ")),
      "i" = "Sample sizes must be valid numbers",
      "!" = "NaN indicates a calculation error occurred",
      ">" = "Check: How are you calculating sample size?",
      ">" = "Use: nrow(data) or sum(!is.na(variable))"
    ))
  }

  # Check 4: Detect Infinite values
  if (any(is.infinite(n))) {
    inf_positions <- which(is.infinite(n))
    rlang::abort(c(
      "Infinite sample size detected",
      "x" = paste0("Position(s): ", paste(inf_positions, collapse = ", ")),
      "i" = "Sample sizes must be finite whole numbers",
      "!" = "This indicates a calculation error",
      ">" = "Check your sample size calculation"
    ))
  }

  # Check 5: Detect negative values
  if (any(n < 0, na.rm = TRUE)) {
    neg_positions <- which(n < 0)
    neg_values <- n[neg_positions]
    rlang::abort(c(
      "Sample size cannot be negative",
      "x" = paste0("Negative value(s) at position(s) ",
                   paste(neg_positions, collapse = ", "), ": ",
                   paste(neg_values, collapse = ", ")),
      "i" = "Sample sizes are counts and must be positive (or zero)",
      "i" = "A negative sample size is mathematically impossible",
      ">" = "Check: Did you accidentally subtract in the wrong order?",
      ">" = "Check: Are you using the correct variable?"
    ))
  }

  # Check 6: Warn about zero sample size
  if (any(n == 0, na.rm = TRUE)) {
    zero_positions <- which(n == 0)
    rlang::warn(c(
      "Zero sample size detected",
      "!" = paste0("Position(s): ", paste(zero_positions, collapse = ", ")),
      "i" = "A sample size of 0 means no observations",
      "i" = "Statistical tests cannot be performed with n = 0",
      ">" = "Check: Is this group empty? Did filtering remove all cases?"
    ))
  }

  # Check 7: Detect decimals and warn
  # APA 7: Sample sizes must be whole numbers (you can't have 150.5 people)
  has_decimals <- !is.na(n) & (n != round(n))
  if (any(has_decimals)) {
    decimal_positions <- which(has_decimals)
    original_vals <- n[decimal_positions]
    rounded_vals <- round(n[decimal_positions])

    rlang::warn(c(
      "Decimal sample size detected - rounding to nearest integer",
      "!" = "APA 7: Sample sizes must be whole numbers",
      "!" = paste0("Position(s) ", paste(decimal_positions, collapse = ", "), ":"),
      "  " = paste0("  Original: ", paste(original_vals, collapse = ", ")),
      "  " = paste0("  Rounded:  ", paste(rounded_vals, collapse = ", ")),
      "i" = "Sample sizes are counts of observations (people, items, trials)",
      "i" = "You cannot have fractional observations (e.g., 150.5 participants)",
      ">" = "This warning is usually harmless if values are very close to integers",
      ">" = "If decimals are far from integers, check your calculation"
    ))
  }

  # Check 8: Warn about very large sample sizes (potential data entry error)
  if (any(n > 1e7, na.rm = TRUE)) {  # More than 10 million
    large_positions <- which(n > 1e7)
    large_values <- n[large_positions]
    rlang::warn(c(
      "Extremely large sample size detected",
      "!" = paste0("Value(s): ", paste(formatC(large_values, format = "f", big.mark = ",", digits = 0), collapse = ", ")),
      "i" = "Sample sizes > 10,000,000 are very unusual",
      "i" = "This might indicate a data entry error or wrong variable",
      ">" = "Double-check: Is this really your sample size?",
      ">" = "Common mistake: Using row numbers instead of counts"
    ))
  }

  # Check 9: Warn about very small sample sizes for statistics
  if (any(n > 0 & n < 3, na.rm = TRUE)) {
    tiny_positions <- which(n > 0 & n < 3)
    tiny_values <- n[tiny_positions]
    rlang::warn(c(
      "Very small sample size detected",
      "!" = paste0("n = ", paste(tiny_values, collapse = ", "),
                   " at position(s) ", paste(tiny_positions, collapse = ", ")),
      "i" = "Most statistical tests require n >= 3 per group",
      "i" = "You cannot calculate standard deviation with n < 2",
      "i" = "Results will be unreliable or impossible to compute",
      ">" = "Consider: Collecting more data or combining groups"
    ))
  }

  # Check 10: Warn if all values are NA
  if (all(is.na(n))) {
    rlang::warn(c(
      "All sample sizes are NA (missing)",
      "!" = "Returning NA for all values",
      "i" = "This usually means sample size couldn't be calculated",
      ">" = "Check: Is your data loaded correctly?",
      ">" = "Try: nrow(your_data) to count rows"
    ))
    return(rep(NA_character_, length(n)))
  }

  # ============================================================================
  # APA 7TH EDITION FORMATTING
  # ============================================================================

  # APA 7: Sample sizes are whole numbers
  # APA 7: Use "N" for total sample, "n" for subsample/group
  # APA 7: Format with commas for large numbers (e.g., N = 1,250)

  # Round to integers (with validation already done above)
  n_int <- as.integer(round(n))

  # Format with commas for readability (APA 7 style for numbers > 999)
  n_formatted <- vapply(n_int, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    }
    # Add commas for thousands (1,250 not 1250)
    formatC(x, format = "f", big.mark = ",", digits = 0)
  }, character(1))

  # Add label if requested
  if (include_label) {
    # APA 7: Use italic N or n in publications (we use plain text here)
    # Users should italicize in their manuscripts
    paste0(type, " = ", n_formatted)
  } else {
    n_formatted
  }
}


#' Format Estimates with Confidence Intervals
#'
#' @description
#' Formats point estimates with confidence intervals in the style:
#' "2.34 [1.23, 3.45]"
#'
#' @param est Numeric vector of point estimates
#' @param lo Numeric vector of lower confidence bounds
#' @param hi Numeric vector of upper confidence bounds
#' @param digits Integer. Number of decimal places (default 2)
#' @param percent Logical. Format as percentages? (default FALSE)
#'
#' @return Character vector of formatted estimates with CIs
#'
#' @examples
#' # Single value
#' format_est_ci(2.34, 1.23, 3.45)  # "2.34 [1.23, 3.45]"
#'
#' # Multiple values
#' format_est_ci(
#'   est = c(2.34, 5.67),
#'   lo = c(1.23, 4.56),
#'   hi = c(3.45, 6.78)
#' )
#'
#' # More decimal places
#' format_est_ci(2.3456, 1.2345, 3.4567, digits = 3)
#'
#' # As percentages
#' format_est_ci(0.234, 0.123, 0.345, percent = TRUE)  # "23.4% [12.3%, 34.5%]"
#'
#' @export
format_est_ci <- function(est, lo, hi, digits = 2, percent = FALSE) {
  # ============================================================================
  # COMPREHENSIVE INPUT VALIDATION (Gold Standard)
  # ============================================================================

  # Check 1: Validate all inputs are provided
  if (missing(est) || missing(lo) || missing(hi)) {
    rlang::abort(c(
      "Missing required arguments",
      "x" = "You must provide all three: est, lo, hi",
      "i" = "est = point estimate (e.g., mean, coefficient)",
      "i" = "lo = lower confidence bound",
      "i" = "hi = upper confidence bound",
      ">" = "Example: format_est_ci(est = 2.5, lo = 1.2, hi = 3.8)",
      ">" = "See ?format_est_ci for more examples"
    ))
  }

  # Check 2: Type validation
  if (!is.numeric(est)) {
    rlang::abort(c(
      "Estimate (est) must be numeric",
      "x" = paste0("You provided: ", class(est)[1]),
      "i" = "Point estimates are numbers (e.g., means, coefficients)",
      ">" = "If est is character, convert: as.numeric(est)"
    ))
  }

  if (!is.numeric(lo)) {
    rlang::abort(c(
      "Lower bound (lo) must be numeric",
      "x" = paste0("You provided: ", class(lo)[1]),
      "i" = "Confidence interval bounds are numbers",
      ">" = "If lo is character, convert: as.numeric(lo)"
    ))
  }

  if (!is.numeric(hi)) {
    rlang::abort(c(
      "Upper bound (hi) must be numeric",
      "x" = paste0("You provided: ", class(hi)[1]),
      "i" = "Confidence interval bounds are numbers",
      ">" = "If hi is character, convert: as.numeric(hi)"
    ))
  }

  # Check 3: Handle empty inputs
  if (length(est) == 0 || length(lo) == 0 || length(hi) == 0) {
    rlang::abort(c(
      "Cannot format empty vectors",
      "x" = paste0("Lengths: est=", length(est), ", lo=", length(lo), ", hi=", length(hi)),
      ">" = "Check: Did you subset incorrectly?"
    ))
  }

  # Check 4: Verify lengths match
  len <- length(est)
  if (length(lo) != len || length(hi) != len) {
    rlang::abort(c(
      "est, lo, and hi must have the same length",
      "x" = paste0("Lengths: est = ", length(est), ", lo = ", length(lo), ", hi = ", length(hi)),
      "i" = "Each estimate needs exactly one CI (lower and upper bound)",
      ">" = "If formatting multiple estimates, all vectors must be same length",
      ">" = "Example: format_est_ci(c(2.5, 3.1), c(1.2, 2.0), c(3.8, 4.2))"
    ))
  }

  # Check 5: Detect NaN values
  if (any(is.nan(est)) || any(is.nan(lo)) || any(is.nan(hi))) {
    rlang::abort(c(
      "NaN (Not a Number) detected in estimates or confidence bounds",
      "!" = "This indicates a calculation error in your statistical test",
      ">" = "Check: Did your statistical test fail?",
      ">" = "Common causes: zero variance, invalid inputs"
    ))
  }

  # Check 6: Detect Infinite values
  if (any(is.infinite(est)) || any(is.infinite(lo)) || any(is.infinite(hi))) {
    rlang::abort(c(
      "Infinite values detected in estimates or confidence bounds",
      "!" = "Estimates and CIs must be finite numbers",
      "i" = "This indicates a serious problem with your analysis",
      ">" = "Check: Are there extreme outliers?",
      ">" = "Check: Is there zero variance in your data?"
    ))
  }

  # Check 7: Validate CI bounds are in correct order
  # APA 7: CIs must have lower < upper (mathematically required)
  invalid_ci <- !is.na(lo) & !is.na(hi) & (lo > hi)
  if (any(invalid_ci)) {
    bad_positions <- which(invalid_ci)
    first_bad <- bad_positions[1]
    rlang::abort(c(
      "Invalid confidence interval: lower bound exceeds upper bound",
      "x" = paste0("Position ", first_bad, ": lo = ", round(lo[first_bad], 4),
                   ", hi = ", round(hi[first_bad], 4)),
      "!" = "All problematic positions: ", paste(bad_positions, collapse = ", "),
      "i" = "Confidence intervals must have: lower ≤ upper",
      "i" = "This is mathematically required, not optional",
      ">" = "Did you accidentally swap lo and hi?",
      ">" = "Check your statistical output for errors"
    ))
  }

  # Check 8: Warn about zero-width CIs (unusual)
  # APA 7: A CI with width=0 suggests zero variance or calculation error
  zero_width <- !is.na(lo) & !is.na(hi) & (abs(hi - lo) < 1e-10)
  if (any(zero_width)) {
    zero_positions <- which(zero_width)
    rlang::warn(c(
      "Zero-width confidence interval detected",
      "!" = paste0("Position(s): ", paste(zero_positions, collapse = ", ")),
      "!" = "Lower and upper bounds are identical",
      "i" = "This is very unusual and may indicate:",
      "  " = "  • Extremely precise estimate (rare in real data)",
      "  " = "  • Zero variance in your data",
      "  " = "  • Calculation error or bug",
      "  " = "  • Perfect fit (R² = 1, which is suspicious)",
      ">" = "Double-check your statistical analysis",
      ">" = "Inspect: plot(your_data) to look for issues"
    ))
  }

  # Check 9: Warn if estimate is outside its own CI
  # APA 7: Point estimate should be within [lo, hi]
  outside_ci <- !is.na(est) & !is.na(lo) & !is.na(hi) & (est < lo | est > hi)
  if (any(outside_ci)) {
    bad_positions <- which(outside_ci)
    first_bad <- bad_positions[1]
    rlang::warn(c(
      "Point estimate falls outside confidence interval",
      "!" = paste0("Position ", first_bad, ": est = ", round(est[first_bad], 4),
                   " not in [", round(lo[first_bad], 4), ", ", round(hi[first_bad], 4), "]"),
      "!" = "All problematic positions: ", paste(bad_positions, collapse = ", "),
      "i" = "The point estimate should always be within its CI",
      "i" = "This indicates an error in your statistical output",
      ">" = "Check: Did you match the right estimate with the right CI?",
      ">" = "Check: Are you using the correct CI (not a different statistic)?"
    ))
  }

  # Check 10: Validate digits parameter
  if (!is.numeric(digits) || length(digits) != 1 || digits < 0 || digits > 10) {
    rlang::abort(c(
      "Invalid 'digits' parameter",
      "x" = paste0("You provided: ", digits),
      "i" = "digits must be a single integer between 0 and 10",
      "i" = "APA 7th edition: Use 2 digits for most statistics",
      ">" = "Example: format_est_ci(2.345, 1.234, 3.456, digits = 2)"
    ))
  }

  # Check 11: Warn about excessive precision
  # APA 7: Typically 2 decimal places for most statistics
  if (digits > 4) {
    rlang::warn(c(
      "Unusually high precision requested",
      "!" = paste0("You requested ", digits, " decimal places"),
      "i" = "APA 7th edition: Use 2 decimal places for most statistics",
      "i" = "Exceptions: Use 3 decimals for p-values, correlations",
      "i" = "More precision rarely adds meaningful information",
      ">" = "Consider: digits = 2 (default)"
    ))
  }

  # Check 12: Warn if all values are NA
  if (all(is.na(est)) && all(is.na(lo)) && all(is.na(hi))) {
    rlang::warn(c(
      "All estimates and confidence bounds are NA",
      "!" = "Returning NA for all formatted values",
      "i" = "This means your statistical test couldn't produce results",
      ">" = "Check: Did the analysis fail? Are there error messages?"
    ))
    return(rep(NA_character_, len))
  }

  # ============================================================================
  # APA 7TH EDITION FORMATTING
  # ============================================================================

  # APA 7: Format as "estimate [lower, upper]" with square brackets
  # APA 7: Use 2 decimal places for most statistics
  # APA 7: Include % sign for percentages, placed after each number

  # Format based on whether we want percentages
  if (percent) {
    # APA 7: For percentages, use format like "23.4% [12.3%, 34.5%]"
    est_str <- paste0(format(round(est * 100, digits), nsmall = digits), "%")
    lo_str <- paste0(format(round(lo * 100, digits), nsmall = digits), "%")
    hi_str <- paste0(format(round(hi * 100, digits), nsmall = digits), "%")
  } else {
    # APA 7: Standard format for coefficients, means, etc.
    est_str <- format(round(est, digits), nsmall = digits)
    lo_str <- format(round(lo, digits), nsmall = digits)
    hi_str <- format(round(hi, digits), nsmall = digits)
  }

  # APA 7: Square brackets for CIs, comma separator
  # Format: "2.34 [1.23, 3.45]"
  paste0(est_str, " [", lo_str, ", ", hi_str, "]")
}


#' Assert Required Variables are Present
#'
#' @description
#' Validates that required variables exist in a data frame.
#' Provides informative error messages listing missing variables and
#' suggesting available alternatives.
#'
#' @param df Data frame to check
#' @param vars Character vector of required variable names
#' @param label Character. Descriptive label for error message (e.g., "alpha calculation")
#'
#' @return Invisible TRUE if all variables present (for piping)
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:5, y = 6:10)
#'
#' # Success - no error
#' assert_vars_present(df, c("x", "y"), "my analysis")
#'
#' # Error - missing variable
#' assert_vars_present(df, c("x", "z"), "my analysis")
#' # Error: Missing variables for my analysis: z
#' #   Available variables: x, y
#' }
#'
#' @export
assert_vars_present <- function(df, vars, label = NULL) {
  # ============================================================================
  # COMPREHENSIVE INPUT VALIDATION (Gold Standard)
  # ============================================================================

  # Check 1: Validate df is a data frame
  if (!is.data.frame(df)) {
    rlang::abort(c(
      "df must be a data frame",
      "x" = paste0("You provided: ", class(df)[1]),
      "i" = "This function checks for variables (columns) in a data frame",
      ">" = "If you have a matrix, convert: as.data.frame(your_matrix)",
      ">" = "If you have a vector, create data frame: data.frame(var = your_vector)"
    ))
  }

  # Check 2: Validate vars is character
  if (!is.character(vars)) {
    rlang::abort(c(
      "vars must be a character vector",
      "x" = paste0("You provided: ", class(vars)[1]),
      "i" = "Variable names must be quoted strings",
      ">" = "Example: assert_vars_present(df, c(\"age\", \"gender\"))",
      ">" = "Not: assert_vars_present(df, c(age, gender))"
    ))
  }

  # Check 3: Handle empty vars vector
  if (length(vars) == 0) {
    rlang::warn(c(
      "No variables specified to check",
      "!" = "You provided an empty character vector",
      "i" = "Function returns TRUE (vacuously true - nothing to check)",
      ">" = "This is usually a mistake - did you forget to specify variables?"
    ))
    return(invisible(TRUE))
  }

  # Check 4: Warn if df has no columns
  if (ncol(df) == 0) {
    rlang::abort(c(
      "Data frame has no columns",
      "x" = "Cannot check for variables in an empty data frame",
      "i" = "Your data frame has 0 columns",
      ">" = "Check: Did you load your data correctly?",
      ">" = "Try: names(df) to see column names"
    ))
  }

  # ============================================================================
  # FUZZY MATCHING FOR HELPFUL ERROR MESSAGES
  # ============================================================================

  missing_vars <- setdiff(vars, names(df))

  if (length(missing_vars) > 0) {
    context <- if (!is.null(label)) paste0(" for ", label) else ""

    # Find similar variable names using fuzzy matching (Levenshtein distance)
    suggestions <- list()
    for (missing in missing_vars) {
      # Calculate edit distances to all existing variables
      distances <- utils::adist(missing, names(df), ignore.case = TRUE)[1, ]

      # Consider matches within edit distance of 3 (typos, minor differences)
      # Also check for partial matches
      close_matches <- character()

      # Exact case-insensitive match
      exact_match_idx <- which(tolower(names(df)) == tolower(missing))
      if (length(exact_match_idx) > 0) {
        close_matches <- c(close_matches, names(df)[exact_match_idx])
      }

      # Small edit distance (typos)
      small_dist_idx <- which(distances <= 3 & distances > 0)
      if (length(small_dist_idx) > 0) {
        close_matches <- c(close_matches, names(df)[small_dist_idx])
      }

      # Partial string matches (one is substring of other)
      for (i in seq_along(names(df))) {
        if (grepl(missing, names(df)[i], ignore.case = TRUE) ||
            grepl(names(df)[i], missing, ignore.case = TRUE)) {
          close_matches <- c(close_matches, names(df)[i])
        }
      }

      # Remove duplicates and limit to top 3 suggestions
      close_matches <- unique(close_matches)
      if (length(close_matches) > 3) {
        # Sort by edit distance and take top 3
        dists <- utils::adist(missing, close_matches, ignore.case = TRUE)[1, ]
        close_matches <- close_matches[order(dists)][1:3]
      }

      if (length(close_matches) > 0) {
        suggestions[[missing]] <- close_matches
      }
    }

    # Build comprehensive error message
    err_bullets <- c(
      paste0("Missing variable", ifelse(length(missing_vars) > 1, "s", ""),
             context, ": ", paste(missing_vars, collapse = ", "))
    )

    # Add specific suggestions for each missing variable
    if (length(suggestions) > 0) {
      err_bullets <- c(err_bullets, "!" = "Did you mean:")
      for (missing in names(suggestions)) {
        suggestion_text <- paste0("  '", missing, "' → '",
                                  paste(suggestions[[missing]], collapse = "' or '"), "'?")
        err_bullets <- c(err_bullets, "  " = suggestion_text)
      }
    }

    # Add list of all available variables
    # For large data frames, limit the display
    all_vars <- names(df)
    if (length(all_vars) <= 20) {
      err_bullets <- c(err_bullets,
        "i" = paste0("Available variables (", length(all_vars), "): ",
                     paste(all_vars, collapse = ", "))
      )
    } else {
      # Show first 15 and indicate there are more
      err_bullets <- c(err_bullets,
        "i" = paste0("Available variables (showing 15 of ", length(all_vars), "): ",
                     paste(all_vars[1:15], collapse = ", "), ", ...")
      )
      err_bullets <- c(err_bullets,
        ">" = "Use names(your_data) to see all column names"
      )
    }

    # Add general troubleshooting tips
    err_bullets <- c(err_bullets,
      "!" = "Common causes:",
      "  " = "  • Typo in variable name (check spelling)",
      "  " = "  • Case sensitivity: 'Age' ≠ 'age' ≠ 'AGE'",
      "  " = "  • Variable not loaded (check your data import)",
      "  " = "  • Wrong data frame (check you're using correct object)"
    )

    err_bullets <- c(err_bullets,
      ">" = "Debugging steps:",
      ">" = "  names(your_data)  # See all column names",
      ">" = "  str(your_data)    # See data structure",
      ">" = "  View(your_data)   # Open in viewer (RStudio)"
    )

    rlang::abort(err_bullets)
  }

  invisible(TRUE)
}


#' Clean Variable Names Safely
#'
#' @description
#' Wrapper around janitor::clean_names() with graceful handling when
#' janitor is not installed.
#'
#' @param df Data frame with names to clean
#' @param ... Additional arguments passed to janitor::clean_names()
#'
#' @return Data frame with cleaned names (if janitor available) or original df
#'
#' @details
#' If janitor is not installed, returns the original data frame with a warning.
#' This allows the function to be used in contexts where janitor is optional.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(`First Name` = "John", `Last Name` = "Doe")
#' clean_names_safe(df)  # first_name, last_name (if janitor installed)
#' }
#'
#' @export
clean_names_safe <- function(df, ...) {
  # ============================================================================
  # COMPREHENSIVE INPUT VALIDATION (Gold Standard)
  # ============================================================================

  # Check 1: Validate df is a data frame
  if (!is.data.frame(df)) {
    rlang::abort(c(
      "df must be a data frame",
      "x" = paste0("You provided: ", class(df)[1]),
      "i" = "This function cleans column names in data frames",
      ">" = "If you have a matrix, convert: as.data.frame(your_matrix)",
      ">" = "If you have a vector, you don't need to clean names"
    ))
  }

  # Check 2: Handle empty data frame (0 rows is OK, 0 columns is not useful)
  if (ncol(df) == 0) {
    rlang::warn(c(
      "Data frame has no columns to clean",
      "!" = "Your data frame has 0 columns",
      "i" = "Returning empty data frame unchanged",
      ">" = "Check: Did you load your data correctly?",
      ">" = "Try: str(df) to inspect structure"
    ))
    return(df)
  }

  # Check 3: Warn about single column (cleaning is less useful)
  if (ncol(df) == 1) {
    rlang::warn(c(
      "Data frame has only one column",
      "!" = paste0("Column name: '", names(df)[1], "'"),
      "i" = "Name cleaning is less critical for single columns",
      "i" = "Proceeding with cleaning anyway"
    ))
  }

  # Check 4: Detect if names are already clean (snake_case)
  # APA 7 / best practices: use lowercase with underscores
  all_clean <- all(grepl("^[a-z][a-z0-9_]*$", names(df)))
  if (all_clean && nchar(paste(names(df), collapse = "")) > 0) {
    rlang::inform(c(
      "Column names are already clean (snake_case)",
      "i" = "No changes needed",
      "i" = "All names follow best practices: lowercase with underscores"
    ))
    return(df)
  }

  # Check 5: Detect problematic column names and warn
  problematic_names <- character()

  # Check for names with spaces
  if (any(grepl(" ", names(df)))) {
    space_names <- names(df)[grepl(" ", names(df))]
    problematic_names <- c(problematic_names,
                          paste0("Spaces: ", paste(head(space_names, 3), collapse = ", ")))
  }

  # Check for names with special characters
  if (any(grepl("[^a-zA-Z0-9_\\.]", names(df)))) {
    special_names <- names(df)[grepl("[^a-zA-Z0-9_\\.]", names(df))]
    problematic_names <- c(problematic_names,
                          paste0("Special chars: ", paste(head(special_names, 3), collapse = ", ")))
  }

  # Check for names starting with numbers
  if (any(grepl("^[0-9]", names(df)))) {
    number_names <- names(df)[grepl("^[0-9]", names(df))]
    problematic_names <- c(problematic_names,
                          paste0("Start with number: ", paste(head(number_names, 3), collapse = ", ")))
  }

  # Check for duplicate names
  if (any(duplicated(names(df)))) {
    dup_names <- unique(names(df)[duplicated(names(df))])
    problematic_names <- c(problematic_names,
                          paste0("Duplicates: ", paste(head(dup_names, 3), collapse = ", ")))
  }

  if (length(problematic_names) > 0) {
    rlang::inform(c(
      "Problematic column names detected - will be cleaned:",
      paste0("  • ", problematic_names)
    ))
  }

  # ============================================================================
  # CLEAN NAMES (with or without janitor)
  # ============================================================================

  # Check if janitor is available
  if (!requireNamespace("janitor", quietly = TRUE)) {
    rlang::warn(c(
      "Package 'janitor' is not installed - using basic name cleaning",
      "!" = "For best results, install janitor: install.packages('janitor')",
      "i" = "Applying basic cleaning: lowercase, underscores, no special chars",
      ">" = "janitor::clean_names() provides more robust cleaning"
    ))

    # Basic cleaning without janitor
    cleaned_names <- names(df)

    # Convert to lowercase
    cleaned_names <- tolower(cleaned_names)

    # Replace spaces and dots with underscores
    cleaned_names <- gsub("[ \\.]+", "_", cleaned_names)

    # Remove special characters (keep only letters, numbers, underscores)
    cleaned_names <- gsub("[^a-z0-9_]", "", cleaned_names)

    # Remove leading/trailing underscores
    cleaned_names <- gsub("^_+|_+$", "", cleaned_names)

    # Remove multiple consecutive underscores
    cleaned_names <- gsub("_+", "_", cleaned_names)

    # Ensure names don't start with numbers (prepend 'x')
    cleaned_names <- ifelse(grepl("^[0-9]", cleaned_names),
                           paste0("x", cleaned_names),
                           cleaned_names)

    # Handle empty names
    if (any(cleaned_names == "")) {
      empty_idx <- which(cleaned_names == "")
      cleaned_names[empty_idx] <- paste0("var_", empty_idx)
      rlang::warn(c(
        "Empty column names detected",
        "!" = paste0("Replacing with: ", paste(cleaned_names[empty_idx], collapse = ", ")),
        ">" = "Consider giving these columns meaningful names"
      ))
    }

    # Handle duplicates after cleaning
    if (any(duplicated(cleaned_names))) {
      # Make unique by adding numbers
      cleaned_names <- make.unique(cleaned_names, sep = "_")
      rlang::warn(c(
        "Duplicate names after cleaning - made unique",
        "i" = "Added numeric suffixes to duplicates",
        ">" = "Review cleaned names: names(your_data)"
      ))
    }

    names(df) <- cleaned_names

    # Inform user of changes
    original_names <- names(df)
    rlang::inform(c(
      "Column names cleaned (basic method)",
      "i" = "Use names(your_data) to see new names",
      ">" = "For better cleaning, install janitor"
    ))

    return(df)
  }

  # Use janitor if available
  tryCatch({
    cleaned_df <- janitor::clean_names(df, ...)

    # Check if any names actually changed
    names_changed <- !identical(names(df), names(cleaned_df))

    if (names_changed) {
      n_changed <- sum(names(df) != names(cleaned_df))
      rlang::inform(c(
        "Column names cleaned successfully",
        "i" = paste0(n_changed, " of ", ncol(df), " names were modified"),
        "i" = "All names now follow snake_case convention",
        ">" = "Use names(your_data) to see new names"
      ))
    }

    return(cleaned_df)
  }, error = function(e) {
    rlang::abort(c(
      "janitor::clean_names() failed",
      "x" = conditionMessage(e),
      "!" = "This is unusual - janitor is installed but cleaning failed",
      ">" = "Try basic cleaning by uninstalling janitor temporarily",
      ">" = "Or check if your data frame is corrupted: str(df)"
    ))
  })
}


#' Assert Input is Safe for Beginner Users (Comprehensive Validation)
#'
#' @description
#' **Internal L1 Helper.** Centralized validation function that checks common
#' input assumptions and provides educational error messages for beginners.
#' Ensures consistency across all package functions.
#'
#' This function enforces the "beginner-first" philosophy by catching common
#' mistakes early with helpful guidance.
#'
#' @param x Object to validate
#' @param type Character. Expected type: "numeric", "data.frame", "character",
#'   "logical", "factor", "integer", or "matrix"
#' @param allow_na Logical. Are NA values allowed? Default FALSE.
#' @param min_length Integer. Minimum length/nrow required. Default 1.
#' @param max_length Integer. Maximum length/nrow allowed. Default Inf.
#' @param min_value Numeric. Minimum value allowed (for numeric types). Default -Inf.
#' @param max_value Numeric. Maximum value allowed (for numeric types). Default Inf.
#' @param allow_infinite Logical. Are Inf/-Inf allowed? Default FALSE.
#' @param arg_name Character. Name of the argument being validated (for error messages).
#'   Default "x".
#' @param context Character. Context for the error (e.g., "t-test", "correlation").
#'   Helps provide more specific guidance.
#'
#' @return Invisible TRUE if all checks pass. Errors otherwise.
#'
#' @details
#' **What this checks:**
#' - Type correctness (numeric, data.frame, etc.)
#' - NA/NaN/Inf handling
#' - Length/dimension constraints
#' - Value range constraints (for numeric)
#' - Common beginner mistakes
#'
#' **Educational error messages:**
#' Every error explains:
#' 1. What went wrong
#' 2. Why it's a problem
#' 3. How to fix it
#' 4. What to check in the data
#'
#' **Philosophy:**
#' This function embodies the consumeR philosophy: "Silence is a bug."
#' We catch problems early and guide the user to solutions.
#'
#' @examples
#' \dontrun{
#' # Basic type check
#' x <- c(1, 2, 3)
#' assert_beginner_safe(x, type = "numeric")  # Pass
#'
#' # With constraints
#' assert_beginner_safe(x,
#'   type = "numeric",
#'   min_length = 2,
#'   min_value = 0,
#'   max_value = 10
#' )
#'
#' # Data frame with NA check
#' df <- data.frame(a = 1:5, b = 6:10)
#' assert_beginner_safe(df,
#'   type = "data.frame",
#'   min_length = 3,  # min 3 rows
#'   arg_name = "data"
#' )
#'
#' # Will error with educational message
#' assert_beginner_safe(c(1, NA, 3),
#'   type = "numeric",
#'   allow_na = FALSE,
#'   context = "correlation analysis"
#' )
#' }
#'
#' @keywords internal
assert_beginner_safe <- function(x,
                                 type = c("numeric", "data.frame", "character",
                                          "logical", "factor", "integer", "matrix"),
                                 allow_na = FALSE,
                                 min_length = 1,
                                 max_length = Inf,
                                 min_value = -Inf,
                                 max_value = Inf,
                                 allow_infinite = FALSE,
                                 arg_name = "x",
                                 context = NULL) {

  type <- match.arg(type)

  # Build context string for errors
  ctx <- if (!is.null(context)) paste0(" (", context, ")") else ""

  # ============================================================================
  # CHECK 1: Type Validation
  # ============================================================================

  type_check <- switch(type,
    "numeric" = is.numeric(x),
    "data.frame" = is.data.frame(x),
    "character" = is.character(x),
    "logical" = is.logical(x),
    "factor" = is.factor(x),
    "integer" = is.integer(x),
    "matrix" = is.matrix(x),
    FALSE
  )

  if (!type_check) {
    actual_type <- class(x)[1]

    # Educational type conversion suggestions
    convert_help <- switch(type,
      "numeric" = c(
        ">" = "To convert character: as.numeric(x)",
        ">" = "To convert factor: as.numeric(as.character(x))",
        ">" = "To convert logical: as.numeric(x)  # TRUE=1, FALSE=0"
      ),
      "data.frame" = c(
        ">" = "To convert matrix: as.data.frame(x)",
        ">" = "To convert tibble: as.data.frame(x)",
        ">" = "To create from vectors: data.frame(var1 = x, var2 = y)"
      ),
      "character" = c(
        ">" = "To convert: as.character(x)"
      ),
      "factor" = c(
        ">" = "To convert: as.factor(x)",
        ">" = "To create with levels: factor(x, levels = c(...))"
      ),
      c(">" = paste0("To convert: as.", type, "(x)"))
    )

    rlang::abort(c(
      paste0(arg_name, " must be ", type, ctx),
      "x" = paste0("You provided: ", actual_type),
      "i" = paste0("Expected type: ", type),
      convert_help
    ))
  }

  # ============================================================================
  # CHECK 2: Length/Dimension Validation
  # ============================================================================

  actual_length <- if (is.data.frame(x) || is.matrix(x)) nrow(x) else length(x)

  if (actual_length < min_length) {
    unit <- if (is.data.frame(x) || is.matrix(x)) "rows" else "elements"

    rlang::abort(c(
      paste0(arg_name, " is too short", ctx),
      "x" = paste0("Has ", actual_length, " ", unit, ", need at least ", min_length),
      "i" = "This analysis requires more data points",
      ">" = "Check: Did you subset your data too aggressively?",
      ">" = "Check: Are there unexpected NAs causing exclusions?"
    ))
  }

  if (actual_length > max_length) {
    unit <- if (is.data.frame(x) || is.matrix(x)) "rows" else "elements"

    rlang::abort(c(
      paste0(arg_name, " is too long", ctx),
      "x" = paste0("Has ", actual_length, " ", unit, ", maximum allowed is ", max_length),
      "i" = "This function has a size limit",
      ">" = "Consider: Sampling your data with sample()",
      ">" = "Consider: Summarizing or aggregating first"
    ))
  }

  # ============================================================================
  # CHECK 3: NA/NaN Validation (for relevant types)
  # ============================================================================

  if (type %in% c("numeric", "character", "logical", "integer", "factor")) {

    # Check for NaN (Not a Number) - more serious than NA
    if (any(is.nan(x))) {
      nan_positions <- which(is.nan(x))
      n_nan <- length(nan_positions)

      rlang::abort(c(
        paste0(arg_name, " contains NaN (Not a Number)", ctx),
        "x" = paste0(n_nan, " NaN value", ifelse(n_nan > 1, "s", ""),
                     " at position", ifelse(n_nan > 1, "s", ""), ": ",
                     paste(head(nan_positions, 5), collapse = ", "),
                     if (n_nan > 5) "..." else ""),
        "i" = "NaN indicates a failed calculation (e.g., 0/0, Inf-Inf)",
        "!" = "This usually means your data has a problem",
        ">" = "Check: summary(your_data) for suspicious values",
        ">" = "Check: Are there zeros where there shouldn't be?",
        ">" = "Fix: Remove NaN with x[!is.nan(x)] or convert to NA"
      ))
    }

    # Check for NA (missing values)
    if (!allow_na && any(is.na(x))) {
      na_positions <- which(is.na(x))
      n_na <- length(na_positions)

      rlang::abort(c(
        paste0(arg_name, " contains missing values (NA)", ctx),
        "x" = paste0(n_na, " NA value", ifelse(n_na > 1, "s", ""),
                     " (", round(n_na / length(x) * 100, 1), "% of data)"),
        "i" = "This analysis cannot handle missing data",
        "!" = "Options to handle missing data:",
        "  " = "  1. Remove NAs: x[!is.na(x)] or na.omit(df)",
        "  " = "  2. Recode NAs: x[is.na(x)] <- replacement_value",
        "  " = "  3. Use different analysis that allows NAs",
        ">" = "Check: Why are values missing? Is it random or systematic?",
        ">" = "Check: summary(your_data) to see extent of missingness"
      ))
    }
  }

  # ============================================================================
  # CHECK 4: Infinite Values (for numeric types)
  # ============================================================================

  if (type %in% c("numeric", "integer")) {

    if (!allow_infinite && any(is.infinite(x))) {
      inf_positions <- which(is.infinite(x))
      n_inf <- length(inf_positions)

      rlang::abort(c(
        paste0(arg_name, " contains infinite values (Inf/-Inf)", ctx),
        "x" = paste0(n_inf, " infinite value", ifelse(n_inf > 1, "s", "")),
        "i" = "Infinite values indicate division by zero or overflow",
        "!" = "Common causes:",
        "  " = "  • Division by zero: x / 0",
        "  " = "  • Log of zero: log(0)",
        "  " = "  • Very large calculations that overflow",
        ">" = "Check: x[is.infinite(x)] to see which values",
        ">" = "Fix: Remove Inf with x[!is.infinite(x)]",
        ">" = "Fix: Replace Inf with max/min: x[is.infinite(x)] <- max_value"
      ))
    }

    # ========================================================================
    # CHECK 5: Value Range (for numeric types)
    # ========================================================================

    # Only check non-NA, non-Inf values
    finite_vals <- x[is.finite(x)]

    if (length(finite_vals) > 0) {

      if (any(finite_vals < min_value)) {
        n_below <- sum(finite_vals < min_value)
        min_actual <- min(finite_vals)

        rlang::abort(c(
          paste0(arg_name, " contains values below minimum", ctx),
          "x" = paste0(n_below, " value", ifelse(n_below > 1, "s", ""),
                       " below ", min_value),
          "x" = paste0("Minimum value in data: ", round(min_actual, 4)),
          "i" = paste0("This analysis requires values >= ", min_value),
          ">" = "Check: Are there data entry errors or outliers?",
          ">" = "Check: summary(x) to see full range",
          ">" = "Fix: Filter data: x[x >= min_value]"
        ))
      }

      if (any(finite_vals > max_value)) {
        n_above <- sum(finite_vals > max_value)
        max_actual <- max(finite_vals)

        rlang::abort(c(
          paste0(arg_name, " contains values above maximum", ctx),
          "x" = paste0(n_above, " value", ifelse(n_above > 1, "s", ""),
                       " above ", max_value),
          "x" = paste0("Maximum value in data: ", round(max_actual, 4)),
          "i" = paste0("This analysis requires values <= ", max_value),
          ">" = "Check: Are there data entry errors or outliers?",
          ">" = "Check: summary(x) to see full range",
          ">" = "Fix: Filter data: x[x <= max_value]"
        ))
      }
    }
  }

  # ============================================================================
  # All checks passed
  # ============================================================================

  invisible(TRUE)
}
