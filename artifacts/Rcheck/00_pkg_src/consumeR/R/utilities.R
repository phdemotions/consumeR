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

  # Validate input
  if (!is.numeric(p)) {
    rlang::abort(c(
      "p must be numeric",
      "x" = "You provided {class(p)[1]}"
    ))
  }

  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    rlang::abort(c(
      "p-values must be between 0 and 1",
      "x" = "Found values outside [0, 1] range"
    ))
  }

  # Determine threshold for "p < " reporting
  threshold <- 10^(-digits)

  # Format each p-value
  formatted <- vapply(p, function(p_val) {
    if (is.na(p_val)) {
      return(NA_character_)
    }

    if (p_val < threshold) {
      # Very small p-value
      if (style == "apa") {
        paste0("p < ", sub("^0", "", sprintf(paste0("%.", digits, "f"), threshold)))
      } else {
        paste0("p < ", sprintf(paste0("%.", digits, "f"), threshold))
      }
    } else {
      # Regular p-value
      p_str <- format(round(p_val, digits), nsmall = digits)
      if (style == "apa") {
        # Remove leading zero
        p_str <- sub("^0", "", p_str)
      }
      paste0("p = ", p_str)
    }
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

  # Validate input
  if (!is.numeric(n)) {
    rlang::abort(c(
      "n must be numeric",
      "x" = "You provided {class(n)[1]}"
    ))
  }

  # Round to integers
  n_int <- as.integer(round(n))

  # Format
  if (include_label) {
    paste0(type, " = ", n_int)
  } else {
    as.character(n_int)
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
  # Validate inputs
  if (!is.numeric(est) || !is.numeric(lo) || !is.numeric(hi)) {
    rlang::abort("est, lo, and hi must all be numeric")
  }

  # Check lengths match
  len <- length(est)
  if (length(lo) != len || length(hi) != len) {
    rlang::abort(c(
      "est, lo, and hi must have the same length",
      "x" = "Lengths: est = {length(est)}, lo = {length(lo)}, hi = {length(hi)}"
    ))
  }

  # Format
  if (percent) {
    est_str <- paste0(format(round(est * 100, digits), nsmall = digits), "%")
    lo_str <- paste0(format(round(lo * 100, digits), nsmall = digits), "%")
    hi_str <- paste0(format(round(hi * 100, digits), nsmall = digits), "%")
  } else {
    est_str <- format(round(est, digits), nsmall = digits)
    lo_str <- format(round(lo, digits), nsmall = digits)
    hi_str <- format(round(hi, digits), nsmall = digits)
  }

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
  missing_vars <- setdiff(vars, names(df))

  if (length(missing_vars) > 0) {
    context <- if (!is.null(label)) paste0(" for ", label) else ""

    rlang::abort(c(
      paste0("Missing variables", context, ": ", paste(missing_vars, collapse = ", ")),
      "i" = paste0("Available variables: ", paste(names(df), collapse = ", "))
    ))
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
  if (!requireNamespace("janitor", quietly = TRUE)) {
    warning(
      "Package 'janitor' is not installed. ",
      "Column names will not be cleaned. ",
      "Install with: install.packages('janitor')"
    )
    return(df)
  }

  janitor::clean_names(df, ...)
}
