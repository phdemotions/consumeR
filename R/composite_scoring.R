#' Composite Scoring Functions
#'
#' @description
#' Functions for scoring composite scales including reverse scoring,
#' row-wise statistics, and structured composite creation with metadata.
#'
#' @name composite_scoring
NULL


#' Reverse Score Likert-Type Items
#'
#' @description
#' Reverses the direction of Likert-type scale items. For example,
#' converts a 7-point scale so that 1 becomes 7, 2 becomes 6, etc.
#'
#' @param x Numeric vector of item responses
#' @param min Integer. Minimum value of the scale (default 1)
#' @param max Integer. Maximum value of the scale (default 7)
#' @param strict Logical. If TRUE (default), aborts if values outside [min, max].
#'   If FALSE, only warns and reverse scores valid values.
#'
#' @return Numeric vector of reverse-scored values
#'
#' @details
#' **Reverse scoring formula:**
#' \code{new_value = (max + min) - old_value}
#'
#' For a 1-7 scale:
#' \itemize{
#'   \item 1 becomes 7
#'   \item 2 becomes 6
#'   \item 3 becomes 5
#'   \item 4 stays 4 (midpoint)
#'   \item 5 becomes 3
#'   \item 6 becomes 2
#'   \item 7 becomes 1
#' }
#'
#' **When to use:**
#' - Some scale items are worded negatively (e.g., "I am NOT satisfied")
#' - Need to reverse direction so all items measure the same construct
#'
#' **Strict mode:**
#' - strict = TRUE: Errors if any values fall outside [min, max] range
#' - strict = FALSE: Warns and only reverses valid values, preserves out-of-range
#'
#' @examples
#' # Basic usage (1-7 scale)
#' responses <- c(1, 2, 3, 4, 5, 6, 7)
#' reverse_score_likert(responses)  # c(7, 6, 5, 4, 3, 2, 1)
#'
#' # 1-5 scale
#' responses_5pt <- c(1, 2, 3, 4, 5)
#' reverse_score_likert(responses_5pt, min = 1, max = 5)  # c(5, 4, 3, 2, 1)
#'
#' # Handles NA
#' responses_na <- c(1, NA, 3, 5, NA, 7)
#' reverse_score_likert(responses_na)  # c(7, NA, 5, 3, NA, 1)
#'
#' # With out-of-range values (strict = FALSE)
#' responses_bad <- c(1, 2, 8, 4, 5)  # 8 is out of range
#' reverse_score_likert(responses_bad, strict = FALSE)
#' # Warning issued, 8 preserved
#'
#' @export
reverse_score_likert <- function(x, min = 1, max = 7, strict = TRUE) {
  # Validate input
  if (!is.numeric(x)) {
    rlang::abort(c(
      "x must be numeric",
      "x" = "You provided {class(x)[1]}"
    ))
  }

  if (!is.numeric(min) || !is.numeric(max) || length(min) != 1 || length(max) != 1) {
    rlang::abort("min and max must be single numeric values")
  }

  if (min >= max) {
    rlang::abort(c(
      "min must be less than max",
      "x" = "You provided min = {min}, max = {max}"
    ))
  }

  # Check for out-of-range values (excluding NA)
  out_of_range <- !is.na(x) & (x < min | x > max)

  if (any(out_of_range)) {
    n_bad <- sum(out_of_range)
    bad_vals <- unique(x[out_of_range])

    if (strict) {
      rlang::abort(c(
        "Values outside [{min}, {max}] range detected",
        "x" = "{n_bad} value{?s} out of range: {paste(bad_vals, collapse = ', ')}",
        "i" = "Set strict = FALSE to reverse only valid values"
      ))
    } else {
      warning(
        n_bad, " value(s) outside [", min, ", ", max, "] range. ",
        "These will not be reverse scored: ", paste(bad_vals, collapse = ", ")
      )
    }
  }

  # Reverse score
  reversed <- (max + min) - x

  # If not strict, preserve out-of-range values
  if (!strict && any(out_of_range)) {
    reversed[out_of_range] <- x[out_of_range]
  }

  reversed
}


#' Calculate Row-Wise Standard Deviation
#'
#' @description
#' Calculates the standard deviation across specified columns for each row.
#' Useful for assessing response variability within individuals.
#'
#' @param data Data frame or matrix
#' @param items Character vector of column names to include, or NULL to use all columns
#' @param na.rm Logical. Remove NA values when calculating SD? (default TRUE)
#'
#' @return Numeric vector of row-wise standard deviations
#'
#' @details
#' **What this measures:**
#' Row-wise SD indicates how much variability exists in a person's responses
#' across items. High SD suggests inconsistent responses, low SD suggests
#' consistent responses (e.g., all 7s or all 1s).
#'
#' **Use cases:**
#' - Detect straight-lining (all same response -> SD = 0)
#' - Identify careless responding
#' - Calculate intra-individual variability
#'
#' @examples
#' # Basic usage
#' df <- data.frame(
#'   q1 = c(5, 7, 1, 4),
#'   q2 = c(6, 7, 1, 5),
#'   q3 = c(5, 7, 1, 3)
#' )
#' row_sd(df)  # SD for each row
#'
#' # Specific items
#' row_sd(df, items = c("q1", "q2"))
#'
#' # With missing values
#' df_na <- data.frame(
#'   q1 = c(5, NA, 1),
#'   q2 = c(6, 7, NA),
#'   q3 = c(5, 7, 1)
#' )
#' row_sd(df_na, na.rm = TRUE)
#'
#' # Detect straight-lining
#' straight_liner <- data.frame(q1 = 7, q2 = 7, q3 = 7)
#' row_sd(straight_liner)  # Returns 0
#'
#' @export
row_sd <- function(data, items = NULL, na.rm = TRUE) {
  # Convert to data frame if matrix
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }

  if (!is.data.frame(data)) {
    rlang::abort(c(
      "data must be a data frame or matrix",
      "x" = "You provided {class(data)[1]}"
    ))
  }

  # Select items
  if (!is.null(items)) {
    assert_vars_present(data, items, "row_sd")
    data_subset <- data[, items, drop = FALSE]
  } else {
    data_subset <- data
  }

  # Check all columns are numeric
  non_numeric <- names(data_subset)[!vapply(data_subset, is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    rlang::abort(c(
      "All columns must be numeric",
      "x" = "Non-numeric columns: {paste(non_numeric, collapse = ', ')}"
    ))
  }

  # Calculate row-wise SD
  apply(data_subset, 1, sd, na.rm = na.rm)
}


#' Score Composite Scale with Metadata
#'
#' @description
#' Calculates composite scores from multiple items with optional reverse scoring
#' and flexible NA handling. Returns a structured object with scores and metadata.
#'
#' @param df Data frame containing items
#' @param items Character vector of item column names
#' @param reverse_items Character vector of items to reverse score (optional)
#' @param reverse_min Minimum value for reverse scoring (default 1)
#' @param reverse_max Maximum value for reverse scoring (default 7)
#' @param method Character. Composite method: "mean" (default) or "sum"
#' @param na_rule Character. How to handle missing values:
#'   - "preserve" (default): Preserve NA (na.rm = FALSE)
#'   - "threshold": Require minimum number of non-missing items
#' @param threshold Integer. If na_rule = "threshold", minimum non-missing items required.
#'   If NULL, defaults to ceiling(length(items) * 0.5) (i.e., at least 50%)
#' @param name Character. Optional name for the composite (for metadata)
#'
#' @return A list with class "composite_score" containing:
#' \itemize{
#'   \item \code{value}: Numeric vector of composite scores
#'   \item \code{meta}: List with metadata:
#'     \itemize{
#'       \item \code{name}: Composite name (if provided)
#'       \item \code{items}: Character vector of items used
#'       \item \code{reverse_items}: Character vector of reversed items (or NULL)
#'       \item \code{k}: Number of items
#'       \item \code{method}: Composite method ("mean" or "sum")
#'       \item \code{na_rule}: NA handling rule used
#'       \item \code{threshold}: Threshold used (or NULL)
#'       \item \code{n_missing}: Integer vector showing count of missing items per row
#'       \item \code{below_threshold}: Logical vector indicating which rows didn't meet threshold
#'     }
#' }
#'
#' @details
#' **NA handling rules:**
#' - "preserve": Standard behavior, NA propagates (if any item is NA, composite is NA)
#' - "threshold": Calculate composite if at least `threshold` items are non-missing
#'
#' **Default threshold:** 50% of items (rounded up)
#' - 3 items -> need 2 non-missing
#' - 5 items -> need 3 non-missing
#' - 10 items -> need 5 non-missing
#'
#' @examples
#' # Basic composite (mean)
#' df <- data.frame(
#'   q1 = c(7, 6, 5, NA),
#'   q2 = c(6, 7, 4, 5),
#'   q3 = c(7, 6, 5, 6)
#' )
#' result <- score_composite(df, items = c("q1", "q2", "q3"))
#' result$value  # c(6.67, 6.33, 4.67, NA)
#' result$meta$k  # 3 items
#'
#' # With reverse scoring
#' df2 <- data.frame(
#'   happy1 = c(7, 6, 5),   # Positive item
#'   sad2 = c(2, 3, 4),     # Negative item (needs reversing)
#'   happy3 = c(7, 6, 5)    # Positive item
#' )
#' result <- score_composite(
#'   df2,
#'   items = c("happy1", "sad2", "happy3"),
#'   reverse_items = "sad2"
#' )
#' result$value  # Sad2 reversed: (7,6,5) + (6,5,4) + (7,6,5) / 3
#'
#' # Threshold rule (allow some missing)
#' df3 <- data.frame(
#'   q1 = c(7, NA, 5, NA, 7),
#'   q2 = c(6, 7, NA, NA, 6),
#'   q3 = c(7, 6, 5, NA, 7)
#' )
#' result <- score_composite(
#'   df3,
#'   items = c("q1", "q2", "q3"),
#'   na_rule = "threshold",
#'   threshold = 2  # Need at least 2 of 3 items
#' )
#' result$value  # Rows with 2+ items get score, row 4 (all NA) gets NA
#' result$meta$below_threshold  # Which rows didn't meet threshold
#'
#' # Sum instead of mean
#' result <- score_composite(df, items = c("q1", "q2", "q3"), method = "sum")
#'
#' @export
score_composite <- function(df,
                            items,
                            reverse_items = NULL,
                            reverse_min = 1,
                            reverse_max = 7,
                            method = c("mean", "sum"),
                            na_rule = c("preserve", "threshold"),
                            threshold = NULL,
                            name = NULL) {
  method <- match.arg(method)
  na_rule <- match.arg(na_rule)

  # Validate inputs
  if (!is.data.frame(df)) {
    rlang::abort(c(
      "df must be a data frame",
      "x" = "You provided {class(df)[1]}"
    ))
  }

  if (!is.character(items) || length(items) < 1) {
    rlang::abort("items must be a character vector with at least 1 item")
  }

  assert_vars_present(df, items, "composite scoring")

  # Check items are numeric
  non_numeric <- items[!vapply(df[items], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    rlang::abort(c(
      "All items must be numeric",
      "x" = "Non-numeric items: {paste(non_numeric, collapse = ', ')}"
    ))
  }

  # Validate reverse_items
  if (!is.null(reverse_items)) {
    invalid_reverse <- setdiff(reverse_items, items)
    if (length(invalid_reverse) > 0) {
      rlang::abort(c(
        "reverse_items must be a subset of items",
        "x" = "Not in items: {paste(invalid_reverse, collapse = ', ')}"
      ))
    }
  }

  # Validate threshold
  if (na_rule == "threshold" && is.null(threshold)) {
    # Default: 50% of items (rounded up)
    threshold <- ceiling(length(items) * 0.5)
  }

  if (na_rule == "threshold" && (threshold < 1 || threshold > length(items))) {
    rlang::abort(c(
      "threshold must be between 1 and {length(items)}",
      "x" = "You provided {threshold}"
    ))
  }

  # Extract item data
  item_data <- df[, items, drop = FALSE]

  # Reverse score if needed
  if (!is.null(reverse_items)) {
    for (item in reverse_items) {
      item_data[[item]] <- reverse_score_likert(
        item_data[[item]],
        min = reverse_min,
        max = reverse_max,
        strict = TRUE
      )
    }
  }

  # Count missing per row
  n_missing <- rowSums(is.na(item_data))
  n_present <- length(items) - n_missing

  # Determine which rows don't meet threshold
  if (na_rule == "threshold") {
    below_threshold <- n_present < threshold
  } else {
    below_threshold <- rep(FALSE, nrow(df))
  }

  # Calculate composite
  if (na_rule == "preserve") {
    # Standard NA propagation
    if (method == "mean") {
      composite_values <- rowMeans(item_data, na.rm = FALSE)
    } else {
      composite_values <- rowSums(item_data, na.rm = FALSE)
    }
  } else {
    # Threshold rule
    if (method == "mean") {
      composite_values <- rowMeans(item_data, na.rm = TRUE)
    } else {
      composite_values <- rowSums(item_data, na.rm = TRUE)
    }
    # Set to NA if below threshold
    composite_values[below_threshold] <- NA_real_
  }

  # Create result object
  result <- structure(
    list(
      value = composite_values,
      meta = list(
        name = name,
        items = items,
        reverse_items = reverse_items,
        k = length(items),
        method = method,
        na_rule = na_rule,
        threshold = threshold,
        n_missing = n_missing,
        below_threshold = below_threshold
      )
    ),
    class = c("composite_score", "list")
  )

  result
}


#' Add Composite Score to Data Frame
#'
#' @description
#' Wrapper around \code{score_composite()} that adds the composite score
#' as a new column to the data frame.
#'
#' @param df Data frame
#' @param col_name Character. Name for the new composite column
#' @param ... Arguments passed to \code{score_composite()}
#'
#' @return Data frame with new composite column added
#'
#' @examples
#' df <- data.frame(
#'   q1 = c(7, 6, 5),
#'   q2 = c(6, 7, 4),
#'   q3 = c(7, 6, 5)
#' )
#'
#' # Add composite column
#' df_with_composite <- add_composite(
#'   df,
#'   col_name = "satisfaction",
#'   items = c("q1", "q2", "q3")
#' )
#'
#' # Result has new "satisfaction" column
#' df_with_composite$satisfaction
#'
#' @export
add_composite <- function(df, col_name, ...) {
  if (!is.character(col_name) || length(col_name) != 1) {
    rlang::abort("col_name must be a single character string")
  }

  # Score composite
  result <- score_composite(df, ...)

  # Add to data frame
  df[[col_name]] <- result$value

  # Optionally store metadata as attribute
  attr(df[[col_name]], "composite_meta") <- result$meta

  df
}


#' @export
print.composite_score <- function(x, ...) {
  cat("\n")
  cat("Composite Score\n")
  cat("===============\n\n")

  if (!is.null(x$meta$name)) {
    cat("Name:", x$meta$name, "\n")
  }

  cat("Method:", x$meta$method, "\n")
  cat("Items (k):", x$meta$k, "\n")
  cat("  ", paste(x$meta$items, collapse = ", "), "\n")

  if (!is.null(x$meta$reverse_items) && length(x$meta$reverse_items) > 0) {
    cat("Reverse scored:", paste(x$meta$reverse_items, collapse = ", "), "\n")
  }

  cat("\nNA handling:", x$meta$na_rule, "\n")
  if (x$meta$na_rule == "threshold") {
    cat("  Threshold:", x$meta$threshold, "of", x$meta$k, "items required\n")
    n_below <- sum(x$meta$below_threshold)
    if (n_below > 0) {
      cat("  ", n_below, "row(s) below threshold (set to NA)\n")
    }
  }

  cat("\nSummary of scores:\n")
  print(summary(x$value))

  cat("\nMissing data:\n")
  cat("  Total NA:", sum(is.na(x$value)), "of", length(x$value), "\n")
  cat("  Items missing per row (range):", range(x$meta$n_missing), "\n")

  cat("\n")
  invisible(x)
}
