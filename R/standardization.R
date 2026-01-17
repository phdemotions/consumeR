#' Internal Helpers for Peer-Review-Grade Variable Standardization
#'
#' @description
#' **Level 1 (L1) internal helpers** for standardizing variables to comparable
#' scales. Essential for creating composite measures from items measured on
#' different scales (e.g., combining a 1-5 scale with a 1-7 scale).
#'
#' **These are internal utilities** designed to produce defensible, transparent
#' standardization suitable for top-tier journal peer review.
#'
#' @name standardization_helpers
#' @keywords internal
NULL


#' Z-Score Standardization (Mean = 0, SD = 1)
#'
#' @description
#' **L1 Internal Helper.** Standardizes a numeric vector to have mean = 0 and SD = 1.
#' This is the most common and statistically defensible standardization method.
#'
#' @param x Numeric vector to standardize
#' @param na.rm Logical. Should missing values be removed? Default TRUE.
#' @param check_variance Logical. Should function check for zero variance? Default TRUE.
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{standardized}: Numeric vector of z-scores
#'     \item \code{meta}: List with:
#'       \itemize{
#'         \item \code{method}: "z-score"
#'         \item \code{original_mean}: Mean before standardization
#'         \item \code{original_sd}: SD before standardization
#'         \item \code{n_valid}: Number of non-missing values
#'         \item \code{n_missing}: Number of missing values
#'       }
#'   }
#'
#' @details
#' **Formula:** \code{z = (x - mean(x)) / sd(x)}
#'
#' **What this produces:**
#' - Mean of standardized variable = 0
#' - SD of standardized variable = 1
#' - Preserves shape of distribution
#' - Interpretation: "how many standard deviations above/below the mean"
#'
#' **When to use:**
#' - Combining variables measured on different scales
#' - Creating composites where items have different variances
#' - Standard practice in psychology and consumer research
#'
#' **Peer review justification:**
#' Z-score standardization is the default in most statistical software and
#' is universally accepted in peer review. It preserves the relative distances
#' between observations while putting variables on a common scale.
#'
#' **Warnings:**
#' - If SD = 0 (no variance), standardization is impossible. Function will
#'   error with an educational message suggesting alternatives.
#' - With very small n (< 10), standardization may be unstable.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' x <- c(1, 2, 3, 4, 5, 6, 7)
#' result <- standardize_z(x)
#' result$standardized
#' # Mean ≈ 0, SD ≈ 1
#'
#' # With missing values
#' x_na <- c(1, 2, NA, 4, 5, 6, 7)
#' result <- standardize_z(x_na, na.rm = TRUE)
#'
#' # Check metadata
#' result$meta$original_mean  # 3.71 (mean of non-missing)
#' result$meta$original_sd    # 2.14
#' }
#'
#' @keywords internal
standardize_z <- function(x, na.rm = TRUE, check_variance = TRUE) {

  if (!is.numeric(x)) {
    rlang::abort(c(
      "x must be numeric",
      "x" = "You provided {class(x)[1]}",
      "i" = "Convert to numeric before standardizing"
    ))
  }

  n_total <- length(x)
  n_missing <- sum(is.na(x))
  n_valid <- n_total - n_missing

  if (n_valid == 0) {
    rlang::abort(c(
      "Cannot standardize: all values are missing",
      "x" = "All {n_total} value{?s} are NA"
    ))
  }

  if (n_valid < 3) {
    rlang::warn(c(
      "Standardizing with very few observations (n = {n_valid})",
      "!" = "Results may be unstable",
      "i" = "Consider whether standardization is appropriate with n < 10"
    ))
  }

  # Calculate mean and SD
  x_mean <- mean(x, na.rm = na.rm)
  x_sd <- stats::sd(x, na.rm = na.rm)

  # Check for zero variance
  if (check_variance && (is.na(x_sd) || x_sd == 0 || x_sd < .Machine$double.eps)) {
    rlang::abort(c(
      "Cannot standardize: variable has zero variance",
      "x" = "All non-missing values are identical ({unique(stats::na.omit(x))[1]})",
      "i" = "Standardization requires variation in the data",
      "i" = "Consider removing this variable or using it as-is"
    ))
  }

  # Standardize
  z_scores <- (x - x_mean) / x_sd

  # Return with metadata
  list(
    standardized = z_scores,
    meta = list(
      method = "z-score",
      original_mean = round(x_mean, 4),
      original_sd = round(x_sd, 4),
      n_valid = n_valid,
      n_missing = n_missing
    )
  )
}


#' Min-Max Range Standardization
#'
#' @description
#' **L1 Internal Helper.** Rescales a numeric vector to a specified range,
#' typically [0, 1]. Preserves the original distribution shape while
#' compressing to new bounds.
#'
#' @param x Numeric vector to standardize
#' @param new_min Numeric. Minimum of new scale (default 0)
#' @param new_max Numeric. Maximum of new scale (default 1)
#' @param na.rm Logical. Should missing values be removed? Default TRUE.
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{standardized}: Numeric vector rescaled to [new_min, new_max]
#'     \item \code{meta}: List with:
#'       \itemize{
#'         \item \code{method}: "range"
#'         \item \code{original_min}: Minimum before standardization
#'         \item \code{original_max}: Maximum before standardization
#'         \item \code{new_min}: Target minimum
#'         \item \code{new_max}: Target maximum
#'         \item \code{n_valid}: Number of non-missing values
#'         \item \code{n_missing}: Number of missing values
#'       }
#'   }
#'
#' @details
#' **Formula:**
#' \code{x_new = ((x - min(x)) / (max(x) - min(x))) * (new_max - new_min) + new_min}
#'
#' **What this produces:**
#' - Minimum value → new_min
#' - Maximum value → new_max
#' - All other values linearly rescaled between
#' - Preserves relative ordering and distribution shape
#'
#' **Common uses:**
#' - Rescale to [0, 1]: useful for proportions, probabilities
#' - Rescale to [1, 7]: convert different scales to common Likert scale
#' - Rescale to [0, 100]: convert to percentage-like scale
#'
#' **When to use vs. z-scores:**
#' - Use range standardization when you want bounded output (e.g., [0,1])
#' - Use z-scores when you want to preserve variance information
#' - Both are defensible; choice depends on research goals
#'
#' **Peer review justification:**
#' Min-max standardization is standard practice when combining variables
#' into composites. It's intuitive (everyone understands a 0-1 scale) and
#' preserves the original distribution shape.
#'
#' **Warnings:**
#' - If all values are identical, standardization is impossible
#' - Sensitive to outliers (they define the range)
#'
#' @examples
#' \dontrun{
#' # Rescale to [0, 1]
#' x <- c(1, 2, 3, 4, 5, 6, 7)
#' result <- standardize_range(x)
#' result$standardized  # c(0, 0.167, 0.333, ..., 1)
#'
#' # Rescale to [1, 7] (convert 1-5 scale to 1-7 scale)
#' x_5pt <- c(1, 2, 3, 4, 5)
#' result <- standardize_range(x_5pt, new_min = 1, new_max = 7)
#' result$standardized  # c(1, 2.5, 4, 5.5, 7)
#'
#' # Rescale to [0, 100]
#' x <- c(10, 20, 30, 40, 50)
#' result <- standardize_range(x, new_min = 0, new_max = 100)
#' result$standardized  # c(0, 25, 50, 75, 100)
#' }
#'
#' @keywords internal
standardize_range <- function(x, new_min = 0, new_max = 1, na.rm = TRUE) {

  if (!is.numeric(x)) {
    rlang::abort(c(
      "x must be numeric",
      "x" = "You provided {class(x)[1]}",
      "i" = "Convert to numeric before standardizing"
    ))
  }

  if (!is.numeric(new_min) || !is.numeric(new_max) ||
      length(new_min) != 1 || length(new_max) != 1) {
    rlang::abort("new_min and new_max must be single numeric values")
  }

  if (new_min >= new_max) {
    rlang::abort(c(
      "new_min must be less than new_max",
      "x" = "You provided new_min = {new_min}, new_max = {new_max}"
    ))
  }

  n_total <- length(x)
  n_missing <- sum(is.na(x))
  n_valid <- n_total - n_missing

  if (n_valid == 0) {
    rlang::abort(c(
      "Cannot standardize: all values are missing",
      "x" = "All {n_total} value{?s} are NA"
    ))
  }

  # Calculate original range
  x_min <- min(x, na.rm = na.rm)
  x_max <- max(x, na.rm = na.rm)

  # Check for zero range
  if (x_min == x_max) {
    rlang::abort(c(
      "Cannot standardize: variable has zero range",
      "x" = "All non-missing values are identical ({x_min})",
      "i" = "Standardization requires variation in the data",
      "i" = "Consider removing this variable or using it as-is"
    ))
  }

  # Standardize using min-max formula
  x_std <- ((x - x_min) / (x_max - x_min)) * (new_max - new_min) + new_min

  # Return with metadata
  list(
    standardized = x_std,
    meta = list(
      method = "range",
      original_min = x_min,
      original_max = x_max,
      new_min = new_min,
      new_max = new_max,
      n_valid = n_valid,
      n_missing = n_missing
    )
  )
}


#' Standardize to Specific Scale (e.g., 1-7 Likert)
#'
#' @description
#' **L1 Internal Helper.** Converts a variable measured on any numeric scale
#' to a target scale (e.g., 1-7 Likert). This is a convenience wrapper around
#' standardize_range() with clearer naming for scale conversion.
#'
#' @param x Numeric vector to convert
#' @param target_min Numeric. Minimum of target scale (e.g., 1 for Likert)
#' @param target_max Numeric. Maximum of target scale (e.g., 7 for Likert)
#' @param na.rm Logical. Should missing values be removed? Default TRUE.
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{standardized}: Numeric vector on target scale
#'     \item \code{meta}: List with standardization metadata (see standardize_range)
#'   }
#'
#' @details
#' **What this does:**
#' Linearly transforms a variable to match a target scale. For example:
#' - Convert 0-100 scale → 1-7 Likert scale
#' - Convert 1-5 scale → 1-7 scale
#' - Convert any range → any target range
#'
#' **Formula:** Same as standardize_range()
#'
#' **Common use case:**
#' You want to create a composite from items on different scales:
#' - Item 1: 1-5 scale (satisfaction)
#' - Item 2: 0-100 scale (NPS)
#' - Item 3: 1-7 scale (likelihood)
#'
#' Convert all to 1-7 scale, then average them.
#'
#' **Peer review justification:**
#' "Items were standardized to a common 1-7 scale using min-max transformation
#' before averaging to create the composite measure."
#'
#' **Note:** This is mathematically identical to standardize_range(), but with
#' clearer semantics for scale conversion.
#'
#' @examples
#' \dontrun{
#' # Convert 1-5 scale to 1-7 scale
#' satisfaction_5pt <- c(1, 2, 3, 4, 5)
#' result <- standardize_scale(satisfaction_5pt, target_min = 1, target_max = 7)
#' result$standardized  # c(1, 2.5, 4, 5.5, 7)
#'
#' # Convert 0-100 NPS to 1-7 scale
#' nps <- c(0, 25, 50, 75, 100)
#' result <- standardize_scale(nps, target_min = 1, target_max = 7)
#' result$standardized  # c(1, 2.5, 4, 5.5, 7)
#'
#' # Create composite from mixed scales
#' df <- data.frame(
#'   item1_5pt = c(1, 2, 3, 4, 5),
#'   item2_100pt = c(0, 25, 50, 75, 100),
#'   item3_7pt = c(1, 2.5, 4, 5.5, 7)
#' )
#'
#' # Standardize first two items to 1-7
#' df$item1_std <- standardize_scale(df$item1_5pt, 1, 7)$standardized
#' df$item2_std <- standardize_scale(df$item2_100pt, 1, 7)$standardized
#'
#' # Now all three items are on 1-7 scale, can average them
#' df$composite <- rowMeans(df[, c("item1_std", "item2_std", "item3_7pt")])
#' }
#'
#' @keywords internal
standardize_scale <- function(x, target_min, target_max, na.rm = TRUE) {

  # Validate target scale
  if (missing(target_min) || missing(target_max)) {
    rlang::abort(c(
      "target_min and target_max are required",
      "i" = "Specify the desired scale range (e.g., target_min = 1, target_max = 7)"
    ))
  }

  # Use range standardization
  result <- standardize_range(
    x = x,
    new_min = target_min,
    new_max = target_max,
    na.rm = na.rm
  )

  # Update method name for clarity
  result$meta$method <- "scale_conversion"
  result$meta$target_min <- target_min
  result$meta$target_max <- target_max

  result
}


#' Standardize Multiple Variables for Composite Creation
#'
#' @description
#' **L1 Internal Helper.** Batch standardization of multiple variables using
#' the same method. Useful for creating composites from multiple items.
#'
#' @param data Data frame containing variables to standardize
#' @param vars Character vector of column names to standardize
#' @param method Character. Standardization method: "z-score", "range", or "scale"
#' @param ... Additional arguments passed to the standardization function
#'   (e.g., new_min, new_max for range/scale methods)
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{data}: Data frame with standardized variables (original names)
#'     \item \code{meta}: Named list with standardization metadata for each variable
#'   }
#'
#' @details
#' **What this does:**
#' Applies the same standardization method to multiple variables at once,
#' preserving metadata for transparency.
#'
#' **Use case:**
#' You have 5 items on different scales that you want to combine:
#' ```
#' result <- standardize_multiple(
#'   data = df,
#'   vars = c("item1", "item2", "item3", "item4", "item5"),
#'   method = "scale",
#'   target_min = 1,
#'   target_max = 7
#' )
#' df_std <- result$data  # All items now on 1-7 scale
#' ```
#'
#' @examples
#' \dontrun{
#' # Standardize multiple items to z-scores
#' df <- data.frame(
#'   item1 = c(1, 2, 3, 4, 5),
#'   item2 = c(10, 20, 30, 40, 50),
#'   item3 = c(100, 200, 300, 400, 500)
#' )
#'
#' result <- standardize_multiple(
#'   data = df,
#'   vars = c("item1", "item2", "item3"),
#'   method = "z-score"
#' )
#'
#' df_std <- result$data  # All items now z-scores
#'
#' # Check metadata for each variable
#' result$meta$item1$original_mean
#' result$meta$item2$original_sd
#' }
#'
#' @keywords internal
standardize_multiple <- function(data, vars, method = c("z-score", "range", "scale"), ...) {

  method <- match.arg(method)

  if (!is.data.frame(data)) {
    rlang::abort(c(
      "data must be a data frame",
      "x" = "You provided {class(data)[1]}"
    ))
  }

  if (!is.character(vars) || length(vars) < 1) {
    rlang::abort("vars must be a character vector with at least one variable name")
  }

  # Check all vars exist
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    rlang::abort(c(
      "Variables not found in data",
      "x" = "Missing: {paste(missing_vars, collapse = ', ')}"
    ))
  }

  # Check all vars are numeric
  non_numeric <- vars[!vapply(data[vars], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    rlang::abort(c(
      "All variables must be numeric",
      "x" = "Non-numeric: {paste(non_numeric, collapse = ', ')}"
    ))
  }

  # Choose standardization function
  std_func <- switch(method,
    "z-score" = standardize_z,
    "range" = standardize_range,
    "scale" = standardize_scale
  )

  # Standardize each variable
  standardized_data <- data
  meta_list <- list()

  for (var in vars) {
    result <- std_func(data[[var]], ...)
    standardized_data[[var]] <- result$standardized
    meta_list[[var]] <- result$meta
  }

  list(
    data = standardized_data,
    meta = meta_list
  )
}
