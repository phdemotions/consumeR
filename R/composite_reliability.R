#' Calculate Composite Reliability and Validity Measures
#'
#' This function calculates multiple reliability and validity measures for
#' multi-item scales, going beyond Cronbach's alpha. It includes Composite
#' Reliability (CR) and Average Variance Extracted (AVE), which are commonly
#' required in consumer research and psychology publications.
#'
#' Think of this as a comprehensive health check for your survey scale -
#' it tells you if your questions reliably measure what you think they measure.
#'
#' @param data A data frame containing your survey items
#' @param items Character vector of column names for the items in your scale
#' @param scale_name Character string. Descriptive name for your scale
#' @param item_labels Optional named character vector for plain English labels
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{cronbachs_alpha}: Traditional Cronbach's alpha
#'     \item \code{composite_reliability}: Composite reliability (CR)
#'     \item \code{ave}: Average variance extracted
#'     \item \code{sqrt_ave}: Square root of AVE (for discriminant validity)
#'     \item \code{interpretation}: Plain English summary
#'     \item \code{factor_loadings}: Standardized loadings for each item
#'     \item \code{passes_threshold}: Whether scale meets quality thresholds
#'   }
#'
#' @details
#' ## What These Measures Mean:
#'
#' **Composite Reliability (CR)**:
#' - Like Cronbach's alpha but more accurate
#' - Uses factor loadings instead of correlations
#' - Threshold: CR >= 0.70 is good
#'
#' **Average Variance Extracted (AVE)**:
#' - % of variance explained by the construct vs. measurement error
#' - Threshold: AVE >= 0.50 means construct explains majority of variance
#' - If AVE < 0.50, more variance is due to error than true scores
#'
#' **Square Root of AVE**:
#' - Used to check discriminant validity
#' - Should be higher than correlations with other constructs
#'
#' ## Quality Thresholds (Fornell & Larcker, 1981):
#' - CR >= 0.70: Adequate reliability
#' - AVE >= 0.50: Adequate convergent validity
#' - sqrtAVE > correlation with other constructs: Discriminant validity
#'
#' ## How This is Calculated:
#' 1. Run a factor analysis to get item loadings
#' 2. CR = (sum of loadings)^2 / [(sum of loadings)^2 + sum of error variances]
#' 3. AVE = sum of squared loadings / number of items
#'
#' @examples
#' # Example: Employee engagement scale
#' engagement_data <- data.frame(
#'   engaged_1 = c(7,8,6,9,7,8,9,7,6,8,7,9,8,7,8,9,7,8,7,8),
#'   engaged_2 = c(8,9,7,9,8,9,9,8,7,9,8,9,9,8,9,9,8,9,8,9),
#'   engaged_3 = c(7,8,6,8,7,8,8,7,6,8,7,8,8,7,8,8,7,8,7,8),
#'   engaged_4 = c(8,8,7,9,8,9,9,8,7,9,8,9,9,8,9,9,8,9,8,9)
#' )
#'
#' # Calculate comprehensive reliability
#' reliability <- calculate_composite_reliability(
#'   data = engagement_data,
#'   items = c("engaged_1", "engaged_2", "engaged_3", "engaged_4"),
#'   scale_name = "Employee Engagement"
#' )
#'
#' # View results
#' print(reliability)
#' cat(reliability$interpretation)
#'
#' @export
calculate_composite_reliability <- function(data,
                                           items,
                                           scale_name = "Scale",
                                           item_labels = NULL) {

  message("\n==== COMPOSITE RELIABILITY & VALIDITY ANALYSIS ====")
  message("Scale: ", scale_name)

  # Step 1: Input Validation (same as calculate_alpha)
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data frame.")
  }

  if (!is.character(items) || length(items) < 3) {
    stop("Error: Need at least 3 items for composite reliability. ",
         "You provided ", length(items), " items.")
  }

  missing_items <- items[!items %in% names(data)]
  if (length(missing_items) > 0) {
    stop("Error: Items not found in data: ",
         paste(missing_items, collapse = ", "))
  }

  # Step 2: Extract and Clean Data
  item_data <- data[, items, drop = FALSE]

  # Check numeric
  non_numeric <- items[!sapply(item_data, is.numeric)]
  if (length(non_numeric) > 0) {
    stop("Error: All items must be numeric. Non-numeric: ",
         paste(non_numeric, collapse = ", "))
  }

  # Remove missing
  n_original <- nrow(item_data)
  item_data_complete <- na.omit(item_data)
  n_complete <- nrow(item_data_complete)
  n_missing <- n_original - n_complete

  if (n_missing > 0) {
    message("Removed ", n_missing, " cases with missing values")
    message("Using ", n_complete, " complete responses")
  }

  if (n_complete < 10) {
    warning("Sample size is very small (n=", n_complete, "). ",
            "Results may not be reliable. Recommend n >= 100 for factor analysis.")
  }

  # Step 3: Calculate Cronbach's Alpha (for comparison)
  message("\nCalculating Cronbach's Alpha...")
  k <- ncol(item_data_complete)
  item_variances <- sapply(item_data_complete, var)
  total_variance <- var(rowSums(item_data_complete))
  alpha <- (k / (k - 1)) * (1 - sum(item_variances) / total_variance)
  message("  Cronbach's alpha = ", round(alpha, 3))

  # Step 4: Standardize Items (mean=0, sd=1)
  message("\nStandardizing items for factor analysis...")
  item_data_std <- scale(item_data_complete)

  # Step 5: Run Principal Component Analysis to Get Loadings
  message("Running principal component analysis...")

  # Use first principal component as factor
  pca <- princomp(item_data_std, cor = TRUE)

  # Extract loadings for first component
  # These represent how strongly each item loads on the underlying factor
  loadings <- pca$loadings[, 1]

  # Make sure loadings are positive (flip sign if needed)
  if (sum(loadings) < 0) {
    loadings <- -loadings
  }

  # Absolute values (we care about strength, not direction)
  loadings <- abs(loadings)

  message("  Factor loadings calculated for ", length(loadings), " items")

  # Step 6: Calculate Composite Reliability (CR)
  message("\nCalculating Composite Reliability...")

  # Sum of loadings
  sum_loadings <- sum(loadings)

  # Sum of error variances (1 - loading^2)
  sum_error_variances <- sum(1 - loadings^2)

  # CR formula: (Sumlambda)^2 / [(Sumlambda)^2 + Sum(1-lambda^2)]
  # where lambda = factor loading
  composite_reliability <- (sum_loadings^2) / ((sum_loadings^2) + sum_error_variances)

  message("  Composite Reliability (CR) = ", round(composite_reliability, 3))

  # Step 7: Calculate Average Variance Extracted (AVE)
  message("\nCalculating Average Variance Extracted...")

  # AVE = average of squared loadings
  ave <- mean(loadings^2)

  message("  AVE = ", round(ave, 3), " (", round(ave * 100, 1), "% of variance explained)")

  # Square root of AVE (used for discriminant validity checks)
  sqrt_ave <- sqrt(ave)
  message("  sqrtAVE = ", round(sqrt_ave, 3))

  # Step 8: Create Item Statistics Table
  item_stats <- data.frame(
    item = items,
    loading = loadings,
    loading_squared = loadings^2,
    variance_explained = round(loadings^2 * 100, 1),
    stringsAsFactors = FALSE
  )

  # Add labels if provided
  if (!is.null(item_labels)) {
    item_stats$label <- ifelse(items %in% names(item_labels),
                               item_labels[items],
                               items)
  } else {
    item_stats$label <- items
  }

  # Reorder columns
  item_stats <- item_stats[, c("item", "label", "loading", "loading_squared", "variance_explained")]

  # Step 9: Check Quality Thresholds
  message("\nChecking quality thresholds...")

  cr_pass <- composite_reliability >= 0.70
  ave_pass <- ave >= 0.50

  message("  CR >= 0.70? ", ifelse(cr_pass, "PASS PASS", "FAIL FAIL"))
  message("  AVE >= 0.50? ", ifelse(ave_pass, "PASS PASS", "FAIL FAIL"))

  passes_threshold <- cr_pass && ave_pass

  # Step 10: Generate Interpretation
  interpretation <- paste0(
    "COMPOSITE RELIABILITY ANALYSIS for ", scale_name, ":\n\n",
    "Cronbach's Alpha: alpha = ", round(alpha, 3), "\n",
    "Composite Reliability: CR = ", round(composite_reliability, 3),
    ifelse(cr_pass, " PASS (Good)", " FAIL (Below threshold)"), "\n",
    "Average Variance Extracted: AVE = ", round(ave, 3),
    ifelse(ave_pass, " PASS (Good)", " FAIL (Below threshold)"), "\n",
    "sqrtAVE = ", round(sqrt_ave, 3), "\n\n"
  )

  if (passes_threshold) {
    interpretation <- paste0(interpretation,
      "INTERPRETATION: This scale demonstrates GOOD reliability and validity.\n",
      "- CR >= 0.70 indicates adequate internal consistency\n",
      "- AVE >= 0.50 indicates adequate convergent validity\n",
      "- The construct explains ", round(ave * 100, 1), "% of item variance\n\n",
      "This scale is suitable for use in research and should be accepted by reviewers."
    )
  } else {
    interpretation <- paste0(interpretation,
      "INTERPRETATION: This scale has RELIABILITY or VALIDITY ISSUES.\n"
    )

    if (!cr_pass) {
      interpretation <- paste0(interpretation,
        "- CR < 0.70: Items do not consistently measure the same construct\n")
    }

    if (!ave_pass) {
      interpretation <- paste0(interpretation,
        "- AVE < 0.50: More variance due to measurement error than true construct\n",
        "- Consider removing low-loading items or revising the scale\n")
    }

    interpretation <- paste0(interpretation,
      "\nRECOMMENDATION: Revise scale or remove problematic items before publication."
    )
  }

  # Identify weak items (loading < 0.70)
  weak_items <- item_stats$item[item_stats$loading < 0.70]
  if (length(weak_items) > 0) {
    interpretation <- paste0(interpretation,
      "\n\nWEAK ITEMS (loading < 0.70):\n",
      paste("  - ", weak_items, " (loading = ",
            round(item_stats$loading[item_stats$item %in% weak_items], 3), ")",
            collapse = "\n"),
      "\nConsider removing these items to improve CR and AVE."
    )
  }

  message("\n", interpretation)

  # Step 11: Package Results
  results <- list(
    scale_name = scale_name,
    n_items = k,
    n_observations = n_complete,
    cronbachs_alpha = alpha,
    composite_reliability = composite_reliability,
    ave = ave,
    sqrt_ave = sqrt_ave,
    cr_pass = cr_pass,
    ave_pass = ave_pass,
    passes_threshold = passes_threshold,
    item_statistics = item_stats,
    interpretation = interpretation
  )

  class(results) <- c("composite_reliability", "list")

  message("\n==== ANALYSIS COMPLETE ====\n")

  return(results)
}


#' Print Method for Composite Reliability
#'
#' @param x A composite_reliability object
#' @param ... Additional arguments (not used)
#' @export
print.composite_reliability <- function(x, ...) {
  cat("\n")
  cat("==========================================================\n")
  cat("|       COMPOSITE RELIABILITY & VALIDITY ANALYSIS        |\n")
  cat("==========================================================\n\n")

  cat("Scale:", x$scale_name, "\n")
  cat("Number of items:", x$n_items, "\n")
  cat("Sample size:", x$n_observations, "\n\n")

  cat("RELIABILITY MEASURES:\n")
  cat("  Cronbach's Alpha:", round(x$cronbachs_alpha, 3), "\n")
  cat("  Composite Reliability (CR):", round(x$composite_reliability, 3),
      ifelse(x$cr_pass, "PASS", "FAIL"), "\n\n")

  cat("VALIDITY MEASURES:\n")
  cat("  Average Variance Extracted (AVE):", round(x$ave, 3),
      ifelse(x$ave_pass, "PASS", "FAIL"), "\n")
  cat("  Square Root of AVE (sqrtAVE):", round(x$sqrt_ave, 3), "\n")
  cat("  Variance explained by construct:", round(x$ave * 100, 1), "%\n\n")

  cat("QUALITY ASSESSMENT:\n")
  if (x$passes_threshold) {
    cat("  Status: PASS PASSED all thresholds\n")
    cat("  This scale is suitable for research use\n\n")
  } else {
    cat("  Status: FAIL FAILED quality thresholds\n")
    cat("  Consider revising scale before publication\n\n")
  }

  cat("ITEM LOADINGS:\n")
  print(x$item_statistics[, c("label", "loading", "variance_explained")],
        row.names = FALSE)
  cat("\nNote: Loadings >= 0.70 are considered strong\n\n")

  invisible(x)
}
