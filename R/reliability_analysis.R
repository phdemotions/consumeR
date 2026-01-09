#' Calculate Cronbach's Alpha for Scale Reliability
#'
#' This function calculates Cronbach's alpha and other reliability measures
#' for multi-item scales in a completely transparent way. Every step is
#' documented so reviewers can understand exactly how reliability was assessed.
#'
#' Think of this like checking if survey questions measure the same thing
#' consistently - like asking "How satisfied are you?" in three different ways
#' and making sure people answer similarly to all three.
#'
#' @param data A data frame containing your survey items
#' @param items Character vector of column names for the items in your scale.
#'   For example: c("satisfaction_1", "satisfaction_2", "satisfaction_3")
#' @param scale_name Character string. A descriptive name for your scale
#'   (e.g., "Customer Satisfaction"). This makes output easier to read.
#' @param reverse_items Optional character vector of items to reverse-score.
#'   Use this when some items are worded negatively. Default is NULL.
#' @param item_labels Optional named character vector mapping item names to
#'   plain English labels. For example: c(satisfaction_1 = "Overall satisfaction",
#'   satisfaction_2 = "Would recommend"). Default is NULL.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{alpha}: Cronbach's alpha coefficient (0 to 1)
#'     \item \code{interpretation}: Plain English interpretation
#'     \item \code{n_items}: Number of items in the scale
#'     \item \code{n_observations}: Number of complete responses
#'     \item \code{item_statistics}: Data frame with statistics for each item
#'     \item \code{alpha_if_deleted}: What alpha would be if each item removed
#'     \item \code{inter_item_correlations}: Average correlation between items
#'     \item \code{scale_mean}: Mean of the composite scale
#'     \item \code{scale_sd}: Standard deviation of the composite scale
#'   }
#'
#' @details
#' ## What is Cronbach's Alpha?
#' Cronbach's alpha measures internal consistency - whether items that are
#' supposed to measure the same thing actually correlate with each other.
#'
#' ## Interpreting Alpha:
#' - **α ≥ 0.90**: Excellent reliability
#' - **α ≥ 0.80**: Good reliability
#' - **α ≥ 0.70**: Acceptable reliability
#' - **α ≥ 0.60**: Questionable reliability
#' - **α < 0.60**: Poor reliability
#'
#' ## How It's Calculated:
#' Alpha = (k / (k-1)) * (1 - sum of item variances / total variance)
#' where k = number of items
#'
#' ## Transparency for Reviewers:
#' This function shows:
#' 1. Which items were included
#' 2. How many responses were used
#' 3. Statistics for each individual item
#' 4. What happens if you remove each item
#' 5. The final alpha value with interpretation
#'
#' @examples
#' # Example 1: Customer satisfaction scale
#' # Imagine we asked Cloud 9 customers three satisfaction questions
#' satisfaction_data <- data.frame(
#'   customer_id = 1:20,
#'   sat_overall = c(7,8,6,9,7,8,9,7,6,8,7,9,8,7,8,9,7,8,7,8),
#'   sat_recommend = c(8,9,7,9,8,9,9,8,7,9,8,9,9,8,9,9,8,9,8,9),
#'   sat_return = c(7,8,6,8,7,8,8,7,6,8,7,8,8,7,8,8,7,8,7,8)
#' )
#'
#' # Calculate reliability
#' reliability <- calculate_alpha(
#'   data = satisfaction_data,
#'   items = c("sat_overall", "sat_recommend", "sat_return"),
#'   scale_name = "Customer Satisfaction"
#' )
#'
#' # View results
#' cat(reliability$interpretation)
#' print(reliability$alpha)
#'
#' # Example 2: With item labels for clarity
#' reliability <- calculate_alpha(
#'   data = satisfaction_data,
#'   items = c("sat_overall", "sat_recommend", "sat_return"),
#'   scale_name = "Customer Satisfaction",
#'   item_labels = c(
#'     sat_overall = "Overall satisfaction with Cloud 9",
#'     sat_recommend = "Would recommend to friends",
#'     sat_return = "Likelihood to return"
#'   )
#' )
#'
#' # Example 3: With reverse-coded items
#' # Some items might be worded negatively (e.g., "I was dissatisfied")
#' mixed_data <- data.frame(
#'   happy_1 = c(7,8,9,7,8),      # Positive wording
#'   unhappy_2 = c(3,2,1,3,2),    # Negative wording (needs reversing)
#'   happy_3 = c(8,9,9,8,9)       # Positive wording
#' )
#'
#' reliability <- calculate_alpha(
#'   data = mixed_data,
#'   items = c("happy_1", "unhappy_2", "happy_3"),
#'   scale_name = "Happiness",
#'   reverse_items = c("unhappy_2")  # Reverse the negative item
#' )
#'
#' @export
calculate_alpha <- function(data,
                           items,
                           scale_name = "Scale",
                           reverse_items = NULL,
                           item_labels = NULL) {

  # Step 1: Input Validation
  # -------------------------
  # Make sure we have valid input before doing any calculations

  # Check that data is a data frame
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data frame. ",
         "You provided: ", class(data)[1])
  }

  # Check that items is a character vector
  if (!is.character(items) || length(items) < 2) {
    stop("Error: 'items' must be a character vector with at least 2 item names. ",
         "You need at least 2 items to calculate reliability.")
  }

  # Check that all items exist in the data
  missing_items <- items[!items %in% names(data)]
  if (length(missing_items) > 0) {
    stop("Error: The following items were not found in data: ",
         paste(missing_items, collapse = ", "), "\n",
         "Available columns: ", paste(names(data), collapse = ", "))
  }

  # Step 2: Extract and Prepare Item Data
  # --------------------------------------
  message("\n==== CRONBACH'S ALPHA ANALYSIS ====")
  message("Scale: ", scale_name)
  message("Number of items: ", length(items))

  # Select only the items we need
  item_data <- data[, items, drop = FALSE]

  # Check if items are numeric
  non_numeric <- items[!sapply(item_data, is.numeric)]
  if (length(non_numeric) > 0) {
    stop("Error: All items must be numeric. ",
         "The following items are not numeric: ",
         paste(non_numeric, collapse = ", "))
  }

  # Step 3: Handle Reverse-Coded Items
  # -----------------------------------
  if (!is.null(reverse_items)) {
    message("\nReverse-coding items: ", paste(reverse_items, collapse = ", "))

    # Check that reverse items are in the item list
    invalid_reverse <- reverse_items[!reverse_items %in% items]
    if (length(invalid_reverse) > 0) {
      stop("Error: The following reverse items are not in the items list: ",
           paste(invalid_reverse, collapse = ", "))
    }

    # Reverse-code by: new_value = (max + min) - old_value
    for (item in reverse_items) {
      item_min <- min(item_data[[item]], na.rm = TRUE)
      item_max <- max(item_data[[item]], na.rm = TRUE)
      item_data[[item]] <- (item_max + item_min) - item_data[[item]]
      message("  - Reversed ", item, " (scale: ", item_min, "-", item_max, ")")
    }
  }

  # Step 4: Remove Missing Data
  # ----------------------------
  n_original <- nrow(item_data)
  n_missing <- sum(!complete.cases(item_data))

  # Remove rows with any missing values
  item_data_complete <- na.omit(item_data)
  n_complete <- nrow(item_data_complete)

  if (n_missing > 0) {
    message("\nMissing data:")
    message("  - Original n: ", n_original)
    message("  - Removed ", n_missing, " cases with missing values")
    message("  - Final n: ", n_complete)
  }

  if (n_complete < 2) {
    stop("Error: Need at least 2 complete responses to calculate alpha. ",
         "Only ", n_complete, " complete responses found.")
  }

  # Step 5: Calculate Item Statistics
  # ----------------------------------
  message("\nCalculating item statistics...")

  # For each item, calculate mean, SD, and correlation with total
  item_stats <- data.frame(
    item = items,
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

  # Calculate statistics for each item
  item_stats$mean <- sapply(item_data_complete, mean)
  item_stats$sd <- sapply(item_data_complete, sd)

  # Calculate total score (sum of all items)
  total_score <- rowSums(item_data_complete)

  # Correlation of each item with total score
  item_stats$item_total_correlation <- sapply(names(item_data_complete), function(item) {
    # Corrected item-total correlation (correlation with total minus this item)
    total_minus_item <- total_score - item_data_complete[[item]]
    cor(item_data_complete[[item]], total_minus_item)
  })

  # Step 6: Calculate Cronbach's Alpha
  # -----------------------------------
  message("\nCalculating Cronbach's alpha...")

  # Number of items
  k <- ncol(item_data_complete)

  # Variance of each item
  item_variances <- sapply(item_data_complete, var)

  # Variance of the total score
  total_variance <- var(total_score)

  # Cronbach's alpha formula:
  # α = (k / (k-1)) * (1 - sum of item variances / total variance)
  alpha <- (k / (k - 1)) * (1 - sum(item_variances) / total_variance)

  message("  Alpha = ", round(alpha, 3))

  # Step 7: Calculate Alpha if Item Deleted
  # ----------------------------------------
  # This shows what alpha would be if we removed each item
  # Useful for identifying problematic items

  alpha_if_deleted <- sapply(names(item_data_complete), function(item) {
    # Remove this item temporarily
    temp_data <- item_data_complete[, names(item_data_complete) != item, drop = FALSE]

    # Recalculate alpha
    k_temp <- ncol(temp_data)
    item_vars_temp <- sapply(temp_data, var)
    total_var_temp <- var(rowSums(temp_data))

    (k_temp / (k_temp - 1)) * (1 - sum(item_vars_temp) / total_var_temp)
  })

  item_stats$alpha_if_deleted <- alpha_if_deleted

  # Step 8: Calculate Inter-Item Correlations
  # ------------------------------------------
  # Average correlation between all pairs of items

  item_cors <- cor(item_data_complete)
  # Get lower triangle (to avoid counting correlations twice)
  lower_tri <- item_cors[lower.tri(item_cors)]
  mean_inter_item_cor <- mean(lower_tri)

  # Step 9: Interpret Alpha
  # ------------------------
  interpretation <- if (alpha >= 0.90) {
    paste0("EXCELLENT reliability (α = ", round(alpha, 3), "). ",
           "The ", scale_name, " scale shows excellent internal consistency. ",
           "Items are highly inter-correlated and measure the same construct well.")
  } else if (alpha >= 0.80) {
    paste0("GOOD reliability (α = ", round(alpha, 3), "). ",
           "The ", scale_name, " scale shows good internal consistency. ",
           "Items correlate well with each other.")
  } else if (alpha >= 0.70) {
    paste0("ACCEPTABLE reliability (α = ", round(alpha, 3), "). ",
           "The ", scale_name, " scale shows acceptable internal consistency. ",
           "This meets the minimum threshold for research purposes.")
  } else if (alpha >= 0.60) {
    paste0("QUESTIONABLE reliability (α = ", round(alpha, 3), "). ",
           "The ", scale_name, " scale shows questionable internal consistency. ",
           "Consider revising items or removing poorly performing items.")
  } else {
    paste0("POOR reliability (α = ", round(alpha, 3), "). ",
           "The ", scale_name, " scale shows poor internal consistency. ",
           "Items may not be measuring the same construct. ",
           "Substantial revision needed.")
  }

  message("\n", interpretation)

  # Step 10: Check for Problematic Items
  # -------------------------------------
  # Items that would improve alpha if removed

  problematic <- item_stats$item[item_stats$alpha_if_deleted > alpha]
  if (length(problematic) > 0) {
    message("\nNote: Removing these items would INCREASE alpha:")
    for (prob_item in problematic) {
      new_alpha <- item_stats$alpha_if_deleted[item_stats$item == prob_item]
      improvement <- new_alpha - alpha
      message("  - ", prob_item, " (new α = ", round(new_alpha, 3),
              ", improvement = +", round(improvement, 3), ")")
    }
    message("Consider whether these items truly belong in this scale.")
  }

  # Step 11: Package Results
  # -------------------------
  results <- list(
    scale_name = scale_name,
    alpha = alpha,
    interpretation = interpretation,
    n_items = k,
    n_observations = n_complete,
    n_missing = n_missing,
    item_statistics = item_stats,
    inter_item_correlation = mean_inter_item_cor,
    scale_mean = mean(total_score),
    scale_sd = sd(total_score),
    reverse_coded_items = reverse_items,
    raw_data = item_data_complete
  )

  class(results) <- c("alpha_analysis", "list")

  message("\n==== ANALYSIS COMPLETE ====\n")

  return(results)
}


#' Print Method for Alpha Analysis
#'
#' @param x An alpha_analysis object
#' @param ... Additional arguments (not used)
#' @export
print.alpha_analysis <- function(x, ...) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════╗\n")
  cat("║           CRONBACH'S ALPHA RELIABILITY ANALYSIS        ║\n")
  cat("╚════════════════════════════════════════════════════════╝\n\n")

  cat("Scale:", x$scale_name, "\n")
  cat("Cronbach's Alpha: α =", round(x$alpha, 3), "\n\n")

  cat(x$interpretation, "\n\n")

  cat("Sample Information:\n")
  cat("  Number of items:", x$n_items, "\n")
  cat("  Number of observations:", x$n_observations, "\n")
  if (x$n_missing > 0) {
    cat("  Missing cases removed:", x$n_missing, "\n")
  }
  cat("\n")

  cat("Scale Statistics:\n")
  cat("  Mean:", round(x$scale_mean, 2), "\n")
  cat("  SD:", round(x$scale_sd, 2), "\n")
  cat("  Average inter-item correlation:", round(x$inter_item_correlation, 3), "\n")
  cat("\n")

  cat("Item Statistics:\n")
  print(x$item_statistics[, c("label", "mean", "sd", "item_total_correlation", "alpha_if_deleted")],
        row.names = FALSE)
  cat("\n")

  invisible(x)
}
