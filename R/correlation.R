#' Perform Correlation Analysis with Comprehensive Diagnostics
#'
#' @description
#' Calculates correlation coefficients between variables with explicit assumption
#' testing and publication-ready output. Supports Pearson (parametric) and
#' Spearman (non-parametric) correlations.
#'
#' @param data Data frame containing variables, or first numeric vector
#' @param var1 Name of first variable (if data is data frame), or NULL
#' @param var2 Name of second variable (if data is data frame), or second numeric vector
#' @param method Correlation method: "auto" (default), "pearson", or "spearman"
#' @param check_assumptions Logical. Test assumptions? Default is TRUE
#' @param alpha Significance level (default 0.05)
#' @param verbose Logical. Show detailed explanations? Default is TRUE
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{method}: Correlation method used
#'     \item \code{correlation}: Correlation coefficient (r or ρ)
#'     \item \code{p_value}: Statistical significance
#'     \item \code{significant}: Is correlation significant?
#'     \item \code{n}: Sample size
#'     \item \code{ci_lower}: Lower confidence interval (95%, Pearson only)
#'     \item \code{ci_upper}: Upper confidence interval (95%, Pearson only)
#'     \item \code{strength}: Interpretation of correlation strength
#'     \item \code{direction}: Positive, negative, or near-zero
#'     \item \code{assumptions}: Assumption check results (if requested)
#'     \item \code{publication_text}: Publication-ready text
#'     \item \code{interpretation}: Plain English interpretation
#'   }
#'
#' @details
#' ## Correlation Assumptions:
#'
#' **Pearson Correlation:**
#' \itemize{
#'   \item Linearity: Variables have linear relationship
#'   \item Bivariate normality: Both variables normally distributed
#'   \item No outliers: Extreme values can distort correlation
#' }
#'
#' **Spearman Correlation:**
#' \itemize{
#'   \item Monotonic relationship: Variables increase/decrease together
#'   \item No assumptions about distribution
#'   \item Robust to outliers
#' }
#'
#' ## Strength Interpretation (absolute value):
#' \itemize{
#'   \item 0.00 - 0.10: Negligible
#'   \item 0.10 - 0.30: Weak
#'   \item 0.30 - 0.50: Moderate
#'   \item 0.50 - 0.70: Strong
#'   \item 0.70 - 1.00: Very strong
#' }
#'
#' @examples
#' # Example 1: Data frame with variable names
#' data <- data.frame(
#'   customer_satisfaction = c(7, 8, 6, 9, 5, 8, 7, 6, 9, 8),
#'   purchase_intention = c(8, 9, 7, 9, 6, 8, 7, 7, 9, 8)
#' )
#' result <- analyze_correlation(data, "customer_satisfaction", "purchase_intention")
#'
#' # Example 2: Two numeric vectors
#' x <- c(23, 45, 67, 34, 56, 78, 89, 12, 45, 67)
#' y <- c(34, 56, 78, 45, 67, 89, 90, 23, 56, 78)
#' result <- analyze_correlation(x, var2 = y)
#'
#' # Example 3: Force Spearman (non-parametric)
#' result <- analyze_correlation(data, "customer_satisfaction",
#'                                "purchase_intention", method = "spearman")
#'
#' # View publication text
#' print(result, show_publication = TRUE)
#'
#' @export
analyze_correlation <- function(data,
                                 var1 = NULL,
                                 var2 = NULL,
                                 method = "auto",
                                 check_assumptions = TRUE,
                                 alpha = 0.05,
                                 verbose = TRUE) {

  # Step 1: Input processing
  # -------------------------
  if (is.data.frame(data)) {
    # Data frame input
    if (is.null(var1) || is.null(var2)) {
      stop("Error: When 'data' is a data frame, you must specify 'var1' and 'var2'")
    }

    if (!var1 %in% names(data)) {
      stop("Error: Variable '", var1, "' not found in data")
    }
    if (!var2 %in% names(data)) {
      stop("Error: Variable '", var2, "' not found in data")
    }

    x <- data[[var1]]
    y <- data[[var2]]
    x_name <- var1
    y_name <- var2

  } else if (is.numeric(data) && !is.null(var2) && is.numeric(var2)) {
    # Two vectors
    x <- data
    y <- var2
    x_name <- deparse(substitute(data))
    y_name <- deparse(substitute(var2))

  } else {
    stop("Error: Provide either (data.frame, var1, var2) or (vector1, var2 = vector2)")
  }

  # Step 2: Data cleaning
  # ---------------------
  # Remove cases with missing values
  valid_idx <- !is.na(x) & !is.na(y)
  x_clean <- x[valid_idx]
  y_clean <- y[valid_idx]

  n_missing <- sum(!valid_idx)
  if (n_missing > 0 && verbose) {
    message("Note: Removed ", n_missing, " observation(s) with missing values.")
  }

  n <- length(x_clean)

  if (n < 3) {
    stop("Error: Need at least 3 valid paired observations (you have ", n, ")")
  }

  # Step 3: Check assumptions (for method selection if auto)
  # ---------------------------------------------------------
  assumptions <- list()

  if (check_assumptions) {
    if (verbose) {
      message("\nChecking assumptions for correlation analysis...")
    }

    # Check normality of both variables (for Pearson)
    assumptions$normality_x <- check_normality(
      x_clean,
      variable_name = x_name,
      alpha = alpha
    )

    assumptions$normality_y <- check_normality(
      y_clean,
      variable_name = y_name,
      alpha = alpha
    )

    # Check for outliers (simple z-score method)
    z_scores_x <- abs((x_clean - mean(x_clean)) / sd(x_clean))
    z_scores_y <- abs((y_clean - mean(y_clean)) / sd(y_clean))
    outliers_x <- sum(z_scores_x > 3)
    outliers_y <- sum(z_scores_y > 3)

    has_outliers <- (outliers_x + outliers_y) > 0

    if (has_outliers) {
      outlier_text <- paste0(
        "Potential outliers detected: ", outliers_x, " in ", x_name,
        ", ", outliers_y, " in ", y_name,
        " (using |z| > 3 criterion). Outliers can distort Pearson correlations."
      )
    } else {
      outlier_text <- "No major outliers detected (|z| > 3 criterion)."
    }

    assumptions$outliers <- list(
      has_outliers = has_outliers,
      n_outliers_x = outliers_x,
      n_outliers_y = outliers_y,
      interpretation = outlier_text
    )

    # Note: Linearity would be assessed visually via scatterplot
    # We'll mention this in the output

    if (verbose) {
      message("Assumption checks complete.\n")
    }
  }

  # Step 4: Determine correlation method
  # -------------------------------------
  if (method == "auto") {
    # Decision based on assumptions
    if (check_assumptions) {
      norm_x_met <- assumptions$normality_x$assumption_met
      norm_y_met <- assumptions$normality_y$assumption_met

      if (norm_x_met && norm_y_met && !has_outliers) {
        method <- "pearson"
        if (verbose) {
          message("Auto-selection: Using Pearson correlation (assumptions met)")
        }
      } else {
        method <- "spearman"
        if (verbose) {
          message("Auto-selection: Using Spearman correlation (normality violated or outliers present)")
        }
      }
    } else {
      # Default to Pearson if no checks
      method <- "pearson"
      if (verbose) {
        message("Auto-selection: Using Pearson correlation (default)")
      }
    }
  }

  # Validate method
  if (!method %in% c("pearson", "spearman")) {
    stop("Error: method must be 'auto', 'pearson', or 'spearman'")
  }

  # Step 5: Compute correlation
  # ----------------------------
  if (verbose) {
    message("Computing ", method, " correlation...\n")
  }

  cor_test <- cor.test(x_clean, y_clean, method = method)

  r <- cor_test$estimate
  p_value <- cor_test$p.value
  is_significant <- p_value < alpha

  # Confidence interval (only for Pearson)
  if (method == "pearson") {
    ci_lower <- cor_test$conf.int[1]
    ci_upper <- cor_test$conf.int[2]
  } else {
    ci_lower <- NA
    ci_upper <- NA
  }

  # Step 6: Interpret correlation
  # ------------------------------
  # Direction
  if (r > 0.05) {
    direction <- "positive"
  } else if (r < -0.05) {
    direction <- "negative"
  } else {
    direction <- "near-zero"
  }

  # Strength
  abs_r <- abs(r)
  if (abs_r < 0.10) {
    strength <- "negligible"
  } else if (abs_r < 0.30) {
    strength <- "weak"
  } else if (abs_r < 0.50) {
    strength <- "moderate"
  } else if (abs_r < 0.70) {
    strength <- "strong"
  } else {
    strength <- "very strong"
  }

  # Create interpretation
  method_name <- ifelse(method == "pearson", "Pearson", "Spearman")

  interpretation <- paste0(
    "The ", method_name, " correlation between ", x_name, " and ", y_name, " is ",
    strength, " and ", direction, " (r = ", round(r, 3), ", n = ", n,
    ", p = ", round(p_value, 4), "). "
  )

  if (is_significant) {
    interpretation <- paste0(
      interpretation,
      "This correlation is statistically significant at α = ", alpha, ". "
    )

    if (abs_r < 0.30) {
      interpretation <- paste0(
        interpretation,
        "However, note that while statistically significant, the correlation is ",
        strength, ", suggesting a limited practical relationship."
      )
    } else {
      interpretation <- paste0(
        interpretation,
        "The ", strength, " relationship suggests meaningful covariation between the variables."
      )
    }
  } else {
    interpretation <- paste0(
      interpretation,
      "This correlation is NOT statistically significant at α = ", alpha, ". ",
      "There is insufficient evidence of a linear relationship."
    )
  }

  # Add r-squared interpretation
  r_squared <- r^2
  interpretation <- paste0(
    interpretation,
    " Approximately ", round(r_squared * 100, 1),
    "% of variance in one variable is associated with variance in the other."
  )

  # Step 7: Generate publication text
  # ----------------------------------
  pub_block <- generate_publication_block(
    test_type = "correlation",
    assumptions_checks = if (check_assumptions) assumptions else NULL,
    test_results = list(
      method = method_name,
      correlation = r,
      p_value = p_value,
      n = n,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      var1_name = x_name,
      var2_name = y_name
    ),
    additional_notes = paste0(
      "Correlation strength was interpreted using Cohen's (1988) guidelines. ",
      "The correlation coefficient of ", round(r, 3), " indicates that the variables ",
      "share approximately ", round(r_squared * 100, 1), "% common variance (r² = ",
      round(r_squared, 3), ")."
    )
  )

  # Step 8: Package results
  # -----------------------
  results <- list(
    method = method_name,
    correlation = round(r, 4),
    p_value = round(p_value, 6),
    significant = is_significant,
    alpha = alpha,
    n = n,
    ci_lower = if (!is.na(ci_lower)) round(ci_lower, 4) else NA,
    ci_upper = if (!is.na(ci_upper)) round(ci_upper, 4) else NA,
    r_squared = round(r_squared, 4),
    strength = strength,
    direction = direction,
    var1_name = x_name,
    var2_name = y_name,
    assumptions = if (check_assumptions && length(assumptions) > 0) assumptions else NULL,
    interpretation = interpretation,
    publication_text = pub_block,
    cor_test_output = cor_test
  )

  class(results) <- c("correlation_result", "list")

  if (verbose) {
    message("Correlation analysis complete!\n")
  }

  return(results)
}


#' Print Method for Correlation Results
#'
#' @param x A correlation_result object
#' @param show_assumptions Logical - show assumption checks? (default FALSE)
#' @param show_publication Logical - show publication text? (default FALSE)
#' @param ... Additional arguments (not used)
#'
#' @export
print.correlation_result <- function(x, show_assumptions = FALSE, show_publication = FALSE, ...) {
  cat("\n")
  cat("=" %+% rep("=", 75) %+% "=\n", sep = "")
  cat(toupper(x$method), " CORRELATION RESULTS\n", sep = "")
  cat("=" %+% rep("=", 75) %+% "=\n", sep = "")
  cat("\n")

  cat("VARIABLES:\n")
  cat("  Variable 1:", x$var1_name, "\n")
  cat("  Variable 2:", x$var2_name, "\n")
  cat("  Sample size:", x$n, "\n")
  cat("\n")

  cat("CORRELATION:\n")
  cat("  r =", x$correlation, "\n")

  if (!is.na(x$ci_lower) && !is.na(x$ci_upper)) {
    cat("  95% CI: [", x$ci_lower, ",", x$ci_upper, "]\n", sep = "")
  }

  cat("  r² =", x$r_squared, "(", round(x$r_squared * 100, 1), "% shared variance)\n")
  cat("  p-value:", x$p_value, "\n")
  cat("  Significant:", ifelse(x$significant, "YES (p < .05)", "NO (p >= .05)"), "\n")
  cat("\n")

  cat("INTERPRETATION:\n")
  cat("  Strength:", x$strength, "\n")
  cat("  Direction:", x$direction, "\n")
  cat("\n")

  cat("DETAILED INTERPRETATION:\n")
  cat(strwrap(x$interpretation, width = 75, prefix = "  "), sep = "\n")
  cat("\n")

  if (show_assumptions && !is.null(x$assumptions)) {
    cat("\n")
    cat("-" %+% rep("-", 75) %+% "-\n", sep = "")
    cat("ASSUMPTION CHECKS:\n")
    cat("-" %+% rep("-", 75) %+% "-\n", sep = "")

    if (!is.null(x$assumptions$normality_x)) {
      cat("\nNORMALITY (", x$var1_name, "):\n", sep = "")
      print(x$assumptions$normality_x, verbose = FALSE)
    }

    if (!is.null(x$assumptions$normality_y)) {
      cat("\nNORMALITY (", x$var2_name, "):\n", sep = "")
      print(x$assumptions$normality_y, verbose = FALSE)
    }

    if (!is.null(x$assumptions$outliers)) {
      cat("\nOUTLIERS:\n")
      cat("  ", x$assumptions$outliers$interpretation, "\n", sep = "")
    }

    cat("\nNOTE: Linearity should be assessed visually using a scatterplot.\n")
  }

  if (show_publication && !is.null(x$publication_text)) {
    cat("\n")
    print(x$publication_text)
  }

  # Tips
  if (!show_assumptions && !is.null(x$assumptions)) {
    cat("\nTip: Use print(result, show_assumptions = TRUE) to see assumption checks\n")
  }
  if (!show_publication) {
    cat("Tip: Use print(result, show_publication = TRUE) to see publication-ready text\n")
  }

  cat("Tip: Create a scatterplot to visualize the relationship:\n")
  cat("      plot(", x$var1_name, ", ", x$var2_name, ")\n", sep = "")
  cat("\n")

  invisible(x)
}

# Helper function
`%+%` <- function(a, b) paste0(a, b)
