#' Check Statistical Assumptions
#'
#' @description
#' This file contains functions to explicitly check statistical assumptions
#' for various tests. Each function provides detailed interpretation suitable
#' for publication and educational purposes.
#'
#' @name assumptions
NULL

#' Check Normality Assumption (Gold Standard)
#'
#' @description
#' Tests whether data follows a normal distribution using the Shapiro-Wilk test.
#' Provides comprehensive interpretation for beginners, including visual diagnostics,
#' educational explanations, and APA 7 formatted results.
#'
#' This function is designed to be self-explanatory for users new to statistics,
#' with extensive validation and beginner-friendly error messages.
#'
#' @param data Numeric vector to test for normality
#' @param variable_name Character string naming the variable (for reporting).
#'   Default is "data". Used in all output messages.
#' @param alpha Numeric. Significance level for the test (default 0.05).
#'   Must be between 0 and 1.
#'
#' @return An S3 object of class "assumption_check" containing:
#'   \itemize{
#'     \item \code{test}: Name of the test ("Shapiro-Wilk")
#'     \item \code{assumption}: Which assumption was tested ("Normality")
#'     \item \code{assumption_met}: Logical. TRUE if assumption is met (p > alpha)
#'     \item \code{p_value}: P-value from Shapiro-Wilk test (APA 7 formatted)
#'     \item \code{statistic}: W statistic (ranges 0-1, higher = more normal)
#'     \item \code{n}: Sample size (after removing NA)
#'     \item \code{n_missing}: Number of NA values removed
#'     \item \code{alpha}: Significance level used
#'     \item \code{skewness}: Measure of asymmetry (0 = symmetric)
#'     \item \code{kurtosis}: Measure of tail heaviness (0 = normal tails)
#'     \item \code{interpretation}: One-sentence summary for quick reference
#'     \item \code{recommendation}: Actionable next steps
#'     \item \code{publication_text}: APA 7 formatted text for manuscripts
#'     \item \code{verbose_explanation}: Educational explanation for beginners
#'     \item \code{visual_guide}: How to create diagnostic plots
#'   }
#'
#' @details
#' **What is the normality assumption?**
#'
#' Many statistical tests (t-tests, ANOVA, linear regression) assume that data
#' follows a "normal distribution" (bell-shaped curve). This function tests
#' whether your data meets this assumption.
#'
#' **How to interpret results:**
#' - If p > 0.05: Normality assumption is MET (good news!)
#' - If p < 0.05: Normality assumption is VIOLATED (need alternatives)
#'
#' **What to do if violated:**
#' 1. Use non-parametric tests (don't assume normality)
#' 2. Transform data (log, square root, Box-Cox)
#' 3. If n > 30, parametric tests often still work (Central Limit Theorem)
#'
#' @examples
#' # Example 1: Normal data
#' normal_data <- rnorm(100, mean = 5, sd = 1)
#' check_normality(normal_data, "satisfaction_scores")
#'
#' # Example 2: Skewed data (not normal)
#' skewed_data <- rexp(100, rate = 0.5)
#' check_normality(skewed_data, "income")
#'
#' # Example 3: With missing values
#' data_with_na <- c(rnorm(95), NA, NA, NA, NA, NA)
#' check_normality(data_with_na, "responses")
#'
#' @export
check_normality <- function(data, variable_name = "data", alpha = 0.05) {

  # ============================================================================
  # COMPREHENSIVE INPUT VALIDATION (Gold Standard)
  # ============================================================================

  # Check 1: Type validation for data
  if (!is.numeric(data)) {
    rlang::abort(c(
      "Data must be numeric",
      "x" = paste0("You provided: ", class(data)[1], " (", typeof(data), ")"),
      "i" = "Normality tests require numeric data (numbers)",
      "i" = "Common mistake: Passing factor or character variables",
      ">" = "For factors: Convert with as.numeric(as.character(your_data))",
      ">" = "For data frames: Extract column with df$column_name",
      ">" = "Check data type: class(your_data)"
    ))
  }

  # Check 2: Validate variable_name is character
  if (!is.character(variable_name) || length(variable_name) != 1) {
    rlang::abort(c(
      "variable_name must be a single character string",
      "x" = paste0("You provided: ", class(variable_name)[1]),
      "i" = "This is used for labeling output",
      ">" = "Example: check_normality(data, variable_name = \"age\")"
    ))
  }

  # Check 3: Validate alpha parameter
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    rlang::abort(c(
      "alpha must be a single number between 0 and 1",
      "x" = paste0("You provided: ", alpha),
      "i" = "alpha is the significance level for the test",
      "i" = "Common values: 0.05 (5%), 0.01 (1%), 0.10 (10%)",
      ">" = "Example: check_normality(data, alpha = 0.05)"
    ))
  }

  # Check 4: Handle empty data
  if (length(data) == 0) {
    rlang::abort(c(
      "Cannot test normality on empty data",
      "x" = "You provided a vector with 0 elements",
      "i" = "Need at least 3 observations to test normality",
      ">" = "Check: Did you subset incorrectly?",
      ">" = "Try: length(your_data) to see how many values you have"
    ))
  }

  # Check 5: Detect NaN values
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

  # Check 6: Detect Infinite values
  if (any(is.infinite(data))) {
    inf_count <- sum(is.infinite(data))
    inf_pos <- sum(data == Inf, na.rm = TRUE)
    inf_neg <- sum(data == -Inf, na.rm = TRUE)
    rlang::warn(c(
      "Infinite values detected",
      "!" = paste0(inf_count, " infinite value(s): ",
                   if (inf_pos > 0) paste0(inf_pos, " Inf"), " ",
                   if (inf_neg > 0) paste0(inf_neg, " -Inf")),
      "i" = "Infinite values prevent normality testing",
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

  # Step 7: Remove missing values and count them
  n_missing <- sum(is.na(data))
  data_clean <- data[!is.na(data)]
  n <- length(data_clean)

  # Check 8: Handle all-NA data
  if (n == 0) {
    rlang::abort(c(
      "No valid (non-missing) data points found",
      "x" = paste0("All ", length(data), " values are NA, NaN, or Inf"),
      "i" = "Cannot test normality without any valid numbers",
      "!" = "Possible causes:",
      "  " = "  • Data import failed",
      "  " = "  • Wrong column selected",
      "  " = "  • Calculation error created only missing values",
      ">" = "Check: summary(your_original_data)",
      ">" = "Check: str(your_data) to see structure"
    ))
  }

  # Check 9: Warn about missing data
  if (n_missing > 0) {
    pct_missing <- round(100 * n_missing / length(data), 1)

    if (pct_missing > 50) {
      rlang::warn(c(
        "More than half of data is missing",
        "!" = paste0(n_missing, " of ", length(data), " values are NA (",
                     pct_missing, "%)"),
        "i" = "Normality test based on only ", n, " observations",
        "!" = "High missingness may indicate:",
        "  " = "  • Data collection problems",
        "  " = "  • Wrong variable selected",
        "  " = "  • Systematic bias (non-random missingness)",
        ">" = "Investigate: Why is so much data missing?",
        ">" = "Consider: Are results still meaningful with this much missing data?"
      ))
    } else if (pct_missing > 20) {
      rlang::warn(c(
        "Substantial missing data",
        "!" = paste0(n_missing, " of ", length(data), " values are NA (",
                     pct_missing, "%)"),
        "i" = "Normality test computed on ", n, " complete cases",
        ">" = "Consider: Investigating pattern of missingness"
      ))
    } else {
      message("Note: ", n_missing, " missing value(s) removed from normality test (",
              pct_missing, "% of data)")
    }
  }

  # Check 10: Sample size requirements for Shapiro-Wilk test
  if (n < 3) {
    rlang::abort(c(
      "Sample size too small for normality test",
      "x" = paste0("n = ", n, " (after removing ", n_missing, " missing values)"),
      "i" = "The Shapiro-Wilk test requires at least 3 observations",
      "i" = "Most statistical tests need n >= 3 anyway",
      "!" = "Options:",
      "  " = "  • Collect more data (recommended)",
      "  " = "  • Combine with other groups (if appropriate)",
      "  " = "  • Use non-parametric tests (don't assume normality)",
      ">" = "With n < 3, statistical testing is generally not feasible"
    ))
  }

  # Check 11: Warn about very small samples (normality test unreliable)
  if (n >= 3 && n < 10) {
    rlang::warn(c(
      "Very small sample size",
      "!" = paste0("n = ", n, " (after removing ", n_missing, " missing)"),
      "i" = "Normality tests are unreliable with n < 10",
      "i" = "The Shapiro-Wilk test needs larger samples for reliable results",
      "!" = "Recommendations:",
      "  " = "  • Collect more data if possible",
      "  " = "  • Use visual inspection (Q-Q plot, histogram)",
      "  " = "  • Consider non-parametric tests as safer option",
      ">" = "Results should be interpreted with extreme caution"
    ))
  }

  # Check 12: Warn about zero variance (all values identical)
  if (length(unique(data_clean)) == 1) {
    rlang::abort(c(
      "Zero variance: all values are identical",
      "!" = paste0("All ", n, " values = ", data_clean[1]),
      "i" = "Data with zero variance cannot be tested for normality",
      "i" = "A constant variable is technically \"normal\" but uninformative",
      "!" = "This is unusual and may indicate:",
      "  " = "  • Constant/fixed value (by design)",
      "  " = "  • Rounding error (all rounded to same value)",
      "  " = "  • Data entry error",
      "  " = "  • Wrong variable selected",
      ">" = "Check: Is this expected?",
      ">" = "Statistical tests will fail with zero variance"
    ))
  }

  # Sample size warning
  if (n > 5000) {
    size_note <- paste0(
      "Note: With large samples (n = ", n, "), the Shapiro-Wilk test becomes ",
      "very sensitive to minor deviations from normality. Visual inspection ",
      "(Q-Q plots, histograms) should also be used to assess practical significance."
    )
  } else {
    size_note <- NULL
  }

  # Perform Shapiro-Wilk test
  if (n <= 5000) {
    test_result <- shapiro.test(data_clean)
    w_stat <- test_result$statistic
    p_val <- test_result$p.value
  } else {
    # For very large samples, use subset for Shapiro-Wilk
    test_result <- shapiro.test(sample(data_clean, 5000))
    w_stat <- test_result$statistic
    p_val <- test_result$p.value
  }

  # Determine if assumption is met
  assumption_met <- p_val > alpha

  # Calculate descriptive statistics for context
  mean_val <- mean(data_clean)
  sd_val <- sd(data_clean)
  skew <- (sum((data_clean - mean_val)^3) / n) / (sd_val^3)
  kurt <- (sum((data_clean - mean_val)^4) / n) / (sd_val^4) - 3

  # ============================================================================
  # INTEGRATE LEVEL 1 UTILITIES (Gold Standard)
  # ============================================================================

  # Format p-value using Level 1 function
  p_value_formatted <- format_p(p_val, digits = 3, style = "apa")

  # Format sample size using Level 1 function
  n_formatted <- format_n(n, type = "n", include_label = TRUE)

  # ============================================================================
  # CREATE INTERPRETATION AND RECOMMENDATIONS
  # ============================================================================

  # Create interpretation
  if (assumption_met) {
    basic_interp <- paste0(
      "The normality assumption is MET for ", variable_name,
      " (Shapiro-Wilk W = ", round(w_stat, 4),
      ", ", p_value_formatted, ", p > ", alpha, ")."
    )

    recommendation <- paste0(
      "Parametric tests (e.g., t-tests, ANOVA, linear regression) are appropriate. ",
      "The data distribution does not significantly deviate from normality."
    )

    pub_text <- paste0(
      "Data normality was assessed using the Shapiro-Wilk test. ",
      "The ", variable_name, " variable met the normality assumption ",
      "(W = ", round(w_stat, 4), ", ", p_value_formatted, ")."
    )

    verbose <- paste0(
      "The Shapiro-Wilk test evaluates whether a sample comes from a normally distributed ",
      "population. The null hypothesis states that the data are normally distributed. ",
      "With a p-value of ", round(p_val, 4), " (which is greater than our alpha level of ",
      alpha, "), we fail to reject the null hypothesis. This means we do not have ",
      "sufficient evidence to conclude that the data significantly deviate from a normal ",
      "distribution.\n\n",
      "In practical terms: Your data for ", variable_name, " appears to follow a bell-shaped ",
      "(normal) distribution, which is excellent news. This means you can proceed with ",
      "parametric statistical tests that assume normality (such as t-tests, ANOVA, and ",
      "linear regression) with confidence. These tests tend to be more powerful than their ",
      "non-parametric alternatives when assumptions are met.\n\n",
      "Technical details: The Shapiro-Wilk W statistic ranges from 0 to 1, with values closer ",
      "to 1 indicating better fit to normality. Your W value of ", round(w_stat, 4),
      " suggests a good fit. Skewness = ", round(skew, 3), " (values near 0 indicate symmetry), ",
      "and excess kurtosis = ", round(kurt, 3), " (values near 0 indicate normal tail behavior)."
    )

  } else {
    basic_interp <- paste0(
      "The normality assumption is VIOLATED for ", variable_name,
      " (Shapiro-Wilk W = ", round(w_stat, 4),
      ", ", p_value_formatted, ", p < ", alpha, ")."
    )

    # Create transformation suggestion based on skewness
    transform_suggestion <- ""
    if (abs(skew) > 1) {
      if (skew > 0) {
        transform_suggestion <- paste0(
          "\n\nTRANSFORMATION SUGGESTION:\n",
          "Your data shows positive skew (", round(skew, 3), "), meaning it has a long right tail.\n",
          "Try these transformations in order:\n",
          "  1. Log transformation:    log_", variable_name, " <- log(", variable_name, ")\n",
          "  2. Square root:           sqrt_", variable_name, " <- sqrt(", variable_name, ")\n",
          "Note: Log requires all positive values. If you have zeros, use log(", variable_name, " + 1)."
        )
      } else {
        transform_suggestion <- paste0(
          "\n\nTRANSFORMATION SUGGESTION:\n",
          "Your data shows negative skew (", round(skew, 3), "), meaning it has a long left tail.\n",
          "Try reflecting then transforming:\n",
          "  1. Reflect:               reflected <- max(", variable_name, ") + 1 - ", variable_name, "\n",
          "  2. Then log or sqrt:      log(reflected) or sqrt(reflected)\n",
          "  3. Reflect back if needed for interpretation"
        )
      }
    }

    recommendation <- paste0(
      "Consider: (1) Using non-parametric alternatives (e.g., Mann-Whitney U instead of t-test, ",
      "Kruskal-Wallis instead of ANOVA), (2) Transforming the data (log, square root, Box-Cox), ",
      "or (3) If sample size is large (n > 30), parametric tests may still be robust due to ",
      "the Central Limit Theorem. Your sample size is ", n, ".",
      transform_suggestion
    )

    pub_text <- paste0(
      "Data normality was assessed using the Shapiro-Wilk test. ",
      "The ", variable_name, " variable violated the normality assumption ",
      "(W = ", round(w_stat, 4), ", ", p_value_formatted, "). ",
      "Accordingly, non-parametric tests were employed."
    )

    verbose <- paste0(
      "The Shapiro-Wilk test evaluates whether a sample comes from a normally distributed ",
      "population. The null hypothesis states that the data are normally distributed. ",
      "With a p-value of ", round(p_val, 4), " (which is less than our alpha level of ",
      alpha, "), we reject the null hypothesis. This means we have strong evidence that ",
      "the data significantly deviate from a normal distribution.\n\n",
      "In practical terms: Your data for ", variable_name, " does NOT follow a bell-shaped ",
      "(normal) distribution. This is actually quite common in real-world consumer research data. ",
      "When normality is violated, you have several options:\n\n",
      "1. USE NON-PARAMETRIC TESTS: These tests don't assume normality and are perfectly valid. ",
      "Examples include Mann-Whitney U test (instead of t-test), Kruskal-Wallis test (instead of ANOVA), ",
      "and Spearman correlation (instead of Pearson).\n\n",
      "2. TRANSFORM YOUR DATA: Sometimes a mathematical transformation (like taking the log or square root) ",
      "can make skewed data more normal. However, remember that you'll be analyzing the transformed values, ",
      "which can make interpretation more complex.",
      transform_suggestion, "\n\n",
      "3. PROCEED WITH CAUTION IF SAMPLE IS LARGE: If you have a large sample (typically n > 30 per group), ",
      "parametric tests are often still robust to violations of normality due to the Central Limit Theorem. ",
      "Your sample size is ", n, ".\n\n",
      "Technical details: The Shapiro-Wilk W statistic of ", round(w_stat, 4),
      " indicates deviation from normality. Skewness = ", round(skew, 3),
      " (positive values indicate right skew, negative indicate left skew), ",
      "and excess kurtosis = ", round(kurt, 3), " (positive values indicate heavy tails, ",
      "negative indicate light tails)."
    )
  }

  # Add size note if applicable
  if (!is.null(size_note)) {
    verbose <- paste0(verbose, "\n\n", size_note)
  }

  # ============================================================================
  # VISUAL DIAGNOSTIC GUIDE (Gold Standard)
  # ============================================================================

  visual_guide <- paste0(
    "========================================================================\n",
    "VISUAL DIAGNOSTICS: How to Check Normality with Plots\n",
    "========================================================================\n\n",
    "Creating diagnostic plots is often more informative than statistical tests alone.\n",
    "Here's how to create and interpret the two most useful plots:\n\n",
    "--- Q-Q PLOT (Quantile-Quantile Plot) ---\n\n",
    "Create the plot:\n",
    "  qqnorm(your_data, main = \"Q-Q Plot: ", variable_name, "\")\n",
    "  qqline(your_data, col = \"red\")\n\n",
    "How to interpret:\n",
    "  • Points fall ON the red line = Normal distribution ✓\n",
    "  • Points curve ABOVE the line at ends = Heavy tails (outliers)\n",
    "  • Points curve BELOW the line at ends = Light tails (no outliers)\n",
    "  • S-shaped curve = Skewed distribution\n",
    "  • Systematic deviation = Non-normal\n\n",
    "What's considered 'good enough':\n",
    "  - Some deviation is normal, especially at the extremes\n",
    "  - Focus on the middle 80% of points\n",
    "  - Minor wiggles are OK; systematic patterns are concerning\n\n",
    "--- HISTOGRAM WITH NORMAL CURVE ---\n\n",
    "Create the plot:\n",
    "  hist(your_data, \n",
    "       breaks = 20,\n",
    "       prob = TRUE,\n",
    "       main = \"Distribution of ", variable_name, "\",\n",
    "       xlab = \"", variable_name, "\",\n",
    "       col = \"lightblue\")\n",
    "  curve(dnorm(x, mean = mean(your_data), sd = sd(your_data)),\n",
    "        add = TRUE, col = \"red\", lwd = 2)\n\n",
    "How to interpret:\n",
    "  • Bell-shaped curve = Likely normal ✓\n",
    "  • Long tail on right = Positive skew (common in income, prices)\n",
    "  • Long tail on left = Negative skew (common in test scores near max)\n",
    "  • Two peaks = Bimodal (may have two subgroups)\n",
    "  • Flat distribution = Uniform (not normal)\n\n",
    "--- COMBINED DIAGNOSTIC CODE ---\n\n",
    "Run both plots side-by-side:\n",
    "  par(mfrow = c(1, 2))  # Create 2-panel layout\n",
    "  \n",
    "  # Q-Q Plot\n",
    "  qqnorm(your_data, main = \"Q-Q Plot\")\n",
    "  qqline(your_data, col = \"red\")\n",
    "  \n",
    "  # Histogram\n",
    "  hist(your_data, prob = TRUE, main = \"Distribution\",\n",
    "       col = \"lightblue\", breaks = 20)\n",
    "  curve(dnorm(x, mean = mean(your_data), sd = sd(your_data)),\n",
    "        add = TRUE, col = \"red\", lwd = 2)\n",
    "  \n",
    "  par(mfrow = c(1, 1))  # Reset layout\n\n",
    "YOUR DATA CHARACTERISTICS:\n",
    "  Sample size:  ", n, if (n_missing > 0) paste0(" (", n_missing, " missing)") else "", "\n",
    "  Mean:         ", round(mean_val, 2), "\n",
    "  SD:           ", round(sd_val, 2), "\n",
    "  Skewness:     ", round(skew, 3), if (skew > 1) " (right-skewed)" else if (skew < -1) " (left-skewed)" else " (approximately symmetric)", "\n",
    "  Kurtosis:     ", round(kurt, 3), if (kurt > 1) " (heavy-tailed)" else if (kurt < -1) " (light-tailed)" else " (normal tails)", "\n\n",
    "PRACTICAL ADVICE:\n",
    "  • With n < 30: Visual inspection is more reliable than statistical tests\n",
    "  • With n > 100: Tests become very sensitive; focus on practical importance\n",
    "  • Mild violations + large sample = Usually OK to proceed with parametric tests\n",
    "  • Severe violations = Use non-parametric tests or transformations\n",
    "========================================================================\n"
  )

  # Return results
  structure(
    list(
      test = "Shapiro-Wilk",
      assumption = "Normality",
      assumption_met = assumption_met,
      p_value = p_val,
      p_value_formatted = p_value_formatted,  # APA 7 formatted (Level 1)
      statistic = w_stat,
      n = n,
      n_missing = n_missing,
      n_formatted = n_formatted,  # Level 1 formatting
      alpha = alpha,
      mean = mean_val,
      sd = sd_val,
      skewness = skew,
      kurtosis = kurt,
      interpretation = basic_interp,
      recommendation = recommendation,
      publication_text = pub_text,
      verbose_explanation = verbose,
      visual_guide = visual_guide  # Gold Standard addition
    ),
    class = "assumption_check"
  )
}


#' Check Homogeneity of Variance (Homoscedasticity) - Gold Standard
#'
#' @description
#' Tests whether groups have equal variances using Levene's test.
#' Required assumption for t-tests and ANOVA. Provides comprehensive
#' interpretation for beginners, including visual diagnostics, variance ratios,
#' and APA 7 formatted results.
#'
#' This function is designed to be self-explanatory for users new to statistics,
#' with extensive validation and beginner-friendly error messages.
#'
#' @param data Numeric vector of values to test
#' @param groups Factor or vector indicating group membership. Will be coerced to factor.
#' @param alpha Numeric. Significance level for the test (default 0.05).
#'   Must be between 0 and 1.
#' @param group_names Character. Optional name for the grouping variable (for reporting).
#'   Default is "groups". Used in all output messages.
#'
#' @return An S3 object of class "assumption_check" containing:
#'   \itemize{
#'     \item \code{test}: Name of the test ("Levene's Test")
#'     \item \code{assumption}: Which assumption was tested ("Homogeneity of Variance")
#'     \item \code{assumption_met}: Logical. TRUE if assumption is met (p > alpha)
#'     \item \code{p_value}: P-value from Levene's test
#'     \item \code{p_value_formatted}: APA 7 formatted p-value (from Level 1)
#'     \item \code{statistic}: F-statistic from Levene's test
#'     \item \code{df1}: Numerator degrees of freedom
#'     \item \code{df2}: Denominator degrees of freedom
#'     \item \code{n_total}: Total sample size (after removing NA)
#'     \item \code{n_missing}: Number of NA values removed
#'     \item \code{n_groups}: Number of groups
#'     \item \code{group_names}: Names of the groups
#'     \item \code{variance_ratio}: Ratio of largest to smallest group variance
#'     \item \code{group_statistics}: Detailed statistics for each group
#'     \item \code{alpha}: Significance level used
#'     \item \code{interpretation}: One-sentence summary for quick reference
#'     \item \code{recommendation}: Actionable next steps
#'     \item \code{publication_text}: APA 7 formatted text for manuscripts
#'     \item \code{verbose_explanation}: Educational explanation for beginners
#'     \item \code{visual_guide}: How to create diagnostic plots
#'   }
#'
#' @details
#' **What is homogeneity of variance?**
#'
#' This assumption means that all groups being compared should have similar
#' amounts of variability (spread). For example, if comparing customer satisfaction
#' across 3 segments, each segment should have roughly the same standard deviation.
#'
#' **How to interpret results:**
#' - If p > 0.05: Variances are equal (good news!)
#' - If p < 0.05: Variances differ (use Welch corrections)
#'
#' **What to do if violated:**
#' 1. Use Welch's t-test (for 2 groups) instead of Student's t-test
#' 2. Use Welch's ANOVA (for 3+ groups) instead of standard ANOVA
#' 3. Consider data transformation if variance ratio > 10
#'
#' @examples
#' # Example 1: Equal variances (assumption met)
#' set.seed(123)
#' data1 <- c(rnorm(50, mean = 10, sd = 2), rnorm(50, mean = 12, sd = 2))
#' groups1 <- rep(c("Group A", "Group B"), each = 50)
#' check_homogeneity_of_variance(data1, groups1)
#'
#' # Example 2: Unequal variances (assumption violated)
#' data2 <- c(rnorm(50, mean = 10, sd = 1), rnorm(50, mean = 12, sd = 5))
#' groups2 <- rep(c("Low Var", "High Var"), each = 50)
#' check_homogeneity_of_variance(data2, groups2)
#'
#' # Example 3: With missing values
#' data3 <- c(rnorm(45, 10, 2), rep(NA, 5), rnorm(48, 12, 2), rep(NA, 2))
#' groups3 <- rep(c("A", "B"), each = 50)
#' check_homogeneity_of_variance(data3, groups3)
#'
#' @export
check_homogeneity_of_variance <- function(data, groups, alpha = 0.05,
                                           group_names = "groups") {

  # ============================================================================
  # COMPREHENSIVE INPUT VALIDATION (Gold Standard)
  # ============================================================================

  # Check 1: Type validation for data
  if (!is.numeric(data)) {
    rlang::abort(c(
      "Data must be numeric",
      "x" = paste0("You provided: ", class(data)[1], " (", typeof(data), ")"),
      "i" = "Levene's test requires numeric data (numbers)",
      "i" = "Common mistake: Passing factor or character variables",
      ">" = "For factors: Convert with as.numeric(as.character(your_data))",
      ">" = "For data frames: Extract column with df$column_name",
      ">" = "Check data type: class(your_data)"
    ))
  }

  # Check 2: Type validation for groups (can be factor, character, or numeric)
  if (!is.factor(groups) && !is.character(groups) && !is.numeric(groups)) {
    rlang::abort(c(
      "Groups must be a vector (factor, character, or numeric)",
      "x" = paste0("You provided: ", class(groups)[1]),
      "i" = "Groups indicate which group each observation belongs to",
      ">" = "Example: groups = c('A', 'A', 'B', 'B', 'C', 'C')",
      ">" = "Or from data frame: df$treatment_group"
    ))
  }

  # Check 3: Validate group_names is character
  if (!is.character(group_names) || length(group_names) != 1) {
    rlang::abort(c(
      "group_names must be a single character string",
      "x" = paste0("You provided: ", class(group_names)[1]),
      "i" = "This is used for labeling output",
      ">" = "Example: group_names = \"treatment_condition\""
    ))
  }

  # Check 4: Validate alpha parameter
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    rlang::abort(c(
      "alpha must be a single number between 0 and 1",
      "x" = paste0("You provided: ", alpha),
      "i" = "alpha is the significance level for the test",
      "i" = "Common values: 0.05 (5%), 0.01 (1%), 0.10 (10%)",
      ">" = "Example: check_homogeneity_of_variance(data, groups, alpha = 0.05)"
    ))
  }

  # Check 5: Equal length vectors
  if (length(data) != length(groups)) {
    rlang::abort(c(
      "Data and groups must have the same length",
      "x" = paste0("data has ", length(data), " elements, groups has ", length(groups)),
      "i" = "Each data value needs a corresponding group label",
      ">" = "Check: length(your_data) should equal length(your_groups)",
      ">" = "Common mistake: Subsetting one but not the other"
    ))
  }

  # Check 6: Handle empty data
  if (length(data) == 0) {
    rlang::abort(c(
      "Cannot test homogeneity of variance on empty data",
      "x" = "You provided vectors with 0 elements",
      "i" = "Need at least 2 groups with 2+ observations each",
      ">" = "Check: Did you subset incorrectly?",
      ">" = "Try: length(your_data) to see how many values you have"
    ))
  }

  # Check 7: Detect NaN values in data
  if (any(is.nan(data))) {
    nan_count <- sum(is.nan(data))
    rlang::warn(c(
      "NaN (Not a Number) values detected in data",
      "!" = paste0(nan_count, " NaN value(s) found"),
      "i" = "NaN indicates a calculation error (e.g., 0/0, Inf/Inf)",
      "i" = "These will be treated as missing (NA) and removed",
      ">" = "Check: How was this data created?",
      ">" = "Run: summary(your_data) to inspect"
    ))
    data[is.nan(data)] <- NA
  }

  # Check 8: Detect Infinite values in data
  if (any(is.infinite(data))) {
    inf_count <- sum(is.infinite(data))
    rlang::warn(c(
      "Infinite values detected in data",
      "!" = paste0(inf_count, " infinite value(s) found"),
      "i" = "Infinite values prevent variance testing",
      "i" = "These will be treated as missing (NA) and removed",
      ">" = "Inspect: your_data[is.infinite(your_data)]"
    ))
    data[is.infinite(data)] <- NA
  }

  # Step 9: Remove missing values and count them
  valid_idx <- !is.na(data) & !is.na(groups)
  n_missing <- sum(!valid_idx)
  data_clean <- data[valid_idx]
  groups_clean <- as.factor(groups[valid_idx])

  n_total <- length(data_clean)
  n_groups <- nlevels(groups_clean)
  group_levels <- levels(groups_clean)

  # Check 10: Handle all-NA data
  if (n_total == 0) {
    rlang::abort(c(
      "No valid (non-missing) data points found",
      "x" = paste0("All ", length(data), " values are NA, NaN, or Inf"),
      "i" = "Cannot test homogeneity of variance without any valid numbers",
      ">" = "Check: summary(your_original_data)",
      ">" = "Check: table(is.na(your_data))"
    ))
  }

  # Check 11: Warn about missing data
  if (n_missing > 0) {
    pct_missing <- round(100 * n_missing / length(data), 1)

    if (pct_missing > 50) {
      rlang::warn(c(
        "More than half of data is missing",
        "!" = paste0(n_missing, " of ", length(data), " values are NA (",
                     pct_missing, "%)"),
        "i" = "Test based on only ", n_total, " observations",
        ">" = "Investigate: Why is so much data missing?",
        ">" = "Consider: Are results still meaningful?"
      ))
    } else if (pct_missing > 20) {
      rlang::warn(c(
        "Substantial missing data",
        "!" = paste0(n_missing, " of ", length(data), " values are NA (",
                     pct_missing, "%)"),
        "i" = "Test computed on ", n_total, " complete cases"
      ))
    } else {
      message("Note: ", n_missing, " missing value(s) removed from test (",
              pct_missing, "% of data)")
    }
  }

  # Check 12: At least 2 groups required
  if (n_groups < 2) {
    rlang::abort(c(
      "At least 2 groups required for homogeneity of variance test",
      "x" = paste0("Only ", n_groups, " group found: '", group_levels[1], "'"),
      "i" = "This test compares variances ACROSS groups",
      "i" = "You need at least 2 groups to compare",
      "!" = "Common causes:",
      "  " = "  • All participants in same condition",
      "  " = "  • Incorrect grouping variable selected",
      "  " = "  • Groups filtered out accidentally",
      ">" = "Check: table(your_groups) to see group counts",
      ">" = "Fix: Use correct grouping variable or collect data from multiple groups"
    ))
  }

  # Check 13: Each group needs at least 2 observations
  group_sizes <- table(groups_clean)
  small_groups <- group_sizes[group_sizes < 2]

  if (length(small_groups) > 0) {
    rlang::abort(c(
      "Each group must have at least 2 observations",
      "x" = paste0("Group(s) with < 2 observations: ",
                   paste(names(small_groups), " (n = ", small_groups, ")",
                         collapse = ", ")),
      "i" = "Cannot calculate variance with only 1 observation",
      "i" = "Levene's test requires within-group variance",
      "!" = "Options:",
      "  " = "  • Collect more data in small groups",
      "  " = "  • Combine small groups with similar ones (if justified)",
      "  " = "  • Exclude groups with insufficient data (document in methods)",
      ">" = paste0("Current group sizes: ",
                   paste(names(group_sizes), " (n = ", group_sizes, ")",
                         collapse = ", "))
    ))
  }

  # Check 14: Total sample size requirement
  if (n_total < 6) {
    rlang::warn(c(
      "Very small total sample size for homogeneity of variance test",
      "!" = paste0("Total n = ", n_total, " across ", n_groups, " groups"),
      "i" = "Levene's test is unreliable with very small samples",
      ">" = "Results should be interpreted with extreme caution",
      ">" = "Consider: Visual inspection of spreads via boxplots"
    ))
  }

  # ============================================================================
  # CHECK FOR ZERO VARIANCE WITHIN GROUPS
  # ============================================================================

  # Check 15: Detect zero variance within any group
  group_stats <- tapply(data_clean, groups_clean, function(x) {
    c(n = length(x), mean = mean(x), sd = sd(x), var = var(x))
  })

  variances <- sapply(group_stats, function(x) x["var"])
  zero_var_groups <- names(variances)[variances == 0 | is.na(variances)]

  if (length(zero_var_groups) > 0) {
    rlang::abort(c(
      "Zero variance detected in one or more groups",
      "x" = paste0("Group(s) with zero variance: ",
                   paste(zero_var_groups, collapse = ", ")),
      "i" = "All values in these groups are identical",
      "i" = "Cannot test homogeneity when a group has no variability",
      "!" = "This is unusual and may indicate:",
      "  " = "  • Data entry error (all values accidentally set to same number)",
      "  " = "  • Constant/fixed value by design",
      "  " = "  • Severe rounding (all values rounded to same number)",
      ">" = "Check: tapply(your_data, your_groups, function(x) unique(x))",
      ">" = "Statistical tests require variation within groups"
    ))
  }

  # ============================================================================
  # PERFORM LEVENE'S TEST
  # ============================================================================

  # Levene's test using absolute deviations from median (robust to non-normality)
  group_medians <- tapply(data_clean, groups_clean, median)
  abs_dev <- abs(data_clean - group_medians[groups_clean])

  levene_anova <- anova(lm(abs_dev ~ groups_clean))
  f_stat <- levene_anova$`F value`[1]
  p_val <- levene_anova$`Pr(>F)`[1]
  df1 <- levene_anova$Df[1]
  df2 <- levene_anova$Df[2]

  assumption_met <- p_val > alpha

  # Calculate variance ratio (for interpretation)
  var_ratio <- max(variances) / min(variances)

  # ============================================================================
  # INTEGRATE LEVEL 1 UTILITIES (Gold Standard)
  # ============================================================================

  # Format p-value using Level 1 function
  p_value_formatted <- format_p(p_val, digits = 3, style = "apa")

  # Format sample size using Level 1 function
  n_formatted <- format_n(n_total, type = "N", include_label = TRUE)

  # ============================================================================
  # CREATE INTERPRETATION AND RECOMMENDATIONS
  # ============================================================================

  # Interpretation
  if (assumption_met) {
    basic_interp <- paste0(
      "The homogeneity of variance assumption is MET ",
      "(Levene's test: F(", df1, ", ", df2, ") = ", round(f_stat, 3),
      ", ", p_value_formatted, ")."
    )

    # Create variance ratio interpretation
    var_ratio_interp <- if (var_ratio < 2) {
      paste0("excellent (ratio = ", round(var_ratio, 2), " < 2)")
    } else if (var_ratio < 3) {
      paste0("acceptable (ratio = ", round(var_ratio, 2), " < 3)")
    } else {
      paste0("borderline (ratio = ", round(var_ratio, 2), " ≥ 3, consider Welch correction)")
    }

    recommendation <- paste0(
      "The groups have similar variances. Standard t-test or ANOVA procedures are appropriate. ",
      "The variance ratio (largest/smallest) is ", var_ratio_interp, "."
    )

    pub_text <- paste0(
      "Homogeneity of variance was assessed using Levene's test. ",
      "The assumption was met (F(", df1, ", ", df2, ") = ", round(f_stat, 3),
      ", ", p_value_formatted, "), indicating equal variances across groups."
    )

    verbose <- paste0(
      "WHAT IS HOMOGENEITY OF VARIANCE?\n\n",
      "Homogeneity of variance (also called homoscedasticity) is a critical assumption for ",
      "t-tests and ANOVA. It means that the variability (spread) of scores should be roughly ",
      "the same across all groups being compared. Levene's test evaluates this assumption.\n\n",
      "HOW LEVENE'S TEST WORKS:\n\n",
      "The null hypothesis states that all groups have equal variances. ",
      "With a p-value of ", round(p_val, 4), " (greater than alpha = ", alpha, "), we fail to ",
      "reject the null hypothesis. This is good news! It means the variances across your groups ",
      "are statistically similar.\n\n",
      "WHAT THIS MEANS FOR YOUR ANALYSIS:\n\n",
      "The spread of scores is similar across all ", n_groups, " groups. ",
      "This means you can use standard statistical procedures (regular t-test or ANOVA) without ",
      "modification. If this assumption had been violated, you would need to use Welch's t-test ",
      "or Welch's ANOVA, which don't assume equal variances.\n\n",
      "VARIANCE RATIO INTERPRETATION:\n\n",
      "The variance ratio (largest group variance ÷ smallest) is ", round(var_ratio, 2), ". ",
      "Common guidelines:\n",
      "  • Ratio < 2.0: Excellent equality\n",
      "  • Ratio 2.0-3.0: Acceptable\n",
      "  • Ratio 3.0-10.0: Consider Welch correction\n",
      "  • Ratio > 10.0: Strongly recommend Welch correction\n\n",
      "Your ratio is ", var_ratio_interp, ".\n\n",
      "TECHNICAL DETAILS:\n\n",
      "F-statistic: ", round(f_stat, 3), " with df = (", df1, ", ", df2, ")\n",
      "Sample size: ", n_total, " across ", n_groups, " groups"
    )

  } else {
    basic_interp <- paste0(
      "The homogeneity of variance assumption is VIOLATED ",
      "(Levene's test: F(", df1, ", ", df2, ") = ", round(f_stat, 3),
      ", ", p_value_formatted, ")."
    )

    # Create severity interpretation based on variance ratio
    severity <- if (var_ratio < 3) {
      "mild violation"
    } else if (var_ratio < 10) {
      "moderate violation"
    } else {
      "severe violation"
    }

    recommendation <- paste0(
      "REQUIRED: Use Welch's t-test (for 2 groups) or Welch's ANOVA (for 3+ groups), ",
      "which do not assume equal variances. The variance ratio is ", round(var_ratio, 2),
      " (", severity, "), indicating meaningful differences in group variability.\n\n",
      "IN R:\n",
      if (n_groups == 2) {
        "  t.test(formula, data = your_data, var.equal = FALSE)  # Welch's t-test\n"
      } else {
        "  oneway.test(formula, data = your_data, var.equal = FALSE)  # Welch's ANOVA\n"
      }
    )

    pub_text <- paste0(
      "Homogeneity of variance was assessed using Levene's test. ",
      "The assumption was violated (F(", df1, ", ", df2, ") = ", round(f_stat, 3),
      ", ", p_value_formatted, "). Accordingly, Welch's correction was applied."
    )

    verbose <- paste0(
      "WHAT IS HOMOGENEITY OF VARIANCE?\n\n",
      "Homogeneity of variance (also called homoscedasticity) is a critical assumption for ",
      "t-tests and ANOVA. It means that the variability (spread) of scores should be roughly ",
      "the same across all groups being compared. Levene's test evaluates this assumption.\n\n",
      "WHAT THE VIOLATION MEANS:\n\n",
      "The null hypothesis of Levene's test states that all groups have equal variances. ",
      "With a p-value of ", round(p_val, 4), " (less than alpha = ", alpha, "), we reject the ",
      "null hypothesis. This means the variances across your groups are significantly different.\n\n",
      "This is a ", severity, " (variance ratio = ", round(var_ratio, 2), ").\n\n",
      "WHY THIS MATTERS:\n\n",
      "When variances are unequal, standard t-tests and ANOVA:\n",
      "  • Produce incorrect standard errors\n",
      "  • Give unreliable p-values (often too small)\n",
      "  • Can lead to false positive findings\n\n",
      "This is actually quite common in consumer research, especially when comparing different ",
      "market segments or experimental conditions. Don't worry - this doesn't invalidate your analysis!\n\n",
      "WHAT TO DO (REQUIRED STEPS):\n\n",
      "You MUST use modified versions of your planned tests:\n\n",
      if (n_groups == 2) {
        paste0(
          "For 2 groups: Use Welch's t-test\n",
          "  R code: t.test(outcome ~ group, data = your_data, var.equal = FALSE)\n\n",
          "  The var.equal = FALSE argument tells R to use Welch's correction.\n"
        )
      } else {
        paste0(
          "For 3+ groups: Use Welch's ANOVA (also called Brown-Forsythe test)\n",
          "  R code: oneway.test(outcome ~ group, data = your_data, var.equal = FALSE)\n\n",
          "  For post-hoc tests, use Games-Howell procedure (doesn't assume equal variances).\n"
        )
      },
      "\nThese 'Welch' versions do not assume equal variances and will give you valid results. ",
      "They're widely accepted in peer-reviewed publications and are often recommended as the ",
      "default approach even when variances are equal.\n\n",
      "VARIANCE RATIO INTERPRETATION:\n\n",
      "The variance ratio (largest group variance ÷ smallest) is ", round(var_ratio, 2), ".\n",
      "  • Ratio < 3.0: Mild concern, Welch still recommended\n",
      "  • Ratio 3.0-10.0: Moderate concern, MUST use Welch\n",
      "  • Ratio > 10.0: Severe concern, MUST use Welch + consider transformation\n\n",
      "TECHNICAL DETAILS:\n\n",
      "F-statistic: ", round(f_stat, 3), " with df = (", df1, ", ", df2, ")\n",
      "Sample size: ", n_total, " across ", n_groups, " groups\n",
      "Group variances: ", paste(names(variances), " = ", round(variances, 2), collapse = ", ")
    )
  }

  # ============================================================================
  # VISUAL DIAGNOSTIC GUIDE (Gold Standard)
  # ============================================================================

  # Create group variance table for display
  group_var_table <- paste(
    paste0("  ", group_levels, ": SD = ", round(sqrt(variances), 2),
           ", Var = ", round(variances, 2),
           ", n = ", sapply(group_stats, function(x) x["n"])),
    collapse = "\n"
  )

  visual_guide <- paste0(
    "========================================================================\n",
    "VISUAL DIAGNOSTICS: How to Check Variance Equality with Plots\n",
    "========================================================================\n\n",
    "Visual inspection often reveals variance differences more clearly than\n",
    "statistical tests. Here's how to create and interpret diagnostic plots:\n\n",
    "--- BOXPLOT (Best for Comparing Group Spreads) ---\n\n",
    "Create the plot:\n",
    "  boxplot(your_data ~ your_groups,\n",
    "          main = \"Distribution by ", group_names, "\",\n",
    "          xlab = \"", group_names, "\",\n",
    "          ylab = \"Values\",\n",
    "          col = c(\"lightblue\", \"lightgreen\", \"lightyellow\"))\n\n",
    "How to interpret:\n",
    "  • BOX HEIGHT = Interquartile Range (IQR), reflects spread\n",
    "  • Similar box heights = Similar variances ✓\n",
    "  • Very different box heights = Unequal variances\n",
    "  • Whisker length also indicates spread\n\n",
    "Rule of thumb:\n",
    "  - If one box is 2-3x taller than another → Possible violation\n",
    "  - If one box is 5x+ taller than another → Clear violation\n\n",
    "--- VARIANCE PLOT (Side-by-Side Standard Deviations) ---\n\n",
    "Create the plot:\n",
    "  # Calculate group SDs\n",
    "  group_sds <- tapply(your_data, your_groups, sd)\n",
    "  \n",
    "  # Plot\n",
    "  barplot(group_sds,\n",
    "          main = \"Standard Deviation by Group\",\n",
    "          xlab = \"", group_names, "\",\n",
    "          ylab = \"Standard Deviation\",\n",
    "          col = \"steelblue\")\n",
    "  abline(h = mean(group_sds), col = \"red\", lty = 2)\n\n",
    "How to interpret:\n",
    "  • Bars of similar height = Equal variances ✓\n",
    "  • One bar much taller/shorter = Unequal variances\n",
    "  • Red line shows average SD across groups\n\n",
    "--- SPREAD-LOCATION PLOT (Advanced) ---\n\n",
    "Create the plot:\n",
    "  # Fit a model\n",
    "  model <- lm(your_data ~ your_groups)\n",
    "  \n",
    "  # Plot residuals vs fitted\n",
    "  plot(model, which = 3)  # Scale-Location plot\n\n",
    "How to interpret:\n",
    "  • Horizontal red line = Equal variances ✓\n",
    "  • Upward/downward trend = Variances increase/decrease with mean\n",
    "  • Funnel shape = Heteroscedasticity\n\n",
    "--- YOUR GROUP STATISTICS ---\n\n",
    group_var_table, "\n\n",
    "Variance Ratio: ", round(var_ratio, 2), " (", names(variances)[which.max(variances)],
    " / ", names(variances)[which.min(variances)], ")\n\n",
    "--- PRACTICAL ADVICE ---\n\n",
    "• With equal sample sizes: Violations less problematic (tests are 'robust')\n",
    "• With unequal sample sizes: Violations more serious, definitely use Welch\n",
    "• If largest variance goes with smallest n: Most problematic scenario\n",
    "• If largest variance goes with largest n: Less problematic but still use Welch\n",
    "========================================================================\n"
  )

  # Return results
  structure(
    list(
      test = "Levene's Test",
      assumption = "Homogeneity of Variance",
      assumption_met = assumption_met,
      p_value = p_val,
      p_value_formatted = p_value_formatted,  # APA 7 formatted (Level 1)
      statistic = f_stat,
      df1 = df1,
      df2 = df2,
      n_total = n_total,
      n_missing = n_missing,
      n_formatted = n_formatted,  # Level 1 formatting
      n_groups = n_groups,
      group_names = group_levels,
      variance_ratio = var_ratio,
      group_variances = variances,
      group_statistics = group_stats,
      alpha = alpha,
      interpretation = basic_interp,
      recommendation = recommendation,
      publication_text = pub_text,
      verbose_explanation = verbose,
      visual_guide = visual_guide  # Gold Standard addition
    ),
    class = "assumption_check"
  )
}


#' Check Independence of Observations - Gold Standard
#'
#' @description
#' Provides comprehensive guidance and testing for the independence assumption.
#' While true independence is primarily a design issue, this function helps you:
#' (1) Think through your study design, (2) Detect potential violations,
#' (3) Test for autocorrelation in time series or sequential data, and
#' (4) Get actionable recommendations for violations.
#'
#' Independence violations are among the most serious assumption violations because
#' they directly inflate Type I error (false positives). This function provides
#' extensive educational support for beginners.
#'
#' @param data Numeric vector of residuals or raw data (optional). If provided,
#'   will test for autocorrelation. If NULL, provides design-based guidance only.
#' @param data_structure Character. Description of your data collection design.
#'   Options: "cross-sectional survey" (default), "repeated measures",
#'   "time series", "clustered", "matched pairs", "panel data", "other".
#' @param is_independent Logical. Do you believe observations are independent
#'   based on your study design? Default TRUE.
#' @param id_var Optional vector of participant/cluster IDs for detecting
#'   repeated observations.
#' @param time_var Optional vector indicating time order for autocorrelation testing.
#' @param clustering_note Optional character string with additional context
#'   about data structure (e.g., "students nested within 12 classrooms").
#'
#' @return An S3 object of class "assumption_check" containing:
#'   \itemize{
#'     \item \code{test}: Type of assessment performed
#'     \item \code{assumption}: "Independence of Observations"
#'     \item \code{assumption_met}: Logical. TRUE if independent (based on design + tests)
#'     \item \code{data_structure}: Your specified data structure
#'     \item \code{has_autocorrelation}: Logical or NA (if tested)
#'     \item \code{autocorrelation}: Lag-1 autocorrelation coefficient (if tested)
#'     \item \code{durbin_watson}: Durbin-Watson statistic (if tested)
#'     \item \code{n_unique_ids}: Number of unique IDs (if id_var provided)
#'     \item \code{n_obs}: Total observations
#'     \item \code{avg_obs_per_id}: Average observations per ID (if applicable)
#'     \item \code{interpretation}: One-sentence summary
#'     \item \code{recommendation}: Actionable next steps
#'     \item \code{publication_text}: APA 7 formatted text
#'     \item \code{verbose_explanation}: Educational explanation
#'     \item \code{design_checklist}: Questions to assess independence
#'     \item \code{visual_guide}: How to diagnose independence violations
#'   }
#'
#' @details
#' **What is the independence assumption?**
#'
#' Independence means each observation is unrelated to other observations.
#' One person's response doesn't influence another's response (beyond random chance).
#'
#' **Common violations:**
#' - Repeated measures (same person multiple times)
#' - Time series (today depends on yesterday)
#' - Clusters (students in same class, customers in same store)
#' - Matched pairs or siblings
#' - Social networks (friends influence friends)
#'
#' **How to check:**
#' 1. Think about study design (primary method)
#' 2. Provide data to test for autocorrelation
#' 3. Provide ID variable to detect repeated measures
#'
#' @examples
#' # Example 1: Design-based check (independent)
#' check_independence(data_structure = "cross-sectional survey",
#'                    is_independent = TRUE)
#'
#' # Example 2: Design-based check (violation)
#' check_independence(data_structure = "repeated measures",
#'                    is_independent = FALSE,
#'                    clustering_note = "3 measurements per participant")
#'
#' # Example 3: Test for autocorrelation
#' set.seed(123)
#' residuals <- arima.sim(model = list(ar = 0.7), n = 100)
#' check_independence(data = residuals,
#'                    data_structure = "time series")
#'
#' # Example 4: Detect repeated measures
#' set.seed(123)
#' data_vals <- rnorm(150)
#' ids <- rep(1:50, each = 3)  # 3 obs per person
#' check_independence(data = data_vals,
#'                    id_var = ids,
#'                    data_structure = "repeated measures")
#'
#' @export
check_independence <- function(data = NULL,
                                data_structure = "cross-sectional survey",
                                is_independent = TRUE,
                                id_var = NULL,
                                time_var = NULL,
                                clustering_note = NULL) {

  # ============================================================================
  # COMPREHENSIVE INPUT VALIDATION (Gold Standard)
  # ============================================================================

  # Check 1: Validate data_structure options
  valid_structures <- c("cross-sectional survey", "repeated measures",
                        "time series", "clustered", "matched pairs",
                        "panel data", "other")

  if (!is.character(data_structure) || length(data_structure) != 1) {
    rlang::abort(c(
      "data_structure must be a single character string",
      "x" = paste0("You provided: ", class(data_structure)[1]),
      "i" = "This describes your study design",
      ">" = paste0("Valid options: ", paste(valid_structures, collapse = ", "))
    ))
  }

  if (!data_structure %in% valid_structures) {
    rlang::warn(c(
      "Unrecognized data_structure",
      "!" = paste0("You provided: '", data_structure, "'"),
      "i" = paste0("Common options: ", paste(valid_structures[1:3], collapse = ", ")),
      ">" = "Proceeding with custom structure, but results may be less specific"
    ))
  }

  # Check 2: Validate is_independent
  if (!is.logical(is_independent) || length(is_independent) != 1) {
    rlang::abort(c(
      "is_independent must be TRUE or FALSE",
      "x" = paste0("You provided: ", class(is_independent)[1]),
      "i" = "This indicates whether you believe observations are independent",
      ">" = "Example: is_independent = TRUE (for independent obs)",
      ">" = "Example: is_independent = FALSE (for repeated measures)"
    ))
  }

  # Check 3: Validate data if provided
  if (!is.null(data)) {
    if (!is.numeric(data)) {
      rlang::abort(c(
        "Data must be numeric (if provided)",
        "x" = paste0("You provided: ", class(data)[1]),
        "i" = "Use residuals from your model or raw data values",
        ">" = "Example: check_independence(data = residuals(your_model))"
      ))
    }

    if (length(data) == 0) {
      rlang::abort(c(
        "Cannot test independence on empty data",
        "x" = "You provided a vector with 0 elements",
        ">" = "Check: length(your_data)"
      ))
    }

    # Handle NaN
    if (any(is.nan(data))) {
      nan_count <- sum(is.nan(data))
      rlang::warn(c(
        "NaN values detected in data",
        "!" = paste0(nan_count, " NaN value(s) found"),
        "i" = "These will be treated as NA and removed"
      ))
      data[is.nan(data)] <- NA
    }

    # Handle Inf
    if (any(is.infinite(data))) {
      inf_count <- sum(is.infinite(data))
      rlang::warn(c(
        "Infinite values detected in data",
        "!" = paste0(inf_count, " infinite value(s) found"),
        "i" = "These will be treated as NA and removed"
      ))
      data[is.infinite(data)] <- NA
    }

    # Remove NAs
    n_missing <- sum(is.na(data))
    if (n_missing > 0) {
      message("Note: ", n_missing, " missing value(s) removed from data")
      data <- data[!is.na(data)]
    }

    if (length(data) == 0) {
      rlang::abort(c(
        "No valid data after removing missing values",
        ">" = "Cannot test for autocorrelation"
      ))
    }

    if (length(data) < 3) {
      rlang::abort(c(
        "Too few observations for autocorrelation test",
        "x" = paste0("n = ", length(data), " after removing missing"),
        "i" = "Need at least 3 observations",
        ">" = "Provide more data or use design-based assessment only"
      ))
    }
  }

  # Check 4: Validate id_var if provided
  if (!is.null(id_var)) {
    if (is.null(data)) {
      rlang::warn(c(
        "id_var provided but data is NULL",
        "i" = "Cannot detect repeated measures without data",
        ">" = "Ignoring id_var for this assessment"
      ))
      id_var <- NULL
    } else if (length(id_var) != length(data)) {
      rlang::abort(c(
        "id_var must have same length as data",
        "x" = paste0("data has ", length(data), " elements, id_var has ", length(id_var)),
        ">" = "Each observation needs an ID"
      ))
    }
  }

  # Check 5: Validate time_var if provided
  if (!is.null(time_var)) {
    if (is.null(data)) {
      rlang::warn(c(
        "time_var provided but data is NULL",
        "i" = "Cannot test autocorrelation without data",
        ">" = "Ignoring time_var for this assessment"
      ))
      time_var <- NULL
    } else if (length(time_var) != length(data)) {
      rlang::abort(c(
        "time_var must have same length as data",
        "x" = paste0("data has ", length(data), " elements, time_var has ", length(time_var)),
        ">" = "Each observation needs a time indicator"
      ))
    } else {
      # Sort data by time
      time_order <- order(time_var)
      data <- data[time_order]
      if (!is.null(id_var)) {
        id_var <- id_var[time_order]
      }
      message("Note: Data sorted by time_var for autocorrelation testing")
    }
  }

  # ============================================================================
  # STATISTICAL TESTING (if data provided)
  # ============================================================================

  has_autocorrelation <- NA
  autocorr <- NA
  dw_stat <- NA
  n_obs <- ifelse(is.null(data), NA, length(data))
  n_unique_ids <- NA
  avg_obs_per_id <- NA

  # Test for repeated measures
  if (!is.null(id_var)) {
    n_unique_ids <- length(unique(id_var))
    avg_obs_per_id <- round(length(id_var) / n_unique_ids, 2)

    if (n_unique_ids < length(id_var)) {
      # Detected repeated measures
      is_independent <- FALSE
      data_structure <- "repeated measures (detected)"
      if (is.null(clustering_note)) {
        clustering_note <- paste0(
          "Detected ", length(id_var), " observations from ", n_unique_ids,
          " unique IDs (average ", avg_obs_per_id, " obs per ID)"
        )
      }
    }
  }

  # Test for autocorrelation
  if (!is.null(data) && length(data) >= 3) {
    # Calculate lag-1 autocorrelation
    autocorr <- cor(data[-length(data)], data[-1], use = "complete.obs")

    # Calculate Durbin-Watson statistic
    residuals_diff <- diff(data)
    dw_stat <- sum(residuals_diff^2) / sum(data^2)

    # Simple autocorrelation test (|r| > 0.3 suggests dependence)
    if (abs(autocorr) > 0.3) {
      has_autocorrelation <- TRUE
      if (is_independent) {
        rlang::warn(c(
          "Autocorrelation detected in data",
          "!" = paste0("Lag-1 correlation = ", round(autocorr, 3)),
          "i" = "This suggests observations are NOT independent",
          ">" = "Consider: Is there a time or sequence component?"
        ))
      }
    } else {
      has_autocorrelation <- FALSE
    }
  }

  # ============================================================================
  # CREATE INTERPRETATION AND RECOMMENDATIONS
  # ============================================================================

  if (is_independent && (is.na(has_autocorrelation) || !has_autocorrelation)) {
    interpretation <- paste0(
      "The independence assumption is MET based on study design (",
      data_structure, ")",
      if (!is.na(has_autocorrelation)) {
        paste0(" and autocorrelation testing (r = ", round(autocorr, 3), ")")
      } else {
        ""
      },
      "."
    )

    recommendation <- paste0(
      "Each observation represents a unique, independent unit. ",
      "Standard statistical procedures (t-tests, ANOVA, linear regression) are appropriate. ",
      "No special corrections needed."
    )

    pub_text <- paste0(
      "The data were collected using a ", data_structure, " design, ",
      "ensuring independence of observations",
      if (!is.na(has_autocorrelation)) {
        paste0(". Independence was confirmed via autocorrelation testing (r = ",
               round(autocorr, 3), ")")
      } else {
        ""
      },
      "."
    )

    verbose <- paste0(
      "WHAT IS INDEPENDENCE OF OBSERVATIONS?\n\n",
      "Independence of observations is one of the most important assumptions in statistics. ",
      "It means each data point represents a unique individual or unit, and one participant's ",
      "response doesn't influence another's response (beyond random chance).\n\n",
      "Think of it this way: If you know Person A's score, does that tell you anything about ",
      "Person B's score beyond what you'd expect by chance? If not, they're independent.\n\n",
      "YOUR STUDY DESIGN:\n\n",
      "Design type: ", data_structure, "\n",
      if (!is.null(clustering_note)) paste0("Context: ", clustering_note, "\n"),
      if (!is.na(n_obs)) paste0("Observations: ", n_obs, "\n"),
      if (!is.na(n_unique_ids)) {
        paste0("Unique IDs: ", n_unique_ids, " (avg ", avg_obs_per_id, " obs per ID)\n")
      },
      if (!is.na(autocorr)) {
        paste0("Autocorrelation: ", round(autocorr, 3), " (< 0.3 threshold)\n")
      },
      "\nBased on your design", if (!is.na(has_autocorrelation)) " and testing", "",
      ", the observations are independent. This is excellent! You can proceed with standard ",
      "statistical analyses without special corrections.\n\n",
      "WHY THIS MATTERS:\n\n",
      "When independence is met:\n",
      "  • Standard errors are correctly estimated\n",
      "  • P-values are accurate\n",
      "  • Confidence intervals have proper coverage\n",
      "  • You can use all standard statistical tests\n\n",
      "If independence were violated (it's not in your case):\n",
      "  • Standard errors would be TOO SMALL\n",
      "  • P-values would be TOO SMALL (inflated Type I error)\n",
      "  • You'd get false positives\n\n",
      "COMMON VIOLATIONS TO WATCH FOR (in future studies):\n\n",
      "  • Repeated measures: Same person measured multiple times\n",
      "  • Time series: Today's value depends on yesterday's\n",
      "  • Clustered data: Students within classrooms, customers within stores\n",
      "  • Matched pairs: Siblings, couples, or intentionally paired units\n",
      "  • Social networks: Friends influencing friends\n\n",
      "If you had violations, you'd need:\n",
      "  • Repeated measures ANOVA (for repeated data)\n",
      "  • Mixed effects models (for clustered/nested data)\n",
      "  • Time series methods (for sequential data)\n",
      "  • Paired tests (for matched data)"
    )

  } else {
    # Independence violated
    interpretation <- paste0(
      "The independence assumption is VIOLATED based on ",
      if (!is.na(has_autocorrelation) && has_autocorrelation) {
        paste0("autocorrelation testing (r = ", round(autocorr, 3), ") and ")
      } else {
        ""
      },
      "study design (", data_structure, ")."
    )

    # Determine appropriate method based on data structure
    recommended_method <- switch(
      data_structure,
      "repeated measures" = "repeated measures ANOVA or mixed effects model",
      "repeated measures (detected)" = "repeated measures ANOVA or mixed effects model",
      "time series" = "time series analysis (ARIMA, autocorrelation modeling)",
      "clustered" = "mixed effects model with random intercepts for clusters",
      "matched pairs" = "paired t-test or repeated measures ANOVA",
      "panel data" = "panel regression with fixed or random effects",
      "mixed effects model or specialized method for your data structure"
    )

    recommendation <- paste0(
      "REQUIRED: Use ", recommended_method, ".\n\n",
      "DO NOT use standard t-tests, ANOVA, or regression - they assume independence and will ",
      "give you incorrect results (inflated Type I error, false positives).\n\n",
      if (!is.na(n_unique_ids)) {
        paste0(
          "YOUR DATA: ", length(data), " observations from ", n_unique_ids, " unique units ",
          "(avg ", avg_obs_per_id, " obs per unit).\n\n"
        )
      },
      "IN R:\n",
      if (data_structure %in% c("repeated measures", "repeated measures (detected)")) {
        paste0(
          "  # Option 1: Repeated measures ANOVA (ezANOVA package)\n",
          "  library(ez)\n",
          "  ezANOVA(data = your_data, dv = outcome, wid = id, within = time)\n\n",
          "  # Option 2: Mixed effects model (lme4 package)\n",
          "  library(lme4)\n",
          "  lmer(outcome ~ predictor + (1 | id), data = your_data)\n"
        )
      } else if (data_structure == "clustered") {
        paste0(
          "  # Mixed effects model with random intercepts\n",
          "  library(lme4)\n",
          "  lmer(outcome ~ predictor + (1 | cluster_id), data = your_data)\n"
        )
      } else if (data_structure == "matched pairs") {
        paste0(
          "  # Paired t-test\n",
          "  t.test(group1, group2, paired = TRUE)\n"
        )
      } else if (data_structure == "time series") {
        paste0(
          "  # ARIMA model\n",
          "  library(forecast)\n",
          "  auto.arima(your_time_series)\n"
        )
      } else {
        paste0(
          "  # Mixed effects model (general case)\n",
          "  library(lme4)\n",
          "  lmer(outcome ~ predictors + (1 | grouping_var), data = your_data)\n"
        )
      }
    )

    pub_text <- paste0(
      "The data structure (", data_structure, ") involved non-independent observations",
      if (!is.na(autocorr)) paste0(" (autocorrelation r = ", round(autocorr, 3), ")"),
      ". Accordingly, ", recommended_method, " was employed to account for dependencies."
    )

    verbose <- paste0(
      "CRITICAL: INDEPENDENCE ASSUMPTION VIOLATED\n\n",
      "Independence of observations is one of the MOST important assumptions in statistics. ",
      "Your study design (", data_structure, ") creates dependencies in the data.\n\n",
      "WHAT THIS MEANS:\n\n",
      "Some observations are related to each other beyond random chance. This could be:\n",
      "  • Same people measured multiple times (repeated measures)\n",
      "  • Observations nested within groups (students in classrooms)\n",
      "  • Sequential/time-ordered data (autocorrelation)\n",
      "  • Matched or paired observations\n\n",
      if (!is.null(clustering_note)) paste0("Your context: ", clustering_note, "\n\n"),
      if (!is.na(n_unique_ids)) {
        paste0(
          "DETECTED STRUCTURE:\n",
          "  Total observations: ", length(data), "\n",
          "  Unique units: ", n_unique_ids, "\n",
          "  Avg observations per unit: ", avg_obs_per_id, "\n",
          "  → This indicates REPEATED MEASURES or CLUSTERING\n\n"
        )
      },
      if (!is.na(autocorr)) {
        paste0(
          "AUTOCORRELATION DETECTED:\n",
          "  Lag-1 correlation: ", round(autocorr, 3), "\n",
          "  Durbin-Watson: ", round(dw_stat, 3), "\n",
          "  → This indicates TIME-SERIES DEPENDENCE\n\n"
        )
      },
      "WHY THIS IS CRITICAL:\n\n",
      "When you ignore dependence, standard tests (t-test, ANOVA, regression):\n",
      "  ✖ Underestimate standard errors (make them TOO SMALL)\n",
      "  ✖ Produce p-values that are TOO SMALL\n",
      "  ✖ Inflate Type I error rate (false positives)\n",
      "  ✖ Give you 'significant' results that are actually wrong\n\n",
      "Example: If your true Type I error is 20% instead of 5%, you'll get false positives ",
      "4 times more often than you think!\n\n",
      "WHAT YOU MUST DO:\n\n",
      "Use specialized methods that account for dependence: ", recommended_method, ".\n\n",
      "These methods:\n",
      "  ✓ Correctly estimate standard errors\n",
      "  ✓ Give accurate p-values\n",
      "  ✓ Account for correlation structure\n",
      "  ✓ Are widely accepted in publications\n\n",
      "DO NOT:\n",
      "  ✖ Use standard t-tests\n",
      "  ✖ Use standard ANOVA\n",
      "  ✖ Use standard linear regression\n",
      "  ✖ Ignore the dependence\n\n",
      "These will give you WRONG results."
    )
  }

  # ============================================================================
  # DESIGN CHECKLIST (Gold Standard)
  # ============================================================================

  design_checklist <- paste0(
    "========================================================================\n",
    "INDEPENDENCE CHECKLIST: Questions to Ask About Your Study\n",
    "========================================================================\n\n",
    "Work through these questions to assess independence:\n\n",
    "1. DATA COLLECTION DESIGN:\n",
    "   □ Did each participant contribute only ONE observation?\n",
    "   □ Were participants randomly sampled from the population?\n",
    "   □ Could one participant's response influence another's?\n\n",
    "2. REPEATED MEASURES:\n",
    "   □ Is anyone measured more than once?\n",
    "   □ Are there pre-test and post-test measurements?\n",
    "   □ Do you have multiple items per person?\n",
    "   → If YES to any: Use repeated measures methods\n\n",
    "3. CLUSTERING/NESTING:\n",
    "   □ Are participants grouped (classrooms, stores, teams)?\n",
    "   □ Do participants within groups share experiences?\n",
    "   □ Were participants recruited as groups?\n",
    "   → If YES to any: Use mixed effects models\n\n",
    "4. TIME/SEQUENCE:\n",
    "   □ Are observations ordered in time?\n",
    "   □ Could today's value depend on yesterday's?\n",
    "   □ Is there a trend over time?\n",
    "   → If YES to any: Use time series methods or include time effects\n\n",
    "5. MATCHING/PAIRING:\n",
    "   □ Are observations intentionally paired (couples, siblings)?\n",
    "   □ Did you match participants on characteristics?\n",
    "   □ Are there natural pairs in your data?\n",
    "   → If YES to any: Use paired tests\n\n",
    "6. SOCIAL NETWORK:\n",
    "   □ Do participants know each other?\n",
    "   □ Could social influence affect responses?\n",
    "   □ Did you sample from interconnected groups?\n",
    "   → If YES to any: Use network analysis methods\n\n",
    "DECISION RULE:\n\n",
    "If you answered YES to ANY question above, your data likely violates independence.\n",
    "You MUST use specialized methods that account for the dependence structure.\n\n",
    "When in doubt: Consult a statistician or use mixed effects models (they're robust).\n",
    "========================================================================\n"
  )

  # ============================================================================
  # VISUAL DIAGNOSTIC GUIDE (Gold Standard)
  # ============================================================================

  visual_guide <- paste0(
    "========================================================================\n",
    "VISUAL DIAGNOSTICS: How to Detect Independence Violations\n",
    "========================================================================\n\n",
    "Independence violations can sometimes be detected visually:\n\n",
    "--- AUTOCORRELATION PLOT (For Time Series Data) ---\n\n",
    "Create the plot:\n",
    "  # Using residuals from a model\n",
    "  model <- lm(outcome ~ predictors, data = your_data)\n",
    "  res <- residuals(model)\n",
    "  \n",
    "  # Autocorrelation function\n",
    "  acf(res, main = \"Autocorrelation of Residuals\")\n\n",
    "How to interpret:\n",
    "  • Spikes within blue dashed lines = No autocorrelation ✓\n",
    "  • Spikes outside blue lines = Autocorrelation detected\n",
    "  • Lag 1 spike = Adjacent observations correlated\n",
    "  • Pattern of spikes = Systematic dependence\n\n",
    "--- RESIDUALS VS ORDER PLOT ---\n\n",
    "Create the plot:\n",
    "  model <- lm(outcome ~ predictors, data = your_data)\n",
    "  plot(residuals(model), type = \"l\",\n",
    "       main = \"Residuals Over Time\",\n",
    "       ylab = \"Residuals\")\n",
    "  abline(h = 0, col = \"red\")\n\n",
    "How to interpret:\n",
    "  • Random scatter around zero = Independent ✓\n",
    "  • Patterns or trends = Dependence\n",
    "  • Clustering of similar values = Autocorrelation\n",
    "  • Runs above/below zero = Non-independence\n\n",
    "--- CLUSTER/GROUP COMPARISON ---\n\n",
    "Create the plot:\n",
    "  boxplot(outcome ~ cluster_id, data = your_data,\n",
    "          main = \"Outcome by Cluster\")\n\n",
    "How to interpret:\n",
    "  • Different median levels across clusters = Clustering effect\n",
    "  • Wide variation between clusters = Non-independence\n",
    "  → Need mixed effects model with random intercepts\n\n",
    "--- DURBIN-WATSON TEST (Formal Test) ---\n\n",
    if (!is.na(dw_stat)) {
      paste0(
        "YOUR DURBIN-WATSON STATISTIC: ", round(dw_stat, 3), "\n\n",
        "Interpretation:\n",
        "  • DW ≈ 2.0: No autocorrelation ✓\n",
        "  • DW < 1.5: Positive autocorrelation (successive values similar)\n",
        "  • DW > 2.5: Negative autocorrelation (successive values alternate)\n\n",
        "Your value suggests ",
        if (dw_stat < 1.5) {
          "POSITIVE AUTOCORRELATION - observations are dependent"
        } else if (dw_stat > 2.5) {
          "NEGATIVE AUTOCORRELATION - unusual pattern"
        } else {
          "NO STRONG AUTOCORRELATION - independence is plausible"
        },
        "\n\n"
      )
    } else {
      "Run in R:\n  library(lmtest)\n  dwtest(your_model)\n\n"
    },
    "PRACTICAL ADVICE:\n\n",
    "  • If unsure: Assume dependence and use robust methods\n",
    "  • Mixed effects models are rarely wrong (even if independence holds)\n",
    "  • Standard methods with dependence are often wrong\n",
    "  • Better to be conservative than to get false positives\n",
    "========================================================================\n"
  )

  # Return results
  structure(
    list(
      test = "Independence Assessment",
      assumption = "Independence of Observations",
      assumption_met = is_independent && (is.na(has_autocorrelation) || !has_autocorrelation),
      data_structure = data_structure,
      has_autocorrelation = has_autocorrelation,
      autocorrelation = autocorr,
      durbin_watson = dw_stat,
      n_obs = n_obs,
      n_unique_ids = n_unique_ids,
      avg_obs_per_id = avg_obs_per_id,
      clustering_note = clustering_note,
      interpretation = interpretation,
      recommendation = recommendation,
      publication_text = pub_text,
      verbose_explanation = verbose,
      design_checklist = design_checklist,  # Gold Standard addition
      visual_guide = visual_guide  # Gold Standard addition
    ),
    class = "assumption_check"
  )
}


#' Print Method for Assumption Checks
#'
#' @param x An assumption_check object
#' @param verbose Logical - show detailed explanation? (default TRUE)
#' @param ... Additional arguments (not used)
#'
#' @export
print.assumption_check <- function(x, verbose = TRUE, ...) {
  cat("\n")
  cat("=" %+% rep("=", 78) %+% "=\n", sep = "")
  cat("STATISTICAL ASSUMPTION CHECK: ", x$assumption, "\n", sep = "")
  cat("=" %+% rep("=", 78) %+% "=\n", sep = "")
  cat("\n")

  cat("Test Used:", x$test, "\n")

  if (!is.na(x$assumption_met)) {
    cat("Result:", ifelse(x$assumption_met, "PASS ASSUMPTION MET", "FAIL ASSUMPTION VIOLATED"), "\n")
  }

  if (!is.null(x$p_value) && !is.na(x$p_value)) {
    cat("p-value:", round(x$p_value, 4), "\n")
  }

  if (!is.null(x$n)) {
    cat("Sample size:", x$n, "\n")
  }

  cat("\n")
  cat("INTERPRETATION:\n")
  cat(strwrap(x$interpretation, width = 80, prefix = "  "), sep = "\n")
  cat("\n")

  cat("\nRECOMMENDATION:\n")
  cat(strwrap(x$recommendation, width = 80, prefix = "  "), sep = "\n")
  cat("\n")

  cat("\nPUBLICATION-READY TEXT:\n")
  cat(strwrap(x$publication_text, width = 80, prefix = "  "), sep = "\n")
  cat("\n")

  if (verbose && !is.null(x$verbose_explanation)) {
    cat("\n")
    cat("-" %+% rep("-", 78) %+% "-\n", sep = "")
    cat("DETAILED EXPLANATION (for learning and context):\n")
    cat("-" %+% rep("-", 78) %+% "-\n", sep = "")
    cat("\n")
    cat(strwrap(x$verbose_explanation, width = 80, prefix = "  "), sep = "\n")
    cat("\n")
  }

  # Show design checklist if available (for independence checks)
  if (verbose && !is.null(x$design_checklist)) {
    cat("\n")
    cat(x$design_checklist)
  }

  # Show visual guide if available
  if (verbose && !is.null(x$visual_guide)) {
    cat("\n")
    cat(x$visual_guide)
  }

  cat("\n")

  invisible(x)
}
