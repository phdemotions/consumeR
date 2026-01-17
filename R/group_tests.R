#' Test for Differences Between Consumer Groups
#'
#' @description
#' This function compares two groups of consumers to determine if they
#' are statistically different. It is designed for maximum transparency
#' during peer review, with clear documentation of every step and assumption.
#'
#' @param group1 A numeric vector containing data for the first group.
#'   For example, spending amounts for treatment group consumers.
#' @param group2 A numeric vector containing data for the second group.
#'   For example, spending amounts for control group consumers.
#' @param test_type Character string specifying which test to use.
#'   Options are:
#'   \itemize{
#'     \item "t.test" - Student's t-test (assumes normal distribution)
#'     \item "wilcoxon" - Wilcoxon rank-sum test (non-parametric)
#'     \item "auto" - Automatically chooses based on sample size (default)
#'   }
#' @param alternative Character string specifying the alternative hypothesis.
#'   Options are "two.sided" (default), "greater", or "less".
#' @param conf_level Numeric. Confidence level for the interval. Default is 0.95, i.e., 95 percent confidence.
#' @param paired Logical. Are the observations paired? Default is FALSE.
#' @param check_assumptions Logical. Should assumptions be tested? Default is TRUE. When TRUE, tests for normality and homogeneity of variance are performed and results are included in output.
#' @param verbose Logical. Should detailed explanations be provided? Default is TRUE. When TRUE, includes verbose explanations suitable for learning.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{test_used}: Name of the statistical test performed
#'     \item \code{p_value}: P-value from the test
#'     \item \code{significant}: Logical, TRUE if p < 0.05
#'     \item \code{group1_mean}: Mean of group 1
#'     \item \code{group2_mean}: Mean of group 2
#'     \item \code{group1_sd}: Standard deviation of group 1
#'     \item \code{group2_sd}: Standard deviation of group 2
#'     \item \code{difference}: Difference between group means
#'     \item \code{effect_size}: Cohen's d effect size
#'     \item \code{interpretation}: Plain English interpretation of results
#'     \item \code{assumptions}: List of assumption check results (if check_assumptions = TRUE)
#'     \item \code{publication_text}: Publication-ready text block
#'     \item \code{full_test_output}: Complete test results object
#'   }
#'
#' @details
#' The function performs either a t-test (for normally distributed data)
#' or a Wilcoxon test (for non-normal data). All steps are documented
#' and assumptions are clearly stated.
#'
#' ## How the Test Selection Works:
#' When test_type = "auto" (default), the function chooses:
#' \itemize{
#'   \item t-test if both groups have n >= 30 (Central Limit Theorem applies)
#'   \item Wilcoxon test if either group has n < 30 (safer for small samples)
#' }
#'
#' ## Interpreting Results:
#' \itemize{
#'   \item p_value < 0.05: Groups are significantly different (reject null hypothesis)
#'   \item p_value >= 0.05: No significant difference detected (fail to reject null)
#' }
#'
#' @examples
#' # Example 1: Compare spending between two groups
#' treatment_spending <- c(45.2, 67.8, 23.4, 89.1, 34.5, 56.7, 78.9, 12.3)
#' control_spending <- c(34.1, 45.2, 28.9, 56.3, 41.2, 38.7, 49.1, 31.4)
#' test_group_differences(treatment_spending, control_spending)
#'
#' # Example 2: One-sided test (is group 1 greater than group 2?)
#' satisfaction_a <- c(7, 8, 9, 7, 8, 9, 8, 7)
#' satisfaction_b <- c(5, 6, 7, 5, 6, 6, 5, 7)
#' test_group_differences(satisfaction_a, satisfaction_b, alternative = "greater")
#'
#' # Example 3: Paired observations (before/after measurements)
#' before <- c(65, 72, 68, 71, 69, 70)
#' after <- c(62, 68, 65, 69, 66, 67)
#' test_group_differences(before, after, paired = TRUE)
#'
#' @export
test_group_differences <- function(group1,
                                  group2,
                                  test_type = "auto",
                                  alternative = "two.sided",
                                  conf_level = 0.95,
                                  paired = FALSE,
                                  check_assumptions = TRUE,
                                  verbose = TRUE) {

  # Step 1: Input validation
  # -------------------------
  # Ensure both inputs are numeric
  if (!is.numeric(group1) || !is.numeric(group2)) {
    stop("Error: Both group1 and group2 must be numeric vectors.")
  }

  # Check that alternative hypothesis is valid
  valid_alternatives <- c("two.sided", "greater", "less")
  if (!alternative %in% valid_alternatives) {
    stop("Error: 'alternative' must be one of: ",
         paste(valid_alternatives, collapse = ", "))
  }

  # Check that test_type is valid
  valid_tests <- c("auto", "t.test", "wilcoxon")
  if (!test_type %in% valid_tests) {
    stop("Error: 'test_type' must be one of: ",
         paste(valid_tests, collapse = ", "))
  }

  # Step 2: Remove missing values
  # ------------------------------
  # Count missing values for transparency
  n_missing_g1 <- sum(is.na(group1))
  n_missing_g2 <- sum(is.na(group2))

  # Remove NAs
  group1_clean <- group1[!is.na(group1)]
  group2_clean <- group2[!is.na(group2)]

  # Report missing values if any were found
  if (n_missing_g1 > 0 || n_missing_g2 > 0) {
    message("Note: Removed ", n_missing_g1, " missing value(s) from group1 and ",
            n_missing_g2, " missing value(s) from group2.")
  }

  # Check we have sufficient data
  if (length(group1_clean) < 2 || length(group2_clean) < 2) {
    stop("Error: Each group must have at least 2 valid observations.")
  }

  # For paired tests, groups must be same size
  if (paired && length(group1_clean) != length(group2_clean)) {
    stop("Error: For paired tests, group1 and group2 must have the same length ",
         "after removing missing values.")
  }

  # Step 2.5: Check statistical assumptions
  # ----------------------------------------
  assumptions <- NULL

  if (check_assumptions && !paired) {
    if (verbose) {
      message("\nChecking statistical assumptions...")
    }

    assumptions_data <- data.frame(
      dv = c(group1_clean, group2_clean),
      group = factor(
        c(rep("Group 1", length(group1_clean)),
          rep("Group 2", length(group2_clean)))
      )
    )

    assumptions_model <- lm(dv ~ group, data = assumptions_data)

    assumptions <- check_test_assumptions(
      test_type = "t_test",
      data = assumptions_data,
      groups = "group",
      residuals = residuals(assumptions_model),
      verbose = verbose
    )

    if (verbose) {
      message("Assumption checks complete. See 'assumptions' element in results for details.\n")
    }
  }

  # Step 3: Determine which test to use
  # ------------------------------------
  actual_test <- test_type
  use_welch <- FALSE

  if (test_type == "auto") {
    # Use assumptions checks and sample size to select test
    if (check_assumptions && !paired && !is.null(assumptions)) {
      # Decision based on assumptions
      norm_met <- assumptions$normality$assumption_met
      homog_met <- if (!is.null(assumptions$homogeneity)) {
        assumptions$homogeneity$assumption_met
      } else {
        TRUE
      }

      if (norm_met) {
        # Residuals appear normal
        actual_test <- "t.test"
        if (!homog_met) {
          use_welch <- TRUE
          message("Auto-selection: Using Welch's t-test (normality met, but variances unequal)")
        } else {
          message("Auto-selection: Using t-test (assumptions met)")
        }
      } else {
        # Normality violated
        actual_test <- "wilcoxon"
        message("Auto-selection: Using Wilcoxon test (normality assumption violated)")
      }
    } else {
      # No assumptions checked - use sample size heuristic
      # Use Central Limit Theorem as guide: n >= 30 is often sufficient for normality
      # For smaller samples, use non-parametric test to be safe
      if (length(group1_clean) >= 30 && length(group2_clean) >= 30) {
        actual_test <- "t.test"
        message("Auto-selection: Using t-test (both groups have n >= 30)")
      } else {
        actual_test <- "wilcoxon"
        message("Auto-selection: Using Wilcoxon test (small sample size)")
      }
    }
  } else if (test_type == "t.test" && check_assumptions && !paired &&
             !is.null(assumptions) && !is.null(assumptions$homogeneity)) {
    # Manual t-test selection - check if Welch's should be used
    if (!assumptions$homogeneity$assumption_met) {
      use_welch <- TRUE
      message("Note: Using Welch's correction for unequal variances")
    }
  }

  # Step 4: Perform the statistical test
  # -------------------------------------
  if (actual_test == "t.test") {
    # T-test: Assumes normal distribution
    # Tests if means are different between groups
    test_result <- t.test(
      x = group1_clean,
      y = group2_clean,
      alternative = alternative,
      conf.level = conf_level,
      paired = paired,
      var.equal = !use_welch  # Use Welch's if use_welch = TRUE
    )

    if (paired) {
      test_name <- "Paired t-test"
    } else if (use_welch) {
      test_name <- "Welch's t-test"
    } else {
      test_name <- "Student's t-test"
    }

  } else if (actual_test == "wilcoxon") {
    # Wilcoxon test: Non-parametric, doesn't assume normality
    # Tests if distributions are different between groups
    test_result <- wilcox.test(
      x = group1_clean,
      y = group2_clean,
      alternative = alternative,
      conf.level = conf_level,
      paired = paired
    )
    test_name <- ifelse(paired, "Paired Wilcoxon test", "Wilcoxon rank-sum test")
  }

  # Step 5: Calculate descriptive statistics and effect sizes
  # ----------------------------------------------------------
  # Calculate means and SDs to understand the difference
  mean_g1 <- mean(group1_clean)
  mean_g2 <- mean(group2_clean)
  sd_g1 <- sd(group1_clean)
  sd_g2 <- sd(group2_clean)
  mean_diff <- mean_g1 - mean_g2

  # Calculate Cohen's d effect size
  if (paired) {
    # For paired data, use SD of differences
    differences <- group1_clean - group2_clean
    pooled_sd <- sd(differences)
    cohens_d <- mean_diff / pooled_sd
  } else {
    # For independent groups, use pooled SD
    n1 <- length(group1_clean)
    n2 <- length(group2_clean)
    pooled_sd <- sqrt(((n1 - 1) * sd_g1^2 + (n2 - 1) * sd_g2^2) / (n1 + n2 - 2))
    cohens_d <- mean_diff / pooled_sd
  }

  # Interpret effect size
  abs_d <- abs(cohens_d)
  if (abs_d < 0.2) {
    effect_interp <- "negligible"
  } else if (abs_d < 0.5) {
    effect_interp <- "small"
  } else if (abs_d < 0.8) {
    effect_interp <- "medium"
  } else {
    effect_interp <- "large"
  }

  # Step 6: Extract and interpret results
  # --------------------------------------
  p_value <- test_result$p.value
  is_significant <- p_value < 0.05

  # Create human-readable interpretation
  if (is_significant) {
    if (mean_diff > 0) {
      interpretation <- paste0(
        "The groups are significantly different (p = ", round(p_value, 4), "). ",
        "Group 1 (M = ", round(mean_g1, 2), ", SD = ", round(sd_g1, 2), ") ",
        "has a higher mean than Group 2 (M = ", round(mean_g2, 2), ", SD = ",
        round(sd_g2, 2), "), with a mean difference of ", round(mean_diff, 2), ". ",
        "The effect size is ", effect_interp, " (Cohen's d = ", round(cohens_d, 2), ")."
      )
    } else {
      interpretation <- paste0(
        "The groups are significantly different (p = ", round(p_value, 4), "). ",
        "Group 2 (M = ", round(mean_g2, 2), ", SD = ", round(sd_g2, 2), ") ",
        "has a higher mean than Group 1 (M = ", round(mean_g1, 2), ", SD = ",
        round(sd_g1, 2), "), with a mean difference of ", round(abs(mean_diff), 2), ". ",
        "The effect size is ", effect_interp, " (Cohen's d = ", round(cohens_d, 2), ")."
      )
    }
  } else {
    interpretation <- paste0(
      "No significant difference detected between groups (p = ", round(p_value, 4), "). ",
      "Group 1: M = ", round(mean_g1, 2), ", SD = ", round(sd_g1, 2), "; ",
      "Group 2: M = ", round(mean_g2, 2), ", SD = ", round(sd_g2, 2), ". ",
      "The effect size is ", effect_interp, " (Cohen's d = ", round(cohens_d, 2), ")."
    )
  }

  # Step 6.5: Generate publication-ready text
  # ------------------------------------------
  # Get confidence interval (handle cases where conf.int is NULL or unavailable)
  ci_lower <- if (!is.null(test_result$conf.int)) test_result$conf.int[1] else NA
  ci_upper <- if (!is.null(test_result$conf.int)) test_result$conf.int[2] else NA

  ci_text <- if (!is.na(ci_lower) && !is.na(ci_upper)) {
    paste0(
      round(conf_level * 100),
      "% CI [",
      round(ci_lower, 2),
      ", ",
      round(ci_upper, 2),
      "]"
    )
  } else {
    "CI not reported"
  }

  pub_block <- NULL
  if (actual_test == "t.test" && !paired) {
    pub_test_type <- if (use_welch) "welch_t" else "t_test"
    pub_block <- render_apa7_text(
      test_type = pub_test_type,
      section = "results",
      values = list(
        significance_text = if (p_value < 0.05) "was significant" else "was not significant",
        df = round(test_result$parameter, 2),
        t_stat = format(test_result$statistic, digits = 2),
        p_text = format_p_apa7(p_value),
        ci_text = ci_text
      )
    )
  }

  # Step 7: Package results into a clear list
  # ------------------------------------------
  sample_n <- if (paired) length(group1_clean) else length(group1_clean) + length(group2_clean)
  results <- build_analysis_result(
    test_type = if (actual_test == "t.test") "t_test" else "wilcoxon",
    test_name = test_name,
    core_stats = list(
      p_value = p_value,
      n = sample_n,
      significant = is_significant
    ),
    specific_stats = list(
      statistic = test_result$statistic,
      df = test_result$parameter
    ),
    assumptions = assumptions,
    interpretation = interpretation,
    publication_text = pub_block,
    test_used = test_name,
    group1_mean = round(mean_g1, 2),
    group2_mean = round(mean_g2, 2),
    group1_sd = round(sd_g1, 2),
    group2_sd = round(sd_g2, 2),
    difference = round(mean_diff, 2),
    ci_lower = if (!is.na(ci_lower)) round(ci_lower, 2) else NA,
    ci_upper = if (!is.na(ci_upper)) round(ci_upper, 2) else NA,
    effect_size = round(cohens_d, 2),
    effect_interpretation = effect_interp,
    group1_n = length(group1_clean),
    group2_n = length(group2_clean),
    full_test_output = test_result
  )

  # Step 8: Return results
  # ----------------------
  return(results)
}


#' Print Method for Group Comparison Results
#'
#' @param x A group_comparison object
#' @param show_assumptions Logical - show assumption check details? (default FALSE)
#' @param show_publication Logical - show publication text? (default FALSE)
#' @param ... Additional arguments (not used)
#'
#' @export
print.group_comparison <- function(x, show_assumptions = FALSE, show_publication = FALSE, ...) {
  cat("\n")
  cat("=" %+% rep("=", 70) %+% "=\n", sep = "")
  cat("GROUP COMPARISON RESULTS\n")
  cat("=" %+% rep("=", 70) %+% "=\n", sep = "")
  cat("\n")

  cat("Test Used:", x$test_used, "\n")
  cat("Sample Sizes: Group 1 (n =", x$group1_n, "), Group 2 (n =", x$group2_n, ")\n")
  cat("\n")

  cat("DESCRIPTIVE STATISTICS:\n")
  cat("  Group 1: M =", x$group1_mean, ", SD =", x$group1_sd, "\n")
  cat("  Group 2: M =", x$group2_mean, ", SD =", x$group2_sd, "\n")
  if (!is.na(x$ci_lower) && !is.na(x$ci_upper)) {
    cat("  Difference:", x$difference, ", 95% CI [", x$ci_lower, ",", x$ci_upper, "]\n")
  } else {
    cat("  Difference:", x$difference, "\n")
  }
  cat("\n")

  cat("STATISTICAL RESULTS:\n")
  cat("  Test statistic:", round(x$statistic, 3), "\n")
  if (!is.null(x$df) && !is.na(x$df)) {
    cat("  Degrees of freedom:", round(x$df, 1), "\n")
  }
  cat("  p-value:", x$p_value, "\n")
  cat("  Significant:", ifelse(x$significant, "YES (p < .05)", "NO (p >= .05)"), "\n")
  cat("\n")

  cat("EFFECT SIZE:\n")
  cat("  Cohen's d =", x$effect_size, "(", x$effect_interpretation, ")\n")
  cat("\n")

  cat("INTERPRETATION:\n")
  cat(strwrap(x$interpretation, width = 70, prefix = "  "), sep = "\n")
  cat("\n")

  if (show_assumptions && !is.null(x$assumptions)) {
    cat("\n")
    cat("-" %+% rep("-", 70) %+% "-\n", sep = "")
    cat("ASSUMPTION CHECKS:\n")
    cat("-" %+% rep("-", 70) %+% "-\n", sep = "")
    for (assump_name in names(x$assumptions)) {
      cat("\n", toupper(gsub("_", " ", assump_name)), ":\n", sep = "")
      print(x$assumptions[[assump_name]], verbose = FALSE)
    }
  }

  if (show_publication && !is.null(x$publication_text)) {
    cat("\n")
    print(x$publication_text)
  }

  if (!show_assumptions && !is.null(x$assumptions)) {
    cat("\nTip: Use print(result, show_assumptions = TRUE) to see assumption checks\n")
  }

  if (!show_publication) {
    cat("Tip: Use print(result, show_publication = TRUE) to see publication-ready text\n")
  }

  cat("\n")

  invisible(x)
}
