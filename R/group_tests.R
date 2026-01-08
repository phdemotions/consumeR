#' Test for Differences Between Consumer Groups
#'
#' This function compares two groups of consumers to determine if they
#' are statistically different. It is designed for maximum transparency
#' during peer review, with clear documentation of every step and assumption.
#'
#' The function performs either a t-test (for normally distributed data)
#' or a Wilcoxon test (for non-normal data). All steps are documented
#' and assumptions are clearly stated.
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
#' @param conf_level Numeric. Confidence level for the interval. Default is 0.95
#'   (95% confidence).
#' @param paired Logical. Are the observations paired? Default is FALSE.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{test_used}: Name of the statistical test performed
#'     \item \code{p_value}: P-value from the test
#'     \item \code{significant}: Logical, TRUE if p < 0.05
#'     \item \code{group1_mean}: Mean of group 1
#'     \item \code{group2_mean}: Mean of group 2
#'     \item \code{difference}: Difference between group means
#'     \item \code{interpretation}: Plain English interpretation of results
#'     \item \code{full_test_output}: Complete test results object
#'   }
#'
#' @details
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
#' @importFrom stats t.test wilcox.test
test_group_differences <- function(group1,
                                  group2,
                                  test_type = "auto",
                                  alternative = "two.sided",
                                  conf_level = 0.95,
                                  paired = FALSE) {

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

  # Step 3: Determine which test to use
  # ------------------------------------
  actual_test <- test_type

  if (test_type == "auto") {
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
      paired = paired
    )
    test_name <- ifelse(paired, "Paired t-test", "Two-sample t-test")

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

  # Step 5: Calculate descriptive statistics
  # -----------------------------------------
  # Calculate means to understand the direction of difference
  mean_g1 <- mean(group1_clean)
  mean_g2 <- mean(group2_clean)
  mean_diff <- mean_g1 - mean_g2

  # Step 6: Extract and interpret results
  # --------------------------------------
  p_value <- test_result$p.value
  is_significant <- p_value < 0.05

  # Create human-readable interpretation
  if (is_significant) {
    if (mean_diff > 0) {
      interpretation <- paste0(
        "The groups are significantly different (p = ", round(p_value, 4), "). ",
        "Group 1 has a higher mean (", round(mean_g1, 2), ") than Group 2 (",
        round(mean_g2, 2), "), with a difference of ", round(mean_diff, 2), "."
      )
    } else {
      interpretation <- paste0(
        "The groups are significantly different (p = ", round(p_value, 4), "). ",
        "Group 2 has a higher mean (", round(mean_g2, 2), ") than Group 1 (",
        round(mean_g1, 2), "), with a difference of ", round(abs(mean_diff), 2), "."
      )
    }
  } else {
    interpretation <- paste0(
      "No significant difference detected between groups (p = ", round(p_value, 4), "). ",
      "Group 1 mean: ", round(mean_g1, 2), ", Group 2 mean: ", round(mean_g2, 2), "."
    )
  }

  # Step 7: Package results into a clear list
  # ------------------------------------------
  results <- list(
    test_used = test_name,
    p_value = round(p_value, 6),
    significant = is_significant,
    group1_mean = round(mean_g1, 2),
    group2_mean = round(mean_g2, 2),
    difference = round(mean_diff, 2),
    group1_n = length(group1_clean),
    group2_n = length(group2_clean),
    interpretation = interpretation,
    full_test_output = test_result
  )

  # Step 8: Return results
  # ----------------------
  return(results)
}
