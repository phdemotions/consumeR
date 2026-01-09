#' Check Statistical Assumptions
#'
#' @description
#' This file contains functions to explicitly check statistical assumptions
#' for various tests. Each function provides detailed interpretation suitable
#' for publication and educational purposes.
#'
#' @name assumptions
NULL

#' Check Normality Assumption
#'
#' @description
#' Tests whether data follows a normal distribution using the Shapiro-Wilk test.
#' Provides detailed interpretation for publication and education.
#'
#' @param data Numeric vector to test
#' @param variable_name Name of variable (for reporting)
#' @param alpha Significance level (default 0.05)
#'
#' @return List with test results and interpretation
#' @export
#'
#' @examples
#' check_normality(rnorm(100), "satisfaction_scores")
check_normality <- function(data, variable_name = "data", alpha = 0.05) {

  # Remove missing values
  data_clean <- data[!is.na(data)]
  n <- length(data_clean)
  n_missing <- sum(is.na(data))

  # Check sample size
  if (n < 3) {
    return(list(
      test = "Shapiro-Wilk",
      assumption_met = NA,
      p_value = NA,
      statistic = NA,
      n = n,
      interpretation = paste0(
        "Sample size too small (n = ", n, ") to test normality assumption. ",
        "The Shapiro-Wilk test requires at least 3 observations."
      ),
      recommendation = "Collect more data or use non-parametric tests.",
      publication_text = "Due to insufficient sample size, normality could not be assessed.",
      verbose_explanation = paste0(
        "The normality assumption is a fundamental requirement for many parametric ",
        "statistical tests (such as t-tests, ANOVA, and linear regression). However, ",
        "with only ", n, " observation(s), we cannot reliably assess whether the data ",
        "follows a normal distribution. Non-parametric alternatives that do not assume ",
        "normality should be used instead."
      )
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

  # Create interpretation
  if (assumption_met) {
    basic_interp <- paste0(
      "The normality assumption is MET for ", variable_name,
      " (Shapiro-Wilk W = ", round(w_stat, 4),
      ", p = ", round(p_val, 4), ", p > ", alpha, ")."
    )

    recommendation <- paste0(
      "Parametric tests (e.g., t-tests, ANOVA, linear regression) are appropriate. ",
      "The data distribution does not significantly deviate from normality."
    )

    pub_text <- paste0(
      "Data normality was assessed using the Shapiro-Wilk test. ",
      "The ", variable_name, " variable met the normality assumption ",
      "(W = ", round(w_stat, 4), ", p = ", round(p_val, 4), ")."
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
      ", p = ", round(p_val, 4), ", p < ", alpha, ")."
    )

    recommendation <- paste0(
      "Consider: (1) Using non-parametric alternatives (e.g., Mann-Whitney U instead of t-test, ",
      "Kruskal-Wallis instead of ANOVA), (2) Transforming the data (log, square root, Box-Cox), ",
      "or (3) If sample size is large (n > 30), parametric tests may still be robust due to ",
      "the Central Limit Theorem."
    )

    pub_text <- paste0(
      "Data normality was assessed using the Shapiro-Wilk test. ",
      "The ", variable_name, " variable violated the normality assumption ",
      "(W = ", round(w_stat, 4), ", p = ", round(p_val, 4), "). ",
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
      "which can make interpretation more complex.\n\n",
      "3. PROCEED WITH CAUTION IF SAMPLE IS LARGE: If you have a large sample (typically n > 30 per group), ",
      "parametric tests are often still robust to violations of normality due to the Central Limit Theorem. ",
      "Your sample size is n = ", n, ".\n\n",
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

  # Return results
  structure(
    list(
      test = "Shapiro-Wilk",
      assumption = "Normality",
      assumption_met = assumption_met,
      p_value = p_val,
      statistic = w_stat,
      n = n,
      n_missing = n_missing,
      alpha = alpha,
      skewness = skew,
      kurtosis = kurt,
      interpretation = basic_interp,
      recommendation = recommendation,
      publication_text = pub_text,
      verbose_explanation = verbose
    ),
    class = "assumption_check"
  )
}


#' Check Homogeneity of Variance (Homoscedasticity)
#'
#' @description
#' Tests whether groups have equal variances using Levene's test.
#' Required for t-tests and ANOVA.
#'
#' @param data Numeric vector of values
#' @param groups Factor or vector indicating group membership
#' @param alpha Significance level (default 0.05)
#'
#' @return List with test results and interpretation
#' @export
#'
#' @examples
#' check_homogeneity_of_variance(c(rnorm(50, 5, 1), rnorm(50, 6, 1)),
#'                                rep(c("A", "B"), each = 50))
check_homogeneity_of_variance <- function(data, groups, alpha = 0.05) {

  # Clean data
  valid_idx <- !is.na(data) & !is.na(groups)
  data_clean <- data[valid_idx]
  groups_clean <- as.factor(groups[valid_idx])

  n_total <- length(data_clean)
  n_missing <- sum(!valid_idx)
  n_groups <- nlevels(groups_clean)
  group_names <- levels(groups_clean)

  # Check requirements
  if (n_groups < 2) {
    return(list(
      test = "Levene's Test",
      assumption_met = NA,
      interpretation = "At least 2 groups required for homogeneity of variance test.",
      publication_text = "Homogeneity of variance could not be assessed (only one group).",
      verbose_explanation = "The homogeneity of variance assumption requires comparing variability across multiple groups. Only one group was provided."
    ))
  }

  # Calculate group statistics
  group_stats <- tapply(data_clean, groups_clean, function(x) {
    c(n = length(x), mean = mean(x), sd = sd(x), var = var(x))
  })

  # Perform Levene's test (using car package if available, otherwise manual)
  # Manual Levene's test using absolute deviations from median
  group_medians <- tapply(data_clean, groups_clean, median)
  abs_dev <- abs(data_clean - group_medians[groups_clean])

  levene_anova <- anova(lm(abs_dev ~ groups_clean))
  f_stat <- levene_anova$`F value`[1]
  p_val <- levene_anova$`Pr(>F)`[1]
  df1 <- levene_anova$Df[1]
  df2 <- levene_anova$Df[2]

  assumption_met <- p_val > alpha

  # Calculate variance ratio (for interpretation)
  variances <- sapply(group_stats, function(x) x["var"])
  var_ratio <- max(variances) / min(variances)

  # Interpretation
  if (assumption_met) {
    basic_interp <- paste0(
      "The homogeneity of variance assumption is MET ",
      "(Levene's test: F(", df1, ", ", df2, ") = ", round(f_stat, 3),
      ", p = ", round(p_val, 4), ")."
    )

    recommendation <- paste0(
      "The groups have similar variances. Standard t-test or ANOVA procedures are appropriate. ",
      "The variance ratio (largest/smallest) is ", round(var_ratio, 2), ", which is acceptable."
    )

    pub_text <- paste0(
      "Homogeneity of variance was assessed using Levene's test. ",
      "The assumption was met (F(", df1, ", ", df2, ") = ", round(f_stat, 3),
      ", p = ", round(p_val, 4), "), indicating equal variances across groups."
    )

    verbose <- paste0(
      "Homogeneity of variance (also called homoscedasticity) is a critical assumption for ",
      "t-tests and ANOVA. It means that the variability (spread) of scores should be roughly ",
      "the same across all groups being compared. Levene's test evaluates this assumption.\n\n",
      "The null hypothesis of Levene's test states that all groups have equal variances. ",
      "With a p-value of ", round(p_val, 4), " (greater than alpha = ", alpha, "), we fail to ",
      "reject the null hypothesis. This is good news! It means the variances across your groups ",
      "are statistically similar.\n\n",
      "In practical terms: The spread of scores is similar across all ", n_groups, " groups. ",
      "This means you can use standard statistical procedures (regular t-test or ANOVA) without ",
      "modification. If this assumption had been violated, you would need to use Welch's t-test ",
      "or Welch's ANOVA, which don't assume equal variances.\n\n",
      "Technical details: The variance ratio (largest group variance divided by smallest) is ",
      round(var_ratio, 2), ". A rule of thumb is that ratios less than 3:1 are generally acceptable, ",
      "and yours meets this criterion. The F-statistic from Levene's test is ", round(f_stat, 3),
      " with ", df1, " and ", df2, " degrees of freedom."
    )

  } else {
    basic_interp <- paste0(
      "The homogeneity of variance assumption is VIOLATED ",
      "(Levene's test: F(", df1, ", ", df2, ") = ", round(f_stat, 3),
      ", p = ", round(p_val, 4), ")."
    )

    recommendation <- paste0(
      "Use Welch's t-test (for two groups) or Welch's ANOVA (for multiple groups), ",
      "which do not assume equal variances. The variance ratio is ", round(var_ratio, 2),
      ", indicating meaningful differences in group variability."
    )

    pub_text <- paste0(
      "Homogeneity of variance was assessed using Levene's test. ",
      "The assumption was violated (F(", df1, ", ", df2, ") = ", round(f_stat, 3),
      ", p = ", round(p_val, 4), "). Accordingly, Welch's correction was applied."
    )

    verbose <- paste0(
      "Homogeneity of variance (also called homoscedasticity) is a critical assumption for ",
      "t-tests and ANOVA. It means that the variability (spread) of scores should be roughly ",
      "the same across all groups being compared. Levene's test evaluates this assumption.\n\n",
      "The null hypothesis of Levene's test states that all groups have equal variances. ",
      "With a p-value of ", round(p_val, 4), " (less than alpha = ", alpha, "), we reject the ",
      "null hypothesis. This means the variances across your groups are significantly different.\n\n",
      "In practical terms: At least one group has notably different variability than the others. ",
      "This is actually quite common in consumer research, especially when comparing different ",
      "market segments or experimental conditions. Don't worry - this doesn't invalidate your analysis!\n\n",
      "What to do: You should use modified versions of your planned tests:\n",
      "- If comparing 2 groups: Use Welch's t-test instead of Student's t-test\n",
      "- If comparing 3+ groups: Use Welch's ANOVA instead of standard ANOVA\n\n",
      "These 'Welch' versions do not assume equal variances and will give you valid results. ",
      "They're widely accepted in peer-reviewed publications and are often recommended as the ",
      "default approach even when variances are equal.\n\n",
      "Technical details: The variance ratio (largest group variance divided by smallest) is ",
      round(var_ratio, 2), ". Ratios greater than 3:1 are particularly concerning. ",
      "The F-statistic from Levene's test is ", round(f_stat, 3), " with ", df1, " and ",
      df2, " degrees of freedom."
    )
  }

  # Return results
  structure(
    list(
      test = "Levene's Test",
      assumption = "Homogeneity of Variance",
      assumption_met = assumption_met,
      p_value = p_val,
      statistic = f_stat,
      df1 = df1,
      df2 = df2,
      n_total = n_total,
      n_missing = n_missing,
      n_groups = n_groups,
      group_names = group_names,
      variance_ratio = var_ratio,
      group_statistics = group_stats,
      alpha = alpha,
      interpretation = basic_interp,
      recommendation = recommendation,
      publication_text = pub_text,
      verbose_explanation = verbose
    ),
    class = "assumption_check"
  )
}


#' Check Independence of Observations
#'
#' @description
#' Provides guidance on the independence assumption.
#' True independence is a design issue, not something that can be statistically tested
#' after data collection.
#'
#' @param data_structure Description of data collection design
#' @param is_independent Logical - are observations independent?
#' @param clustering_note Optional note about clustering/nesting in data
#'
#' @return List with guidance on independence assumption
#' @export
check_independence <- function(data_structure = "cross-sectional survey",
                                is_independent = TRUE,
                                clustering_note = NULL) {

  if (is_independent) {
    interpretation <- paste0(
      "The independence assumption is MET based on study design (",
      data_structure, ")."
    )

    recommendation <- paste0(
      "Each observation represents a unique, independent unit. ",
      "Standard statistical procedures are appropriate."
    )

    pub_text <- paste0(
      "The data were collected using a ", data_structure, " design, ",
      "ensuring independence of observations."
    )

    verbose <- paste0(
      "Independence of observations is one of the most important assumptions in statistics, ",
      "yet it cannot be tested statistically - it's a function of your research design.\n\n",
      "What independence means: Each data point should represent a unique individual or unit, ",
      "and the response of one participant should not influence the response of another. ",
      "Think of it this way - if you know Person A's score, does that tell you anything about ",
      "Person B's score (beyond what you'd expect by chance)? If not, they're independent.\n\n",
      "Your study uses a ", data_structure, " design. Based on this design and your confirmation, ",
      "the observations are independent. This is excellent and means you can proceed with standard ",
      "statistical analyses.\n\n",
      "Common violations to watch for in future studies:\n",
      "- Repeated measures (same person measured multiple times)\n",
      "- Clustered data (students within classrooms, customers within stores)\n",
      "- Matched pairs or siblings\n",
      "- Time series data (observations across time)\n",
      "- Social network data (friends influencing friends)\n\n",
      "If you had violations, you'd need specialized methods like repeated measures ANOVA, ",
      "mixed effects models, or multilevel modeling."
    )

  } else {
    interpretation <- paste0(
      "The independence assumption is VIOLATED based on study design (",
      data_structure, ")."
    )

    recommendation <- paste0(
      "Use appropriate methods for dependent data: repeated measures ANOVA, ",
      "mixed effects models, paired tests, or multilevel modeling depending on the structure."
    )

    pub_text <- paste0(
      "The data structure (", data_structure, ") involved non-independent observations. ",
      "Accordingly, [appropriate method for dependent data] was employed."
    )

    verbose <- paste0(
      "Independence of observations is one of the most important assumptions in statistics. ",
      "Your study design (", data_structure, ") creates dependencies in the data.\n\n",
      "What this means: Some observations are related to each other in ways beyond random chance. ",
      "This could be because you measured the same people multiple times, or because observations ",
      "are nested within groups (like customers within stores, or students within classrooms).\n\n",
      "Why it matters: Standard statistical tests (t-tests, ANOVA, regression) assume independence. ",
      "When observations are dependent, these tests:\n",
      "- Underestimate standard errors (making them too small)\n",
      "- Produce p-values that are too small (increasing Type I error)\n",
      "- Can lead to false positive findings\n\n",
      "What to do: You need specialized statistical methods that account for the dependency:\n",
      "- Repeated measures: Use repeated measures ANOVA or paired t-tests\n",
      "- Clustered/nested data: Use mixed effects models (also called multilevel or hierarchical models)\n",
      "- Time series: Use time series analysis methods\n",
      "- Matched pairs: Use paired tests\n\n",
      "These methods are widely available in R and well-accepted in publications. They're not ",
      "more difficult to report - you just need to specify that you accounted for the data structure."
    )
  }

  if (!is.null(clustering_note)) {
    verbose <- paste0(verbose, "\n\nAdditional context: ", clustering_note)
  }

  structure(
    list(
      test = "Design Review",
      assumption = "Independence of Observations",
      assumption_met = is_independent,
      data_structure = data_structure,
      clustering_note = clustering_note,
      interpretation = interpretation,
      recommendation = recommendation,
      publication_text = pub_text,
      verbose_explanation = verbose
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
    cat("Result:", ifelse(x$assumption_met, "✓ ASSUMPTION MET", "✗ ASSUMPTION VIOLATED"), "\n")
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

  cat("\n")

  invisible(x)
}

# Helper function for string concatenation
`%+%` <- function(a, b) paste0(a, b)
