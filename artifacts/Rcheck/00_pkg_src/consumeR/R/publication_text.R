#' Generate Publication-Ready Text for Statistical Results
#'
#' @description
#' Functions to generate publication-ready text snippets for various statistical
#' analyses, following APA 7th edition style and Journal of Consumer Psychology
#' standards.
#'
#' @name publication_text
NULL


#' Generate Complete Publication Text Block
#'
#' @description
#' Creates a comprehensive publication-ready text block including assumptions,
#' methods, results, and interpretation.
#'
#' @param test_type Type of statistical test
#' @param assumptions_checks List of assumption check results
#' @param test_results Main statistical test results
#' @param effect_sizes Optional effect sizes
#' @param additional_notes Optional additional methodological notes
#'
#' @return Character vector with publication text sections
#' @export
generate_publication_block <- function(test_type,
                                        assumptions_checks = NULL,
                                        test_results,
                                        effect_sizes = NULL,
                                        additional_notes = NULL) {

  sections <- list()

  # Assumptions section
  if (!is.null(assumptions_checks) && length(assumptions_checks) > 0) {
    sections$assumptions <- generate_assumptions_text(assumptions_checks)
  }

  # Methods section
  sections$methods <- generate_methods_text(test_type, test_results)

  # Results section
  sections$results <- generate_results_text(test_type, test_results, effect_sizes)

  # Interpretation section
  sections$interpretation <- generate_interpretation_text(test_type, test_results)

  # Additional notes
  if (!is.null(additional_notes)) {
    sections$additional <- additional_notes
  }

  structure(
    sections,
    class = "publication_block"
  )
}


#' Generate Assumptions Text
#'
#' @param assumptions_checks List of assumption check objects
#' @return Character vector
#' @noRd
generate_assumptions_text <- function(assumptions_checks) {

  if (length(assumptions_checks) == 0) {
    return("No formal assumption checks were performed.")
  }

  texts <- sapply(assumptions_checks, function(check) {
    if ("publication_text" %in% names(check)) {
      check$publication_text
    } else {
      NULL
    }
  })

  texts <- texts[!is.null(texts)]

  if (length(texts) == 0) {
    return(NULL)
  }

  # Combine into paragraph
  paste(texts, collapse = " ")
}


#' Generate Methods Text for Different Test Types
#'
#' @param test_type Type of test
#' @param test_results Test results object
#' @return Character vector
#' @noRd
generate_methods_text <- function(test_type, test_results) {

  switch(test_type,
    "t_test" = generate_ttest_methods(test_results),
    "welch_t" = generate_welch_methods(test_results),
    "paired_t" = generate_paired_t_methods(test_results),
    "anova" = generate_anova_methods(test_results),
    "regression" = generate_regression_methods(test_results),
    "correlation" = generate_correlation_methods(test_results),
    paste0("Statistical analysis was conducted using ", test_type, ".")
  )
}


#' Generate Results Text for Different Test Types
#'
#' @param test_type Type of test
#' @param test_results Test results object
#' @param effect_sizes Optional effect sizes
#' @return Character vector
#' @noRd
generate_results_text <- function(test_type, test_results, effect_sizes = NULL) {

  switch(test_type,
    "t_test" = generate_ttest_results(test_results, effect_sizes),
    "welch_t" = generate_welch_results(test_results, effect_sizes),
    "paired_t" = generate_paired_t_results(test_results, effect_sizes),
    "anova" = generate_anova_results(test_results, effect_sizes),
    "regression" = generate_regression_results(test_results),
    "correlation" = generate_correlation_results(test_results),
    "Results were statistically significant."
  )
}


#' Generate Interpretation Text
#'
#' @param test_type Type of test
#' @param test_results Test results object
#' @return Character vector
#' @noRd
generate_interpretation_text <- function(test_type, test_results) {

  # Extract significance
  is_sig <- if ("p_value" %in% names(test_results)) {
    test_results$p_value < 0.05
  } else if ("significant" %in% names(test_results)) {
    test_results$significant
  } else {
    NA
  }

  if (is.na(is_sig)) {
    return("Results should be interpreted in the context of the research question.")
  }

  if (is_sig) {
    "These results provide evidence of a statistically significant effect."
  } else {
    paste0(
      "These results do not provide evidence of a statistically significant effect. ",
      "However, absence of evidence is not evidence of absence; the study may be ",
      "underpowered to detect smaller effects."
    )
  }
}


#' Generate T-Test Methods Text
#' @noRd
generate_ttest_methods <- function(results) {
  paste0(
    "An independent samples t-test was conducted to compare means between groups. ",
    "The test assumes normality of distributions and homogeneity of variance."
  )
}


#' Generate Welch T-Test Methods Text
#' @noRd
generate_welch_methods <- function(results) {
  paste0(
    "A Welch's t-test (also known as Welch's unequal variances t-test) was conducted ",
    "to compare means between groups. This test does not assume equal variances ",
    "and is robust to heteroscedasticity."
  )
}


#' Generate Paired T-Test Methods Text
#' @noRd
generate_paired_t_methods <- function(results) {
  paste0(
    "A paired-samples t-test was conducted to compare means within subjects ",
    "across conditions. This test accounts for the correlation between paired observations."
  )
}


#' Generate T-Test Results Text
#' @noRd
generate_ttest_results <- function(results, effect_sizes = NULL) {

  # Extract values
  m1 <- if ("group1_mean" %in% names(results)) results$group1_mean else results$mean1
  m2 <- if ("group2_mean" %in% names(results)) results$group2_mean else results$mean2
  sd1 <- if ("group1_sd" %in% names(results)) results$group1_sd else NULL
  sd2 <- if ("group2_sd" %in% names(results)) results$group2_sd else NULL
  t_stat <- results$statistic
  df <- results$df
  p_val <- results$p_value
  ci_lower <- if ("ci_lower" %in% names(results)) results$ci_lower else NULL
  ci_upper <- if ("ci_upper" %in% names(results)) results$ci_upper else NULL

  # Format p-value
  p_text <- if (p_val < 0.001) {
    "p < .001"
  } else {
    paste0("p = ", format_pvalue(p_val))
  }

  # Build results text
  result_text <- paste0(
    "The independent samples t-test revealed "
  )

  if (p_val < 0.05) {
    result_text <- paste0(
      result_text,
      "a statistically significant difference between groups (t(",
      round(df, 1), ") = ", round(t_stat, 2), ", ", p_text, ")"
    )
  } else {
    result_text <- paste0(
      result_text,
      "no statistically significant difference between groups (t(",
      round(df, 1), ") = ", round(t_stat, 2), ", ", p_text, ")"
    )
  }

  # Add means and SDs
  if (!is.null(sd1) && !is.null(sd2)) {
    result_text <- paste0(
      result_text,
      ". Group 1 (M = ", round(m1, 2), ", SD = ", round(sd1, 2),
      ") ",
      ifelse(m1 > m2, "exceeded", "was lower than"),
      " Group 2 (M = ", round(m2, 2), ", SD = ", round(sd2, 2), ")"
    )
  }

  # Add confidence interval
  if (!is.null(ci_lower) && !is.null(ci_upper)) {
    result_text <- paste0(
      result_text,
      ", 95% CI [", round(ci_lower, 2), ", ", round(ci_upper, 2), "]"
    )
  }

  # Add effect size
  if (!is.null(effect_sizes) && "cohens_d" %in% names(effect_sizes)) {
    d <- effect_sizes$cohens_d
    d_interp <- interpret_cohens_d(d)
    result_text <- paste0(
      result_text,
      ", Cohen's d = ", round(d, 2),
      " (", d_interp, " effect size)"
    )
  }

  result_text <- paste0(result_text, ".")

  result_text
}


#' Generate Welch Results Text
#' @noRd
generate_welch_results <- function(results, effect_sizes = NULL) {
  # Similar to t-test but mention Welch's correction
  text <- generate_ttest_results(results, effect_sizes)
  text <- sub("independent samples t-test", "Welch's t-test", text)
  text
}


#' Generate Paired T-Test Results Text
#' @noRd
generate_paired_t_results <- function(results, effect_sizes = NULL) {

  m_diff <- results$mean_difference
  t_stat <- results$statistic
  df <- results$df
  p_val <- results$p_value

  p_text <- if (p_val < 0.001) {
    "p < .001"
  } else {
    paste0("p = ", format_pvalue(p_val))
  }

  result_text <- paste0(
    "The paired-samples t-test ",
    ifelse(p_val < 0.05, "revealed a statistically significant", "did not reveal a statistically significant"),
    " difference (t(", df, ") = ", round(t_stat, 2), ", ", p_text, ")"
  )

  if (!is.null(m_diff)) {
    result_text <- paste0(
      result_text,
      ", with a mean difference of ", round(m_diff, 2)
    )
  }

  if (!is.null(effect_sizes) && "cohens_d" %in% names(effect_sizes)) {
    d <- effect_sizes$cohens_d
    result_text <- paste0(
      result_text,
      ", Cohen's d = ", round(d, 2)
    )
  }

  paste0(result_text, ".")
}


#' Format P-Value for Publication
#' @noRd
format_pvalue <- function(p) {
  if (p < 0.001) return("< .001")
  if (p >= 0.10) return(sprintf("%.2f", p))
  # Remove leading zero
  formatted <- sprintf("%.3f", p)
  sub("^0", "", formatted)
}


#' Interpret Cohen's d Effect Size
#' @noRd
interpret_cohens_d <- function(d) {
  abs_d <- abs(d)
  if (abs_d < 0.2) return("negligible")
  if (abs_d < 0.5) return("small")
  if (abs_d < 0.8) return("medium")
  return("large")
}


#' Generate ANOVA Methods Text
#' @noRd
generate_anova_methods <- function(results) {
  if ("anova_type" %in% names(results)) {
    if (results$anova_type == "one-way") {
      return(paste0(
        "A one-way analysis of variance (ANOVA) was conducted to examine differences ",
        "across groups. ANOVA assumes normality of residuals, homogeneity of variance ",
        "across groups, and independence of observations."
      ))
    } else if (results$anova_type == "welch") {
      return(paste0(
        "A Welch's one-way ANOVA was conducted to examine differences across groups. ",
        "This robust alternative to standard ANOVA does not assume equal variances."
      ))
    }
  }

  "An analysis of variance (ANOVA) was conducted to examine group differences."
}


#' Generate ANOVA Results Text
#' @noRd
generate_anova_results <- function(results, effect_sizes = NULL) {

  f_stat <- results$statistic
  df1 <- results$df1
  df2 <- results$df2
  p_val <- results$p_value

  p_text <- format_pvalue(p_val)

  result_text <- paste0(
    "The ANOVA ",
    ifelse(p_val < 0.05, "revealed a statistically significant", "did not reveal a statistically significant"),
    " effect (F(", df1, ", ", round(df2, 1), ") = ", round(f_stat, 2),
    ", p = ", p_text, ")"
  )

  if (!is.null(effect_sizes) && "eta_squared" %in% names(effect_sizes)) {
    eta_sq <- effect_sizes$eta_squared
    result_text <- paste0(
      result_text,
      ", eta-squared = ", round(eta_sq, 3)
    )
  }

  paste0(result_text, ".")
}


#' Generate Regression Methods Text
#' @noRd
generate_regression_methods <- function(results) {
  paste0(
    "Linear regression analysis was conducted to examine the relationship between ",
    "the predictor variable(s) and the outcome variable. Regression assumes linearity, ",
    "independence of residuals, homoscedasticity of residuals, and normality of residuals."
  )
}


#' Generate Regression Results Text
#' @noRd
generate_regression_results <- function(results) {

  if ("coefficients" %in% names(results)) {
    # Multiple predictors
    r_sq <- results$r_squared
    f_stat <- results$f_statistic
    p_val <- results$p_value

    result_text <- paste0(
      "The overall regression model ",
      ifelse(p_val < 0.05, "was statistically significant", "was not statistically significant"),
      " (F = ", round(f_stat, 2), ", p = ", format_pvalue(p_val),
      ", R^2 = ", round(r_sq, 3), ")"
    )

    return(paste0(result_text, "."))
  }

  "Regression analysis results should be interpreted in context."
}


#' Generate Correlation Methods Text
#' @noRd
generate_correlation_methods <- function(results) {
  cor_type <- if ("method" %in% names(results)) results$method else "Pearson"

  if (cor_type == "pearson") {
    return(paste0(
      "Pearson's correlation coefficient was calculated to assess the linear ",
      "relationship between variables. This test assumes bivariate normality."
    ))
  } else if (cor_type == "spearman") {
    return(paste0(
      "Spearman's rank correlation coefficient was calculated to assess the ",
      "monotonic relationship between variables. This non-parametric test does ",
      "not assume normality."
    ))
  }

  "Correlation analysis was conducted to examine relationships between variables."
}


#' Generate Correlation Results Text
#' @noRd
generate_correlation_results <- function(results) {

  r <- results$correlation
  p_val <- results$p_value
  n <- results$n

  method <- if ("method" %in% names(results)) results$method else "Pearson"

  result_text <- paste0(
    method, "'s correlation analysis ",
    ifelse(p_val < 0.05, "revealed a statistically significant", "did not reveal a statistically significant"),
    " relationship (r = ", round(r, 2), ", n = ", n,
    ", p = ", format_pvalue(p_val), ")"
  )

  paste0(result_text, ".")
}


#' Print Method for Publication Blocks
#'
#' @param x A publication_block object
#' @param ... Additional arguments (not used)
#'
#' @export
print.publication_block <- function(x, ...) {
  cat("\n")
  cat("=" %+% rep("=", 78) %+% "=\n", sep = "")
  cat("PUBLICATION-READY TEXT\n")
  cat("=" %+% rep("=", 78) %+% "=\n", sep = "")
  cat("\n")

  if (!is.null(x$assumptions)) {
    cat("ASSUMPTIONS:\n")
    cat(strwrap(x$assumptions, width = 80, prefix = "  "), sep = "\n")
    cat("\n\n")
  }

  if (!is.null(x$methods)) {
    cat("METHODS:\n")
    cat(strwrap(x$methods, width = 80, prefix = "  "), sep = "\n")
    cat("\n\n")
  }

  if (!is.null(x$results)) {
    cat("RESULTS:\n")
    cat(strwrap(x$results, width = 80, prefix = "  "), sep = "\n")
    cat("\n\n")
  }

  if (!is.null(x$interpretation)) {
    cat("INTERPRETATION:\n")
    cat(strwrap(x$interpretation, width = 80, prefix = "  "), sep = "\n")
    cat("\n\n")
  }

  if (!is.null(x$additional)) {
    cat("ADDITIONAL NOTES:\n")
    cat(strwrap(x$additional, width = 80, prefix = "  "), sep = "\n")
    cat("\n\n")
  }

  cat("\nUSAGE NOTE:\n")
  cat("  Copy the sections above into your manuscript. Modify as needed for your\n")
  cat("  specific journal's requirements and integrate with your narrative.\n")
  cat("\n")

  invisible(x)
}
