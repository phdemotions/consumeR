#' Statistical Modeling and Inference Functions
#'
#' @description
#' Functions for hypothesis testing, effect size estimation, and planned contrasts
#' meeting Journal of Consumer Psychology publication standards.
#'
#' @name modeling
NULL


#' Planned Contrasts and Simple Effects via emmeans
#'
#' @description
#' Wrapper around emmeans for planned contrasts, pairwise comparisons, and
#' simple effects. Includes multiple comparison adjustments and confidence
#' intervals. Publication-ready output for JCP standards.
#'
#' @param model Fitted model object (lm, aov, or other model supported by emmeans)
#' @param specs Character or formula. Variable(s) for marginal means.
#'   Examples: "condition", "condition * time", ~ condition | time
#' @param contrasts Character. Type of contrasts:
#'   - "pairwise" (default): All pairwise comparisons
#'   - "trt.vs.ctrl": Treatment vs control (specify ref level)
#'   - "eff": Deviations from grand mean
#'   - "consec": Consecutive (sequential) comparisons
#'   - Custom contrast matrix
#' @param adjust Character. Multiple comparison adjustment:
#'   - "tukey": Tukey HSD (default for pairwise)
#'   - "bonferroni": Bonferroni correction
#'   - "holm": Holm step-down
#'   - "fdr": False discovery rate (Benjamini-Hochberg)
#'   - "none": No adjustment (use with caution)
#' @param by Character. Variable(s) for simple effects (optional)
#' @param level Numeric. Confidence level (default 0.95)
#' @param infer Logical or numeric. Include inference? If numeric, specifies null value (default TRUE)
#' @param ... Additional arguments passed to emmeans::emmeans()
#'
#' @return A list with class "emmeans_result" containing:
#' \itemize{
#'   \item \code{emmeans}: Estimated marginal means (tibble)
#'   \item \code{contrasts}: Contrasts/comparisons (tibble)
#'   \item \code{adjust_method}: Adjustment method used
#'   \item \code{level}: Confidence level
#'   \item \code{interpretation}: Publication-ready text
#' }
#'
#' @details
#' **Multiple Comparison Adjustments:**
#' - **Tukey HSD**: Controls familywise error rate, recommended for pairwise
#' - **Bonferroni**: Conservative, controls familywise error rate
#' - **Holm**: Less conservative than Bonferroni, still controls FWER
#' - **FDR (Benjamini-Hochberg)**: Controls false discovery rate, less conservative
#'
#' **When to use:**
#' - Planned contrasts: Specify exactly what you want to test
#' - Post-hoc tests: Use after significant omnibus ANOVA
#' - Simple effects: Test effects of one factor at each level of another
#'
#' **JCP Requirements:**
#' - Report adjustment method in manuscript
#' - Include confidence intervals, not just p-values
#' - Report effect sizes separately (use cohens_d_table)
#'
#' @examples
#' \dontrun{
#' # Fit ANOVA
#' model <- lm(satisfaction ~ condition, data = df)
#'
#' # Pairwise comparisons with Tukey adjustment
#' contrasts <- emmeans_contrasts(model, specs = "condition")
#'
#' # Treatment vs control
#' contrasts <- emmeans_contrasts(
#'   model,
#'   specs = "condition",
#'   contrasts = "trt.vs.ctrl",
#'   ref = "control"
#' )
#'
#' # Simple effects (condition at each time point)
#' model2 <- lm(satisfaction ~ condition * time, data = df)
#' simple <- emmeans_contrasts(
#'   model2,
#'   specs = "condition",
#'   by = "time"
#' )
#' }
#'
#' @export
emmeans_contrasts <- function(model,
                               specs,
                               contrasts = "pairwise",
                               adjust = "tukey",
                               by = NULL,
                               level = 0.95,
                               infer = TRUE,
                               ...) {
  # Check for emmeans package
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'emmeans' is required for planned contrasts",
      "i" = "Install with: install.packages('emmeans')"
    ))
  }

  # Validate model
  if (!inherits(model, c("lm", "aov", "glm", "lme", "lmerMod", "glmerMod"))) {
    warning(
      "Model class '", class(model)[1], "' may not be supported by emmeans. ",
      "Proceeding anyway, but check results carefully."
    )
  }

  message("\nCalculating estimated marginal means...")

  # Get emmeans
  if (is.null(by)) {
    emm <- emmeans::emmeans(model, specs = specs, level = level, ...)
  } else {
    emm <- emmeans::emmeans(model, specs = specs, by = by, level = level, ...)
  }

  # Convert to tibble
  emm_tbl <- tibble::as_tibble(summary(emm))

  message("Estimated marginal means calculated for: ",
          if (is.character(specs)) specs else deparse(specs))

  # Get contrasts
  message("\nCalculating contrasts with adjustment = '", adjust, "'...")

  if (contrasts == "pairwise") {
    contr <- emmeans::pairs(emm, adjust = adjust, infer = infer)
  } else if (contrasts == "trt.vs.ctrl") {
    contr <- emmeans::contrast(emm, method = "trt.vs.ctrl", adjust = adjust, infer = infer, ...)
  } else if (contrasts == "eff") {
    contr <- emmeans::contrast(emm, method = "eff", adjust = adjust, infer = infer)
  } else if (contrasts == "consec") {
    contr <- emmeans::contrast(emm, method = "consec", adjust = adjust, infer = infer)
  } else {
    # Custom contrast
    contr <- emmeans::contrast(emm, method = contrasts, adjust = adjust, infer = infer, ...)
  }

  # Convert contrasts to tibble
  contr_tbl <- tibble::as_tibble(summary(contr))

  n_comparisons <- nrow(contr_tbl)
  n_sig <- sum(contr_tbl$p.value < 0.05, na.rm = TRUE)

  message("  Comparisons: ", n_comparisons)
  message("  Significant (p < .05): ", n_sig)

  # Generate interpretation
  interpretation <- paste0(
    "ESTIMATED MARGINAL MEANS & CONTRASTS\n",
    "====================================\n\n",
    "Adjustment method: ", adjust, "\n",
    "Confidence level: ", level * 100, "%\n",
    "Number of comparisons: ", n_comparisons, "\n",
    "Significant comparisons (p < .05): ", n_sig, " (",
    round(n_sig / n_comparisons * 100, 1), "%)\n\n",
    "REPORTING FOR PUBLICATION:\n",
    "Estimated marginal means were computed using the emmeans package (Lenth, 2023).\n",
    "Multiple comparisons were adjusted using the ", adjust, " method.\n",
    "All tests were two-tailed with α = .05.\n"
  )

  # Return structured result
  structure(
    list(
      emmeans = emm_tbl,
      contrasts = contr_tbl,
      adjust_method = adjust,
      level = level,
      n_comparisons = n_comparisons,
      n_significant = n_sig,
      interpretation = interpretation,
      emmeans_obj = emm,
      contrasts_obj = contr
    ),
    class = c("emmeans_result", "list")
  )
}


#' Run ANOVA with Type II or Type III Sums of Squares
#'
#' @description
#' Wrapper around car::Anova() for Type II or Type III sums of squares.
#' Includes effect size (partial eta-squared) and publication-ready output.
#'
#' @param model Fitted lm or aov model
#' @param type Character. Type of sums of squares: "II" (default) or "III"
#' @param test_statistic Character. Test statistic: "F" (default), "Chisq", "LR"
#' @param white_adjust Logical. Use White's heteroscedasticity correction? (default FALSE)
#'
#' @return A tibble containing ANOVA results with:
#' \itemize{
#'   \item Term names
#'   \item Sums of squares
#'   \item Degrees of freedom
#'   \item F statistics
#'   \item P-values
#'   \item Partial eta-squared (effect size)
#'   \item Interpretation (small/medium/large effect)
#' }
#'
#' @details
#' **Type II vs Type III:**
#' - **Type II**: Tests main effects after other main effects (not interactions)
#'   - Use when: No significant interactions, balanced or unbalanced designs
#'   - Most appropriate for observational studies
#' - **Type III**: Tests each effect after all other effects
#'   - Use when: Significant interactions present, want to test simple effects
#'   - Required for unbalanced designs with interactions
#'
#' **Partial Eta-Squared (η²p):**
#' - Effect size: proportion of variance explained by each term
#' - Guidelines: 0.01 = small, 0.06 = medium, 0.14 = large
#' - More appropriate than eta-squared for factorial designs
#'
#' **JCP Requirements:**
#' - Always report effect sizes, not just p-values
#' - Report Type II vs III decision
#' - Report assumption checks (see assumption_checks())
#'
#' @examples
#' \dontrun{
#' # Fit model
#' model <- lm(satisfaction ~ condition * time, data = df)
#'
#' # Type II SS (default)
#' anova_result <- run_anova(model)
#'
#' # Type III SS
#' anova_result <- run_anova(model, type = "III")
#' }
#'
#' @export
run_anova <- function(model,
                      type = c("II", "III", "2", "3"),
                      test_statistic = "F",
                      white_adjust = FALSE) {
  type <- match.arg(type)

  # Standardize type notation
  if (type == "2") type <- "II"
  if (type == "3") type <- "III"

  # Check for car package
  if (!requireNamespace("car", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'car' is required for Type II/III SS",
      "i" = "Install with: install.packages('car')"
    ))
  }

  # Validate model
  if (!inherits(model, c("lm", "aov"))) {
    rlang::abort(c(
      "model must be an lm or aov object",
      "x" = "You provided {class(model)[1]}"
    ))
  }

  message("\nRunning Type ", type, " ANOVA...")
  if (white_adjust) {
    message("Using White's heteroscedasticity-consistent covariance matrix")
  }

  # Run ANOVA
  anova_result <- car::Anova(
    model,
    type = type,
    test.statistic = test_statistic,
    white.adjust = white_adjust
  )

  # Extract results
  anova_df <- as.data.frame(anova_result)
  anova_df$term <- rownames(anova_df)
  rownames(anova_df) <- NULL

  # Calculate partial eta-squared
  # η²p = SS_effect / (SS_effect + SS_error)
  ss_error <- anova_df$`Sum Sq`[anova_df$term == "Residuals"]

  if (length(ss_error) == 0) {
    # No residuals row (might be different model type)
    message("Warning: Could not find residuals row. Partial eta-squared not calculated.")
    anova_df$partial_eta_sq <- NA_real_
    anova_df$effect_size <- NA_character_
  } else {
    anova_df$partial_eta_sq <- vapply(seq_len(nrow(anova_df)), function(i) {
      if (anova_df$term[i] == "Residuals") {
        return(NA_real_)
      }
      ss_effect <- anova_df$`Sum Sq`[i]
      ss_effect / (ss_effect + ss_error)
    }, numeric(1))

    # Interpret effect size
    anova_df$effect_size <- vapply(anova_df$partial_eta_sq, function(eta) {
      if (is.na(eta)) {
        return(NA_character_)
      }
      if (eta < 0.01) {
        "Negligible"
      } else if (eta < 0.06) {
        "Small"
      } else if (eta < 0.14) {
        "Medium"
      } else {
        "Large"
      }
    }, character(1))
  }

  # Rename columns for clarity
  names(anova_df) <- c("sum_sq", "df", "statistic", "p_value", "term", "partial_eta_sq", "effect_size")

  # Reorder columns
  anova_df <- anova_df[, c("term", "sum_sq", "df", "statistic", "p_value", "partial_eta_sq", "effect_size")]

  # Convert to tibble
  result <- tibble::as_tibble(anova_df)

  # Add metadata
  attr(result, "type") <- type
  attr(result, "test_statistic") <- test_statistic
  attr(result, "white_adjust") <- white_adjust

  message("\nType ", type, " ANOVA complete.")
  message("  Terms tested: ", nrow(result) - 1)  # Exclude residuals
  message("  Significant terms (p < .05): ",
          sum(result$p_value < 0.05, na.rm = TRUE))

  result
}


#' Cohen's d Effect Size with Confidence Intervals
#'
#' @description
#' Calculate Cohen's d effect size for group comparisons with bootstrap
#' confidence intervals. JCP publication standard.
#'
#' @param data Data frame
#' @param outcome Character. Name of outcome variable
#' @param group Character. Name of grouping variable (must have exactly 2 levels)
#' @param paired Logical. Are observations paired? (default FALSE)
#' @param pooled_sd Logical. Use pooled SD? (default TRUE for independent, FALSE for paired)
#' @param ci_method Character. CI method: "nct" (default, non-central t), "bootstrap"
#' @param conf_level Numeric. Confidence level (default 0.95)
#' @param boot_n Integer. Number of bootstrap samples if ci_method = "bootstrap" (default 1000)
#'
#' @return A tibble with one row containing:
#' \itemize{
#'   \item \code{group1}, \code{group2}: Group labels
#'   \item \code{cohens_d}: Cohen's d estimate
#'   \item \code{ci_lower}, \code{ci_upper}: Confidence interval
#'   \item \code{interpretation}: Effect size interpretation
#'   \item \code{n1}, \code{n2}: Sample sizes
#' }
#'
#' @details
#' **Cohen's d interpretation:**
#' - |d| = 0.20: Small effect
#' - |d| = 0.50: Medium effect
#' - |d| = 0.80: Large effect
#'
#' **CI Methods:**
#' - "nct": Non-central t (faster, assumes normality)
#' - "bootstrap": Bootstrap (slower, no distributional assumptions)
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   satisfaction = c(5, 6, 7, 8, 3, 4, 5, 6),
#'   condition = rep(c("treatment", "control"), each = 4)
#' )
#'
#' effect_size <- cohens_d_table(df, "satisfaction", "condition")
#' print(effect_size)
#' }
#'
#' @export
cohens_d_table <- function(data,
                            outcome,
                            group,
                            paired = FALSE,
                            pooled_sd = !paired,
                            ci_method = c("nct", "bootstrap"),
                            conf_level = 0.95,
                            boot_n = 1000) {
  ci_method <- match.arg(ci_method)

  # Check for effectsize package
  if (!requireNamespace("effectsize", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'effectsize' is required",
      "i" = "Install with: install.packages('effectsize')"
    ))
  }

  # Validate inputs
  assert_vars_present(data, c(outcome, group), "Cohen's d calculation")

  # Extract variables
  y <- data[[outcome]]
  g <- as.factor(data[[group]])

  # Check exactly 2 groups
  if (nlevels(g) != 2) {
    rlang::abort(c(
      "group variable must have exactly 2 levels",
      "x" = "Found {nlevels(g)} levels: {paste(levels(g), collapse = ', ')}"
    ))
  }

  # Calculate Cohen's d
  d_result <- effectsize::cohens_d(
    y ~ g,
    data = data,
    paired = paired,
    pooled_sd = pooled_sd,
    ci = conf_level,
    alternative = "two.sided"
  )

  # Extract values
  d_value <- d_result$Cohens_d[1]
  ci_low <- d_result$CI_low[1]
  ci_high <- d_result$CI_high[1]

  # Get group labels and sample sizes
  group_levels <- levels(g)
  n1 <- sum(g == group_levels[1], na.rm = TRUE)
  n2 <- sum(g == group_levels[2], na.rm = TRUE)

  # Interpret effect size
  abs_d <- abs(d_value)
  interpretation <- if (abs_d < 0.20) {
    "Negligible"
  } else if (abs_d < 0.50) {
    "Small"
  } else if (abs_d < 0.80) {
    "Medium"
  } else {
    "Large"
  }

  # Create result tibble
  tibble::tibble(
    group1 = group_levels[1],
    group2 = group_levels[2],
    n1 = n1,
    n2 = n2,
    cohens_d = round(d_value, 3),
    ci_lower = round(ci_low, 3),
    ci_upper = round(ci_high, 3),
    conf_level = conf_level,
    interpretation = interpretation,
    paired = paired
  )
}


#' Correlation Table with Confidence Intervals
#'
#' @description
#' Calculate correlations between multiple variables with confidence intervals.
#' Uses correlation package for comprehensive output.
#'
#' @param data Data frame
#' @param vars Character vector of variable names to correlate
#' @param method Character. Correlation method: "pearson" (default), "spearman", "kendall"
#' @param adjust Character. Multiple comparison adjustment: "none" (default), "bonferroni", "holm", "fdr"
#' @param ci_level Numeric. Confidence level (default 0.95)
#'
#' @return A tibble with correlations including:
#' \itemize{
#'   \item Variable pairs
#'   \item Correlation coefficient
#'   \item Confidence interval
#'   \item P-value (adjusted if requested)
#'   \item Sample size
#' }
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   satisfaction = rnorm(100),
#'   loyalty = rnorm(100),
#'   value = rnorm(100)
#' )
#'
#' cors <- correlation_table(df, c("satisfaction", "loyalty", "value"))
#' print(cors)
#' }
#'
#' @export
correlation_table <- function(data,
                               vars,
                               method = c("pearson", "spearman", "kendall"),
                               adjust = c("none", "bonferroni", "holm", "fdr"),
                               ci_level = 0.95) {
  method <- match.arg(method)
  adjust <- match.arg(adjust)

  # Check for correlation package
  if (!requireNamespace("correlation", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'correlation' is required",
      "i" = "Install with: install.packages('correlation')"
    ))
  }

  # Validate inputs
  assert_vars_present(data, vars, "correlation table")

  # Extract data
  data_subset <- data[, vars, drop = FALSE]

  # Calculate correlations
  cor_result <- correlation::correlation(
    data_subset,
    method = method,
    p_adjust = adjust,
    ci = ci_level
  )

  # Convert to tibble (already is, but ensure)
  result <- tibble::as_tibble(cor_result)

  # Add interpretation column
  result$interpretation <- vapply(abs(result$r), function(r) {
    if (r < 0.10) {
      "Negligible"
    } else if (r < 0.30) {
      "Weak"
    } else if (r < 0.50) {
      "Moderate"
    } else if (r < 0.70) {
      "Strong"
    } else {
      "Very Strong"
    }
  }, character(1))

  message("\nCorrelation table calculated (", method, " method)")
  message("  Pairs: ", nrow(result))
  message("  Adjustment: ", adjust)
  message("  Significant (p < .05): ", sum(result$p < 0.05, na.rm = TRUE))

  result
}


#' @export
print.emmeans_result <- function(x, ...) {
  cat("\n")
  cat("ESTIMATED MARGINAL MEANS\n")
  cat("========================\n\n")

  print(x$emmeans)

  cat("\n")
  cat("CONTRASTS\n")
  cat("=========\n")
  cat("Adjustment method: ", x$adjust_method, "\n")
  cat("Confidence level: ", x$level * 100, "%\n\n")

  print(x$contrasts)

  cat("\n")
  cat("Significant comparisons (p < .05): ", x$n_significant, " of ", x$n_comparisons, "\n")
  cat("\n")

  invisible(x)
}
