#' Perform One-Way ANOVA with Comprehensive Diagnostics
#'
#' @description
#' Conducts one-way analysis of variance (ANOVA) to compare means across
#' three or more groups. Includes explicit assumption testing, effect sizes,
#' post-hoc tests, and publication-ready output.
#'
#' @param data Numeric vector of values, or a data frame
#' @param groups Factor or vector indicating group membership (if data is a vector)
#' @param formula Formula specifying model (if data is a data frame), e.g., score ~ condition
#' @param check_assumptions Logical. Should assumptions be tested? Default is TRUE
#' @param use_welch Logical. Use Welch's ANOVA (doesn't assume equal variances)? Default is "auto"
#'   which selects based on homogeneity test
#' @param post_hoc Character. Type of post-hoc test. Options: "tukey" (default), "bonferroni", "none"
#' @param alpha Significance level for tests (default 0.05)
#' @param verbose Logical. Show detailed explanations? Default is TRUE
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{anova_type}: Type of ANOVA performed
#'     \item \code{f_statistic}: F-statistic value
#'     \item \code{df1}: Degrees of freedom (numerator)
#'     \item \code{df2}: Degrees of freedom (denominator)
#'     \item \code{p_value}: P-value from the test
#'     \item \code{significant}: Logical, TRUE if p < alpha
#'     \item \code{eta_squared}: Effect size (η²)
#'     \item \code{group_statistics}: Descriptive stats for each group
#'     \item \code{assumptions}: Assumption check results (if requested)
#'     \item \code{post_hoc_results}: Post-hoc test results (if requested)
#'     \item \code{publication_text}: Publication-ready text
#'     \item \code{interpretation}: Plain English interpretation
#'     \item \code{full_output}: Complete ANOVA table
#'   }
#'
#' @details
#' ## ANOVA Assumptions:
#' \itemize{
#'   \item \strong{Independence}: Observations must be independent
#'   \item \strong{Normality}: Residuals should be normally distributed
#'   \item \strong{Homogeneity of variance}: Groups should have similar variances
#' }
#'
#' When homogeneity is violated, Welch's ANOVA is recommended.
#'
#' ## Effect Size Interpretation (η²):
#' \itemize{
#'   \item 0.01 - 0.05: Small effect
#'   \item 0.06 - 0.13: Medium effect
#'   \item 0.14+: Large effect
#' }
#'
#' @examples
#' # Example 1: Vector input
#' satisfaction <- c(5, 6, 7, 8, 7, 6,    # Group A
#'                   3, 4, 5, 4, 3, 4,    # Group B
#'                   7, 8, 9, 8, 9, 8)    # Group C
#' condition <- factor(rep(c("A", "B", "C"), each = 6))
#' result <- compare_groups_anova(satisfaction, condition)
#'
#' # Example 2: Data frame with formula
#' data <- data.frame(
#'   score = c(45, 67, 54, 78, 56, 34, 56, 78, 89, 67),
#'   group = factor(rep(c("Control", "Treatment A", "Treatment B"), c(3, 4, 3)))
#' )
#' result <- compare_groups_anova(data, formula = score ~ group)
#'
#' # View publication text
#' print(result, show_publication = TRUE)
#'
#' @export
compare_groups_anova <- function(data,
                                  groups = NULL,
                                  formula = NULL,
                                  check_assumptions = TRUE,
                                  use_welch = "auto",
                                  post_hoc = "tukey",
                                  alpha = 0.05,
                                  verbose = TRUE) {

  # Step 1: Input processing
  # -------------------------
  if (is.data.frame(data) && !is.null(formula)) {
    # Extract from formula
    model_frame <- model.frame(formula, data = data)
    dv_values <- model_frame[[1]]
    groups_factor <- model_frame[[2]]
    dv_name <- names(model_frame)[1]
    group_name <- names(model_frame)[2]

  } else if (is.numeric(data) && !is.null(groups)) {
    # Vector input
    dv_values <- data
    groups_factor <- as.factor(groups)
    dv_name <- deparse(substitute(data))
    group_name <- deparse(substitute(groups))

  } else {
    stop("Error: Must provide either (data + groups) or (data.frame + formula)")
  }

  # Step 2: Data cleaning
  # ---------------------
  # Remove missing values
  valid_idx <- !is.na(dv_values) & !is.na(groups_factor)
  dv_clean <- dv_values[valid_idx]
  groups_clean <- groups_factor[valid_idx]

  n_missing <- sum(!valid_idx)
  if (n_missing > 0 && verbose) {
    message("Note: Removed ", n_missing, " observations with missing values.")
  }

  # Check we have enough groups and data
  n_groups <- nlevels(groups_clean)
  if (n_groups < 2) {
    stop("Error: Need at least 2 groups for ANOVA (you have ", n_groups, ")")
  }

  if (n_groups == 2) {
    message("Note: You have only 2 groups. Consider using test_group_differences() instead.")
  }

  group_ns <- table(groups_clean)
  if (any(group_ns < 2)) {
    stop("Error: Each group must have at least 2 observations")
  }

  # Step 3: Calculate descriptive statistics
  # -----------------------------------------
  if (verbose) {
    message("\nCalculating descriptive statistics for ", n_groups, " groups...")
  }

  group_stats <- data.frame(
    group = levels(groups_clean),
    n = as.vector(table(groups_clean)),
    mean = tapply(dv_clean, groups_clean, mean),
    sd = tapply(dv_clean, groups_clean, sd),
    min = tapply(dv_clean, groups_clean, min),
    max = tapply(dv_clean, groups_clean, max)
  )
  rownames(group_stats) <- NULL

  # Step 4: Check assumptions
  # --------------------------
  assumptions <- list()

  if (check_assumptions) {
    if (verbose) {
      message("\nChecking ANOVA assumptions...")
    }

    # Independence
    assumptions$independence <- check_independence(
      data_structure = "independent groups ANOVA design",
      is_independent = TRUE,
      clustering_note = paste0(
        "This ANOVA assumes independent observations across groups. ",
        "If you have repeated measures or nested data, use repeated measures ANOVA or mixed models."
      )
    )

    # Fit model to get residuals
    temp_model <- lm(dv_clean ~ groups_clean)
    residuals <- residuals(temp_model)

    # Normality of residuals
    assumptions$normality <- check_normality(
      residuals,
      variable_name = "ANOVA residuals",
      alpha = alpha
    )

    # Homogeneity of variance
    assumptions$homogeneity <- check_homogeneity_of_variance(
      data = dv_clean,
      groups = groups_clean,
      alpha = alpha
    )

    if (verbose) {
      message("Assumption checks complete.\n")
    }
  }

  # Step 5: Determine which ANOVA to use
  # -------------------------------------
  if (use_welch == "auto") {
    # Decide based on homogeneity test
    if (check_assumptions && !is.null(assumptions$homogeneity)) {
      use_welch <- !assumptions$homogeneity$assumption_met
    } else {
      use_welch <- FALSE
    }
  }

  # Step 6: Perform ANOVA
  # ---------------------
  if (use_welch) {
    # Welch's ANOVA (doesn't assume equal variances)
    if (verbose) {
      message("Performing Welch's one-way ANOVA (robust to unequal variances)...")
    }

    welch_result <- oneway.test(dv_clean ~ groups_clean, var.equal = FALSE)

    f_stat <- welch_result$statistic
    df1 <- welch_result$parameter[1]
    df2 <- welch_result$parameter[2]
    p_value <- welch_result$p.value

    anova_type <- "Welch's One-Way ANOVA"
    full_output <- welch_result

    # Calculate eta-squared (approximate for Welch's)
    grand_mean <- mean(dv_clean)
    ss_between <- sum(group_ns * (group_stats$mean - grand_mean)^2)
    ss_total <- sum((dv_clean - grand_mean)^2)
    eta_squared <- ss_between / ss_total

  } else {
    # Standard one-way ANOVA
    if (verbose) {
      message("Performing standard one-way ANOVA...")
    }

    aov_model <- aov(dv_clean ~ groups_clean)
    anova_table <- anova(aov_model)

    f_stat <- anova_table$`F value`[1]
    df1 <- anova_table$Df[1]
    df2 <- anova_table$Df[2]
    p_value <- anova_table$`Pr(>F)`[1]

    anova_type <- "One-Way ANOVA"
    full_output <- anova_table

    # Calculate eta-squared
    ss_between <- anova_table$`Sum Sq`[1]
    ss_total <- sum(anova_table$`Sum Sq`)
    eta_squared <- ss_between / ss_total
  }

  is_significant <- p_value < alpha

  # Interpret effect size
  if (eta_squared < 0.01) {
    effect_interp <- "negligible"
  } else if (eta_squared < 0.06) {
    effect_interp <- "small"
  } else if (eta_squared < 0.14) {
    effect_interp <- "medium"
  } else {
    effect_interp <- "large"
  }

  # Step 7: Post-hoc tests
  # ----------------------
  post_hoc_results <- NULL

  if (is_significant && post_hoc != "none" && n_groups > 2) {
    if (verbose) {
      message("Performing post-hoc tests (", post_hoc, ")...")
    }

    if (post_hoc == "tukey" && !use_welch) {
      # Tukey HSD (requires equal variances)
      tukey_result <- TukeyHSD(aov_model)
      post_hoc_results <- as.data.frame(tukey_result$groups_clean)
      post_hoc_results$comparison <- rownames(post_hoc_results)
      rownames(post_hoc_results) <- NULL
      post_hoc_results <- post_hoc_results[, c("comparison", "diff", "lwr", "upr", "p adj")]
      names(post_hoc_results) <- c("comparison", "mean_diff", "ci_lower", "ci_upper", "p_adj")

    } else if (post_hoc == "bonferroni" || use_welch) {
      # Bonferroni correction (works with Welch's too)
      bonf_result <- pairwise.t.test(
        dv_clean,
        groups_clean,
        p.adjust.method = "bonferroni",
        pool.sd = !use_welch
      )

      # Convert to data frame
      comparisons <- which(!is.na(bonf_result$p.value), arr.ind = TRUE)
      post_hoc_results <- data.frame(
        comparison = paste(
          rownames(bonf_result$p.value)[comparisons[, 1]],
          "vs",
          colnames(bonf_result$p.value)[comparisons[, 2]]
        ),
        p_adj = bonf_result$p.value[comparisons],
        significant = bonf_result$p.value[comparisons] < alpha
      )
    }
  }

  # Step 8: Generate interpretation
  # --------------------------------
  interpretation <- paste0(
    "The ", anova_type, " ",
    ifelse(is_significant, "revealed a statistically significant", "did not reveal a statistically significant"),
    " difference among groups (F(", df1, ", ", round(df2, 1), ") = ", round(f_stat, 2),
    ", p = ", round(p_value, 4), "). ",
    "The effect size is ", effect_interp, " (η² = ", round(eta_squared, 3), "). "
  )

  if (is_significant && !is.null(post_hoc_results)) {
    n_sig_pairs <- sum(post_hoc_results$p_adj < alpha, na.rm = TRUE)
    interpretation <- paste0(
      interpretation,
      "Post-hoc tests revealed ", n_sig_pairs, " significant pairwise difference(s). ",
      "See post_hoc_results for details."
    )
  } else if (is_significant && n_groups == 2) {
    interpretation <- paste0(
      interpretation,
      "With only 2 groups, the direction of difference is: ",
      group_stats$group[which.max(group_stats$mean)], " (M = ",
      round(max(group_stats$mean), 2), ") > ",
      group_stats$group[which.min(group_stats$mean)], " (M = ",
      round(min(group_stats$mean), 2), ")."
    )
  }

  # Step 9: Generate publication text
  # ----------------------------------
  pub_block <- generate_publication_block(
    test_type = "anova",
    assumptions_checks = if (check_assumptions) assumptions else NULL,
    test_results = list(
      anova_type = anova_type,
      statistic = f_stat,
      df1 = df1,
      df2 = df2,
      p_value = p_value,
      eta_squared = eta_squared,
      group_statistics = group_stats
    ),
    effect_sizes = list(eta_squared = eta_squared),
    additional_notes = if (!is.null(post_hoc_results)) {
      paste0(
        "Post-hoc comparisons were conducted using ",
        ifelse(post_hoc == "tukey", "Tukey's HSD test", "Bonferroni correction"),
        " to control for Type I error inflation."
      )
    } else {
      NULL
    }
  )

  # Step 10: Package results
  # ------------------------
  results <- list(
    anova_type = anova_type,
    f_statistic = round(f_stat, 3),
    df1 = df1,
    df2 = round(df2, 1),
    p_value = round(p_value, 6),
    significant = is_significant,
    alpha = alpha,
    eta_squared = round(eta_squared, 3),
    effect_interpretation = effect_interp,
    n_groups = n_groups,
    total_n = length(dv_clean),
    group_statistics = group_stats,
    assumptions = if (check_assumptions && length(assumptions) > 0) assumptions else NULL,
    post_hoc_results = post_hoc_results,
    interpretation = interpretation,
    publication_text = pub_block,
    full_output = full_output
  )

  class(results) <- c("anova_result", "list")

  if (verbose) {
    message("ANOVA complete!\n")
  }

  return(results)
}


#' Print Method for ANOVA Results
#'
#' @param x An anova_result object
#' @param show_assumptions Logical - show assumption checks? (default FALSE)
#' @param show_posthoc Logical - show post-hoc results? (default TRUE if available)
#' @param show_publication Logical - show publication text? (default FALSE)
#' @param ... Additional arguments (not used)
#'
#' @export
print.anova_result <- function(x, show_assumptions = FALSE, show_posthoc = TRUE, show_publication = FALSE, ...) {
  cat("\n")
  cat("=" %+% rep("=", 75) %+% "=\n", sep = "")
  cat(toupper(x$anova_type), " RESULTS\n", sep = "")
  cat("=" %+% rep("=", 75) %+% "=\n", sep = "")
  cat("\n")

  cat("OVERVIEW:\n")
  cat("  Number of groups:", x$n_groups, "\n")
  cat("  Total sample size:", x$total_n, "\n")
  cat("\n")

  cat("DESCRIPTIVE STATISTICS BY GROUP:\n")
  print(x$group_statistics, row.names = FALSE)
  cat("\n")

  cat("ANOVA RESULTS:\n")
  cat("  F(", x$df1, ", ", x$df2, ") = ", x$f_statistic, "\n", sep = "")
  cat("  p-value:", x$p_value, "\n")
  cat("  Significant:", ifelse(x$significant, "YES (p < .05)", "NO (p >= .05)"), "\n")
  cat("\n")

  cat("EFFECT SIZE:\n")
  cat("  η² =", x$eta_squared, "(", x$effect_interpretation, ")\n")
  cat("  Interpretation: This effect accounts for", round(x$eta_squared * 100, 1), "% of variance\n")
  cat("\n")

  if (show_posthoc && !is.null(x$post_hoc_results)) {
    cat("POST-HOC TESTS:\n")
    print(x$post_hoc_results, row.names = FALSE)
    cat("\n")
  }

  cat("INTERPRETATION:\n")
  cat(strwrap(x$interpretation, width = 75, prefix = "  "), sep = "\n")
  cat("\n")

  if (show_assumptions && !is.null(x$assumptions)) {
    cat("\n")
    cat("-" %+% rep("-", 75) %+% "-\n", sep = "")
    cat("ASSUMPTION CHECKS:\n")
    cat("-" %+% rep("-", 75) %+% "-\n", sep = "")
    for (assump_name in names(x$assumptions)) {
      cat("\n", toupper(gsub("_", " ", assump_name)), ":\n", sep = "")
      print(x$assumptions[[assump_name]], verbose = FALSE)
    }
  }

  if (show_publication && !is.null(x$publication_text)) {
    cat("\n")
    print(x$publication_text)
  }

  # Tips
  if (!show_assumptions && !is.null(x$assumptions)) {
    cat("\nTip: Use print(result, show_assumptions = TRUE) to see assumption checks\n")
  }
  if (!show_posthoc && !is.null(x$post_hoc_results)) {
    cat("Tip: Use print(result, show_posthoc = TRUE) to see post-hoc tests\n")
  }
  if (!show_publication) {
    cat("Tip: Use print(result, show_publication = TRUE) to see publication-ready text\n")
  }

  cat("\n")

  invisible(x)
}
