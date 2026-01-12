#' Categorical Data Analysis Functions
#'
#' @description
#' Functions for analyzing categorical variables including chi-square tests,
#' Fisher's exact test, McNemar's test, and odds ratios. JCP publication quality.
#'
#' @name categorical_tests
NULL


#' Chi-Square Test of Independence with Effect Size
#'
#' @description
#' Conducts chi-square test of independence for contingency tables with
#' Cramer's V effect size and standardized residuals. Publication-ready
#' output for JCP standards.
#'
#' @param data Data frame (optional if x and y are vectors)
#' @param x Character (column name) or vector (first categorical variable)
#' @param y Character (column name) or vector (second categorical variable)
#' @param correct Logical. Apply Yates' continuity correction for 2x2 tables? (default TRUE)
#' @param simulate_p Logical. Use Monte Carlo simulation for p-value? (default FALSE)
#' @param B Integer. Number of Monte Carlo replicates if simulate_p = TRUE (default 2000)
#'
#' @return A list with class "chisq_result" containing:
#' \itemize{
#'   \item \code{statistic}: Chi-square statistic
#'   \item \code{df}: Degrees of freedom
#'   \item \code{p_value}: P-value
#'   \item \code{cramers_v}: Cramer's V effect size
#'   \item \code{effect_interp}: Effect size interpretation
#'   \item \code{n}: Total sample size
#'   \item \code{observed}: Observed frequencies table
#'   \item \code{expected}: Expected frequencies table
#'   \item \code{residuals}: Standardized residuals
#'   \item \code{cells_low_expected}: Cells with expected count < 5
#'   \item \code{use_fisher}: Recommendation to use Fisher's exact if needed
#'   \item \code{interpretation}: Publication-ready text
#'   \item \code{publication_text}: APA-style results statement
#' }
#'
#' @details
#' **Cramer's V Effect Size:**
#' - For 2x2 tables: V = sqrt(chi^2/n)
#' - For larger tables: V = sqrt(chi^2/(n x min(rows-1, cols-1)))
#' - Interpretation: 0.10 = small, 0.30 = medium, 0.50 = large
#'
#' **Assumptions:**
#' - Expected frequency >= 5 in all cells (for 2x2)
#' - Expected frequency >= 5 in 80% of cells (for larger tables)
#' - If violated, use Fisher's exact test instead
#'
#' **Yates' Correction:**
#' - Applied by default for 2x2 tables
#' - Makes test more conservative (reduces Type I error)
#' - Can turn off with correct = FALSE
#'
#' @examples
#' \dontrun{
#' # From data frame
#' result <- chisq_test(
#'   data = customer_data,
#'   x = "condition",
#'   y = "purchased"
#' )
#'
#' # From vectors
#' condition <- c(rep("A", 50), rep("B", 50))
#' purchased <- c(rep(c("yes", "no"), each = 25),
#'                rep(c("yes", "no"), c(35, 15)))
#' result <- chisq_test(x = condition, y = purchased)
#'
#' print(result)
#' }
#'
#' @export
chisq_test <- function(data = NULL,
                       x,
                       y,
                       correct = TRUE,
                       simulate_p = FALSE,
                       B = 2000) {
  # Extract variables
  if (is.null(data)) {
    # x and y should be vectors
    if (!is.vector(x) || !is.vector(y)) {
      rlang::abort("If data is NULL, x and y must be vectors")
    }
    x_var <- x
    y_var <- y
    x_name <- deparse(substitute(x))
    y_name <- deparse(substitute(y))
  } else {
    # x and y should be column names
    if (!is.character(x) || !is.character(y)) {
      rlang::abort("If data is provided, x and y must be column names (characters)")
    }
    assert_vars_present(data, c(x, y), "chi-square test")
    x_var <- data[[x]]
    y_var <- data[[y]]
    x_name <- x
    y_name <- y
  }

  # Remove missing
  valid_idx <- !is.na(x_var) & !is.na(y_var)
  x_clean <- x_var[valid_idx]
  y_clean <- y_var[valid_idx]

  n_missing <- sum(!valid_idx)
  if (n_missing > 0) {
    message("Removed ", n_missing, " observations with missing values")
  }

  # Create contingency table
  obs_table <- table(x_clean, y_clean)

  n_rows <- nrow(obs_table)
  n_cols <- ncol(obs_table)
  n_total <- sum(obs_table)

  message("\nChi-square test of independence")
  message("  Table size: ", n_rows, " x ", n_cols)
  message("  Total n: ", n_total)

  # Run chi-square test
  chisq_result <- stats::chisq.test(
    x_clean,
    y_clean,
    correct = correct,
    simulate.p.value = simulate_p,
    B = B
  )

  # Extract results
  chisq_stat <- chisq_result$statistic
  df <- chisq_result$parameter
  p_value <- chisq_result$p.value
  exp_table <- chisq_result$expected

  # Check assumption (expected frequencies)
  cells_low <- sum(exp_table < 5)
  pct_low <- (cells_low / length(exp_table)) * 100

  use_fisher <- FALSE
  if (n_rows == 2 && n_cols == 2 && cells_low > 0) {
    warning("Expected frequency < 5 in some cells. Consider using fisher_exact_test() instead.")
    use_fisher <- TRUE
  } else if (pct_low > 20) {
    warning("Expected frequency < 5 in >20% of cells. Consider using fisher_exact_test() or combining categories.")
    use_fisher <- TRUE
  }

  # Calculate Cramer's V effect size
  # V = sqrt(chi^2 / (n * min(rows-1, cols-1)))
  min_dim <- min(n_rows - 1, n_cols - 1)
  cramers_v <- sqrt(chisq_stat / (n_total * min_dim))

  # Interpret effect size (Cohen's guidelines)
  if (min_dim == 1) {
    # 2x2 table
    if (cramers_v < 0.10) {
      effect_interp <- "Negligible"
    } else if (cramers_v < 0.30) {
      effect_interp <- "Small"
    } else if (cramers_v < 0.50) {
      effect_interp <- "Medium"
    } else {
      effect_interp <- "Large"
    }
  } else {
    # Larger tables (more conservative thresholds)
    if (cramers_v < 0.07) {
      effect_interp <- "Negligible"
    } else if (cramers_v < 0.21) {
      effect_interp <- "Small"
    } else if (cramers_v < 0.35) {
      effect_interp <- "Medium"
    } else {
      effect_interp <- "Large"
    }
  }

  # Standardized residuals (for interpretation)
  std_residuals <- chisq_result$residuals

  # Generate interpretation
  interpretation <- paste0(
    "CHI-SQUARE TEST OF INDEPENDENCE\n",
    "================================\n\n",
    "Variables: ", x_name, " x ", y_name, "\n",
    "Table: ", n_rows, " rows x ", n_cols, " columns\n",
    "Sample size: N = ", n_total, "\n\n",
    "RESULTS:\n",
    "chi^2(", df, ") = ", round(chisq_stat, 2),
    ", p ", if (p_value < 0.001) "< .001" else paste("=", round(p_value, 3)), "\n",
    "Cramer's V = ", round(cramers_v, 3), " (", effect_interp, " effect)\n"
  )

  if (correct && n_rows == 2 && n_cols == 2) {
    interpretation <- paste0(interpretation, "\n",
                             "Note: Yates' continuity correction applied.\n")
  }

  if (use_fisher) {
    interpretation <- paste0(interpretation, "\n",
                             "WARNING: WARNING: Expected frequencies assumption violated.\n",
                             "Consider using fisher_exact_test() instead.\n")
  }

  interpretation <- paste0(interpretation, "\n",
    "INTERPRETATION:\n",
    if (p_value < 0.05) {
      paste0("There is a significant association between ", x_name, " and ", y_name, ".\n",
             "The variables are NOT independent (p < .05).\n")
    } else {
      paste0("No significant association detected between ", x_name, " and ", y_name, ".\n",
             "Cannot reject independence hypothesis (p >= .05).\n")
    }
  )

  # Publication text
  pub_text <- paste0(
    "A chi-square test of independence was conducted to examine the ",
    "association between ", x_name, " and ", y_name, ". ",
    "The association was ",
    if (p_value < 0.05) "significant" else "not significant",
    ", chi^2(", df, ") = ", round(chisq_stat, 2),
    ", p ", if (p_value < 0.001) "< .001" else paste("=", round(p_value, 3)),
    ", Cramer's V = ", round(cramers_v, 3),
    ", indicating a ", tolower(effect_interp), " effect size."
  )

  message("\n", interpretation)

  # Return structured result
  structure(
    list(
      statistic = as.numeric(chisq_stat),
      df = as.numeric(df),
      p_value = p_value,
      cramers_v = cramers_v,
      effect_interp = effect_interp,
      n = n_total,
      observed = obs_table,
      expected = exp_table,
      residuals = std_residuals,
      cells_low_expected = cells_low,
      use_fisher = use_fisher,
      interpretation = interpretation,
      publication_text = pub_text
    ),
    class = c("chisq_result", "list")
  )
}


#' Fisher's Exact Test for Small Samples
#'
#' @description
#' Conducts Fisher's exact test for 2x2 (and larger) contingency tables.
#' Appropriate when chi-square assumptions are violated (expected frequencies < 5).
#'
#' @param data Data frame (optional)
#' @param x Character or vector (first categorical variable)
#' @param y Character or vector (second categorical variable)
#' @param alternative Character. Alternative hypothesis: "two.sided" (default), "greater", "less"
#' @param conf_level Numeric. Confidence level for odds ratio CI (default 0.95)
#'
#' @return A list containing test results and odds ratio (for 2x2 tables)
#'
#' @examples
#' \dontrun{
#' # Small sample 2x2 table
#' result <- fisher_exact_test(
#'   data = df,
#'   x = "treatment",
#'   y = "response"
#' )
#' }
#'
#' @export
fisher_exact_test <- function(data = NULL,
                               x,
                               y,
                               alternative = c("two.sided", "greater", "less"),
                               conf_level = 0.95) {
  alternative <- match.arg(alternative)

  # Extract variables (same logic as chisq_test)
  if (is.null(data)) {
    x_var <- x
    y_var <- y
    x_name <- deparse(substitute(x))
    y_name <- deparse(substitute(y))
  } else {
    assert_vars_present(data, c(x, y), "Fisher's exact test")
    x_var <- data[[x]]
    y_var <- data[[y]]
    x_name <- x
    y_name <- y
  }

  # Remove missing
  valid_idx <- !is.na(x_var) & !is.na(y_var)
  x_clean <- x_var[valid_idx]
  y_clean <- y_var[valid_idx]

  # Create table
  obs_table <- table(x_clean, y_clean)

  message("\nFisher's exact test")
  message("  Table size: ", nrow(obs_table), " x ", ncol(obs_table))
  message("  Total n: ", sum(obs_table))

  # Run Fisher's exact test
  fisher_result <- stats::fisher.test(
    obs_table,
    alternative = alternative,
    conf.level = conf_level
  )

  # Extract results
  p_value <- fisher_result$p.value

  # For 2x2 tables, get odds ratio
  is_2x2 <- nrow(obs_table) == 2 && ncol(obs_table) == 2
  if (is_2x2) {
    or <- fisher_result$estimate
    or_ci_lower <- fisher_result$conf.int[1]
    or_ci_upper <- fisher_result$conf.int[2]
  } else {
    or <- NA
    or_ci_lower <- NA
    or_ci_upper <- NA
  }

  # Interpretation
  interpretation <- paste0(
    "FISHER'S EXACT TEST\n",
    "===================\n\n",
    "Variables: ", x_name, " x ", y_name, "\n",
    "Sample size: N = ", sum(obs_table), "\n",
    "Alternative: ", alternative, "\n\n",
    "RESULTS:\n",
    "p-value = ", if (p_value < 0.001) "< .001" else round(p_value, 4), "\n"
  )

  if (is_2x2) {
    interpretation <- paste0(interpretation,
      "Odds Ratio = ", round(or, 3),
      ", 95% CI [", round(or_ci_lower, 3), ", ", round(or_ci_upper, 3), "]\n"
    )
  }

  interpretation <- paste0(interpretation, "\n",
    "INTERPRETATION:\n",
    if (p_value < 0.05) {
      "Significant association detected (p < .05).\n"
    } else {
      "No significant association (p >= .05).\n"
    }
  )

  message("\n", interpretation)

  tibble::tibble(
    p_value = p_value,
    odds_ratio = or,
    or_ci_lower = or_ci_lower,
    or_ci_upper = or_ci_upper,
    alternative = alternative,
    n = sum(obs_table),
    interpretation = interpretation
  )
}


#' McNemar's Test for Paired Categorical Data
#'
#' @description
#' Conducts McNemar's test for paired nominal data (e.g., before-after
#' measurements on the same individuals).
#'
#' @param data Data frame
#' @param var1 Character. Name of first variable (before)
#' @param var2 Character. Name of second variable (after)
#' @param correct Logical. Apply continuity correction? (default TRUE)
#'
#' @return Tibble with test results
#'
#' @details
#' **When to use:**
#' - Paired/matched categorical data
#' - Before-after measurements
#' - Matched case-control studies
#'
#' **Assumptions:**
#' - Paired observations (same individuals measured twice)
#' - Nominal or ordinal variables
#' - For 2x2 tables with >=25 discordant pairs
#'
#' @examples
#' \dontrun{
#' # Before-after purchase behavior
#' result <- mcnemar_test(
#'   data = df,
#'   var1 = "purchased_before",
#'   var2 = "purchased_after"
#' )
#' }
#'
#' @export
mcnemar_test <- function(data,
                         var1,
                         var2,
                         correct = TRUE) {
  assert_vars_present(data, c(var1, var2), "McNemar's test")

  # Extract variables
  x <- data[[var1]]
  y <- data[[var2]]

  # Remove missing (pairwise)
  valid_idx <- !is.na(x) & !is.na(y)
  x_clean <- x[valid_idx]
  y_clean <- y[valid_idx]

  # Create table
  obs_table <- table(x_clean, y_clean)

  message("\nMcNemar's test for paired categorical data")
  message("  Pairs: ", sum(obs_table))

  # Run McNemar's test
  mcnemar_result <- stats::mcnemar.test(obs_table, correct = correct)

  # Extract results
  chisq_stat <- mcnemar_result$statistic
  p_value <- mcnemar_result$p.value

  # Calculate effect size (phi for 2x2)
  if (nrow(obs_table) == 2 && ncol(obs_table) == 2) {
    # Discordant pairs
    b <- obs_table[1, 2]  # Changed from no to yes
    c <- obs_table[2, 1]  # Changed from yes to no

    # Effect size (odds ratio of change)
    if (c > 0) {
      or_change <- b / c
    } else {
      or_change <- NA
    }

    n_changed <- b + c
    pct_changed <- (n_changed / sum(obs_table)) * 100
  } else {
    or_change <- NA
    n_changed <- NA
    pct_changed <- NA
  }

  # Interpretation
  interpretation <- paste0(
    "McNEMAR'S TEST\n",
    "==============\n\n",
    "Variables: ", var1, " -> ", var2, " (paired)\n",
    "Pairs: n = ", sum(obs_table), "\n\n",
    "RESULTS:\n",
    "chi^2(1) = ", round(chisq_stat, 2),
    ", p ", if (p_value < 0.001) "< .001" else paste("=", round(p_value, 3)), "\n"
  )

  if (!is.na(or_change)) {
    interpretation <- paste0(interpretation,
      "\nChange pattern:\n",
      "  Changed: ", n_changed, " (", round(pct_changed, 1), "%)\n",
      "  Odds of change = ", round(or_change, 2), "\n"
    )
  }

  interpretation <- paste0(interpretation, "\n",
    "INTERPRETATION:\n",
    if (p_value < 0.05) {
      "Significant change detected (p < .05).\n"
    } else {
      "No significant change (p >= .05).\n"
    }
  )

  message("\n", interpretation)

  tibble::tibble(
    statistic = as.numeric(chisq_stat),
    df = 1,
    p_value = p_value,
    n_pairs = sum(obs_table),
    n_changed = n_changed,
    pct_changed = pct_changed,
    odds_ratio_change = or_change,
    interpretation = interpretation
  )
}


#' Odds Ratio with Confidence Interval
#'
#' @description
#' Calculates odds ratio with confidence interval for 2x2 contingency tables.
#' Common in case-control studies and binary outcome analysis.
#'
#' @param data Data frame (optional)
#' @param x Character or vector (exposure/treatment variable)
#' @param y Character or vector (outcome variable)
#' @param conf_level Numeric. Confidence level (default 0.95)
#'
#' @return Tibble with odds ratio, CI, and interpretation
#'
#' @details
#' **Odds Ratio (OR):**
#' - OR = 1: No association
#' - OR > 1: Positive association (exposure increases odds of outcome)
#' - OR < 1: Negative association (exposure decreases odds of outcome)
#'
#' **Interpretation:**
#' - OR = 2: Exposure doubles the odds of outcome
#' - OR = 0.5: Exposure halves the odds of outcome
#'
#' @examples
#' \dontrun{
#' # Treatment effect on success
#' result <- odds_ratio_table(
#'   data = df,
#'   x = "treatment",  # Exposed vs unexposed
#'   y = "success"     # Outcome
#' )
#' }
#'
#' @export
odds_ratio_table <- function(data = NULL,
                              x,
                              y,
                              conf_level = 0.95) {
  # Extract variables
  if (is.null(data)) {
    x_var <- x
    y_var <- y
    x_name <- deparse(substitute(x))
    y_name <- deparse(substitute(y))
  } else {
    assert_vars_present(data, c(x, y), "odds ratio")
    x_var <- data[[x]]
    y_var <- data[[y]]
    x_name <- x
    y_name <- y
  }

  # Remove missing
  valid_idx <- !is.na(x_var) & !is.na(y_var)
  x_clean <- as.factor(x_var[valid_idx])
  y_clean <- as.factor(y_var[valid_idx])

  # Check 2x2
  if (nlevels(x_clean) != 2 || nlevels(y_clean) != 2) {
    rlang::abort(c(
      "Both variables must have exactly 2 levels for odds ratio",
      "x" = "{x_name} has {nlevels(x_clean)} levels",
      "x" = "{y_name} has {nlevels(y_clean)} levels"
    ))
  }

  # Create table
  obs_table <- table(x_clean, y_clean)

  # Extract cells (standard epidemiology notation)
  a <- obs_table[1, 1]  # Exposed & outcome
  b <- obs_table[1, 2]  # Exposed & no outcome
  c <- obs_table[2, 1]  # Not exposed & outcome
  d <- obs_table[2, 2]  # Not exposed & no outcome

  # Calculate odds ratio
  or <- (a * d) / (b * c)

  # Log OR and SE
  log_or <- log(or)
  se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)

  # Confidence interval
  alpha <- 1 - conf_level
  z_crit <- stats::qnorm(1 - alpha/2)

  log_ci_lower <- log_or - z_crit * se_log_or
  log_ci_upper <- log_or + z_crit * se_log_or

  or_ci_lower <- exp(log_ci_lower)
  or_ci_upper <- exp(log_ci_upper)

  # Wald test (H0: OR = 1)
  z_stat <- log_or / se_log_or
  p_value <- 2 * stats::pnorm(abs(z_stat), lower.tail = FALSE)

  # Interpret effect
  if (or > 1) {
    direction <- "increases"
    magnitude <- round((or - 1) * 100, 1)
    effect_desc <- paste0("+", magnitude, "%")
  } else if (or < 1) {
    direction <- "decreases"
    magnitude <- round((1 - or) * 100, 1)
    effect_desc <- paste0("-", magnitude, "%")
  } else {
    direction <- "does not affect"
    effect_desc <- "0%"
  }

  # Create result
  result <- tibble::tibble(
    exposure = x_name,
    outcome = y_name,
    odds_ratio = round(or, 3),
    or_ci_lower = round(or_ci_lower, 3),
    or_ci_upper = round(or_ci_upper, 3),
    conf_level = conf_level,
    p_value = p_value,
    interpretation = paste0(
      x_name, " ", direction, " odds of ", y_name,
      " by ", magnitude, "%"
    )
  )

  message("\nOdds Ratio: OR = ", round(or, 3),
          ", 95% CI [", round(or_ci_lower, 3), ", ", round(or_ci_upper, 3), "]")
  message("Interpretation: ", result$interpretation)
  if (!is.na(p_value)) {
    message("p-value = ", if (p_value < 0.001) "< .001" else round(p_value, 3))
  } else {
    message("p-value = NA")
  }

  result
}


#' @export
print.chisq_result <- function(x, ...) {
  cat("\n")
  cat("Chi-Square Test of Independence\n")
  cat("================================\n\n")

  cat("chi^2(", x$df, ") = ", round(x$statistic, 2),
      ", p ", if (x$p_value < 0.001) "< .001" else paste("=", round(x$p_value, 3)), "\n")
  cat("Cramer's V = ", round(x$cramers_v, 3), " (", x$effect_interp, ")\n")
  cat("Sample size: N = ", x$n, "\n\n")

  cat("Observed frequencies:\n")
  print(x$observed)

  if (x$use_fisher) {
    cat("\nWARNING: Consider using Fisher's exact test (low expected frequencies)\n")
  }

  cat("\n")
  invisible(x)
}
