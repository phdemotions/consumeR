#' Mann-Whitney U Test (Wilcoxon Rank-Sum Test)
#'
#' Performs the Mann-Whitney U test for comparing two independent groups when
#' normality assumptions are violated. This is the non-parametric alternative
#' to the independent samples t-test, commonly used in consumer psychology when
#' data are ordinal or non-normal.
#'
#' @param data A data frame containing the variables (optional if x and y are vectors)
#' @param outcome Character; name of the outcome variable (if data provided)
#' @param group Character; name of the grouping variable (if data provided)
#' @param x Numeric vector for group 1 (if data not provided)
#' @param y Numeric vector for group 2 (if data not provided)
#' @param alternative Direction of test: "two.sided" (default), "greater", or "less"
#' @param conf_level Confidence level for effect size CI (default: 0.95)
#'
#' @return A list of class "mann_whitney" containing:
#'   - statistic: U statistic
#'   - p_value: p-value
#'   - rank_biserial: Rank-biserial correlation effect size (r_rb)
#'   - n1, n2: Sample sizes
#'   - median1, median2: Group medians
#'   - alternative: Direction of test
#'   - interpretation: Publication-ready text
#'
#' @details
#' **Effect Size (Rank-Biserial Correlation):**
#' - r_rb = 1 - (2U)/(n1*n2)
#' - Interpretation: |r_rb| < 0.3 small, 0.3-0.5 medium, > 0.5 large
#'
#' **When to Use:**
#' - Ordinal data (Likert scales analyzed as ranks)
#' - Non-normal continuous data with outliers
#' - Small samples where normality cannot be verified
#'
#' **For JCP Publications:**
#' - Report medians, not means
#' - Report effect size (rank-biserial)
#' - Justify choice of non-parametric test
#'
#' @references
#' Kerby, D. S. (2014). The simple difference formula: An approach to teaching
#' nonparametric correlation. Comprehensive Psychology, 3, 11-IT.
#'
#' @export
#' @examples
#' # Compare purchase intention between groups
#' set.seed(42)
#' df <- data.frame(
#'   intention = c(rpois(50, 3), rpois(50, 5)),  # Non-normal
#'   condition = rep(c("Control", "Treatment"), each = 50)
#' )
#'
#' result <- mann_whitney_test(df, outcome = "intention", group = "condition")
#' print(result)
mann_whitney_test <- function(data = NULL,
                              outcome = NULL,
                              group = NULL,
                              x = NULL,
                              y = NULL,
                              alternative = c("two.sided", "less", "greater"),
                              conf_level = 0.95) {
  alternative <- match.arg(alternative)

  # Get data
  if (!is.null(data)) {
    if (is.null(outcome) || is.null(group)) {
      rlang::abort(
        "If `data` is provided, both `outcome` and `group` must be specified",
        class = "invalid_input"
      )
    }

    if (!outcome %in% names(data)) {
      rlang::abort(sprintf("Outcome '%s' not found in data", outcome), class = "variable_not_found")
    }
    if (!group %in% names(data)) {
      rlang::abort(sprintf("Group '%s' not found in data", group), class = "variable_not_found")
    }

    group_var <- data[[group]]
    outcome_var <- data[[outcome]]

    # Get unique groups
    groups <- unique(stats::na.omit(group_var))
    if (length(groups) != 2) {
      rlang::abort(
        sprintf("Group variable must have exactly 2 levels, found %d", length(groups)),
        class = "invalid_groups"
      )
    }

    x <- outcome_var[group_var == groups[1]]
    y <- outcome_var[group_var == groups[2]]
    group_names <- as.character(groups)
  } else {
    if (is.null(x) || is.null(y)) {
      rlang::abort("Either provide `data`, `outcome`, and `group`, or provide `x` and `y`", class = "invalid_input")
    }
    group_names <- c("Group 1", "Group 2")
  }

  # Remove NA
  x <- stats::na.omit(x)
  y <- stats::na.omit(y)

  n1 <- length(x)
  n2 <- length(y)

  if (n1 < 3 || n2 < 3) {
    rlang::warn("Very small sample sizes (n < 3). Results may be unreliable.", class = "small_sample")
  }

  # Perform test
  test_result <- stats::wilcox.test(x, y, alternative = alternative, conf.int = FALSE)

  # Calculate effect size (rank-biserial correlation)
  U <- test_result$statistic
  r_rb <- 1 - (2 * U) / (n1 * n2)

  # Medians
  median1 <- stats::median(x)
  median2 <- stats::median(y)

  # Interpretation
  effect_size_interp <- if (abs(r_rb) < 0.3) {
    "small"
  } else if (abs(r_rb) < 0.5) {
    "medium"
  } else {
    "large"
  }

  sig_text <- if (test_result$p.value < 0.05) "significant" else "non-significant"

  interpretation <- sprintf(
    "%s difference between %s (Mdn = %.2f) and %s (Mdn = %.2f), U = %.1f, %s, r_rb = %.3f (%s effect).",
    tools::toTitleCase(sig_text),
    group_names[1], median1,
    group_names[2], median2,
    U,
    format_p(test_result$p.value, style = "apa"),
    r_rb,
    effect_size_interp
  )

  result <- list(
    statistic = as.numeric(U),
    p_value = test_result$p.value,
    rank_biserial = r_rb,
    n1 = n1,
    n2 = n2,
    median1 = median1,
    median2 = median2,
    group_names = group_names,
    alternative = alternative,
    interpretation = interpretation
  )

  class(result) <- "mann_whitney"
  result
}


#' @export
print.mann_whitney <- function(x, ...) {
  cat("Mann-Whitney U Test\n")
  cat("===================\n\n")

  cat(sprintf("%s vs %s\n", x$group_names[1], x$group_names[2]))
  cat(sprintf("n1 = %d, n2 = %d\n\n", x$n1, x$n2))

  cat(sprintf("U statistic: %.2f\n", x$statistic))
  cat(sprintf("p-value: %s\n", format_p(x$p_value, style = "apa")))
  cat(sprintf("Rank-biserial r: %.3f\n\n", x$rank_biserial))

  cat(sprintf("Median (%s): %.2f\n", x$group_names[1], x$median1))
  cat(sprintf("Median (%s): %.2f\n\n", x$group_names[2], x$median2))

  cat("Interpretation:\n")
  cat(sprintf("   %s\n", x$interpretation))

  invisible(x)
}


#' Kruskal-Wallis Test
#'
#' Performs the Kruskal-Wallis test for comparing three or more independent groups
#' when normality assumptions are violated. This is the non-parametric alternative
#' to one-way ANOVA.
#'
#' @param data A data frame containing the variables
#' @param outcome Character; name of the outcome variable
#' @param group Character; name of the grouping variable (3+ levels)
#'
#' @return A list of class "kruskal_wallis" containing:
#'   - statistic: H statistic (chi-square approximation)
#'   - df: Degrees of freedom
#'   - p_value: p-value
#'   - epsilon_squared: Effect size (η² analog for ranks)
#'   - group_medians: Tibble with medians for each group
#'   - interpretation: Publication-ready text
#'
#' @details
#' **Effect Size (Epsilon-squared):**
#' - ε² = H / (n² - 1) / (n + 1)
#' - Interpretation: Similar to η², < 0.01 negligible, 0.01-0.06 small,
#'   0.06-0.14 medium, > 0.14 large
#'
#' **Post-hoc Tests:**
#' If the Kruskal-Wallis test is significant, follow up with pairwise
#' Mann-Whitney tests with Bonferroni or Holm correction.
#'
#' @export
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'   rating = c(rpois(40, 3), rpois(40, 5), rpois(40, 4)),
#'   product = rep(c("A", "B", "C"), each = 40)
#' )
#'
#' result <- kruskal_wallis_test(df, outcome = "rating", group = "product")
#' print(result)
kruskal_wallis_test <- function(data, outcome, group) {
  # Validate
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame", class = "invalid_input")
  }

  if (!outcome %in% names(data)) {
    rlang::abort(sprintf("Outcome '%s' not found", outcome), class = "variable_not_found")
  }
  if (!group %in% names(data)) {
    rlang::abort(sprintf("Group '%s' not found", group), class = "variable_not_found")
  }

  outcome_var <- data[[outcome]]
  group_var <- data[[group]]

  # Check groups
  groups <- unique(stats::na.omit(group_var))
  if (length(groups) < 3) {
    rlang::abort("Kruskal-Wallis requires 3+ groups. Use mann_whitney_test() for 2 groups.", class = "invalid_groups")
  }

  # Perform test
  test_result <- stats::kruskal.test(outcome_var ~ group_var)

  # Effect size (epsilon-squared)
  n <- sum(!is.na(outcome_var) & !is.na(group_var))
  H <- test_result$statistic
  epsilon_sq <- H / ((n^2 - 1) / (n + 1))

  # Group medians
  group_medians <- data |>
    dplyr::group_by(!!rlang::sym(group)) |>
    dplyr::summarise(
      n = dplyr::n(),
      median = stats::median(!!rlang::sym(outcome), na.rm = TRUE),
      .groups = "drop"
    )

  # Interpretation
  sig_text <- if (test_result$p.value < 0.05) "Significant" else "Non-significant"

  interpretation <- sprintf(
    "%s difference across %d groups, H(%d) = %.2f, %s, ε² = %.3f. ",
    sig_text,
    length(groups),
    test_result$parameter,
    H,
    format_p(test_result$p.value, style = "apa"),
    epsilon_sq
  )

  if (test_result$p.value < 0.05) {
    interpretation <- paste0(interpretation, "Follow up with pairwise comparisons using Bonferroni correction.")
  }

  result <- list(
    statistic = as.numeric(H),
    df = test_result$parameter,
    p_value = test_result$p.value,
    epsilon_squared = as.numeric(epsilon_sq),
    group_medians = group_medians,
    n_groups = length(groups),
    interpretation = interpretation
  )

  class(result) <- "kruskal_wallis"
  result
}


#' @export
print.kruskal_wallis <- function(x, ...) {
  cat("Kruskal-Wallis Test\n")
  cat("===================\n\n")

  cat(sprintf("H statistic: %.2f\n", x$statistic))
  cat(sprintf("df: %d\n", x$df))
  cat(sprintf("p-value: %s\n", format_p(x$p_value, style = "apa")))
  cat(sprintf("Epsilon-squared: %.3f\n\n", x$epsilon_squared))

  cat("Group Medians:\n")
  print(x$group_medians, n = Inf)
  cat("\n")

  cat("Interpretation:\n")
  cat(sprintf("   %s\n", x$interpretation))

  invisible(x)
}


#' Wilcoxon Signed-Rank Test
#'
#' Performs the Wilcoxon signed-rank test for paired samples when normality
#' assumptions are violated. This is the non-parametric alternative to the
#' paired t-test, used for pre-post designs or matched pairs.
#'
#' @param data A data frame containing the variables (optional if x and y are vectors)
#' @param before Character; name of the "before" variable (if data provided)
#' @param after Character; name of the "after" variable (if data provided)
#' @param x Numeric vector for time 1 (if data not provided)
#' @param y Numeric vector for time 2 (if data not provided)
#' @param alternative Direction of test: "two.sided" (default), "greater", or "less"
#'
#' @return A list of class "wilcoxon_signed_rank" containing:
#'   - statistic: V statistic
#'   - p_value: p-value
#'   - rank_biserial: Effect size
#'   - n_pairs: Number of pairs
#'   - median_diff: Median of differences
#'   - interpretation: Publication-ready text
#'
#' @export
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'   pre = rpois(50, 4),
#'   post = rpois(50, 6)
#' )
#'
#' result <- wilcoxon_signed_rank_test(df, before = "pre", after = "post")
#' print(result)
wilcoxon_signed_rank_test <- function(data = NULL,
                                       before = NULL,
                                       after = NULL,
                                       x = NULL,
                                       y = NULL,
                                       alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  # Get data
  if (!is.null(data)) {
    if (is.null(before) || is.null(after)) {
      rlang::abort("If `data` provided, both `before` and `after` must be specified", class = "invalid_input")
    }

    x <- data[[before]]
    y <- data[[after]]
    var_names <- c(before, after)
  } else {
    if (is.null(x) || is.null(y)) {
      rlang::abort("Either provide `data` + `before` + `after`, or provide `x` + `y`", class = "invalid_input")
    }
    var_names <- c("Before", "After")
  }

  # Paired data: remove pairs with NA
  complete_pairs <- stats::complete.cases(x, y)
  x <- x[complete_pairs]
  y <- y[complete_pairs]

  n_pairs <- length(x)

  if (n_pairs < 5) {
    rlang::warn("Very small sample (n < 5). Results may be unreliable.", class = "small_sample")
  }

  # Perform test
  test_result <- stats::wilcox.test(x, y, paired = TRUE, alternative = alternative)

  # Effect size (rank-biserial for paired data)
  # r = Z / sqrt(n)
  V <- test_result$statistic
  # Approximate Z from V
  z_approx <- stats::qnorm(test_result$p.value / 2, lower.tail = FALSE)
  if (stats::median(y - x) < 0) z_approx <- -z_approx
  r_rb <- z_approx / sqrt(n_pairs)

  # Median difference
  median_diff <- stats::median(y - x)

  # Interpretation
  sig_text <- if (test_result$p.value < 0.05) "Significant" else "Non-significant"

  interpretation <- sprintf(
    "%s change from %s (Mdn = %.2f) to %s (Mdn = %.2f), V = %.1f, %s, r_rb = %.3f, median difference = %.2f.",
    sig_text,
    var_names[1], stats::median(x),
    var_names[2], stats::median(y),
    V,
    format_p(test_result$p.value, style = "apa"),
    r_rb,
    median_diff
  )

  result <- list(
    statistic = as.numeric(V),
    p_value = test_result$p.value,
    rank_biserial = r_rb,
    n_pairs = n_pairs,
    median_before = stats::median(x),
    median_after = stats::median(y),
    median_diff = median_diff,
    var_names = var_names,
    alternative = alternative,
    interpretation = interpretation
  )

  class(result) <- "wilcoxon_signed_rank"
  result
}


#' @export
print.wilcoxon_signed_rank <- function(x, ...) {
  cat("Wilcoxon Signed-Rank Test\n")
  cat("=========================\n\n")

  cat(sprintf("%s vs %s (paired)\n", x$var_names[1], x$var_names[2]))
  cat(sprintf("n pairs = %d\n\n", x$n_pairs))

  cat(sprintf("V statistic: %.2f\n", x$statistic))
  cat(sprintf("p-value: %s\n", format_p(x$p_value, style = "apa")))
  cat(sprintf("Rank-biserial r: %.3f\n\n", x$rank_biserial))

  cat(sprintf("Median (%s): %.2f\n", x$var_names[1], x$median_before))
  cat(sprintf("Median (%s): %.2f\n", x$var_names[2], x$median_after))
  cat(sprintf("Median difference: %.2f\n\n", x$median_diff))

  cat("Interpretation:\n")
  cat(sprintf("   %s\n", x$interpretation))

  invisible(x)
}


#' Friedman Test
#'
#' Performs the Friedman test for repeated measures with three or more time points
#' when normality assumptions are violated. This is the non-parametric alternative
#' to repeated measures ANOVA.
#'
#' @param data A data frame in long format with columns for subject ID, time/condition, and outcome
#' @param outcome Character; name of the outcome variable
#' @param time Character; name of the time/condition variable (3+ levels)
#' @param subject Character; name of the subject ID variable
#'
#' @return A list of class "friedman" containing:
#'   - statistic: Friedman chi-square statistic
#'   - df: Degrees of freedom
#'   - p_value: p-value
#'   - kendall_w: Kendall's W effect size (coefficient of concordance)
#'   - time_medians: Tibble with medians for each time point
#'   - interpretation: Publication-ready text
#'
#' @details
#' **Effect Size (Kendall's W):**
#' - W = χ²_F / (n(k-1))
#' - Interpretation: < 0.1 negligible, 0.1-0.3 small, 0.3-0.5 medium, > 0.5 large
#'
#' @export
#' @examples
#' set.seed(42)
#' df <- expand.grid(
#'   subject = 1:30,
#'   time = c("T1", "T2", "T3")
#' )
#' df$rating <- rpois(nrow(df), lambda = 4 + as.numeric(factor(df$time)))
#'
#' result <- friedman_test(df, outcome = "rating", time = "time", subject = "subject")
#' print(result)
friedman_test <- function(data, outcome, time, subject) {
  # Validate
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame", class = "invalid_input")
  }

  vars <- c(outcome, time, subject)
  missing <- setdiff(vars, names(data))
  if (length(missing) > 0) {
    rlang::abort(sprintf("Variables not found: %s", paste(missing, collapse = ", ")), class = "variable_not_found")
  }

  # Check time points
  times <- unique(stats::na.omit(data[[time]]))
  if (length(times) < 3) {
    rlang::abort("Friedman test requires 3+ time points. Use wilcoxon_signed_rank_test() for 2 time points.", class = "invalid_input")
  }

  # Perform test
  # Convert to wide format for friedman.test
  data_wide <- data |>
    dplyr::select(!!rlang::sym(subject), !!rlang::sym(time), !!rlang::sym(outcome)) |>
    tidyr::pivot_wider(names_from = !!rlang::sym(time), values_from = !!rlang::sym(outcome))

  # Extract matrix (exclude subject column)
  data_matrix <- as.matrix(data_wide[, -1])

  test_result <- stats::friedman.test(data_matrix)

  # Effect size (Kendall's W)
  n <- nrow(data_matrix)
  k <- ncol(data_matrix)
  chi_sq <- test_result$statistic
  kendall_w <- chi_sq / (n * (k - 1))

  # Time medians
  time_medians <- data |>
    dplyr::group_by(!!rlang::sym(time)) |>
    dplyr::summarise(
      median = stats::median(!!rlang::sym(outcome), na.rm = TRUE),
      .groups = "drop"
    )

  # Interpretation
  sig_text <- if (test_result$p.value < 0.05) "Significant" else "Non-significant"

  interpretation <- sprintf(
    "%s difference across %d time points, χ²(%d) = %.2f, %s, Kendall's W = %.3f. ",
    sig_text,
    k,
    test_result$parameter,
    chi_sq,
    format_p(test_result$p.value, style = "apa"),
    kendall_w
  )

  if (test_result$p.value < 0.05) {
    interpretation <- paste0(interpretation, "Follow up with pairwise Wilcoxon signed-rank tests.")
  }

  result <- list(
    statistic = as.numeric(chi_sq),
    df = test_result$parameter,
    p_value = test_result$p.value,
    kendall_w = as.numeric(kendall_w),
    time_medians = time_medians,
    n_subjects = n,
    n_times = k,
    interpretation = interpretation
  )

  class(result) <- "friedman"
  result
}


#' @export
print.friedman <- function(x, ...) {
  cat("Friedman Test\n")
  cat("=============\n\n")

  cat(sprintf("n subjects: %d\n", x$n_subjects))
  cat(sprintf("n time points: %d\n\n", x$n_times))

  cat(sprintf("χ² statistic: %.2f\n", x$statistic))
  cat(sprintf("df: %d\n", x$df))
  cat(sprintf("p-value: %s\n", format_p(x$p_value, style = "apa")))
  cat(sprintf("Kendall's W: %.3f\n\n", x$kendall_w))

  cat("Time Point Medians:\n")
  print(x$time_medians, n = Inf)
  cat("\n")

  cat("Interpretation:\n")
  cat(sprintf("   %s\n", x$interpretation))

  invisible(x)
}
