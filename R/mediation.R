#' Simple Mediation Analysis with Bootstrap Confidence Intervals
#'
#' Performs simple mediation analysis for the causal path X → M → Y using
#' bootstrap resampling to estimate confidence intervals for indirect effects.
#' This is the gold standard approach for mediation in consumer psychology
#' research, recommended by Preacher & Hayes (2004, 2008).
#'
#' @param data A data frame containing the variables
#' @param x Character; name of the independent variable (predictor)
#' @param m Character; name of the mediator variable
#' @param y Character; name of the dependent variable (outcome)
#' @param covariates Character vector; names of covariates to control for (optional)
#' @param conf_level Confidence level for intervals (default: 0.95)
#' @param boot_samples Number of bootstrap samples (default: 5000; min 1000 for JCP)
#' @param boot_method Bootstrap CI method: "bca" (bias-corrected and accelerated,
#'   default), "perc" (percentile), or "norm" (normal approximation)
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return A list of class "mediation_simple" containing:
#'   - paths: Tibble with path coefficients (a, b, c, c_prime) and SEs
#'   - indirect_effect: Tibble with indirect effect (a*b), SE, and bootstrap CI
#'   - total_effect: Total effect (c path)
#'   - direct_effect: Direct effect (c' path)
#'   - proportion_mediated: Proportion of total effect mediated (PM = ab/c)
#'   - boot_samples: Number of bootstrap samples used
#'   - boot_distribution: Full bootstrap distribution of indirect effect
#'   - interpretation: Publication-ready interpretation text
#'
#' @details
#' **Mediation Model:**
#' - Path a: X → M (effect of X on mediator)
#' - Path b: M → Y (effect of mediator on Y, controlling for X)
#' - Path c: X → Y (total effect, without mediator)
#' - Path c': X → Y (direct effect, controlling for mediator)
#' - Indirect effect: a*b (mediated effect through M)
#'
#' **Significance Testing:**
#' The indirect effect is significant if the bootstrap confidence interval
#' does not include zero. This is more powerful than the Sobel test and does
#' not assume normality of the sampling distribution.
#'
#' **Types of Mediation:**
#' - Full mediation: Indirect effect significant, direct effect (c') not significant
#' - Partial mediation: Both indirect and direct effects significant
#' - No mediation: Indirect effect not significant
#'
#' **For JCP Publications:**
#' - Use ≥ 5000 bootstrap samples (10000 for critical tests)
#' - Report bias-corrected and accelerated (BCa) CIs
#' - Report all path coefficients with SEs
#' - Report proportion mediated if total effect is significant
#' - Visualize mediation model with path diagram
#'
#' @references
#' Preacher, K. J., & Hayes, A. F. (2004). SPSS and SAS procedures for estimating
#' indirect effects in simple mediation models. Behavior Research Methods,
#' Instruments, & Computers, 36(4), 717-731.
#'
#' Preacher, K. J., & Hayes, A. F. (2008). Asymptotic and resampling strategies
#' for assessing and comparing indirect effects in multiple mediator models.
#' Behavior Research Methods, 40(3), 879-891.
#'
#' Zhao, X., Lynch, J. G., & Chen, Q. (2010). Reconsidering Baron and Kenny:
#' Myths and truths about mediation analysis. Journal of Consumer Research,
#' 37(2), 197-206.
#'
#' @export
#' @examples
#' # Brand attitude mediates price → purchase intention
#' set.seed(42)
#' n <- 200
#' df <- data.frame(
#'   price_perception = rnorm(n, 5, 1),
#'   brand_attitude = rnorm(n, 5, 1),
#'   purchase_intention = rnorm(n, 5, 1)
#' )
#' # Create mediation relationship
#' df$brand_attitude <- df$brand_attitude + 0.5 * df$price_perception
#' df$purchase_intention <- df$purchase_intention +
#'   0.3 * df$price_perception + 0.6 * df$brand_attitude
#'
#' # Run mediation analysis
#' result <- mediation_simple(
#'   data = df,
#'   x = "price_perception",
#'   m = "brand_attitude",
#'   y = "purchase_intention",
#'   boot_samples = 1000,  # Use 5000+ for publications
#'   seed = 42
#' )
#'
#' print(result)
#' summary(result)
mediation_simple <- function(data,
                             x,
                             m,
                             y,
                             covariates = NULL,
                             conf_level = 0.95,
                             boot_samples = 5000,
                             boot_method = c("bca", "perc", "norm"),
                             seed = NULL) {
  # Validate inputs
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame", class = "invalid_input")
  }

  vars_needed <- c(x, m, y, covariates)
  missing_vars <- setdiff(vars_needed, names(data))
  if (length(missing_vars) > 0) {
    rlang::abort(
      sprintf("Variables not found in data: %s", paste(missing_vars, collapse = ", ")),
      class = "variable_not_found"
    )
  }

  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    rlang::abort("`conf_level` must be between 0 and 1", class = "invalid_input")
  }

  if (!is.numeric(boot_samples) || boot_samples < 1000) {
    rlang::abort(
      "`boot_samples` must be at least 1000 for reliable inference (5000+ recommended for JCP)",
      class = "invalid_input"
    )
  }

  boot_method <- match.arg(boot_method)

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Remove missing data
  data_complete <- data[stats::complete.cases(data[, vars_needed]), ]
  n_removed <- nrow(data) - nrow(data_complete)

  if (n_removed > 0) {
    message(sprintf("Removed %d rows with missing data (%.1f%% of cases)",
                    n_removed, 100 * n_removed / nrow(data)))
  }

  if (nrow(data_complete) < 30) {
    rlang::warn(
      sprintf("Small sample size (n = %d). Mediation results may be unstable.", nrow(data_complete)),
      class = "small_sample"
    )
  }

  # Build formulas
  if (is.null(covariates)) {
    formula_m <- stats::as.formula(sprintf("%s ~ %s", m, x))
    formula_y_total <- stats::as.formula(sprintf("%s ~ %s", y, x))
    formula_y_direct <- stats::as.formula(sprintf("%s ~ %s + %s", y, x, m))
  } else {
    cov_string <- paste(covariates, collapse = " + ")
    formula_m <- stats::as.formula(sprintf("%s ~ %s + %s", m, x, cov_string))
    formula_y_total <- stats::as.formula(sprintf("%s ~ %s + %s", y, x, cov_string))
    formula_y_direct <- stats::as.formula(sprintf("%s ~ %s + %s + %s", y, x, m, cov_string))
  }

  # Fit models
  # Path a: X → M
  model_a <- stats::lm(formula_m, data = data_complete)
  coef_a <- stats::coef(model_a)[x]
  se_a <- summary(model_a)$coefficients[x, "Std. Error"]
  p_a <- summary(model_a)$coefficients[x, "Pr(>|t|)"]

  # Path c: X → Y (total effect)
  model_c <- stats::lm(formula_y_total, data = data_complete)
  coef_c <- stats::coef(model_c)[x]
  se_c <- summary(model_c)$coefficients[x, "Std. Error"]
  p_c <- summary(model_c)$coefficients[x, "Pr(>|t|)"]

  # Paths b and c': M → Y and X → Y (direct effect), controlling for mediator
  model_bc <- stats::lm(formula_y_direct, data = data_complete)
  coef_b <- stats::coef(model_bc)[m]
  se_b <- summary(model_bc)$coefficients[m, "Std. Error"]
  p_b <- summary(model_bc)$coefficients[m, "Pr(>|t|)"]

  coef_c_prime <- stats::coef(model_bc)[x]
  se_c_prime <- summary(model_bc)$coefficients[x, "Std. Error"]
  p_c_prime <- summary(model_bc)$coefficients[x, "Pr(>|t|)"]

  # Point estimate of indirect effect
  indirect_point <- coef_a * coef_b

  # Sobel SE (for reference, but we'll use bootstrap)
  sobel_se <- sqrt(coef_b^2 * se_a^2 + coef_a^2 * se_b^2)

  # Bootstrap indirect effect
  boot_indirect <- numeric(boot_samples)

  for (i in seq_len(boot_samples)) {
    # Resample with replacement
    boot_indices <- sample(seq_len(nrow(data_complete)), replace = TRUE)
    boot_data <- data_complete[boot_indices, ]

    # Fit bootstrap models
    tryCatch({
      boot_model_a <- stats::lm(formula_m, data = boot_data)
      boot_model_bc <- stats::lm(formula_y_direct, data = boot_data)

      boot_a <- stats::coef(boot_model_a)[x]
      boot_b <- stats::coef(boot_model_bc)[m]

      boot_indirect[i] <- boot_a * boot_b
    }, error = function(e) {
      # If bootstrap sample fails, use NA
      boot_indirect[i] <- NA
    })
  }

  # Remove NA values from failed bootstrap samples
  boot_indirect <- boot_indirect[!is.na(boot_indirect)]
  n_boot_success <- length(boot_indirect)

  if (n_boot_success < boot_samples * 0.95) {
    rlang::warn(
      sprintf("%.1f%% of bootstrap samples failed. Results may be unstable.",
              100 * (1 - n_boot_success / boot_samples)),
      class = "bootstrap_failure"
    )
  }

  # Calculate bootstrap CI
  if (boot_method == "perc") {
    # Percentile method
    alpha <- 1 - conf_level
    ci_lower <- stats::quantile(boot_indirect, alpha / 2)
    ci_upper <- stats::quantile(boot_indirect, 1 - alpha / 2)
  } else if (boot_method == "norm") {
    # Normal approximation
    boot_se <- stats::sd(boot_indirect)
    z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)
    ci_lower <- indirect_point - z_crit * boot_se
    ci_upper <- indirect_point + z_crit * boot_se
  } else {
    # BCa (bias-corrected and accelerated)
    # Bias correction
    z0 <- stats::qnorm(mean(boot_indirect < indirect_point))

    # Acceleration (jackknife)
    n <- nrow(data_complete)
    jack_indirect <- numeric(n)

    for (i in seq_len(n)) {
      jack_data <- data_complete[-i, ]
      jack_model_a <- stats::lm(formula_m, data = jack_data)
      jack_model_bc <- stats::lm(formula_y_direct, data = jack_data)
      jack_a <- stats::coef(jack_model_a)[x]
      jack_b <- stats::coef(jack_model_bc)[m]
      jack_indirect[i] <- jack_a * jack_b
    }

    jack_mean <- mean(jack_indirect)
    jack_diff <- jack_mean - jack_indirect
    acceleration <- sum(jack_diff^3) / (6 * sum(jack_diff^2)^1.5)

    # BCa adjusted percentiles
    alpha <- 1 - conf_level
    z_alpha_lower <- stats::qnorm(alpha / 2)
    z_alpha_upper <- stats::qnorm(1 - alpha / 2)

    p_lower <- stats::pnorm(z0 + (z0 + z_alpha_lower) / (1 - acceleration * (z0 + z_alpha_lower)))
    p_upper <- stats::pnorm(z0 + (z0 + z_alpha_upper) / (1 - acceleration * (z0 + z_alpha_upper)))

    ci_lower <- stats::quantile(boot_indirect, p_lower)
    ci_upper <- stats::quantile(boot_indirect, p_upper)
  }

  # Proportion mediated (only if total effect is non-zero)
  if (abs(coef_c) > 1e-10) {
    proportion_mediated <- indirect_point / coef_c
  } else {
    proportion_mediated <- NA
  }

  # Determine mediation type
  indirect_sig <- !(ci_lower <= 0 && ci_upper >= 0)
  direct_sig <- p_c_prime < 0.05

  if (indirect_sig && !direct_sig) {
    mediation_type <- "Full mediation"
  } else if (indirect_sig && direct_sig) {
    mediation_type <- "Partial mediation"
  } else {
    mediation_type <- "No mediation"
  }

  # Create paths tibble
  paths <- tibble::tibble(
    path = c("a (X → M)", "b (M → Y)", "c (X → Y, total)", "c' (X → Y, direct)"),
    label = c("a", "b", "c", "c'"),
    estimate = c(coef_a, coef_b, coef_c, coef_c_prime),
    std_error = c(se_a, se_b, se_c, se_c_prime),
    p_value = c(p_a, p_b, p_c, p_c_prime),
    significant = c(p_a < 0.05, p_b < 0.05, p_c < 0.05, p_c_prime < 0.05)
  )

  # Create indirect effect tibble
  indirect <- tibble::tibble(
    effect = "Indirect (a*b)",
    estimate = indirect_point,
    std_error_sobel = sobel_se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    ci_method = boot_method,
    boot_samples = n_boot_success,
    significant = indirect_sig
  )

  # Interpretation
  interpretation <- sprintf(
    "%s detected. Indirect effect = %.3f, %d%% CI [%.3f, %.3f]. Path a: %.3f (%s), Path b: %.3f (%s), Direct effect (c'): %.3f (%s).",
    mediation_type,
    indirect_point,
    round(conf_level * 100),
    ci_lower,
    ci_upper,
    coef_a,
    format_p(p_a, style = "apa"),
    coef_b,
    format_p(p_b, style = "apa"),
    coef_c_prime,
    format_p(p_c_prime, style = "apa")
  )

  # Create result object
  result <- list(
    paths = paths,
    indirect_effect = indirect,
    total_effect = coef_c,
    direct_effect = coef_c_prime,
    proportion_mediated = proportion_mediated,
    mediation_type = mediation_type,
    boot_samples = n_boot_success,
    boot_distribution = boot_indirect,
    conf_level = conf_level,
    interpretation = interpretation,
    n_obs = nrow(data_complete),
    variables = list(x = x, m = m, y = y, covariates = covariates)
  )

  class(result) <- "mediation_simple"
  result
}


#' @export
print.mediation_simple <- function(x, ...) {
  cat("Simple Mediation Analysis\n")
  cat("=========================\n\n")

  cat(sprintf("Model: %s → %s → %s\n", x$variables$x, x$variables$m, x$variables$y))
  if (!is.null(x$variables$covariates)) {
    cat(sprintf("Covariates: %s\n", paste(x$variables$covariates, collapse = ", ")))
  }
  cat(sprintf("Sample size: N = %d\n", x$n_obs))
  cat(sprintf("Bootstrap samples: %d\n", x$boot_samples))
  cat("\n")

  cat("Path Coefficients:\n")
  print(x$paths, n = Inf)
  cat("\n")

  cat("Indirect Effect:\n")
  print(x$indirect_effect, n = Inf)
  cat("\n")

  cat(sprintf("Total effect (c): %.3f\n", x$total_effect))
  cat(sprintf("Direct effect (c'): %.3f\n", x$direct_effect))

  if (!is.na(x$proportion_mediated)) {
    cat(sprintf("Proportion mediated: %.1f%%\n", x$proportion_mediated * 100))
  }
  cat("\n")

  cat("Result:\n")
  cat(sprintf("   %s\n\n", x$mediation_type))

  cat("Interpretation:\n")
  cat(sprintf("   %s\n", x$interpretation))

  invisible(x)
}


#' @export
summary.mediation_simple <- function(object, ...) {
  print(object, ...)
}
