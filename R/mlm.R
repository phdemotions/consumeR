#' Multi-Level Modeling (MLM/HLM)
#'
#' @description
#' Fit multilevel (hierarchical linear) models for nested data structures using
#' lme4. Handles random intercepts, random slopes, and crossed random effects.
#' Provides publication-ready output for consumer psychology research.
#'
#' @param formula Model formula in lme4 syntax. Use \code{(1 | group)} for
#'   random intercept, \code{(predictor | group)} for random intercept and
#'   slope. See \code{lme4::lmer} for full syntax.
#' @param data Data frame containing all variables in the formula.
#' @param REML Logical. Use REML (restricted maximum likelihood) estimation?
#'   Default is TRUE. Use FALSE for ML when comparing models with different
#'   fixed effects.
#' @param control Optional list of control parameters. See
#'   \code{lme4::lmerControl} for options.
#' @param ... Additional arguments passed to \code{lme4::lmer}.
#'
#' @return Object of class \code{mlm_result} (extends lmerMod) with additional
#'   elements:
#'   \describe{
#'     \item{fixed_effects}{Tibble of fixed effect estimates with CIs}
#'     \item{random_effects}{Tibble of random effect variances and SDs}
#'     \item{icc}{Intraclass correlation coefficient(s)}
#'     \item{model_fit}{Model fit statistics (AIC, BIC, log-likelihood)}
#'     \item{n_groups}{Number of groups per grouping factor}
#'     \item{n_obs}{Total number of observations}
#'     \item{interpretation}{Publication-ready interpretation text}
#'     \item{lmer_fit}{Original lmer fit object}
#'   }
#'
#' @details
#' This function provides a simplified interface to lme4::lmer() with output
#' formatted for publication in consumer psychology journals. It automatically:
#' \itemize{
#'   \item Calculates intraclass correlation (ICC)
#'   \item Extracts fixed and random effects
#'   \item Computes confidence intervals
#'   \item Formats results for publication
#'   \item Generates interpretation text
#' }
#'
#' **Random Effects Syntax**:
#' \itemize{
#'   \item Random intercept: \code{(1 | group)}
#'   \item Random slope: \code{(x | group)} (includes random intercept)
#'   \item Uncorrelated: \code{(x || group)} (no correlation between intercept and slope)
#'   \item Crossed effects: \code{(1 | group1) + (1 | group2)}
#'   \item Nested effects: \code{(1 | group1/group2)} or \code{(1 | group1:group2)}
#' }
#'
#' **Intraclass Correlation (ICC)**:
#' Proportion of total variance due to grouping. Indicates necessity of MLM:
#' \itemize{
#'   \item ICC < 0.05: Little clustering, OLS may suffice
#'   \item ICC 0.05-0.10: Moderate clustering, MLM recommended
#'   \item ICC > 0.10: Substantial clustering, MLM required
#' }
#'
#' **Model Comparison**:
#' \itemize{
#'   \item Use REML = TRUE for final models (default)
#'   \item Use REML = FALSE when comparing models with different fixed effects
#'   \item Compare nested models with anova()
#'   \item Use AIC/BIC for non-nested models
#' }
#'
#' @references
#' Bates, D., Machler, M., Bolker, B., & Walker, S. (2015). Fitting linear
#'   mixed-effects models using lme4. Journal of Statistical Software, 67(1),
#'   1-48.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). Multilevel analysis: An
#'   introduction to basic and advanced multilevel modeling (2nd ed.). Sage.
#'
#' @examples
#' \dontrun{
#' # Random intercept model: Satisfaction nested within stores
#' model1 <- run_mlm(
#'   satisfaction ~ price + quality + (1 | store_id),
#'   data = consumer_data
#' )
#' print(model1)
#'
#' # Random slope model: Price effect varies by store
#' model2 <- run_mlm(
#'   satisfaction ~ price + quality + (price | store_id),
#'   data = consumer_data
#' )
#'
#' # Crossed random effects: Consumers and products
#' model3 <- run_mlm(
#'   rating ~ brand + price + (1 | consumer_id) + (1 | product_id),
#'   data = rating_data
#' )
#'
#' # Extract results
#' tidy_mlm(model1)
#' }
#'
#' @export
run_mlm <- function(formula,
                    data,
                    REML = TRUE,
                    control = NULL,
                    ...) {

  # Input validation
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame", class = "invalid_input")
  }

  if (!inherits(formula, "formula")) {
    rlang::abort("`formula` must be a formula object", class = "invalid_input")
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    rlang::abort(
      "Package 'lme4' is required for MLM. Install with: install.packages('lme4')",
      class = "missing_package"
    )
  }

  # Fit model
  fit <- tryCatch(
    lme4::lmer(formula = formula, data = data, REML = REML, control = control, ...),
    error = function(e) {
      rlang::abort(
        sprintf("MLM model failed to converge: %s", e$message),
        class = "convergence_error"
      )
    }
  )

  # Check convergence
  if (!is.null(fit@optinfo$conv$lme4$messages)) {
    rlang::warn(
      paste("Convergence warnings:", fit@optinfo$conv$lme4$messages),
      class = "convergence_warning"
    )
  }

  # Extract fixed effects
  fixed <- extract_mlm_fixed(fit)

  # Extract random effects
  random <- extract_mlm_random(fit)

  # Calculate ICC
  icc_vals <- calculate_icc_internal(fit)

  # Extract model fit
  model_fit <- extract_mlm_fit(fit)

  # Get group information
  n_groups <- lme4::ngrps(fit)
  n_obs <- stats::nobs(fit)

  # Generate interpretation
  interpretation <- generate_mlm_interpretation(fixed, icc_vals, n_groups)

  # Create result object
  result <- list(
    fixed_effects = fixed,
    random_effects = random,
    icc = icc_vals,
    model_fit = model_fit,
    n_groups = n_groups,
    n_obs = n_obs,
    formula = formula,
    REML = REML,
    interpretation = interpretation,
    lmer_fit = fit
  )

  class(result) <- c("mlm_result", "list")
  result
}


#' Extract MLM Fixed Effects
#'
#' @param fit Fitted lmer object
#'
#' @return Tibble of fixed effects
#' @keywords internal
extract_mlm_fixed <- function(fit) {

  # Get coefficients
  coefs <- lme4::fixef(fit)

  # Get confidence intervals
  ci <- tryCatch(
    stats::confint(fit, method = "Wald", parm = "beta_"),
    error = function(e) {
      # If CI calculation fails, use approximate CIs from SE
      se <- sqrt(diag(as.matrix(stats::vcov(fit))))
      cbind(
        coefs - 1.96 * se,
        coefs + 1.96 * se
      )
    }
  )

  # Get p-values using lmerTest if available, otherwise use Wald z-tests
  if (requireNamespace("lmerTest", quietly = TRUE)) {
    sum_fit <- summary(lmerTest::as_lmerModLmerTest(fit))
    p_values <- sum_fit$coefficients[, "Pr(>|t|)"]
  } else {
    # Approximate p-values using Wald z-test
    se <- sqrt(diag(as.matrix(stats::vcov(fit))))
    z_vals <- coefs / se
    p_values <- 2 * stats::pnorm(-abs(z_vals))
  }

  # Create tibble
  fixed_tbl <- tibble::tibble(
    term = names(coefs),
    estimate = as.numeric(coefs),
    ci_lower = ci[, 1],
    ci_upper = ci[, 2],
    p_value = p_values,
    significant = p_values < 0.05
  )

  fixed_tbl
}


#' Extract MLM Random Effects
#'
#' @param fit Fitted lmer object
#'
#' @return Tibble of random effect variances and standard deviations
#' @keywords internal
extract_mlm_random <- function(fit) {

  vc <- lme4::VarCorr(fit)

  # Extract variances and SDs for each grouping factor
  groups <- names(vc)
  random_list <- list()

  for (g in groups) {
    vc_g <- vc[[g]]
    vars <- diag(vc_g)
    sds <- sqrt(vars)
    terms <- rownames(vc_g)

    random_list[[g]] <- tibble::tibble(
      group = g,
      term = terms,
      variance = vars,
      sd = sds
    )
  }

  # Add residual variance
  resid_var <- attr(vc, "sc")^2
  random_list[["Residual"]] <- tibble::tibble(
    group = "Residual",
    term = "Residual",
    variance = resid_var,
    sd = sqrt(resid_var)
  )

  # Combine into single tibble
  dplyr::bind_rows(random_list)
}


#' Calculate Intraclass Correlation
#'
#' @param fit Fitted lmer object
#'
#' @return Named vector of ICC values (one per grouping factor)
#' @keywords internal
calculate_icc_internal <- function(fit) {

  vc <- lme4::VarCorr(fit)

  # Total variance
  var_random <- sum(sapply(vc, function(x) sum(diag(x))))
  var_resid <- attr(vc, "sc")^2
  var_total <- var_random + var_resid

  # ICC for each grouping factor
  groups <- names(vc)
  icc_vals <- numeric(length(groups))
  names(icc_vals) <- groups

  for (i in seq_along(groups)) {
    var_group <- sum(diag(vc[[groups[i]]]))
    icc_vals[i] <- var_group / var_total
  }

  icc_vals
}


#' Extract MLM Model Fit Statistics
#'
#' @param fit Fitted lmer object
#'
#' @return Tibble of fit statistics
#' @keywords internal
extract_mlm_fit <- function(fit) {

  tibble::tibble(
    statistic = c("AIC", "BIC", "logLik", "deviance"),
    value = c(
      stats::AIC(fit),
      stats::BIC(fit),
      as.numeric(stats::logLik(fit)),
      stats::deviance(fit)
    )
  )
}


#' Generate MLM Interpretation Text
#'
#' @param fixed Tibble of fixed effects
#' @param icc Named vector of ICC values
#' @param n_groups Named vector of group counts
#'
#' @return Character string with interpretation
#' @keywords internal
generate_mlm_interpretation <- function(fixed, icc, n_groups) {

  # Significant fixed effects
  sig_fixed <- dplyr::filter(fixed, .data$significant)
  n_sig <- nrow(sig_fixed)

  fixed_text <- sprintf(
    "The multilevel model revealed %d significant fixed effect%s",
    n_sig,
    if (n_sig == 1) "" else "s"
  )

  # ICC interpretation
  max_icc <- max(icc)
  icc_text <- if (max_icc > 0.10) {
    "with substantial clustering"
  } else if (max_icc > 0.05) {
    "with moderate clustering"
  } else {
    "with minimal clustering"
  }

  # Grouping structure
  group_text <- sprintf(
    "across %s",
    paste(
      paste0(names(n_groups), " (n = ", n_groups, ")"),
      collapse = " and "
    )
  )

  paste0(fixed_text, " ", icc_text, " ", group_text, ".")
}


#' Calculate Intraclass Correlation Coefficient
#'
#' @description
#' Calculate ICC for a multilevel model or unconditional (null) model. ICC
#' quantifies the proportion of variance in the outcome that is due to
#' grouping. Justifies the need for multilevel modeling.
#'
#' @param model Either a fitted \code{mlm_result} object from \code{run_mlm()}
#'   or a formula for an unconditional model (intercept-only with random
#'   intercept).
#' @param data Data frame (required if \code{model} is a formula).
#'
#' @return List with ICC values and interpretation:
#'   \describe{
#'     \item{icc}{Named vector of ICC for each grouping factor}
#'     \item{interpretation}{Text describing clustering level}
#'     \item{variance_components}{Tibble of variance decomposition}
#'   }
#'
#' @details
#' ICC represents the proportion of total variance attributable to between-group
#' differences. It is calculated as:
#'
#' ICC = sigma^2_between / (sigma^2_between + sigma^2_within)
#'
#' **Interpretation Guidelines**:
#' \itemize{
#'   \item ICC < 0.05: Minimal clustering, MLM may not be necessary
#'   \item ICC 0.05-0.10: Moderate clustering, MLM recommended
#'   \item ICC > 0.10: Substantial clustering, MLM required
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate ICC from unconditional model
#' icc <- icc_calculate(satisfaction ~ (1 | store_id), data = mydata)
#' print(icc)
#'
#' # Or from fitted model
#' model <- run_mlm(satisfaction ~ price + (1 | store_id), data = mydata)
#' icc <- icc_calculate(model)
#' }
#'
#' @export
icc_calculate <- function(model, data = NULL) {

  if (inherits(model, "mlm_result")) {
    # Extract from fitted model
    icc_vals <- model$icc
    vc_tbl <- model$random_effects

  } else if (inherits(model, "formula")) {
    # Fit unconditional model
    if (is.null(data)) {
      rlang::abort(
        "`data` is required when `model` is a formula",
        class = "invalid_input"
      )
    }

    if (!requireNamespace("lme4", quietly = TRUE)) {
      rlang::abort(
        "Package 'lme4' is required. Install with: install.packages('lme4')",
        class = "missing_package"
      )
    }

    fit <- lme4::lmer(model, data = data, REML = TRUE)
    icc_vals <- calculate_icc_internal(fit)
    vc_tbl <- extract_mlm_random(fit)

  } else {
    rlang::abort(
      "`model` must be either mlm_result object or formula",
      class = "invalid_input"
    )
  }

  # Generate interpretation
  max_icc <- max(icc_vals)

  interpretation <- if (max_icc < 0.05) {
    "Minimal clustering detected (ICC < 0.05). Standard OLS regression may be sufficient."
  } else if (max_icc < 0.10) {
    "Moderate clustering detected (ICC 0.05-0.10). Multilevel modeling is recommended."
  } else {
    "Substantial clustering detected (ICC > 0.10). Multilevel modeling is required."
  }

  list(
    icc = icc_vals,
    variance_components = vc_tbl,
    interpretation = interpretation
  )
}


#' Extract Publication-Ready MLM Results
#'
#' @description
#' Extract and format MLM results for publication. Returns fixed effects,
#' random effects, ICC, and fit statistics in publication-ready format.
#'
#' @param mlm_result Result from \code{run_mlm()}.
#' @param fixed Logical. Include fixed effects table? Default TRUE.
#' @param random Logical. Include random effects table? Default TRUE.
#' @param icc Logical. Include ICC values? Default TRUE.
#' @param fit Logical. Include model fit statistics? Default TRUE.
#'
#' @return List with tibbles for each requested component.
#'
#' @examples
#' \dontrun{
#' model <- run_mlm(y ~ x + (1 | group), data = mydata)
#' tidy_mlm(model)
#' }
#'
#' @export
tidy_mlm <- function(mlm_result,
                     fixed = TRUE,
                     random = TRUE,
                     icc = TRUE,
                     fit = TRUE) {

  if (!inherits(mlm_result, "mlm_result")) {
    rlang::abort(
      "`mlm_result` must be output from run_mlm()",
      class = "invalid_input"
    )
  }

  output <- list()

  if (fixed) {
    output$fixed_effects <- mlm_result$fixed_effects
  }

  if (random) {
    output$random_effects <- mlm_result$random_effects
  }

  if (icc) {
    output$icc <- tibble::tibble(
      group = names(mlm_result$icc),
      icc = as.numeric(mlm_result$icc)
    )
  }

  if (fit) {
    output$model_fit <- mlm_result$model_fit
  }

  output
}


#' Check MLM Assumptions
#'
#' @description
#' Check assumptions for multilevel models: normality of residuals, normality
#' of random effects, and homoscedasticity. Returns diagnostic plots and
#' statistical tests.
#'
#' @param mlm_result Result from \code{run_mlm()}.
#' @param plot Logical. Generate diagnostic plots? Default TRUE.
#'
#' @return List with assumption checks:
#'   \describe{
#'     \item{residuals_normality}{Shapiro-Wilk test for level-1 residuals}
#'     \item{random_effects_normality}{Shapiro-Wilk tests for random effects}
#'     \item{homoscedasticity}{Visual assessment recommendation}
#'     \item{plots}{List of diagnostic plots (if plot = TRUE)}
#'     \item{summary}{Overall assessment text}
#'   }
#'
#' @details
#' **MLM Assumptions**:
#' \itemize{
#'   \item Level-1 residuals are normally distributed
#'   \item Random effects are normally distributed
#'   \item Homoscedasticity (constant variance across groups)
#'   \item Independence of level-1 residuals (within groups)
#' }
#'
#' **Remediation**:
#' \itemize{
#'   \item Non-normal residuals: Transform outcome, check for outliers
#'   \item Non-normal random effects: May indicate model misspecification
#'   \item Heteroscedasticity: Consider weighted models or robust SEs
#' }
#'
#' @examples
#' \dontrun{
#' model <- run_mlm(y ~ x + (1 | group), data = mydata)
#' assumptions <- mlm_assumptions(model)
#' print(assumptions$summary)
#' }
#'
#' @export
mlm_assumptions <- function(mlm_result, plot = TRUE) {

  if (!inherits(mlm_result, "mlm_result")) {
    rlang::abort(
      "`mlm_result` must be output from run_mlm()",
      class = "invalid_input"
    )
  }

  fit <- mlm_result$lmer_fit

  # Level-1 residuals
  resids <- stats::residuals(fit)

  # Normality test for residuals
  if (length(resids) >= 3 && length(resids) <= 5000) {
    resid_normality <- stats::shapiro.test(resids)
    resid_normal <- resid_normality$p.value > 0.05
  } else {
    resid_normality <- list(p.value = NA)
    resid_normal <- NA
  }

  # Random effects
  ranef_list <- lme4::ranef(fit)

  # Normality tests for random effects
  re_normality <- list()
  for (g in names(ranef_list)) {
    re_vals <- ranef_list[[g]][[1]]  # First random effect (usually intercept)
    if (length(re_vals) >= 3 && length(re_vals) <= 5000) {
      re_normality[[g]] <- stats::shapiro.test(re_vals)
    } else {
      re_normality[[g]] <- list(p.value = NA)
    }
  }

  # Generate summary
  summary_text <- generate_assumption_summary(resid_normal, re_normality)

  # Plots
  plots <- if (plot) {
    generate_mlm_plots(fit, resids, ranef_list)
  } else {
    NULL
  }

  list(
    residuals_normality = list(
      test = "Shapiro-Wilk",
      statistic = if (!is.na(resid_normality$p.value)) resid_normality$statistic else NA,
      p_value = resid_normality$p.value,
      assumption_met = resid_normal
    ),
    random_effects_normality = lapply(re_normality, function(x) {
      list(
        test = "Shapiro-Wilk",
        p_value = x$p.value,
        assumption_met = if (!is.na(x$p.value)) x$p.value > 0.05 else NA
      )
    }),
    homoscedasticity = list(
      note = "Visual inspection recommended using residuals vs fitted plot"
    ),
    plots = plots,
    summary = summary_text
  )
}


#' Generate MLM Assumption Summary
#'
#' @param resid_normal Logical, are residuals normal?
#' @param re_normality List of random effects normality tests
#'
#' @return Character string with summary
#' @keywords internal
generate_assumption_summary <- function(resid_normal, re_normality) {

  parts <- character()

  if (is.na(resid_normal)) {
    parts <- c(parts, "Residuals normality: Unable to test (sample size)")
  } else if (resid_normal) {
    parts <- c(parts, "Residuals normality: PASS Assumption met")
  } else {
    parts <- c(parts, "Residuals normality: FAIL Violated (p < .05)")
  }

  for (g in names(re_normality)) {
    p_val <- re_normality[[g]]$p.value
    if (is.na(p_val)) {
      parts <- c(parts, sprintf("Random effects (%s): Unable to test", g))
    } else if (p_val > 0.05) {
      parts <- c(parts, sprintf("Random effects (%s): PASS Assumption met", g))
    } else {
      parts <- c(parts, sprintf("Random effects (%s): FAIL Violated (p < .05)", g))
    }
  }

  paste(parts, collapse = "\n")
}


#' Generate MLM Diagnostic Plots
#'
#' @param fit lmer fit object
#' @param resids Residuals
#' @param ranef_list List of random effects
#'
#' @return List of plots (or NULL if plotting not possible)
#' @keywords internal
generate_mlm_plots <- function(fit, resids, ranef_list) {

  # Note: This is a placeholder. In practice, would generate:
  # 1. Residuals vs Fitted
  # 2. Q-Q plot for residuals
  # 3. Q-Q plots for random effects
  # 4. Scale-location plot

  # For now, return NULL (plots would require additional graphics code)
  NULL
}


#' Print Method for MLM Results
#'
#' @param x Object of class \code{mlm_result}
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#' @export
print.mlm_result <- function(x, ...) {

  cat("Multilevel Model (MLM/HLM)\n")
  cat("==========================\n\n")

  cat("Formula:", deparse(x$formula), "\n")
  cat("Estimation method:", if (x$REML) "REML" else "ML", "\n")
  cat("Number of observations:", x$n_obs, "\n\n")

  # Grouping structure
  cat("Grouping Structure:\n")
  cat("-------------------\n")
  for (g in names(x$n_groups)) {
    cat(sprintf("%s: %d groups\n", g, x$n_groups[g]))
  }
  cat("\n")

  # ICC
  cat("Intraclass Correlation (ICC):\n")
  cat("-----------------------------\n")
  for (g in names(x$icc)) {
    cat(sprintf("%s: %.3f\n", g, x$icc[g]))
  }
  cat("\n")

  # Fixed effects
  cat("Fixed Effects:\n")
  cat("--------------\n")
  print(x$fixed_effects, n = Inf)
  cat("\n")

  # Random effects
  cat("Random Effects (Variance Components):\n")
  cat("-------------------------------------\n")
  print(x$random_effects, n = Inf)
  cat("\n")

  # Model fit
  cat("Model Fit:\n")
  cat("----------\n")
  print(x$model_fit, n = Inf)
  cat("\n")

  # Interpretation
  cat("Interpretation:\n")
  cat("--------------\n")
  cat(x$interpretation, "\n")

  invisible(x)
}
