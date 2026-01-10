#' Run Logistic Regression for Binary Outcomes
#'
#' Fits a logistic regression model for binary outcomes, commonly used in consumer
#' psychology research to model purchase decisions, choice behavior, adoption,
#' and other binary outcomes. Returns a standard glm object that can be further
#' analyzed with \code{tidy_logistic()} for publication-ready results.
#'
#' @param formula A formula specifying the model (e.g., \code{outcome ~ predictor1 + predictor2})
#' @param data A data frame containing the variables
#' @param family The family to use (default: binomial(link = "logit"))
#' @param ... Additional arguments passed to \code{glm()}
#'
#' @return A glm object of class "logistic_model" with additional attributes:
#'   - n_obs: Number of observations
#'   - n_events: Number of events (outcome = 1)
#'   - event_rate: Proportion of events
#'   - predictors: Names of predictor variables
#'
#' @details
#' This function is a wrapper around \code{glm()} with sensible defaults for
#' consumer psychology research. The outcome variable must be binary (0/1, FALSE/TRUE,
#' or a two-level factor). Use \code{tidy_logistic()} to extract odds ratios and
#' confidence intervals, and \code{logistic_assumptions()} to check model assumptions.
#'
#' For JCP publications, always report:
#' - Odds ratios with 95% CIs
#' - Model fit statistics (AIC, pseudo-R²)
#' - Assumption checks (linearity of logit, multicollinearity)
#' - Classification accuracy if relevant
#'
#' @references
#' Hosmer, D. W., Lemeshow, S., & Sturdivant, R. X. (2013).
#' Applied Logistic Regression (3rd ed.). Wiley.
#'
#' @export
#' @examples
#' # Purchase decision based on price and quality perceptions
#' set.seed(42)
#' n <- 200
#' df <- data.frame(
#'   purchased = sample(0:1, n, replace = TRUE, prob = c(0.4, 0.6)),
#'   price = rnorm(n, 50, 10),
#'   quality = rnorm(n, 5, 1),
#'   age = sample(18:65, n, replace = TRUE)
#' )
#'
#' # Fit logistic regression
#' model <- run_logistic(purchased ~ price + quality + age, data = df)
#' summary(model)
#'
#' # Get publication-ready results with odds ratios
#' results <- tidy_logistic(model)
#' print(results)
run_logistic <- function(formula, data, family = stats::binomial(link = "logit"), ...) {
  # Validate inputs
  if (!inherits(formula, "formula")) {
    rlang::abort("`formula` must be a formula object", class = "invalid_input")
  }
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame", class = "invalid_input")
  }

  # Extract outcome variable name
  outcome_var <- as.character(formula[[2]])

  # Check outcome variable exists
  if (!outcome_var %in% names(data)) {
    rlang::abort(
      sprintf("Outcome variable '%s' not found in data", outcome_var),
      class = "variable_not_found"
    )
  }

  # Get outcome variable
  outcome <- data[[outcome_var]]

  # Check if binary
  unique_vals <- unique(stats::na.omit(outcome))
  if (length(unique_vals) != 2) {
    rlang::abort(
      sprintf(
        "Outcome variable '%s' must be binary (2 unique values), found %d values: %s",
        outcome_var,
        length(unique_vals),
        paste(unique_vals, collapse = ", ")
      ),
      class = "invalid_outcome"
    )
  }

  # Fit model
  model <- stats::glm(formula, data = data, family = family, ...)

  # Calculate metadata
  n_obs <- nrow(model$model)
  outcome_values <- model$model[[1]]

  # Convert to numeric if factor
  if (is.factor(outcome_values)) {
    outcome_numeric <- as.numeric(outcome_values) - 1
  } else {
    outcome_numeric <- as.numeric(outcome_values)
  }

  n_events <- sum(outcome_numeric)
  event_rate <- n_events / n_obs

  # Get predictor names (exclude intercept)
  predictors <- names(stats::coef(model))[-1]

  # Add class and attributes
  class(model) <- c("logistic_model", class(model))
  attr(model, "n_obs") <- n_obs
  attr(model, "n_events") <- n_events
  attr(model, "event_rate") <- event_rate
  attr(model, "predictors") <- predictors

  model
}


#' Tidy Logistic Regression Results with Odds Ratios
#'
#' Extracts publication-ready results from logistic regression models, including
#' odds ratios with confidence intervals, Wald tests, and interpretation text.
#' Designed for reporting in Journal of Consumer Psychology (JCP) publications.
#'
#' @param model A logistic regression model from \code{run_logistic()} or \code{glm()}
#' @param conf_level Confidence level for intervals (default: 0.95)
#' @param exponentiate Logical; if TRUE (default), returns odds ratios. If FALSE,
#'   returns log-odds (coefficients on logit scale)
#' @param include_intercept Logical; include intercept in results (default: FALSE)
#'
#' @return A tibble with columns:
#'   - term: Predictor variable name
#'   - estimate: Log-odds coefficient (if exponentiate = FALSE) or odds ratio (if TRUE)
#'   - std_error: Standard error of log-odds
#'   - statistic: Wald z-statistic
#'   - p_value: p-value from Wald test
#'   - conf_low: Lower bound of confidence interval
#'   - conf_high: Upper bound of confidence interval
#'   - interpretation: Publication-ready interpretation text
#'
#' @details
#' For JCP publications, odds ratios are the preferred effect size for logistic
#' regression. Interpretation:
#' - OR = 1: No association
#' - OR > 1: Positive association (predictor increases odds of outcome)
#' - OR < 1: Negative association (predictor decreases odds of outcome)
#'
#' The confidence interval provides the range of plausible values. If the CI
#' excludes 1, the effect is statistically significant at the specified alpha level.
#'
#' @references
#' Norton, E. C., Dowd, B. E., & Maciejewski, M. L. (2019).
#' Marginal Effects—Quantifying the Effect of Changes in Risk Factors in
#' Logistic Regression Models. JAMA, 321(13), 1304-1305.
#'
#' @export
#' @examples
#' # Fit model
#' set.seed(42)
#' df <- data.frame(
#'   purchased = sample(0:1, 100, replace = TRUE),
#'   price = rnorm(100, 50, 10),
#'   quality = rnorm(100, 5, 1)
#' )
#' model <- run_logistic(purchased ~ price + quality, data = df)
#'
#' # Get odds ratios with CIs
#' results <- tidy_logistic(model)
#' print(results)
#'
#' # Get log-odds (coefficient scale)
#' results_logodds <- tidy_logistic(model, exponentiate = FALSE)
tidy_logistic <- function(model,
                          conf_level = 0.95,
                          exponentiate = TRUE,
                          include_intercept = FALSE) {
  # Validate inputs
  if (!inherits(model, "glm")) {
    rlang::abort("`model` must be a glm object from run_logistic() or glm()", class = "invalid_input")
  }

  if (model$family$family != "binomial") {
    rlang::abort("`model` must be a binomial (logistic) regression", class = "invalid_model")
  }

  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    rlang::abort("`conf_level` must be between 0 and 1", class = "invalid_input")
  }

  # Extract coefficients and SEs
  coef_summary <- summary(model)$coefficients
  coef_est <- coef_summary[, "Estimate"]
  coef_se <- coef_summary[, "Std. Error"]
  coef_z <- coef_summary[, "z value"]
  coef_p <- coef_summary[, "Pr(>|z|)"]

  # Calculate confidence intervals
  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  ci_lower <- coef_est - z_crit * coef_se
  ci_upper <- coef_est + z_crit * coef_se

  # Create tibble
  result <- tibble::tibble(
    term = names(coef_est),
    estimate = coef_est,
    std_error = coef_se,
    statistic = coef_z,
    p_value = coef_p,
    conf_low = ci_lower,
    conf_high = ci_upper
  )

  # Exponentiate if requested
  if (exponentiate) {
    result <- result |>
      dplyr::mutate(
        estimate = exp(estimate),
        conf_low = exp(conf_low),
        conf_high = exp(conf_high)
      )
  }

  # Add interpretation
  result <- result |>
    dplyr::mutate(
      interpretation = purrr::map2_chr(
        .data$estimate, .data$p_value,
        function(est, p) {
          if (exponentiate) {
            # Odds ratio interpretation
            sig <- if (p < 0.05) "significant" else "non-significant"
            direction <- if (est > 1) {
              sprintf("%.1f%% increase in odds", (est - 1) * 100)
            } else if (est < 1) {
              sprintf("%.1f%% decrease in odds", (1 - est) * 100)
            } else {
              "no change in odds"
            }
            sprintf("%s (%s, p = %s)", direction, sig, format_p(p, style = "apa"))
          } else {
            # Log-odds interpretation
            sig <- if (p < 0.05) "significant" else "non-significant"
            direction <- if (est > 0) "positive" else if (est < 0) "negative" else "null"
            sprintf("%s association (%s, p = %s)", direction, sig, format_p(p, style = "apa"))
          }
        }
      )
    )

  # Remove intercept if requested
  if (!include_intercept) {
    result <- result |>
      dplyr::filter(.data$term != "(Intercept)")
  }

  # Add class
  class(result) <- c("tidy_logistic", class(result))

  result
}


#' Check Logistic Regression Assumptions
#'
#' Performs comprehensive diagnostic checks for logistic regression models,
#' including linearity of the logit, influential cases, multicollinearity,
#' and separation issues. Returns remediation recommendations for violations.
#'
#' @param model A logistic regression model from \code{run_logistic()} or \code{glm()}
#' @param data The original data frame used to fit the model
#' @param vif_threshold Variance Inflation Factor threshold for multicollinearity
#'   (default: 5; values > threshold indicate problematic collinearity)
#' @param cooks_threshold Cook's distance threshold for influential cases
#'   (default: 4/n where n is sample size)
#' @param alpha Significance level for tests (default: 0.05)
#'
#' @return A list of class "logistic_assumptions" containing:
#'   - linearity: Box-Tidwell test results for linearity of logit
#'   - vif: Variance Inflation Factors for multicollinearity
#'   - influential: Number and IDs of influential cases (Cook's D)
#'   - separation: Check for perfect/quasi-complete separation
#'   - hosmer_lemeshow: Goodness-of-fit test (if available)
#'   - remediation: Text recommendations for addressing violations
#'   - all_pass: Logical indicating if all assumptions met
#'
#' @details
#' Key assumptions for logistic regression:
#'
#' 1. **Linearity of the logit**: Continuous predictors should have a linear
#'    relationship with log-odds. Tested using Box-Tidwell transformation.
#'
#' 2. **No multicollinearity**: VIF < 5 (conservative) or < 10 (liberal).
#'    High VIF inflates standard errors and reduces power.
#'
#' 3. **No influential outliers**: Cook's D > 4/n suggests influential case.
#'    Remove or investigate cases with high influence.
#'
#' 4. **No separation**: Perfect separation occurs when outcome can be perfectly
#'    predicted by predictors. Causes infinite coefficient estimates.
#'
#' For JCP publications, report assumption checks and remedial actions taken.
#'
#' @references
#' Hosmer, D. W., Lemeshow, S., & Sturdivant, R. X. (2013).
#' Applied Logistic Regression (3rd ed.). Wiley.
#'
#' @export
#' @examples
#' # Fit model
#' set.seed(42)
#' df <- data.frame(
#'   purchased = sample(0:1, 150, replace = TRUE),
#'   price = rnorm(150, 50, 10),
#'   quality = rnorm(150, 5, 1),
#'   age = sample(25:65, 150, replace = TRUE)
#' )
#' model <- run_logistic(purchased ~ price + quality + age, data = df)
#'
#' # Check assumptions
#' checks <- logistic_assumptions(model, data = df)
#' print(checks)
logistic_assumptions <- function(model,
                                 data,
                                 vif_threshold = 5,
                                 cooks_threshold = NULL,
                                 alpha = 0.05) {
  # Validate inputs
  if (!inherits(model, "glm")) {
    rlang::abort("`model` must be a glm object", class = "invalid_input")
  }
  if (model$family$family != "binomial") {
    rlang::abort("`model` must be a binomial regression", class = "invalid_model")
  }
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame", class = "invalid_input")
  }

  n <- nrow(model$model)
  if (is.null(cooks_threshold)) {
    cooks_threshold <- 4 / n
  }

  remediation <- character()
  all_pass <- TRUE

  # 1. Multicollinearity (VIF)
  # Only calculate if more than 1 predictor
  vif_check <- list(pass = TRUE, values = NULL, max_vif = NA)
  predictors <- names(stats::coef(model))[-1]  # Exclude intercept

  if (length(predictors) > 1) {
    tryCatch({
      vif_vals <- car::vif(model)
      vif_check$values <- vif_vals
      vif_check$max_vif <- max(vif_vals)
      vif_check$pass <- all(vif_vals < vif_threshold)

      if (!vif_check$pass) {
        all_pass <- FALSE
        high_vif <- names(vif_vals)[vif_vals >= vif_threshold]
        remediation <- c(
          remediation,
          sprintf(
            "Multicollinearity detected (VIF ≥ %.1f for: %s). Consider centering variables, removing redundant predictors, or using ridge regression.",
            vif_threshold,
            paste(high_vif, collapse = ", ")
          )
        )
      }
    }, error = function(e) {
      vif_check$values <- NA
      vif_check$pass <- NA
    })
  }

  # 2. Influential cases (Cook's distance)
  cooks_d <- stats::cooks.distance(model)
  influential_ids <- which(cooks_d > cooks_threshold)
  n_influential <- length(influential_ids)

  influential_check <- list(
    pass = n_influential == 0,
    n_influential = n_influential,
    ids = influential_ids,
    threshold = cooks_threshold
  )

  if (!influential_check$pass) {
    all_pass <- FALSE
    remediation <- c(
      remediation,
      sprintf(
        "%d influential case(s) detected (Cook's D > %.4f). Investigate cases: %s. Consider removing or using robust methods.",
        n_influential,
        cooks_threshold,
        paste(head(influential_ids, 10), collapse = ", ")
      )
    )
  }

  # 3. Separation check
  # Check for very large coefficient estimates (suggests separation)
  coefs <- stats::coef(model)[-1]  # Exclude intercept
  large_coefs <- abs(coefs) > 10

  separation_check <- list(
    pass = !any(large_coefs),
    large_coefs = names(coefs)[large_coefs]
  )

  if (!separation_check$pass) {
    all_pass <- FALSE
    remediation <- c(
      remediation,
      sprintf(
        "Potential separation issue detected (large coefficients for: %s). Consider Firth penalized regression or collapsing categories.",
        paste(separation_check$large_coefs, collapse = ", ")
      )
    )
  }

  # 4. Linearity of logit (Box-Tidwell for continuous predictors)
  # Get continuous predictors
  model_terms <- attr(stats::terms(model), "term.labels")
  continuous_preds <- character()

  for (term in model_terms) {
    if (term %in% names(data)) {
      var <- data[[term]]
      if (is.numeric(var) && !is.factor(var) && length(unique(var)) > 10) {
        continuous_preds <- c(continuous_preds, term)
      }
    }
  }

  linearity_check <- list(
    pass = TRUE,
    continuous_predictors = continuous_preds,
    violations = character()
  )

  # Box-Tidwell test: add interaction of predictor with log(predictor)
  # Significant interaction suggests non-linearity
  if (length(continuous_preds) > 0) {
    for (pred in continuous_preds) {
      # Need positive values for log transformation
      pred_vals <- data[[pred]]

      # Shift to positive if needed
      if (any(pred_vals <= 0)) {
        pred_vals <- pred_vals - min(pred_vals) + 1
      }

      # Create interaction term
      data_bt <- data
      bt_term <- pred_vals * log(pred_vals)
      data_bt[[paste0(pred, "_bt")]] <- bt_term

      # Fit model with Box-Tidwell term
      formula_bt <- stats::update(
        stats::formula(model),
        stats::as.formula(paste0(". ~ . + ", pred, "_bt"))
      )

      tryCatch({
        model_bt <- stats::glm(formula_bt, data = data_bt, family = stats::binomial())
        bt_coef_name <- paste0(pred, "_bt")

        if (bt_coef_name %in% names(stats::coef(model_bt))) {
          bt_p <- summary(model_bt)$coefficients[bt_coef_name, "Pr(>|z|)"]

          if (bt_p < alpha) {
            linearity_check$pass <- FALSE
            linearity_check$violations <- c(linearity_check$violations, pred)
            all_pass <- FALSE
          }
        }
      }, error = function(e) {
        # If Box-Tidwell fails, skip
      })
    }

    if (!linearity_check$pass) {
      remediation <- c(
        remediation,
        sprintf(
          "Non-linear relationship with log-odds detected for: %s. Consider adding polynomial terms, splines, or transformations.",
          paste(linearity_check$violations, collapse = ", ")
        )
      )
    }
  }

  # Compile results
  result <- list(
    vif = vif_check,
    influential = influential_check,
    separation = separation_check,
    linearity = linearity_check,
    remediation = if (length(remediation) > 0) remediation else "All assumptions met.",
    all_pass = all_pass
  )

  class(result) <- "logistic_assumptions"
  result
}


#' @export
print.logistic_assumptions <- function(x, ...) {
  cat("Logistic Regression Assumption Checks\n")
  cat("=====================================\n\n")

  # VIF
  cat("1. Multicollinearity (VIF):\n")
  if (!is.null(x$vif$values) && !all(is.na(x$vif$values))) {
    cat(sprintf("   Max VIF: %.2f\n", x$vif$max_vif))
    cat(sprintf("   Status: %s\n", if (x$vif$pass) "✓ PASS" else "✗ FAIL"))
  } else {
    cat("   Status: Not applicable (single predictor)\n")
  }
  cat("\n")

  # Influential cases
  cat("2. Influential Cases (Cook's D):\n")
  cat(sprintf("   Number of influential cases: %d\n", x$influential$n_influential))
  cat(sprintf("   Threshold: %.4f\n", x$influential$threshold))
  cat(sprintf("   Status: %s\n", if (x$influential$pass) "✓ PASS" else "✗ FAIL"))
  cat("\n")

  # Separation
  cat("3. Separation Check:\n")
  if (x$separation$pass) {
    cat("   Status: ✓ PASS (no extreme coefficients)\n")
  } else {
    cat("   Status: ✗ FAIL (potential separation)\n")
    cat(sprintf("   Large coefficients: %s\n", paste(x$separation$large_coefs, collapse = ", ")))
  }
  cat("\n")

  # Linearity
  cat("4. Linearity of Logit:\n")
  if (length(x$linearity$continuous_predictors) == 0) {
    cat("   Status: Not applicable (no continuous predictors)\n")
  } else {
    cat(sprintf("   Continuous predictors tested: %s\n",
                paste(x$linearity$continuous_predictors, collapse = ", ")))
    cat(sprintf("   Status: %s\n", if (x$linearity$pass) "✓ PASS" else "✗ FAIL"))
    if (!x$linearity$pass) {
      cat(sprintf("   Non-linear: %s\n", paste(x$linearity$violations, collapse = ", ")))
    }
  }
  cat("\n")

  # Overall
  cat("Overall Status:\n")
  cat(sprintf("   %s\n", if (x$all_pass) "✓ All assumptions met" else "✗ Some assumptions violated"))
  cat("\n")

  # Remediation
  cat("Remediation:\n")
  if (is.character(x$remediation)) {
    for (rec in x$remediation) {
      cat(sprintf("   • %s\n", rec))
    }
  }

  invisible(x)
}


#' Calculate Pseudo R-squared for Logistic Regression
#'
#' Calculates multiple pseudo R-squared measures for logistic regression models.
#' These are analogous to R² in linear regression but appropriate for binary outcomes.
#'
#' @param model A logistic regression model from \code{run_logistic()} or \code{glm()}
#'
#' @return A tibble with pseudo R-squared measures:
#'   - mcfadden: McFadden's R² (most common)
#'   - cox_snell: Cox & Snell R²
#'   - nagelkerke: Nagelkerke R² (normalized Cox & Snell)
#'   - tjur: Tjur's R² (coefficient of discrimination)
#'   - aic: Akaike Information Criterion
#'   - bic: Bayesian Information Criterion
#'
#' @details
#' Interpretation (McFadden's R²):
#' - 0.2-0.4: Excellent fit
#' - 0.1-0.2: Good fit
#' - < 0.1: Weak fit
#'
#' For JCP publications, report McFadden's R² and AIC/BIC for model comparison.
#'
#' @references
#' McFadden, D. (1977). Quantitative Methods for Analyzing Travel Behaviour of
#' Individuals: Some Recent Developments. Cowles Foundation Discussion Papers 474.
#'
#' @export
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'   outcome = sample(0:1, 100, replace = TRUE),
#'   x1 = rnorm(100),
#'   x2 = rnorm(100)
#' )
#' model <- run_logistic(outcome ~ x1 + x2, data = df)
#' pseudo_r2(model)
pseudo_r2 <- function(model) {
  if (!inherits(model, "glm") || model$family$family != "binomial") {
    rlang::abort("`model` must be a binomial glm object", class = "invalid_input")
  }

  # Null model (intercept only)
  null_model <- stats::glm(
    stats::update(stats::formula(model), . ~ 1),
    data = model$model,
    family = stats::binomial()
  )

  # Log-likelihoods
  ll_model <- stats::logLik(model)[1]
  ll_null <- stats::logLik(null_model)[1]

  n <- nrow(model$model)

  # McFadden's R²
  mcfadden <- 1 - (ll_model / ll_null)

  # Cox & Snell R²
  cox_snell <- 1 - exp(-2 * (ll_model - ll_null) / n)

  # Nagelkerke R²
  nagelkerke <- cox_snell / (1 - exp(2 * ll_null / n))

  # Tjur's R² (coefficient of discrimination)
  fitted_probs <- stats::fitted(model)
  outcome <- model$model[[1]]
  if (is.factor(outcome)) {
    outcome <- as.numeric(outcome) - 1
  }
  tjur <- mean(fitted_probs[outcome == 1]) - mean(fitted_probs[outcome == 0])

  # Information criteria
  aic <- stats::AIC(model)
  bic <- stats::BIC(model)

  tibble::tibble(
    mcfadden = mcfadden,
    cox_snell = cox_snell,
    nagelkerke = nagelkerke,
    tjur = tjur,
    aic = aic,
    bic = bic
  )
}
