#' Robust Inference for Regression Models
#'
#' @description
#' Functions for heteroscedasticity-robust inference using sandwich estimators.
#' Essential for JCP publication when assumption violations are present.
#'
#' @name robust
NULL


#' Tidy Regression Results with Robust Standard Errors
#'
#' @description
#' Extract regression coefficients with heteroscedasticity-consistent (HC)
#' robust standard errors. Uses sandwich package for HC covariance matrix
#' and lmtest for inference. Publication-ready output for JCP.
#'
#' @param model Fitted lm or glm model
#' @param hc_type Character. Type of HC estimator:
#'   - "HC3" (default): Recommended for small samples (Long & Ervin, 2000)
#'   - "HC0": Original White (1980) estimator
#'   - "HC1": Degrees of freedom adjustment
#'   - "HC2": Improved small sample performance
#'   - "HC4": Good for influential observations
#'   - "HC5": Alternative for influential observations
#' @param conf_level Numeric. Confidence level for intervals (default 0.95)
#' @param test Character. Test statistic: "t" (default) or "z"
#'
#' @return A tibble containing:
#' \itemize{
#'   \item \code{term}: Coefficient name
#'   \item \code{estimate}: Point estimate
#'   \item \code{std_error}: Robust standard error
#'   \item \code{statistic}: t or z statistic
#'   \item \code{p_value}: Two-tailed p-value
#'   \item \code{ci_lower}: Lower confidence bound
#'   \item \code{ci_upper}: Upper confidence bound
#'   \item \code{stars}: Significance stars
#' }
#'
#' @details
#' **When to use robust SEs:**
#' - Heteroscedasticity detected (Breusch-Pagan test significant)
#' - Residual plots show non-constant variance
#' - Conservative inference desired
#' - Required by many journals when assumptions violated
#'
#' **HC Estimator Choice:**
#' - **HC3** (default): Best for small to moderate samples (n < 250)
#' - **HC0**: Original White, anticonservative in small samples
#' - **HC4/HC5**: Use when influential observations present
#'
#' **JCP Requirements:**
#' - Report which HC estimator used
#' - Justify choice based on diagnostics
#' - Report whether results changed from OLS
#' - Include in robustness checks section
#'
#' **References:**
#' - Long, J. S., & Ervin, L. H. (2000). Using heteroscedasticity
#'   consistent standard errors in the linear regression model.
#'   The American Statistician, 54(3), 217-224.
#' - White, H. (1980). A heteroskedasticity-consistent covariance
#'   matrix estimator and a direct test for heteroskedasticity.
#'   Econometrica, 48(4), 817-838.
#'
#' @examples
#' \dontrun{
#' # Fit model
#' model <- lm(satisfaction ~ price + quality + service, data = df)
#'
#' # Check for heteroscedasticity
#' assumptions <- assumption_checks(model)
#'
#' # If violated, use robust SEs
#' robust_results <- tidy_lm_robust(model, hc_type = "HC3")
#' print(robust_results)
#'
#' # Compare to OLS
#' ols_results <- broom::tidy(model, conf.int = TRUE)
#' }
#'
#' @export
tidy_lm_robust <- function(model,
                            hc_type = c("HC3", "HC0", "HC1", "HC2", "HC4", "HC5"),
                            conf_level = 0.95,
                            test = c("t", "z")) {
  hc_type <- match.arg(hc_type)
  test <- match.arg(test)

  # Check for required packages
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'sandwich' is required for robust SEs",
      "i" = "Install with: install.packages('sandwich')"
    ))
  }

  if (!requireNamespace("lmtest", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'lmtest' is required for robust inference",
      "i" = "Install with: install.packages('lmtest')"
    ))
  }

  # Validate model
  if (!inherits(model, c("lm", "glm"))) {
    rlang::abort(c(
      "model must be an lm or glm object",
      "x" = "You provided {class(model)[1]}"
    ))
  }

  message("\nCalculating robust standard errors...")
  message("  HC type: ", hc_type)
  message("  Test statistic: ", test)

  # Calculate HC covariance matrix
  vcov_hc <- sandwich::vcovHC(model, type = hc_type)

  # Get coefficient test results
  if (test == "t") {
    coef_test <- lmtest::coeftest(model, vcov. = vcov_hc)
  } else {
    coef_test <- lmtest::coeftest(model, vcov. = vcov_hc, df = Inf)
  }

  # Extract results
  coef_df <- as.data.frame(coef_test)
  coef_df$term <- rownames(coef_df)
  rownames(coef_df) <- NULL

  # Calculate confidence intervals
  alpha <- 1 - conf_level
  if (test == "t") {
    df_resid <- stats::df.residual(model)
    crit_val <- stats::qt(1 - alpha / 2, df = df_resid)
  } else {
    crit_val <- stats::qnorm(1 - alpha / 2)
  }

  coef_df$ci_lower <- coef_df$Estimate - crit_val * coef_df$`Std. Error`
  coef_df$ci_upper <- coef_df$Estimate + crit_val * coef_df$`Std. Error`

  # Add significance stars
  coef_df$stars <- vapply(coef_df$`Pr(>|t|)`, function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01) return("**")
    if (p < 0.05) return("*")
    if (p < 0.10) return(".")
    ""
  }, character(1))

  # Rename columns
  names(coef_df) <- c("estimate", "std_error", "statistic", "p_value", "term",
                      "ci_lower", "ci_upper", "stars")

  # Reorder columns
  coef_df <- coef_df[, c("term", "estimate", "std_error", "statistic",
                          "p_value", "ci_lower", "ci_upper", "stars")]

  # Convert to tibble
  result <- tibble::as_tibble(coef_df)

  # Add metadata
  attr(result, "hc_type") <- hc_type
  attr(result, "conf_level") <- conf_level
  attr(result, "test") <- test

  # Count significant predictors
  n_sig <- sum(result$p_value < 0.05, na.rm = TRUE) - 1  # Exclude intercept

  message("\nRobust inference complete.")
  message("  Coefficients: ", nrow(result))
  message("  Significant predictors (p < .05): ", n_sig)

  # Compare to OLS if requested
  ols_se <- sqrt(diag(stats::vcov(model)))
  robust_se <- result$std_error
  pct_change <- ((robust_se - ols_se) / ols_se) * 100

  max_change <- max(abs(pct_change), na.rm = TRUE)
  if (max_change > 10) {
    message("\n  Note: Robust SEs differ from OLS by up to ",
            round(max_change, 1), "%")
    message("  This suggests heteroscedasticity is present.")
  }

  result
}


#' Compare OLS and Robust Standard Errors
#'
#' @description
#' Side-by-side comparison of ordinary least squares (OLS) and
#' heteroscedasticity-robust standard errors. Helps assess impact
#' of heteroscedasticity on inference.
#'
#' @param model Fitted lm model
#' @param hc_type Character. HC estimator type (default "HC3")
#'
#' @return A tibble comparing OLS and robust results
#'
#' @examples
#' \dontrun{
#' model <- lm(y ~ x1 + x2, data = df)
#' comparison <- compare_ols_robust(model)
#' print(comparison)
#' }
#'
#' @export
compare_ols_robust <- function(model, hc_type = "HC3") {
  # Get OLS results
  if (!requireNamespace("broom", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'broom' is required",
      "i" = "Install with: install.packages('broom')"
    ))
  }

  ols <- broom::tidy(model, conf.int = TRUE)

  # Get robust results
  robust <- tidy_lm_robust(model, hc_type = hc_type)

  # Combine
  comparison <- tibble::tibble(
    term = ols$term,
    estimate = ols$estimate,
    se_ols = ols$std.error,
    se_robust = robust$std_error,
    se_pct_change = round(((robust$std_error - ols$std.error) / ols$std.error) * 100, 1),
    p_ols = ols$p.value,
    p_robust = robust$p_value,
    sig_changed = (ols$p.value < 0.05) != (robust$p_value < 0.05)
  )

  # Add interpretation
  comparison$interpretation <- vapply(seq_len(nrow(comparison)), function(i) {
    if (comparison$sig_changed[i]) {
      "Significance changed"
    } else if (abs(comparison$se_pct_change[i]) > 20) {
      "Large SE change"
    } else if (abs(comparison$se_pct_change[i]) > 10) {
      "Moderate SE change"
    } else {
      "Minimal change"
    }
  }, character(1))

  message("\nComparison complete:")
  message("  Terms with changed significance: ",
          sum(comparison$sig_changed, na.rm = TRUE))
  message("  Max SE change: ", max(abs(comparison$se_pct_change), na.rm = TRUE), "%")

  comparison
}


#' Assumption Checks with Remediation Recommendations
#'
#' @description
#' Comprehensive assumption checking for linear models with
#' specific recommendations for JCP publication standards.
#'
#' @param model Fitted lm model
#' @param alpha Significance level for tests (default 0.05)
#'
#' @return A list with assumption check results and recommendations
#'
#' @examples
#' \dontrun{
#' model <- lm(satisfaction ~ condition, data = df)
#' checks <- assumption_checks(model)
#' print(checks)
#' }
#'
#' @export
assumption_checks <- function(model, alpha = 0.05) {
  # Check for car package
  if (!requireNamespace("car", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'car' is required",
      "i" = "Install with: install.packages('car')"
    ))
  }

  message("\nRunning comprehensive assumption checks...")

  results <- list()

  # 1. Homoscedasticity (Breusch-Pagan)
  residuals_vec <- stats::residuals(model)
  fitted_vec <- stats::fitted(model)

  squared_resid <- residuals_vec^2
  bp_model <- stats::lm(squared_resid ~ fitted_vec)
  bp_summary <- summary(bp_model)
  bp_r2 <- bp_summary$r.squared
  n <- length(residuals_vec)
  bp_statistic <- n * bp_r2
  bp_p <- stats::pchisq(bp_statistic, df = 1, lower.tail = FALSE)

  results$homoscedasticity <- list(
    test = "Breusch-Pagan",
    statistic = bp_statistic,
    p_value = bp_p,
    assumption_met = bp_p > alpha,
    recommendation = if (bp_p <= alpha) {
      paste0("Use robust standard errors (HC3). Report: 'Heteroscedasticity was ",
             "detected (BP chi^2(1) = ", round(bp_statistic, 2), ", p ",
             if (bp_p < 0.001) "< .001" else paste("=", round(bp_p, 3)),
             "). Accordingly, we report heteroscedasticity-robust standard errors ",
             "(HC3; Long & Ervin, 2000).'")
    } else {
      "Homoscedasticity assumption met. Standard OLS inference appropriate."
    }
  )

  # 2. Multicollinearity (VIF)
  if (length(stats::coef(model)) > 2) {  # More than just intercept + 1 predictor
    vif_values <- car::vif(model)

    if (is.matrix(vif_values)) {
      # GVIF case (generalized VIF for categorical predictors)
      max_vif <- max(vif_values[, "GVIF^(1/(2*Df))"], na.rm = TRUE)
    } else {
      max_vif <- max(vif_values, na.rm = TRUE)
    }

    results$multicollinearity <- list(
      test = "Variance Inflation Factor (VIF)",
      max_vif = max_vif,
      assumption_met = max_vif < 5,
      recommendation = if (max_vif >= 5) {
        paste0("High multicollinearity detected (max VIF = ", round(max_vif, 2),
               "). Consider: (1) removing correlated predictors, ",
               "(2) combining into composite, or (3) using ridge regression.")
      } else {
        "Multicollinearity within acceptable limits (all VIF < 5)."
      }
    )
  }

  # 3. Normality of residuals (Shapiro-Wilk)
  if (n <= 5000) {  # Shapiro-Wilk limitation
    sw_test <- stats::shapiro.test(residuals_vec)
    results$normality <- list(
      test = "Shapiro-Wilk",
      statistic = sw_test$statistic,
      p_value = sw_test$p.value,
      assumption_met = sw_test$p.value > alpha,
      recommendation = if (sw_test$p.value <= alpha && n < 30) {
        "Non-normality detected with small sample. Consider: (1) transformation, (2) bootstrap, or (3) robust methods."
      } else if (sw_test$p.value <= alpha) {
        "Non-normality detected but with large sample, CLT applies. Proceed with caution or use bootstrap."
      } else {
        "Normality assumption met."
      }
    )
  }

  # 4. Independence (Durbin-Watson for time series)
  if (!is.null(model$model) && "time" %in% names(model$model)) {
    dw_test <- car::durbinWatsonTest(model)
    results$independence <- list(
      test = "Durbin-Watson",
      statistic = dw_test$dw,
      p_value = dw_test$p,
      assumption_met = dw_test$p > alpha,
      recommendation = if (dw_test$p <= alpha) {
        "Autocorrelation detected. Use time series methods or clustered SEs."
      } else {
        "No evidence of autocorrelation."
      }
    )
  }

  # Summary
  results$summary <- list(
    n_assumptions_checked = length(results) - 1,  # Exclude summary itself
    all_met = all(vapply(results[names(results) != "summary"],
                         function(x) x$assumption_met, logical(1)))
  )

  message("\nAssumption checks complete:")
  message("  Homoscedasticity: ", if (results$homoscedasticity$assumption_met) "PASS" else "FAIL")
  if (!is.null(results$multicollinearity)) {
    message("  Multicollinearity: ", if (results$multicollinearity$assumption_met) "PASS" else "FAIL")
  }
  if (!is.null(results$normality)) {
    message("  Normality: ", if (results$normality$assumption_met) "PASS" else "FAIL")
  }

  structure(results, class = c("assumption_checks", "list"))
}


#' @export
print.assumption_checks <- function(x, ...) {
  cat("\n")
  cat("ASSUMPTION CHECKS\n")
  cat("=================\n\n")

  for (check_name in setdiff(names(x), "summary")) {
    check <- x[[check_name]]
    cat(toupper(check_name), "\n")
    cat("Test: ", check$test, "\n")
    cat("Result: ", if (check$assumption_met) "PASS PASSED" else "FAIL VIOLATED", "\n")
    cat("Recommendation: ", check$recommendation, "\n\n")
  }

  if (!x$summary$all_met) {
    cat("WARNING: WARNING: Some assumptions violated. Follow recommendations above.\n")
  } else {
    cat("PASS All assumptions met. Standard inference appropriate.\n")
  }

  cat("\n")
  invisible(x)
}
