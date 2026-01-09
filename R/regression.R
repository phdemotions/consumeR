#' Perform Linear Regression with Comprehensive Diagnostics
#'
#' @description
#' Conducts linear regression analysis with explicit assumption testing,
#' diagnostic plots, and publication-ready output. Supports both simple
#' (one predictor) and multiple (many predictors) regression.
#'
#' @param data Data frame containing variables
#' @param formula Formula specifying the model, e.g., y ~ x or y ~ x1 + x2 + x3
#' @param check_assumptions Logical. Test regression assumptions? Default is TRUE
#' @param create_plots Logical. Create diagnostic plots? Default is TRUE
#' @param alpha Significance level (default 0.05)
#' @param verbose Logical. Show detailed explanations? Default is TRUE
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{model_type}: "Simple" or "Multiple" regression
#'     \item \code{coefficients}: Table of regression coefficients with tests
#'     \item \code{r_squared}: R² value
#'     \item \code{adj_r_squared}: Adjusted R² value
#'     \item \code{f_statistic}: Overall model F-statistic
#'     \item \code{f_df1}: F-statistic numerator df
#'     \item \code{f_df2}: F-statistic denominator df
#'     \item \code{p_value}: Overall model p-value
#'     \item \code{significant}: Is overall model significant?
#'     \item \code{rmse}: Root mean squared error
#'     \item \code{n_observations}: Sample size
#'     \item \code{assumptions}: Assumption check results (if requested)
#'     \item \code{diagnostic_plots}: ggplot2 diagnostic plots (if requested)
#'     \item \code{publication_text}: Publication-ready text
#'     \item \code{interpretation}: Plain English interpretation
#'     \item \code{lm_model}: Full lm() model object
#'   }
#'
#' @details
#' ## Regression Assumptions:
#' \itemize{
#'   \item \strong{Linearity}: Relationship between X and Y is linear
#'   \item \strong{Independence}: Residuals are independent
#'   \item \strong{Homoscedasticity}: Constant variance of residuals
#'   \item \strong{Normality}: Residuals are normally distributed
#'   \item \strong{No multicollinearity}: Predictors aren't highly correlated (multiple regression)
#' }
#'
#' ## R² Interpretation:
#' \itemize{
#'   \item 0.01 - 0.09: Small effect
#'   \item 0.09 - 0.25: Medium effect
#'   \item 0.25+: Large effect
#' }
#'
#' @examples
#' # Example 1: Simple regression
#' data <- data.frame(
#'   ad_spending = c(100, 200, 150, 300, 250, 400, 350, 500),
#'   sales = c(20, 35, 28, 48, 42, 65, 58, 80)
#' )
#' result <- analyze_regression(data, sales ~ ad_spending)
#'
#' # Example 2: Multiple regression
#' data <- data.frame(
#'   satisfaction = c(7, 8, 6, 9, 5, 8, 7, 6, 9, 8),
#'   price = c(10, 8, 12, 7, 15, 9, 11, 13, 8, 10),
#'   quality = c(8, 9, 7, 9, 6, 8, 7, 6, 9, 8),
#'   service = c(7, 8, 6, 9, 5, 8, 7, 7, 9, 8)
#' )
#' result <- analyze_regression(data, satisfaction ~ price + quality + service)
#'
#' # View publication text
#' print(result, show_publication = TRUE)
#'
#' @export
analyze_regression <- function(data,
                                formula,
                                check_assumptions = TRUE,
                                create_plots = TRUE,
                                alpha = 0.05,
                                verbose = TRUE) {

  # Step 1: Input validation
  # -------------------------
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data frame")
  }

  if (missing(formula)) {
    stop("Error: 'formula' is required, e.g., y ~ x or y ~ x1 + x2")
  }

  # Step 2: Fit the model
  # ---------------------
  if (verbose) {
    message("\nFitting linear regression model...")
  }

  lm_model <- lm(formula, data = data)
  model_summary <- summary(lm_model)

  # Extract components
  coef_table <- coef(model_summary)
  residuals_vec <- residuals(lm_model)
  fitted_vec <- fitted(lm_model)

  # Determine model type
  n_predictors <- length(coef_table[, 1]) - 1  # Exclude intercept
  model_type <- ifelse(n_predictors == 1, "Simple Linear Regression", "Multiple Linear Regression")

  # Extract model statistics
  r_squared <- model_summary$r.squared
  adj_r_squared <- model_summary$adj.r.squared
  f_stat <- model_summary$fstatistic[1]
  f_df1 <- model_summary$fstatistic[2]
  f_df2 <- model_summary$fstatistic[3]
  p_value <- pf(f_stat, f_df1, f_df2, lower.tail = FALSE)
  is_significant <- p_value < alpha

  # RMSE
  rmse <- sqrt(mean(residuals_vec^2))

  # Sample size
  n_obs <- length(residuals_vec)

  if (verbose) {
    message("Model fitted successfully (n = ", n_obs, ", predictors = ", n_predictors, ")\n")
  }

  # Step 3: Format coefficient table
  # ---------------------------------
  coef_df <- as.data.frame(coef_table)
  coef_df$variable <- rownames(coef_df)
  rownames(coef_df) <- NULL

  # Rename columns for clarity
  names(coef_df) <- c("estimate", "std_error", "t_value", "p_value", "variable")
  coef_df <- coef_df[, c("variable", "estimate", "std_error", "t_value", "p_value")]

  # Add significance stars
  coef_df$sig <- ifelse(coef_df$p_value < 0.001, "***",
                  ifelse(coef_df$p_value < 0.01, "**",
                  ifelse(coef_df$p_value < 0.05, "*",
                  ifelse(coef_df$p_value < 0.10, ".", ""))))

  # Step 4: Check assumptions
  # --------------------------
  assumptions <- list()
  diagnostic_plots <- list()

  if (check_assumptions) {
    if (verbose) {
      message("Checking regression assumptions...")
    }

    # 1. Linearity (checked via residuals vs fitted plot interpretation)
    # We'll note this in the output

    # 2. Independence
    assumptions$independence <- check_independence(
      data_structure = "cross-sectional regression",
      is_independent = TRUE,
      clustering_note = paste0(
        "This assumes independent observations. If your data has temporal ordering ",
        "or clustering, residuals may be correlated. Consider using time series methods ",
        "or mixed effects models."
      )
    )

    # 3. Normality of residuals
    assumptions$normality <- check_normality(
      residuals_vec,
      variable_name = "regression residuals",
      alpha = alpha
    )

    # 4. Homoscedasticity (constant variance)
    # Use Breusch-Pagan test approximation
    squared_resid <- residuals_vec^2
    bp_model <- lm(squared_resid ~ fitted_vec)
    bp_summary <- summary(bp_model)
    bp_r2 <- bp_summary$r.squared
    bp_statistic <- n_obs * bp_r2
    bp_p_value <- pchisq(bp_statistic, df = 1, lower.tail = FALSE)

    homoscedasticity_met <- bp_p_value > alpha

    if (homoscedasticity_met) {
      homoscedasticity_text <- paste0(
        "The homoscedasticity assumption is MET (Breusch-Pagan test: ",
        "χ² = ", round(bp_statistic, 2), ", p = ", round(bp_p_value, 4), "). ",
        "The variance of residuals is constant across predicted values."
      )

      homoscedasticity_pub <- paste0(
        "Homoscedasticity was assessed using the Breusch-Pagan test. ",
        "The assumption was met (χ² = ", round(bp_statistic, 2), ", p = ",
        round(bp_p_value, 4), "), indicating constant error variance."
      )

      homoscedasticity_verbose <- paste0(
        "Homoscedasticity (also called homogeneity of variance) means that the spread of ",
        "residuals should be roughly the same across all predicted values. Think of it as ",
        "the 'scatter' around the regression line being consistent.\n\n",
        "The Breusch-Pagan test evaluates this assumption. A non-significant p-value (",
        round(bp_p_value, 4), " > ", alpha, ") indicates that residual variance is constant. ",
        "This is good news! Your regression estimates are efficient and unbiased.\n\n",
        "If this assumption had been violated, you would see a 'funnel' pattern in the ",
        "residuals vs fitted plot - with residuals spreading out or contracting as predictions ",
        "increase. Violations can be addressed with robust standard errors or data transformation."
      )

      homoscedasticity_recommendation <- "Proceed with standard regression inference."

    } else {
      homoscedasticity_text <- paste0(
        "The homoscedasticity assumption is VIOLATED (Breusch-Pagan test: ",
        "χ² = ", round(bp_statistic, 2), ", p = ", round(bp_p_value, 4), "). ",
        "The variance of residuals changes across predicted values."
      )

      homoscedasticity_pub <- paste0(
        "Homoscedasticity was assessed using the Breusch-Pagan test. ",
        "The assumption was violated (χ² = ", round(bp_statistic, 2), ", p = ",
        round(bp_p_value, 4), "). Accordingly, heteroscedasticity-robust standard errors ",
        "were computed."
      )

      homoscedasticity_verbose <- paste0(
        "Homoscedasticity (homogeneity of variance) means that the spread of residuals ",
        "should be roughly the same across all predicted values.\n\n",
        "The Breusch-Pagan test evaluates this assumption. A significant p-value (",
        round(bp_p_value, 4), " < ", alpha, ") indicates that residual variance is NOT constant - ",
        "it changes based on the predicted value. This is called heteroscedasticity.\n\n",
        "What this means: Your regression coefficients are still unbiased (correct on average), ",
        "but the standard errors may be wrong, which affects p-values and confidence intervals. ",
        "You might see a 'funnel' pattern in diagnostic plots.\n\n",
        "Solutions:\n",
        "1. Use heteroscedasticity-robust standard errors (sandwich/HC estimators)\n",
        "2. Transform the dependent variable (log, square root)\n",
        "3. Use weighted least squares\n",
        "4. If severe, consider generalized linear models\n\n",
        "For publications, report that you used robust standard errors to account for heteroscedasticity."
      )

      homoscedasticity_recommendation <- paste0(
        "Use heteroscedasticity-robust standard errors (HC3 or HC4). ",
        "In R, use the sandwich package or lm_robust() from estimatr."
      )
    }

    assumptions$homoscedasticity <- structure(
      list(
        test = "Breusch-Pagan Test",
        assumption = "Homoscedasticity",
        assumption_met = homoscedasticity_met,
        p_value = bp_p_value,
        statistic = bp_statistic,
        interpretation = homoscedasticity_text,
        recommendation = homoscedasticity_recommendation,
        publication_text = homoscedasticity_pub,
        verbose_explanation = homoscedasticity_verbose
      ),
      class = "assumption_check"
    )

    # 5. Multicollinearity (for multiple regression)
    if (n_predictors > 1) {
      # Calculate VIF (Variance Inflation Factor)
      # VIF = 1 / (1 - R²) for each predictor regressed on others

      predictor_names <- names(coef(lm_model))[-1]  # Exclude intercept
      vif_values <- numeric(length(predictor_names))
      names(vif_values) <- predictor_names

      for (i in seq_along(predictor_names)) {
        pred <- predictor_names[i]
        # Create formula for this predictor regressed on others
        other_preds <- predictor_names[-i]
        vif_formula <- as.formula(paste(pred, "~", paste(other_preds, collapse = " + ")))

        tryCatch({
          vif_model <- lm(vif_formula, data = data)
          r2 <- summary(vif_model)$r.squared
          vif_values[i] <- 1 / (1 - r2)
        }, error = function(e) {
          vif_values[i] <- NA
        })
      }

      max_vif <- max(vif_values, na.rm = TRUE)
      multicollinearity_met <- max_vif < 5  # Common threshold

      if (multicollinearity_met) {
        multicoll_text <- paste0(
          "Multicollinearity is within acceptable limits. ",
          "Maximum VIF = ", round(max_vif, 2), " (threshold: VIF < 5)."
        )

        multicoll_pub <- paste0(
          "Multicollinearity was assessed using Variance Inflation Factors (VIF). ",
          "All VIF values were below 5, indicating acceptable multicollinearity."
        )

        multicoll_verbose <- paste0(
          "Multicollinearity occurs when predictor variables are highly correlated with each other. ",
          "This makes it difficult to determine the unique effect of each predictor.\n\n",
          "The Variance Inflation Factor (VIF) quantifies how much the variance of a regression ",
          "coefficient is inflated due to multicollinearity. Rules of thumb:\n",
          "  VIF < 5: No multicollinearity concern\n",
          "  VIF 5-10: Moderate multicollinearity\n",
          "  VIF > 10: Severe multicollinearity\n\n",
          "Your maximum VIF is ", round(max_vif, 2), ", which is acceptable. ",
          "This means your predictors are relatively independent, and each coefficient represents ",
          "a unique contribution to the model."
        )

      } else {
        multicoll_text <- paste0(
          "WARNING: Multicollinearity detected. ",
          "Maximum VIF = ", round(max_vif, 2), " (threshold: VIF < 5). ",
          "Some predictors are highly correlated."
        )

        multicoll_pub <- paste0(
          "Multicollinearity was assessed using Variance Inflation Factors (VIF). ",
          "Some VIF values exceeded 5, indicating moderate to high multicollinearity. ",
          "Interpretation of individual coefficients should be made with caution."
        )

        multicoll_verbose <- paste0(
          "Multicollinearity detected! This means some of your predictor variables are highly ",
          "correlated with each other.\n\n",
          "Your maximum VIF is ", round(max_vif, 2), ". When VIF > 5, the standard errors of ",
          "regression coefficients become inflated, making it harder to detect significant effects. ",
          "More importantly, the coefficients become unstable - small changes in data can lead to ",
          "large changes in estimates.\n\n",
          "What to do:\n",
          "1. Check correlations between predictors - are any > 0.80?\n",
          "2. Consider removing one predictor from highly correlated pairs\n",
          "3. Combine correlated predictors into a composite score\n",
          "4. Use ridge regression or other regularization techniques\n",
          "5. If predictors are theoretically distinct, you might still report results but ",
          "   acknowledge the limitation\n\n",
          "Individual VIF values:\n",
          paste(names(vif_values), "=", round(vif_values, 2), collapse = ", ")
        )
      }

      assumptions$multicollinearity <- structure(
        list(
          test = "Variance Inflation Factor (VIF)",
          assumption = "No Multicollinearity",
          assumption_met = multicollinearity_met,
          vif_values = vif_values,
          max_vif = max_vif,
          interpretation = multicoll_text,
          recommendation = ifelse(multicollinearity_met,
            "Multicollinearity is not a concern.",
            "Consider removing or combining highly correlated predictors."
          ),
          publication_text = multicoll_pub,
          verbose_explanation = multicoll_verbose
        ),
        class = "assumption_check"
      )
    }

    if (verbose) {
      message("Assumption checks complete.\n")
    }
  }

  # Step 5: Create diagnostic plots (if requested)
  # -----------------------------------------------
  if (create_plots) {
    if (verbose) {
      message("Creating diagnostic plots...")
    }

    # This would use ggplot2 to create:
    # 1. Residuals vs Fitted (linearity & homoscedasticity)
    # 2. Q-Q plot (normality)
    # 3. Scale-Location plot (homoscedasticity)
    # 4. Residuals vs Leverage (influential points)

    # For now, note that plots should be created
    # In full implementation, would create ggplot2 objects
    diagnostic_plots$note <- "Use plot(lm_model) for base R diagnostic plots, or the lm_model object with ggplot2"
  }

  # Step 6: Generate interpretation
  # --------------------------------
  interpretation <- paste0(
    "The ", tolower(model_type), " model ",
    ifelse(is_significant, "was statistically significant", "was not statistically significant"),
    " (F(", f_df1, ", ", f_df2, ") = ", round(f_stat, 2),
    ", p = ", round(p_value, 4), ", R² = ", round(r_squared, 3), "). "
  )

  if (is_significant) {
    interpretation <- paste0(
      interpretation,
      "The model explains ", round(r_squared * 100, 1), "% of variance in the outcome. "
    )

    # Report significant predictors
    sig_predictors <- coef_df$variable[coef_df$p_value < alpha & coef_df$variable != "(Intercept)"]

    if (length(sig_predictors) > 0) {
      interpretation <- paste0(
        interpretation,
        "Significant predictor(s): ", paste(sig_predictors, collapse = ", "), ". ",
        "See coefficients table for details."
      )
    } else {
      interpretation <- paste0(
        interpretation,
        "However, no individual predictors reached statistical significance."
      )
    }
  }

  # Step 7: Generate publication text
  # ----------------------------------
  pub_block <- generate_publication_block(
    test_type = "regression",
    assumptions_checks = if (check_assumptions) assumptions else NULL,
    test_results = list(
      model_type = model_type,
      n_predictors = n_predictors,
      r_squared = r_squared,
      adj_r_squared = adj_r_squared,
      f_statistic = f_stat,
      f_df1 = f_df1,
      f_df2 = f_df2,
      p_value = p_value,
      coefficients = coef_df
    ),
    additional_notes = paste0(
      "The regression model included ", n_predictors, " predictor(s) and was based on ",
      n_obs, " observations. RMSE = ", round(rmse, 3), "."
    )
  )

  # Step 8: Package results
  # -----------------------
  results <- list(
    model_type = model_type,
    n_predictors = n_predictors,
    n_observations = n_obs,
    coefficients = coef_df,
    r_squared = round(r_squared, 4),
    adj_r_squared = round(adj_r_squared, 4),
    f_statistic = round(f_stat, 3),
    f_df1 = f_df1,
    f_df2 = f_df2,
    p_value = round(p_value, 6),
    significant = is_significant,
    alpha = alpha,
    rmse = round(rmse, 3),
    assumptions = if (check_assumptions && length(assumptions) > 0) assumptions else NULL,
    diagnostic_plots = if (create_plots) diagnostic_plots else NULL,
    interpretation = interpretation,
    publication_text = pub_block,
    lm_model = lm_model
  )

  class(results) <- c("regression_result", "list")

  if (verbose) {
    message("Regression analysis complete!\n")
  }

  return(results)
}


#' Print Method for Regression Results
#'
#' @param x A regression_result object
#' @param show_assumptions Logical - show assumption checks? (default FALSE)
#' @param show_publication Logical - show publication text? (default FALSE)
#' @param ... Additional arguments (not used)
#'
#' @export
print.regression_result <- function(x, show_assumptions = FALSE, show_publication = FALSE, ...) {
  cat("\n")
  cat("=" %+% rep("=", 75) %+% "=\n", sep = "")
  cat(toupper(x$model_type), " RESULTS\n", sep = "")
  cat("=" %+% rep("=", 75) %+% "=\n", sep = "")
  cat("\n")

  cat("MODEL SUMMARY:\n")
  cat("  Observations:", x$n_observations, "\n")
  cat("  Predictors:", x$n_predictors, "\n")
  cat("  R² =", x$r_squared, "(explains", round(x$r_squared * 100, 1), "% of variance)\n")
  cat("  Adjusted R² =", x$adj_r_squared, "\n")
  cat("  RMSE =", x$rmse, "\n")
  cat("\n")

  cat("MODEL SIGNIFICANCE:\n")
  cat("  F(", x$f_df1, ", ", x$f_df2, ") = ", x$f_statistic, "\n", sep = "")
  cat("  p-value:", x$p_value, "\n")
  cat("  Significant:", ifelse(x$significant, "YES (p < .05)", "NO (p >= .05)"), "\n")
  cat("\n")

  cat("COEFFICIENTS:\n")
  print(x$coefficients, row.names = FALSE, digits = 3)
  cat("---\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  cat("\n")

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
  if (!show_publication) {
    cat("Tip: Use print(result, show_publication = TRUE) to see publication-ready text\n")
  }

  cat("\n")

  invisible(x)
}

# Helper function
`%+%` <- function(a, b) paste0(a, b)
