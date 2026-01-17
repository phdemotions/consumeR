#' Assumption Checking Wrapper
#'
#' This file provides a unified interface for checking statistical assumptions
#' across different test types. It eliminates duplicated assumption-checking
#' logic across analysis functions.
#'
#' @family infrastructure
#' @keywords internal

#' Check Statistical Assumptions for Analysis
#'
#' Unified wrapper that sequences appropriate assumption checks based on test type.
#' Calls existing check_normality(), check_homogeneity_of_variance(), and
#' check_independence() functions in the correct order.
#'
#' @param test_type Character. One of: "t_test", "anova", "regression", "correlation"
#' @param data Data frame or NULL. Required for independence checks
#' @param groups Character vector or NULL. Group variable for homogeneity tests
#' @param residuals Numeric vector or NULL. Residuals for normality tests
#' @param predictors_data Data frame or NULL. Predictors for VIF/multicollinearity
#' @param alpha Numeric. Significance level (default: 0.05)
#' @param data_structure Character. "cross_sectional" or "longitudinal" (default: "cross_sectional")
#' @param verbose Logical. Show beginner-friendly warnings? (default: TRUE)
#'
#' @return S3 object of class c("assumptions_result", "list") with components:
#'   \describe{
#'     \item{independence}{Result from check_independence()}
#'     \item{normality}{Result from check_normality()}
#'     \item{homogeneity}{Result from check_homogeneity_of_variance() (if applicable)}
#'     \item{multicollinearity}{Result from check_vif() (if applicable)}
#'     \item{overall_pass}{Logical indicating if all assumptions met}
#'     \item{test_type}{Character string of test type}
#'     \item{warnings}{Character vector of beginner-friendly warnings}
#'   }
#'
#' @examples
#' # T-test assumptions
#' data <- data.frame(
#'   group = rep(c("A", "B"), each = 50),
#'   value = c(rnorm(50, 10, 2), rnorm(50, 12, 2))
#' )
#' model <- lm(value ~ group, data = data)
#' check_test_assumptions(
#'   test_type = "t_test",
#'   data = data,
#'   groups = "group",
#'   residuals = residuals(model)
#' )
#'
#' @export
check_test_assumptions <- function(
  test_type = c("t_test", "anova", "regression", "correlation"),
  data = NULL,
  groups = NULL,
  residuals = NULL,
  predictors_data = NULL,
  alpha = 0.05,
  data_structure = "cross_sectional",
  verbose = TRUE
) {
  test_type <- match.arg(test_type)

  # Validate required arguments for test type
  if (test_type %in% c("t_test", "anova", "regression") && is.null(residuals)) {
    rlang::abort(
      "residuals required for normality testing in t-tests, ANOVA, and regression",
      class = "missing_residuals_error"
    )
  }

  if (test_type %in% c("t_test", "anova") && is.null(groups)) {
    rlang::abort(
      "groups required for homogeneity testing in t-tests and ANOVA",
      class = "missing_groups_error"
    )
  }

  # Initialize result structure
  result <- list(
    independence = NULL,
    normality = NULL,
    homogeneity = NULL,
    multicollinearity = NULL,
    overall_pass = TRUE,
    test_type = test_type,
    warnings = character(0)
  )

  # 1. Independence (if data provided)
  if (!is.null(data)) {
    data_for_independence <- NULL
    if (is.data.frame(data)) {
      numeric_cols <- vapply(data, is.numeric, logical(1))
      if (any(numeric_cols)) {
        data_for_independence <- data[[which(numeric_cols)[1]]]
      }
    } else if (is.numeric(data)) {
      data_for_independence <- data
    }

    independence_structure <- switch(
      data_structure,
      cross_sectional = "cross-sectional survey",
      longitudinal = "repeated measures",
      data_structure
    )

    result$independence <- check_independence(
      data = data_for_independence,
      data_structure = independence_structure
    )
    if (!result$independence$assumption_met) {
      result$overall_pass <- FALSE
      result$warnings <- c(result$warnings, "Independence assumption may be violated")
    }
  }

  # 2. Normality (test residuals, not raw groups - more statistically correct)
  if (!is.null(residuals)) {
    result$normality <- check_normality(residuals, alpha = alpha)
    if (!result$normality$assumption_met) {
      result$overall_pass <- FALSE
      result$warnings <- c(result$warnings, "Normality assumption may be violated")
    }
  }

  # 3. Homogeneity of variance (for t-test/ANOVA)
  if (test_type %in% c("t_test", "anova") && !is.null(data) && !is.null(groups)) {
    homogeneity_groups <- groups
    if (is.data.frame(data) && is.character(groups) && length(groups) == 1 &&
        groups %in% names(data)) {
      homogeneity_groups <- data[[groups]]
    }

    homogeneity_data <- data
    if (is.data.frame(data)) {
      numeric_cols <- vapply(data, is.numeric, logical(1))
      if (any(numeric_cols)) {
        homogeneity_data <- data[[which(numeric_cols)[1]]]
      }
    }

    result$homogeneity <- check_homogeneity_of_variance(
      data = homogeneity_data,
      groups = homogeneity_groups,
      alpha = alpha
    )
    if (!result$homogeneity$assumption_met) {
      result$overall_pass <- FALSE
      result$warnings <- c(result$warnings, "Homogeneity of variance assumption may be violated")
    }
  }

  # 4. Multicollinearity (for regression)
  if (test_type == "regression" && !is.null(predictors_data)) {
    # Note: check_vif() not yet implemented in current codebase
    # Placeholder for future implementation
    # result$multicollinearity <- check_vif(predictors_data)
  }

  # Print warnings if verbose
  if (verbose && length(result$warnings) > 0) {
    cli::cli_alert_warning("Assumption checks:")
    for (w in result$warnings) {
      cli::cli_bullets(c("x" = w))
    }
  }

  structure(result, class = c("assumptions_result", "list"))
}
