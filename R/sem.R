#' Structural Equation Modeling (SEM) Path Analysis
#'
#' @description
#' Fit structural equation models with latent and observed variables using
#' lavaan. Provides publication-ready output for path analysis, mediation
#' models, and confirmatory factor analysis with multiple outcomes.
#'
#' @param model Lavaan model syntax (character string) or list of equations.
#'   See \code{lavaan::model.syntax} for details. Can use simplified syntax:
#'   \code{c("y ~ x + m", "m ~ x")} or full lavaan syntax with latent variables.
#' @param data Data frame containing all variables in the model.
#' @param estimator Estimator to use. Default is "ML" (maximum likelihood).
#'   Options: "ML", "MLM" (robust), "MLR" (robust), "ULS", "DWLS", "WLS".
#' @param se Standard error method. Default is "standard". Use "bootstrap" for
#'   bootstrap standard errors (recommended for indirect effects).
#' @param bootstrap Number of bootstrap samples if \code{se = "bootstrap"}.
#'   Minimum 1000, 5000+ recommended for publication.
#' @param missing How to handle missing data. Default is "listwise" (complete
#'   cases). Use "fiml" for full information maximum likelihood (only with ML
#'   estimator).
#' @param std_estimates Logical. If TRUE (default), returns standardized
#'   estimates in addition to unstandardized.
#' @param fit_measures Character vector of fit indices to include. Default is
#'   c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"). See
#'   \code{lavaan::fitMeasures} for all available options.
#' @param seed Random seed for reproducibility (used with bootstrap).
#' @param ... Additional arguments passed to \code{lavaan::sem()}.
#'
#' @return Object of class \code{sem_result} (extends lavaan object) with
#'   additional elements:
#'   \describe{
#'     \item{params}{Tibble of parameter estimates with CIs}
#'     \item{fit}{Tibble of model fit indices}
#'     \item{r2}{Tibble of R^2 for endogenous variables}
#'     \item{indirect_effects}{Tibble of indirect effects (if present)}
#'     \item{model_syntax}{Original model specification}
#'     \item{interpretation}{Publication-ready interpretation text}
#'     \item{lavaan_fit}{Original lavaan fit object}
#'   }
#'
#' @details
#' This function provides a simplified interface to lavaan::sem() with sensible
#' defaults for consumer psychology research. It automatically:
#' \itemize{
#'   \item Validates model syntax and data
#'   \item Calculates standardized and unstandardized estimates
#'   \item Extracts fit indices following JCP 2026 guidelines
#'   \item Identifies and extracts indirect effects
#'   \item Generates publication-ready interpretation
#'   \item Handles bootstrap CIs for indirect effects
#' }
#'
#' **Model Syntax**:
#' \itemize{
#'   \item Regression: \code{y ~ x + z}
#'   \item Latent variable: \code{latent =~ indicator1 + indicator2}
#'   \item Covariance: \code{x ~~ y}
#'   \item Indirect effect (labeled): \code{y ~ b*m; m ~ a*x; ab := a*b}
#' }
#'
#' **Fit Index Guidelines (Hu & Bentler, 1999)**:
#' \itemize{
#'   \item CFI >= 0.95 (good), >= 0.90 (acceptable)
#'   \item TLI >= 0.95 (good), >= 0.90 (acceptable)
#'   \item RMSEA <= 0.06 (good), <= 0.08 (acceptable)
#'   \item SRMR <= 0.08 (good), <= 0.10 (acceptable)
#' }
#'
#' @references
#' Rosseel, Y. (2012). lavaan: An R package for structural equation modeling.
#'   Journal of Statistical Software, 48(2), 1-36.
#'
#' Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
#'   covariance structure analysis. Structural Equation Modeling, 6(1), 1-55.
#'
#' @examples
#' \dontrun{
#' # Simple mediation model
#' model <- "
#'   # Direct effects
#'   satisfaction ~ b*quality + c*price
#'   quality ~ a*brand_reputation
#'
#'   # Indirect effect
#'   indirect := a * b
#' "
#'
#' result <- run_sem(model, data = consumer_survey, se = "bootstrap",
#'                   bootstrap = 5000, seed = 123)
#' print(result)
#'
#' # Path model with multiple outcomes
#' model2 <- c(
#'   "purchase_intent ~ attitude + subjective_norm",
#'   "attitude ~ ad_exposure + prior_belief",
#'   "subjective_norm ~ ad_exposure"
#' )
#'
#' result2 <- run_sem(model2, data = mydata)
#' tidy_sem(result2)
#' }
#'
#' @export
run_sem <- function(model,
                    data,
                    estimator = "ML",
                    se = c("standard", "bootstrap", "robust"),
                    bootstrap = 5000,
                    missing = c("listwise", "fiml"),
                    std_estimates = TRUE,
                    fit_measures = c("chisq", "df", "pvalue", "cfi", "tli",
                                     "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
                                     "srmr"),
                    seed = NULL,
                    ...) {

  # Input validation
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame", class = "invalid_input")
  }

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    rlang::abort(
      "Package 'lavaan' is required for SEM. Install with: install.packages('lavaan')",
      class = "missing_package"
    )
  }

  se <- match.arg(se)
  missing <- match.arg(missing)

  # Convert vector of equations to lavaan syntax
  if (is.character(model) && length(model) > 1) {
    model <- paste(model, collapse = "\n")
  }

  if (!is.character(model) || nchar(model) == 0) {
    rlang::abort("`model` must be a non-empty character string",
                 class = "invalid_input")
  }

  # Validate bootstrap samples
  if (se == "bootstrap" && bootstrap < 1000) {
    rlang::abort(
      "`bootstrap` must be at least 1000 for reliable inference. 5000+ recommended.",
      class = "invalid_input"
    )
  }

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Prepare lavaan arguments
  lav_args <- list(
    model = model,
    data = data,
    estimator = estimator,
    se = se,
    missing = missing,
    ...
  )

  # Add bootstrap if requested
  if (se == "bootstrap") {
    lav_args$bootstrap <- bootstrap
  }

  # Fit model
  fit <- tryCatch(
    do.call(lavaan::sem, lav_args),
    error = function(e) {
      rlang::abort(
        sprintf("SEM model failed to converge: %s", e$message),
        class = "convergence_error"
      )
    }
  )

  # Check convergence
  if (!lavaan::lavInspect(fit, "converged")) {
    rlang::warn(
      "Model did not converge. Results may be unreliable.",
      class = "convergence_warning"
    )
  }

  # Extract parameter estimates
  params <- extract_sem_parameters(fit, std_estimates = std_estimates)

  # Extract fit indices
  fit_stats <- extract_sem_fit(fit, measures = fit_measures)

  # Extract R^2 for endogenous variables
  r2_vals <- extract_sem_r2(fit)

  # Extract indirect effects if defined
  indirect <- extract_sem_indirect(fit)

  # Generate interpretation
  interpretation <- generate_sem_interpretation(fit_stats, params, indirect)

  # Create result object
  result <- list(
    params = params,
    fit = fit_stats,
    r2 = r2_vals,
    indirect_effects = indirect,
    model_syntax = model,
    estimator = estimator,
    se_method = se,
    interpretation = interpretation,
    lavaan_fit = fit
  )

  class(result) <- c("sem_result", "list")
  result
}


#' Extract SEM Parameter Estimates
#'
#' @param fit Fitted lavaan object
#' @param std_estimates Logical, include standardized estimates
#'
#' @return Tibble of parameter estimates
#' @keywords internal
extract_sem_parameters <- function(fit, std_estimates = TRUE) {

  # Get parameter table
  params <- lavaan::parameterEstimates(fit,
                                       standardized = std_estimates,
                                       ci = TRUE)

  # Convert to tibble and clean
  params <- tibble::as_tibble(params)

  # Filter to structural and measurement parameters (exclude variances/covariances for now)
  params <- dplyr::filter(params, .data$op %in% c("~", "=~", ":="))

  # Create readable labels
  params <- dplyr::mutate(
    params,
    parameter = dplyr::case_when(
      .data$op == "~" ~ paste(.data$lhs, "<~", .data$rhs),
      .data$op == "=~" ~ paste(.data$lhs, "=~", .data$rhs),
      .data$op == ":=" ~ .data$lhs,
      TRUE ~ paste(.data$lhs, .data$op, .data$rhs)
    ),
    significant = .data$pvalue < 0.05
  )

  # Select relevant columns
  cols <- c("parameter", "est", "se", "z", "pvalue", "ci.lower", "ci.upper",
            "significant")

  if (std_estimates && "std.all" %in% names(params)) {
    cols <- c(cols, "std.all")
  }

  params <- dplyr::select(params, dplyr::any_of(cols))

  # Rename for clarity
  params <- dplyr::rename(
    params,
    estimate = "est",
    p_value = "pvalue"
  )

  if ("std.all" %in% names(params)) {
    params <- dplyr::rename(params, std_estimate = "std.all")
  }

  params
}


#' Extract SEM Fit Indices
#'
#' @param fit Fitted lavaan object
#' @param measures Character vector of fit measures to extract
#'
#' @return Tibble of fit indices with interpretation
#' @keywords internal
extract_sem_fit <- function(fit, measures) {

  # Get all fit measures
  all_fit <- lavaan::fitMeasures(fit)

  # Extract requested measures
  fit_vals <- all_fit[measures]

  # Create tibble
  fit_tbl <- tibble::tibble(
    measure = names(fit_vals),
    value = as.numeric(fit_vals)
  )

  # Add interpretation for common indices
  fit_tbl <- dplyr::mutate(
    fit_tbl,
    interpretation = dplyr::case_when(
      measure == "cfi" & value >= 0.95 ~ "Good fit",
      measure == "cfi" & value >= 0.90 ~ "Acceptable fit",
      measure == "cfi" ~ "Poor fit",
      measure == "tli" & value >= 0.95 ~ "Good fit",
      measure == "tli" & value >= 0.90 ~ "Acceptable fit",
      measure == "tli" ~ "Poor fit",
      measure == "rmsea" & value <= 0.06 ~ "Good fit",
      measure == "rmsea" & value <= 0.08 ~ "Acceptable fit",
      measure == "rmsea" ~ "Poor fit",
      measure == "srmr" & value <= 0.08 ~ "Good fit",
      measure == "srmr" & value <= 0.10 ~ "Acceptable fit",
      measure == "srmr" ~ "Poor fit",
      TRUE ~ ""
    )
  )

  fit_tbl
}


#' Extract R^2 for Endogenous Variables
#'
#' @param fit Fitted lavaan object
#'
#' @return Tibble of R^2 values
#' @keywords internal
extract_sem_r2 <- function(fit) {

  r2_vals <- lavaan::lavInspect(fit, "r2")

  if (length(r2_vals) == 0) {
    return(NULL)
  }

  tibble::tibble(
    variable = names(r2_vals),
    r_squared = as.numeric(r2_vals)
  )
}


#' Extract Indirect Effects from SEM
#'
#' @param fit Fitted lavaan object
#'
#' @return Tibble of indirect effects with CIs, or NULL if none defined
#' @keywords internal
extract_sem_indirect <- function(fit) {

  # Get parameter table
  params <- lavaan::parameterEstimates(fit, ci = TRUE)

  # Filter to defined parameters (indirect effects)
  indirect <- dplyr::filter(params, .data$op == ":=")

  if (nrow(indirect) == 0) {
    return(NULL)
  }

  # Create clean tibble
  indirect <- tibble::tibble(
    effect = indirect$lhs,
    estimate = indirect$est,
    se = indirect$se,
    z = indirect$z,
    p_value = indirect$pvalue,
    ci_lower = indirect$ci.lower,
    ci_upper = indirect$ci.upper,
    significant = indirect$pvalue < 0.05
  )

  indirect
}


#' Generate SEM Interpretation Text
#'
#' @param fit_stats Tibble of fit statistics
#' @param params Tibble of parameters
#' @param indirect Tibble of indirect effects (or NULL)
#'
#' @return Character string with interpretation
#' @keywords internal
generate_sem_interpretation <- function(fit_stats, params, indirect) {

  # Model fit interpretation
  cfi <- fit_stats$value[fit_stats$measure == "cfi"]
  rmsea <- fit_stats$value[fit_stats$measure == "rmsea"]

  fit_text <- if (length(cfi) > 0 && length(rmsea) > 0) {
    if (cfi >= 0.95 && rmsea <= 0.06) {
      "The model demonstrated good fit to the data"
    } else if (cfi >= 0.90 && rmsea <= 0.08) {
      "The model demonstrated acceptable fit to the data"
    } else {
      "The model demonstrated poor fit to the data"
    }
  } else {
    "Model fit indices were calculated"
  }

  # Significant paths
  sig_params <- dplyr::filter(params, .data$significant)
  n_sig <- nrow(sig_params)

  path_text <- sprintf(
    "with %d significant path%s",
    n_sig,
    if (n_sig == 1) "" else "s"
  )

  # Indirect effects
  indirect_text <- if (!is.null(indirect)) {
    sig_indirect <- dplyr::filter(indirect, .data$significant)
    n_sig_ind <- nrow(sig_indirect)

    if (n_sig_ind > 0) {
      sprintf(
        " and %d significant indirect effect%s",
        n_sig_ind,
        if (n_sig_ind == 1) "" else "s"
      )
    } else {
      ""
    }
  } else {
    ""
  }

  paste0(fit_text, " ", path_text, indirect_text, ".")
}


#' Extract Publication-Ready SEM Results
#'
#' @description
#' Extract and format SEM results for publication. Returns parameter estimates,
#' fit indices, and R^2 values in a publication-ready format.
#'
#' @param sem_result Result from \code{run_sem()}.
#' @param parameters Logical. Include parameter estimates table? Default TRUE.
#' @param fit Logical. Include fit indices? Default TRUE.
#' @param r2 Logical. Include R^2 values? Default TRUE.
#' @param indirect Logical. Include indirect effects? Default TRUE.
#' @param standardized Logical. Use standardized estimates? Default TRUE.
#'
#' @return List with tibbles for each requested component.
#'
#' @examples
#' \dontrun{
#' result <- run_sem(model, data = mydata)
#' tidy_sem(result)
#' }
#'
#' @export
tidy_sem <- function(sem_result,
                     parameters = TRUE,
                     fit = TRUE,
                     r2 = TRUE,
                     indirect = TRUE,
                     standardized = TRUE) {

  if (!inherits(sem_result, "sem_result")) {
    rlang::abort(
      "`sem_result` must be output from run_sem()",
      class = "invalid_input"
    )
  }

  output <- list()

  if (parameters) {
    params <- sem_result$params

    # Select estimate column based on standardized preference
    if (standardized && "std_estimate" %in% names(params)) {
      params <- dplyr::select(
        params,
        parameter, estimate = "std_estimate", se, z, p_value,
        ci.lower, ci.upper, significant
      )
    }

    output$parameters <- params
  }

  if (fit) {
    output$fit_indices <- sem_result$fit
  }

  if (r2 && !is.null(sem_result$r2)) {
    output$r_squared <- sem_result$r2
  }

  if (indirect && !is.null(sem_result$indirect_effects)) {
    output$indirect_effects <- sem_result$indirect_effects
  }

  output
}


#' Compare SEM Models
#'
#' @description
#' Compare nested or non-nested SEM models using likelihood ratio tests (for
#' nested models) or information criteria (AIC, BIC).
#'
#' @param ... Two or more \code{sem_result} objects from \code{run_sem()}.
#' @param model_names Optional character vector of model names.
#'
#' @return Tibble comparing models with fit indices and difference tests.
#'
#' @examples
#' \dontrun{
#' model1 <- run_sem(syntax1, data = mydata)
#' model2 <- run_sem(syntax2, data = mydata)
#'
#' compare_sem_models(model1, model2,
#'                    model_names = c("Partial mediation", "Full mediation"))
#' }
#'
#' @export
compare_sem_models <- function(..., model_names = NULL) {

  models <- list(...)

  # Validate inputs
  if (length(models) < 2) {
    rlang::abort(
      "At least two models are required for comparison",
      class = "invalid_input"
    )
  }

  if (!all(vapply(models, inherits, logical(1), "sem_result"))) {
    rlang::abort(
      "All inputs must be sem_result objects from run_sem()",
      class = "invalid_input"
    )
  }

  # Validate model_names length BEFORE calling lavaan::anova
  if (!is.null(model_names)) {
    if (length(model_names) != length(models)) {
      rlang::abort(
        "`model_names` must have same length as number of models",
        class = "invalid_input"
      )
    }
  }

  # Extract lavaan fits
  fits <- lapply(models, function(x) x$lavaan_fit)

  # Use lavaan's anova for comparison
  comparison <- do.call(lavaan::anova, fits)

  # Convert to tibble
  comp_tbl <- tibble::as_tibble(comparison, rownames = "model")

  # Add model names if provided
  if (!is.null(model_names)) {
    comp_tbl$model_name <- model_names
  }

  comp_tbl
}


#' Print Method for SEM Results
#'
#' @param x Object of class \code{sem_result}
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#' @export
print.sem_result <- function(x, ...) {

  cat("Structural Equation Model\n")
  cat("==========================\n\n")

  cat("Estimator:", x$estimator, "\n")
  cat("Standard errors:", x$se_method, "\n\n")

  # Model fit
  cat("Model Fit Indices:\n")
  cat("------------------\n")
  print(x$fit, n = Inf)
  cat("\n")

  # R^2
  if (!is.null(x$r2)) {
    cat("R^2 for Endogenous Variables:\n")
    cat("----------------------------\n")
    print(x$r2, n = Inf)
    cat("\n")
  }

  # Parameter estimates
  cat("Parameter Estimates:\n")
  cat("-------------------\n")
  print(x$params, n = Inf)
  cat("\n")

  # Indirect effects
  if (!is.null(x$indirect_effects)) {
    cat("Indirect Effects:\n")
    cat("----------------\n")
    print(x$indirect_effects, n = Inf)
    cat("\n")
  }

  # Interpretation
  cat("Interpretation:\n")
  cat("--------------\n")
  cat(x$interpretation, "\n")

  invisible(x)
}
