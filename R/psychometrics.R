#' Psychometric Analysis Functions
#'
#' @description
#' Advanced psychometric functions for confirmatory factor analysis (CFA),
#' exploratory factor analysis diagnostics, and batch reliability assessment.
#'
#' @name psychometrics
NULL


#' EFA Diagnostics: KMO, Bartlett's Test, Parallel Analysis
#'
#' @description
#' Runs comprehensive pre-EFA diagnostics to assess whether data is suitable
#' for factor analysis. Includes Kaiser-Meyer-Olkin (KMO) measure, Bartlett's
#' test of sphericity, and parallel analysis for determining number of factors.
#'
#' @param df_items Data frame containing only the items to be factor analyzed
#'   (all columns must be numeric)
#' @param n_obs Integer. Sample size (if df_items is a correlation matrix).
#'   If df_items is a data frame, this is ignored and nrow(df_items) is used.
#' @param fa_method Character. Factor analysis method for parallel analysis:
#'   "ml" (maximum likelihood, default), "minres", "pa" (principal axis)
#'
#' @return A list with class "efa_diagnostics" containing:
#' \itemize{
#'   \item \code{kmo}: KMO results (MSA values, overall KMO)
#'   \item \code{bartlett}: Bartlett's test results (chi-square, df, p-value)
#'   \item \code{parallel}: Parallel analysis results (suggested factors/components)
#'   \item \code{suitable}: Logical indicating if data suitable for EFA
#'   \item \code{interpretation}: Plain English interpretation
#' }
#'
#' @details
#' **Kaiser-Meyer-Olkin (KMO):**
#' - Measures sampling adequacy
#' - Overall KMO ≥ 0.80: Excellent
#' - Overall KMO ≥ 0.70: Good
#' - Overall KMO ≥ 0.60: Adequate
#' - Overall KMO < 0.60: Inadequate (factor analysis not recommended)
#'
#' **Bartlett's Test of Sphericity:**
#' - Tests if correlation matrix is significantly different from identity matrix
#' - Significant p-value (p < .05): Good, correlations exist
#' - Non-significant p: Bad, items don't correlate (factor analysis inappropriate)
#'
#' **Parallel Analysis:**
#' - Compares eigenvalues from data to random data
#' - Determines optimal number of factors to retain
#' - More accurate than Kaiser criterion (eigenvalue > 1)
#'
#' @examples
#' \dontrun{
#' # Prepare item data
#' items <- customer_survey[, c("q1", "q2", "q3", "q4", "q5", "q6")]
#'
#' # Run diagnostics
#' diagnostics <- efa_diagnostics(items)
#' print(diagnostics)
#'
#' # Check suitability
#' if (diagnostics$suitable) {
#'   # Proceed with EFA
#'   efa_result <- perform_efa(items, n_factors = diagnostics$parallel$nfact)
#' }
#' }
#'
#' @export
efa_diagnostics <- function(df_items, n_obs = NULL, fa_method = "ml") {
  # Check for psych package
  if (!requireNamespace("psych", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'psych' is required for EFA diagnostics",
      "i" = "Install with: install.packages('psych')"
    ))
  }

  # Validate input
  if (is.matrix(df_items) && is.null(n_obs)) {
    rlang::abort(c(
      "If df_items is a correlation matrix, n_obs must be specified",
      "i" = "Provide the sample size used to compute the correlation matrix"
    ))
  }

  if (is.data.frame(df_items)) {
    # Check all numeric
    non_numeric <- names(df_items)[!vapply(df_items, is.numeric, logical(1))]
    if (length(non_numeric) > 0) {
      rlang::abort(c(
        "All items must be numeric",
        "x" = "Non-numeric columns: {paste(non_numeric, collapse = ', ')}"
      ))
    }

    # Remove rows with missing values
    df_complete <- stats::na.omit(df_items)
    n_removed <- nrow(df_items) - nrow(df_complete)
    if (n_removed > 0) {
      message("Removed ", n_removed, " rows with missing values")
    }

    n_obs <- nrow(df_complete)
    cor_matrix <- stats::cor(df_complete)
  } else {
    cor_matrix <- df_items
    df_complete <- NULL
  }

  # 1. Kaiser-Meyer-Olkin (KMO) Test
  message("\nRunning KMO test...")
  kmo_result <- psych::KMO(cor_matrix)

  overall_kmo <- kmo_result$MSA
  kmo_interpretation <- if (overall_kmo >= 0.90) {
    "Excellent"
  } else if (overall_kmo >= 0.80) {
    "Great"
  } else if (overall_kmo >= 0.70) {
    "Good"
  } else if (overall_kmo >= 0.60) {
    "Adequate"
  } else if (overall_kmo >= 0.50) {
    "Marginal"
  } else {
    "Unacceptable"
  }

  message("  Overall KMO = ", round(overall_kmo, 3), " (", kmo_interpretation, ")")

  # 2. Bartlett's Test of Sphericity
  message("\nRunning Bartlett's test of sphericity...")
  bartlett_result <- psych::cortest.bartlett(cor_matrix, n = n_obs)

  bartlett_suitable <- bartlett_result$p.value < 0.05

  message("  χ² = ", round(bartlett_result$chisq, 2),
          ", df = ", bartlett_result$df,
          ", p ", if (bartlett_result$p.value < 0.001) "< .001" else paste("=", round(bartlett_result$p.value, 3)))

  if (bartlett_suitable) {
    message("  Result: Significant (good - correlations exist)")
  } else {
    message("  Result: Non-significant (bad - insufficient correlations)")
  }

  # 3. Parallel Analysis
  message("\nRunning parallel analysis...")

  if (!is.null(df_complete)) {
    pa_result <- psych::fa.parallel(
      df_complete,
      fm = fa_method,
      fa = "both",  # Both FA and PCA
      n.iter = 20,
      plot = FALSE,
      quant = 0.95
    )
  } else {
    pa_result <- psych::fa.parallel(
      cor_matrix,
      n.obs = n_obs,
      fm = fa_method,
      fa = "both",
      n.iter = 20,
      plot = FALSE,
      quant = 0.95
    )
  }

  n_factors_suggested <- pa_result$nfact
  n_components_suggested <- pa_result$ncomp

  message("  Suggested factors (FA): ", n_factors_suggested)
  message("  Suggested components (PCA): ", n_components_suggested)

  # Overall suitability
  suitable <- overall_kmo >= 0.60 && bartlett_suitable

  # Generate interpretation
  interpretation <- paste0(
    "EFA DIAGNOSTICS SUMMARY\n",
    "======================\n\n",
    "Sample size: N = ", n_obs, "\n",
    "Number of items: ", ncol(cor_matrix), "\n\n",
    "KAISER-MEYER-OLKIN (KMO) TEST\n",
    "Overall KMO = ", round(overall_kmo, 3), " (", kmo_interpretation, ")\n"
  )

  if (overall_kmo < 0.60) {
    interpretation <- paste0(interpretation,
      "⚠ WARNING: KMO < 0.60 indicates sampling inadequacy.\n",
      "Factor analysis may not be appropriate for this data.\n\n"
    )
  } else {
    interpretation <- paste0(interpretation,
      "✓ KMO is adequate for factor analysis.\n\n"
    )
  }

  interpretation <- paste0(interpretation,
    "BARTLETT'S TEST OF SPHERICITY\n",
    "χ²(", bartlett_result$df, ") = ", round(bartlett_result$chisq, 2),
    ", p ", if (bartlett_result$p.value < 0.001) "< .001\n" else paste("=", round(bartlett_result$p.value, 3), "\n")
  )

  if (bartlett_suitable) {
    interpretation <- paste0(interpretation,
      "✓ Significant result indicates correlations exist among items.\n\n"
    )
  } else {
    interpretation <- paste0(interpretation,
      "⚠ WARNING: Non-significant result suggests insufficient correlations.\n",
      "Factor analysis may not be appropriate.\n\n"
    )
  }

  interpretation <- paste0(interpretation,
    "PARALLEL ANALYSIS\n",
    "Suggested number of factors: ", n_factors_suggested, "\n",
    "Suggested number of components: ", n_components_suggested, "\n\n"
  )

  if (suitable) {
    interpretation <- paste0(interpretation,
      "RECOMMENDATION: ✓ Data is SUITABLE for factor analysis.\n",
      "Proceed with EFA using ", n_factors_suggested, " factor", if (n_factors_suggested != 1) "s" else "", ".\n"
    )
  } else {
    interpretation <- paste0(interpretation,
      "RECOMMENDATION: ✗ Data may NOT be suitable for factor analysis.\n",
      "Consider revising items or collecting more data.\n"
    )
  }

  message("\n", interpretation)

  # Return results
  structure(
    list(
      kmo = list(
        overall_msa = overall_kmo,
        item_msa = kmo_result$MSAi,
        interpretation = kmo_interpretation
      ),
      bartlett = list(
        chisq = bartlett_result$chisq,
        df = bartlett_result$df,
        p_value = bartlett_result$p.value,
        suitable = bartlett_suitable
      ),
      parallel = list(
        nfact = n_factors_suggested,
        ncomp = n_components_suggested,
        fa_values = pa_result$fa.values,
        pc_values = pa_result$pc.values
      ),
      n_obs = n_obs,
      n_items = ncol(cor_matrix),
      suitable = suitable,
      interpretation = interpretation
    ),
    class = c("efa_diagnostics", "list")
  )
}


#' Run Confirmatory Factor Analysis (CFA)
#'
#' @description
#' Wrapper around lavaan::cfa() for confirmatory factor analysis.
#' Provides sensible defaults for consumer research while allowing
#' full customization via pass-through arguments.
#'
#' @param model Character string specifying the CFA model in lavaan syntax
#' @param data Data frame containing observed variables
#' @param estimator Character. Estimator to use (default "MLR" = robust ML)
#' @param missing Character. Missing data handling (default "fiml" = full information ML)
#' @param std.lv Logical. Standardize latent variables? (default TRUE)
#' @param ... Additional arguments passed to lavaan::cfa()
#'
#' @return lavaan fit object (class lavaan)
#'
#' @details
#' **Recommended defaults:**
#' - estimator = "MLR": Maximum likelihood with robust (Huber-White) SEs
#' - missing = "fiml": Full information maximum likelihood for missing data
#' - std.lv = TRUE: Standardize latent variables (makes loadings interpretable)
#'
#' **Alternative estimators:**
#' - "ML": Standard maximum likelihood (faster, but not robust)
#' - "WLSMV": Weighted least squares (for categorical data)
#' - "ULS": Unweighted least squares (no distributional assumptions)
#'
#' **lavaan model syntax:**
#' ```
#' # Latent variable defined by items
#' satisfaction =~ q1 + q2 + q3
#' loyalty =~ q4 + q5 + q6
#'
#' # Covariances
#' satisfaction ~~ loyalty
#' ```
#'
#' @examples
#' \dontrun{
#' # Define CFA model
#' model <- "
#'   # Latent variables
#'   satisfaction =~ sat1 + sat2 + sat3
#'   loyalty =~ loy1 + loy2 + loy3
#'
#'   # Covariance
#'   satisfaction ~~ loyalty
#' "
#'
#' # Run CFA
#' fit <- run_cfa(model, data = customer_survey)
#'
#' # Get fit indices
#' fit_indices <- tidy_cfa_fit(fit)
#' }
#'
#' @export
run_cfa <- function(model,
                    data,
                    estimator = "MLR",
                    missing = "fiml",
                    std.lv = TRUE,
                    ...) {
  # Check for lavaan package
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'lavaan' is required for CFA",
      "i" = "Install with: install.packages('lavaan')"
    ))
  }

  # Validate inputs
  if (!is.character(model) || length(model) != 1) {
    rlang::abort("model must be a character string in lavaan syntax")
  }

  if (!is.data.frame(data)) {
    rlang::abort(c(
      "data must be a data frame",
      "x" = "You provided {class(data)[1]}"
    ))
  }

  message("\nRunning CFA with estimator = ", estimator, ", missing = ", missing, "...")

  # Run CFA
  fit <- lavaan::cfa(
    model = model,
    data = data,
    estimator = estimator,
    missing = missing,
    std.lv = std.lv,
    ...
  )

  message("CFA complete. Use tidy_cfa_fit() to extract fit indices.")

  fit
}


#' Extract Tidy CFA Fit Indices
#'
#' @description
#' Extracts key fit indices from a lavaan CFA model and returns them
#' in a tidy, single-row tibble format.
#'
#' @param fit lavaan fit object (from run_cfa() or lavaan::cfa())
#'
#' @return A tibble with one row containing fit indices:
#' \itemize{
#'   \item \code{chisq}: Chi-square statistic
#'   \item \code{df}: Degrees of freedom
#'   \item \code{pvalue}: Chi-square p-value
#'   \item \code{cfi}: Comparative Fit Index
#'   \item \code{tli}: Tucker-Lewis Index
#'   \item \code{rmsea}: Root Mean Square Error of Approximation
#'   \item \code{rmsea_ci_lower}: RMSEA 90% CI lower bound
#'   \item \code{rmsea_ci_upper}: RMSEA 90% CI upper bound
#'   \item \code{srmr}: Standardized Root Mean Square Residual
#'   \item \code{aic}: Akaike Information Criterion
#'   \item \code{bic}: Bayesian Information Criterion
#' }
#'
#' @details
#' **Fit index cutoffs (common guidelines):**
#' - **CFI/TLI:** ≥ 0.95 excellent, ≥ 0.90 acceptable
#' - **RMSEA:** ≤ 0.06 excellent, ≤ 0.08 acceptable, ≤ 0.10 marginal
#' - **SRMR:** ≤ 0.08 good fit
#'
#' References:
#' - Hu & Bentler (1999): Cutoff criteria for fit indexes
#' - Kline (2015): Principles and Practice of SEM
#'
#' @examples
#' \dontrun{
#' # Run CFA
#' model <- "satisfaction =~ q1 + q2 + q3"
#' fit <- run_cfa(model, data = df)
#'
#' # Extract fit indices
#' fit_indices <- tidy_cfa_fit(fit)
#' print(fit_indices)
#' }
#'
#' @export
tidy_cfa_fit <- function(fit) {
  # Check for lavaan package
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'lavaan' is required",
      "i" = "Install with: install.packages('lavaan')"
    ))
  }

  # Validate input
  if (!inherits(fit, "lavaan")) {
    rlang::abort(c(
      "fit must be a lavaan object",
      "x" = "You provided {class(fit)[1]}",
      "i" = "Use run_cfa() or lavaan::cfa() to create a fit object"
    ))
  }

  # Extract fit measures
  fit_measures <- lavaan::fitMeasures(fit, c(
    "chisq", "df", "pvalue",
    "cfi", "tli",
    "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
    "srmr",
    "aic", "bic"
  ))

  # Create tibble
  result <- tibble::tibble(
    chisq = fit_measures[["chisq"]],
    df = fit_measures[["df"]],
    pvalue = fit_measures[["pvalue"]],
    cfi = fit_measures[["cfi"]],
    tli = fit_measures[["tli"]],
    rmsea = fit_measures[["rmsea"]],
    rmsea_ci_lower = fit_measures[["rmsea.ci.lower"]],
    rmsea_ci_upper = fit_measures[["rmsea.ci.upper"]],
    srmr = fit_measures[["srmr"]],
    aic = fit_measures[["aic"]],
    bic = fit_measures[["bic"]]
  )

  result
}


#' Compare Nested CFA Models
#'
#' @description
#' Compares two nested CFA models using chi-square difference test.
#' Also reports AIC and BIC for model comparison.
#'
#' @param fit_small lavaan fit object for the more constrained (smaller) model
#' @param fit_large lavaan fit object for the less constrained (larger) model
#' @param test Character. Test to use: "default" (chi-square difference),
#'   "satorra.bentler", "yuan.bentler.mplus"
#'
#' @return A tibble comparing the two models with:
#' \itemize{
#'   \item Fit indices for each model
#'   \item Chi-square difference test results
#'   \item AIC/BIC comparison
#' }
#'
#' @details
#' **Nested models:**
#' - Model A is nested in Model B if A is a restricted version of B
#' - Example: 1-factor model is nested in 2-factor model
#'
#' **Chi-square difference test:**
#' - Tests if the larger model fits significantly better
#' - Δχ² = χ²_small - χ²_large
#' - Δdf = df_small - df_large
#' - If p < .05: Larger model fits significantly better
#'
#' **AIC/BIC:**
#' - Lower is better
#' - Difference > 10 indicates strong preference
#'
#' @examples
#' \dontrun{
#' # Fit nested models
#' model_1factor <- "general =~ q1 + q2 + q3 + q4"
#' model_2factor <- "
#'   factor1 =~ q1 + q2
#'   factor2 =~ q3 + q4
#' "
#'
#' fit1 <- run_cfa(model_1factor, data = df)
#' fit2 <- run_cfa(model_2factor, data = df)
#'
#' # Compare
#' comparison <- compare_cfa_models(fit1, fit2)
#' print(comparison)
#' }
#'
#' @export
compare_cfa_models <- function(fit_small, fit_large, test = "default") {
  # Check for lavaan package
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'lavaan' is required",
      "i" = "Install with: install.packages('lavaan')"
    ))
  }

  # Validate inputs
  if (!inherits(fit_small, "lavaan") || !inherits(fit_large, "lavaan")) {
    rlang::abort("Both fit_small and fit_large must be lavaan objects")
  }

  # Get fit indices for both models
  fit_small_indices <- tidy_cfa_fit(fit_small)
  fit_large_indices <- tidy_cfa_fit(fit_large)

  # Run chi-square difference test
  message("\nComparing nested models...")
  message("Note: Assumes models are nested (smaller is constrained version of larger)")

  # Use lavaan::anova for comparison
  anova_result <- lavaan::anova(fit_small, fit_large, test = test)

  # Check if models are actually nested by df
  if (fit_small_indices$df <= fit_large_indices$df) {
    warning(
      "Warning: fit_small has df = ", fit_small_indices$df,
      " and fit_large has df = ", fit_large_indices$df, ". ",
      "Are you sure the models are nested? ",
      "Typically, the smaller (more constrained) model should have MORE df."
    )
  }

  # Extract comparison statistics
  chisq_diff <- anova_result$`Chisq diff`[2]
  df_diff <- anova_result$`Df diff`[2]
  p_diff <- anova_result$`Pr(>Chisq)`[2]

  # AIC/BIC differences
  aic_diff <- fit_small_indices$aic - fit_large_indices$aic
  bic_diff <- fit_small_indices$bic - fit_large_indices$bic

  # Create comparison table
  comparison <- tibble::tibble(
    model = c("Small (constrained)", "Large (free)"),
    chisq = c(fit_small_indices$chisq, fit_large_indices$chisq),
    df = c(fit_small_indices$df, fit_large_indices$df),
    cfi = c(fit_small_indices$cfi, fit_large_indices$cfi),
    tli = c(fit_small_indices$tli, fit_large_indices$tli),
    rmsea = c(fit_small_indices$rmsea, fit_large_indices$rmsea),
    srmr = c(fit_small_indices$srmr, fit_large_indices$srmr),
    aic = c(fit_small_indices$aic, fit_large_indices$aic),
    bic = c(fit_small_indices$bic, fit_large_indices$bic)
  )

  # Add difference row
  comparison <- dplyr::bind_rows(
    comparison,
    tibble::tibble(
      model = "Difference",
      chisq = chisq_diff,
      df = df_diff,
      cfi = NA_real_,
      tli = NA_real_,
      rmsea = NA_real_,
      srmr = NA_real_,
      aic = aic_diff,
      bic = bic_diff
    )
  )

  # Add test result
  attr(comparison, "chi_diff_p") <- p_diff
  attr(comparison, "test") <- test

  # Print interpretation
  message("\nChi-square difference test:")
  message("  Δχ²(", df_diff, ") = ", round(chisq_diff, 2), ", p ",
          if (p_diff < 0.001) "< .001" else paste("=", round(p_diff, 3)))

  if (p_diff < 0.05) {
    message("  Result: Larger model fits significantly better (p < .05)")
  } else {
    message("  Result: No significant difference. Prefer smaller (more parsimonious) model.")
  }

  message("\nAIC difference: ", round(aic_diff, 2), " (negative = large model better)")
  message("BIC difference: ", round(bic_diff, 2), " (negative = large model better)")

  comparison
}


#' Batch Calculate Cronbach's Alpha for Multiple Scales
#'
#' @description
#' Calculates Cronbach's alpha for multiple scales in a single call.
#' Returns a tidy table with one row per scale.
#'
#' @param data Data frame containing all items
#' @param scales_spec Named list where names are scale names and values are
#'   character vectors of item names. Example:
#'   \code{list(satisfaction = c("sat1", "sat2", "sat3"), loyalty = c("loy1", "loy2"))}
#' @param method Character. Method to use:
#'   - "internal" (default): Use built-in calculate_alpha()
#'   - "performance": Use performance::cronbachs_alpha() if available
#'   - "psych": Use psych::alpha() if available
#'
#' @return A tibble with one row per scale containing:
#' \itemize{
#'   \item \code{scale_name}: Name of the scale
#'   \item \code{alpha}: Cronbach's alpha coefficient
#'   \item \code{n_items}: Number of items in scale
#'   \item \code{n_obs}: Number of complete observations
#'   \item \code{interpretation}: Quality rating (Excellent/Good/Acceptable/etc.)
#' }
#'
#' @examples
#' \dontrun{
#' scales <- list(
#'   satisfaction = c("sat1", "sat2", "sat3", "sat4"),
#'   loyalty = c("loy1", "loy2", "loy3"),
#'   value = c("val1", "val2", "val3", "val4", "val5")
#' )
#'
#' alpha_results <- alpha_table(customer_data, scales)
#' print(alpha_results)
#' }
#'
#' @export
alpha_table <- function(data, scales_spec, method = c("internal", "performance", "psych")) {
  method <- match.arg(method)

  # Validate inputs
  if (!is.data.frame(data)) {
    rlang::abort("data must be a data frame")
  }

  if (!is.list(scales_spec) || is.null(names(scales_spec))) {
    rlang::abort(c(
      "scales_spec must be a named list",
      "i" = "Example: list(scale1 = c('item1', 'item2'), scale2 = c('item3', 'item4'))"
    ))
  }

  # Choose method
  if (method == "performance" && !requireNamespace("performance", quietly = TRUE)) {
    warning("Package 'performance' not available. Falling back to 'internal' method.")
    method <- "internal"
  }

  if (method == "psych" && !requireNamespace("psych", quietly = TRUE)) {
    warning("Package 'psych' not available. Falling back to 'internal' method.")
    method <- "internal"
  }

  # Calculate alpha for each scale
  results_list <- lapply(names(scales_spec), function(scale_name) {
    items <- scales_spec[[scale_name]]

    # Validate items exist
    tryCatch({
      assert_vars_present(data, items, paste0("scale '", scale_name, "'"))

      if (method == "internal") {
        # Use existing calculate_alpha function
        alpha_result <- calculate_alpha(
          data = data,
          items = items,
          scale_name = scale_name
        )

        # Extract key values
        alpha_val <- alpha_result$alpha
        n_items <- alpha_result$n_items
        n_obs <- alpha_result$n_observations
        interp <- if (alpha_val >= 0.90) {
          "Excellent"
        } else if (alpha_val >= 0.80) {
          "Good"
        } else if (alpha_val >= 0.70) {
          "Acceptable"
        } else if (alpha_val >= 0.60) {
          "Questionable"
        } else {
          "Poor"
        }

      } else if (method == "performance") {
        item_data <- stats::na.omit(data[, items, drop = FALSE])
        alpha_result <- performance::cronbachs_alpha(item_data)
        alpha_val <- alpha_result$alpha
        n_items <- length(items)
        n_obs <- nrow(item_data)
        interp <- if (alpha_val >= 0.90) "Excellent" else if (alpha_val >= 0.80) "Good" else if (alpha_val >= 0.70) "Acceptable" else if (alpha_val >= 0.60) "Questionable" else "Poor"

      } else if (method == "psych") {
        item_data <- stats::na.omit(data[, items, drop = FALSE])
        alpha_result <- psych::alpha(item_data, check.keys = FALSE)
        alpha_val <- alpha_result$total$raw_alpha
        n_items <- length(items)
        n_obs <- nrow(item_data)
        interp <- if (alpha_val >= 0.90) "Excellent" else if (alpha_val >= 0.80) "Good" else if (alpha_val >= 0.70) "Acceptable" else if (alpha_val >= 0.60) "Questionable" else "Poor"
      }

      tibble::tibble(
        scale_name = scale_name,
        alpha = round(alpha_val, 3),
        n_items = n_items,
        n_obs = n_obs,
        interpretation = interp
      )

    }, error = function(e) {
      # Return NA row if error
      tibble::tibble(
        scale_name = scale_name,
        alpha = NA_real_,
        n_items = length(items),
        n_obs = NA_integer_,
        interpretation = paste0("Error: ", e$message)
      )
    })
  })

  # Combine into single tibble
  dplyr::bind_rows(results_list)
}


#' @export
print.efa_diagnostics <- function(x, ...) {
  cat(x$interpretation)
  invisible(x)
}
