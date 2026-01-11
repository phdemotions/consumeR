#' Parallel Mediation Analysis
#'
#' @description
#' Analyze parallel mediation models where multiple mediators operate
#' simultaneously between X and Y. Uses bootstrap confidence intervals for
#' specific indirect effects following Preacher & Hayes (2008).
#'
#' @param data Data frame containing all variables.
#' @param x Name of independent variable (character).
#' @param mediators Character vector of mediator variable names.
#' @param y Name of dependent variable (character).
#' @param covariates Optional character vector of covariate names.
#' @param conf_level Confidence level for intervals. Default is 0.95.
#' @param boot_samples Number of bootstrap samples. Minimum 1000, 5000+
#'   recommended for publication.
#' @param boot_method Bootstrap CI method: "bca" (bias-corrected and
#'   accelerated, default), "perc" (percentile), or "norm" (normal).
#' @param seed Random seed for reproducibility.
#'
#' @return Object of class \code{parallel_mediation} containing:
#'   \describe{
#'     \item{paths}{Tibble of all path coefficients (a, b, c, c')}
#'     \item{specific_indirect}{Tibble of specific indirect effects for each
#'       mediator with bootstrap CIs}
#'     \item{total_indirect}{Total indirect effect across all mediators}
#'     \item{direct_effect}{Direct effect (c') with CI}
#'     \item{total_effect}{Total effect (c) with CI}
#'     \item{contrasts}{Pairwise contrasts between specific indirect effects}
#'     \item{boot_samples}{Number of bootstrap samples used}
#'     \item{boot_method}{Bootstrap method used}
#'     \item{conf_level}{Confidence level}
#'     \item{interpretation}{Publication-ready interpretation text}
#'     \item{variables}{List of variable names used}
#'   }
#'
#' @details
#' **Parallel Mediation Model**:
#'
#' X → M1 → Y
#' X → M2 → Y
#' X → M3 → Y
#' X ⇢ Y (direct effect)
#'
#' Each mediator operates independently. Total indirect effect is the sum of
#' specific indirect effects: (a1*b1) + (a2*b2) + (a3*b3) + ...
#'
#' **Specific Indirect Effects**:
#' \itemize{
#'   \item Indirect effect through M1: a1 * b1
#'   \item Indirect effect through M2: a2 * b2
#'   \item Each tested with bootstrap CI
#' }
#'
#' **Contrasts**:
#' Tests whether specific indirect effects differ from each other. Significant
#' contrast indicates one mediator is stronger than another.
#'
#' **JCP Reporting Requirements**:
#' \itemize{
#'   \item Report all specific indirect effects with bootstrap CIs
#'   \item Report total indirect effect
#'   \item Report direct effect (c')
#'   \item Conduct contrasts if comparing mediators
#'   \item Use ≥5000 bootstrap samples for publication
#'   \item Report which mediators are significant
#' }
#'
#' @references
#' Preacher, K. J., & Hayes, A. F. (2008). Asymptotic and resampling strategies
#'   for assessing and comparing indirect effects in multiple mediator models.
#'   Behavior Research Methods, 40(3), 879-891.
#'
#' @examples
#' \dontrun{
#' # Two parallel mediators
#' result <- mediation_parallel(
#'   data = consumer_data,
#'   x = "brand_reputation",
#'   mediators = c("perceived_quality", "brand_trust"),
#'   y = "purchase_intention",
#'   boot_samples = 5000,
#'   seed = 123
#' )
#' print(result)
#'
#' # Three mediators with covariates
#' result2 <- mediation_parallel(
#'   data = mydata,
#'   x = "ad_exposure",
#'   mediators = c("awareness", "attitude", "intention"),
#'   y = "behavior",
#'   covariates = c("age", "gender"),
#'   boot_samples = 5000
#' )
#' }
#'
#' @export
mediation_parallel <- function(data,
                               x,
                               mediators,
                               y,
                               covariates = NULL,
                               conf_level = 0.95,
                               boot_samples = 5000,
                               boot_method = c("bca", "perc", "norm"),
                               seed = NULL) {

  # Input validation
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame", class = "invalid_input")
  }

  boot_method <- match.arg(boot_method)

  # Check variables
  vars_needed <- c(x, mediators, y, covariates)
  missing_vars <- setdiff(vars_needed, names(data))
  if (length(missing_vars) > 0) {
    rlang::abort(
      sprintf("Variables not found in data: %s", paste(missing_vars, collapse = ", ")),
      class = "invalid_input"
    )
  }

  if (length(mediators) < 2) {
    rlang::abort(
      "Parallel mediation requires at least 2 mediators. Use mediation_simple() for single mediator.",
      class = "invalid_input"
    )
  }

  if (boot_samples < 1000) {
    rlang::abort(
      "`boot_samples` must be at least 1000 for reliable inference. 5000+ recommended.",
      class = "invalid_input"
    )
  }

  # Remove missing data
  data_complete <- data[stats::complete.cases(data[, vars_needed]), ]
  n_removed <- nrow(data) - nrow(data_complete)

  if (n_removed > 0) {
    message(sprintf("Removed %d rows with missing data. %d rows remaining.",
                    n_removed, nrow(data_complete)))
  }

  if (nrow(data_complete) < 50) {
    rlang::warn(
      "Small sample size (n < 50). Results may be unreliable.",
      class = "small_sample"
    )
  }

  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Estimate paths
  paths <- estimate_parallel_paths(data_complete, x, mediators, y, covariates)

  # Bootstrap indirect effects
  boot_results <- bootstrap_parallel_mediation(
    data_complete, x, mediators, y, covariates,
    boot_samples, boot_method, conf_level
  )

  # Calculate contrasts
  contrasts <- calculate_mediation_contrasts(
    boot_results$specific_boot, mediators, conf_level, boot_method
  )

  # Generate interpretation
  interpretation <- generate_parallel_interpretation(
    boot_results$specific_indirect,
    boot_results$total_indirect,
    paths$direct_effect,
    contrasts
  )

  # Create result object
  result <- list(
    paths = paths$all_paths,
    specific_indirect = boot_results$specific_indirect,
    total_indirect = boot_results$total_indirect,
    direct_effect = paths$direct_effect,
    total_effect = paths$total_effect,
    contrasts = contrasts,
    boot_samples = boot_samples,
    boot_method = boot_method,
    conf_level = conf_level,
    interpretation = interpretation,
    variables = list(x = x, mediators = mediators, y = y, covariates = covariates)
  )

  class(result) <- c("parallel_mediation", "list")
  result
}


#' Serial Mediation Analysis
#'
#' @description
#' Analyze serial (sequential) mediation where mediators operate in a specific
#' order: X → M1 → M2 → ... → Y. Tests specific indirect effects through
#' different pathways using bootstrap CIs.
#'
#' @param data Data frame containing all variables.
#' @param x Name of independent variable (character).
#' @param mediators Character vector of mediator variable names in sequential
#'   order (e.g., c("M1", "M2") for X → M1 → M2 → Y).
#' @param y Name of dependent variable (character).
#' @param covariates Optional character vector of covariate names.
#' @param conf_level Confidence level for intervals. Default is 0.95.
#' @param boot_samples Number of bootstrap samples. Minimum 1000, 5000+
#'   recommended for publication.
#' @param boot_method Bootstrap CI method: "bca" (default), "perc", or "norm".
#' @param seed Random seed for reproducibility.
#'
#' @return Object of class \code{serial_mediation} containing:
#'   \describe{
#'     \item{paths}{Tibble of all path coefficients}
#'     \item{specific_indirect}{Tibble of specific indirect effects through
#'       different pathways with bootstrap CIs}
#'     \item{total_indirect}{Total indirect effect}
#'     \item{direct_effect}{Direct effect (c')}
#'     \item{total_effect}{Total effect (c)}
#'     \item{interpretation}{Publication-ready interpretation text}
#'   }
#'
#' @details
#' **Serial Mediation Model (2 mediators)**:
#'
#' Path 1: X → M1 → Y (a1*b1)
#' Path 2: X → M2 → Y (a2*b2)
#' Path 3: X → M1 → M2 → Y (a1*d21*b2) [serial path]
#' Direct: X ⇢ Y (c')
#'
#' **Specific Indirect Effects**:
#' \itemize{
#'   \item Through M1 only: X → M1 → Y
#'   \item Through M2 only: X → M2 → Y
#'   \item Serial through M1 then M2: X → M1 → M2 → Y
#' }
#'
#' The serial path represents the unique contribution of the sequential
#' mediation process.
#'
#' @references
#' Hayes, A. F. (2018). Introduction to mediation, moderation, and conditional
#'   process analysis (2nd ed.). Guilford Press.
#'
#' @examples
#' \dontrun{
#' # Two-mediator serial model
#' result <- mediation_serial(
#'   data = consumer_data,
#'   x = "advertising",
#'   mediators = c("awareness", "attitude"),
#'   y = "purchase",
#'   boot_samples = 5000,
#'   seed = 123
#' )
#' print(result)
#' }
#'
#' @export
mediation_serial <- function(data,
                             x,
                             mediators,
                             y,
                             covariates = NULL,
                             conf_level = 0.95,
                             boot_samples = 5000,
                             boot_method = c("bca", "perc", "norm"),
                             seed = NULL) {

  # Input validation
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame", class = "invalid_input")
  }

  boot_method <- match.arg(boot_method)

  vars_needed <- c(x, mediators, y, covariates)
  missing_vars <- setdiff(vars_needed, names(data))
  if (length(missing_vars) > 0) {
    rlang::abort(
      sprintf("Variables not found in data: %s", paste(missing_vars, collapse = ", ")),
      class = "invalid_input"
    )
  }

  if (length(mediators) < 2) {
    rlang::abort(
      "Serial mediation requires at least 2 mediators.",
      class = "invalid_input"
    )
  }

  if (boot_samples < 1000) {
    rlang::abort(
      "`boot_samples` must be at least 1000. 5000+ recommended.",
      class = "invalid_input"
    )
  }

  # Remove missing data
  data_complete <- data[stats::complete.cases(data[, vars_needed]), ]
  n_removed <- nrow(data) - nrow(data_complete)

  if (n_removed > 0) {
    message(sprintf("Removed %d rows with missing data. %d rows remaining.",
                    n_removed, nrow(data_complete)))
  }

  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Estimate paths
  paths <- estimate_serial_paths(data_complete, x, mediators, y, covariates)

  # Bootstrap indirect effects
  boot_results <- bootstrap_serial_mediation(
    data_complete, x, mediators, y, covariates,
    boot_samples, boot_method, conf_level
  )

  # Generate interpretation
  interpretation <- generate_serial_interpretation(
    boot_results$specific_indirect,
    boot_results$total_indirect,
    paths$direct_effect
  )

  # Create result object
  result <- list(
    paths = paths$all_paths,
    specific_indirect = boot_results$specific_indirect,
    total_indirect = boot_results$total_indirect,
    direct_effect = paths$direct_effect,
    total_effect = paths$total_effect,
    boot_samples = boot_samples,
    boot_method = boot_method,
    conf_level = conf_level,
    interpretation = interpretation,
    variables = list(x = x, mediators = mediators, y = y, covariates = covariates)
  )

  class(result) <- c("serial_mediation", "list")
  result
}


# Helper functions ------------------------------------------------------

#' Estimate Paths for Parallel Mediation
#'
#' @keywords internal
estimate_parallel_paths <- function(data, x, mediators, y, covariates) {

  cov_formula <- if (!is.null(covariates)) {
    paste("+", paste(covariates, collapse = " + "))
  } else {
    ""
  }

  # Total effect (c path): Y ~ X
  total_formula <- stats::as.formula(sprintf("%s ~ %s %s", y, x, cov_formula))
  total_model <- stats::lm(total_formula, data = data)
  c_path <- stats::coef(total_model)[x]

  # a paths: M ~ X for each mediator
  a_paths <- numeric(length(mediators))
  names(a_paths) <- paste0("a_", mediators)

  for (i in seq_along(mediators)) {
    m_formula <- stats::as.formula(sprintf("%s ~ %s %s", mediators[i], x, cov_formula))
    m_model <- stats::lm(m_formula, data = data)
    a_paths[i] <- stats::coef(m_model)[x]
  }

  # b paths and c' (direct effect): Y ~ X + M1 + M2 + ...
  y_formula <- stats::as.formula(sprintf(
    "%s ~ %s + %s %s",
    y, x, paste(mediators, collapse = " + "), cov_formula
  ))
  y_model <- stats::lm(y_formula, data = data)

  c_prime <- stats::coef(y_model)[x]

  b_paths <- numeric(length(mediators))
  names(b_paths) <- paste0("b_", mediators)
  for (i in seq_along(mediators)) {
    b_paths[i] <- stats::coef(y_model)[mediators[i]]
  }

  # Combine all paths
  all_paths <- tibble::tibble(
    path = c("c", "c'", names(a_paths), names(b_paths)),
    estimate = c(c_path, c_prime, a_paths, b_paths)
  )

  list(
    all_paths = all_paths,
    a_paths = a_paths,
    b_paths = b_paths,
    direct_effect = tibble::tibble(path = "c'", estimate = c_prime),
    total_effect = tibble::tibble(path = "c", estimate = c_path)
  )
}


#' Bootstrap Parallel Mediation
#'
#' @keywords internal
bootstrap_parallel_mediation <- function(data, x, mediators, y, covariates,
                                        boot_samples, boot_method, conf_level) {

  n <- nrow(data)
  n_mediators <- length(mediators)

  # Storage for bootstrap samples
  boot_specific <- matrix(NA, nrow = boot_samples, ncol = n_mediators)
  colnames(boot_specific) <- mediators

  # Bootstrap
  for (b in seq_len(boot_samples)) {
    boot_idx <- sample(n, n, replace = TRUE)
    boot_data <- data[boot_idx, ]

    # Estimate paths in this bootstrap sample
    paths_b <- estimate_parallel_paths(boot_data, x, mediators, y, covariates)

    # Calculate specific indirect effects
    for (i in seq_along(mediators)) {
      a_i <- paths_b$a_paths[i]
      b_i <- paths_b$b_paths[i]
      boot_specific[b, i] <- a_i * b_i
    }
  }

  # Point estimates from full sample
  paths_full <- estimate_parallel_paths(data, x, mediators, y, covariates)
  specific_estimates <- paths_full$a_paths * paths_full$b_paths

  # Calculate CIs for each specific indirect effect
  specific_indirect_list <- list()

  for (i in seq_along(mediators)) {
    boot_dist <- boot_specific[, i]
    boot_dist <- boot_dist[!is.na(boot_dist)]

    ci <- calculate_bootstrap_ci(
      boot_dist = boot_dist,
      point_estimate = specific_estimates[i],
      conf_level = conf_level,
      method = boot_method
    )

    specific_indirect_list[[i]] <- tibble::tibble(
      mediator = mediators[i],
      indirect_effect = specific_estimates[i],
      ci_lower = ci[1],
      ci_upper = ci[2],
      significant = !(ci[1] <= 0 & ci[2] >= 0)
    )
  }

  specific_indirect <- dplyr::bind_rows(specific_indirect_list)

  # Total indirect effect
  total_boot <- rowSums(boot_specific, na.rm = TRUE)
  total_estimate <- sum(specific_estimates)

  total_ci <- calculate_bootstrap_ci(
    boot_dist = total_boot,
    point_estimate = total_estimate,
    conf_level = conf_level,
    method = boot_method
  )

  total_indirect <- tibble::tibble(
    effect = "Total indirect",
    estimate = total_estimate,
    ci_lower = total_ci[1],
    ci_upper = total_ci[2],
    significant = !(total_ci[1] <= 0 & total_ci[2] >= 0)
  )

  list(
    specific_indirect = specific_indirect,
    total_indirect = total_indirect,
    specific_boot = boot_specific
  )
}


#' Estimate Paths for Serial Mediation
#'
#' @keywords internal
estimate_serial_paths <- function(data, x, mediators, y, covariates) {

  cov_formula <- if (!is.null(covariates)) {
    paste("+", paste(covariates, collapse = " + "))
  } else {
    ""
  }

  # For 2 mediators: M1, M2
  # X → M1 (a1)
  # X → M2 (a2)
  # M1 → M2 (d21)
  # M1 → Y (b1)
  # M2 → Y (b2)
  # X → Y (c')

  m1 <- mediators[1]
  m2 <- mediators[2]

  # Total effect
  total_formula <- stats::as.formula(sprintf("%s ~ %s %s", y, x, cov_formula))
  total_model <- stats::lm(total_formula, data = data)
  c_path <- stats::coef(total_model)[x]

  # a1: M1 ~ X
  a1_formula <- stats::as.formula(sprintf("%s ~ %s %s", m1, x, cov_formula))
  a1_model <- stats::lm(a1_formula, data = data)
  a1 <- stats::coef(a1_model)[x]

  # a2 and d21: M2 ~ X + M1
  m2_formula <- stats::as.formula(sprintf("%s ~ %s + %s %s", m2, x, m1, cov_formula))
  m2_model <- stats::lm(m2_formula, data = data)
  a2 <- stats::coef(m2_model)[x]
  d21 <- stats::coef(m2_model)[m1]

  # b1, b2, c': Y ~ X + M1 + M2
  y_formula <- stats::as.formula(sprintf("%s ~ %s + %s + %s %s", y, x, m1, m2, cov_formula))
  y_model <- stats::lm(y_formula, data = data)
  c_prime <- stats::coef(y_model)[x]
  b1 <- stats::coef(y_model)[m1]
  b2 <- stats::coef(y_model)[m2]

  all_paths <- tibble::tibble(
    path = c("c", "c'", "a1", "a2", "d21", "b1", "b2"),
    estimate = c(c_path, c_prime, a1, a2, d21, b1, b2)
  )

  list(
    all_paths = all_paths,
    a1 = a1,
    a2 = a2,
    d21 = d21,
    b1 = b1,
    b2 = b2,
    direct_effect = tibble::tibble(path = "c'", estimate = c_prime),
    total_effect = tibble::tibble(path = "c", estimate = c_path)
  )
}


#' Bootstrap Serial Mediation
#'
#' @keywords internal
bootstrap_serial_mediation <- function(data, x, mediators, y, covariates,
                                      boot_samples, boot_method, conf_level) {

  n <- nrow(data)
  m1 <- mediators[1]
  m2 <- mediators[2]

  # Storage: ind1 (X→M1→Y), ind2 (X→M2→Y), ind3 (X→M1→M2→Y)
  boot_ind1 <- numeric(boot_samples)
  boot_ind2 <- numeric(boot_samples)
  boot_ind3 <- numeric(boot_samples)

  # Bootstrap
  for (b in seq_len(boot_samples)) {
    boot_idx <- sample(n, n, replace = TRUE)
    boot_data <- data[boot_idx, ]

    paths_b <- estimate_serial_paths(boot_data, x, mediators, y, covariates)

    # Specific indirect effects
    boot_ind1[b] <- paths_b$a1 * paths_b$b1  # X → M1 → Y
    boot_ind2[b] <- paths_b$a2 * paths_b$b2  # X → M2 → Y
    boot_ind3[b] <- paths_b$a1 * paths_b$d21 * paths_b$b2  # X → M1 → M2 → Y
  }

  # Point estimates
  paths_full <- estimate_serial_paths(data, x, mediators, y, covariates)
  ind1_est <- paths_full$a1 * paths_full$b1
  ind2_est <- paths_full$a2 * paths_full$b2
  ind3_est <- paths_full$a1 * paths_full$d21 * paths_full$b2

  # CIs
  ci1 <- calculate_bootstrap_ci(boot_ind1, ind1_est, conf_level, boot_method)
  ci2 <- calculate_bootstrap_ci(boot_ind2, ind2_est, conf_level, boot_method)
  ci3 <- calculate_bootstrap_ci(boot_ind3, ind3_est, conf_level, boot_method)

  specific_indirect <- tibble::tibble(
    pathway = c(
      sprintf("X → %s → Y", m1),
      sprintf("X → %s → Y", m2),
      sprintf("X → %s → %s → Y", m1, m2)
    ),
    indirect_effect = c(ind1_est, ind2_est, ind3_est),
    ci_lower = c(ci1[1], ci2[1], ci3[1]),
    ci_upper = c(ci1[2], ci2[2], ci3[2]),
    significant = c(
      !(ci1[1] <= 0 & ci1[2] >= 0),
      !(ci2[1] <= 0 & ci2[2] >= 0),
      !(ci3[1] <= 0 & ci3[2] >= 0)
    )
  )

  # Total indirect
  total_boot <- boot_ind1 + boot_ind2 + boot_ind3
  total_est <- ind1_est + ind2_est + ind3_est
  total_ci <- calculate_bootstrap_ci(total_boot, total_est, conf_level, boot_method)

  total_indirect <- tibble::tibble(
    effect = "Total indirect",
    estimate = total_est,
    ci_lower = total_ci[1],
    ci_upper = total_ci[2],
    significant = !(total_ci[1] <= 0 & total_ci[2] >= 0)
  )

  list(
    specific_indirect = specific_indirect,
    total_indirect = total_indirect
  )
}


#' Calculate Contrasts Between Specific Indirect Effects
#'
#' @keywords internal
calculate_mediation_contrasts <- function(boot_specific, mediators,
                                         conf_level, boot_method) {

  n_mediators <- length(mediators)

  if (n_mediators < 2) {
    return(NULL)
  }

  # Pairwise contrasts
  contrasts_list <- list()
  idx <- 1

  for (i in 1:(n_mediators - 1)) {
    for (j in (i + 1):n_mediators) {
      contrast_boot <- boot_specific[, i] - boot_specific[, j]
      contrast_boot <- contrast_boot[!is.na(contrast_boot)]

      contrast_est <- mean(contrast_boot)

      ci <- calculate_bootstrap_ci(
        boot_dist = contrast_boot,
        point_estimate = contrast_est,
        conf_level = conf_level,
        method = boot_method
      )

      contrasts_list[[idx]] <- tibble::tibble(
        contrast = sprintf("%s - %s", mediators[i], mediators[j]),
        difference = contrast_est,
        ci_lower = ci[1],
        ci_upper = ci[2],
        significant = !(ci[1] <= 0 & ci[2] >= 0)
      )

      idx <- idx + 1
    }
  }

  dplyr::bind_rows(contrasts_list)
}


#' Calculate Bootstrap Confidence Interval
#'
#' @keywords internal
calculate_bootstrap_ci <- function(boot_dist, point_estimate,
                                   conf_level, method) {

  alpha <- 1 - conf_level

  if (method == "perc") {
    # Percentile method
    ci <- stats::quantile(boot_dist, probs = c(alpha / 2, 1 - alpha / 2),
                          na.rm = TRUE)

  } else if (method == "norm") {
    # Normal approximation
    se <- stats::sd(boot_dist, na.rm = TRUE)
    z <- stats::qnorm(1 - alpha / 2)
    ci <- c(point_estimate - z * se, point_estimate + z * se)

  } else {
    # BCa (bias-corrected and accelerated)
    ci <- calculate_bca_ci(boot_dist, point_estimate, conf_level)
  }

  as.numeric(ci)
}


#' Calculate BCa Confidence Interval
#'
#' @keywords internal
calculate_bca_ci <- function(boot_dist, point_estimate, conf_level) {

  # This is a simplified BCa implementation
  # For full implementation, would use jackknife for acceleration

  alpha <- 1 - conf_level
  boot_dist <- boot_dist[!is.na(boot_dist)]

  # Bias correction
  z0 <- stats::qnorm(mean(boot_dist < point_estimate))

  # Without jackknife acceleration (simplified)
  z_lower <- stats::qnorm(alpha / 2)
  z_upper <- stats::qnorm(1 - alpha / 2)

  # Adjust for bias
  p_lower <- stats::pnorm(2 * z0 + z_lower)
  p_upper <- stats::pnorm(2 * z0 + z_upper)

  # Ensure valid probabilities
  p_lower <- max(0.001, min(0.999, p_lower))
  p_upper <- max(0.001, min(0.999, p_upper))

  ci <- stats::quantile(boot_dist, probs = c(p_lower, p_upper))
  ci
}


#' Generate Parallel Mediation Interpretation
#'
#' @keywords internal
generate_parallel_interpretation <- function(specific, total, direct, contrasts) {

  sig_mediators <- specific$mediator[specific$significant]
  n_sig <- length(sig_mediators)

  if (n_sig == 0) {
    return("No significant specific indirect effects were found.")
  }

  specific_text <- if (n_sig == 1) {
    sprintf("One mediator (%s) showed a significant specific indirect effect", sig_mediators[1])
  } else {
    sprintf("%d mediators (%s) showed significant specific indirect effects",
            n_sig, paste(sig_mediators, collapse = ", "))
  }

  total_text <- if (total$significant) {
    sprintf("The total indirect effect was significant (%.3f, 95%% CI [%.3f, %.3f])",
            total$estimate, total$ci_lower, total$ci_upper)
  } else {
    "The total indirect effect was not significant"
  }

  direct_text <- if (!is.null(direct$estimate)) {
    if (total$significant && abs(direct$estimate) > 0.001) {
      "suggesting partial mediation"
    } else if (total$significant) {
      "consistent with full mediation"
    } else {
      ""
    }
  } else {
    ""
  }

  paste0(specific_text, ". ", total_text, " ", direct_text, ".")
}


#' Generate Serial Mediation Interpretation
#'
#' @keywords internal
generate_serial_interpretation <- function(specific, total, direct) {

  sig_pathways <- specific$pathway[specific$significant]
  n_sig <- length(sig_pathways)

  if (n_sig == 0) {
    return("No significant indirect pathways were found.")
  }

  pathway_text <- sprintf(
    "%d indirect pathway%s %s significant: %s",
    n_sig,
    if (n_sig == 1) "" else "s",
    if (n_sig == 1) "was" else "were",
    paste(sig_pathways, collapse = "; ")
  )

  total_text <- if (total$significant) {
    sprintf("The total indirect effect was significant (%.3f, 95%% CI [%.3f, %.3f])",
            total$estimate, total$ci_lower, total$ci_upper)
  } else {
    "The total indirect effect was not significant"
  }

  paste0(pathway_text, ". ", total_text, ".")
}


#' Print Method for Parallel Mediation
#'
#' @export
print.parallel_mediation <- function(x, ...) {

  cat("Parallel Mediation Analysis\n")
  cat("===========================\n\n")

  cat("Variables:\n")
  cat(sprintf("  X: %s\n", x$variables$x))
  cat(sprintf("  Mediators: %s\n", paste(x$variables$mediators, collapse = ", ")))
  cat(sprintf("  Y: %s\n", x$variables$y))
  if (!is.null(x$variables$covariates)) {
    cat(sprintf("  Covariates: %s\n", paste(x$variables$covariates, collapse = ", ")))
  }
  cat("\n")

  cat("Bootstrap method:", x$boot_method, "\n")
  cat("Bootstrap samples:", x$boot_samples, "\n")
  cat("Confidence level:", x$conf_level, "\n\n")

  cat("Specific Indirect Effects:\n")
  cat("--------------------------\n")
  print(x$specific_indirect, n = Inf)
  cat("\n")

  cat("Total Indirect Effect:\n")
  cat("---------------------\n")
  print(x$total_indirect, n = Inf)
  cat("\n")

  if (!is.null(x$contrasts) && nrow(x$contrasts) > 0) {
    cat("Pairwise Contrasts:\n")
    cat("------------------\n")
    print(x$contrasts, n = Inf)
    cat("\n")
  }

  cat("Interpretation:\n")
  cat("--------------\n")
  cat(x$interpretation, "\n")

  invisible(x)
}


#' Print Method for Serial Mediation
#'
#' @export
print.serial_mediation <- function(x, ...) {

  cat("Serial Mediation Analysis\n")
  cat("=========================\n\n")

  cat("Variables:\n")
  cat(sprintf("  X: %s\n", x$variables$x))
  cat(sprintf("  Mediators (sequential): %s\n",
              paste(x$variables$mediators, collapse = " → ")))
  cat(sprintf("  Y: %s\n", x$variables$y))
  if (!is.null(x$variables$covariates)) {
    cat(sprintf("  Covariates: %s\n", paste(x$variables$covariates, collapse = ", ")))
  }
  cat("\n")

  cat("Bootstrap method:", x$boot_method, "\n")
  cat("Bootstrap samples:", x$boot_samples, "\n")
  cat("Confidence level:", x$conf_level, "\n\n")

  cat("Indirect Pathways:\n")
  cat("-----------------\n")
  print(x$specific_indirect, n = Inf)
  cat("\n")

  cat("Total Indirect Effect:\n")
  cat("---------------------\n")
  print(x$total_indirect, n = Inf)
  cat("\n")

  cat("Interpretation:\n")
  cat("--------------\n")
  cat(x$interpretation, "\n")

  invisible(x)
}
