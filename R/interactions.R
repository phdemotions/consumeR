#' Simple Slopes Analysis for Interaction Effects
#'
#' Probes two-way interactions by testing the effect of a focal predictor at
#' different values of a moderator variable. This is essential for interpreting
#' significant interactions in consumer psychology research, as recommended by
#' Aiken & West (1991) and Hayes (2018).
#'
#' @param model A fitted linear model (lm object) with an interaction term
#' @param focal Character; name of the focal predictor (X variable)
#' @param moderator Character; name of the moderator variable (W variable)
#' @param mod_values Numeric vector of moderator values to probe. Default: NULL
#'   (uses mean +/- 1 SD for continuous moderators, or all levels for factors)
#' @param conf_level Confidence level for intervals (default: 0.95)
#' @param center_moderator Logical; center moderator before probing (default: TRUE)
#'
#' @return A list of class "simple_slopes" containing:
#'   - slopes: Tibble with simple slopes at each moderator value
#'   - mod_values: Values of moderator used
#'   - focal: Name of focal predictor
#'   - moderator: Name of moderator
#'   - interpretation: Publication-ready interpretation text
#'
#' @details
#' Simple slopes analysis examines the effect of X on Y at specific values of W.
#' For continuous moderators, the default values are:
#' - Low: Mean - 1 SD
#' - Medium: Mean
#' - High: Mean + 1 SD
#'
#' For categorical moderators, slopes are computed at each level.
#'
#' **Interpretation:**
#' - If slopes differ significantly across moderator values, the interaction is
#'   meaningful
#' - Report the slope and significance test at each moderator value
#' - Consider Johnson-Neyman technique for continuous moderators to find regions
#'   of significance
#'
#' **For JCP Publications:**
#' - Always probe significant interactions
#' - Report simple slopes with standard errors and CIs
#' - Include visualization (interaction plot)
#' - Consider Johnson-Neyman if continuous moderator
#'
#' @references
#' Aiken, L. S., & West, S. G. (1991). Multiple regression: Testing and
#' interpreting interactions. Sage.
#'
#' Hayes, A. F. (2018). Introduction to mediation, moderation, and conditional
#' process analysis (2nd ed.). Guilford Press.
#'
#' @export
#' @examples
#' # Price sensitivity moderated by brand loyalty
#' set.seed(42)
#' n <- 150
#' df <- data.frame(
#'   price = rnorm(n, 50, 10),
#'   loyalty = rnorm(n, 5, 1.5),
#'   purchase = rnorm(n, 5, 1)
#' )
#' # Interaction: price effect depends on loyalty
#' df$purchase <- df$purchase - 0.1 * df$price + 0.3 * df$loyalty +
#'                0.05 * df$price * df$loyalty
#'
#' # Fit model with interaction
#' model <- lm(purchase ~ price * loyalty, data = df)
#'
#' # Probe interaction with simple slopes
#' slopes <- simple_slopes(model, focal = "price", moderator = "loyalty")
#' print(slopes)
simple_slopes <- function(model,
                          focal,
                          moderator,
                          mod_values = NULL,
                          conf_level = 0.95,
                          center_moderator = TRUE) {
  # Validate inputs
  if (!inherits(model, "lm")) {
    rlang::abort("`model` must be an lm object", class = "invalid_input")
  }

  if (!is.character(focal) || length(focal) != 1) {
    rlang::abort("`focal` must be a single character string", class = "invalid_input")
  }

  if (!is.character(moderator) || length(moderator) != 1) {
    rlang::abort("`moderator` must be a single character string", class = "invalid_input")
  }

  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    rlang::abort("`conf_level` must be between 0 and 1", class = "invalid_input")
  }

  # Get model data
  data <- model$model

  # Check that variables exist
  if (!focal %in% names(data)) {
    rlang::abort(sprintf("Focal variable '%s' not found in model", focal), class = "variable_not_found")
  }
  if (!moderator %in% names(data)) {
    rlang::abort(sprintf("Moderator variable '%s' not found in model", moderator), class = "variable_not_found")
  }

  # Check for interaction term
  interaction_term <- paste0(focal, ":", moderator)
  interaction_term_alt <- paste0(moderator, ":", focal)

  coef_names <- names(stats::coef(model))
  has_interaction <- interaction_term %in% coef_names || interaction_term_alt %in% coef_names

  if (!has_interaction) {
    rlang::abort(
      sprintf("Model does not contain interaction between '%s' and '%s'", focal, moderator),
      class = "no_interaction"
    )
  }

  # Get moderator values
  mod_var <- data[[moderator]]

  if (is.null(mod_values)) {
    if (is.factor(mod_var)) {
      # For factors, use all levels
      mod_values <- levels(mod_var)
      mod_values_numeric <- seq_along(mod_values)
    } else {
      # For continuous, use mean +/- 1 SD
      mod_mean <- mean(mod_var, na.rm = TRUE)
      mod_sd <- stats::sd(mod_var, na.rm = TRUE)
      mod_values <- c(mod_mean - mod_sd, mod_mean, mod_mean + mod_sd)
      mod_values_numeric <- mod_values
    }
  } else {
    mod_values_numeric <- mod_values
  }

  # Compute simple slopes at each moderator value
  slopes_list <- list()

  for (i in seq_along(mod_values_numeric)) {
    mod_val <- mod_values_numeric[i]

    # Create data for prediction
    # Set moderator to specific value, vary focal predictor
    newdata_low <- data
    newdata_high <- data

    if (is.factor(mod_var)) {
      newdata_low[[moderator]] <- factor(mod_values[i], levels = levels(mod_var))
      newdata_high[[moderator]] <- factor(mod_values[i], levels = levels(mod_var))
    } else {
      newdata_low[[moderator]] <- mod_val
      newdata_high[[moderator]] <- mod_val
    }

    # Get focal predictor values (mean +/- 1 SD for slope calculation)
    focal_var <- data[[focal]]
    if (is.factor(focal_var)) {
      rlang::abort(
        sprintf("Focal predictor '%s' is categorical. Simple slopes require continuous focal predictor.", focal),
        class = "invalid_focal"
      )
    }

    focal_mean <- mean(focal_var, na.rm = TRUE)
    focal_sd <- stats::sd(focal_var, na.rm = TRUE)

    newdata_low[[focal]] <- focal_mean - focal_sd
    newdata_high[[focal]] <- focal_mean + focal_sd

    # Predict at low and high focal values
    pred_low <- stats::predict(model, newdata = newdata_low, se.fit = TRUE)
    pred_high <- stats::predict(model, newdata = newdata_high, se.fit = TRUE)

    # Compute slope (change in Y per unit change in X)
    # Slope = (Y_high - Y_low) / (X_high - X_low)
    y_diff <- mean(pred_high$fit) - mean(pred_low$fit)
    x_diff <- 2 * focal_sd  # high - low = (mean + SD) - (mean - SD) = 2*SD

    slope <- y_diff / x_diff

    # Standard error of slope (delta method approximation)
    # For interaction: slope at W = b1 + b3*W, where b1 is main effect, b3 is interaction
    # SE(slope) = sqrt(SE(b1)^2 + W^2 * SE(b3)^2 + 2*W*Cov(b1, b3))

    vcov_mat <- stats::vcov(model)

    # Get coefficient names
    b1_name <- focal
    b3_name <- interaction_term
    if (!b3_name %in% coef_names) {
      b3_name <- interaction_term_alt
    }

    if (b1_name %in% coef_names && b3_name %in% coef_names) {
      var_b1 <- vcov_mat[b1_name, b1_name]
      var_b3 <- vcov_mat[b3_name, b3_name]
      cov_b1_b3 <- vcov_mat[b1_name, b3_name]

      slope_se <- sqrt(var_b1 + mod_val^2 * var_b3 + 2 * mod_val * cov_b1_b3)
    } else {
      # Fallback: use SE from prediction
      slope_se <- sqrt(mean(pred_high$se.fit^2) + mean(pred_low$se.fit^2)) / x_diff
    }

    # t-statistic and p-value
    t_stat <- slope / slope_se
    df_resid <- stats::df.residual(model)
    p_value <- 2 * stats::pt(-abs(t_stat), df = df_resid)

    # Confidence interval
    t_crit <- stats::qt(1 - (1 - conf_level) / 2, df = df_resid)
    ci_lower <- slope - t_crit * slope_se
    ci_upper <- slope + t_crit * slope_se

    # Store results
    slopes_list[[i]] <- tibble::tibble(
      moderator_value = if (is.factor(mod_var)) mod_values[i] else mod_val,
      moderator_label = if (is.factor(mod_var)) {
        mod_values[i]
      } else {
        if (i == 1) "Low (-1 SD)" else if (i == 2) "Mean" else "High (+1 SD)"
      },
      slope = slope,
      std_error = slope_se,
      t_statistic = t_stat,
      p_value = p_value,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      significant = p_value < 0.05
    )
  }

  # Combine results
  slopes_df <- dplyr::bind_rows(slopes_list)

  # Interpretation
  sig_slopes <- slopes_df$significant
  if (all(sig_slopes)) {
    interpretation <- sprintf(
      "The effect of %s on the outcome is significant at all levels of %s. ",
      focal, moderator
    )
  } else if (any(sig_slopes)) {
    sig_labels <- slopes_df$moderator_label[sig_slopes]
    interpretation <- sprintf(
      "The effect of %s on the outcome is significant at %s levels of %s. ",
      focal,
      paste(sig_labels, collapse = " and "),
      moderator
    )
  } else {
    interpretation <- sprintf(
      "The effect of %s on the outcome is not significant at any tested level of %s. ",
      focal, moderator
    )
  }

  # Check if slopes differ
  slope_range <- max(slopes_df$slope) - min(slopes_df$slope)
  if (slope_range > 0.1) {
    interpretation <- paste0(
      interpretation,
      sprintf(
        "Simple slopes range from %.3f to %.3f, indicating the moderating effect of %s.",
        min(slopes_df$slope),
        max(slopes_df$slope),
        moderator
      )
    )
  }

  result <- list(
    slopes = slopes_df,
    mod_values = mod_values_numeric,
    focal = focal,
    moderator = moderator,
    conf_level = conf_level,
    interpretation = interpretation
  )

  class(result) <- "simple_slopes"
  result
}


#' @export
print.simple_slopes <- function(x, ...) {
  cat("Simple Slopes Analysis\n")
  cat("======================\n\n")

  cat(sprintf("Focal predictor: %s\n", x$focal))
  cat(sprintf("Moderator: %s\n", x$moderator))
  cat(sprintf("Confidence level: %.0f%%\n\n", x$conf_level * 100))

  cat("Simple Slopes at Moderator Values:\n")
  print(x$slopes, n = Inf)
  cat("\n")

  cat("Interpretation:\n")
  cat(sprintf("   %s\n", x$interpretation))

  invisible(x)
}


#' Johnson-Neyman Technique for Regions of Significance
#'
#' Identifies the range of moderator values where the effect of a focal predictor
#' is statistically significant. This provides more nuanced interpretation than
#' simple slopes at arbitrary values (e.g., +/- 1 SD), showing exactly where the
#' effect "turns on" and "turns off" as a function of the moderator.
#'
#' @param model A fitted linear model (lm object) with an interaction term
#' @param focal Character; name of the focal predictor (X variable)
#' @param moderator Character; name of the moderator variable (W variable)
#' @param alpha Significance level (default: 0.05)
#' @param mod_range Numeric vector of length 2 specifying range of moderator
#'   values to search. Default: NULL (uses observed range +/- 1 SD)
#' @param n_points Number of points to evaluate (default: 1000)
#'
#' @return A list of class "johnson_neyman" containing:
#'   - critical_values: Tibble with moderator values where effect transitions
#'     between significant and non-significant
#'   - regions: Tibble describing regions of significance
#'   - slopes_at_points: Tibble with slopes and CIs across moderator range
#'   - focal: Name of focal predictor
#'   - moderator: Name of moderator
#'   - interpretation: Publication-ready interpretation text
#'
#' @details
#' The Johnson-Neyman technique solves for the value(s) of W where the confidence
#' interval for the simple slope of X equals zero. This identifies "transition
#' points" between significant and non-significant effects.
#'
#' **Output Regions:**
#' - Region 1: W < lower bound -> effect significant/non-significant
#' - Region 2: lower < W < upper -> effect non-significant/significant
#' - Region 3: W > upper bound -> effect significant/non-significant
#'
#' **For JCP Publications:**
#' - Use JN technique for continuous moderators
#' - Report transition point(s) with precision
#' - Visualize with region of significance plot
#' - Describe substantive meaning (e.g., "The price effect is significant for
#'   customers with loyalty scores above 4.2")
#'
#' @references
#' Johnson, P. O., & Neyman, J. (1936). Tests of certain linear hypotheses and
#' their application to some educational problems. Statistical Research Memoirs, 1, 57-93.
#'
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
#' multilevel regression: Inferential and graphical techniques. Multivariate
#' Behavioral Research, 40(3), 373-400.
#'
#' @export
#' @examples
#' # Price sensitivity moderated by brand loyalty
#' set.seed(42)
#' n <- 200
#' df <- data.frame(
#'   price = rnorm(n, 50, 10),
#'   loyalty = rnorm(n, 5, 1.5),
#'   purchase = rnorm(n, 5, 1)
#' )
#' df$purchase <- df$purchase - 0.1 * df$price + 0.3 * df$loyalty +
#'                0.05 * df$price * df$loyalty
#'
#' model <- lm(purchase ~ price * loyalty, data = df)
#'
#' # Find regions of significance
#' jn <- johnson_neyman(model, focal = "price", moderator = "loyalty")
#' print(jn)
johnson_neyman <- function(model,
                           focal,
                           moderator,
                           alpha = 0.05,
                           mod_range = NULL,
                           n_points = 1000) {
  # Validate inputs
  if (!inherits(model, "lm")) {
    rlang::abort("`model` must be an lm object", class = "invalid_input")
  }

  # Check for interaction term
  interaction_term <- paste0(focal, ":", moderator)
  interaction_term_alt <- paste0(moderator, ":", focal)

  coef_names <- names(stats::coef(model))
  has_interaction <- interaction_term %in% coef_names || interaction_term_alt %in% coef_names

  if (!has_interaction) {
    rlang::abort(
      sprintf("Model does not contain interaction between '%s' and '%s'", focal, moderator),
      class = "no_interaction"
    )
  }

  # Get model data
  data <- model$model
  mod_var <- data[[moderator]]

  if (is.factor(mod_var)) {
    rlang::abort(
      "Johnson-Neyman technique requires continuous moderator. Use simple_slopes() for categorical moderators.",
      class = "categorical_moderator"
    )
  }

  # Get moderator range
  if (is.null(mod_range)) {
    mod_mean <- mean(mod_var, na.rm = TRUE)
    mod_sd <- stats::sd(mod_var, na.rm = TRUE)
    mod_min <- min(mod_var, na.rm = TRUE)
    mod_max <- max(mod_var, na.rm = TRUE)

    # Use observed range +/- 1 SD, but don't go beyond data range
    mod_range <- c(
      max(mod_min, mod_mean - 2 * mod_sd),
      min(mod_max, mod_mean + 2 * mod_sd)
    )
  }

  # Create sequence of moderator values
  mod_seq <- seq(mod_range[1], mod_range[2], length.out = n_points)

  # Get coefficients and variance-covariance matrix
  coefs <- stats::coef(model)
  vcov_mat <- stats::vcov(model)

  # Get coefficient names
  b1_name <- focal
  b3_name <- if (interaction_term %in% coef_names) interaction_term else interaction_term_alt

  b1 <- coefs[b1_name]
  b3 <- coefs[b3_name]

  var_b1 <- vcov_mat[b1_name, b1_name]
  var_b3 <- vcov_mat[b3_name, b3_name]
  cov_b1_b3 <- vcov_mat[b1_name, b3_name]

  # Critical t-value
  df_resid <- stats::df.residual(model)
  t_crit <- stats::qt(1 - alpha / 2, df = df_resid)

  # For each moderator value, calculate slope and CI
  slopes_df <- tibble::tibble(
    moderator_value = mod_seq,
    slope = b1 + b3 * mod_seq,
    std_error = sqrt(var_b1 + mod_seq^2 * var_b3 + 2 * mod_seq * cov_b1_b3),
    ci_lower = (b1 + b3 * mod_seq) - t_crit * sqrt(var_b1 + mod_seq^2 * var_b3 + 2 * mod_seq * cov_b1_b3),
    ci_upper = (b1 + b3 * mod_seq) + t_crit * sqrt(var_b1 + mod_seq^2 * var_b3 + 2 * mod_seq * cov_b1_b3),
    significant = !(ci_lower <= 0 & ci_upper >= 0)
  )

  # Find transition points (where CI crosses zero)
  # CI crosses zero when: (b1 + b3*W)^2 = t^2 * [var_b1 + W^2*var_b3 + 2*W*cov_b1_b3]
  # This is a quadratic equation in W

  # Rearrange: (b1 + b3*W)^2 - t^2 * [var_b1 + W^2*var_b3 + 2*W*cov_b1_b3] = 0
  # Expand and collect terms to get: a*W^2 + b*W + c = 0

  a_coef <- b3^2 - t_crit^2 * var_b3
  b_coef <- 2 * b1 * b3 - 2 * t_crit^2 * cov_b1_b3
  c_coef <- b1^2 - t_crit^2 * var_b1

  # Solve quadratic
  discriminant <- b_coef^2 - 4 * a_coef * c_coef

  if (discriminant < 0) {
    # No real solutions: effect is either always significant or never significant
    all_sig <- all(slopes_df$significant)
    none_sig <- !any(slopes_df$significant)

    critical_values <- tibble::tibble(
      transition_point = numeric(0),
      transition_type = character(0)
    )

    if (all_sig) {
      regions <- tibble::tibble(
        region = "All values",
        moderator_range = sprintf("[%.2f, %.2f]", mod_range[1], mod_range[2]),
        effect_status = "Significant"
      )
      interpretation <- sprintf(
        "The effect of %s is significant across the entire range of %s (%.2f to %.2f).",
        focal, moderator, mod_range[1], mod_range[2]
      )
    } else {
      regions <- tibble::tibble(
        region = "All values",
        moderator_range = sprintf("[%.2f, %.2f]", mod_range[1], mod_range[2]),
        effect_status = "Non-significant"
      )
      interpretation <- sprintf(
        "The effect of %s is not significant at any value of %s in the tested range (%.2f to %.2f).",
        focal, moderator, mod_range[1], mod_range[2]
      )
    }
  } else {
    # Two transition points
    w1 <- (-b_coef - sqrt(discriminant)) / (2 * a_coef)
    w2 <- (-b_coef + sqrt(discriminant)) / (2 * a_coef)

    # Order transition points
    trans_points <- sort(c(w1, w2))

    # Only keep points within our range
    trans_points <- trans_points[trans_points >= mod_range[1] & trans_points <= mod_range[2]]

    if (length(trans_points) == 0) {
      # No transitions in range
      critical_values <- tibble::tibble(
        transition_point = numeric(0),
        transition_type = character(0)
      )

      effect_status <- if (slopes_df$significant[1]) "Significant" else "Non-significant"
      regions <- tibble::tibble(
        region = "All values",
        moderator_range = sprintf("[%.2f, %.2f]", mod_range[1], mod_range[2]),
        effect_status = effect_status
      )

      interpretation <- sprintf(
        "The effect of %s is %s across the entire tested range of %s.",
        focal, tolower(effect_status), moderator
      )
    } else {
      # Create critical values table
      critical_values <- tibble::tibble(
        transition_point = trans_points,
        transition_type = "Significance threshold"
      )

      # Create regions
      if (length(trans_points) == 1) {
        # One transition point
        status_below <- if (slopes_df$significant[1]) "Significant" else "Non-significant"
        status_above <- if (slopes_df$significant[n_points]) "Significant" else "Non-significant"

        regions <- tibble::tibble(
          region = c(sprintf("%s < %.3f", moderator, trans_points[1]),
                     sprintf("%s >= %.3f", moderator, trans_points[1])),
          moderator_range = c(sprintf("[%.2f, %.3f)", mod_range[1], trans_points[1]),
                              sprintf("[%.3f, %.2f]", trans_points[1], mod_range[2])),
          effect_status = c(status_below, status_above)
        )

        interpretation <- sprintf(
          "The effect of %s transitions from %s to %s at %s = %.3f.",
          focal,
          tolower(status_below),
          tolower(status_above),
          moderator,
          trans_points[1]
        )
      } else {
        # Two transition points
        status_low <- if (slopes_df$significant[1]) "Significant" else "Non-significant"
        status_mid <- if (slopes_df$significant[round(n_points/2)]) "Significant" else "Non-significant"
        status_high <- if (slopes_df$significant[n_points]) "Significant" else "Non-significant"

        regions <- tibble::tibble(
          region = c(sprintf("%s < %.3f", moderator, trans_points[1]),
                     sprintf("%.3f <= %s < %.3f", trans_points[1], moderator, trans_points[2]),
                     sprintf("%s >= %.3f", moderator, trans_points[2])),
          moderator_range = c(sprintf("[%.2f, %.3f)", mod_range[1], trans_points[1]),
                              sprintf("[%.3f, %.3f)", trans_points[1], trans_points[2]),
                              sprintf("[%.3f, %.2f]", trans_points[2], mod_range[2])),
          effect_status = c(status_low, status_mid, status_high)
        )

        interpretation <- sprintf(
          "The effect of %s is %s when %s < %.3f, %s when %.3f <= %s < %.3f, and %s when %s >= %.3f.",
          focal,
          tolower(status_low), moderator, trans_points[1],
          tolower(status_mid), trans_points[1], moderator, trans_points[2],
          tolower(status_high), moderator, trans_points[2]
        )
      }
    }
  }

  result <- list(
    critical_values = critical_values,
    regions = regions,
    slopes_at_points = slopes_df,
    focal = focal,
    moderator = moderator,
    alpha = alpha,
    mod_range = mod_range,
    interpretation = interpretation
  )

  class(result) <- "johnson_neyman"
  result
}


#' @export
print.johnson_neyman <- function(x, ...) {
  cat("Johnson-Neyman Technique\n")
  cat("========================\n\n")

  cat(sprintf("Focal predictor: %s\n", x$focal))
  cat(sprintf("Moderator: %s\n", x$moderator))
  cat(sprintf("Significance level: alpha = %.3f\n\n", x$alpha))

  if (nrow(x$critical_values) > 0) {
    cat("Transition Point(s):\n")
    print(x$critical_values, n = Inf)
    cat("\n")
  } else {
    cat("No transition points found in the tested range.\n\n")
  }

  cat("Regions of Significance:\n")
  print(x$regions, n = Inf)
  cat("\n")

  cat("Interpretation:\n")
  cat(sprintf("   %s\n", x$interpretation))

  invisible(x)
}
