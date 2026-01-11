#' Moderated Mediation Analysis (Hayes PROCESS Models 7, 8, 14)
#'
#' Performs moderated mediation analysis where the indirect effect of X on Y through
#' M depends on a moderator W. This tests conditional process models as described
#' by Hayes (2018), allowing researchers to examine when and for whom mediation occurs.
#'
#' @param data A data frame containing the variables
#' @param x Character; name of the independent variable (predictor)
#' @param m Character; name of the mediator variable
#' @param y Character; name of the dependent variable (outcome)
#' @param moderator Character; name of the moderator variable
#' @param model Character; which Hayes PROCESS model:
#'   "7" (default): Moderates M→Y path (b path)
#'   "8": Moderates both X→M and M→Y paths (a and b paths)
#'   "14": Moderates X→M path (a path)
#' @param moderator_values Numeric vector of moderator values to probe. Default: NULL
#'   (uses mean ± 1 SD)
#' @param covariates Character vector; names of covariates to control for (optional)
#' @param conf_level Confidence level for intervals (default: 0.95)
#' @param boot_samples Number of bootstrap samples (default: 5000; min 1000)
#' @param boot_method Bootstrap CI method: "bca" (default), "perc", or "norm"
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return A list of class "moderated_mediation" containing:
#'   - paths: Path coefficients including interaction terms
#'   - conditional_indirect: Conditional indirect effects at moderator values
#'   - index_modmed: Index of moderated mediation with bootstrap CI
#'   - simple_slopes: Simple slopes of moderated paths
#'   - boot_samples: Number of bootstrap samples used
#'   - model_type: Which Hayes model was fit
#'   - interpretation: Publication-ready text
#'
#' @details
#' **Hayes PROCESS Models:**
#'
#' **Model 7** (Moderator on b path, M→Y):
#' - X → M (path a)
#' - M → Y depends on W (path b + interaction)
#' - Indirect effect = a × (b + b₃W)
#' - Index of moderated mediation = a × b₃
#'
#' **Model 8** (Moderator on both a and b paths):
#' - X → M depends on W (path a + interaction)
#' - M → Y depends on W (path b + interaction)
#' - Indirect effect = (a + a₃W) × (b + b₃W)
#' - Most complex model
#'
#' **Model 14** (Moderator on a path, X→M):
#' - X → M depends on W (path a + interaction)
#' - M → Y (path b)
#' - Indirect effect = (a + a₃W) × b
#' - Index of moderated mediation = a₃ × b
#'
#' **Index of Moderated Mediation:**
#' - Quantifies how much the indirect effect changes per unit of moderator
#' - Significant index = moderated mediation is present
#' - Bootstrap CI that excludes zero indicates significance
#'
#' **Conditional Indirect Effects:**
#' - Indirect effect at low W (M - 1 SD)
#' - Indirect effect at mean W
#' - Indirect effect at high W (M + 1 SD)
#' - Each with bootstrap CIs
#'
#' **For JCP Publications:**
#' - Report index of moderated mediation with bootstrap CI
#' - Report conditional indirect effects at low/mean/high W
#' - Report which Hayes model was used
#' - Visualize conditional indirect effects
#' - Use 5000+ bootstrap samples
#'
#' @references
#' Hayes, A. F. (2018). Introduction to mediation, moderation, and conditional
#' process analysis: A regression-based approach (2nd ed.). Guilford Press.
#'
#' Hayes, A. F. (2015). An index and test of linear moderated mediation.
#' Multivariate Behavioral Research, 50(1), 1-22.
#'
#' @export
#' @examples
#' # Model 7: Trust mediates CSR → loyalty, moderated by involvement
#' set.seed(42)
#' n <- 200
#' df <- data.frame(
#'   csr = rnorm(n),
#'   trust = rnorm(n),
#'   loyalty = rnorm(n),
#'   involvement = rnorm(n)
#' )
#' # Create moderated mediation structure
#' df$trust <- df$trust + 0.5 * df$csr
#' df$loyalty <- df$loyalty + 0.3 * df$trust +
#'   0.4 * df$involvement + 0.2 * df$trust * df$involvement
#'
#' result <- moderated_mediation(
#'   data = df,
#'   x = "csr",
#'   m = "trust",
#'   y = "loyalty",
#'   moderator = "involvement",
#'   model = "7",
#'   boot_samples = 1000,  # Use 5000+ for publication
#'   seed = 42
#' )
#' print(result)
moderated_mediation <- function(data,
                               x,
                               m,
                               y,
                               moderator,
                               model = c("7", "8", "14"),
                               moderator_values = NULL,
                               covariates = NULL,
                               conf_level = 0.95,
                               boot_samples = 5000,
                               boot_method = c("bca", "perc", "norm"),
                               seed = NULL) {

  # Validate inputs
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame", class = "invalid_input")
  }

  model <- match.arg(model)
  boot_method <- match.arg(boot_method)

  vars_needed <- c(x, m, y, moderator, covariates)
  missing_vars <- setdiff(vars_needed, names(data))
  if (length(missing_vars) > 0) {
    rlang::abort(
      sprintf("Variables not found: %s", paste(missing_vars, collapse = ", ")),
      class = "variable_not_found"
    )
  }

  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    rlang::abort("`conf_level` must be between 0 and 1", class = "invalid_input")
  }

  if (!is.numeric(boot_samples) || boot_samples < 1000) {
    rlang::abort(
      "`boot_samples` must be at least 1000 for reliable inference (5000+ recommended)",
      class = "invalid_input"
    )
  }

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Remove missing data
  data_complete <- data[stats::complete.cases(data[, vars_needed]), ]
  n_removed <- nrow(data) - nrow(data_complete)

  if (n_removed > 0) {
    message(sprintf("Removed %d rows with missing data", n_removed))
  }

  # Get moderator values for conditional effects
  mod_var <- data_complete[[moderator]]

  if (is.null(moderator_values)) {
    mod_mean <- mean(mod_var, na.rm = TRUE)
    mod_sd <- stats::sd(mod_var, na.rm = TRUE)
    moderator_values <- c(mod_mean - mod_sd, mod_mean, mod_mean + mod_sd)
    mod_labels <- c("Low (-1 SD)", "Mean", "High (+1 SD)")
  } else {
    mod_labels <- as.character(moderator_values)
  }

  # Center moderator for interpretation
  data_complete$moderator_c <- data_complete[[moderator]] - mean(mod_var)

  # Build formulas based on model type
  if (is.null(covariates)) {
    cov_string <- ""
  } else {
    cov_string <- paste0(" + ", paste(covariates, collapse = " + "))
  }

  if (model == "7") {
    # Model 7: Moderator on b path (M→Y)
    # a path: M ~ X
    formula_a <- stats::as.formula(sprintf("%s ~ %s%s", m, x, cov_string))

    # b path with moderation: Y ~ X + M + M*W
    formula_b <- stats::as.formula(sprintf(
      "%s ~ %s + %s + %s:moderator_c%s", y, x, m, m, cov_string
    ))

  } else if (model == "8") {
    # Model 8: Moderator on both a and b paths
    # a path with moderation: M ~ X + X*W
    formula_a <- stats::as.formula(sprintf(
      "%s ~ %s + %s:moderator_c%s", m, x, x, cov_string
    ))

    # b path with moderation: Y ~ X + M + M*W
    formula_b <- stats::as.formula(sprintf(
      "%s ~ %s + %s + %s:moderator_c%s", y, x, m, m, cov_string
    ))

  } else {  # model == "14"
    # Model 14: Moderator on a path (X→M)
    # a path with moderation: M ~ X + X*W
    formula_a <- stats::as.formula(sprintf(
      "%s ~ %s + %s:moderator_c%s", m, x, x, cov_string
    ))

    # b path: Y ~ X + M
    formula_b <- stats::as.formula(sprintf("%s ~ %s + %s%s", y, x, m, cov_string))
  }

  # Fit models
  model_a <- stats::lm(formula_a, data = data_complete)
  model_b <- stats::lm(formula_b, data = data_complete)

  # Extract coefficients
  coef_a <- stats::coef(model_a)
  coef_b <- stats::coef(model_b)

  # Store paths
  paths <- list(
    a = coef_a[x],
    b = coef_b[m],
    c_prime = coef_b[x]
  )

  # Add interaction terms depending on model
  if (model == "7") {
    paths$b_mod <- coef_b[paste0(m, ":moderator_c")]
    paths$a_mod <- NA
  } else if (model == "8") {
    paths$a_mod <- coef_a[paste0(x, ":moderator_c")]
    paths$b_mod <- coef_b[paste0(m, ":moderator_c")]
  } else {  # model == "14"
    paths$a_mod <- coef_a[paste0(x, ":moderator_c")]
    paths$b_mod <- NA
  }

  # Calculate conditional indirect effects at moderator values
  conditional_indirect <- list()

  for (i in seq_along(moderator_values)) {
    w_val <- moderator_values[i]
    w_centered <- w_val - mean(mod_var)

    if (model == "7") {
      # Indirect = a × (b + b₃W)
      indirect_w <- paths$a * (paths$b + paths$b_mod * w_centered)
    } else if (model == "8") {
      # Indirect = (a + a₃W) × (b + b₃W)
      indirect_w <- (paths$a + paths$a_mod * w_centered) *
        (paths$b + paths$b_mod * w_centered)
    } else {  # model == "14"
      # Indirect = (a + a₃W) × b
      indirect_w <- (paths$a + paths$a_mod * w_centered) * paths$b
    }

    conditional_indirect[[i]] <- list(
      moderator_value = w_val,
      moderator_label = mod_labels[i],
      indirect_effect = indirect_w
    )
  }

  # Bootstrap for CIs
  boot_indirect_matrix <- matrix(NA, nrow = boot_samples, ncol = length(moderator_values))
  boot_index_modmed <- numeric(boot_samples)

  for (b in seq_len(boot_samples)) {
    boot_indices <- sample(seq_len(nrow(data_complete)), replace = TRUE)
    boot_data <- data_complete[boot_indices, ]

    tryCatch({
      boot_model_a <- stats::lm(formula_a, data = boot_data)
      boot_model_b <- stats::lm(formula_b, data = boot_data)

      boot_coef_a <- stats::coef(boot_model_a)
      boot_coef_b <- stats::coef(boot_model_b)

      boot_a <- boot_coef_a[x]
      boot_b <- boot_coef_b[m]

      if (model == "7") {
        boot_b_mod <- boot_coef_b[paste0(m, ":moderator_c")]
        boot_index_modmed[b] <- boot_a * boot_b_mod

        for (i in seq_along(moderator_values)) {
          w_centered <- moderator_values[i] - mean(mod_var)
          boot_indirect_matrix[b, i] <- boot_a * (boot_b + boot_b_mod * w_centered)
        }

      } else if (model == "8") {
        boot_a_mod <- boot_coef_a[paste0(x, ":moderator_c")]
        boot_b_mod <- boot_coef_b[paste0(m, ":moderator_c")]

        # Index for Model 8 is complex - use product of interactions
        boot_index_modmed[b] <- boot_a_mod * boot_b_mod

        for (i in seq_along(moderator_values)) {
          w_centered <- moderator_values[i] - mean(mod_var)
          boot_indirect_matrix[b, i] <- (boot_a + boot_a_mod * w_centered) *
            (boot_b + boot_b_mod * w_centered)
        }

      } else {  # model == "14"
        boot_a_mod <- boot_coef_a[paste0(x, ":moderator_c")]
        boot_index_modmed[b] <- boot_a_mod * boot_b

        for (i in seq_along(moderator_values)) {
          w_centered <- moderator_values[i] - mean(mod_var)
          boot_indirect_matrix[b, i] <- (boot_a + boot_a_mod * w_centered) * boot_b
        }
      }

    }, error = function(e) {
      boot_indirect_matrix[b, ] <- NA
      boot_index_modmed[b] <- NA
    })
  }

  # Calculate bootstrap CIs for conditional indirect effects
  for (i in seq_along(moderator_values)) {
    boot_dist <- boot_indirect_matrix[, i]
    boot_dist <- boot_dist[!is.na(boot_dist)]

    if (boot_method == "perc") {
      ci_lower <- stats::quantile(boot_dist, (1 - conf_level) / 2)
      ci_upper <- stats::quantile(boot_dist, 1 - (1 - conf_level) / 2)
    } else if (boot_method == "norm") {
      boot_se <- stats::sd(boot_dist)
      z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)
      ci_lower <- conditional_indirect[[i]]$indirect_effect - z_crit * boot_se
      ci_upper <- conditional_indirect[[i]]$indirect_effect + z_crit * boot_se
    } else {  # bca
      # Simplified BCa
      z0 <- stats::qnorm(mean(boot_dist < conditional_indirect[[i]]$indirect_effect))
      alpha <- 1 - conf_level
      z_alpha_lower <- stats::qnorm(alpha / 2)
      z_alpha_upper <- stats::qnorm(1 - alpha / 2)

      p_lower <- stats::pnorm(z0 + z_alpha_lower)
      p_upper <- stats::pnorm(z0 + z_alpha_upper)

      ci_lower <- stats::quantile(boot_dist, p_lower)
      ci_upper <- stats::quantile(boot_dist, p_upper)
    }

    conditional_indirect[[i]]$ci_lower <- ci_lower
    conditional_indirect[[i]]$ci_upper <- ci_upper
    conditional_indirect[[i]]$significant <- !(ci_lower <= 0 && ci_upper >= 0)
  }

  # Calculate index of moderated mediation CI
  boot_index_clean <- boot_index_modmed[!is.na(boot_index_modmed)]

  if (model == "7") {
    index_point <- paths$a * paths$b_mod
  } else if (model == "8") {
    index_point <- paths$a_mod * paths$b_mod
  } else {
    index_point <- paths$a_mod * paths$b
  }

  if (boot_method == "perc") {
    index_ci_lower <- stats::quantile(boot_index_clean, (1 - conf_level) / 2)
    index_ci_upper <- stats::quantile(boot_index_clean, 1 - (1 - conf_level) / 2)
  } else {
    index_ci_lower <- stats::quantile(boot_index_clean, (1 - conf_level) / 2)
    index_ci_upper <- stats::quantile(boot_index_clean, 1 - (1 - conf_level) / 2)
  }

  index_significant <- !(index_ci_lower <= 0 && index_ci_upper >= 0)

  # Create conditional indirect tibble
  cond_ind_tibble <- dplyr::bind_rows(lapply(conditional_indirect, tibble::as_tibble))

  # Interpretation
  if (index_significant) {
    interpretation <- sprintf(
      "Moderated mediation detected (Hayes Model %s). Index of moderated mediation = %.3f, %d%% CI [%.3f, %.3f]. The indirect effect %s as %s increases.",
      model,
      index_point,
      round(conf_level * 100),
      index_ci_lower,
      index_ci_upper,
      if (index_point > 0) "strengthens" else "weakens",
      moderator
    )
  } else {
    interpretation <- sprintf(
      "No evidence of moderated mediation (Hayes Model %s). Index of moderated mediation = %.3f, %d%% CI [%.3f, %.3f] includes zero.",
      model,
      index_point,
      round(conf_level * 100),
      index_ci_lower,
      index_ci_upper
    )
  }

  # Compile results
  result <- list(
    paths = tibble::tibble(
      path = c("a (X→M)", "b (M→Y)", "c' (direct)", "a×W", "b×W"),
      estimate = c(paths$a, paths$b, paths$c_prime, paths$a_mod, paths$b_mod)
    ),
    conditional_indirect = cond_ind_tibble,
    index_modmed = tibble::tibble(
      index = index_point,
      ci_lower = index_ci_lower,
      ci_upper = index_ci_upper,
      significant = index_significant
    ),
    model_type = paste0("Hayes PROCESS Model ", model),
    boot_samples = length(boot_index_clean),
    boot_method = boot_method,
    conf_level = conf_level,
    interpretation = interpretation,
    variables = list(x = x, m = m, y = y, moderator = moderator)
  )

  class(result) <- "moderated_mediation"
  result
}


#' @export
print.moderated_mediation <- function(x, ...) {
  cat("Moderated Mediation Analysis\n")
  cat("============================\n\n")

  cat(sprintf("Model: %s\n", x$model_type))
  cat(sprintf("X = %s, M = %s, Y = %s, W = %s\n\n",
              x$variables$x, x$variables$m, x$variables$y, x$variables$moderator))

  cat("Path Coefficients:\n")
  print(x$paths, n = Inf)
  cat("\n")

  cat("Index of Moderated Mediation:\n")
  print(x$index_modmed, n = Inf)
  cat("\n")

  cat("Conditional Indirect Effects:\n")
  print(x$conditional_indirect, n = Inf)
  cat("\n")

  cat("Interpretation:\n")
  cat(sprintf("   %s\n", x$interpretation))

  invisible(x)
}
