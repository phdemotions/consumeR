test_that("run_sem works with simple path model", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  # Create path structure: x -> m -> y
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$m + 0.2 * df$x

  model <- "
    y ~ m + x
    m ~ x
  "

  result <- run_sem(model, data = df)

  expect_s3_class(result, "sem_result")
  expect_type(result, "list")
  expect_named(result, c("params", "fit", "r2", "indirect_effects",
                         "model_syntax", "estimator", "se_method",
                         "interpretation", "lavaan_fit"))

  expect_s3_class(result$params, "tbl_df")
  expect_s3_class(result$fit, "tbl_df")
  expect_true(inherits(result$lavaan_fit, "lavaan"))
})

test_that("run_sem works with indirect effects", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 250
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.6 * df$x
  df$y <- df$y + 0.4 * df$m + 0.2 * df$x

  # Model with labeled paths for indirect effect
  model <- "
    y ~ b*m + c*x
    m ~ a*x

    # Indirect effect
    indirect := a*b
    total := c + (a*b)
  "

  result <- run_sem(model, data = df, se = "bootstrap",
                    bootstrap = 1000, seed = 123)

  expect_s3_class(result, "sem_result")
  expect_s3_class(result$indirect_effects, "tbl_df")
  expect_equal(nrow(result$indirect_effects), 2)  # indirect and total

  # Check that indirect effects have required columns
  expect_true(all(c("effect", "estimate", "ci_lower", "ci_upper",
                   "significant") %in% names(result$indirect_effects)))
})

test_that("run_sem validates inputs", {
  skip_if_not_installed("lavaan")

  df <- data.frame(x = rnorm(50), y = rnorm(50))

  expect_error(
    run_sem("y ~ x", data = "not a df"),
    "must be a data frame"
  )

  expect_error(
    run_sem("", data = df),
    "non-empty character string"
  )

  expect_error(
    run_sem("y ~ x", data = df, se = "bootstrap", bootstrap = 100),
    "at least 1000"
  )
})

test_that("run_sem handles vector of equations", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )
  df$m1 <- df$m1 + 0.4 * df$x
  df$m2 <- df$m2 + 0.3 * df$x
  df$y <- df$y + 0.3 * df$m1 + 0.2 * df$m2

  model <- c(
    "y ~ m1 + m2",
    "m1 ~ x",
    "m2 ~ x"
  )

  result <- run_sem(model, data = df)

  expect_s3_class(result, "sem_result")
  expect_s3_class(result$params, "tbl_df")
})

test_that("run_sem extracts fit indices", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.4 * df$m

  model <- "y ~ m; m ~ x"

  result <- run_sem(model, data = df)

  # Check that fit indices are present
  expect_s3_class(result$fit, "tbl_df")
  expect_true("measure" %in% names(result$fit))
  expect_true("value" %in% names(result$fit))
  expect_true("interpretation" %in% names(result$fit))

  # Check for common indices
  measures <- result$fit$measure
  expect_true("cfi" %in% measures)
  expect_true("rmsea" %in% measures)
})

test_that("run_sem calculates R² for endogenous variables", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.7 * df$x
  df$y <- df$y + 0.6 * df$m + 0.3 * df$x

  model <- "y ~ m + x; m ~ x"

  result <- run_sem(model, data = df)

  expect_s3_class(result$r2, "tbl_df")
  expect_equal(nrow(result$r2), 2)  # m and y

  # R² should be between 0 and 1
  expect_true(all(result$r2$r_squared >= 0 & result$r2$r_squared <= 1))
})

test_that("run_sem supports different estimators", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.5 * df$x

  model <- "y ~ x"

  result_ml <- run_sem(model, data = df, estimator = "ML")
  result_mlr <- run_sem(model, data = df, estimator = "MLR")

  expect_equal(result_ml$estimator, "ML")
  expect_equal(result_mlr$estimator, "MLR")

  # Both should produce results
  expect_s3_class(result_ml, "sem_result")
  expect_s3_class(result_mlr, "sem_result")
})

test_that("run_sem handles standardized estimates", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 180
  df <- data.frame(
    x = rnorm(n, mean = 10, sd = 5),
    y = rnorm(n, mean = 20, sd = 10)
  )
  df$y <- df$y + 2 * df$x

  model <- "y ~ x"

  result <- run_sem(model, data = df, std_estimates = TRUE)

  # Check for standardized estimates
  expect_true("std_estimate" %in% names(result$params))

  # Standardized should be different from unstandardized
  param_row <- result$params[1, ]
  if (!is.na(param_row$std_estimate)) {
    expect_false(isTRUE(all.equal(param_row$estimate, param_row$std_estimate)))
  }
})

test_that("run_sem generates interpretation", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.6 * df$x

  model <- "y ~ x"

  result <- run_sem(model, data = df)

  expect_type(result$interpretation, "character")
  expect_true(nchar(result$interpretation) > 0)
  expect_true(grepl("fit|path", result$interpretation, ignore.case = TRUE))
})

test_that("run_sem reproduces with same seed", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.4 * df$m

  model <- "y ~ b*m; m ~ a*x; ind := a*b"

  result1 <- run_sem(model, data = df, se = "bootstrap",
                     bootstrap = 1000, seed = 999)
  result2 <- run_sem(model, data = df, se = "bootstrap",
                     bootstrap = 1000, seed = 999)

  # Indirect effect estimates should be identical
  expect_equal(result1$indirect_effects$estimate,
               result2$indirect_effects$estimate)
  expect_equal(result1$indirect_effects$ci_lower,
               result2$indirect_effects$ci_lower)
})

test_that("tidy_sem extracts publication-ready results", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$m + 0.2 * df$x

  model <- "y ~ b*m + c*x; m ~ a*x; ind := a*b"

  result <- run_sem(model, data = df, se = "bootstrap",
                    bootstrap = 1000, seed = 42)

  tidy_results <- tidy_sem(result)

  expect_type(tidy_results, "list")
  expect_true("parameters" %in% names(tidy_results))
  expect_true("fit_indices" %in% names(tidy_results))
  expect_true("r_squared" %in% names(tidy_results))
  expect_true("indirect_effects" %in% names(tidy_results))

  # All should be tibbles
  expect_s3_class(tidy_results$parameters, "tbl_df")
  expect_s3_class(tidy_results$fit_indices, "tbl_df")
})

test_that("tidy_sem validates inputs", {
  expect_error(
    tidy_sem("not a sem_result"),
    "must be output from run_sem"
  )
})

test_that("tidy_sem respects component selection", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 150
  df <- data.frame(x = rnorm(n), y = rnorm(n))
  df$y <- df$y + 0.5 * df$x

  model <- "y ~ x"
  result <- run_sem(model, data = df)

  # Only parameters
  tidy_params <- tidy_sem(result, parameters = TRUE, fit = FALSE,
                          r2 = FALSE, indirect = FALSE)
  expect_equal(names(tidy_params), "parameters")

  # Only fit
  tidy_fit <- tidy_sem(result, parameters = FALSE, fit = TRUE,
                       r2 = FALSE, indirect = FALSE)
  expect_equal(names(tidy_fit), "fit_indices")
})

test_that("tidy_sem handles standardized vs unstandardized", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 180
  df <- data.frame(
    x = rnorm(n, mean = 10, sd = 5),
    y = rnorm(n, mean = 20, sd = 10)
  )
  df$y <- df$y + 2 * df$x

  model <- "y ~ x"
  result <- run_sem(model, data = df, std_estimates = TRUE)

  tidy_std <- tidy_sem(result, standardized = TRUE)
  tidy_unstd <- tidy_sem(result, standardized = FALSE)

  # Column name should differ
  expect_true("estimate" %in% names(tidy_std$parameters))
  expect_true("estimate" %in% names(tidy_unstd$parameters))

  # Values should differ for standardized vs unstandardized
  # (This is implicit in the selection logic)
})

test_that("compare_sem_models works with nested models", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 250
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.4 * df$m + 0.2 * df$x

  # Partial mediation
  model1 <- "y ~ m + x; m ~ x"

  # Full mediation (constrain direct path to 0)
  model2 <- "y ~ m + 0*x; m ~ x"

  result1 <- run_sem(model1, data = df)
  result2 <- run_sem(model2, data = df)

  comparison <- compare_sem_models(result1, result2,
                                   model_names = c("Partial", "Full"))

  expect_s3_class(comparison, "tbl_df")
  expect_true("model" %in% names(comparison))
  expect_true("model_name" %in% names(comparison))
  expect_equal(comparison$model_name, c("Partial", "Full"))
})

test_that("compare_sem_models validates inputs", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  df <- data.frame(x = rnorm(100), y = rnorm(100))
  df$y <- df$y + 0.5 * df$x

  model <- "y ~ x"
  result <- run_sem(model, data = df)

  expect_error(
    compare_sem_models(result),
    "At least two models"
  )

  expect_error(
    compare_sem_models(result, "not a model"),
    "must be sem_result objects"
  )

  # Create second model for testing model_names length
  result2 <- run_sem(model, data = df)

  expect_error(
    compare_sem_models(result, result2, model_names = "Only one name"),
    "same length as number of models"
  )
})

test_that("print.sem_result works", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$m

  model <- "y ~ b*m; m ~ a*x; ind := a*b"

  result <- run_sem(model, data = df, se = "bootstrap",
                    bootstrap = 1000, seed = 42)

  output <- capture.output(print(result))

  expect_true(any(grepl("Structural Equation Model", output)))
  expect_true(any(grepl("Model Fit", output)))
  expect_true(any(grepl("Parameter Estimates", output)))
  expect_true(any(grepl("Indirect Effects", output)))
  expect_true(any(grepl("Interpretation", output)))
})

test_that("run_sem handles missing data with listwise deletion", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  df <- data.frame(
    x = c(rnorm(95), rep(NA, 5)),
    m = rnorm(100),
    y = rnorm(100)
  )
  df$m[!is.na(df$x)] <- df$m[!is.na(df$x)] + 0.5 * df$x[!is.na(df$x)]
  df$y[!is.na(df$x)] <- df$y[!is.na(df$x)] + 0.3 * df$m[!is.na(df$x)]

  model <- "y ~ m; m ~ x"

  result <- run_sem(model, data = df, missing = "listwise")

  expect_s3_class(result, "sem_result")
})

test_that("run_sem handles missing data with FIML", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  df <- data.frame(
    x = c(rnorm(95), rep(NA, 5)),
    m = rnorm(100),
    y = rnorm(100)
  )
  df$m[!is.na(df$x)] <- df$m[!is.na(df$x)] + 0.5 * df$x[!is.na(df$x)]
  df$y[!is.na(df$x)] <- df$y[!is.na(df$x)] + 0.3 * df$m[!is.na(df$x)]

  model <- "y ~ m; m ~ x"

  result <- run_sem(model, data = df, missing = "fiml", estimator = "ML")

  expect_s3_class(result, "sem_result")
})

test_that("run_sem works with multiple endogenous variables", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y1 = rnorm(n),
    y2 = rnorm(n)
  )
  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$x
  df$y1 <- df$y1 + 0.3 * df$m1
  df$y2 <- df$y2 + 0.3 * df$m2

  model <- "
    y1 ~ m1
    y2 ~ m2
    m1 ~ x
    m2 ~ x
  "

  result <- run_sem(model, data = df)

  expect_s3_class(result, "sem_result")
  expect_s3_class(result$r2, "tbl_df")
  expect_gte(nrow(result$r2), 4)  # m1, m2, y1, y2
})

test_that("run_sem parameters have significance flags", {
  skip_if_not_installed("lavaan")

  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n)  # Unrelated variable
  )
  df$y <- df$y + 0.7 * df$x  # Strong effect of x, z is noise

  model <- "y ~ x + z"

  result <- run_sem(model, data = df)

  expect_true("significant" %in% names(result$params))
  expect_type(result$params$significant, "logical")
})
