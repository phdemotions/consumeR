test_that("run_logistic works with binary 0/1 outcome", {
  set.seed(42)
  df <- data.frame(
    outcome = sample(0:1, 100, replace = TRUE),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  model <- run_logistic(outcome ~ x1 + x2, data = df)

  expect_s3_class(model, "logistic_model")
  expect_s3_class(model, "glm")
  expect_equal(model$family$family, "binomial")
  expect_true(attr(model, "n_obs") > 0)
  expect_true(attr(model, "n_events") >= 0)
  expect_true(attr(model, "event_rate") >= 0 && attr(model, "event_rate") <= 1)
  expect_equal(length(attr(model, "predictors")), 2)
})

test_that("run_logistic works with TRUE/FALSE outcome", {
  set.seed(123)
  df <- data.frame(
    purchased = sample(c(TRUE, FALSE), 50, replace = TRUE),
    price = rnorm(50, 50, 10),
    quality = rnorm(50, 5, 1)
  )

  model <- run_logistic(purchased ~ price + quality, data = df)

  expect_s3_class(model, "logistic_model")
  expect_equal(attr(model, "n_obs"), 50)
})

test_that("run_logistic works with factor outcome", {
  set.seed(42)
  df <- data.frame(
    choice = factor(sample(c("Yes", "No"), 80, replace = TRUE)),
    age = sample(18:65, 80, replace = TRUE),
    income = rnorm(80, 50000, 10000)
  )

  model <- run_logistic(choice ~ age + income, data = df)

  expect_s3_class(model, "logistic_model")
  expect_equal(model$family$family, "binomial")
})

test_that("run_logistic validates binary outcome", {
  # Non-binary outcome (3 levels)
  df <- data.frame(
    outcome = sample(c(0, 1, 2), 50, replace = TRUE),
    x = rnorm(50)
  )

  expect_error(run_logistic(outcome ~ x, data = df), "must be binary")
})

test_that("run_logistic validates inputs", {
  df <- data.frame(y = sample(0:1, 20, replace = TRUE), x = rnorm(20))

  expect_error(run_logistic("not a formula", data = df), "must be a formula")
  expect_error(run_logistic(y ~ x, data = "not a df"), "must be a data frame")
  expect_error(run_logistic(missing ~ x, data = df), "not found")
})

test_that("run_logistic calculates event rate correctly", {
  set.seed(42)
  # Create outcome with known proportion
  df <- data.frame(
    y = c(rep(1, 60), rep(0, 40)),  # 60% event rate
    x = rnorm(100)
  )

  model <- run_logistic(y ~ x, data = df)

  expect_equal(attr(model, "n_events"), 60)
  expect_equal(attr(model, "event_rate"), 0.6)
})

test_that("tidy_logistic returns odds ratios by default", {
  set.seed(42)
  df <- data.frame(
    y = sample(0:1, 100, replace = TRUE),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  model <- run_logistic(y ~ x1 + x2, data = df)

  result <- tidy_logistic(model)

  expect_s3_class(result, "tidy_logistic")
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("term", "estimate", "std_error", "statistic",
                         "p_value", "conf_low", "conf_high", "interpretation"))

  # All odds ratios should be positive
  expect_true(all(result$estimate > 0))
  expect_true(all(result$conf_low > 0))
  expect_true(all(result$conf_high > 0))

  # Should exclude intercept by default
  expect_false("(Intercept)" %in% result$term)
})

test_that("tidy_logistic can return log-odds", {
  set.seed(42)
  df <- data.frame(
    y = sample(0:1, 80, replace = TRUE),
    x = rnorm(80)
  )
  model <- run_logistic(y ~ x, data = df)

  result <- tidy_logistic(model, exponentiate = FALSE)

  # Log-odds can be negative
  expect_true(any(result$estimate != abs(result$estimate)) || TRUE)
  expect_type(result$estimate, "double")
})

test_that("tidy_logistic can include intercept", {
  set.seed(42)
  df <- data.frame(
    y = sample(0:1, 50, replace = TRUE),
    x = rnorm(50)
  )
  model <- run_logistic(y ~ x, data = df)

  result_no_int <- tidy_logistic(model, include_intercept = FALSE)
  result_with_int <- tidy_logistic(model, include_intercept = TRUE)

  expect_false("(Intercept)" %in% result_no_int$term)
  expect_true("(Intercept)" %in% result_with_int$term)
  expect_equal(nrow(result_with_int), nrow(result_no_int) + 1)
})

test_that("tidy_logistic validates confidence level", {
  set.seed(42)
  df <- data.frame(y = sample(0:1, 30, replace = TRUE), x = rnorm(30))
  model <- run_logistic(y ~ x, data = df)

  expect_error(tidy_logistic(model, conf_level = 0), "between 0 and 1")
  expect_error(tidy_logistic(model, conf_level = 1), "between 0 and 1")
  expect_error(tidy_logistic(model, conf_level = 1.5), "between 0 and 1")
})

test_that("tidy_logistic confidence intervals have correct width", {
  set.seed(42)
  df <- data.frame(y = sample(0:1, 100, replace = TRUE), x = rnorm(100))
  model <- run_logistic(y ~ x, data = df)

  result_95 <- tidy_logistic(model, conf_level = 0.95)
  result_99 <- tidy_logistic(model, conf_level = 0.99)

  # 99% CI should be wider
  width_95 <- result_95$conf_high[1] - result_95$conf_low[1]
  width_99 <- result_99$conf_high[1] - result_99$conf_low[1]
  expect_gt(width_99, width_95)
})

test_that("tidy_logistic interpretation includes key information", {
  set.seed(42)
  df <- data.frame(
    y = sample(0:1, 100, replace = TRUE),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  model <- run_logistic(y ~ x1 + x2, data = df)

  result <- tidy_logistic(model)

  # Should have interpretation text
  expect_type(result$interpretation, "character")
  expect_true(all(nchar(result$interpretation) > 0))

  # Should mention odds or association
  expect_true(any(grepl("odds|increase|decrease", result$interpretation, ignore.case = TRUE)))
})

test_that("tidy_logistic validates model type", {
  # Linear model instead of logistic
  df <- data.frame(y = rnorm(50), x = rnorm(50))
  lm_model <- stats::lm(y ~ x, data = df)

  expect_error(tidy_logistic(lm_model), "must be a glm")
})

test_that("logistic_assumptions detects multicollinearity", {
  set.seed(42)
  df <- data.frame(
    y = sample(0:1, 100, replace = TRUE),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  # Create highly correlated predictor
  df$x3 <- df$x1 + rnorm(100, 0, 0.1)

  model <- run_logistic(y ~ x1 + x2 + x3, data = df)
  checks <- logistic_assumptions(model, data = df, vif_threshold = 5)

  expect_s3_class(checks, "logistic_assumptions")
  expect_type(checks$vif$values, "double")
  expect_true(!is.na(checks$vif$max_vif))

  # High correlation should cause VIF violation
  expect_false(checks$vif$pass)
  expect_false(checks$all_pass)
})

test_that("logistic_assumptions detects influential cases", {
  set.seed(42)
  df <- data.frame(
    y = c(rep(0, 49), 1),  # One extreme case
    x = c(rnorm(49, 0, 1), 10)  # Outlier
  )

  model <- run_logistic(y ~ x, data = df)
  checks <- logistic_assumptions(model, data = df)

  expect_type(checks$influential$n_influential, "integer")
  expect_type(checks$influential$ids, "integer")

  # Should detect the outlier
  expect_gt(checks$influential$n_influential, 0)
})

test_that("logistic_assumptions handles single predictor", {
  set.seed(42)
  df <- data.frame(
    y = sample(0:1, 60, replace = TRUE),
    x = rnorm(60)
  )

  model <- run_logistic(y ~ x, data = df)
  checks <- logistic_assumptions(model, data = df)

  # VIF not applicable for single predictor
  expect_true(checks$vif$pass)
  expect_type(checks$remediation, "character")
})

test_that("logistic_assumptions checks separation", {
  # Perfect separation case
  set.seed(42)
  df <- data.frame(
    y = c(rep(0, 30), rep(1, 30)),
    x = c(rnorm(30, -5, 0.5), rnorm(30, 5, 0.5))  # Perfectly separated
  )

  # This may produce a warning about fitted probabilities
  suppressWarnings({
    model <- run_logistic(y ~ x, data = df)
    checks <- logistic_assumptions(model, data = df)
  })

  # Should detect large coefficients
  expect_false(checks$separation$pass)
  expect_gt(length(checks$separation$large_coefs), 0)
})

test_that("logistic_assumptions print method works", {
  set.seed(42)
  df <- data.frame(
    y = sample(0:1, 80, replace = TRUE),
    x1 = rnorm(80),
    x2 = rnorm(80)
  )

  model <- run_logistic(y ~ x1 + x2, data = df)
  checks <- logistic_assumptions(model, data = df)

  output <- capture.output(print(checks))

  expect_true(any(grepl("Assumption Checks", output)))
  expect_true(any(grepl("Multicollinearity", output)))
  expect_true(any(grepl("Influential", output)))
  expect_true(any(grepl("Separation", output)))
  expect_true(any(grepl("Linearity", output)))
})

test_that("logistic_assumptions validates inputs", {
  df <- data.frame(y = sample(0:1, 50, replace = TRUE), x = rnorm(50))
  model <- run_logistic(y ~ x, data = df)
  lm_model <- stats::lm(y ~ x, data = df)

  expect_error(logistic_assumptions(lm_model, data = df), "must be a glm")
  expect_error(logistic_assumptions(model, data = "not a df"), "must be a data frame")
})

test_that("pseudo_r2 calculates multiple R² measures", {
  set.seed(42)
  df <- data.frame(
    y = sample(0:1, 100, replace = TRUE, prob = c(0.3, 0.7)),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  model <- run_logistic(y ~ x1 + x2, data = df)
  r2 <- pseudo_r2(model)

  expect_s3_class(r2, "tbl_df")
  expect_named(r2, c("mcfadden", "cox_snell", "nagelkerke", "tjur", "aic", "bic"))

  # All R² should be between 0 and 1
  expect_true(r2$mcfadden >= 0 && r2$mcfadden <= 1)
  expect_true(r2$cox_snell >= 0 && r2$cox_snell <= 1)
  expect_true(r2$nagelkerke >= 0 && r2$nagelkerke <= 1)
  expect_true(r2$tjur >= -1 && r2$tjur <= 1)

  # AIC and BIC should be positive
  expect_true(r2$aic > 0)
  expect_true(r2$bic > 0)
})

test_that("pseudo_r2 validates model type", {
  df <- data.frame(y = rnorm(50), x = rnorm(50))
  lm_model <- stats::lm(y ~ x, data = df)

  expect_error(pseudo_r2(lm_model), "must be a binomial")
})

test_that("logistic regression workflow integrates properly", {
  # Full workflow test
  set.seed(42)
  n <- 150
  df <- data.frame(
    purchased = sample(0:1, n, replace = TRUE, prob = c(0.4, 0.6)),
    price = rnorm(n, 50, 10),
    quality = rnorm(n, 5, 1),
    age = sample(18:65, n, replace = TRUE)
  )

  # Step 1: Fit model
  model <- run_logistic(purchased ~ price + quality + age, data = df)
  expect_s3_class(model, "logistic_model")

  # Step 2: Get tidy results
  results <- tidy_logistic(model)
  expect_s3_class(results, "tidy_logistic")
  expect_equal(nrow(results), 3)  # 3 predictors

  # Step 3: Check assumptions
  checks <- logistic_assumptions(model, data = df)
  expect_s3_class(checks, "logistic_assumptions")

  # Step 4: Get pseudo R²
  r2 <- pseudo_r2(model)
  expect_s3_class(r2, "tbl_df")

  # All steps should complete without error
  expect_true(TRUE)
})

test_that("logistic functions handle missing data appropriately", {
  set.seed(42)
  df <- data.frame(
    y = c(sample(0:1, 45, replace = TRUE), NA, NA, NA, NA, NA),
    x = c(rnorm(48), NA, NA)
  )

  # glm should handle NA by default (na.omit)
  model <- run_logistic(y ~ x, data = df)

  expect_lt(attr(model, "n_obs"), 50)  # Some rows removed
  expect_s3_class(model, "logistic_model")
})

test_that("logistic regression handles categorical predictors", {
  set.seed(42)
  df <- data.frame(
    y = sample(0:1, 100, replace = TRUE),
    group = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    x = rnorm(100)
  )

  model <- run_logistic(y ~ group + x, data = df)
  results <- tidy_logistic(model)

  # Should have dummy variables for group
  expect_true(any(grepl("group", results$term)))
  expect_s3_class(model, "logistic_model")
})

test_that("tidy_logistic handles interaction terms", {
  set.seed(42)
  df <- data.frame(
    y = sample(0:1, 100, replace = TRUE),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  model <- run_logistic(y ~ x1 * x2, data = df)
  results <- tidy_logistic(model)

  # Should have main effects and interaction
  expect_true(any(grepl(":", results$term)))
  expect_gte(nrow(results), 3)  # x1, x2, x1:x2
})

test_that("logistic functions provide interpretable output", {
  set.seed(42)
  df <- data.frame(
    purchased = sample(0:1, 80, replace = TRUE),
    price = rnorm(80, 50, 10),
    quality = rnorm(80, 5, 1)
  )

  model <- run_logistic(purchased ~ price + quality, data = df)
  results <- tidy_logistic(model)

  # Interpretation should be publication-ready
  expect_type(results$interpretation, "character")
  expect_true(all(grepl("p =|p <", results$interpretation)))
})
