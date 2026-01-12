test_that("moderated_mediation works with Model 7", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    w = rnorm(n)
  )
  # Create Model 7 structure: moderator on b path
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$m + 0.4 * df$w + 0.2 * df$m * df$w

  result <- moderated_mediation(
    data = df,
    x = "x",
    m = "m",
    y = "y",
    moderator = "w",
    model = "7",
    boot_samples = 1000,
    seed = 123
  )

  expect_s3_class(result, "moderated_mediation")
  expect_type(result, "list")
  expect_named(result, c("paths", "conditional_indirect", "index_modmed",
                         "model_type", "boot_samples", "boot_method",
                         "conf_level", "interpretation", "variables"))

  expect_equal(result$model_type, "Hayes PROCESS Model 7")
  expect_s3_class(result$conditional_indirect, "tbl_df")
  expect_equal(nrow(result$conditional_indirect), 3)  # Low, Mean, High
})

test_that("moderated_mediation works with Model 8", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    w = rnorm(n)
  )
  # Model 8: moderator on both paths
  df$m <- df$m + 0.4 * df$x + 0.3 * df$x * df$w
  df$y <- df$y + 0.3 * df$m + 0.2 * df$m * df$w

  result <- moderated_mediation(
    data = df,
    x = "x",
    m = "m",
    y = "y",
    moderator = "w",
    model = "8",
    boot_samples = 1000,
    seed = 123
  )

  expect_s3_class(result, "moderated_mediation")
  expect_equal(result$model_type, "Hayes PROCESS Model 8")
})

test_that("moderated_mediation works with Model 14", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    w = rnorm(n)
  )
  # Model 14: moderator on a path only
  df$m <- df$m + 0.5 * df$x + 0.3 * df$x * df$w
  df$y <- df$y + 0.4 * df$m

  result <- moderated_mediation(
    data = df,
    x = "x",
    m = "m",
    y = "y",
    moderator = "w",
    model = "14",
    boot_samples = 1000,
    seed = 123
  )

  expect_s3_class(result, "moderated_mediation")
  expect_equal(result$model_type, "Hayes PROCESS Model 14")
})

test_that("moderated_mediation validates inputs", {
  df <- data.frame(x = rnorm(50), m = rnorm(50), y = rnorm(50), w = rnorm(50))

  expect_error(
    moderated_mediation(data = "not a df", x = "x", m = "m", y = "y", moderator = "w"),
    "must be a data frame"
  )

  expect_error(
    moderated_mediation(data = df, x = "missing", m = "m", y = "y", moderator = "w"),
    "not found"
  )

  expect_error(
    moderated_mediation(data = df, x = "x", m = "m", y = "y", moderator = "w", boot_samples = 100),
    "at least 1000"
  )
})

test_that("moderated_mediation calculates index of moderated mediation", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    w = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$m + 0.5 * df$w + 0.4 * df$m * df$w

  result <- moderated_mediation(
    df, x = "x", m = "m", y = "y", moderator = "w",
    model = "7", boot_samples = 1000, seed = 42
  )

  # Index should be calculated
  expect_s3_class(result$index_modmed, "tbl_df")
  expect_true("index" %in% names(result$index_modmed))
  expect_true("ci_lower" %in% names(result$index_modmed))
  expect_true("ci_upper" %in% names(result$index_modmed))
  expect_true("significant" %in% names(result$index_modmed))

  # Index should be numeric
  expect_type(result$index_modmed$index, "double")
})

test_that("moderated_mediation provides conditional indirect effects", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    w = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$m + 0.3 * df$w + 0.3 * df$m * df$w

  result <- moderated_mediation(
    df, x = "x", m = "m", y = "y", moderator = "w",
    model = "7", boot_samples = 1000, seed = 42
  )

  cond_ind <- result$conditional_indirect

  # Should have 3 rows (low, mean, high)
  expect_equal(nrow(cond_ind), 3)

  # Should have required columns
  expect_true(all(c("moderator_value", "moderator_label", "indirect_effect",
                   "ci_lower", "ci_upper", "significant") %in% names(cond_ind)))

  # Labels should be present
  expect_true(any(grepl("Low", cond_ind$moderator_label)))
  expect_true(any(grepl("Mean", cond_ind$moderator_label)))
  expect_true(any(grepl("High", cond_ind$moderator_label)))
})

test_that("moderated_mediation handles custom moderator values", {
  set.seed(42)
  n <- 100
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    w = rnorm(n)
  )
  df$m <- df$m + 0.4 * df$x
  df$y <- df$y + 0.3 * df$m + 0.2 * df$m * df$w

  custom_vals <- c(-2, 0, 2)

  result <- moderated_mediation(
    df, x = "x", m = "m", y = "y", moderator = "w",
    model = "7", moderator_values = custom_vals,
    boot_samples = 1000, seed = 42
  )

  expect_equal(nrow(result$conditional_indirect), 3)
  expect_equal(result$conditional_indirect$moderator_value, custom_vals)
})

test_that("moderated_mediation handles missing data", {
  set.seed(42)
  df <- data.frame(
    x = c(rnorm(95), rep(NA, 5)),
    m = rnorm(100),
    y = rnorm(100),
    w = rnorm(100)
  )

  expect_message(
    result <- moderated_mediation(
      df, x = "x", m = "m", y = "y", moderator = "w",
      model = "7", boot_samples = 1000, seed = 42
    ),
    "Removed.*missing"
  )

  expect_s3_class(result, "moderated_mediation")
})

test_that("moderated_mediation supports different bootstrap methods", {
  set.seed(42)
  n <- 100
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    w = rnorm(n)
  )
  df$m <- df$m + 0.4 * df$x
  df$y <- df$y + 0.3 * df$m + 0.2 * df$m * df$w

  result_bca <- moderated_mediation(
    df, x = "x", m = "m", y = "y", moderator = "w",
    model = "7", boot_method = "bca", boot_samples = 1000, seed = 42
  )

  result_perc <- moderated_mediation(
    df, x = "x", m = "m", y = "y", moderator = "w",
    model = "7", boot_method = "perc", boot_samples = 1000, seed = 42
  )

  expect_equal(result_bca$boot_method, "bca")
  expect_equal(result_perc$boot_method, "perc")
})

test_that("moderated_mediation print method works", {
  set.seed(42)
  n <- 100
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    w = rnorm(n)
  )
  df$m <- df$m + 0.4 * df$x
  df$y <- df$y + 0.3 * df$m + 0.2 * df$m * df$w

  result <- moderated_mediation(
    df, x = "x", m = "m", y = "y", moderator = "w",
    model = "7", boot_samples = 1000, seed = 42
  )

  output <- capture.output(print(result))

  expect_true(any(grepl("Moderated Mediation", output)))
  expect_true(any(grepl("Hayes PROCESS Model", output)))
  expect_true(any(grepl("Index of Moderated Mediation", output)))
  expect_true(any(grepl("Conditional Indirect Effects", output)))
})

test_that("moderated_mediation interpretation mentions index significance", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    w = rnorm(n)
  )
  # Strong moderation
  df$m <- df$m + 0.6 * df$x
  df$y <- df$y + 0.4 * df$m + 0.5 * df$m * df$w

  result <- moderated_mediation(
    df, x = "x", m = "m", y = "y", moderator = "w",
    model = "7", boot_samples = 1000, seed = 42
  )

  expect_type(result$interpretation, "character")
  expect_true(grepl("Index of moderated mediation|Moderated mediation", result$interpretation))
})

test_that("moderated_mediation works with covariates", {
  set.seed(42)
  n <- 120
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    w = rnorm(n),
    cov1 = rnorm(n),
    cov2 = rnorm(n)
  )
  df$m <- df$m + 0.4 * df$x + 0.1 * df$cov1
  df$y <- df$y + 0.3 * df$m + 0.2 * df$m * df$w + 0.1 * df$cov2

  result <- moderated_mediation(
    df, x = "x", m = "m", y = "y", moderator = "w",
    covariates = c("cov1", "cov2"),
    model = "7", boot_samples = 1000, seed = 42
  )

  expect_s3_class(result, "moderated_mediation")
})

test_that("moderated_mediation reproduces with same seed", {
  set.seed(42)
  n <- 100
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    w = rnorm(n)
  )
  df$m <- df$m + 0.4 * df$x
  df$y <- df$y + 0.3 * df$m + 0.2 * df$m * df$w

  result1 <- moderated_mediation(
    df, x = "x", m = "m", y = "y", moderator = "w",
    model = "7", boot_samples = 1000, seed = 123
  )

  result2 <- moderated_mediation(
    df, x = "x", m = "m", y = "y", moderator = "w",
    model = "7", boot_samples = 1000, seed = 123
  )

  expect_equal(result1$index_modmed$index, result2$index_modmed$index)
  expect_equal(result1$index_modmed$ci_lower, result2$index_modmed$ci_lower)
})
