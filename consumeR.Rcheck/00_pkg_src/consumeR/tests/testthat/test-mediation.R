test_that("mediation_simple works with basic mediation", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  # Create mediation structure
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$x + 0.4 * df$m

  result <- mediation_simple(
    data = df,
    x = "x",
    m = "m",
    y = "y",
    boot_samples = 1000,
    seed = 123
  )

  expect_s3_class(result, "mediation_simple")
  expect_type(result, "list")
  expect_named(result, c("paths", "indirect_effect", "total_effect", "direct_effect",
                         "proportion_mediated", "mediation_type", "boot_samples",
                         "boot_distribution", "conf_level", "interpretation",
                         "n_obs", "variables"))

  # Check paths tibble
  expect_s3_class(result$paths, "tbl_df")
  expect_equal(nrow(result$paths), 4)  # a, b, c, c'
  expect_named(result$paths, c("path", "label", "estimate", "std_error",
                               "p_value", "significant"))

  # Check indirect effect tibble
  expect_s3_class(result$indirect_effect, "tbl_df")
  expect_equal(nrow(result$indirect_effect), 1)
  expect_true("estimate" %in% names(result$indirect_effect))
  expect_true("ci_lower" %in% names(result$indirect_effect))
  expect_true("ci_upper" %in% names(result$indirect_effect))

  # Check bootstrap distribution
  expect_equal(length(result$boot_distribution), 1000)
})

test_that("mediation_simple detects full mediation", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  # Create full mediation: strong indirect, no direct
  df$m <- df$m + 0.7 * df$x
  df$y <- df$y + 0.8 * df$m  # No direct effect of x

  result <- mediation_simple(
    data = df,
    x = "x",
    m = "m",
    y = "y",
    boot_samples = 1000,
    seed = 42
  )

  # Should detect mediation
  expect_true(result$indirect_effect$significant)

  # Check that interpretation mentions mediation
  expect_true(grepl("mediation", result$mediation_type, ignore.case = TRUE))
})

test_that("mediation_simple detects partial mediation", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  # Create partial mediation: both indirect and direct effects
  df$m <- df$m + 0.6 * df$x
  df$y <- df$y + 0.4 * df$x + 0.5 * df$m

  result <- mediation_simple(
    data = df,
    x = "x",
    m = "m",
    y = "y",
    boot_samples = 1000,
    seed = 42
  )

  # Both indirect and direct should be significant
  expect_true(result$indirect_effect$significant)
  path_c_prime <- result$paths$p_value[result$paths$label == "c'"]
  expect_lt(path_c_prime, 0.05)
})

test_that("mediation_simple detects no mediation", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  # No mediation: x affects y directly, m is independent
  df$y <- df$y + 0.5 * df$x

  result <- mediation_simple(
    data = df,
    x = "x",
    m = "m",
    y = "y",
    boot_samples = 1000,
    seed = 42
  )

  # Indirect effect should not be significant
  expect_false(result$indirect_effect$significant)
  expect_true(grepl("No mediation", result$mediation_type))
})

test_that("mediation_simple works with covariates", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n),
    cov1 = rnorm(n),
    cov2 = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x + 0.2 * df$cov1
  df$y <- df$y + 0.3 * df$x + 0.4 * df$m + 0.1 * df$cov2

  result <- mediation_simple(
    data = df,
    x = "x",
    m = "m",
    y = "y",
    covariates = c("cov1", "cov2"),
    boot_samples = 1000,
    seed = 42
  )

  expect_s3_class(result, "mediation_simple")
  expect_equal(result$variables$covariates, c("cov1", "cov2"))
})

test_that("mediation_simple validates inputs", {
  df <- data.frame(x = rnorm(50), m = rnorm(50), y = rnorm(50))

  expect_error(
    mediation_simple(data = "not a df", x = "x", m = "m", y = "y"),
    "must be a data frame"
  )

  expect_error(
    mediation_simple(data = df, x = "missing", m = "m", y = "y"),
    "not found"
  )

  expect_error(
    mediation_simple(data = df, x = "x", m = "m", y = "y", conf_level = 0),
    "between 0 and 1"
  )

  expect_error(
    mediation_simple(data = df, x = "x", m = "m", y = "y", boot_samples = 100),
    "at least 1000"
  )
})

test_that("mediation_simple handles different bootstrap methods", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$x + 0.4 * df$m

  result_bca <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, boot_method = "bca", seed = 42
  )

  result_perc <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, boot_method = "perc", seed = 42
  )

  result_norm <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, boot_method = "norm", seed = 42
  )

  # All should complete successfully
  expect_equal(result_bca$indirect_effect$ci_method, "bca")
  expect_equal(result_perc$indirect_effect$ci_method, "perc")
  expect_equal(result_norm$indirect_effect$ci_method, "norm")

  # Point estimates should be the same (or very similar)
  expect_equal(
    result_bca$indirect_effect$estimate,
    result_perc$indirect_effect$estimate,
    tolerance = 0.001
  )
})

test_that("mediation_simple confidence intervals have correct properties", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$x + 0.4 * df$m

  result_95 <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, conf_level = 0.95, seed = 42
  )

  result_99 <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, conf_level = 0.99, seed = 42
  )

  # 99% CI should be wider
  width_95 <- result_95$indirect_effect$ci_upper - result_95$indirect_effect$ci_lower
  width_99 <- result_99$indirect_effect$ci_upper - result_99$indirect_effect$ci_lower

  expect_gt(width_99, width_95)
})

test_that("mediation_simple calculates proportion mediated correctly", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  # Create strong mediation
  df$m <- df$m + 0.7 * df$x
  df$y <- df$y + 0.3 * df$x + 0.6 * df$m

  result <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, seed = 42
  )

  # Proportion mediated should be between 0 and 1 for partial mediation
  expect_true(!is.na(result$proportion_mediated))
  expect_type(result$proportion_mediated, "double")

  # For partial mediation with positive effects, PM should be > 0
  if (result$total_effect > 0 && result$indirect_effect$estimate > 0) {
    expect_gt(result$proportion_mediated, 0)
  }
})

test_that("mediation_simple handles missing data", {
  set.seed(42)
  n <- 100
  df <- data.frame(
    x = c(rnorm(95), rep(NA, 5)),
    m = c(rnorm(97), rep(NA, 3)),
    y = rnorm(100)
  )

  # Should remove missing and warn
  expect_message(
    result <- mediation_simple(
      data = df, x = "x", m = "m", y = "y",
      boot_samples = 1000, seed = 42
    ),
    "Removed.*missing"
  )

  # Should use complete cases
  expect_lt(result$n_obs, 100)
})

test_that("mediation_simple warns about small samples", {
  set.seed(42)
  df <- data.frame(
    x = rnorm(25),
    m = rnorm(25),
    y = rnorm(25)
  )

  expect_warning(
    mediation_simple(data = df, x = "x", m = "m", y = "y", boot_samples = 1000, seed = 42),
    "Small sample size"
  )
})

test_that("mediation_simple print method works", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$x + 0.4 * df$m

  result <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, seed = 42
  )

  output <- capture.output(print(result))

  expect_true(any(grepl("Mediation Analysis", output)))
  expect_true(any(grepl("Path Coefficients", output)))
  expect_true(any(grepl("Indirect Effect", output)))
  expect_true(any(grepl("Sample size", output)))
})

test_that("mediation_simple summary method works", {
  set.seed(42)
  df <- data.frame(
    x = rnorm(100),
    m = rnorm(100),
    y = rnorm(100)
  )

  result <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, seed = 42
  )

  # Summary should call print
  output <- capture.output(summary(result))
  expect_true(any(grepl("Mediation", output)))
})

test_that("mediation_simple bootstrap distribution is valid", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$x + 0.4 * df$m

  result <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, seed = 42
  )

  # Bootstrap distribution should have requested length
  expect_equal(length(result$boot_distribution), 1000)

  # Should be numeric
  expect_type(result$boot_distribution, "double")

  # Should not have all identical values
  expect_gt(stats::sd(result$boot_distribution), 0)
})

test_that("mediation_simple interpretation includes key information", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$x + 0.4 * df$m

  result <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, seed = 42
  )

  # Interpretation should mention key components
  expect_type(result$interpretation, "character")
  expect_true(grepl("Indirect effect", result$interpretation))
  expect_true(grepl("CI", result$interpretation))
  expect_true(grepl("Path a", result$interpretation))
  expect_true(grepl("Path b", result$interpretation))
})

test_that("mediation_simple reproduces results with same seed", {
  set.seed(42)
  n <- 120
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$x + 0.4 * df$m

  result1 <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, seed = 123
  )

  result2 <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, seed = 123
  )

  # Should get identical results
  expect_equal(result1$indirect_effect$estimate, result2$indirect_effect$estimate)
  expect_equal(result1$indirect_effect$ci_lower, result2$indirect_effect$ci_lower)
  expect_equal(result1$indirect_effect$ci_upper, result2$indirect_effect$ci_upper)
})

test_that("mediation_simple handles categorical independent variable", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = factor(sample(c("Control", "Treatment"), n, replace = TRUE)),
    m = rnorm(n),
    y = rnorm(n)
  )

  # Create mediation with categorical X
  df$m <- df$m + 0.5 * (as.numeric(df$x) - 1)
  df$y <- df$y + 0.3 * (as.numeric(df$x) - 1) + 0.4 * df$m

  # Should work with categorical predictor
  expect_no_error(
    result <- mediation_simple(
      data = df, x = "x", m = "m", y = "y",
      boot_samples = 1000, seed = 42
    )
  )
})

test_that("mediation_simple all paths are reported", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    m = rnorm(n),
    y = rnorm(n)
  )
  df$m <- df$m + 0.5 * df$x
  df$y <- df$y + 0.3 * df$x + 0.4 * df$m

  result <- mediation_simple(
    data = df, x = "x", m = "m", y = "y",
    boot_samples = 1000, seed = 42
  )

  # Check all paths are present
  path_labels <- result$paths$label
  expect_true("a" %in% path_labels)
  expect_true("b" %in% path_labels)
  expect_true("c" %in% path_labels)
  expect_true("c'" %in% path_labels)

  # All paths should have estimates and SEs
  expect_true(all(!is.na(result$paths$estimate)))
  expect_true(all(!is.na(result$paths$std_error)))
  expect_true(all(result$paths$std_error > 0))
})
