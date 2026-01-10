test_that("simple_slopes works with continuous moderator", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  # Create interaction
  df$y <- df$y + 0.5 * df$x + 0.3 * df$w + 0.4 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)
  result <- simple_slopes(model, focal = "x", moderator = "w")

  expect_s3_class(result, "simple_slopes")
  expect_type(result, "list")
  expect_named(result, c("slopes", "mod_values", "focal", "moderator",
                         "conf_level", "interpretation"))

  # Should have 3 rows (low, mean, high)
  expect_equal(nrow(result$slopes), 3)
  expect_named(result$slopes, c("moderator_value", "moderator_label", "slope",
                                "std_error", "t_statistic", "p_value",
                                "ci_lower", "ci_upper", "significant"))

  # Interpretation should be text
  expect_type(result$interpretation, "character")
})

test_that("simple_slopes validates inputs", {
  df <- data.frame(x = rnorm(50), w = rnorm(50), y = rnorm(50))
  model <- stats::lm(y ~ x * w, data = df)
  lm_simple <- stats::lm(y ~ x + w, data = df)  # No interaction

  expect_error(simple_slopes("not a model", focal = "x", moderator = "w"), "must be an lm")
  expect_error(simple_slopes(model, focal = "missing", moderator = "w"), "not found")
  expect_error(simple_slopes(model, focal = "x", moderator = "missing"), "not found")
  expect_error(simple_slopes(lm_simple, focal = "x", moderator = "w"), "does not contain interaction")
  expect_error(simple_slopes(model, focal = "x", moderator = "w", conf_level = 0), "between 0 and 1")
})

test_that("simple_slopes detects significant slopes", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  # Strong interaction
  df$y <- df$y + 0.3 * df$x + 0.2 * df$w + 0.6 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)
  result <- simple_slopes(model, focal = "x", moderator = "w")

  # Should have significant slopes
  expect_true(any(result$slopes$significant))
  expect_true(all(result$slopes$p_value >= 0 & result$slopes$p_value <= 1))
})

test_that("simple_slopes handles custom moderator values", {
  set.seed(42)
  n <- 120
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.4 * df$x + 0.3 * df$w + 0.5 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)

  # Custom moderator values
  result <- simple_slopes(model, focal = "x", moderator = "w",
                         mod_values = c(-2, 0, 2))

  expect_equal(nrow(result$slopes), 3)
  expect_equal(result$slopes$moderator_value, c(-2, 0, 2))
})

test_that("simple_slopes works with different variable orders in interaction", {
  set.seed(42)
  n <- 100
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.3 * df$x + 0.2 * df$w + 0.4 * df$x * df$w

  # Try both orders
  model1 <- stats::lm(y ~ x * w, data = df)
  model2 <- stats::lm(y ~ w * x, data = df)

  result1 <- simple_slopes(model1, focal = "x", moderator = "w")
  result2 <- simple_slopes(model2, focal = "x", moderator = "w")

  # Should get same slopes (approximately)
  expect_equal(result1$slopes$slope, result2$slopes$slope, tolerance = 0.01)
})

test_that("simple_slopes print method works", {
  set.seed(42)
  n <- 100
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.3 * df$x + 0.2 * df$w + 0.4 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)
  result <- simple_slopes(model, focal = "x", moderator = "w")

  output <- capture.output(print(result))

  expect_true(any(grepl("Simple Slopes", output)))
  expect_true(any(grepl("Focal predictor", output)))
  expect_true(any(grepl("Moderator", output)))
  expect_true(any(grepl("Interpretation", output)))
})

test_that("simple_slopes confidence intervals have correct width", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.3 * df$x + 0.2 * df$w + 0.4 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)

  result_95 <- simple_slopes(model, focal = "x", moderator = "w", conf_level = 0.95)
  result_99 <- simple_slopes(model, focal = "x", moderator = "w", conf_level = 0.99)

  # 99% CI should be wider
  width_95 <- result_95$slopes$ci_upper[1] - result_95$slopes$ci_lower[1]
  width_99 <- result_99$slopes$ci_upper[1] - result_99$slopes$ci_lower[1]

  expect_gt(width_99, width_95)
})

test_that("johnson_neyman works with continuous moderator", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n, 5, 2),
    y = rnorm(n)
  )
  # Create interaction where effect changes from non-sig to sig
  df$y <- df$y - 0.2 * df$x + 0.3 * df$w + 0.15 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)
  result <- johnson_neyman(model, focal = "x", moderator = "w")

  expect_s3_class(result, "johnson_neyman")
  expect_type(result, "list")
  expect_named(result, c("critical_values", "regions", "slopes_at_points",
                         "focal", "moderator", "alpha", "mod_range",
                         "interpretation"))

  # Should have regions
  expect_s3_class(result$regions, "tbl_df")
  expect_true(nrow(result$regions) >= 1)

  # Slopes at points should be detailed
  expect_s3_class(result$slopes_at_points, "tbl_df")
  expect_true(nrow(result$slopes_at_points) > 0)
})

test_that("johnson_neyman validates inputs", {
  df <- data.frame(x = rnorm(50), w = rnorm(50), y = rnorm(50))
  model <- stats::lm(y ~ x * w, data = df)
  lm_simple <- stats::lm(y ~ x + w, data = df)

  expect_error(johnson_neyman("not a model", focal = "x", moderator = "w"), "must be an lm")
  expect_error(johnson_neyman(lm_simple, focal = "x", moderator = "w"), "does not contain interaction")
})

test_that("johnson_neyman rejects categorical moderator", {
  set.seed(42)
  df <- data.frame(
    x = rnorm(100),
    w = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    y = rnorm(100)
  )
  df$y <- as.numeric(df$y) + 0.3 * df$x + 0.5 * as.numeric(df$w)

  model <- stats::lm(y ~ x * w, data = df)

  expect_error(
    johnson_neyman(model, focal = "x", moderator = "w"),
    "requires continuous moderator"
  )
})

test_that("johnson_neyman finds transition points", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n, 5, 2),
    y = rnorm(n)
  )
  # Create interaction with clear transition
  df$y <- df$y - 0.5 * df$x + 0.3 * df$w + 0.2 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)
  result <- johnson_neyman(model, focal = "x", moderator = "w")

  # Should have regions defined
  expect_true(nrow(result$regions) > 0)
  expect_true("effect_status" %in% names(result$regions))
})

test_that("johnson_neyman handles always significant effect", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  # Very strong main effect, weak interaction
  df$y <- df$y + 2.0 * df$x + 0.3 * df$w + 0.05 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)
  result <- johnson_neyman(model, focal = "x", moderator = "w")

  # Should report effect as always significant
  expect_true(any(grepl("significant", result$interpretation, ignore.case = TRUE)))
})

test_that("johnson_neyman handles never significant effect", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  # Very weak effects
  df$y <- df$y + 0.01 * df$x + 0.01 * df$w + 0.005 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)
  result <- johnson_neyman(model, focal = "x", moderator = "w")

  # Should report effect as not significant
  expect_true(nrow(result$regions) >= 1)
})

test_that("johnson_neyman print method works", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.3 * df$x + 0.2 * df$w + 0.4 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)
  result <- johnson_neyman(model, focal = "x", moderator = "w")

  output <- capture.output(print(result))

  expect_true(any(grepl("Johnson-Neyman", output)))
  expect_true(any(grepl("Focal predictor", output)))
  expect_true(any(grepl("Regions of Significance", output)))
  expect_true(any(grepl("Interpretation", output)))
})

test_that("johnson_neyman uses custom moderator range", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n, 10, 3),
    y = rnorm(n)
  )
  df$y <- df$y + 0.3 * df$x + 0.2 * df$w + 0.15 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)

  # Custom range
  result <- johnson_neyman(model, focal = "x", moderator = "w",
                          mod_range = c(5, 15))

  expect_equal(result$mod_range, c(5, 15))
  expect_true(all(result$slopes_at_points$moderator_value >= 5))
  expect_true(all(result$slopes_at_points$moderator_value <= 15))
})

test_that("johnson_neyman has correct number of evaluation points", {
  set.seed(42)
  n <- 120
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.3 * df$x + 0.2 * df$w + 0.4 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)
  result <- johnson_neyman(model, focal = "x", moderator = "w", n_points = 500)

  expect_equal(nrow(result$slopes_at_points), 500)
})

test_that("simple_slopes and johnson_neyman are complementary", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.3 * df$x + 0.2 * df$w + 0.4 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)

  ss <- simple_slopes(model, focal = "x", moderator = "w")
  jn <- johnson_neyman(model, focal = "x", moderator = "w")

  # Both should analyze same interaction
  expect_equal(ss$focal, jn$focal)
  expect_equal(ss$moderator, jn$moderator)

  # Both should provide interpretations
  expect_type(ss$interpretation, "character")
  expect_type(jn$interpretation, "character")
})

test_that("interaction probing handles three-way interactions gracefully", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    z = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.2 * df$x + 0.2 * df$w + 0.2 * df$z +
    0.3 * df$x * df$w + 0.1 * df$x * df$w * df$z

  model <- stats::lm(y ~ x * w * z, data = df)

  # Should still work for two-way interaction
  result <- simple_slopes(model, focal = "x", moderator = "w")
  expect_s3_class(result, "simple_slopes")
})

test_that("simple_slopes interpretation mentions significance pattern", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.5 * df$x + 0.3 * df$w + 0.6 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)
  result <- simple_slopes(model, focal = "x", moderator = "w")

  # Interpretation should describe pattern
  expect_true(grepl("effect|slope|significant", result$interpretation, ignore.case = TRUE))
})

test_that("johnson_neyman interpretation describes regions clearly", {
  set.seed(42)
  n <- 200
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n, 5, 2),
    y = rnorm(n)
  )
  df$y <- df$y - 0.3 * df$x + 0.2 * df$w + 0.15 * df$x * df$w

  model <- stats::lm(y ~ x * w, data = df)
  result <- johnson_neyman(model, focal = "x", moderator = "w")

  # Interpretation should describe regions
  expect_type(result$interpretation, "character")
  expect_true(nchar(result$interpretation) > 20)
})

test_that("interaction functions handle models with covariates", {
  set.seed(42)
  n <- 150
  df <- data.frame(
    x = rnorm(n),
    w = rnorm(n),
    cov1 = rnorm(n),
    cov2 = rnorm(n),
    y = rnorm(n)
  )
  df$y <- df$y + 0.3 * df$x + 0.2 * df$w + 0.4 * df$x * df$w +
    0.1 * df$cov1 + 0.1 * df$cov2

  model <- stats::lm(y ~ x * w + cov1 + cov2, data = df)

  # Should work with covariates
  ss <- simple_slopes(model, focal = "x", moderator = "w")
  jn <- johnson_neyman(model, focal = "x", moderator = "w")

  expect_s3_class(ss, "simple_slopes")
  expect_s3_class(jn, "johnson_neyman")
})
