# Tests for calculate_summary_stats function
# These tests ensure the function works correctly and handles edge cases

test_that("calculate_summary_stats returns correct structure", {
  # Test that output has all expected components (standardized structure)
  data <- c(1, 2, 3, 4, 5)
  result <- calculate_summary_stats(data)

  expect_type(result, "list")
  expect_s3_class(result, "summary_stats_result")
  expect_s3_class(result, "analysis_result")

  # Should have core standardized fields
  expect_true("test_type" %in% names(result))
  expect_true("test_name" %in% names(result))
  expect_true("n" %in% names(result))

  # Should have summary stats fields
  expect_true("mean" %in% names(result))
  expect_true("median" %in% names(result))
  expect_true("sd" %in% names(result))
  expect_true("min" %in% names(result))
  expect_true("max" %in% names(result))
})

test_that("calculate_summary_stats calculates correct values", {
  # Test with known data
  data <- c(1, 2, 3, 4, 5)
  result <- calculate_summary_stats(data, round_digits = 2)

  expect_equal(result$n, 5)
  expect_equal(result$mean, 3)
  expect_equal(result$median, 3)
  expect_equal(result$min, 1)
  expect_equal(result$max, 5)
})

test_that("calculate_summary_stats handles missing values", {
  # Test that NAs are properly handled
  data <- c(1, 2, NA, 4, 5)

  # Refactored version may not produce message (handled by assert_beginner_safe)
  # Just test that it works correctly
  result <- calculate_summary_stats(data)

  # Should calculate on non-missing values only
  expect_equal(result$n, 4)
  expect_equal(result$n_missing, 1)
})

test_that("calculate_summary_stats validates input", {
  # Test that non-numeric input throws error
  # New error message is more beginner-friendly
  expect_error(
    calculate_summary_stats(c("a", "b", "c")),
    "must be numeric"
  )

  # Test that all-NA input throws error
  # New error message talks about data being "too short"
  expect_error(
    calculate_summary_stats(c(NA_real_, NA_real_, NA_real_)),
    "too short|need at least"
  )
})

test_that("calculate_summary_stats respects include_all parameter", {
  data <- c(1, 2, 3, 4, 5)

  # With include_all = TRUE (default)
  result_all <- calculate_summary_stats(data, include_all = TRUE)
  expect_true("variance" %in% names(result_all))
  expect_true("range" %in% names(result_all))
  expect_true("iqr" %in% names(result_all))

  # With include_all = FALSE
  result_basic <- calculate_summary_stats(data, include_all = FALSE)
  expect_false("variance" %in% names(result_basic))
  expect_false("range" %in% names(result_basic))
  expect_false("iqr" %in% names(result_basic))
})

test_that("calculate_summary_stats respects round_digits parameter", {
  data <- c(1.111, 2.222, 3.333)

  # Round to 1 digit
  result_1 <- calculate_summary_stats(data, round_digits = 1)
  expect_equal(result_1$mean, 2.2)

  # Round to 3 digits
  result_3 <- calculate_summary_stats(data, round_digits = 3)
  expect_equal(result_3$mean, 2.222)
})

test_that("calculate_summary_stats works with single observation", {
  # Edge case: only one data point
  # Note: SD will be NA for single observation
  data <- c(5)
  result <- calculate_summary_stats(data)

  expect_equal(result$n, 1)
  expect_equal(result$mean, 5)
  expect_equal(result$median, 5)
})

test_that("calculate_summary_stats handles negative values", {
  # Test with negative numbers
  data <- c(-5, -3, -1, 1, 3, 5)
  result <- calculate_summary_stats(data)

  expect_equal(result$mean, 0)
  expect_equal(result$min, -5)
  expect_equal(result$max, 5)
})

test_that("calculate_summary_stats handles large numbers", {
  # Test with large values
  data <- c(1e6, 2e6, 3e6, 4e6, 5e6)
  result <- calculate_summary_stats(data)

  expect_equal(result$n, 5)
  expect_equal(result$mean, 3e6)
})
