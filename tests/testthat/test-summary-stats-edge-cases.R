# ==============================================================================
# LEVEL 1: SUMMARY STATISTICS EDGE CASE TESTS (Gold Standard)
# ==============================================================================
#
# This file comprehensively tests all edge cases for calculate_summary_stats():
#   - Normal numeric data
#   - Missing values (NA)
#   - NaN values
#   - Infinite values
#   - Single values
#   - Zero variance
#   - Very small samples
#   - Empty vectors
#   - Non-numeric data
#   - Parameter validation
#
# Tests ensure bulletproof operation and beginner-friendly error messages
# following APA 7 standards.
# ==============================================================================

library(testthat)

# Source files directly to test latest implementation
source(test_path("../../R/summary_stats.R"))
suppressPackageStartupMessages(library(rlang))

# ==============================================================================
# Basic Functionality Tests
# ==============================================================================

test_that("calculate_summary_stats() handles normal numeric data", {
  data <- c(45.2, 67.8, 23.4, 89.1, 34.5, 56.7, 78.9, 12.3)
  result <- calculate_summary_stats(data)

  expect_equal(result$n, 8)
  expect_equal(result$n_missing, 0)
  expect_true(is.numeric(result$mean))
  expect_true(is.numeric(result$median))
  expect_true(is.numeric(result$sd))
  expect_true(is.numeric(result$min))
  expect_true(is.numeric(result$max))
  expect_true(is.numeric(result$q25))
  expect_true(is.numeric(result$q75))
})

test_that("calculate_summary_stats() computes correct values", {
  data <- c(1, 2, 3, 4, 5)
  result <- calculate_summary_stats(data)

  expect_equal(result$n, 5)
  expect_equal(result$mean, 3)
  expect_equal(result$median, 3)
  expect_equal(result$min, 1)
  expect_equal(result$max, 5)
})

test_that("calculate_summary_stats() includes additional stats when requested", {
  data <- c(1, 2, 3, 4, 5)
  result <- calculate_summary_stats(data, include_all = TRUE)

  expect_true("variance" %in% names(result))
  expect_true("range" %in% names(result))
  expect_true("iqr" %in% names(result))
  expect_equal(result$range, 4)  # max - min = 5 - 1
})

test_that("calculate_summary_stats() excludes additional stats when not requested", {
  data <- c(1, 2, 3, 4, 5)
  result <- calculate_summary_stats(data, include_all = FALSE)

  expect_false("variance" %in% names(result))
  expect_false("range" %in% names(result))
  expect_false("iqr" %in% names(result))
})

test_that("calculate_summary_stats() respects round_digits parameter", {
  data <- c(1.111, 2.222, 3.333)
  result <- calculate_summary_stats(data, round_digits = 1)

  # Check that results are rounded to 1 decimal place
  expect_equal(nchar(sub(".*\\.", "", as.character(result$mean))), 1)
})

test_that("calculate_summary_stats() returns object with correct class", {
  data <- c(1, 2, 3, 4, 5)
  result <- calculate_summary_stats(data)

  expect_true("summary_stats" %in% class(result))
  expect_type(result, "list")
})


# ==============================================================================
# Missing Values (NA) Tests
# ==============================================================================

test_that("calculate_summary_stats() handles NA values correctly", {
  data <- c(7, 8, NA, 6, 9, 7, NA, 8, 7)
  result <- calculate_summary_stats(data)

  expect_equal(result$n, 7)  # 9 - 2 NA
  expect_equal(result$n_missing, 2)
})

test_that("calculate_summary_stats() warns about moderate missing data (>20%)", {
  data <- c(1, 2, NA, 4, NA, 6)  # 33% missing
  expect_warning(calculate_summary_stats(data), "Substantial missing data")
})

test_that("calculate_summary_stats() warns about high missing data (>50%)", {
  data <- c(1, NA, NA, NA, NA)  # 80% missing
  expect_warning(calculate_summary_stats(data), "More than half of data is missing")
})

test_that("calculate_summary_stats() informs about low missing data (<20%)", {
  data <- c(1, 2, 3, 4, NA, 6, 7, 8, 9, 10)  # 10% missing
  expect_message(calculate_summary_stats(data), "missing value")
})

test_that("calculate_summary_stats() errors on all-NA data", {
  data <- as.numeric(c(NA, NA, NA, NA))  # Must be numeric, not logical
  expect_error(calculate_summary_stats(data), "No valid")
})


# ==============================================================================
# NaN (Not a Number) Tests
# ==============================================================================

test_that("calculate_summary_stats() detects NaN values", {
  data <- c(1, 2, NaN, 4, 5)
  expect_warning(calculate_summary_stats(data), "NaN")
})

test_that("calculate_summary_stats() converts NaN to NA and proceeds", {
  data <- c(1, 2, NaN, 4, 5)
  result <- suppressWarnings(calculate_summary_stats(data))

  expect_equal(result$n, 4)  # 5 - 1 NaN (converted to NA)
  expect_equal(result$n_missing, 1)
})

test_that("calculate_summary_stats() handles multiple NaN values", {
  data <- c(1, NaN, NaN, 4, 5)
  result <- suppressWarnings(calculate_summary_stats(data))

  expect_equal(result$n, 3)
  expect_equal(result$n_missing, 2)
})


# ==============================================================================
# Infinite Values Tests
# ==============================================================================

test_that("calculate_summary_stats() detects Inf values", {
  data <- c(1, 2, Inf, 4, 5)
  expect_warning(calculate_summary_stats(data), "Infinite values detected")
})

test_that("calculate_summary_stats() detects -Inf values", {
  data <- c(1, 2, -Inf, 4, 5)
  expect_warning(calculate_summary_stats(data), "Infinite values detected")
})

test_that("calculate_summary_stats() converts Inf to NA and proceeds", {
  data <- c(1, 2, Inf, 4, -Inf)
  result <- suppressWarnings(calculate_summary_stats(data))

  expect_equal(result$n, 3)  # 5 - 2 Inf values
  expect_equal(result$n_missing, 2)
})

test_that("calculate_summary_stats() reports both +Inf and -Inf counts", {
  data <- c(1, Inf, -Inf, 4, 5)
  expect_warning(calculate_summary_stats(data), "Inf")
})


# ==============================================================================
# Single Value and Very Small Samples Tests
# ==============================================================================

test_that("calculate_summary_stats() warns about single value", {
  data <- c(5)
  expect_warning(calculate_summary_stats(data), "Only one valid data point")
})

test_that("calculate_summary_stats() handles single value correctly (SD = NA)", {
  data <- c(5)
  result <- suppressWarnings(calculate_summary_stats(data))

  expect_equal(result$n, 1)
  expect_equal(result$mean, 5)
  expect_equal(result$median, 5)
  expect_true(is.na(result$sd))  # Cannot calculate SD with n=1
  expect_true(is.na(result$variance))  # Cannot calculate variance with n=1
})

test_that("calculate_summary_stats() warns about very small samples (n < 5)", {
  data <- c(1, 2)
  expect_warning(calculate_summary_stats(data), "Very small sample")

  data <- c(1, 2, 3, 4)
  expect_warning(calculate_summary_stats(data), "Very small sample")
})

test_that("calculate_summary_stats() does NOT warn for n >= 5", {
  data <- c(1, 2, 3, 4, 5)
  expect_silent(calculate_summary_stats(data))
})


# ==============================================================================
# Zero Variance Tests
# ==============================================================================

test_that("calculate_summary_stats() warns about zero variance", {
  data <- c(5, 5, 5, 5)
  expect_warning(calculate_summary_stats(data), "Zero variance")
})

test_that("calculate_summary_stats() handles zero variance correctly", {
  data <- c(5, 5, 5, 5)
  result <- suppressWarnings(calculate_summary_stats(data))

  expect_equal(result$n, 4)
  expect_equal(result$mean, 5)
  expect_equal(result$sd, 0)
  expect_equal(result$variance, 0)
  expect_equal(result$range, 0)
  expect_equal(result$iqr, 0)
})


# ==============================================================================
# Empty Vector Tests
# ==============================================================================

test_that("calculate_summary_stats() errors on empty vector", {
  data <- numeric(0)
  expect_error(calculate_summary_stats(data), "Cannot calculate.*empty")
})

test_that("calculate_summary_stats() errors on empty vector (alternate)", {
  data <- numeric()  # Must be numeric() not c()
  expect_error(calculate_summary_stats(data), "Cannot calculate.*empty")
})


# ==============================================================================
# Non-Numeric Data Tests
# ==============================================================================

test_that("calculate_summary_stats() errors on character data", {
  data <- c("a", "b", "c")
  expect_error(calculate_summary_stats(data), "Data must be numeric")
})

test_that("calculate_summary_stats() errors on factor data", {
  data <- factor(c("low", "medium", "high"))
  expect_error(calculate_summary_stats(data), "Data must be numeric")
})

test_that("calculate_summary_stats() errors on logical data", {
  data <- c(TRUE, FALSE, TRUE)
  expect_error(calculate_summary_stats(data), "Data must be numeric")
})

test_that("calculate_summary_stats() errors on data frame", {
  data <- data.frame(x = 1:5)
  expect_error(calculate_summary_stats(data), "Data must be numeric")
})

test_that("calculate_summary_stats() error message includes helpful guidance", {
  data <- c("1", "2", "3")
  expect_error(
    calculate_summary_stats(data),
    "Convert with as.numeric"
  )
})


# ==============================================================================
# Parameter Validation Tests
# ==============================================================================

test_that("calculate_summary_stats() validates round_digits parameter", {
  data <- c(1, 2, 3, 4, 5)

  # Negative digits
  expect_error(calculate_summary_stats(data, round_digits = -1), "Invalid.*round_digits")

  # Too many digits
  expect_error(calculate_summary_stats(data, round_digits = 11), "Invalid.*round_digits")

  # Non-numeric
  expect_error(calculate_summary_stats(data, round_digits = "2"), "Invalid.*round_digits")

  # Multiple values
  expect_error(calculate_summary_stats(data, round_digits = c(2, 3)), "Invalid.*round_digits")
})

test_that("calculate_summary_stats() accepts valid round_digits", {
  data <- c(1, 2, 3, 4, 5)

  expect_silent(calculate_summary_stats(data, round_digits = 0))
  expect_silent(calculate_summary_stats(data, round_digits = 2))
  expect_silent(calculate_summary_stats(data, round_digits = 5))
  expect_silent(calculate_summary_stats(data, round_digits = 10))
})

test_that("calculate_summary_stats() validates include_all parameter", {
  data <- c(1, 2, 3, 4, 5)

  # Should work with TRUE/FALSE
  expect_silent(calculate_summary_stats(data, include_all = TRUE))
  expect_silent(calculate_summary_stats(data, include_all = FALSE))
})


# ==============================================================================
# APA 7 Compliance Tests
# ==============================================================================

test_that("calculate_summary_stats() follows APA 7 rounding recommendations", {
  data <- c(1.11111, 2.22222, 3.33333)
  result <- calculate_summary_stats(data, round_digits = 2)

  # Default should be 2 decimal places (APA 7 recommendation)
  expect_equal(result$mean, 2.22)  # Rounded to 2 decimals
})

test_that("calculate_summary_stats() includes n_missing for transparency", {
  data <- c(1, 2, NA, 4, 5)
  result <- suppressMessages(calculate_summary_stats(data))

  # APA 7: Report missing data
  expect_true("n_missing" %in% names(result))
  expect_equal(result$n_missing, 1)
})

test_that("calculate_summary_stats() calculates quartiles (Q1, Q3) correctly", {
  data <- 1:100
  result <- calculate_summary_stats(data)

  # APA 7 often reports quartiles
  expect_true("q25" %in% names(result))
  expect_true("q75" %in% names(result))
  expect_true(result$q25 < result$median)
  expect_true(result$q75 > result$median)
})


# ==============================================================================
# Edge Case Combinations
# ==============================================================================

test_that("calculate_summary_stats() handles NA + NaN + Inf combination", {
  data <- c(1, 2, NA, NaN, Inf, -Inf, 7, 8)

  result <- suppressWarnings(calculate_summary_stats(data))

  # Should have 4 valid values (1, 2, 7, 8)
  expect_equal(result$n, 4)
  # Should have 4 missing (1 NA + 1 NaN + 2 Inf)
  expect_equal(result$n_missing, 4)
})

test_that("calculate_summary_stats() handles single valid value after cleaning", {
  data <- c(5, NA, NaN, Inf, -Inf)

  result <- suppressWarnings(calculate_summary_stats(data))

  # Only one valid value
  expect_equal(result$n, 1)
  expect_equal(result$mean, 5)
  expect_true(is.na(result$sd))
})


# ==============================================================================
# INTEGRATION TESTS - Real-World Scenarios
# ==============================================================================

test_that("calculate_summary_stats() works with survey data", {
  # Simulating Likert scale data (1-7)
  satisfaction <- c(7, 6, 5, NA, 7, 6, 5, 4, 7, 6, NA, 5)
  result <- suppressMessages(calculate_summary_stats(satisfaction))

  expect_equal(result$n, 10)
  expect_equal(result$n_missing, 2)
  expect_true(result$mean >= 1 && result$mean <= 7)
})

test_that("calculate_summary_stats() works with spending data", {
  # Simulating consumer spending data (dollars)
  spending <- c(45.20, 67.80, 23.40, 89.10, 34.50, 56.70, 78.90, 12.30)
  result <- calculate_summary_stats(spending, round_digits = 2)

  expect_equal(result$n, 8)
  expect_true(is.numeric(result$mean))
  expect_true(result$sd > 0)  # Should have variance
})

test_that("calculate_summary_stats() works with age data", {
  # Simulating age data
  ages <- c(25, 30, 35, 40, 45, 50, 55, 60, NA)
  result <- suppressMessages(calculate_summary_stats(ages))

  expect_equal(result$n, 8)
  expect_equal(result$n_missing, 1)
  expect_true(result$min >= 0)  # Ages should be non-negative
})

test_that("calculate_summary_stats() works in typical data analysis pipeline", {
  # Simulate cleaning and analyzing data
  raw_data <- c(45.2, 67.8, 23.4, NA, 89.1, 34.5, 56.7, 78.9, 12.3)

  # Clean and analyze
  result <- suppressMessages(calculate_summary_stats(raw_data))

  # Should be able to use results in reporting
  expect_true(is.list(result))
  expect_true(all(c("n", "mean", "sd", "median") %in% names(result)))

  # Results should be ready for APA-style reporting
  expect_true(is.numeric(result$mean))
  expect_true(is.numeric(result$sd))
})
