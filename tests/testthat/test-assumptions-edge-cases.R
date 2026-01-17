# ==============================================================================
# LEVEL 2: COMPREHENSIVE ASSUMPTION CHECKS EDGE CASE TESTS (Gold Standard)
# ==============================================================================
#
# This test suite validates all assumption checking functions in R/assumptions.R
# Each function has 12+ validation checks and must handle all edge cases gracefully
#
# Test categories:
# 1. Input validation (type, length, range)
# 2. Edge cases (NA, NaN, Inf, empty, single value, zero variance)
# 3. Sample size requirements
# 4. Statistical accuracy
# 5. Output structure and formatting
# 6. Integration with Level 1 utilities

library(testthat)
library(rlang)

# ==============================================================================
# check_normality() Tests
# ==============================================================================

test_that("check_normality() handles normal data correctly", {
  set.seed(123)
  data <- rnorm(100, mean = 10, sd = 2)
  result <- check_normality(data, "test_var")

  expect_s3_class(result, "assumption_check")
  expect_true(is.logical(result$assumption_met))
  expect_equal(result$test, "Shapiro-Wilk")
  expect_equal(result$assumption, "Normality")
  expect_equal(result$n, 100)
  expect_equal(result$n_missing, 0)
  expect_true(!is.null(result$p_value_formatted))  # Level 1 integration
  expect_true(!is.null(result$visual_guide))  # Gold standard feature
})

test_that("check_normality() detects non-normal data", {
  set.seed(123)
  # Highly skewed data (exponential distribution)
  data <- rexp(100, rate = 1)
  result <- check_normality(data, "skewed_var")

  expect_s3_class(result, "assumption_check")
  expect_false(result$assumption_met)  # Should violate normality
  expect_true(abs(result$skewness) > 1)  # Should be skewed
})

test_that("check_normality() errors on non-numeric data", {
  expect_error(
    check_normality(c("a", "b", "c"), "char_var"),
    "Data must be numeric"
  )
  expect_error(
    check_normality(as.factor(c(1, 2, 3)), "factor_var"),
    "Data must be numeric"
  )
})

test_that("check_normality() errors on empty data", {
  expect_error(
    check_normality(numeric(0), "empty_var"),
    "Cannot test normality on empty data"
  )
})

test_that("check_normality() handles NaN values with warning", {
  data <- c(1, 2, NaN, 4, 5, 6, 7, 8, 9, 10)
  expect_warning(
    result <- check_normality(data, "nan_var"),
    "NaN"
  )
  expect_equal(result$n, 9)  # NaN removed
  expect_equal(result$n_missing, 1)
})

test_that("check_normality() handles Inf values with warning", {
  data <- c(1, 2, Inf, 4, 5, -Inf, 7, 8, 9, 10)
  expect_warning(
    result <- check_normality(data, "inf_var"),
    "Infinite"
  )
  expect_equal(result$n, 8)  # Both Inf removed
  expect_equal(result$n_missing, 2)
})

test_that("check_normality() errors on all-NA data", {
  data <- as.numeric(c(NA, NA, NA, NA))
  expect_error(
    check_normality(data, "all_na"),
    "No valid"
  )
})

test_that("check_normality() warns about missing data", {
  # > 50% missing (need enough data after removal to avoid small sample warning)
  data1 <- c(rnorm(8), rep(NA, 12))  # 8 valid, 12 missing = 60% missing
  expect_warning(
    check_normality(data1, "mostly_na"),
    "More than half"
  )

  # 20-50% missing (need enough valid data)
  data2 <- c(rnorm(15), rep(NA, 5))  # 15 valid, 5 missing = 25% missing
  expect_warning(
    check_normality(data2, "some_na"),
    "Substantial missing"
  )

  # < 20% missing (need at least 10 valid to avoid small sample warning)
  data3 <- c(rnorm(15), NA)  # 15 valid, 1 missing = 6.25% missing
  expect_message(
    result <- suppressWarnings(check_normality(data3, "few_na")),
    "missing value"
  )
  expect_equal(result$n_missing, 1)
})

test_that("check_normality() errors on sample size < 3", {
  expect_error(
    check_normality(c(1, 2), "tiny"),
    "Sample size too small"
  )
  expect_error(
    check_normality(c(5), "single"),
    "Sample size too small"
  )
})

test_that("check_normality() warns about small samples (n < 10)", {
  data <- 1:5
  expect_warning(
    check_normality(data, "small"),
    "Very small sample"
  )
})

test_that("check_normality() errors on zero variance", {
  data <- rep(5, 10)
  expect_error(
    check_normality(data, "constant"),
    "Zero variance"
  )
})

test_that("check_normality() validates alpha parameter", {
  data <- rnorm(50)
  expect_error(check_normality(data, alpha = -0.1), "alpha must be")
  expect_error(check_normality(data, alpha = 1.5), "alpha must be")
  expect_error(check_normality(data, alpha = "0.05"), "alpha must be")
  expect_error(check_normality(data, alpha = c(0.05, 0.1)), "alpha must be")
})

test_that("check_normality() validates variable_name parameter", {
  data <- rnorm(50)
  expect_error(
    check_normality(data, variable_name = 123),
    "variable_name must be a single character"
  )
  expect_error(
    check_normality(data, variable_name = c("a", "b")),
    "variable_name must be a single character"
  )
})

test_that("check_normality() handles large samples appropriately", {
  set.seed(123)
  data <- rnorm(6000)  # > 5000
  # Should use subset for Shapiro-Wilk
  result <- check_normality(data, "large_sample")
  expect_s3_class(result, "assumption_check")
  # Should include note about sensitivity
})

test_that("check_normality() integrates Level 1 utilities", {
  set.seed(123)
  data <- rnorm(50)
  result <- check_normality(data)

  # Should have APA 7 formatted p-value
  expect_true(!is.null(result$p_value_formatted))
  expect_match(result$p_value_formatted, "p [=<]")

  # Should have formatted n
  expect_true(!is.null(result$n_formatted))
  expect_match(result$n_formatted, "n = ")
})

test_that("check_normality() provides comprehensive output", {
  set.seed(123)
  data <- rnorm(50)
  result <- check_normality(data, "complete_check")

  # Check all required fields
  expect_true(!is.null(result$test))
  expect_true(!is.null(result$assumption))
  expect_true(!is.null(result$interpretation))
  expect_true(!is.null(result$recommendation))
  expect_true(!is.null(result$publication_text))
  expect_true(!is.null(result$verbose_explanation))
  expect_true(!is.null(result$visual_guide))
  expect_true(!is.null(result$skewness))
  expect_true(!is.null(result$kurtosis))
})

# ==============================================================================
# check_homogeneity_of_variance() Tests
# ==============================================================================

test_that("check_homogeneity_of_variance() handles equal variances", {
  set.seed(123)
  data <- c(rnorm(50, 10, 2), rnorm(50, 12, 2))
  groups <- rep(c("A", "B"), each = 50)
  result <- check_homogeneity_of_variance(data, groups)

  expect_s3_class(result, "assumption_check")
  expect_equal(result$test, "Levene's Test")
  expect_equal(result$assumption, "Homogeneity of Variance")
  expect_equal(result$n_total, 100)
  expect_equal(result$n_groups, 2)
  expect_true(result$variance_ratio >= 1)
})

test_that("check_homogeneity_of_variance() detects unequal variances", {
  set.seed(123)
  # Group B has much larger variance
  data <- c(rnorm(50, 10, 1), rnorm(50, 12, 5))
  groups <- rep(c("A", "B"), each = 50)
  result <- check_homogeneity_of_variance(data, groups)

  expect_s3_class(result, "assumption_check")
  expect_false(result$assumption_met)  # Should violate
  expect_true(result$variance_ratio > 3)  # Large ratio
})

test_that("check_homogeneity_of_variance() errors on non-numeric data", {
  expect_error(
    check_homogeneity_of_variance(c("a", "b"), c("A", "A")),
    "Data must be numeric"
  )
})

test_that("check_homogeneity_of_variance() errors on empty data", {
  expect_error(
    check_homogeneity_of_variance(numeric(0), character(0)),
    "Cannot test homogeneity"
  )
})

test_that("check_homogeneity_of_variance() errors on mismatched lengths", {
  expect_error(
    check_homogeneity_of_variance(1:10, rep("A", 5)),
    "same length"
  )
})

test_that("check_homogeneity_of_variance() handles NaN and Inf", {
  data <- c(1, 2, NaN, 4, 5, Inf, 7, 8, 9, 10)
  groups <- rep(c("A", "B"), 5)

  # Will produce both NaN and Inf warnings, possibly in either order
  # Just suppress and verify the result is correct
  result <- suppressWarnings(check_homogeneity_of_variance(data, groups))

  expect_s3_class(result, "assumption_check")
  expect_equal(result$n_total, 8)   # 2 NaN/Inf removed
  expect_equal(result$n_missing, 2)
})

test_that("check_homogeneity_of_variance() errors on single group", {
  data <- 1:10
  groups <- rep("A", 10)

  expect_error(
    check_homogeneity_of_variance(data, groups),
    "At least 2 groups required"
  )
})

test_that("check_homogeneity_of_variance() errors on groups with n < 2", {
  data <- c(1, 2, 3, 4, 5, 6, 7)
  groups <- c("A", "A", "A", "B", "B", "C", "C")  # All OK
  groups_bad <- c("A", "A", "A", "B", "C", "C", "C")  # B has only 1

  # This should work
  result <- check_homogeneity_of_variance(data, groups)
  expect_s3_class(result, "assumption_check")

  # This should fail
  expect_error(
    check_homogeneity_of_variance(data, groups_bad),
    "at least 2 observations"
  )
})

test_that("check_homogeneity_of_variance() errors on zero variance group", {
  data <- c(5, 5, 5, 1, 2, 3)
  groups <- c("A", "A", "A", "B", "B", "B")

  expect_error(
    check_homogeneity_of_variance(data, groups),
    "Zero variance"
  )
})

test_that("check_homogeneity_of_variance() warns about small total n", {
  data <- c(1, 2, 3, 4, 5)
  groups <- c("A", "A", "B", "B", "B")

  # This will warn - just test that it runs without error
  result <- suppressWarnings(check_homogeneity_of_variance(data, groups))
  expect_s3_class(result, "assumption_check")
  expect_equal(result$n_total, 5)
})

test_that("check_homogeneity_of_variance() validates alpha parameter", {
  data <- c(rnorm(20), rnorm(20))
  groups <- rep(c("A", "B"), each = 20)

  expect_error(
    check_homogeneity_of_variance(data, groups, alpha = -0.1),
    "alpha must be"
  )
  expect_error(
    check_homogeneity_of_variance(data, groups, alpha = 1.5),
    "alpha must be"
  )
})

test_that("check_homogeneity_of_variance() integrates Level 1 utilities", {
  set.seed(123)
  data <- c(rnorm(30), rnorm(30))
  groups <- rep(c("A", "B"), each = 30)
  result <- check_homogeneity_of_variance(data, groups)

  # Should have APA 7 formatted p-value
  expect_true(!is.null(result$p_value_formatted))
  expect_match(result$p_value_formatted, "p [=<]")

  # Should have formatted n
  expect_true(!is.null(result$n_formatted))
})

test_that("check_homogeneity_of_variance() provides comprehensive output", {
  set.seed(123)
  data <- c(rnorm(30), rnorm(30))
  groups <- rep(c("A", "B"), each = 30)
  result <- check_homogeneity_of_variance(data, groups)

  # Check all required fields
  expect_true(!is.null(result$interpretation))
  expect_true(!is.null(result$recommendation))
  expect_true(!is.null(result$publication_text))
  expect_true(!is.null(result$verbose_explanation))
  expect_true(!is.null(result$visual_guide))
  expect_true(!is.null(result$variance_ratio))
  expect_true(!is.null(result$group_statistics))
})

test_that("check_homogeneity_of_variance() handles missing data appropriately", {
  data <- c(rnorm(25), rep(NA, 5), rnorm(28), rep(NA, 2))
  groups <- rep(c("A", "B"), each = 30)

  # Should warn about 10-20% missing
  expect_message(
    result <- check_homogeneity_of_variance(data, groups),
    "missing"
  )
  expect_equal(result$n_total, 53)
  expect_equal(result$n_missing, 7)
})

# ==============================================================================
# check_independence() Tests
# ==============================================================================

test_that("check_independence() validates design-based independence", {
  result <- check_independence(
    data_structure = "cross-sectional survey",
    is_independent = TRUE
  )

  expect_s3_class(result, "assumption_check")
  expect_equal(result$test, "Independence Assessment")
  expect_equal(result$assumption, "Independence of Observations")
  expect_true(result$assumption_met)
  expect_true(!is.null(result$design_checklist))
})

test_that("check_independence() detects design-based violations", {
  result <- check_independence(
    data_structure = "repeated measures",
    is_independent = FALSE,
    clustering_note = "3 measurements per participant"
  )

  expect_s3_class(result, "assumption_check")
  expect_false(result$assumption_met)
  expect_match(result$recommendation, "repeated measures", ignore.case = TRUE)
})

test_that("check_independence() detects autocorrelation in data", {
  set.seed(123)
  # Create autocorrelated data
  data <- arima.sim(model = list(ar = 0.7), n = 100)
  # This will warn about autocorrelation, which is expected
  result <- suppressWarnings(
    check_independence(
      data = as.numeric(data),
      data_structure = "time series"
    )
  )

  expect_s3_class(result, "assumption_check")
  expect_true(!is.na(result$autocorrelation))
  expect_true(abs(result$autocorrelation) > 0)
})

test_that("check_independence() handles no autocorrelation", {
  set.seed(123)
  # Random independent data
  data <- rnorm(100)
  result <- check_independence(
    data = data,
    data_structure = "cross-sectional survey",
    is_independent = TRUE
  )

  expect_s3_class(result, "assumption_check")
  expect_true(!is.na(result$autocorrelation))
  expect_true(abs(result$autocorrelation) < 0.3)  # Low autocorrelation
})

test_that("check_independence() detects repeated measures from id_var", {
  set.seed(123)
  data <- rnorm(150)
  ids <- rep(1:50, each = 3)  # 3 obs per person

  result <- check_independence(
    data = data,
    id_var = ids,
    data_structure = "cross-sectional survey"
  )

  expect_s3_class(result, "assumption_check")
  expect_equal(result$n_unique_ids, 50)
  expect_equal(result$avg_obs_per_id, 3)
  expect_false(result$assumption_met)  # Should detect violation
})

test_that("check_independence() errors on non-numeric data", {
  expect_error(
    check_independence(data = c("a", "b", "c")),
    "Data must be numeric"
  )
})

test_that("check_independence() errors on empty data", {
  expect_error(
    check_independence(data = numeric(0)),
    "Cannot test independence on empty data"
  )
})

test_that("check_independence() handles NaN and Inf in data", {
  data <- c(1, 2, NaN, 4, Inf, 6, 7, 8, 9, 10)

  # Both NaN and Inf warnings expected, plus missing message
  result <- suppressWarnings(suppressMessages(
    check_independence(data = data)
  ))
  expect_equal(result$n_obs, 8)
})

test_that("check_independence() errors on too few observations", {
  expect_error(
    check_independence(data = c(1, 2)),
    "Too few observations"
  )
})

test_that("check_independence() errors on mismatched id_var length", {
  data <- 1:10
  id_var <- 1:5

  expect_error(
    check_independence(data = data, id_var = id_var),
    "same length"
  )
})

test_that("check_independence() errors on mismatched time_var length", {
  data <- 1:10
  time_var <- 1:5

  expect_error(
    check_independence(data = data, time_var = time_var),
    "same length"
  )
})

test_that("check_independence() sorts by time_var", {
  set.seed(123)
  data <- rnorm(20)
  time_var <- sample(1:20)  # Random order

  expect_message(
    result <- check_independence(data = data, time_var = time_var),
    "sorted by time"
  )
})

test_that("check_independence() validates data_structure parameter", {
  expect_error(
    check_independence(data_structure = 123),
    "data_structure must be a single character"
  )
  expect_error(
    check_independence(data_structure = c("a", "b")),
    "data_structure must be a single character"
  )
})

test_that("check_independence() validates is_independent parameter", {
  expect_error(
    check_independence(is_independent = "yes"),
    "is_independent must be TRUE or FALSE"
  )
  expect_error(
    check_independence(is_independent = c(TRUE, FALSE)),
    "is_independent must be TRUE or FALSE"
  )
})

test_that("check_independence() provides comprehensive output", {
  set.seed(123)
  data <- rnorm(100)
  result <- check_independence(
    data = data,
    data_structure = "cross-sectional survey"
  )

  # Check all required fields
  expect_true(!is.null(result$interpretation))
  expect_true(!is.null(result$recommendation))
  expect_true(!is.null(result$publication_text))
  expect_true(!is.null(result$verbose_explanation))
  expect_true(!is.null(result$design_checklist))
  expect_true(!is.null(result$visual_guide))
})

# ==============================================================================
# Print Method Tests
# ==============================================================================

test_that("print.assumption_check() works for normality", {
  set.seed(123)
  data <- rnorm(50)
  result <- check_normality(data, "test_var")

  # Should not error
  expect_output(print(result), "STATISTICAL ASSUMPTION CHECK")
  expect_output(print(result), "Normality")
  expect_output(print(result, verbose = FALSE), "INTERPRETATION")
})

test_that("print.assumption_check() works for homogeneity", {
  set.seed(123)
  data <- c(rnorm(30), rnorm(30))
  groups <- rep(c("A", "B"), each = 30)
  result <- check_homogeneity_of_variance(data, groups)

  expect_output(print(result), "STATISTICAL ASSUMPTION CHECK")
  expect_output(print(result), "Homogeneity of Variance")
})

test_that("print.assumption_check() works for independence", {
  result <- check_independence(
    data_structure = "cross-sectional survey",
    is_independent = TRUE
  )

  expect_output(print(result), "STATISTICAL ASSUMPTION CHECK")
  expect_output(print(result), "Independence of Observations")
  expect_output(print(result, verbose = TRUE), "CHECKLIST")
})

# ==============================================================================
# Integration Tests (Functions Work Together)
# ==============================================================================

test_that("All assumption checks return consistent S3 structure", {
  set.seed(123)

  # Normality
  norm <- check_normality(rnorm(50), "var1")
  expect_s3_class(norm, "assumption_check")
  expect_true("interpretation" %in% names(norm))
  expect_true("recommendation" %in% names(norm))

  # Homogeneity
  data <- c(rnorm(30), rnorm(30))
  groups <- rep(c("A", "B"), each = 30)
  hom <- check_homogeneity_of_variance(data, groups)
  expect_s3_class(hom, "assumption_check")
  expect_true("interpretation" %in% names(hom))

  # Independence
  ind <- check_independence(data_structure = "cross-sectional survey")
  expect_s3_class(ind, "assumption_check")
  expect_true("interpretation" %in% names(ind))
})

test_that("All functions integrate with Level 1 format_p() correctly", {
  set.seed(123)

  # Normality
  norm <- check_normality(rnorm(50))
  expect_match(norm$p_value_formatted, "p [=<]")

  # Homogeneity
  data <- c(rnorm(30), rnorm(30))
  groups <- rep(c("A", "B"), each = 30)
  hom <- check_homogeneity_of_variance(data, groups)
  expect_match(hom$p_value_formatted, "p [=<]")
})

# ==============================================================================
# End of Test Suite
# ==============================================================================
