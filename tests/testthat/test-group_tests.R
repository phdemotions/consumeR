# Tests for test_group_differences function
# These tests ensure statistical comparisons work correctly

test_that("test_group_differences returns correct structure", {
  # Test that output has all expected components
  group1 <- c(1, 2, 3, 4, 5)
  group2 <- c(2, 3, 4, 5, 6)

  result <- test_group_differences(group1, group2)

  expect_type(result, "list")
  expect_named(result, c("test_used", "p_value", "significant",
                        "group1_mean", "group2_mean", "difference",
                        "group1_n", "group2_n", "interpretation",
                        "full_test_output"))
})

test_that("test_group_differences detects significant differences", {
  # Create clearly different groups
  group1 <- c(1, 2, 3, 4, 5)
  group2 <- c(10, 11, 12, 13, 14)

  result <- test_group_differences(group1, group2)

  expect_true(result$significant)
  expect_lt(result$p_value, 0.05)
})

test_that("test_group_differences handles similar groups", {
  # Create similar groups
  set.seed(123)
  group1 <- rnorm(50, mean = 5, sd = 1)
  group2 <- rnorm(50, mean = 5.1, sd = 1)

  result <- test_group_differences(group1, group2)

  # Should have a p-value
  expect_true(is.numeric(result$p_value))
  expect_gte(result$p_value, 0)
  expect_lte(result$p_value, 1)
})

test_that("test_group_differences validates input", {
  # Test non-numeric input
  expect_error(
    test_group_differences(c("a", "b"), c(1, 2)),
    "must be numeric"
  )

  # Test insufficient data
  expect_error(
    test_group_differences(c(1), c(2, 3)),
    "at least 2 valid observations"
  )

  # Test invalid alternative
  expect_error(
    test_group_differences(c(1, 2), c(3, 4), alternative = "invalid"),
    "must be one of"
  )

  # Test invalid test_type
  expect_error(
    test_group_differences(c(1, 2), c(3, 4), test_type = "invalid"),
    "must be one of"
  )
})

test_that("test_group_differences handles missing values", {
  group1 <- c(1, 2, NA, 4, 5)
  group2 <- c(2, 3, 4, NA, 6)

  expect_message(
    result <- test_group_differences(group1, group2),
    "missing value"
  )

  expect_equal(result$group1_n, 4)
  expect_equal(result$group2_n, 4)
})

test_that("test_group_differences auto-selects appropriate test", {
  # Large samples should use t-test
  set.seed(123)
  large_group1 <- rnorm(40)
  large_group2 <- rnorm(40)

  expect_message(
    result_large <- test_group_differences(large_group1, large_group2,
                                           test_type = "auto"),
    "t-test"
  )

  # Small samples should use Wilcoxon
  small_group1 <- c(1, 2, 3, 4, 5)
  small_group2 <- c(2, 3, 4, 5, 6)

  expect_message(
    result_small <- test_group_differences(small_group1, small_group2,
                                          test_type = "auto"),
    "Wilcoxon"
  )
})

test_that("test_group_differences respects test_type parameter", {
  group1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  group2 <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

  # Force t-test
  result_t <- test_group_differences(group1, group2, test_type = "t.test")
  expect_true(grepl("t-test", result_t$test_used))

  # Force Wilcoxon
  result_w <- test_group_differences(group1, group2, test_type = "wilcoxon")
  expect_true(grepl("Wilcoxon", result_w$test_used))
})

test_that("test_group_differences handles one-sided tests", {
  group1 <- c(1, 2, 3, 4, 5)
  group2 <- c(3, 4, 5, 6, 7)

  # Two-sided
  result_two <- test_group_differences(group1, group2, alternative = "two.sided")
  expect_true(is.numeric(result_two$p_value))

  # One-sided (less)
  result_less <- test_group_differences(group1, group2, alternative = "less")
  expect_true(is.numeric(result_less$p_value))

  # One-sided (greater)
  result_greater <- test_group_differences(group1, group2, alternative = "greater")
  expect_true(is.numeric(result_greater$p_value))
})

test_that("test_group_differences handles paired data", {
  # Before and after measurements
  before <- c(10, 12, 14, 16, 18)
  after <- c(9, 11, 13, 15, 17)

  result <- test_group_differences(before, after, paired = TRUE)

  expect_true(grepl("Paired", result$test_used))
  expect_true(is.numeric(result$p_value))
})

test_that("test_group_differences requires same length for paired tests", {
  group1 <- c(1, 2, 3, 4, 5)
  group2 <- c(2, 3, 4)

  expect_error(
    test_group_differences(group1, group2, paired = TRUE),
    "same length"
  )
})

test_that("test_group_differences calculates means correctly", {
  group1 <- c(2, 4, 6, 8, 10)
  group2 <- c(1, 3, 5, 7, 9)

  result <- test_group_differences(group1, group2)

  expect_equal(result$group1_mean, 6)
  expect_equal(result$group2_mean, 5)
  expect_equal(result$difference, 1)
})

test_that("test_group_differences provides interpretation", {
  group1 <- c(1, 2, 3, 4, 5)
  group2 <- c(10, 11, 12, 13, 14)

  result <- test_group_differences(group1, group2)

  expect_type(result$interpretation, "character")
  expect_gt(nchar(result$interpretation), 0)
})

test_that("test_group_differences includes full test output", {
  group1 <- c(1, 2, 3, 4, 5)
  group2 <- c(2, 3, 4, 5, 6)

  result <- test_group_differences(group1, group2)

  expect_true(!is.null(result$full_test_output))
  expect_s3_class(result$full_test_output, "htest")
})
