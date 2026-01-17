# Layer 1 Tests: Variable Standardization Helpers
# These tests verify the low-level standardization functions for creating composites
# Run with: testthat::test_file("tests/testthat/test-L1-standardization.R")

# Setup ----
test_that("L1 standardization helpers are available", {
  expect_true(exists("standardize_z", mode = "function"))
  expect_true(exists("standardize_range", mode = "function"))
  expect_true(exists("standardize_scale", mode = "function"))
  expect_true(exists("standardize_multiple", mode = "function"))
})


# standardize_z() ----

test_that("standardize_z produces mean=0 and sd=1", {
  x <- c(1, 2, 3, 4, 5, 6, 7)

  result <- standardize_z(x)

  # Check mean ≈ 0 and SD ≈ 1 (within floating point tolerance)
  expect_equal(mean(result$standardized), 0, tolerance = 1e-10)
  expect_equal(sd(result$standardized), 1, tolerance = 1e-10)
})


test_that("standardize_z returns correct structure", {
  x <- c(1, 2, 3, 4, 5)

  result <- standardize_z(x)

  expect_type(result, "list")
  expect_named(result, c("standardized", "meta"))
  expect_type(result$standardized, "double")
  expect_length(result$standardized, 5)

  expect_named(result$meta, c("method", "original_mean", "original_sd", "n_valid", "n_missing"))
  expect_equal(result$meta$method, "z-score")
})


test_that("standardize_z computes correct metadata", {
  x <- c(10, 20, 30, 40, 50)

  result <- standardize_z(x)

  expect_equal(result$meta$original_mean, 30)
  expect_equal(result$meta$original_sd, round(sd(x), 4))  # Meta rounds to 4 decimals
  expect_equal(result$meta$n_valid, 5)
  expect_equal(result$meta$n_missing, 0)
})


test_that("standardize_z handles missing values with na.rm=TRUE", {
  x <- c(1, 2, NA, 4, 5, NA, 7)

  result <- standardize_z(x, na.rm = TRUE)

  # Should have 2 NAs in output
  expect_equal(sum(is.na(result$standardized)), 2)

  # Non-NA values should have mean≈0, sd≈1
  expect_equal(mean(result$standardized, na.rm = TRUE), 0, tolerance = 1e-10)
  expect_equal(sd(result$standardized, na.rm = TRUE), 1, tolerance = 1e-10)

  expect_equal(result$meta$n_valid, 5)
  expect_equal(result$meta$n_missing, 2)
})


test_that("standardize_z errors with all missing values", {
  x <- c(NA_real_, NA_real_, NA_real_)

  expect_error(
    standardize_z(x),
    "all values are missing"
  )
})


test_that("standardize_z errors with zero variance", {
  x <- c(5, 5, 5, 5, 5)

  expect_error(
    standardize_z(x),
    "zero variance"
  )
})


test_that("standardize_z warns with very small n", {
  x <- c(1, 2)

  expect_warning(
    standardize_z(x),
    "very few observations"
  )
})


test_that("standardize_z can skip variance check", {
  x <- c(5, 5, 5, 5, 5)

  # Should error by default
  expect_error(standardize_z(x))

  # Should not error with check_variance = FALSE
  expect_silent(result <- standardize_z(x, check_variance = FALSE))

  # All values should be NaN (0/0)
  expect_true(all(is.nan(result$standardized)))
})


test_that("standardize_z errors with non-numeric input", {
  x <- c("a", "b", "c")

  expect_error(
    standardize_z(x),
    "must be numeric"
  )
})


test_that("standardize_z preserves relative ordering", {
  x <- c(10, 5, 20, 15, 30)

  result <- standardize_z(x)

  # Order should be preserved
  expect_equal(order(x), order(result$standardized))
})


# standardize_range() ----

test_that("standardize_range produces correct [0,1] range", {
  x <- c(10, 20, 30, 40, 50)

  result <- standardize_range(x)

  expect_equal(min(result$standardized), 0)
  expect_equal(max(result$standardized), 1)
})


test_that("standardize_range produces correct custom range", {
  x <- c(1, 2, 3, 4, 5)

  result <- standardize_range(x, new_min = 1, new_max = 7)

  expect_equal(min(result$standardized), 1)
  expect_equal(max(result$standardized), 7)
})


test_that("standardize_range returns correct structure", {
  x <- c(1, 2, 3, 4, 5)

  result <- standardize_range(x)

  expect_type(result, "list")
  expect_named(result, c("standardized", "meta"))
  expect_type(result$standardized, "double")

  expect_named(result$meta, c("method", "original_min", "original_max",
                              "new_min", "new_max", "n_valid", "n_missing"))
  expect_equal(result$meta$method, "range")
})


test_that("standardize_range computes correct metadata", {
  x <- c(10, 20, 30, 40, 50)

  result <- standardize_range(x, new_min = 0, new_max = 100)

  expect_equal(result$meta$original_min, 10)
  expect_equal(result$meta$original_max, 50)
  expect_equal(result$meta$new_min, 0)
  expect_equal(result$meta$new_max, 100)
  expect_equal(result$meta$n_valid, 5)
  expect_equal(result$meta$n_missing, 0)
})


test_that("standardize_range handles missing values", {
  x <- c(10, NA, 30, 40, NA)

  result <- standardize_range(x, na.rm = TRUE)

  expect_equal(sum(is.na(result$standardized)), 2)
  expect_equal(result$meta$n_valid, 3)
  expect_equal(result$meta$n_missing, 2)
})


test_that("standardize_range errors with all missing", {
  x <- c(NA_real_, NA_real_, NA_real_)

  expect_error(
    standardize_range(x),
    "all values are missing"
  )
})


test_that("standardize_range errors with zero range", {
  x <- c(5, 5, 5, 5, 5)

  expect_error(
    standardize_range(x),
    "zero range"
  )
})


test_that("standardize_range errors with invalid new_min/new_max", {
  x <- c(1, 2, 3)

  expect_error(
    standardize_range(x, new_min = 10, new_max = 5),
    "new_min must be less than new_max"
  )
})


test_that("standardize_range errors with non-numeric input", {
  x <- c("a", "b", "c")

  expect_error(
    standardize_range(x),
    "must be numeric"
  )
})


test_that("standardize_range produces correct transformation", {
  # 1-5 scale to 1-7 scale
  x <- c(1, 2, 3, 4, 5)

  result <- standardize_range(x, new_min = 1, new_max = 7)

  expect_equal(result$standardized, c(1, 2.5, 4, 5.5, 7))
})


test_that("standardize_range preserves relative ordering", {
  x <- c(50, 10, 30, 20, 40)

  result <- standardize_range(x)

  expect_equal(order(x), order(result$standardized))
})


# standardize_scale() ----

test_that("standardize_scale converts to target scale", {
  x <- c(1, 2, 3, 4, 5)

  result <- standardize_scale(x, target_min = 1, target_max = 7)

  expect_equal(min(result$standardized), 1)
  expect_equal(max(result$standardized), 7)
})


test_that("standardize_scale returns correct metadata", {
  x <- c(0, 25, 50, 75, 100)

  result <- standardize_scale(x, target_min = 1, target_max = 7)

  expect_equal(result$meta$method, "scale_conversion")
  expect_equal(result$meta$target_min, 1)
  expect_equal(result$meta$target_max, 7)
  expect_equal(result$meta$original_min, 0)
  expect_equal(result$meta$original_max, 100)
})


test_that("standardize_scale errors without target_min/target_max", {
  x <- c(1, 2, 3)

  expect_error(
    standardize_scale(x),
    "target_min and target_max are required"
  )

  expect_error(
    standardize_scale(x, target_min = 1),
    "target_min and target_max are required"
  )
})


test_that("standardize_scale is identical to standardize_range with same params", {
  x <- c(10, 20, 30, 40, 50)

  result_scale <- standardize_scale(x, target_min = 0, target_max = 1)
  result_range <- standardize_range(x, new_min = 0, new_max = 1)

  expect_equal(result_scale$standardized, result_range$standardized)
})


test_that("standardize_scale converts 1-5 to 1-7 correctly", {
  x <- c(1, 2, 3, 4, 5)

  result <- standardize_scale(x, target_min = 1, target_max = 7)

  expect_equal(result$standardized[1], 1)    # min stays min
  expect_equal(result$standardized[3], 4)    # midpoint becomes midpoint
  expect_equal(result$standardized[5], 7)    # max stays max
})


test_that("standardize_scale converts 0-100 NPS to 1-7", {
  nps <- c(0, 25, 50, 75, 100)

  result <- standardize_scale(nps, target_min = 1, target_max = 7)

  expect_equal(result$standardized, c(1, 2.5, 4, 5.5, 7))
})


# standardize_multiple() ----

test_that("standardize_multiple standardizes multiple variables", {
  df <- data.frame(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(10, 20, 30, 40, 50),
    var3 = c(100, 200, 300, 400, 500)
  )

  result <- standardize_multiple(
    data = df,
    vars = c("var1", "var2", "var3"),
    method = "z-score"
  )

  expect_s3_class(result$data, "data.frame")
  expect_named(result$data, c("var1", "var2", "var3"))

  # Each variable should be z-scored
  expect_equal(mean(result$data$var1), 0, tolerance = 1e-10)
  expect_equal(mean(result$data$var2), 0, tolerance = 1e-10)
  expect_equal(mean(result$data$var3), 0, tolerance = 1e-10)
})


test_that("standardize_multiple returns metadata for each variable", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(10, 20, 30)
  )

  result <- standardize_multiple(
    data = df,
    vars = c("var1", "var2"),
    method = "z-score"
  )

  expect_type(result$meta, "list")
  expect_named(result$meta, c("var1", "var2"))

  expect_equal(result$meta$var1$method, "z-score")
  expect_equal(result$meta$var2$method, "z-score")
  expect_equal(result$meta$var1$original_mean, 2)
  expect_equal(result$meta$var2$original_mean, 20)
})


test_that("standardize_multiple works with range method", {
  df <- data.frame(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(10, 20, 30, 40, 50)
  )

  result <- standardize_multiple(
    data = df,
    vars = c("var1", "var2"),
    method = "range",
    new_min = 0,
    new_max = 1
  )

  expect_equal(min(result$data$var1), 0)
  expect_equal(max(result$data$var1), 1)
  expect_equal(min(result$data$var2), 0)
  expect_equal(max(result$data$var2), 1)
})


test_that("standardize_multiple works with scale method", {
  df <- data.frame(
    item1 = c(1, 2, 3, 4, 5),
    item2 = c(0, 25, 50, 75, 100)
  )

  result <- standardize_multiple(
    data = df,
    vars = c("item1", "item2"),
    method = "scale",
    target_min = 1,
    target_max = 7
  )

  expect_equal(min(result$data$item1), 1)
  expect_equal(max(result$data$item1), 7)
  expect_equal(min(result$data$item2), 1)
  expect_equal(max(result$data$item2), 7)
})


test_that("standardize_multiple errors with missing variables", {
  df <- data.frame(var1 = c(1, 2, 3))

  expect_error(
    standardize_multiple(df, vars = c("var1", "var2"), method = "z-score"),
    "Variables not found"
  )
})


test_that("standardize_multiple errors with non-numeric variables", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c("a", "b", "c")
  )

  expect_error(
    standardize_multiple(df, vars = c("var1", "var2"), method = "z-score"),
    "All variables must be numeric"
  )
})


test_that("standardize_multiple errors with non-data.frame input", {
  x <- c(1, 2, 3)

  expect_error(
    standardize_multiple(x, vars = "var1", method = "z-score"),
    "must be a data frame"
  )
})


test_that("standardize_multiple preserves other columns", {
  df <- data.frame(
    id = c(1, 2, 3),
    var1 = c(10, 20, 30),
    var2 = c(100, 200, 300),
    group = c("A", "B", "C")
  )

  result <- standardize_multiple(
    data = df,
    vars = c("var1", "var2"),
    method = "z-score"
  )

  # Should have all original columns
  expect_true(all(c("id", "var1", "var2", "group") %in% names(result$data)))

  # Non-standardized columns should be unchanged
  expect_equal(result$data$id, df$id)
  expect_equal(result$data$group, df$group)
})


# Practical application tests ----

test_that("z-score standardization enables combining different scales", {
  # Items on different scales
  df <- data.frame(
    satisfaction_5pt = c(1, 2, 3, 4, 5),
    nps_100pt = c(0, 25, 50, 75, 100),
    likelihood_7pt = c(1, 2.5, 4, 5.5, 7)
  )

  # Standardize all to z-scores
  result <- standardize_multiple(
    data = df,
    vars = c("satisfaction_5pt", "nps_100pt", "likelihood_7pt"),
    method = "z-score"
  )

  # Now can create composite by averaging
  composite <- rowMeans(result$data)

  expect_length(composite, 5)
  expect_true(all(!is.na(composite)))
})


test_that("scale standardization creates comparable metrics", {
  # Convert items to common 1-7 scale
  df <- data.frame(
    item1_5pt = c(1, 3, 5),
    item2_100pt = c(0, 50, 100)
  )

  result <- standardize_multiple(
    data = df,
    vars = c("item1_5pt", "item2_100pt"),
    method = "scale",
    target_min = 1,
    target_max = 7
  )

  # Min values should all be 1
  expect_equal(result$data$item1_5pt[1], 1)
  expect_equal(result$data$item2_100pt[1], 1)

  # Max values should all be 7
  expect_equal(result$data$item1_5pt[3], 7)
  expect_equal(result$data$item2_100pt[3], 7)

  # Midpoints should align
  expect_equal(result$data$item1_5pt[2], 4)
  expect_equal(result$data$item2_100pt[2], 4)
})


# Edge cases ----

test_that("standardization handles single unique value after removing NAs", {
  x <- c(5, 5, NA, 5, NA, 5)

  expect_error(
    standardize_z(x, na.rm = TRUE),
    "zero variance"
  )

  expect_error(
    standardize_range(x, na.rm = TRUE),
    "zero range"
  )
})


test_that("standardization handles negative values", {
  x <- c(-10, -5, 0, 5, 10)

  result_z <- standardize_z(x)
  expect_equal(mean(result_z$standardized), 0, tolerance = 1e-10)

  result_range <- standardize_range(x, new_min = 0, new_max = 1)
  expect_equal(min(result_range$standardized), 0)
  expect_equal(max(result_range$standardized), 1)
})


test_that("standardization handles large numbers", {
  x <- c(1e6, 2e6, 3e6, 4e6, 5e6)

  result <- standardize_z(x)
  expect_equal(mean(result$standardized), 0, tolerance = 1e-10)
  expect_equal(sd(result$standardized), 1, tolerance = 1e-10)
})


test_that("standardization handles very small numbers", {
  x <- c(1e-6, 2e-6, 3e-6, 4e-6, 5e-6)

  result <- standardize_z(x)
  expect_equal(mean(result$standardized), 0, tolerance = 1e-10)
  expect_equal(sd(result$standardized), 1, tolerance = 1e-10)
})
