# ==============================================================================
# LEVEL 1: UTILITIES EDGE CASE TESTS (Gold Standard)
# ==============================================================================
#
# This file comprehensively tests all edge cases for utility functions:
#   - format_p()
#   - format_n()
#   - format_est_ci()
#   - assert_vars_present()
#   - clean_names_safe()
#
# Each function is tested against ~10+ edge cases to ensure bulletproof
# operation and beginner-friendly error messages following APA 7 standards.
# ==============================================================================

library(testthat)

# Source files directly to test latest implementation
source(test_path("../../R/utilities.R"))
suppressPackageStartupMessages(library(rlang))

# ==============================================================================
# format_p() Tests - P-Value Formatting
# ==============================================================================

test_that("format_p() handles basic valid inputs", {
  expect_equal(format_p(0.042), "p = .042")
  expect_equal(format_p(0.001), "p = .001")
  expect_equal(format_p(0.5), "p = .500")
})

test_that("format_p() handles p = 0 correctly (APA 7)", {
  expect_equal(format_p(0), "p < .001")
  expect_equal(format_p(0.0001), "p < .001")
  expect_equal(format_p(0.0009), "p < .001")
})

test_that("format_p() warns about p = 1", {
  expect_warning(format_p(1), "p-value of exactly 1.000")
  expect_equal(suppressWarnings(format_p(1)), "p = 1.000")
})

test_that("format_p() handles vectors correctly", {
  result <- format_p(c(0.01, 0.05, 0.10))
  expect_length(result, 3)
  expect_equal(result[1], "p = .010")
})

test_that("format_p() detects and handles NaN values", {
  expect_error(format_p(NaN), "NaN")
  expect_error(format_p(c(0.01, NaN, 0.05)), "NaN")
})

test_that("format_p() detects and handles Infinite values", {
  expect_error(format_p(Inf), "Infinite p-values")
  expect_error(format_p(-Inf), "Infinite p-values")
  expect_error(format_p(c(0.01, Inf)), "Infinite")
})

test_that("format_p() validates range [0, 1]", {
  expect_error(format_p(-0.1), "between 0 and 1")
  expect_error(format_p(1.5), "between 0 and 1")
  expect_error(format_p(999), "between 0 and 1")
})

test_that("format_p() rejects non-numeric input", {
  expect_error(format_p("0.05"), "must be numeric")
  expect_error(format_p(factor(0.05)), "must be numeric")
  expect_error(format_p(TRUE), "must be numeric")
})

test_that("format_p() rejects empty input", {
  expect_error(format_p(numeric(0)), "Cannot format empty")
  expect_error(format_p(c()), "Cannot format empty")
})

test_that("format_p() handles NA values correctly", {
  result <- format_p(c(0.01, NA, 0.05))
  expect_equal(result[2], NA_character_)
  expect_warning(format_p(c(NA, NA, NA)), "all values are NA")
})

test_that("format_p() validates digits parameter", {
  expect_error(format_p(0.042, digits = -1), "Invalid.*digits")
  expect_error(format_p(0.042, digits = 11), "Invalid.*digits")
  expect_error(format_p(0.042, digits = "3"), "Invalid.*digits")
})

test_that("format_p() supports APA and plain styles", {
  expect_equal(format_p(0.042, style = "apa"), "p = .042")
  expect_equal(format_p(0.042, style = "plain"), ".042")
})


# ==============================================================================
# format_n() Tests - Sample Size Formatting
# ==============================================================================

test_that("format_n() handles basic valid inputs", {
  expect_equal(format_n(100), "N = 100")
  expect_equal(format_n(25, type = "n"), "n = 25")
  expect_equal(format_n(100, include_label = FALSE), "100")
})

test_that("format_n() adds comma separators for large numbers (APA 7)", {
  expect_match(format_n(1000), "1,000")
  expect_match(format_n(1234567), "1,234,567")
})

test_that("format_n() detects and rejects negative sample sizes", {
  expect_error(format_n(-10), "Negative sample size")
  expect_error(format_n(c(10, -5)), "Negative")
})

test_that("format_n() warns about decimal sample sizes", {
  expect_warning(format_n(100.5), "Decimal sample size")
  expect_equal(suppressWarnings(format_n(100.5)), "N = 101")
})

test_that("format_n() warns about zero sample size", {
  expect_warning(format_n(0), "Zero sample size")
})

test_that("format_n() warns about very small samples", {
  expect_warning(format_n(2), "Very small sample")
  expect_warning(format_n(1), "single observation")
})

test_that("format_n() warns about very large samples", {
  expect_warning(format_n(10000001), "very large")
})

test_that("format_n() detects NaN and Infinite values", {
  expect_error(format_n(NaN), "NaN")
  expect_error(format_n(Inf), "Infinite")
  expect_error(format_n(-Inf), "Infinite")
})

test_that("format_n() rejects non-numeric input", {
  expect_error(format_n("100"), "must be numeric")
  expect_error(format_n(factor(100)), "must be numeric")
})

test_that("format_n() rejects empty input", {
  expect_error(format_n(numeric(0)), "empty")
})

test_that("format_n() handles vectors correctly", {
  result <- format_n(c(100, 200, 300))
  expect_length(result, 3)
  expect_match(result[1], "100")
})


# ==============================================================================
# format_est_ci() Tests - Confidence Interval Formatting
# ==============================================================================

test_that("format_est_ci() handles basic valid inputs", {
  expect_equal(format_est_ci(2.5, 1.5, 3.5), "2.50 [1.50, 3.50]")
  expect_equal(format_est_ci(10, 8, 12, digits = 0), "10 [8, 12]")
})

test_that("format_est_ci() handles percentage formatting", {
  result <- format_est_ci(50, 40, 60, percent = TRUE)
  expect_match(result, "50.00%.*40.00%.*60.00%")
})

test_that("format_est_ci() validates all inputs provided", {
  expect_error(format_est_ci(2.5, 1.5), "must provide all three")
  expect_error(format_est_ci(2.5, hi = 3.5), "must provide all three")
})

test_that("format_est_ci() detects inverted CIs (lo > hi)", {
  expect_error(format_est_ci(2.5, 3.5, 1.5), "lower bound exceeds upper")
})

test_that("format_est_ci() warns about zero-width CIs", {
  expect_warning(format_est_ci(2.5, 2.5, 2.5), "Zero-width")
})

test_that("format_est_ci() warns if estimate outside its own CI", {
  expect_warning(format_est_ci(5, 1, 3), "falls outside")
  expect_warning(format_est_ci(0, 1, 3), "falls outside")
})

test_that("format_est_ci() validates vector lengths match", {
  expect_error(format_est_ci(c(1, 2), c(0.5), c(1.5, 2.5)), "same length")
})

test_that("format_est_ci() detects NaN values", {
  expect_error(format_est_ci(NaN, 1, 3), "NaN")
  expect_error(format_est_ci(2, NaN, 3), "NaN")
  expect_error(format_est_ci(2, 1, NaN), "NaN")
})

test_that("format_est_ci() detects Infinite values", {
  expect_error(format_est_ci(Inf, 1, 3), "Infinite")
  expect_error(format_est_ci(2, -Inf, 3), "Infinite")
})

test_that("format_est_ci() rejects non-numeric inputs", {
  expect_error(format_est_ci("2.5", 1.5, 3.5), "must be numeric")
  expect_error(format_est_ci(2.5, "1.5", 3.5), "must be numeric")
})

test_that("format_est_ci() validates digits parameter", {
  expect_error(format_est_ci(2.5, 1.5, 3.5, digits = -1), "Invalid.*digits")
  expect_error(format_est_ci(2.5, 1.5, 3.5, digits = 11), "Invalid.*digits")
})

test_that("format_est_ci() handles vectors correctly", {
  result <- format_est_ci(c(2, 3), c(1, 2), c(3, 4))
  expect_length(result, 2)
})


# ==============================================================================
# assert_vars_present() Tests - Variable Validation with Fuzzy Matching
# ==============================================================================

test_that("assert_vars_present() passes with valid variables", {
  df <- data.frame(age = 1:5, gender = letters[1:5], income = 1:5)
  expect_silent(assert_vars_present(df, c("age", "gender")))
  expect_silent(assert_vars_present(df, "income"))
})

test_that("assert_vars_present() detects missing variables", {
  df <- data.frame(age = 1:5, gender = letters[1:5])
  expect_error(assert_vars_present(df, "income"), "Missing variable")
})

test_that("assert_vars_present() provides fuzzy matching suggestions for typos", {
  df <- data.frame(age = 1:5, gender = letters[1:5], income = 1:5)

  # Should suggest 'gender' for 'gendr'
  expect_error(assert_vars_present(df, "gendr"), "Did you mean.*gender")

  # Should suggest 'income' for 'incme'
  expect_error(assert_vars_present(df, "incme"), "Did you mean.*income")
})

test_that("assert_vars_present() handles case-insensitive suggestions", {
  df <- data.frame(age = 1:5, Gender = letters[1:5])

  # 'gender' should suggest 'Gender'
  expect_error(assert_vars_present(df, "gender"), "Did you mean.*Gender")
})

test_that("assert_vars_present() handles multiple missing variables", {
  df <- data.frame(age = 1:5)
  expect_error(assert_vars_present(df, c("gender", "income")), "2 missing")
})

test_that("assert_vars_present() uses label in error messages", {
  df <- data.frame(age = 1:5)
  expect_error(assert_vars_present(df, "gender", "regression"), "regression.*gender")
})

test_that("assert_vars_present() validates df is a data frame", {
  expect_error(assert_vars_present(c(1, 2, 3), "x"), "must be a data frame")
  expect_error(assert_vars_present(matrix(1:9, 3, 3), "x"), "must be a data frame")
})

test_that("assert_vars_present() validates vars is character", {
  df <- data.frame(age = 1:5)
  expect_error(assert_vars_present(df, 1), "must be a character vector")
  expect_error(assert_vars_present(df, factor("age")), "must be a character vector")
})

test_that("assert_vars_present() handles empty vars vector", {
  df <- data.frame(age = 1:5)
  expect_error(assert_vars_present(df, character(0)), "empty")
})

test_that("assert_vars_present() handles partial string matches", {
  df <- data.frame(participant_age = 1:5, participant_income = 1:5)

  # 'age' should suggest 'participant_age'
  expect_error(assert_vars_present(df, "age"), "Did you mean.*participant_age")
})


# ==============================================================================
# clean_names_safe() Tests - Variable Name Cleaning
# ==============================================================================

test_that("clean_names_safe() cleans messy names", {
  df <- data.frame(`First Name` = 1, `Last Name` = 2, check.names = FALSE)
  result <- suppressMessages(clean_names_safe(df))
  expect_true("first_name" %in% names(result))
  expect_true("last_name" %in% names(result))
})

test_that("clean_names_safe() handles already clean names", {
  df <- data.frame(first_name = 1, last_name = 2)
  result <- suppressMessages(clean_names_safe(df))
  expect_equal(names(df), names(result))
})

test_that("clean_names_safe() validates df is a data frame", {
  expect_error(clean_names_safe(c(1, 2, 3)), "must be a data frame")
  expect_error(clean_names_safe(matrix(1:9, 3, 3)), "must be a data frame")
})

test_that("clean_names_safe() handles empty data frame", {
  df <- data.frame()
  expect_warning(clean_names_safe(df), "no columns")
})

test_that("clean_names_safe() warns about single column", {
  df <- data.frame(`My Var` = 1:5, check.names = FALSE)
  expect_warning(clean_names_safe(df), "only one column")
})

test_that("clean_names_safe() handles special characters", {
  df <- data.frame(`Income ($)` = 1, `Rate (%)` = 2, check.names = FALSE)
  result <- suppressMessages(clean_names_safe(df))
  # Should remove special characters
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", names(result))))
})

test_that("clean_names_safe() handles names starting with numbers", {
  df <- data.frame(`2023_sales` = 1, check.names = FALSE)
  result <- suppressMessages(clean_names_safe(df))
  # Should prepend 'x' or handle appropriately
  expect_false(grepl("^[0-9]", names(result)[1]))
})

test_that("clean_names_safe() handles duplicate names", {
  df <- data.frame(age = 1, age = 2, age = 3, check.names = FALSE)
  result <- suppressWarnings(suppressMessages(clean_names_safe(df)))
  # Should make unique
  expect_equal(length(unique(names(result))), 3)
})

test_that("clean_names_safe() handles spaces", {
  df <- data.frame(`First Name` = 1, `Last Name` = 2, check.names = FALSE)
  result <- suppressMessages(clean_names_safe(df))
  expect_false(any(grepl(" ", names(result))))
})

test_that("clean_names_safe() handles uppercase", {
  df <- data.frame(AGE = 1, GENDER = 2, INCOME = 3)
  result <- suppressMessages(clean_names_safe(df))
  # Should convert to lowercase
  expect_true(all(names(result) == tolower(names(result))))
})

test_that("clean_names_safe() detects problematic names and informs user", {
  df <- data.frame(`First Name` = 1, `Income ($)` = 2, check.names = FALSE)
  expect_message(clean_names_safe(df), "Problematic column names detected")
})


# ==============================================================================
# INTEGRATION TESTS - Functions Working Together
# ==============================================================================

test_that("clean_names_safe() output works with other package functions", {
  df <- data.frame(
    `Age (years)` = c(25, 30, 35, 40, 45),
    `Income ($1000s)` = c(45, 67, 23, 89, 34),
    check.names = FALSE
  )

  df_clean <- suppressMessages(clean_names_safe(df))

  # Should be able to use assert_vars_present on cleaned names
  expect_silent(assert_vars_present(df_clean, names(df_clean)))

  # Cleaned names should work with calculate_summary_stats
  stats <- calculate_summary_stats(df_clean[[1]])
  expect_equal(stats$n, 5)
})

test_that("Format functions work together in typical analysis workflow", {
  # Simulating a regression output
  p_val <- 0.042
  n_val <- 150
  est_val <- 2.5
  lo_val <- 1.5
  hi_val <- 3.5

  # All formatting should work without errors
  p_formatted <- format_p(p_val)
  n_formatted <- format_n(n_val)
  ci_formatted <- format_est_ci(est_val, lo_val, hi_val)

  expect_true(is.character(p_formatted))
  expect_true(is.character(n_formatted))
  expect_true(is.character(ci_formatted))

  # Should follow APA 7 formatting
  expect_match(p_formatted, "p = \\.\\d+")  # No leading zero
  expect_match(n_formatted, "N = 150")
  expect_match(ci_formatted, "\\[")  # Square brackets
})
