# Tests for utility functions

test_that("format_p works with APA style", {
  # Basic formatting
  expect_equal(format_p(0.042), "p = .042")
  expect_equal(format_p(0.5), "p = .500")

  # Very small p-values
  expect_equal(format_p(0.0001), "p < .001")
  expect_equal(format_p(0.00001), "p < .001")

  # Edge cases
  expect_equal(format_p(1.0), "p = 1.000")
  expect_equal(format_p(0.001), "p = .001")  # Exactly at threshold
})

test_that("format_p works with plain style", {
  # Basic formatting with leading zero
  expect_equal(format_p(0.042, style = "plain"), "p = 0.042")
  expect_equal(format_p(0.5, style = "plain"), "p = 0.500")

  # Very small p-values
  expect_equal(format_p(0.0001, style = "plain"), "p < 0.001")
})

test_that("format_p handles different digit specifications", {
  # More digits
  expect_equal(format_p(0.04237, digits = 4), "p = .0424")

  # Fewer digits
  expect_equal(format_p(0.04237, digits = 2), "p = .04")

  # Very small with different threshold
  expect_equal(format_p(0.00001, digits = 4), "p < .0001")
})

test_that("format_p handles vectors", {
  p_vals <- c(0.042, 0.0001, 0.523)
  expected <- c("p = .042", "p < .001", "p = .523")
  expect_equal(format_p(p_vals), expected)
})

test_that("format_p handles NA values", {
  expect_equal(format_p(NA_real_), NA_character_)
  expect_equal(format_p(c(0.05, NA, 0.01)), c("p = .050", NA, "p = .010"))
})

test_that("format_p validates input", {
  # Non-numeric
  expect_error(format_p("not a number"), "p must be numeric")

  # Out of range
  expect_error(format_p(-0.1), "p-values must be between 0 and 1")
  expect_error(format_p(1.5), "p-values must be between 0 and 1")
})


# format_n tests ----

test_that("format_n works with default settings", {
  expect_equal(format_n(150), "N = 150")
  expect_equal(format_n(45, type = "n"), "n = 45")
})

test_that("format_n handles vectors", {
  expect_equal(format_n(c(100, 50), type = "n"), c("n = 100", "n = 50"))
})

test_that("format_n rounds to integers", {
  expect_equal(format_n(150.7), "N = 151")
  expect_equal(format_n(45.2), "N = 45")
})

test_that("format_n works without labels", {
  expect_equal(format_n(150, include_label = FALSE), "150")
  expect_equal(format_n(c(100, 50), include_label = FALSE), c("100", "50"))
})

test_that("format_n validates input", {
  expect_error(format_n("not a number"), "n must be numeric")
})


# format_est_ci tests ----

test_that("format_est_ci works with basic input", {
  expect_equal(format_est_ci(2.34, 1.23, 3.45), "2.34 [1.23, 3.45]")
})

test_that("format_est_ci handles different digits", {
  expect_equal(format_est_ci(2.3456, 1.2345, 3.4567, digits = 3), "2.346 [1.235, 3.457]")
  expect_equal(format_est_ci(2.3456, 1.2345, 3.4567, digits = 1), "2.3 [1.2, 3.5]")
})

test_that("format_est_ci handles vectors", {
  est <- c(2.34, 5.67)
  lo <- c(1.23, 4.56)
  hi <- c(3.45, 6.78)
  expected <- c("2.34 [1.23, 3.45]", "5.67 [4.56, 6.78]")
  expect_equal(format_est_ci(est, lo, hi), expected)
})

test_that("format_est_ci works with percentages", {
  expect_equal(
    format_est_ci(0.234, 0.123, 0.345, percent = TRUE),
    "23.40% [12.30%, 34.50%]"
  )
})

test_that("format_est_ci validates input", {
  # Non-numeric
  expect_error(format_est_ci("a", 1, 2), "est, lo, and hi must all be numeric")

  # Mismatched lengths
  expect_error(
    format_est_ci(c(1, 2), 1, c(2, 3)),
    "est, lo, and hi must have the same length"
  )
})


# assert_vars_present tests ----

test_that("assert_vars_present works with valid input", {
  df <- data.frame(x = 1:5, y = 6:10, z = 11:15)

  # Should not error
  expect_silent(assert_vars_present(df, c("x", "y")))
  expect_silent(assert_vars_present(df, c("x", "y", "z")))

  # Should return TRUE invisibly
  expect_true(assert_vars_present(df, c("x", "y")))
})

test_that("assert_vars_present catches missing variables", {
  df <- data.frame(x = 1:5, y = 6:10)

  # Missing one variable
  expect_error(
    assert_vars_present(df, c("x", "z")),
    "Missing variables: z"
  )

  # Missing multiple variables
  expect_error(
    assert_vars_present(df, c("a", "b", "c")),
    "Missing variables: a, b, c"
  )
})

test_that("assert_vars_present includes label in error message", {
  df <- data.frame(x = 1:5)

  expect_error(
    assert_vars_present(df, "z", "alpha calculation"),
    "Missing variables for alpha calculation: z"
  )
})

test_that("assert_vars_present suggests available variables", {
  df <- data.frame(x = 1:5, y = 6:10)

  expect_error(
    assert_vars_present(df, "z"),
    "Available variables: x, y"
  )
})


# clean_names_safe tests ----

test_that("clean_names_safe works when janitor available", {
  skip_if_not_installed("janitor")

  df <- data.frame(`First Name` = "John", `Last Name` = "Doe", check.names = FALSE)
  result <- clean_names_safe(df)

  expect_equal(names(result), c("first_name", "last_name"))
})

test_that("clean_names_safe returns original when janitor not available", {
  # This test would need to temporarily hide janitor
  # For now, we'll just check it doesn't error
  df <- data.frame(x = 1, y = 2)
  expect_silent(clean_names_safe(df))
})
