# Layer 1 Tests: SPSS Labelled Data Helpers
# These tests verify the low-level labelled data inspection and handling functions
# Run with: testthat::test_file("tests/testthat/test-L1-labelled-data.R")

# Setup ----
test_that("L1 labelled data helpers are available", {
  expect_true(exists("extract_value_labels", mode = "function"))
  expect_true(exists("identify_problematic_values", mode = "function"))
  expect_true(exists("recode_to_na", mode = "function"))
})


# extract_value_labels() ----

test_that("extract_value_labels works with haven_labelled vectors", {
  skip_if_not_installed("haven")

  # Create labelled vector
  x <- haven::labelled(
    c(1, 2, 1, 2, 99),
    labels = c("Male" = 1, "Female" = 2, "Prefer not to say" = 99)
  )

  result <- extract_value_labels(x)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("value", "label"))
  expect_equal(nrow(result), 3)
  expect_equal(result$value, c(1, 2, 99))
  expect_equal(result$label, c("Male", "Female", "Prefer not to say"))
})


test_that("extract_value_labels works with factors", {
  x <- factor(c("Low", "Medium", "High", "Low"))

  result <- extract_value_labels(x)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("value", "label"))
  expect_equal(nrow(result), 3)
  expect_equal(result$label, c("High", "Low", "Medium"))  # alphabetical order
})


test_that("extract_value_labels returns NULL for unlabelled vectors", {
  x <- c(1, 2, 3, 4, 5)

  result <- extract_value_labels(x)

  expect_null(result)
})


test_that("extract_value_labels returns NULL for empty factor levels", {
  x <- factor(character(0))

  result <- extract_value_labels(x)

  expect_null(result)
})


test_that("extract_value_labels sorts by value", {
  skip_if_not_installed("haven")

  # Create with unsorted labels
  x <- haven::labelled(
    c(1, 2, 3),
    labels = c("High" = 99, "Low" = 1, "Medium" = 50)
  )

  result <- extract_value_labels(x)

  expect_equal(result$value, c(1, 50, 99))
  expect_equal(result$label, c("Low", "Medium", "High"))
})


# identify_problematic_values() ----

test_that("identify_problematic_values finds 'Don't know' responses", {
  labels_df <- data.frame(
    value = c(1, 2, 3, 99),
    label = c("Yes", "No", "Maybe", "Don't know"),
    stringsAsFactors = FALSE
  )

  result <- identify_problematic_values(labels_df)

  expect_s3_class(result, "data.frame")
  expect_true(99 %in% result$value)
  expect_true("Don't know" %in% result$label)
  expect_true("reason" %in% names(result))
})


test_that("identify_problematic_values finds 'Refused' responses", {
  labels_df <- data.frame(
    value = c(1, 2, 3, 98),
    label = c("Agree", "Neutral", "Disagree", "Refused"),
    stringsAsFactors = FALSE
  )

  result <- identify_problematic_values(labels_df)

  expect_true(98 %in% result$value)
  expect_match(result$reason[result$value == 98], "Refused", ignore.case = TRUE)
})


test_that("identify_problematic_values finds 'Not applicable' responses", {
  labels_df <- data.frame(
    value = c(1, 2, -1),
    label = c("Low", "High", "N/A"),
    stringsAsFactors = FALSE
  )

  result <- identify_problematic_values(labels_df)

  expect_true(-1 %in% result$value)
  expect_match(result$reason[result$value == -1], "Not applicable", ignore.case = TRUE)
})


test_that("identify_problematic_values finds numeric missing codes", {
  labels_df <- data.frame(
    value = c(1, 2, 3, 99, -99, -1),
    label = c("Low", "Medium", "High", "Missing1", "Missing2", "Missing3"),
    stringsAsFactors = FALSE
  )

  result <- identify_problematic_values(labels_df)

  expect_true(all(c(99, -99, -1) %in% result$value))
})


test_that("identify_problematic_values returns NULL when no problems found", {
  labels_df <- data.frame(
    value = c(1, 2, 3),
    label = c("Low", "Medium", "High"),
    stringsAsFactors = FALSE
  )

  result <- identify_problematic_values(labels_df)

  expect_null(result)
})


test_that("identify_problematic_values returns NULL for NULL input", {
  result <- identify_problematic_values(NULL)

  expect_null(result)
})


test_that("identify_problematic_values returns NULL for empty data frame", {
  labels_df <- data.frame(
    value = numeric(),
    label = character(),
    stringsAsFactors = FALSE
  )

  result <- identify_problematic_values(labels_df)

  expect_null(result)
})


test_that("identify_problematic_values errors with invalid input", {
  bad_df <- data.frame(
    wrong_col = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    identify_problematic_values(bad_df),
    "must have columns 'value' and 'label'"
  )
})


test_that("identify_problematic_values handles custom patterns", {
  labels_df <- data.frame(
    value = c(1, 2, 999),
    label = c("Yes", "No", "Skip"),
    stringsAsFactors = FALSE
  )

  result <- identify_problematic_values(labels_df, patterns = "skip")

  expect_true(999 %in% result$value)
})


test_that("identify_problematic_values doesn't duplicate values matched by multiple patterns", {
  labels_df <- data.frame(
    value = c(1, 2, 99),
    label = c("Yes", "No", "Don't know - refused"),
    stringsAsFactors = FALSE
  )

  result <- identify_problematic_values(labels_df)

  # Should only appear once even though it matches "don't know" and "refused"
  expect_equal(sum(result$value == 99), 1)
})


# recode_to_na() ----

test_that("recode_to_na converts specified values to NA", {
  x <- c(1, 2, 3, 99, 5, 99, 7)

  result <- recode_to_na(x, values_to_na = 99, variable_name = "test_var")

  expect_equal(result$recoded, c(1, 2, 3, NA, 5, NA, 7))
  expect_s3_class(result$log, "data.frame")
})


test_that("recode_to_na creates accurate log", {
  x <- c(1, 2, 3, 99, 5, 99, 7)  # 2 values of 99

  result <- recode_to_na(x, values_to_na = 99, variable_name = "satisfaction")

  expect_equal(nrow(result$log), 1)
  expect_equal(result$log$variable, "satisfaction")
  expect_equal(result$log$value_recoded, 99)
  expect_equal(result$log$n_affected, 2)
  expect_equal(result$log$percent_affected, round(2/7 * 100, 2))
})


test_that("recode_to_na handles multiple values to recode", {
  x <- c(1, 2, 3, 99, 5, -1, 7)

  result <- recode_to_na(x, values_to_na = c(99, -1), variable_name = "test_var")

  expect_equal(result$recoded, c(1, 2, 3, NA, 5, NA, 7))
  expect_equal(nrow(result$log), 2)
  expect_true(all(c(99, -1) %in% result$log$value_recoded))
})


test_that("recode_to_na handles haven_labelled vectors", {
  skip_if_not_installed("haven")

  x <- haven::labelled(
    c(1, 2, 99, 3),
    labels = c("Low" = 1, "High" = 3, "Don't know" = 99)
  )

  result <- recode_to_na(x, values_to_na = 99, variable_name = "test_var")

  expect_equal(result$recoded, c(1, 2, NA, 3))
  expect_equal(result$log$n_affected, 1)
})


test_that("recode_to_na preserves existing NAs", {
  x <- c(1, 2, NA, 99, 5, 99, 7)

  result <- recode_to_na(x, values_to_na = 99, variable_name = "test_var")

  expect_equal(sum(is.na(result$recoded)), 3)  # 1 original + 2 recoded
})


test_that("recode_to_na calculates percentage based on originally valid cases", {
  x <- c(1, 2, NA, NA, 99, 5, 99)  # 5 originally valid, 2 are 99

  result <- recode_to_na(x, values_to_na = 99, variable_name = "test_var")

  expect_equal(result$log$percent_affected, round(2/5 * 100, 2))
})


test_that("recode_to_na returns empty log when no values matched", {
  x <- c(1, 2, 3, 4, 5)

  result <- recode_to_na(x, values_to_na = 99, variable_name = "test_var")

  expect_equal(nrow(result$log), 0)
  expect_equal(result$recoded, x)  # Unchanged
})


test_that("recode_to_na errors with non-numeric values_to_na", {
  x <- c(1, 2, 3)

  expect_error(
    recode_to_na(x, values_to_na = "bad", variable_name = "test"),
    "values_to_na must be a numeric vector"
  )
})


test_that("recode_to_na errors with empty values_to_na", {
  x <- c(1, 2, 3)

  expect_error(
    recode_to_na(x, values_to_na = numeric(0), variable_name = "test"),
    "at least one value"
  )
})


test_that("recode_to_na errors with non-numeric input", {
  x <- c("a", "b", "c")

  expect_error(
    recode_to_na(x, values_to_na = 99, variable_name = "test"),
    "must be numeric, factor, or haven_labelled"
  )
})


test_that("recode_to_na handles factors with numeric levels", {
  x <- factor(c("1", "2", "99", "3"))

  result <- recode_to_na(x, values_to_na = 99, variable_name = "test_var")

  expect_equal(result$recoded, c(1, 2, NA, 3))
  expect_equal(result$log$n_affected, 1)
})


test_that("recode_to_na errors with factors that can't convert to numeric", {
  x <- factor(c("Low", "High", "Medium"))

  expect_error(
    recode_to_na(x, values_to_na = 99, variable_name = "test"),
    "Cannot recode factor"
  )
})


# Edge cases ----

test_that("labelled data helpers handle edge cases gracefully", {
  # All NA (must be numeric NA)
  x_all_na <- c(NA_real_, NA_real_, NA_real_)
  result <- recode_to_na(x_all_na, values_to_na = 99, variable_name = "all_na")
  expect_true(all(is.na(result$recoded)))
  expect_equal(nrow(result$log), 0)

  # Single value
  x_single <- 99
  result <- recode_to_na(x_single, values_to_na = 99, variable_name = "single")
  expect_true(is.na(result$recoded))
  expect_equal(result$log$n_affected, 1)
})


test_that("extract_value_labels handles haven_labelled with no labels attribute", {
  skip_if_not_installed("haven")

  # Create labelled vector then strip labels
  x <- haven::labelled(c(1, 2, 3))

  result <- extract_value_labels(x)

  expect_null(result)
})
