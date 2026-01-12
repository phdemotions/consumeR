# Tests for create_analysis_report function
# These tests ensure reporting functionality works correctly

test_that("create_analysis_report works with vector input", {
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  # Should not error
  expect_output(
    result <- create_analysis_report(data, title = "Test Report"),
    "Test Report"
  )

  # Should return a list invisibly
  expect_type(result, "list")
})

test_that("create_analysis_report works with data frame", {
  df <- data.frame(
    value = c(1, 2, 3, 4, 5),
    other = c(6, 7, 8, 9, 10)
  )

  expect_output(
    result <- create_analysis_report(df, variable = "value"),
    "value"
  )
})

test_that("create_analysis_report requires variable for data frames", {
  df <- data.frame(
    value = c(1, 2, 3, 4, 5)
  )

  expect_error(
    create_analysis_report(df),
    "must specify 'variable'"
  )
})

test_that("create_analysis_report validates variable exists", {
  df <- data.frame(
    value = c(1, 2, 3, 4, 5)
  )

  expect_error(
    create_analysis_report(df, variable = "nonexistent"),
    "not found in data"
  )
})

test_that("create_analysis_report includes all sections", {
  data <- c(1, 2, 3, 4, 5)

  output <- capture.output(
    result <- create_analysis_report(data)
  )

  output_text <- paste(output, collapse = "\n")

  # Check for main sections
  expect_true(grepl("DATA OVERVIEW", output_text))
  expect_true(grepl("DESCRIPTIVE STATISTICS", output_text))
  expect_true(grepl("METHODOLOGICAL NOTES", output_text))
})

test_that("create_analysis_report handles group comparisons", {
  df <- data.frame(
    value = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    group = c(rep("A", 5), rep("B", 5))
  )

  output <- capture.output(
    result <- create_analysis_report(df, variable = "value",
                                    group_var = "group")
  )

  output_text <- paste(output, collapse = "\n")

  # Should include group comparison section
  expect_true(grepl("GROUP COMPARISON", output_text))
})

test_that("create_analysis_report validates grouping variable", {
  df <- data.frame(
    value = c(1, 2, 3, 4, 5),
    group = c("A", "A", "B", "B", "B")
  )

  expect_error(
    create_analysis_report(df, variable = "value",
                          group_var = "nonexistent"),
    "not found in data"
  )
})

test_that("create_analysis_report handles wrong number of groups", {
  # More than 2 groups
  df <- data.frame(
    value = c(1, 2, 3, 4, 5, 6),
    group = c("A", "A", "B", "B", "C", "C")
  )

  output <- capture.output(
    result <- create_analysis_report(df, variable = "value",
                                    group_var = "group")
  )

  output_text <- paste(output, collapse = "\n")

  # Should warn about wrong number of groups
  expect_true(grepl("exactly 2 groups", output_text))
})

test_that("create_analysis_report returns correct structure", {
  data <- c(1, 2, 3, 4, 5)

  result <- create_analysis_report(data)

  expect_type(result, "list")
  expect_named(result, c("data_summary", "descriptive_stats",
                        "group_comparison", "report_text"))
})

test_that("create_analysis_report handles missing values", {
  data <- c(1, 2, NA, 4, 5)

  output <- capture.output(
    result <- create_analysis_report(data)
  )

  output_text <- paste(output, collapse = "\n")

  # Should report missing values
  expect_true(grepl("Missing values: 1", output_text))
})

test_that("create_analysis_report includes report date", {
  data <- c(1, 2, 3, 4, 5)

  output <- capture.output(
    result <- create_analysis_report(data)
  )

  output_text <- paste(output, collapse = "\n")

  # Should include current date
  expect_true(grepl("Generated on:", output_text))
})

test_that("create_analysis_report includes R version", {
  data <- c(1, 2, 3, 4, 5)

  output <- capture.output(
    result <- create_analysis_report(data)
  )

  output_text <- paste(output, collapse = "\n")

  # Should include R version in methodological notes
  expect_true(grepl("R version", output_text))
})

test_that("create_analysis_report can save to file", {
  data <- c(1, 2, 3, 4, 5)
  temp_file <- tempfile(fileext = ".txt")

  expect_message(
    create_analysis_report(data, report_file = temp_file),
    "Report saved"
  )

  # Check file was created
  expect_true(file.exists(temp_file))

  # Check file has content
  file_content <- readLines(temp_file)
  expect_true(length(file_content) > 0)

  # Clean up
  unlink(temp_file)
})

test_that("create_analysis_report custom title works", {
  data <- c(1, 2, 3, 4, 5)
  custom_title <- "My Custom Analysis"

  output <- capture.output(
    result <- create_analysis_report(data, title = custom_title)
  )

  output_text <- paste(output, collapse = "\n")

  expect_true(grepl(custom_title, output_text))
})
