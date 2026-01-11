test_that("package loads without errors", {
  expect_error(library(consumeR), NA)
})

test_that("citation file exists and is readable", {
  # Check that inst/CITATION file exists
  citation_path <- system.file("CITATION", package = "consumeR")
  expect_true(file.exists(citation_path) || nchar(citation_path) > 0,
              info = "CITATION file should exist in inst/")

  # Check that citation() returns something
  cit <- citation("consumeR")
  expect_s3_class(cit, "bibentry")
  expect_true(length(cit) > 0)
})

test_that("key exported functions exist and have correct structure", {
  # Test a few representative functions
  expect_true(exists("calculate_summary_stats", where = "package:consumeR"))
  expect_true(exists("test_group_differences", where = "package:consumeR"))
  expect_true(exists("clean_survey_data", where = "package:consumeR"))

  # These should be functions
  expect_true(is.function(calculate_summary_stats))
  expect_true(is.function(test_group_differences))
  expect_true(is.function(clean_survey_data))
})

test_that("example data loads correctly", {
  # Load the consumer_survey data
  data(consumer_survey, package = "consumeR")

  # Check it exists and has expected structure
  expect_true(exists("consumer_survey"))
  expect_s3_class(consumer_survey, "data.frame")
  expect_gt(nrow(consumer_survey), 0)
  expect_true("flyer_group" %in% names(consumer_survey))
  expect_true("spending" %in% names(consumer_survey))
})

test_that("basic workflow executes without error", {
  # Load example data
  data(consumer_survey, package = "consumeR")

  # Test summary stats
  expect_error({
    stats <- calculate_summary_stats(consumer_survey$spending)
  }, NA)

  # Test group comparison
  expect_error({
    library(dplyr)
    flyer <- consumer_survey %>%
      filter(flyer_group == "Got Flyer") %>%
      pull(spending)
    no_flyer <- consumer_survey %>%
      filter(flyer_group == "No Flyer") %>%
      pull(spending)

    result <- test_group_differences(flyer, no_flyer)
  }, NA)
})
