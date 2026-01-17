# Tests for standardized analysis result builder

build_result_fixture <- function(...) {
  build_analysis_result(
    test_type = "t_test",
    test_name = "Independent Samples T-Test",
    core_stats = list(p_value = 0.042, n = 100),
    specific_stats = list(t_statistic = 2.05, df = 98),
    interpretation = "Groups differ significantly",
    ...
  )
}

test_that("build_analysis_result validates required core stats", {
  expect_error(
    build_analysis_result(
      test_type = "t_test",
      test_name = "Independent Samples T-Test",
      core_stats = list(n = 100),
      specific_stats = list(t_statistic = 2.05)
    ),
    class = "missing_p_value_error"
  )

  expect_error(
    build_analysis_result(
      test_type = "t_test",
      test_name = "Independent Samples T-Test",
      core_stats = list(p_value = 0.042),
      specific_stats = list(t_statistic = 2.05)
    ),
    class = "missing_n_error"
  )
})

test_that("build_analysis_result auto-computes significance", {
  result <- build_analysis_result(
    test_type = "t_test",
    test_name = "Independent Samples T-Test",
    core_stats = list(p_value = 0.2, n = 100),
    specific_stats = list(t_statistic = 1.12, df = 98)
  )

  expect_false(result$significant)
})

test_that("build_analysis_result assigns expected classes", {
  result <- build_result_fixture()

  expect_s3_class(result, "analysis_result")
  expect_s3_class(result, "t_test_result")
})

test_that("print.analysis_result formats core sections", {
  result <- build_result_fixture()

  output <- capture.output(print(result))

  expect_true(any(grepl("SUMMARY", output)))
  expect_true(any(grepl("TEST STATISTICS", output)))
  expect_true(any(grepl("Sample size: N = 100", output)))
  expect_true(any(grepl("T statistic: 2.05", output)))
})

test_that("print.analysis_result shows optional sections", {
  result <- build_result_fixture(
    assumptions = list(
      overall_pass = FALSE,
      warnings = c("Normality", "Equal variances")
    ),
    publication_text = "t(98) = 2.05, p = .042"
  )

  output <- capture.output(
    print(result, show_assumptions = TRUE, show_publication = TRUE)
  )

  expect_true(any(grepl("ASSUMPTION CHECKS", output)))
  expect_true(any(grepl("Normality", output)))
  expect_true(any(grepl("PUBLICATION TEXT", output)))
  expect_true(any(grepl("t\\(98\\) = 2.05, p = \\.042", output)))
})
