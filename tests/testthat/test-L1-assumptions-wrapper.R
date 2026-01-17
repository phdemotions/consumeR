# ============================================================================== 
# LEVEL 1: ASSUMPTION WRAPPER TESTS
# ==============================================================================

library(testthat)

make_test_data <- function() {
  set.seed(123)
  data.frame(
    group = rep(c("A", "B"), each = 50),
    value = c(rnorm(50, mean = 10, sd = 1), rnorm(50, mean = 12, sd = 1))
  )
}

test_that("check_test_assumptions() handles all test types", {
  data <- make_test_data()
  model <- lm(value ~ group, data = data)

  t_test_result <- check_test_assumptions(
    test_type = "t_test",
    data = data,
    groups = "group",
    residuals = residuals(model)
  )

  anova_result <- check_test_assumptions(
    test_type = "anova",
    data = data,
    groups = "group",
    residuals = residuals(model)
  )

  regression_data <- data.frame(
    outcome = rnorm(100),
    predictor = rnorm(100)
  )
  regression_model <- lm(outcome ~ predictor, data = regression_data)
  regression_result <- check_test_assumptions(
    test_type = "regression",
    data = regression_data,
    residuals = residuals(regression_model)
  )

  correlation_result <- check_test_assumptions(
    test_type = "correlation",
    data = data
  )

  expect_s3_class(t_test_result, "assumptions_result")
  expect_s3_class(anova_result, "assumptions_result")
  expect_s3_class(regression_result, "assumptions_result")
  expect_s3_class(correlation_result, "assumptions_result")

  expect_equal(t_test_result$test_type, "t_test")
  expect_equal(anova_result$test_type, "anova")
  expect_equal(regression_result$test_type, "regression")
  expect_equal(correlation_result$test_type, "correlation")

  expect_false(is.null(t_test_result$independence))
  expect_false(is.null(t_test_result$normality))
  expect_false(is.null(t_test_result$homogeneity))

  expect_false(is.null(regression_result$independence))
  expect_false(is.null(regression_result$normality))
  expect_null(regression_result$homogeneity)

  expect_false(is.null(correlation_result$independence))
  expect_null(correlation_result$normality)
  expect_null(correlation_result$homogeneity)
})

test_that("check_test_assumptions() validates required arguments", {
  data <- make_test_data()

  expect_error(
    check_test_assumptions(test_type = "t_test", data = data, groups = "group"),
    class = "missing_residuals_error"
  )

  expect_error(
    check_test_assumptions(test_type = "anova", data = data, residuals = rnorm(100)),
    class = "missing_groups_error"
  )
})

test_that("check_test_assumptions() returns expected structure", {
  data <- make_test_data()
  model <- lm(value ~ group, data = data)

  result <- check_test_assumptions(
    test_type = "t_test",
    data = data,
    groups = "group",
    residuals = residuals(model)
  )

  expect_named(
    result,
    c(
      "independence",
      "normality",
      "homogeneity",
      "multicollinearity",
      "overall_pass",
      "test_type",
      "warnings"
    )
  )
  expect_type(result$overall_pass, "logical")
  expect_type(result$warnings, "character")
})

test_that("check_test_assumptions() reports warnings when assumptions fail", {
  set.seed(42)
  data <- data.frame(
    group = rep(c("A", "B"), each = 50),
    value = 1:100
  )

  result <- NULL
  old_options <- options(cli.unicode = FALSE)
  on.exit(options(old_options), add = TRUE)

  expect_output(
    result <- check_test_assumptions(
      test_type = "t_test",
      data = data,
      groups = "group",
      residuals = rexp(100),
      verbose = TRUE
    ),
    "Assumption checks"
  )

  expect_true(length(result$warnings) > 0)
  expect_match(result$warnings, "Normality assumption may be violated")
  expect_match(result$warnings, "Independence assumption may be violated")
})
