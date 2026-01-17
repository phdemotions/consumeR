# Benchmark tests for refactoring performance

test_that("calculate_summary_stats performance not regressed", {
  skip_on_cran()
  set.seed(123)

  test_data <- rnorm(10000)

  time <- system.time({
    result <- calculate_summary_stats(test_data)
  })

  expect_lt(time["elapsed"], 1.0)
  expect_true(is.list(result))
})

test_that("compare_two_groups performance not regressed", {
  skip_on_cran()
  set.seed(123)

  test_data <- data.frame(
    group = rep(c("A", "B"), each = 5000),
    value = rnorm(10000)
  )

  time <- system.time({
    result <- test_group_differences(
      test_data$value[test_data$group == "A"],
      test_data$value[test_data$group == "B"],
      check_assumptions = FALSE,
      verbose = FALSE
    )
  })

  expect_lt(time["elapsed"], 2.0)
  expect_true(is.list(result))
})

test_that("analyze_correlation performance not regressed", {
  skip_on_cran()
  set.seed(123)

  x <- rnorm(10000)
  y <- rnorm(10000)

  time <- system.time({
    result <- analyze_correlation(x, var2 = y, check_assumptions = FALSE, verbose = FALSE)
  })

  expect_lt(time["elapsed"], 2.0)
  expect_true(is.list(result))
})
