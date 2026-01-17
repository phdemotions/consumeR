# Frozen regression baselines for refactoring


test_that("calculate_summary_stats produces identical output after refactoring (FROZEN BASELINE)", {
  # Frozen test data
  test_data <- 1:10

  # Execute
  result <- calculate_summary_stats(test_data)

  # Assert: These values are FROZEN from pre-refactoring output
  expect_equal(result$n, 10)
  expect_equal(result$n_missing, 0)
  expect_equal(result$mean, 5.5)
  expect_equal(result$median, 5.5)
  expect_equal(result$sd, 3.03)
  expect_equal(result$min, 1)
  expect_equal(result$max, 10)
  expect_equal(result$q25, 3.25)
  expect_equal(result$q75, 7.75)
  expect_equal(result$variance, 9.17)
  expect_equal(result$range, 9)
  expect_equal(result$iqr, 4.5)

  # Verify class
  expect_s3_class(result, "summary_stats")
})

test_that("compare_two_groups produces identical output after refactoring (FROZEN BASELINE)", {
  # Frozen test data
  group1 <- c(45.2, 67.8, 23.4, 89.1, 34.5, 56.7, 78.9, 12.3)
  group2 <- c(34.1, 45.2, 28.9, 56.3, 41.2, 38.7, 49.1, 31.4)

  # Execute (fallback to current implementation if compare_two_groups is absent)
  compare_two_groups_fn <- if (exists("compare_two_groups")) {
    compare_two_groups
  } else {
    test_group_differences
  }

  result <- compare_two_groups_fn(
    group1,
    group2,
    test_type = "t.test",
    check_assumptions = FALSE,
    verbose = FALSE
  )

  # Assert: These values are FROZEN from pre-refactoring output
  expect_equal(result$test_used, "Student's t-test")
  expect_equal(result$p_value, 0.321947)
  expect_equal(as.numeric(result$statistic), 1.0267625321, tolerance = 1e-8)
  expect_equal(as.numeric(result$df), 14)
  expect_false(result$significant)
  expect_equal(result$group1_mean, 50.99)
  expect_equal(result$group2_mean, 40.61)
  expect_equal(result$group1_sd, 27.02)
  expect_equal(result$group2_sd, 9.32)
  expect_equal(result$difference, 10.38)
  expect_equal(result$ci_lower, -11.3)
  expect_equal(result$ci_upper, 32.05)
  expect_equal(result$effect_size, 0.51)
  expect_equal(result$effect_interpretation, "medium")
  expect_equal(result$group1_n, 8)
  expect_equal(result$group2_n, 8)
  expect_equal(
    result$interpretation,
    paste0(
      "No significant difference detected between groups (p = 0.3219). ",
      "Group 1: M = 50.99, SD = 27.02; ",
      "Group 2: M = 40.61, SD = 9.32. ",
      "The effect size is medium (Cohen's d = 0.51)."
    )
  )

  # Publication text (frozen)
  expect_s3_class(result$publication_text, "publication_block")
  expect_equal(
    result$publication_text$methods,
    paste0(
      "An independent samples t-test was conducted to compare means between groups. ",
      "The test assumes normality of distributions and homogeneity of variance."
    )
  )
  expect_equal(
    result$publication_text$results,
    paste0(
      "The independent samples t-test revealed no statistically significant difference ",
      "between groups (t(14) = 1.03, p = .322). ",
      "Group 1 (M = 50.99, SD = 27.02) exceeded Group 2 (M = 40.61, SD = 9.32), ",
      "95% CI [-11.3, 32.05], Cohen's d = 0.51 (medium effect size)."
    )
  )
  expect_equal(
    result$publication_text$interpretation,
    paste0(
      "These results do not provide evidence of a statistically significant effect. ",
      "However, absence of evidence is not evidence of absence; the study may be ",
      "underpowered to detect smaller effects."
    )
  )

  # Verify class
  expect_s3_class(result, "group_comparison")
  expect_s3_class(result$full_test_output, "htest")
})

test_that("analyze_correlation produces identical output after refactoring (FROZEN BASELINE)", {
  # Frozen test data
  x_data <- c(23, 45, 67, 34, 56, 78, 89, 12, 45, 67)
  y_data <- c(34, 56, 78, 45, 67, 89, 90, 23, 56, 78)

  # Execute
  result <- analyze_correlation(
    x_data,
    var2 = y_data,
    method = "pearson",
    check_assumptions = FALSE,
    verbose = FALSE
  )

  # Assert: These values are FROZEN from pre-refactoring output
  expect_equal(result$method, "Pearson")
  expect_equal(result$correlation, 0.9932)
  expect_equal(result$p_value, 0)
  expect_true(result$significant)
  expect_equal(result$alpha, 0.05)
  expect_equal(result$n, 10)
  expect_equal(result$ci_lower, 0.9704)
  expect_equal(result$ci_upper, 0.9984)
  expect_equal(result$r_squared, 0.9864)
  expect_equal(result$strength, "very strong")
  expect_equal(result$direction, "positive")
  expect_equal(result$var1_name, "x_data")
  expect_equal(result$var2_name, "y_data")
  expect_equal(
    result$interpretation,
    paste0(
      "The Pearson correlation between x_data and y_data is very strong and positive ",
      "(r = 0.993, n = 10, p = 0). ",
      "This correlation is statistically significant at alpha = 0.05. ",
      "The very strong relationship suggests meaningful covariation between the variables. ",
      "Approximately 98.6% of variance in one variable is associated with variance in the other."
    )
  )

  # Publication text (frozen)
  expect_s3_class(result$publication_text, "publication_block")
  expect_equal(
    result$publication_text$methods,
    "Correlation analysis was conducted to examine relationships between variables."
  )
  expect_equal(
    result$publication_text$results,
    "Pearson's correlation analysis revealed a statistically significant relationship (r = 0.99, n = 10, p < .001)."
  )
  expect_equal(
    result$publication_text$interpretation,
    "These results provide evidence of a statistically significant effect."
  )
  expect_equal(
    result$publication_text$additional,
    paste0(
      "Correlation strength was interpreted using Cohen's (1988) guidelines. ",
      "The correlation coefficient of 0.993 indicates that the variables share ",
      "approximately 98.6% common variance (r^2 = 0.986)."
    )
  )

  # Verify class
  expect_s3_class(result, "correlation_result")
  expect_s3_class(result$cor_test_output, "htest")
})
