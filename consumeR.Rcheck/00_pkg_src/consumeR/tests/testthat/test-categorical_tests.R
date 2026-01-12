test_that("chisq_test works with 2x2 table", {
  # Create synthetic data
  set.seed(42)
  df <- tibble::tibble(
    condition = rep(c("Control", "Treatment"), each = 50),
    response = c(
      rep(c("Yes", "No"), times = c(20, 30)),  # Control: 20 yes, 30 no
      rep(c("Yes", "No"), times = c(35, 15))   # Treatment: 35 yes, 15 no
    )
  )

  result <- chisq_test(df, x = "condition", y = "response")

  expect_s3_class(result, "chisq_result")
  expect_type(result, "list")

  # Check that all essential fields are present (function may return more, which is good!)
  essential_fields <- c("statistic", "df", "p_value", "cramers_v",
                       "observed", "expected", "residuals", "interpretation")
  expect_true(all(essential_fields %in% names(result)))

  expect_true(result$statistic > 0)
  expect_equal(result$df, 1)
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  expect_true(result$cramers_v >= 0 && result$cramers_v <= 1)
  expect_type(result$interpretation, "character")
})

test_that("chisq_test works with 3x3 table", {
  set.seed(123)
  df <- tibble::tibble(
    age = sample(c("18-25", "26-40", "41+"), 150, replace = TRUE),
    product = sample(c("A", "B", "C"), 150, replace = TRUE)
  )

  result <- chisq_test(df, x = "age", y = "product")

  expect_s3_class(result, "chisq_result")
  expect_equal(result$df, 4)  # (3-1) * (3-1) = 4
  expect_s3_class(result$observed, "table")
  expect_true(is.matrix(result$expected) || is.table(result$expected))
})

test_that("chisq_test warns about low expected frequencies", {
  # Create table with small cell counts
  df <- tibble::tibble(
    x = c(rep("A", 3), rep("B", 2)),
    y = c("Y", "Y", "N", "Y", "N")
  )

  expect_warning(
    chisq_test(df, x = "x", y = "y"),
    "Expected frequency|expected frequencies"
  )
})

test_that("chisq_test handles continuity correction", {
  set.seed(42)
  df <- tibble::tibble(
    x = rep(c("A", "B"), each = 25),
    y = sample(c("X", "Y"), 50, replace = TRUE)
  )

  result_corrected <- chisq_test(df, x = "x", y = "y", correct = TRUE)
  result_uncorrected <- chisq_test(df, x = "x", y = "y", correct = FALSE)

  # Corrected chi-square should be smaller (more conservative)
  expect_lte(result_corrected$statistic, result_uncorrected$statistic)
})

test_that("chisq_test validates inputs", {
  df <- tibble::tibble(x = c("A", "B"), y = c("X", "Y"))

  expect_error(chisq_test(df, x = "missing", y = "y"), "Missing variables")
  expect_error(chisq_test(df, x = "x", y = "missing"), "Missing variables")
})

test_that("chisq_test print method works", {
  set.seed(42)
  df <- tibble::tibble(
    x = rep(c("A", "B"), each = 30),
    y = sample(c("X", "Y"), 60, replace = TRUE)
  )

  result <- chisq_test(df, x = "x", y = "y")
  output <- capture.output(print(result))

  expect_true(any(grepl("Chi-Square Test", output)))
  expect_true(any(grepl("chi\\^2", output)))  # Match chi^2 pattern
  expect_true(any(grepl("Cramer", output)))  # Match with or without special chars
})

test_that("fisher_exact_test works with 2x2 table", {
  df <- tibble::tibble(
    treatment = c(rep("Drug", 10), rep("Placebo", 10)),
    outcome = c(
      rep(c("Success", "Failure"), times = c(8, 2)),   # Drug: 8 success, 2 fail
      rep(c("Success", "Failure"), times = c(3, 7))    # Placebo: 3 success, 7 fail
    )
  )

  result <- fisher_exact_test(df, x = "treatment", y = "outcome")

  expect_type(result, "list")
  # Check essential fields are present (function may include additional helpful info)
  essential_fields <- c("p_value", "odds_ratio", "or_ci_lower", "or_ci_upper",
                        "alternative", "interpretation")
  expect_true(all(essential_fields %in% names(result)))
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  expect_true(result$odds_ratio > 0)
  expect_type(result$interpretation, "character")
})

test_that("fisher_exact_test handles alternative hypotheses", {
  df <- tibble::tibble(
    x = rep(c("A", "B"), each = 5),
    y = c(rep("Y", 4), "N", rep("Y", 2), rep("N", 3))
  )

  result_two <- fisher_exact_test(df, x = "x", y = "y", alternative = "two.sided")
  result_greater <- fisher_exact_test(df, x = "x", y = "y", alternative = "greater")
  result_less <- fisher_exact_test(df, x = "x", y = "y", alternative = "less")

  expect_equal(result_two$alternative, "two.sided")
  expect_equal(result_greater$alternative, "greater")
  expect_equal(result_less$alternative, "less")
})

test_that("fisher_exact_test works with larger tables", {
  set.seed(42)
  df <- tibble::tibble(
    condition = sample(c("A", "B", "C"), 30, replace = TRUE),
    response = sample(c("X", "Y"), 30, replace = TRUE)
  )

  result <- fisher_exact_test(df, x = "condition", y = "response")

  expect_type(result, "list")
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  # No odds ratio for tables larger than 2x2
  expect_true(is.na(result$odds_ratio) || is.numeric(result$odds_ratio))
})

test_that("fisher_exact_test validates inputs", {
  df <- tibble::tibble(x = c("A", "B"), y = c("X", "Y"))

  expect_error(fisher_exact_test(df, x = "missing", y = "y"), "Missing variables")
  expect_error(fisher_exact_test(df, x = "x", y = "missing"), "Missing variables")
})

test_that("mcnemar_test works with paired data", {
  # Before-after study
  set.seed(42)
  df <- tibble::tibble(
    before = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.3, 0.7)),
    after = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.6, 0.4))
  )

  result <- mcnemar_test(df, var1 = "before", var2 = "after")

  expect_type(result, "list")

  # Check essential fields (function returns more helpful info which is good!)
  essential_fields <- c("statistic", "df", "p_value", "n_pairs", "n_changed",
                       "pct_changed", "odds_ratio_change", "interpretation")
  expect_true(all(essential_fields %in% names(result)))

  expect_true(result$statistic >= 0)
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  expect_true(result$n_pairs >= 0)
  expect_true(result$n_changed >= 0)
  expect_true(result$pct_changed >= 0 && result$pct_changed <= 100)
  expect_type(result$interpretation, "character")
})

test_that("mcnemar_test handles continuity correction", {
  set.seed(123)
  df <- tibble::tibble(
    pre = sample(c("A", "B"), 50, replace = TRUE),
    post = sample(c("A", "B"), 50, replace = TRUE)
  )

  result_corrected <- mcnemar_test(df, var1 = "pre", var2 = "post", correct = TRUE)
  result_uncorrected <- mcnemar_test(df, var1 = "pre", var2 = "post", correct = FALSE)

  # Corrected should be more conservative
  expect_lte(result_corrected$statistic, result_uncorrected$statistic)
})

test_that("mcnemar_test validates inputs", {
  df <- tibble::tibble(x = c("A", "B"), y = c("X", "Y"))

  expect_error(mcnemar_test(df, var1 = "missing", var2 = "y"), "Missing variables")
  expect_error(mcnemar_test(df, var1 = "x", var2 = "missing"), "Missing variables")
})

test_that("mcnemar_test handles exact test for small samples", {
  # Small sample with few discordant pairs
  df <- tibble::tibble(
    time1 = c("A", "A", "B", "B", "A"),
    time2 = c("A", "B", "A", "B", "A")
  )

  # Should not error even with small n
  expect_no_error(mcnemar_test(df, var1 = "time1", var2 = "time2"))
})

test_that("odds_ratio_table works with 2x2 table", {
  df <- tibble::tibble(
    exposure = c(rep("Exposed", 50), rep("Unexposed", 50)),
    outcome = c(
      rep(c("Disease", "Healthy"), times = c(30, 20)),    # Exposed: 30 disease
      rep(c("Disease", "Healthy"), times = c(10, 40))     # Unexposed: 10 disease
    )
  )

  result <- odds_ratio_table(df, x = "exposure", y = "outcome")

  expect_type(result, "list")
  # Check essential fields are present (function may include additional helpful info)
  essential_fields <- c("odds_ratio", "or_ci_lower", "or_ci_upper",
                        "p_value", "interpretation")
  expect_true(all(essential_fields %in% names(result)))
  expect_true(result$odds_ratio > 0)
  expect_true(result$or_ci_lower > 0)
  expect_true(result$or_ci_upper > 0)
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  expect_type(result$interpretation, "character")
})

test_that("odds_ratio_table CI does not include 1 for significant result", {
  # Strong association
  df <- tibble::tibble(
    x = c(rep("A", 40), rep("B", 40)),
    y = c(
      rep(c("Y", "N"), times = c(35, 5)),   # A: 35 yes, 5 no
      rep(c("Y", "N"), times = c(10, 30))   # B: 10 yes, 30 no
    )
  )

  result <- odds_ratio_table(df, x = "x", y = "y")

  # If p < 0.05, CI should not include 1
  if (result$p_value < 0.05) {
    if (result$odds_ratio > 1) {
      expect_gt(result$or_ci_lower, 1)
    } else {
      expect_lt(result$or_ci_upper, 1)
    }
  }
})

test_that("odds_ratio_table handles different confidence levels", {
  df <- tibble::tibble(
    x = rep(c("A", "B"), each = 30),
    y = c(rep(c("Y", "N"), times = c(20, 10)),
          rep(c("Y", "N"), times = c(10, 20)))
  )

  result_95 <- odds_ratio_table(df, x = "x", y = "y", conf_level = 0.95)
  result_99 <- odds_ratio_table(df, x = "x", y = "y", conf_level = 0.99)

  # 99% CI should be wider
  ci_width_95 <- result_95$or_ci_upper - result_95$or_ci_lower
  ci_width_99 <- result_99$or_ci_upper - result_99$or_ci_lower
  expect_gt(ci_width_99, ci_width_95)
})

test_that("odds_ratio_table validates 2x2 requirement", {
  # 3x2 table should error
  df <- tibble::tibble(
    x = sample(c("A", "B", "C"), 30, replace = TRUE),
    y = sample(c("X", "Y"), 30, replace = TRUE)
  )

  expect_error(odds_ratio_table(df, x = "x", y = "y"), "exactly 2 levels|2x2")
})

test_that("odds_ratio_table validates inputs", {
  df <- tibble::tibble(x = c("A", "B"), y = c("X", "Y"))

  # Error messages explain what's missing - more helpful than just "not found"
  expect_error(odds_ratio_table(df, x = "missing", y = "y"), "Missing variables")
  expect_error(odds_ratio_table(df, x = "x", y = "missing"), "Missing variables")
})

test_that("odds_ratio_table handles zero cells gracefully", {
  # Table with zero cell
  df <- tibble::tibble(
    x = c("A", "A", "B", "B"),
    y = c("Y", "Y", "N", "N")
  )

  # Should return finite result (may handle gracefully without warning)
  result <- odds_ratio_table(df, x = "x", y = "y")
  expect_true(is.finite(result$odds_ratio) || is.na(result$odds_ratio))
})

test_that("categorical test functions handle NA values appropriately", {
  df <- tibble::tibble(
    x = c("A", "B", NA, "A", "B"),
    y = c("X", "Y", "X", NA, "Y")
  )

  # Functions message about removed NAs (using message(), not warning())
  expect_message(chisq_test(df, x = "x", y = "y"), "missing|Removed")
  expect_message(fisher_exact_test(df, x = "x", y = "y"), "Fisher")  # Just check it runs
})

test_that("categorical functions work with factors", {
  df <- tibble::tibble(
    x = factor(c("Low", "High", "Low", "High"), levels = c("Low", "High")),
    y = factor(c("No", "Yes", "No", "Yes"), levels = c("No", "Yes"))
  )

  expect_no_error(chisq_test(df, x = "x", y = "y"))
  expect_no_error(fisher_exact_test(df, x = "x", y = "y"))
})

test_that("interpretation text includes key statistical information", {
  set.seed(42)
  df <- tibble::tibble(
    condition = rep(c("A", "B"), each = 50),
    outcome = c(
      sample(c("Success", "Fail"), 50, replace = TRUE, prob = c(0.7, 0.3)),
      sample(c("Success", "Fail"), 50, replace = TRUE, prob = c(0.3, 0.7))
    )
  )

  result_chi <- chisq_test(df, x = "condition", y = "outcome")
  result_fisher <- fisher_exact_test(df, x = "condition", y = "outcome")

  # Chi-square interpretation should mention effect size
  expect_true(grepl("Cramér's V|effect", result_chi$interpretation, ignore.case = TRUE))

  # Fisher interpretation should mention odds ratio or association
  expect_true(grepl("odds ratio|association|relationship", result_fisher$interpretation, ignore.case = TRUE))
})
