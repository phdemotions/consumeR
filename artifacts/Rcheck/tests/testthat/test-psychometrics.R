# Tests for psychometrics functions

# Create synthetic test data
set.seed(123)
n <- 100

# Generate correlated items for factor analysis
# Factor 1 items (q1-q3)
factor1 <- rnorm(n)
q1 <- factor1 + rnorm(n, 0, 0.5)
q2 <- factor1 + rnorm(n, 0, 0.5)
q3 <- factor1 + rnorm(n, 0, 0.5)

# Factor 2 items (q4-q6)
factor2 <- rnorm(n)
q4 <- factor2 + rnorm(n, 0, 0.5)
q5 <- factor2 + rnorm(n, 0, 0.5)
q6 <- factor2 + rnorm(n, 0, 0.5)

# Scale to 1-7 range
scale_to_likert <- function(x) {
  round(pmin(pmax((x - min(x)) / (max(x) - min(x)) * 6 + 1, 1), 7))
}

test_data <- data.frame(
  id = 1:n,
  q1 = scale_to_likert(q1),
  q2 = scale_to_likert(q2),
  q3 = scale_to_likert(q3),
  q4 = scale_to_likert(q4),
  q5 = scale_to_likert(q5),
  q6 = scale_to_likert(q6)
)


# efa_diagnostics tests ----

test_that("efa_diagnostics works with data frame input", {
  skip_if_not_installed("psych")

  items <- test_data[, c("q1", "q2", "q3", "q4", "q5", "q6")]

  result <- efa_diagnostics(items)

  expect_s3_class(result, "efa_diagnostics")
  expect_named(result, c("kmo", "bartlett", "parallel", "n_obs", "n_items", "suitable", "interpretation"))

  # KMO
  expect_true(is.numeric(result$kmo$overall_msa))
  expect_true(result$kmo$overall_msa >= 0 && result$kmo$overall_msa <= 1)

  # Bartlett
  expect_true(is.numeric(result$bartlett$chisq))
  expect_true(is.numeric(result$bartlett$p_value))
  expect_true(is.logical(result$bartlett$suitable))

  # Parallel analysis
  expect_true(is.numeric(result$parallel$nfact))
  expect_true(is.numeric(result$parallel$ncomp))

  # Meta
  expect_equal(result$n_obs, nrow(items))
  expect_equal(result$n_items, ncol(items))
  expect_true(is.logical(result$suitable))
  expect_true(is.character(result$interpretation))
})

test_that("efa_diagnostics works with correlation matrix", {
  skip_if_not_installed("psych")

  items <- test_data[, c("q1", "q2", "q3", "q4", "q5", "q6")]
  cor_mat <- cor(items)

  result <- efa_diagnostics(cor_mat, n_obs = nrow(items))

  expect_s3_class(result, "efa_diagnostics")
  expect_equal(result$n_obs, nrow(items))
  expect_equal(result$n_items, 6)
})

test_that("efa_diagnostics requires n_obs for correlation matrix", {
  skip_if_not_installed("psych")

  cor_mat <- cor(test_data[, c("q1", "q2", "q3")])

  expect_error(
    efa_diagnostics(cor_mat),
    "n_obs must be specified"
  )
})

test_that("efa_diagnostics validates numeric columns", {
  skip_if_not_installed("psych")

  bad_data <- data.frame(
    q1 = c(1, 2, 3),
    q2 = c("a", "b", "c")
  )

  expect_error(
    efa_diagnostics(bad_data),
    "All items must be numeric"
  )
})


# run_cfa tests ----

test_that("run_cfa works with simple model", {
  skip_if_not_installed("lavaan")

  model <- "
    factor1 =~ q1 + q2 + q3
    factor2 =~ q4 + q5 + q6
  "

  fit <- run_cfa(model, data = test_data)

  expect_s4_class(fit, "lavaan")
})

test_that("run_cfa validates inputs", {
  skip_if_not_installed("lavaan")

  expect_error(run_cfa(123, test_data), "model must be a character string")
  expect_error(run_cfa("factor =~ q1", "not a df"), "data must be a data frame")
})


# tidy_cfa_fit tests ----

test_that("tidy_cfa_fit extracts fit indices", {
  skip_if_not_installed("lavaan")

  model <- "factor1 =~ q1 + q2 + q3"
  fit <- run_cfa(model, data = test_data)

  result <- tidy_cfa_fit(fit)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)

  expected_cols <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
                     "rmsea_ci_lower", "rmsea_ci_upper", "srmr", "aic", "bic")
  expect_named(result, expected_cols)

  # All should be numeric
  expect_true(all(vapply(result, is.numeric, logical(1))))
})

test_that("tidy_cfa_fit validates input", {
  skip_if_not_installed("lavaan")

  expect_error(
    tidy_cfa_fit("not a lavaan object"),
    "fit must be a lavaan object"
  )
})


# compare_cfa_models tests ----

test_that("compare_cfa_models compares nested models", {
  skip_if_not_installed("lavaan")

  # Model 1: 1 factor (more constrained)
  model1 <- "general =~ q1 + q2 + q3 + q4"

  # Model 2: 2 factors (less constrained)
  model2 <- "
    factor1 =~ q1 + q2 + q3
    factor2 =~ q4
  "

  fit1 <- run_cfa(model1, data = test_data)
  fit2 <- run_cfa(model2, data = test_data)

  # Note: fit1 should be more constrained (fewer parameters, more df)
  result <- compare_cfa_models(fit1, fit2)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)  # 2 models + difference row
  expect_equal(result$model, c("Small (constrained)", "Large (free)", "Difference"))

  # Should have p-value attribute
  expect_true(!is.null(attr(result, "chi_diff_p")))
  expect_true(is.numeric(attr(result, "chi_diff_p")))
})

test_that("compare_cfa_models validates inputs", {
  skip_if_not_installed("lavaan")

  model <- "factor =~ q1 + q2"
  fit <- run_cfa(model, data = test_data)

  expect_error(
    compare_cfa_models("not lavaan", fit),
    "Both fit_small and fit_large must be lavaan objects"
  )
})


# alpha_table tests ----

test_that("alpha_table calculates for multiple scales", {
  scales <- list(
    scale1 = c("q1", "q2", "q3"),
    scale2 = c("q4", "q5", "q6")
  )

  result <- alpha_table(test_data, scales, method = "internal")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_named(result, c("scale_name", "alpha", "n_items", "n_obs", "interpretation"))

  expect_equal(result$scale_name, c("scale1", "scale2"))
  expect_equal(result$n_items, c(3, 3))
  expect_true(all(result$alpha >= 0 & result$alpha <= 1, na.rm = TRUE))
})

test_that("alpha_table works with performance method", {
  skip_if_not_installed("performance")

  scales <- list(scale1 = c("q1", "q2", "q3"))

  result <- alpha_table(test_data, scales, method = "performance")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
})

test_that("alpha_table works with psych method", {
  skip_if_not_installed("psych")

  scales <- list(scale1 = c("q1", "q2", "q3"))

  result <- alpha_table(test_data, scales, method = "psych")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
})

test_that("alpha_table handles missing items gracefully", {
  scales <- list(
    good_scale = c("q1", "q2", "q3"),
    bad_scale = c("missing1", "missing2")
  )

  result <- alpha_table(test_data, scales, method = "internal")

  expect_equal(nrow(result), 2)

  # Good scale should have valid alpha
  expect_false(is.na(result$alpha[1]))

  # Bad scale should have NA alpha and error message
  expect_true(is.na(result$alpha[2]))
  expect_true(grepl("Error", result$interpretation[2]))
})

test_that("alpha_table validates inputs", {
  expect_error(
    alpha_table("not a df", list(scale1 = c("q1", "q2"))),
    "data must be a data frame"
  )

  expect_error(
    alpha_table(test_data, c("q1", "q2")),  # Not a named list
    "scales_spec must be a named list"
  )
})


# print.efa_diagnostics tests ----

test_that("print.efa_diagnostics works", {
  skip_if_not_installed("psych")

  items <- test_data[, c("q1", "q2", "q3")]
  result <- efa_diagnostics(items)

  expect_output(print(result), "EFA DIAGNOSTICS")
  expect_output(print(result), "KMO")
  expect_output(print(result), "BARTLETT")
  expect_output(print(result), "PARALLEL ANALYSIS")
})
