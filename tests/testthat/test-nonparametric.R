test_that("mann_whitney_test works with data frame input", {
  set.seed(42)
  df <- data.frame(
    score = c(rnorm(30, 5, 1), rnorm(30, 6, 1)),
    group = rep(c("A", "B"), each = 30)
  )

  result <- mann_whitney_test(df, outcome = "score", group = "group")

  # Updated for standardized structure
  expect_s3_class(result, "mann_whitney")
  expect_s3_class(result, "mann_whitney_result")
  expect_s3_class(result, "analysis_result")
  expect_type(result, "list")

  # Should have core standardized fields
  expect_true("test_type" %in% names(result))
  expect_true("test_name" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("n" %in% names(result))

  # Should have mann_whitney-specific fields
  expect_true("statistic" %in% names(result))
  expect_true("rank_biserial" %in% names(result))
  expect_true("median1" %in% names(result))
  expect_true("median2" %in% names(result))

  expect_true(result$statistic >= 0)
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  expect_true(result$rank_biserial >= -1 && result$rank_biserial <= 1)
  expect_equal(result$n1, 30)
  expect_equal(result$n2, 30)
})

test_that("mann_whitney_test works with vector input", {
  set.seed(42)
  x <- rnorm(25, 5, 1)
  y <- rnorm(25, 6, 1)

  result <- mann_whitney_test(x = x, y = y)

  expect_s3_class(result, "mann_whitney")
  expect_equal(result$n1, 25)
  expect_equal(result$n2, 25)
})

test_that("mann_whitney_test validates inputs", {
  df <- data.frame(score = rnorm(50), group = rep(c("A", "B"), each = 25))

  expect_error(mann_whitney_test(df, outcome = "missing", group = "group"), "not found")
  expect_error(mann_whitney_test(df, outcome = "score", group = "missing"), "not found")
  expect_error(mann_whitney_test(), "Either provide")
})

test_that("mann_whitney_test requires exactly 2 groups", {
  df <- data.frame(
    score = rnorm(60),
    group = rep(c("A", "B", "C"), each = 20)
  )

  expect_error(mann_whitney_test(df, outcome = "score", group = "group"), "exactly 2 levels")
})

test_that("mann_whitney_test calculates effect size correctly", {
  set.seed(42)
  # Create groups with known large difference
  df <- data.frame(
    score = c(rnorm(30, 3, 0.5), rnorm(30, 7, 0.5)),
    group = rep(c("Low", "High"), each = 30)
  )

  result <- mann_whitney_test(df, outcome = "score", group = "group")

  # Should have substantial effect size
  expect_true(abs(result$rank_biserial) > 0.3)
})

test_that("mann_whitney_test print method works", {
  set.seed(42)
  df <- data.frame(
    score = c(rnorm(20, 5, 1), rnorm(20, 6, 1)),
    group = rep(c("A", "B"), each = 20)
  )

  result <- mann_whitney_test(df, outcome = "score", group = "group")
  output <- capture.output(print(result))

  expect_true(any(grepl("Mann-Whitney", output)))
  expect_true(any(grepl("Median", output)))
  expect_true(any(grepl("Rank-biserial", output)))
})

test_that("kruskal_wallis_test works with 3+ groups", {
  set.seed(42)
  df <- data.frame(
    rating = c(rpois(30, 3), rpois(30, 5), rpois(30, 4)),
    product = rep(c("A", "B", "C"), each = 30)
  )

  result <- kruskal_wallis_test(df, outcome = "rating", group = "product")

  # Check classes (new standardized structure)
  expect_s3_class(result, "kruskal_wallis_result")
  expect_s3_class(result, "kruskal_wallis")
  expect_s3_class(result, "analysis_result")
  expect_type(result, "list")

  # Check standardized fields
  expect_true("test_type" %in% names(result))
  expect_true("test_name" %in% names(result))
  expect_equal(result$test_type, "kruskal_wallis")
  expect_equal(result$test_name, "Kruskal-Wallis Test")

  # Check kruskal-specific fields
  expect_true(result$statistic >= 0)
  expect_equal(result$df, 2)  # k - 1 = 3 - 1 = 2
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  expect_s3_class(result$group_medians, "tbl_df")
  expect_equal(nrow(result$group_medians), 3)
  expect_equal(result$n_groups, 3)
  expect_true("interpretation" %in% names(result))
})

test_that("kruskal_wallis_test validates inputs", {
  df <- data.frame(score = rnorm(60), group = rep(c("A", "B", "C"), each = 20))

  expect_error(kruskal_wallis_test("not a df", outcome = "score", group = "group"), "must be a data frame")
  expect_error(kruskal_wallis_test(df, outcome = "missing", group = "group"), "not found")
})

test_that("kruskal_wallis_test requires 3+ groups", {
  df <- data.frame(
    score = rnorm(40),
    group = rep(c("A", "B"), each = 20)
  )

  expect_error(kruskal_wallis_test(df, outcome = "score", group = "group"), "requires 3\\+ groups")
})

test_that("kruskal_wallis_test calculates epsilon-squared", {
  set.seed(42)
  df <- data.frame(
    rating = c(rpois(40, 3), rpois(40, 6), rpois(40, 4), rpois(40, 7)),
    condition = rep(c("A", "B", "C", "D"), each = 40)
  )

  result <- kruskal_wallis_test(df, outcome = "rating", group = "condition")

  # Check standardized structure
  expect_s3_class(result, "analysis_result")

  # Effect size should be between 0 and 1
  expect_true(result$epsilon_squared >= 0 && result$epsilon_squared <= 1)
})

test_that("kruskal_wallis_test print method works", {
  set.seed(42)
  df <- data.frame(
    score = c(rnorm(20, 5), rnorm(20, 6), rnorm(20, 5.5)),
    group = rep(c("A", "B", "C"), each = 20)
  )

  result <- kruskal_wallis_test(df, outcome = "score", group = "group")
  output <- capture.output(print(result))

  expect_true(any(grepl("Kruskal-Wallis", output)))
  expect_true(any(grepl("Epsilon-squared", output)))
  expect_true(any(grepl("Medians", output)))
})

test_that("wilcoxon_signed_rank_test works with data frame input", {
  set.seed(42)
  df <- data.frame(
    pre = rpois(40, 4),
    post = rpois(40, 6)
  )

  result <- wilcoxon_signed_rank_test(df, before = "pre", after = "post")

  # Updated for standardized structure
  expect_s3_class(result, "wilcoxon_signed_rank")
  expect_s3_class(result, "wilcoxon_result")
  expect_s3_class(result, "analysis_result")
  expect_type(result, "list")

  # Should have core standardized fields
  expect_true("test_type" %in% names(result))
  expect_true("test_name" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("n" %in% names(result))

  # Should have wilcoxon-specific fields
  expect_true("statistic" %in% names(result))
  expect_true("rank_biserial" %in% names(result))
  expect_true("median_before" %in% names(result))
  expect_true("median_after" %in% names(result))
  expect_true("median_diff" %in% names(result))

  expect_true(result$statistic >= 0)
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  expect_equal(result$n_pairs, 40)
})

test_that("wilcoxon_signed_rank_test works with vector input", {
  set.seed(42)
  x <- rpois(30, 4)
  y <- rpois(30, 5)

  result <- wilcoxon_signed_rank_test(x = x, y = y)

  expect_s3_class(result, "wilcoxon_signed_rank")
  expect_equal(result$n_pairs, 30)
})

test_that("wilcoxon_signed_rank_test validates inputs", {
  df <- data.frame(pre = rnorm(30), post = rnorm(30))

  expect_error(wilcoxon_signed_rank_test(df, before = "missing", after = "post"), "not found")
  expect_error(wilcoxon_signed_rank_test(), "Either provide")
})

test_that("wilcoxon_signed_rank_test calculates median difference", {
  set.seed(42)
  df <- data.frame(
    pre = c(1, 2, 3, 4, 5),
    post = c(3, 4, 5, 6, 7)
  )

  result <- wilcoxon_signed_rank_test(df, before = "pre", after = "post")

  # Median difference should be 2 (post - pre)
  expect_equal(result$median_diff, 2)
})

test_that("wilcoxon_signed_rank_test handles missing pairs", {
  set.seed(42)
  df <- data.frame(
    pre = c(rnorm(28), NA, NA),
    post = c(rnorm(29), NA)
  )

  result <- wilcoxon_signed_rank_test(df, before = "pre", after = "post")

  # Should use complete pairs only
  expect_lt(result$n_pairs, 30)
})

test_that("wilcoxon_signed_rank_test warns about small samples", {
  df <- data.frame(
    pre = c(1, 2, 3),
    post = c(2, 3, 4)
  )

  expect_warning(
    wilcoxon_signed_rank_test(df, before = "pre", after = "post"),
    "Very small sample"
  )
})

test_that("wilcoxon_signed_rank_test print method works", {
  set.seed(42)
  df <- data.frame(
    pre = rpois(30, 4),
    post = rpois(30, 5)
  )

  result <- wilcoxon_signed_rank_test(df, before = "pre", after = "post")
  output <- capture.output(print(result))

  expect_true(any(grepl("Wilcoxon", output)))
  expect_true(any(grepl("paired", output)))
  expect_true(any(grepl("Median difference", output)))
})

test_that("friedman_test works with long format data", {
  set.seed(42)
  df <- expand.grid(
    subject = 1:25,
    time = c("T1", "T2", "T3")
  )
  df$rating <- rpois(nrow(df), lambda = 3 + as.numeric(factor(df$time)))

  result <- friedman_test(df, outcome = "rating", time = "time", subject = "subject")

  expect_s3_class(result, "friedman")
  expect_type(result, "list")
  expect_named(result, c("statistic", "df", "p_value", "kendall_w",
                         "time_medians", "n_subjects", "n_times",
                         "interpretation"))

  expect_true(result$statistic >= 0)
  expect_equal(result$df, 2)  # k - 1 = 3 - 1
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  expect_equal(result$n_subjects, 25)
  expect_equal(result$n_times, 3)
})

test_that("friedman_test validates inputs", {
  df <- expand.grid(subject = 1:20, time = c("T1", "T2", "T3"))
  df$score <- rnorm(nrow(df))

  expect_error(friedman_test("not a df", outcome = "score", time = "time", subject = "subject"), "must be a data frame")
  expect_error(friedman_test(df, outcome = "missing", time = "time", subject = "subject"), "not found")
})

test_that("friedman_test requires 3+ time points", {
  df <- expand.grid(
    subject = 1:20,
    time = c("T1", "T2")
  )
  df$score <- rnorm(nrow(df))

  expect_error(friedman_test(df, outcome = "score", time = "time", subject = "subject"), "requires 3\\+ time points")
})

test_that("friedman_test calculates Kendall's W", {
  set.seed(42)
  df <- expand.grid(
    subject = 1:30,
    time = c("T1", "T2", "T3", "T4")
  )
  df$rating <- rpois(nrow(df), lambda = 4 + as.numeric(factor(df$time)))

  result <- friedman_test(df, outcome = "rating", time = "time", subject = "subject")

  # Kendall's W should be between 0 and 1
  expect_true(result$kendall_w >= 0 && result$kendall_w <= 1)
})

test_that("friedman_test print method works", {
  set.seed(42)
  df <- expand.grid(
    subject = 1:20,
    time = c("T1", "T2", "T3")
  )
  df$score <- rnorm(nrow(df))

  result <- friedman_test(df, outcome = "score", time = "time", subject = "subject")
  output <- capture.output(print(result))

  expect_true(any(grepl("Friedman", output)))
  expect_true(any(grepl("Kendall", output)))
  expect_true(any(grepl("Time Point", output)))
})

test_that("nonparametric tests have interpretable output", {
  set.seed(42)

  # Mann-Whitney
  df_mw <- data.frame(
    score = c(rnorm(30, 5), rnorm(30, 6)),
    group = rep(c("A", "B"), each = 30)
  )
  mw <- mann_whitney_test(df_mw, outcome = "score", group = "group")
  expect_type(mw$interpretation, "character")
  expect_true(grepl("Mdn|median", mw$interpretation, ignore.case = TRUE))

  # Kruskal-Wallis
  df_kw <- data.frame(
    score = c(rnorm(20, 5), rnorm(20, 6), rnorm(20, 5.5)),
    group = rep(c("A", "B", "C"), each = 20)
  )
  kw <- kruskal_wallis_test(df_kw, outcome = "score", group = "group")
  expect_type(kw$interpretation, "character")
  expect_true(grepl("difference|groups", kw$interpretation, ignore.case = TRUE))

  # Wilcoxon
  df_w <- data.frame(pre = rpois(25, 4), post = rpois(25, 5))
  w <- wilcoxon_signed_rank_test(df_w, before = "pre", after = "post")
  expect_type(w$interpretation, "character")
  expect_true(grepl("change|difference", w$interpretation, ignore.case = TRUE))

  # Friedman
  df_f <- expand.grid(subject = 1:20, time = c("T1", "T2", "T3"))
  df_f$score <- rnorm(nrow(df_f))
  f <- friedman_test(df_f, outcome = "score", time = "time", subject = "subject")
  expect_type(f$interpretation, "character")
  expect_true(grepl("time|difference", f$interpretation, ignore.case = TRUE))
})

test_that("nonparametric tests handle alternative hypotheses", {
  set.seed(42)
  x <- rnorm(30, 5)
  y <- rnorm(30, 6)

  result_two <- mann_whitney_test(x = x, y = y, alternative = "two.sided")
  result_less <- mann_whitney_test(x = x, y = y, alternative = "less")
  result_greater <- mann_whitney_test(x = x, y = y, alternative = "greater")

  expect_equal(result_two$alternative, "two.sided")
  expect_equal(result_less$alternative, "less")
  expect_equal(result_greater$alternative, "greater")
})

test_that("nonparametric tests provide effect sizes", {
  set.seed(42)

  # Mann-Whitney: rank-biserial
  df_mw <- data.frame(
    score = c(rnorm(25, 5), rnorm(25, 7)),
    group = rep(c("A", "B"), each = 25)
  )
  mw <- mann_whitney_test(df_mw, outcome = "score", group = "group")
  expect_true("rank_biserial" %in% names(mw))
  expect_true(abs(mw$rank_biserial) <= 1)

  # Kruskal-Wallis: epsilon-squared
  df_kw <- data.frame(
    score = c(rnorm(20, 5), rnorm(20, 7), rnorm(20, 6)),
    group = rep(c("A", "B", "C"), each = 20)
  )
  kw <- kruskal_wallis_test(df_kw, outcome = "score", group = "group")
  expect_true("epsilon_squared" %in% names(kw))
  expect_true(kw$epsilon_squared >= 0 && kw$epsilon_squared <= 1)

  # Wilcoxon: rank-biserial
  df_w <- data.frame(pre = rnorm(30, 4), post = rnorm(30, 6))
  w <- wilcoxon_signed_rank_test(df_w, before = "pre", after = "post")
  expect_true("rank_biserial" %in% names(w))

  # Friedman: Kendall's W
  df_f <- expand.grid(subject = 1:25, time = c("T1", "T2", "T3"))
  df_f$rating <- rpois(nrow(df_f), 4 + as.numeric(factor(df_f$time)))
  f <- friedman_test(df_f, outcome = "rating", time = "time", subject = "subject")
  expect_true("kendall_w" %in% names(f))
  expect_true(f$kendall_w >= 0 && f$kendall_w <= 1)
})
