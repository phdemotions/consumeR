# Tests for composite scoring functions

# reverse_score_likert tests ----

test_that("reverse_score_likert works with 1-7 scale", {
  x <- c(1, 2, 3, 4, 5, 6, 7)
  expected <- c(7, 6, 5, 4, 3, 2, 1)
  expect_equal(reverse_score_likert(x), expected)
})

test_that("reverse_score_likert works with 1-5 scale", {
  x <- c(1, 2, 3, 4, 5)
  expected <- c(5, 4, 3, 2, 1)
  expect_equal(reverse_score_likert(x, min = 1, max = 5), expected)
})

test_that("reverse_score_likert handles NA values", {
  x <- c(1, NA, 3, 5, NA, 7)
  expected <- c(7, NA, 5, 3, NA, 1)
  expect_equal(reverse_score_likert(x), expected)
})

test_that("reverse_score_likert works with different min/max", {
  x <- c(0, 5, 10)
  expected <- c(10, 5, 0)
  expect_equal(reverse_score_likert(x, min = 0, max = 10), expected)
})

test_that("reverse_score_likert strict mode catches out-of-range", {
  x <- c(1, 2, 8, 4, 5)  # 8 is out of range for 1-7
  expect_error(
    reverse_score_likert(x, strict = TRUE),
    "Values outside.*range"
  )
})

test_that("reverse_score_likert non-strict mode warns and preserves", {
  x <- c(1, 2, 8, 4, 5)

  expect_warning(
    result <- reverse_score_likert(x, strict = FALSE),
    "out of range"
  )

  # 1→7, 2→6, 8→8 (preserved), 4→4, 5→3
  expect_equal(result, c(7, 6, 8, 4, 3))
})

test_that("reverse_score_likert validates input", {
  expect_error(reverse_score_likert("not numeric"), "x must be numeric")
  expect_error(reverse_score_likert(c(1,2,3), min = 7, max = 1), "min must be less than max")
})


# row_sd tests ----

test_that("row_sd calculates correctly", {
  df <- data.frame(
    q1 = c(1, 4, 7),
    q2 = c(2, 5, 8),
    q3 = c(3, 6, 9)
  )

  result <- row_sd(df)

  # Calculate expected values manually
  expected <- c(
    sd(c(1, 2, 3)),
    sd(c(4, 5, 6)),
    sd(c(7, 8, 9))
  )

  expect_equal(result, expected)
})

test_that("row_sd works with specific items", {
  df <- data.frame(
    q1 = c(1, 4),
    q2 = c(2, 5),
    q3 = c(100, 200)  # Should be ignored
  )

  result <- row_sd(df, items = c("q1", "q2"))

  expected <- c(
    sd(c(1, 2)),
    sd(c(4, 5))
  )

  expect_equal(result, expected)
})

test_that("row_sd detects straight-lining", {
  df <- data.frame(
    q1 = c(7, 3),
    q2 = c(7, 3),
    q3 = c(7, 3)
  )

  result <- row_sd(df)
  expect_equal(result, c(0, 0))
})

test_that("row_sd handles NA with na.rm = TRUE", {
  df <- data.frame(
    q1 = c(1, NA, 3),
    q2 = c(2, 3, NA),
    q3 = c(3, 4, 5)
  )

  result <- row_sd(df, na.rm = TRUE)

  # Row 1: sd(1,2,3)
  # Row 2: sd(3,4) - q1 is NA
  # Row 3: sd(3,5) - q2 is NA

  expect_equal(result[1], sd(c(1, 2, 3)))
  expect_equal(result[2], sd(c(3, 4)))
  expect_equal(result[3], sd(c(3, 5)))
})

test_that("row_sd handles NA with na.rm = FALSE", {
  df <- data.frame(
    q1 = c(1, NA, 3),
    q2 = c(2, 3, 4)
  )

  result <- row_sd(df, na.rm = FALSE)

  # Row 1: sd(1,2) = normal
  # Row 2: NA because q1 is NA
  # Row 3: sd(3,4) = normal

  expect_equal(result[1], sd(c(1, 2)))
  expect_true(is.na(result[2]))
  expect_equal(result[3], sd(c(3, 4)))
})

test_that("row_sd works with matrices", {
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  result <- row_sd(mat)

  expected <- c(
    sd(c(1, 3, 5)),
    sd(c(2, 4, 6))
  )

  expect_equal(result, expected)
})

test_that("row_sd validates input", {
  expect_error(row_sd("not a df"), "data must be a data frame or matrix")

  df <- data.frame(q1 = c(1, 2), q2 = c("a", "b"))
  expect_error(row_sd(df), "All columns must be numeric")
})


# score_composite tests ----

test_that("score_composite calculates mean correctly", {
  df <- data.frame(
    q1 = c(6, 4, 2),
    q2 = c(7, 5, 3),
    q3 = c(8, 6, 4)
  )

  result <- score_composite(df, items = c("q1", "q2", "q3"), method = "mean")

  expected <- c(7, 5, 3)
  expect_equal(result$value, expected)
  expect_equal(result$meta$k, 3)
  expect_equal(result$meta$method, "mean")
})

test_that("score_composite calculates sum correctly", {
  df <- data.frame(
    q1 = c(1, 2),
    q2 = c(2, 3),
    q3 = c(3, 4)
  )

  result <- score_composite(df, items = c("q1", "q2", "q3"), method = "sum")

  expected <- c(6, 9)
  expect_equal(result$value, expected)
  expect_equal(result$meta$method, "sum")
})

test_that("score_composite handles reverse scoring", {
  df <- data.frame(
    happy1 = c(7, 6),
    sad2 = c(1, 2),     # Needs reversing (1→7, 2→6)
    happy3 = c(7, 6)
  )

  result <- score_composite(
    df,
    items = c("happy1", "sad2", "happy3"),
    reverse_items = "sad2",
    reverse_min = 1,
    reverse_max = 7
  )

  # sad2 after reversing: c(7, 6)
  # Mean: (7+7+7)/3=7, (6+6+6)/3=6
  expected <- c(7, 6)
  expect_equal(result$value, expected)
  expect_equal(result$meta$reverse_items, "sad2")
})

test_that("score_composite preserves NA by default", {
  df <- data.frame(
    q1 = c(5, NA, 7),
    q2 = c(6, 6, 8),
    q3 = c(7, 7, 9)
  )

  result <- score_composite(df, items = c("q1", "q2", "q3"), na_rule = "preserve")

  # Row 1: mean(5,6,7) = 6
  # Row 2: NA (has NA)
  # Row 3: mean(7,8,9) = 8

  expect_equal(result$value[1], 6)
  expect_true(is.na(result$value[2]))
  expect_equal(result$value[3], 8)
  expect_equal(result$meta$na_rule, "preserve")
})

test_that("score_composite threshold rule works", {
  df <- data.frame(
    q1 = c(6, NA, 5, NA),
    q2 = c(7, 7, NA, NA),
    q3 = c(8, 6, 5, NA)
  )

  result <- score_composite(
    df,
    items = c("q1", "q2", "q3"),
    na_rule = "threshold",
    threshold = 2  # Need at least 2 items
  )

  # Row 1: 3 items → mean(6,7,8) = 7
  # Row 2: 2 items → mean(7,6) = 6.5
  # Row 3: 2 items → mean(5,5) = 5
  # Row 4: 0 items → NA (below threshold)

  expect_equal(result$value[1], 7)
  expect_equal(result$value[2], 6.5)
  expect_equal(result$value[3], 5)
  expect_true(is.na(result$value[4]))

  expect_equal(result$meta$threshold, 2)
  expect_true(result$meta$below_threshold[4])
})

test_that("score_composite default threshold is 50%", {
  df <- data.frame(
    q1 = c(1, NA),
    q2 = c(2, NA),
    q3 = c(3, 3),
    q4 = c(4, 4)
  )

  result <- score_composite(
    df,
    items = c("q1", "q2", "q3", "q4"),
    na_rule = "threshold"
    # threshold not specified → should default to ceiling(4 * 0.5) = 2
  )

  # Row 1: 4 items present → OK
  # Row 2: 2 items present (q3, q4) → OK (meets threshold)

  expect_false(is.na(result$value[1]))
  expect_false(is.na(result$value[2]))
  expect_equal(result$meta$threshold, 2)
})

test_that("score_composite tracks missing data", {
  df <- data.frame(
    q1 = c(1, NA, 3),
    q2 = c(2, 2, NA),
    q3 = c(3, 3, 3)
  )

  result <- score_composite(df, items = c("q1", "q2", "q3"))

  # Row 1: 0 missing
  # Row 2: 1 missing (q1)
  # Row 3: 1 missing (q2)

  expect_equal(result$meta$n_missing, c(0, 1, 1))
})

test_that("score_composite stores metadata correctly", {
  df <- data.frame(q1 = 1, q2 = 2, q3 = 3)

  result <- score_composite(
    df,
    items = c("q1", "q2", "q3"),
    name = "Test Composite"
  )

  expect_equal(result$meta$name, "Test Composite")
  expect_equal(result$meta$items, c("q1", "q2", "q3"))
  expect_equal(result$meta$k, 3)
  expect_s3_class(result, "composite_score")
})

test_that("score_composite validates inputs", {
  df <- data.frame(q1 = 1, q2 = 2)

  # Non-data frame
  expect_error(score_composite("not df", items = "q1"), "df must be a data frame")

  # Missing items
  expect_error(score_composite(df, items = "missing"), "Missing variables")

  # Non-numeric items
  df_bad <- data.frame(q1 = "a", q2 = "b")
  expect_error(score_composite(df_bad, items = c("q1", "q2")), "All items must be numeric")

  # Invalid reverse_items
  expect_error(
    score_composite(df, items = c("q1", "q2"), reverse_items = "q3"),
    "reverse_items must be a subset"
  )

  # Invalid threshold
  expect_error(
    score_composite(df, items = c("q1", "q2"), na_rule = "threshold", threshold = 5),
    "threshold must be between"
  )
})


# add_composite tests ----

test_that("add_composite adds column correctly", {
  df <- data.frame(
    id = 1:3,
    q1 = c(5, 6, 7),
    q2 = c(6, 7, 8),
    q3 = c(7, 8, 9)
  )

  result <- add_composite(
    df,
    col_name = "satisfaction",
    items = c("q1", "q2", "q3")
  )

  # Should have new column
  expect_true("satisfaction" %in% names(result))
  expect_equal(ncol(result), ncol(df) + 1)

  # Values should be correct
  expect_equal(result$satisfaction, c(6, 7, 8))

  # Original columns preserved
  expect_equal(result$id, c(1, 2, 3))
})

test_that("add_composite stores metadata as attribute", {
  df <- data.frame(q1 = c(1, 2), q2 = c(2, 3))

  result <- add_composite(df, col_name = "comp", items = c("q1", "q2"))

  meta <- attr(result$comp, "composite_meta")
  expect_equal(meta$items, c("q1", "q2"))
  expect_equal(meta$k, 2)
})

test_that("add_composite passes arguments to score_composite", {
  df <- data.frame(
    pos = c(7, 6),
    neg = c(1, 2),
    pos2 = c(7, 6)
  )

  result <- add_composite(
    df,
    col_name = "attitude",
    items = c("pos", "neg", "pos2"),
    reverse_items = "neg",
    method = "sum"
  )

  # neg reversed: 1→7, 2→6
  # Sum: (7+7+7)=21, (6+6+6)=18
  expect_equal(result$attitude, c(21, 18))
})

test_that("add_composite validates col_name", {
  df <- data.frame(q1 = 1, q2 = 2)

  expect_error(add_composite(df, col_name = c("a", "b"), items = c("q1", "q2")),
               "col_name must be a single character string")
})


# print.composite_score tests ----

test_that("print.composite_score works", {
  df <- data.frame(q1 = c(1, 2), q2 = c(2, 3), q3 = c(3, 4))
  result <- score_composite(df, items = c("q1", "q2", "q3"), name = "Test")

  # Should print without error
  expect_output(print(result), "Composite Score")
  expect_output(print(result), "Test")
  expect_output(print(result), "Method: mean")
  expect_output(print(result), "Items \\(k\\): 3")
})
