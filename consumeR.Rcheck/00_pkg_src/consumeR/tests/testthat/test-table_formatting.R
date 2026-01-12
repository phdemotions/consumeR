# Tests for table formatting functions

# Test data
test_df <- data.frame(
  Variable = c("Age", "Income", "Satisfaction"),
  Mean = c(34.567, 65432.1, 7.234),
  SD = c(8.321, 15234.7, 1.456),
  stringsAsFactors = FALSE
)


# make_table_md tests ----

test_that("make_table_md works with knitr installed", {
  skip_if_not_installed("knitr")

  # Capture output
  output <- capture.output(result <- make_table_md(test_df, digits = 2))

  # Should return invisibly
  expect_invisible(make_table_md(test_df))

  # Should produce output (exact format depends on knitr version)
  expect_true(length(output) > 0)

  # Should contain table elements
  output_str <- paste(output, collapse = "\n")
  expect_true(grepl("Variable", output_str))
  expect_true(grepl("Mean", output_str))
})

test_that("make_table_md rounds numeric columns", {
  skip_if_not_installed("knitr")

  output <- capture.output(make_table_md(test_df, digits = 1))
  output_str <- paste(output, collapse = "\n")

  # Should round to 1 digit
  expect_true(grepl("34\\.6", output_str) || grepl("35", output_str))
})

test_that("make_table_md validates input", {
  expect_error(make_table_md("not a data frame"), "df must be a data frame")
})

test_that("make_table_md requires knitr", {
  # This would need to temporarily hide knitr - skip for now
  skip("Manual test: unload knitr and verify error")
})


# make_table_tsv tests ----

test_that("make_table_tsv produces TSV output", {
  # Capture output
  output <- capture.output(result <- make_table_tsv(test_df, digits = 2))

  # Should return invisibly
  expect_invisible(make_table_tsv(test_df))

  # Should produce TSV format
  output_str <- paste(output, collapse = "\n")
  expect_true(grepl("Variable\tMean\tSD", output_str, fixed = TRUE))
})

test_that("make_table_tsv rounds numeric columns", {
  output <- capture.output(result <- make_table_tsv(test_df, digits = 1))
  output_str <- paste(output, collapse = "\n")

  # Should round to 1 digit
  expect_true(grepl("34\\.6", output_str) || grepl("35", output_str))
  expect_true(grepl("7\\.2", output_str))
})

test_that("make_table_tsv handles NA values", {
  df_with_na <- test_df
  df_with_na$Mean[1] <- NA

  output <- capture.output(result <- make_table_tsv(df_with_na))
  output_str <- paste(output, collapse = "\n")

  # Should have empty cell for NA (default na_string = "")
  # Format: "Age\t\t8.32" (two tabs with nothing between)
  expect_true(grepl("Age\t", output_str))
})

test_that("make_table_tsv validates input", {
  expect_error(make_table_tsv("not a data frame"), "df must be a data frame")
})


# write_table tests ----

test_that("write_table creates TSV files", {
  skip_if_not_installed("readr")

  tmp <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmp))

  result <- write_table(test_df, tmp, format = "tsv", digits = 2)

  # Should return path invisibly
  expect_equal(result, tmp)

  # File should exist
  expect_true(file.exists(tmp))

  # Should be readable
  df_read <- readr::read_tsv(tmp, show_col_types = FALSE)
  expect_equal(nrow(df_read), 3)
  expect_equal(ncol(df_read), 3)
})

test_that("write_table creates CSV files", {
  skip_if_not_installed("readr")

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  result <- write_table(test_df, tmp, format = "csv", digits = 2)

  # File should exist
  expect_true(file.exists(tmp))

  # Should be readable
  df_read <- readr::read_csv(tmp, show_col_types = FALSE)
  expect_equal(nrow(df_read), 3)
})

test_that("write_table creates markdown files", {
  skip_if_not_installed("knitr")

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp))

  result <- write_table(test_df, tmp, format = "md", digits = 2)

  # File should exist
  expect_true(file.exists(tmp))

  # Should contain markdown table elements
  content <- readLines(tmp)
  content_str <- paste(content, collapse = "\n")
  expect_true(grepl("Variable", content_str))
  expect_true(grepl("\\|", content_str))  # Markdown table separator
})

test_that("write_table creates RDS files", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))

  result <- write_table(test_df, tmp, format = "rds")

  # File should exist
  expect_true(file.exists(tmp))

  # Should be readable and preserve exact values
  df_read <- readRDS(tmp)
  expect_equal(df_read, test_df)
})

test_that("write_table creates directories if needed", {
  skip_if_not_installed("readr")

  tmp_dir <- file.path(tempdir(), "test_subdir")
  tmp_file <- file.path(tmp_dir, "test.tsv")
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Directory should not exist
  expect_false(dir.exists(tmp_dir))

  # Write should create it
  write_table(test_df, tmp_file, format = "tsv")

  # Directory and file should exist
  expect_true(dir.exists(tmp_dir))
  expect_true(file.exists(tmp_file))
})

test_that("write_table validates input", {
  expect_error(write_table("not a df", tempfile()), "df must be a data frame")
})

test_that("write_table rounds except for RDS", {
  skip_if_not_installed("readr")

  # TSV should round
  tmp_tsv <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmp_tsv), add = TRUE)
  write_table(test_df, tmp_tsv, format = "tsv", digits = 1)
  df_tsv <- readr::read_tsv(tmp_tsv, show_col_types = FALSE)
  expect_equal(df_tsv$Mean[1], 34.6)

  # RDS should preserve exact values
  tmp_rds <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_rds), add = TRUE)
  write_table(test_df, tmp_rds, format = "rds", digits = 1)
  df_rds <- readRDS(tmp_rds)
  expect_equal(df_rds$Mean[1], 34.567)  # Original value preserved
})
