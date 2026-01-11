#' Table Formatting and Export Functions
#'
#' @description
#' Functions for creating publication-ready tables in various formats
#' (markdown, TSV, etc.) for manuscripts and supplementary materials.
#'
#' @name table_formatting
NULL


#' Create Markdown Table
#'
#' @description
#' Converts a data frame to a markdown-formatted table suitable for
#' inclusion in R Markdown documents, GitHub README files, or manuscripts.
#'
#' @param df Data frame to convert
#' @param caption Character. Optional table caption
#' @param digits Integer. Number of digits for rounding numeric columns (default 2)
#' @param align Character vector. Column alignment: "l" (left), "r" (right), "c" (center).
#'   If NULL (default), auto-detects based on column type.
#'
#' @return Character string containing markdown table (invisibly)
#'
#' @details
#' Uses knitr::kable() internally for markdown generation.
#' Numeric columns are automatically rounded to specified digits.
#'
#' @examples
#' \dontrun{
#' # Simple table
#' df <- data.frame(
#'   Variable = c("Age", "Income", "Satisfaction"),
#'   Mean = c(34.5, 65000, 7.2),
#'   SD = c(8.3, 15000, 1.4)
#' )
#' make_table_md(df, caption = "Descriptive Statistics")
#'
#' # Custom alignment
#' make_table_md(df, align = c("l", "r", "r"))
#' }
#'
#' @export
make_table_md <- function(df, caption = NULL, digits = 2, align = NULL) {
  # Validate input
  if (!is.data.frame(df)) {
    rlang::abort(c(
      "df must be a data frame",
      "x" = "You provided {class(df)[1]}"
    ))
  }

  # Check for knitr
  if (!requireNamespace("knitr", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'knitr' is required for markdown tables",
      "i" = "Install with: install.packages('knitr')"
    ))
  }

  # Round numeric columns
  df_rounded <- df
  numeric_cols <- vapply(df, is.numeric, logical(1))
  df_rounded[numeric_cols] <- lapply(df_rounded[numeric_cols], round, digits = digits)

  # Auto-detect alignment if not specified
  if (is.null(align)) {
    align <- vapply(df, function(col) {
      if (is.numeric(col)) "r" else "l"
    }, character(1))
  }

  # Generate markdown table
  md_table <- knitr::kable(
    df_rounded,
    format = "markdown",
    caption = caption,
    align = align,
    row.names = FALSE
  )

  # Print and return invisibly
  cat(md_table, sep = "\n")
  invisible(md_table)
}


#' Create TSV Table for Copy-Paste
#'
#' @description
#' Converts a data frame to tab-separated values (TSV) format that can be
#' easily copied and pasted into spreadsheet applications like Excel or
#' Google Sheets.
#'
#' @param df Data frame to convert
#' @param digits Integer. Number of digits for rounding numeric columns (default 2)
#' @param include_rownames Logical. Include row names? (default FALSE)
#' @param na_string Character. How to represent NA values (default "")
#'
#' @return Character string containing TSV data (invisibly)
#'
#' @details
#' The output is printed to the console where it can be copied.
#' Use Ctrl+C (or Cmd+C on Mac) to copy the output, then paste into Excel/Sheets.
#'
#' Alternatively, write to clipboard on supported systems:
#' \code{writeClipboard(make_table_tsv(df))} on Windows
#' \code{clip <- pipe("pbcopy", "w"); write(make_table_tsv(df), clip); close(clip)} on Mac
#'
#' @examples
#' \dontrun{
#' # Create table
#' df <- data.frame(
#'   Variable = c("Age", "Income", "Satisfaction"),
#'   Mean = c(34.5, 65000, 7.2),
#'   SD = c(8.3, 15000, 1.4)
#' )
#'
#' # Print as TSV
#' make_table_tsv(df)
#'
#' # Copy the output and paste into Excel/Sheets
#' }
#'
#' @export
make_table_tsv <- function(df, digits = 2, include_rownames = FALSE, na_string = "") {
  # Validate input
  if (!is.data.frame(df)) {
    rlang::abort(c(
      "df must be a data frame",
      "x" = "You provided {class(df)[1]}"
    ))
  }

  # Round numeric columns
  df_rounded <- df
  numeric_cols <- vapply(df, is.numeric, logical(1))
  df_rounded[numeric_cols] <- lapply(df_rounded[numeric_cols], round, digits = digits)

  # Capture TSV output
  tsv_output <- capture.output({
    write.table(
      df_rounded,
      sep = "\t",
      row.names = include_rownames,
      col.names = TRUE,
      quote = FALSE,
      na = na_string
    )
  })

  # Combine into single string
  tsv_string <- paste(tsv_output, collapse = "\n")

  # Print with instructions
  cat("\n")
  cat("=== TSV TABLE (Copy-Paste Ready) ===\n")
  cat("Select the text below and copy (Ctrl+C or Cmd+C):\n")
  cat("---\n")
  cat(tsv_string, "\n")
  cat("---\n")
  cat("Then paste into Excel, Google Sheets, or other spreadsheet software.\n")
  cat("\n")

  invisible(tsv_string)
}


#' Write Table to File
#'
#' @description
#' Saves a data frame to disk in various formats (TSV, CSV, markdown, RDS).
#'
#' @param df Data frame to save
#' @param path Character. File path for output
#' @param format Character. Output format: "tsv" (default), "csv", "md", or "rds"
#' @param digits Integer. Number of digits for rounding numeric columns (default 2)
#' @param ... Additional arguments passed to format-specific functions
#'
#' @return Invisible path to saved file
#'
#' @details
#' **Format options:**
#' - "tsv": Tab-separated values (readable by Excel, R, Python)
#' - "csv": Comma-separated values
#' - "md": Markdown table (for documentation)
#' - "rds": R data serialization (preserves exact data types)
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   Variable = c("Age", "Income"),
#'   Mean = c(34.5, 65000),
#'   SD = c(8.3, 15000)
#' )
#'
#' # Save as TSV
#' write_table(df, "results/descriptives.tsv")
#'
#' # Save as markdown
#' write_table(df, "results/table1.md", format = "md")
#'
#' # Save as RDS (preserves types)
#' write_table(df, "results/data.rds", format = "rds")
#' }
#'
#' @export
write_table <- function(df, path, format = c("tsv", "csv", "md", "rds"), digits = 2, ...) {
  format <- match.arg(format)

  # Validate input
  if (!is.data.frame(df)) {
    rlang::abort(c(
      "df must be a data frame",
      "x" = "You provided {class(df)[1]}"
    ))
  }

  # Create directory if needed
  dir_path <- dirname(path)
  if (!dir.exists(dir_path) && dir_path != ".") {
    dir.create(dir_path, recursive = TRUE)
  }

  # Round numeric columns (except for RDS)
  if (format != "rds") {
    df_rounded <- df
    numeric_cols <- vapply(df, is.numeric, logical(1))
    df_rounded[numeric_cols] <- lapply(df_rounded[numeric_cols], round, digits = digits)
  } else {
    df_rounded <- df
  }

  # Write based on format
  switch(
    format,
    "tsv" = {
      if (!requireNamespace("readr", quietly = TRUE)) {
        rlang::abort(c(
          "Package 'readr' is required",
          "i" = "Install with: install.packages('readr')"
        ))
      }
      readr::write_tsv(df_rounded, path, ...)
    },
    "csv" = {
      if (!requireNamespace("readr", quietly = TRUE)) {
        rlang::abort(c(
          "Package 'readr' is required",
          "i" = "Install with: install.packages('readr')"
        ))
      }
      readr::write_csv(df_rounded, path, ...)
    },
    "md" = {
      if (!requireNamespace("knitr", quietly = TRUE)) {
        rlang::abort(c(
          "Package 'knitr' is required",
          "i" = "Install with: install.packages('knitr')"
        ))
      }
      md_content <- knitr::kable(df_rounded, format = "markdown", ...)
      writeLines(md_content, path)
    },
    "rds" = {
      saveRDS(df_rounded, path, ...)
    }
  )

  message("Table saved to: ", path)
  invisible(path)
}
