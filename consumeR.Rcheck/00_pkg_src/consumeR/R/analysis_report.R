#' Create a Transparent Analysis Report
#'
#' This function generates a comprehensive, human-readable report of your
#' consumer data analysis. It is specifically designed for peer review,
#' providing complete transparency about data, methods, and results.
#'
#' @param data A numeric vector or data frame containing the data to analyze.
#'   If a data frame, specify the column to analyze with \code{variable}.
#' @param variable Character string. If \code{data} is a data frame, this
#'   specifies which column to analyze. Ignored if \code{data} is a vector.
#' @param group_var Character string. If comparing groups and \code{data} is
#'   a data frame, this specifies the grouping column. Must contain exactly
#'   2 unique groups. Default is NULL (no group comparison).
#' @param title Character string. Title for the report. Default is
#'   "Consumer Data Analysis Report".
#' @param report_file Character string. If provided, saves the report to
#'   this file path. Default is NULL (print to console only).
#'
#' @return Invisibly returns a list containing all calculated statistics
#'   and test results. The report is printed to console and optionally
#'   saved to a file.
#'
#' @details
#' The report includes:
#' \itemize{
#'   \item Sample description (size, missing values)
#'   \item Descriptive statistics (mean, median, SD, quartiles, etc.)
#'   \item Distribution visualization hints
#'   \item Group comparison results (if applicable)
#'   \item Methodological notes for transparency
#' }
#'
#' @examples
#' # Example 1: Simple analysis of a single variable
#' spending <- c(45.2, 67.8, 23.4, 89.1, 34.5, 56.7, 78.9, 12.3, 91.2, 43.5)
#' create_analysis_report(spending, title = "Consumer Spending Analysis")
#'
#' # Example 2: Analysis with a data frame
#' consumer_data <- data.frame(
#'   spending = c(45, 67, 23, 89, 34, 56, 78, 12, 91, 43),
#'   satisfaction = c(7, 8, 6, 9, 7, 8, 9, 5, 9, 7)
#' )
#' create_analysis_report(consumer_data, variable = "satisfaction",
#'                       title = "Customer Satisfaction Analysis")
#'
#' # Example 3: Group comparison
#' study_data <- data.frame(
#'   purchase_amount = c(45, 67, 23, 89, 34, 56, 78, 12, 91, 43,
#'                      34, 45, 29, 56, 41, 39, 49, 31, 52, 38),
#'   condition = c(rep("Treatment", 10), rep("Control", 10))
#' )
#' create_analysis_report(study_data,
#'                       variable = "purchase_amount",
#'                       group_var = "condition",
#'                       title = "Treatment Effect Analysis")
#'
#' @export
create_analysis_report <- function(data,
                                  variable = NULL,
                                  group_var = NULL,
                                  title = "Consumer Data Analysis Report",
                                  report_file = NULL) {

  # Step 1: Input Processing
  # -------------------------
  # Handle both vector and data frame inputs

  # If data is a data frame, extract the specified variable
  if (is.data.frame(data)) {
    if (is.null(variable)) {
      stop("Error: When 'data' is a data frame, you must specify 'variable' ",
           "to indicate which column to analyze.")
    }
    if (!variable %in% names(data)) {
      stop("Error: Variable '", variable, "' not found in data. ",
           "Available columns: ", paste(names(data), collapse = ", "))
    }

    # Extract the variable of interest
    analysis_data <- data[[variable]]

    # Extract grouping variable if specified
    if (!is.null(group_var)) {
      if (!group_var %in% names(data)) {
        stop("Error: Grouping variable '", group_var, "' not found in data.")
      }
      groups <- data[[group_var]]
    }
  } else {
    # Data is already a vector
    analysis_data <- data
    variable <- "Variable"  # Generic name for display
  }

  # Step 2: Basic Data Checks
  # --------------------------
  original_n <- length(analysis_data)
  n_missing <- sum(is.na(analysis_data))
  n_valid <- original_n - n_missing

  # Step 3: Initialize report output
  # ---------------------------------
  report_lines <- c()

  # Add header
  report_lines <- c(report_lines,
    paste(rep("=", nchar(title)), collapse = ""),
    title,
    paste(rep("=", nchar(title)), collapse = ""),
    paste("Generated on:", Sys.Date()),
    "",
    "SECTION 1: DATA OVERVIEW",
    paste(rep("-", 50), collapse = "")
  )

  # Add data description
  report_lines <- c(report_lines,
    paste("Variable analyzed:", variable),
    paste("Total observations:", original_n),
    paste("Valid observations:", n_valid),
    paste("Missing values:", n_missing,
          sprintf("(%.1f%%)", 100 * n_missing / original_n)),
    ""
  )

  # Step 4: Calculate and Report Descriptive Statistics
  # ----------------------------------------------------
  report_lines <- c(report_lines,
    "SECTION 2: DESCRIPTIVE STATISTICS",
    paste(rep("-", 50), collapse = "")
  )

  # Calculate summary statistics
  summary_stats <- calculate_summary_stats(analysis_data, include_all = TRUE)

  # Add statistics to report
  report_lines <- c(report_lines,
    paste("Sample size (n):", summary_stats$n),
    "",
    "Central Tendency Measures:",
    paste("  Mean:", summary_stats$mean),
    paste("  Median:", summary_stats$median),
    "",
    "Variability Measures:",
    paste("  Standard Deviation:", summary_stats$sd),
    paste("  Variance:", summary_stats$variance),
    paste("  Interquartile Range (IQR):", summary_stats$iqr),
    "",
    "Range:",
    paste("  Minimum:", summary_stats$min),
    paste("  Maximum:", summary_stats$max),
    paste("  Range:", summary_stats$range),
    "",
    "Quartiles:",
    paste("  25th percentile (Q1):", summary_stats$q25),
    paste("  50th percentile (Q2/Median):", summary_stats$median),
    paste("  75th percentile (Q3):", summary_stats$q75),
    ""
  )

  # Step 5: Group Comparison (if applicable)
  # -----------------------------------------
  group_results <- NULL

  if (!is.null(group_var) && exists("groups")) {
    report_lines <- c(report_lines,
      "SECTION 3: GROUP COMPARISON",
      paste(rep("-", 50), collapse = "")
    )

    # Check number of groups
    unique_groups <- unique(groups[!is.na(groups)])
    n_groups <- length(unique_groups)

    if (n_groups != 2) {
      report_lines <- c(report_lines,
        paste("Warning: Group comparison requires exactly 2 groups."),
        paste("Found", n_groups, "groups:", paste(unique_groups, collapse = ", ")),
        "Skipping group comparison.",
        ""
      )
    } else {
      # Split data by groups
      group1_data <- analysis_data[groups == unique_groups[1] & !is.na(groups)]
      group2_data <- analysis_data[groups == unique_groups[2] & !is.na(groups)]

      # Perform group comparison
      group_results <- test_group_differences(group1_data, group2_data)

      # Add results to report
      report_lines <- c(report_lines,
        paste("Grouping variable:", group_var),
        paste("Groups compared:", paste(unique_groups, collapse = " vs ")),
        "",
        paste("Group 1 (", unique_groups[1], "):", sep = ""),
        paste("  n =", group_results$group1_n),
        paste("  Mean =", group_results$group1_mean),
        "",
        paste("Group 2 (", unique_groups[2], "):", sep = ""),
        paste("  n =", group_results$group2_n),
        paste("  Mean =", group_results$group2_mean),
        "",
        "Statistical Test Results:",
        paste("  Test used:", group_results$test_used),
        paste("  P-value:", group_results$p_value),
        paste("  Significant at alpha = 0.05:", group_results$significant),
        paste("  Mean difference:", group_results$difference),
        "",
        "Interpretation:",
        paste(" ", group_results$interpretation),
        ""
      )
    }
  }

  # Step 6: Methodological Notes
  # -----------------------------
  report_lines <- c(report_lines,
    "SECTION 4: METHODOLOGICAL NOTES",
    paste(rep("-", 50), collapse = ""),
    "For complete transparency and reproducibility, note the following:",
    "",
    "1. Missing Value Handling:",
    "   - Missing values (NA) were removed before all calculations",
    sprintf("   - %d out of %d observations were missing (%.1f%%)",
            n_missing, original_n, 100 * n_missing / original_n),
    "",
    "2. Statistical Methods:",
    "   - Descriptive statistics calculated using base R functions",
    "   - Standard deviation uses n-1 denominator (sample SD)"
  )

  if (!is.null(group_results)) {
    report_lines <- c(report_lines,
      sprintf("   - Group comparison used: %s", group_results$test_used),
      "   - Significance level (alpha) set at 0.05"
    )
  }

  report_lines <- c(report_lines,
    "",
    "3. Software:",
    paste("   - R version:", R.version.string),
    paste("   - Package: consumeR"),
    "",
    paste(rep("=", 50), collapse = ""),
    "END OF REPORT",
    paste(rep("=", 50), collapse = "")
  )

  # Step 7: Output Report
  # ----------------------
  # Print to console
  cat(paste(report_lines, collapse = "\n"), "\n")

  # Save to file if requested
  if (!is.null(report_file)) {
    writeLines(report_lines, report_file)
    message("\nReport saved to: ", report_file)
  }

  # Step 8: Return Results Invisibly
  # ---------------------------------
  # Package all results for programmatic access
  results <- list(
    data_summary = list(
      n_total = original_n,
      n_valid = n_valid,
      n_missing = n_missing
    ),
    descriptive_stats = summary_stats,
    group_comparison = group_results,
    report_text = report_lines
  )

  invisible(results)
}
