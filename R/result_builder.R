#' Analysis Result Builder
#'
#' Standardized constructor for analysis result objects. Ensures consistent
#' structure across all analysis functions in consumeR.
#'
#' @family infrastructure
#' @keywords internal
#'
#' Build Standardized Analysis Result Object
#'
#' Constructs a standardized result list with consistent field ordering,
#' validation, and class assignment.
#'
#' @param test_type Character. Short identifier (e.g., "t_test", "anova")
#' @param test_name Character. Human-readable name (e.g., "Independent Samples T-Test")
#' @param core_stats List. Must contain at minimum: p_value, n
#' @param specific_stats List. Test-specific statistics (e.g., t_statistic, df)
#' @param assumptions assumptions_result object or NULL
#' @param interpretation Character. Plain language interpretation
#' @param publication_text Character or NULL. APA7 formatted text
#' @param alpha Numeric. Significance threshold (default: 0.05)
#' @param ... Additional fields to include
#'
#' @return S3 object with classes c("<test_type>_result", "analysis_result", "list")
#'   with standardized field ordering and validation
#'
#' @examples
#' result <- build_analysis_result(
#'   test_type = "t_test",
#'   test_name = "Independent Samples T-Test",
#'   core_stats = list(p_value = 0.042, n = 100),
#'   specific_stats = list(t_statistic = 2.05, df = 98),
#'   interpretation = "Groups differ significantly"
#' )
#'
#' @export
build_analysis_result <- function(
  test_type,
  test_name,
  core_stats,
  specific_stats = list(),
  assumptions = NULL,
  interpretation = NULL,
  publication_text = NULL,
  alpha = 0.05,
  ...
) {
  # Validate required core stats
  if (!"p_value" %in% names(core_stats)) {
    rlang::abort("core_stats must contain 'p_value'", class = "missing_p_value_error")
  }
  if (!"n" %in% names(core_stats)) {
    rlang::abort("core_stats must contain 'n'", class = "missing_n_error")
  }

  # Auto-compute significance if not provided
  if (!"significant" %in% names(core_stats)) {
    core_stats$significant <- core_stats$p_value < alpha
  }

  # Build standardized structure
  result <- list(
    # Metadata
    test_type = test_type,
    test_name = test_name,
    timestamp = Sys.time(),

    # Core statistics (always present)
    p_value = round(core_stats$p_value, 6),
    significant = core_stats$significant,
    alpha = alpha,
    n = as.integer(core_stats$n)
  )

  # Merge in specific stats
  result <- c(result, specific_stats)

  # Add diagnostics and interpretation
  result$assumptions <- assumptions
  result$interpretation <- interpretation
  result$publication_text <- publication_text

  # Add any extra fields from ...
  extra_fields <- list(...)
  if (length(extra_fields) > 0) {
    result <- c(result, extra_fields)
  }

  # Assign classes
  structure(
    result,
    class = c(paste0(test_type, "_result"), "analysis_result", "list")
  )
}

#' Print Analysis Result
#'
#' Generic S3 print method for all analysis result objects. Provides consistent
#' formatting with optional sections.
#'
#' @param x An analysis_result object
#' @param show_assumptions Logical. Show assumption checks? (default: FALSE)
#' @param show_publication Logical. Show APA7 text? (default: FALSE)
#' @param show_interpretation Logical. Show interpretation? (default: TRUE)
#' @param width Integer. Console width for formatting (default: 78)
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.analysis_result <- function(
  x,
  show_assumptions = FALSE,
  show_publication = FALSE,
  show_interpretation = TRUE,
  width = 78,
  ...
) {
  # Symbols
  sep_major <- strrep("=", width)
  sep_minor <- strrep("-", width)

  # Header
  cat(sep_major, "\n")
  cat(x$test_name, "RESULTS\n")
  cat(sep_major, "\n\n")

  # Summary section
  cat("SUMMARY\n")
  cat(sep_minor, "\n")
  cat("• Sample size: N =", x$n, "\n")
  cat("• Alpha level:", x$alpha, "\n")
  result_text <- if (x$significant) "Significant" else "Not significant"
  p_comparison <- if (x$p_value < x$alpha) "<" else ">"
  cat("• Result:", result_text, "(p", p_comparison, x$alpha, ")\n\n")

  # Test-specific sections (delegate to helper if exists)
  # For now, print common statistics
  cat("TEST STATISTICS\n")
  cat(sep_minor, "\n")

  # Print test-specific stats (skip metadata and standard fields)
  skip_fields <- c("test_type", "test_name", "timestamp", "p_value", "significant",
                   "alpha", "n", "assumptions", "interpretation", "publication_text")
  stat_fields <- setdiff(names(x), skip_fields)

  for (field in stat_fields) {
    # Format field name nicely
    field_label <- gsub("_", " ", field)
    field_label <- paste0(toupper(substring(field_label, 1, 1)), substring(field_label, 2))
    cat("•", field_label %+% ":", x[[field]], "\n")
  }
  cat("\n")

  # Interpretation
  if (show_interpretation && !is.null(x$interpretation)) {
    cat("INTERPRETATION\n")
    cat(sep_minor, "\n")
    cat(x$interpretation, "\n\n")
  }

  # Assumptions
  if (show_assumptions && !is.null(x$assumptions)) {
    cat("ASSUMPTION CHECKS\n")
    cat(sep_minor, "\n")
    if (x$assumptions$overall_pass) {
      cat("✓ All assumptions satisfied\n")
    } else {
      cat("✗ Some assumptions violated:\n")
      for (w in x$assumptions$warnings) {
        cat("  -", w, "\n")
      }
    }
    cat("\n")
  }

  # Publication text
  if (show_publication && !is.null(x$publication_text)) {
    cat("PUBLICATION TEXT (APA7)\n")
    cat(sep_minor, "\n")
    cat(x$publication_text, "\n\n")
  }

  # Tips
  cat("TIPS\n")
  cat(sep_minor, "\n")
  cat("• Use show_assumptions = TRUE to see diagnostic checks\n")
  cat("• Use show_publication = TRUE to get APA7 formatted text\n")
  cat("• Access raw values via result$<field>\n")
  cat(sep_major, "\n")

  invisible(x)
}
