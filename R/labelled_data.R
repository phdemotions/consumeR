#' Internal Helpers for SPSS Labelled Data Inspection and Handling
#'
#' @description
#' **Level 1 (L1) internal helpers** for working with SPSS .sav files
#' imported via haven::read_sav(). These functions help researchers inspect
#' value labels, identify problematic response codes, and prepare data
#' for analysis.
#'
#' **These are internal utilities.** They will be used by higher-level
#' user-facing functions but are not exported themselves.
#'
#' @name labelled_data_helpers
#' @keywords internal
NULL


#' Extract Value Labels from SPSS Labelled Variables
#'
#' @description
#' **L1 Internal Helper.** Extracts value labels from haven_labelled columns
#' and returns them in a structured, beginner-friendly format.
#'
#' @param x A single vector (can be haven_labelled, factor, or numeric)
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{value}: The numeric code
#'     \item \code{label}: The text label (if available)
#'   }
#'   Returns NULL if no labels are present.
#'
#' @details
#' **What this does:**
#' When you import SPSS data, variables often have "value labels" - for example,
#' gender might be coded as 1 = "Male", 2 = "Female", 99 = "Prefer not to say".
#' This function extracts those mappings so you can see what the numbers mean.
#'
#' **Works with:**
#' - haven_labelled vectors (from haven::read_sav)
#' - Factors (extracts levels)
#' - Plain numeric vectors (returns NULL)
#'
#' @examples
#' \dontrun{
#' # After importing SPSS data:
#' data <- haven::read_sav("survey.sav")
#'
#' # Extract labels for a single variable:
#' extract_value_labels(data$gender)
#' #   value label
#' #   1     Male
#' #   2     Female
#' #   99    Prefer not to say
#' }
#'
#' @keywords internal
extract_value_labels <- function(x) {
  # Handle haven_labelled
  if (inherits(x, "haven_labelled")) {
    labels <- attr(x, "labels", exact = TRUE)

    if (is.null(labels) || length(labels) == 0) {
      return(NULL)
    }

    # Return as data frame
    result <- data.frame(
      value = as.numeric(labels),
      label = names(labels),
      stringsAsFactors = FALSE
    )

    # Sort by value
    result <- result[order(result$value), ]
    rownames(result) <- NULL

    return(result)
  }

  # Handle factors
  if (is.factor(x)) {
    levs <- levels(x)

    if (length(levs) == 0) {
      return(NULL)
    }

    result <- data.frame(
      value = seq_along(levs),
      label = levs,
      stringsAsFactors = FALSE
    )

    return(result)
  }

  # No labels available
  return(NULL)
}


#' Identify Problematic Response Values in SPSS Data
#'
#' @description
#' **L1 Internal Helper.** Scans value labels to identify codes that represent
#' non-substantive responses (e.g., "Don't know", "Refused", "Not applicable").
#' These often need to be recoded to NA before analysis.
#'
#' @param label_df Data frame from extract_value_labels() with columns value, label
#' @param patterns Character vector of regex patterns to match problematic labels.
#'   Default includes common problematic response patterns.
#'
#' @return Data frame subset of label_df containing only problematic values,
#'   or NULL if none found. Adds a "reason" column explaining why it was flagged.
#'
#' @details
#' **What this identifies:**
#' Response codes that don't represent actual data, such as:
#' - "Don't know" / "DK" / "Unsure"
#' - "Refused" / "Decline to answer" / "Prefer not to say"
#' - "Not applicable" / "NA" / "N/A"
#' - "Missing" / "No response"
#' - Numeric codes like 99, 999, -99, -1 (common SPSS missing codes)
#'
#' **Why this matters:**
#' These should usually be recoded to NA before computing means, correlations,
#' or other statistics. Otherwise, "99 = Don't know" will be treated as a
#' very high response, inflating your results.
#'
#' @examples
#' \dontrun{
#' # Get labels for a variable
#' labels <- extract_value_labels(data$satisfaction)
#'
#' # Find problematic values
#' problems <- identify_problematic_values(labels)
#' #   value label              reason
#' #   99    Don't know         Don't know response
#' #   -1    Not applicable     Not applicable response
#' }
#'
#' @keywords internal
identify_problematic_values <- function(label_df,
                                       patterns = c(
                                         "don'?t know", "dk\\b", "unsure",
                                         "refuse", "declined?", "prefer not",
                                         "not applicable", "n/?a\\b",
                                         "missing", "no response", "skipped",
                                         "^99+$", "^-9+$", "^-1$"
                                       )) {

  if (is.null(label_df) || nrow(label_df) == 0) {
    return(NULL)
  }

  if (!all(c("value", "label") %in% names(label_df))) {
    rlang::abort(c(
      "label_df must have columns 'value' and 'label'",
      "x" = "Expected output from extract_value_labels()"
    ))
  }

  # Initialize results
  problematic <- data.frame(
    value = numeric(),
    label = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )

  # Check each pattern
  for (pattern in patterns) {
    matches <- grepl(pattern, label_df$label, ignore.case = TRUE)

    if (any(matches)) {
      matched_rows <- label_df[matches, , drop = FALSE]
      matched_rows$reason <- describe_pattern(pattern)
      problematic <- rbind(problematic, matched_rows)
    }
  }

  # Remove duplicates (same value matched by multiple patterns)
  if (nrow(problematic) > 0) {
    problematic <- problematic[!duplicated(problematic$value), ]
    rownames(problematic) <- NULL
    return(problematic)
  }

  return(NULL)
}


#' Describe Pattern Match Reason (Internal)
#'
#' @description
#' Converts regex pattern into human-readable reason.
#'
#' @param pattern Character string (regex pattern)
#' @return Character string describing what the pattern matches
#'
#' @keywords internal
#' @noRd
describe_pattern <- function(pattern) {
  # Map common patterns to readable descriptions
  # Note: backslashes are already escaped in the pattern strings
  reason_map <- c(
    "don'?t know" = "Don't know response",
    "dk\\b" = "Don't know response",
    "unsure" = "Uncertainty response",
    "refuse" = "Refused to answer",
    "declined?" = "Declined to answer",
    "prefer not" = "Prefer not to answer",
    "not applicable" = "Not applicable response",
    "n/?a\\b" = "Not applicable response",
    "missing" = "Missing data code",
    "no response" = "No response given",
    "skipped" = "Skipped question",
    "^99+$" = "Numeric missing code (99/999)",
    "^-9+$" = "Numeric missing code (-9/-99)",
    "^-1$" = "Numeric missing code (-1)"
  )

  # Try to find exact match
  if (pattern %in% names(reason_map)) {
    return(reason_map[[pattern]])
  }

  # Otherwise return generic
  return("Potentially problematic response")
}


#' Recode Problematic Values to NA with Detailed Logging
#'
#' @description
#' **L1 Internal Helper.** Replaces problematic response codes (e.g., 99 = "Don't know")
#' with NA, and creates a detailed log of what was changed.
#'
#' @param x Vector to recode (numeric, haven_labelled, or factor)
#' @param values_to_na Numeric vector of values to convert to NA
#' @param variable_name Character string, name of the variable (for logging)
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{recoded}: The recoded vector
#'     \item \code{log}: Data frame with columns:
#'       \itemize{
#'         \item \code{variable}: Variable name
#'         \item \code{value_recoded}: The value that was recoded
#'         \item \code{n_affected}: Number of cases changed
#'         \item \code{percent_affected}: Percentage of non-missing cases affected
#'       }
#'   }
#'
#' @details
#' **What this does:**
#' Systematically replaces specific values with NA and tracks the changes.
#' This is critical for transparency and reproducibility.
#'
#' **For peer review:**
#' The log provides exact counts of how many responses were excluded and why,
#' which reviewers expect to see in your Methods section.
#'
#' @examples
#' \dontrun{
#' # Recode 99 (Don't know) and -1 (Not applicable) to NA
#' result <- recode_to_na(
#'   data$satisfaction,
#'   values_to_na = c(99, -1),
#'   variable_name = "satisfaction"
#' )
#'
#' # Use the recoded variable
#' data$satisfaction_clean <- result$recoded
#'
#' # Check what was changed
#' result$log
#' #   variable     value_recoded n_affected percent_affected
#' #   satisfaction 99            12         3.2%
#' #   satisfaction -1            5          1.3%
#' }
#'
#' @keywords internal
recode_to_na <- function(x, values_to_na, variable_name = "variable") {

  if (!is.numeric(values_to_na) || length(values_to_na) == 0) {
    rlang::abort(c(
      "values_to_na must be a numeric vector with at least one value",
      "x" = "You provided {class(values_to_na)[1]} of length {length(values_to_na)}"
    ))
  }

  # Convert to numeric if needed (handles haven_labelled)
  if (inherits(x, "haven_labelled")) {
    x_numeric <- as.numeric(x)
  } else if (is.factor(x)) {
    # For factors, convert levels to numeric if possible
    x_numeric <- suppressWarnings(as.numeric(as.character(x)))
    # Check if conversion worked (at least some non-NA values after conversion that weren't already NA)
    originally_valid <- !is.na(x)
    if (sum(originally_valid) > 0 && all(is.na(x_numeric[originally_valid]))) {
      # Can't convert to numeric, abort
      rlang::abort(c(
        "Cannot recode factor levels that aren't numeric",
        "i" = "Convert to haven_labelled or numeric first"
      ))
    }
  } else if (is.numeric(x)) {
    x_numeric <- x
  } else {
    rlang::abort(c(
      "x must be numeric, factor, or haven_labelled",
      "x" = "You provided {class(x)[1]}"
    ))
  }

  # Create log entry for each value
  n_total <- length(x_numeric)
  n_originally_missing <- sum(is.na(x_numeric))
  n_valid_original <- n_total - n_originally_missing

  log_entries <- list()

  for (val in values_to_na) {
    n_affected <- sum(x_numeric == val, na.rm = TRUE)

    if (n_affected > 0) {
      pct_affected <- if (n_valid_original > 0) {
        round(n_affected / n_valid_original * 100, 2)
      } else {
        0
      }

      log_entries[[length(log_entries) + 1]] <- data.frame(
        variable = variable_name,
        value_recoded = val,
        n_affected = n_affected,
        percent_affected = pct_affected,
        stringsAsFactors = FALSE
      )
    }
  }

  # Perform recoding
  x_recoded <- x_numeric
  x_recoded[x_numeric %in% values_to_na] <- NA_real_

  # Combine log
  if (length(log_entries) > 0) {
    log_df <- do.call(rbind, log_entries)
    rownames(log_df) <- NULL
  } else {
    log_df <- data.frame(
      variable = character(),
      value_recoded = numeric(),
      n_affected = numeric(),
      percent_affected = numeric(),
      stringsAsFactors = FALSE
    )
  }

  list(
    recoded = x_recoded,
    log = log_df
  )
}
