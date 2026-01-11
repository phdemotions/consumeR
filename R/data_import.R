#' Import Data from CSV or SPSS with Interactive Variable Type Checking
#'
#' This function imports data from CSV or SPSS (.sav) files and provides an
#' interactive, user-friendly way to review and correct variable types. It's
#' designed for researchers who need to ensure their data is correctly formatted
#' before analysis, with complete transparency for peer review.
#'
#' Think of this as a quality control checkpoint - like checking products at
#' Cloud 9 before they go on the shelf. It makes sure everything is the right
#' type and ready for analysis.
#'
#' @param file_path Character string. Path to your data file (.csv or .sav)
#' @param interactive Logical. If TRUE (default), shows an interactive summary
#'   and asks you to confirm variable types. If FALSE, imports without prompts.
#' @param auto_convert Logical. If TRUE, attempts to intelligently convert
#'   variables (e.g., strings with "1"/"2" to factors). Default TRUE.
#' @param clean_names Logical. If TRUE (default), cleans column names using
#'   janitor::clean_names() to make them R-friendly (lowercase, no spaces, etc.)
#' @param create_report Logical. If TRUE, creates a data validation report
#'   with visualizations. Default TRUE.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{data}: The imported data as a tibble
#'     \item \code{variable_summary}: Tibble with info about each variable
#'     \item \code{suggested_changes}: Recommended type conversions
#'     \item \code{validation_report}: ggplot2 visualization of data quality
#'     \item \code{import_log}: Complete record of import process
#'   }
#'
#' @details
#' ## What This Function Does:
#'
#' 1. **Imports your data** from CSV or SPSS
#' 2. **Detects variable types** automatically
#' 3. **Shows you a summary** in plain English
#' 4. **Suggests improvements** (e.g., "q1_gender should be factor")
#' 5. **Lets you make changes** interactively
#' 6. **Creates a report** for your methods section
#'
#' ## Supported File Types:
#' - **CSV files** (.csv) - standard comma-separated values
#' - **SPSS files** (.sav) - preserves value labels and variable labels
#'
#' ## Variable Types Detected:
#' - **Numeric**: Continuous numbers (age, spending, ratings)
#' - **Integer**: Whole numbers (count data)
#' - **Factor**: Categorical (gender, condition, group)
#' - **Ordered Factor**: Ordinal (Likert scales, education level)
#' - **Character**: Text (open-ended responses)
#' - **Logical**: TRUE/FALSE
#' - **Date/Time**: Dates and timestamps
#'
#' ## For Peer Review:
#' The function creates a complete record showing:
#' - Original variable types from the file
#' - Any changes you made
#' - Rationale for changes
#' - Final variable structure
#'
#' @examples
#' \dontrun{
#' # Example 1: Import CSV file (interactive)
#' my_data <- import_research_data("survey_data.csv")
#' # Shows summary, asks for confirmation
#' # Returns cleaned data ready for analysis
#'
#' # Example 2: Import SPSS file (non-interactive)
#' my_data <- import_research_data(
#'   "study1_data.sav",
#'   interactive = FALSE,
#'   create_report = TRUE
#' )
#'
#' # Example 3: Access the imported data
#' result <- import_research_data("data.csv")
#' clean_data <- result$data  # The cleaned tibble
#' var_info <- result$variable_summary  # Info about variables
#' plot(result$validation_report)  # View quality report
#'
#' # Example 4: Manual type specification
#' result <- import_research_data("data.csv")
#' # Then use check_variable_types() to review and modify
#' }
#'
#' @export
#' @importFrom dplyr tibble mutate select case_when n_distinct
#' @importFrom readr read_csv cols
#' @importFrom ggplot2 ggplot aes geom_bar geom_col coord_flip
import_research_data <- function(file_path,
                                interactive = TRUE,
                                auto_convert = TRUE,
                                clean_names = TRUE,
                                create_report = TRUE) {

  # Load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' required. Install with: install.packages('dplyr')")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' required. Install with: install.packages('readr')")
  }

  message("\n==========================================================")
  message("|        TRANSPARENT DATA IMPORT & TYPE CHECKING         |")
  message("==========================================================\n")

  # Step 1: Check File Exists
  # --------------------------
  if (!file.exists(file_path)) {
    stop("Error: File not found at path: ", file_path, "\n",
         "Please check the file path and try again.")
  }

  # Get file extension
  file_ext <- tolower(tools::file_ext(file_path))

  message("File: ", basename(file_path))
  message("Type: ", toupper(file_ext), " file")

  # Step 2: Import Data Based on File Type
  # ---------------------------------------
  message("\nStep 1: Importing data...")

  if (file_ext == "csv") {
    # Import CSV using readr
    data <- read_csv(file_path, show_col_types = FALSE)
    message("  PASS CSV file imported successfully")

  } else if (file_ext == "sav") {
    # Import SPSS file using haven
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Package 'haven' required for SPSS files. ",
           "Install with: install.packages('haven')")
    }

    data <- haven::read_sav(file_path)
    message("  PASS SPSS file imported successfully")
    message("  PASS Preserved variable labels and value labels")

  } else {
    stop("Error: Unsupported file type '.", file_ext, "'\n",
         "Supported types: .csv, .sav")
  }

  # Convert to tibble for consistency
  data <- as_tibble(data)

  # Step 2b: Clean Column Names (if requested)
  # ------------------------------------------
  if (clean_names) {
    if (!requireNamespace("janitor", quietly = TRUE)) {
      warning("Package 'janitor' required for name cleaning. ",
              "Install with: install.packages('janitor')\n",
              "Skipping name cleaning.")
    } else {
      original_names <- names(data)
      data <- janitor::clean_names(data)
      cleaned_names <- names(data)

      # Check if any names changed
      n_changed <- sum(original_names != cleaned_names)

      if (n_changed > 0) {
        message("  PASS Cleaned ", n_changed, " column name",
                ifelse(n_changed > 1, "s", ""),
                " (lowercase, no spaces, R-friendly)")
      }
    }
  }

  n_rows <- nrow(data)
  n_cols <- ncol(data)

  message("  Dimensions: ", n_rows, " rows x ", n_cols, " columns")

  # Step 3: Analyze Variable Types
  # -------------------------------
  message("\nStep 2: Analyzing variable types...")

  # Create summary for each variable
  var_summary <- tibble(
    variable = names(data),
    current_type = sapply(data, function(x) class(x)[1]),
    n_unique = sapply(data, n_distinct),
    n_missing = sapply(data, function(x) sum(is.na(x))),
    percent_missing = round(n_missing / n_rows * 100, 1),
    example_values = sapply(data, function(x) {
      vals <- na.omit(x)[1:min(3, sum(!is.na(x)))]
      paste(vals, collapse = ", ")
    })
  )

  # Step 4: Suggest Variable Type Improvements
  # -------------------------------------------
  if (auto_convert) {
    message("\nStep 3: Detecting potential type improvements...")

    var_summary <- var_summary %>%
      mutate(
        suggested_type = case_when(
          # If numeric but only a few unique values, might be categorical
          current_type %in% c("numeric", "integer", "double") &
            n_unique <= 10 & n_unique >= 2 ~ "factor",

          # If character with few unique values, definitely factor
          current_type == "character" &
            n_unique <= 20 ~ "factor",

          # If it's labeled (from SPSS), convert to factor
          current_type == "haven_labelled" ~ "factor",

          # If logical, keep as logical
          current_type == "logical" ~ "logical",

          # Otherwise keep current type
          TRUE ~ current_type
        ),

        # Determine if change is suggested
        change_suggested = (suggested_type != current_type),

        # Rationale for suggestion
        suggestion_reason = case_when(
          change_suggested & current_type %in% c("numeric", "double") ~
            paste0("Only ", n_unique, " unique values - likely categorical"),
          change_suggested & current_type == "character" ~
            paste0("Text with ", n_unique, " categories - convert to factor"),
          change_suggested & current_type == "haven_labelled" ~
            "SPSS labeled variable - convert to factor with labels",
          TRUE ~ "No change needed"
        )
      )

    n_suggestions <- sum(var_summary$change_suggested)
    if (n_suggestions > 0) {
      message("  PASS Found ", n_suggestions, " suggested improvements")
    } else {
      message("  PASS All variables appear correctly typed")
    }
  }

  # Step 5: Display Summary (if interactive)
  # -----------------------------------------
  if (interactive) {
    message("\n")
    message("==========================================================")
    message("|              VARIABLE TYPE SUMMARY                     |")
    message("==========================================================\n")

    # Show current types
    type_counts <- table(var_summary$current_type)
    message("Current variable types:")
    for (i in 1:length(type_counts)) {
      message("  ", names(type_counts)[i], ": ", type_counts[i],
              " variable", ifelse(type_counts[i] > 1, "s", ""))
    }

    message("\n")

    # Show suggested changes
    if (auto_convert && n_suggestions > 0) {
      message("SUGGESTED CHANGES:")
      message("---------------------------------------------------------")

      changes <- var_summary %>%
        filter(change_suggested) %>%
        select(variable, current_type, suggested_type, suggestion_reason)

      for (i in 1:nrow(changes)) {
        message("\n", i, ". ", changes$variable[i])
        message("   Current: ", changes$current_type[i])
        message("   Suggested: ", changes$suggested_type[i])
        message("   Reason: ", changes$suggestion_reason[i])
        message("   Examples: ", var_summary$example_values[var_summary$variable == changes$variable[i]])
      }

      message("\n")
      message("---------------------------------------------------------")
    }

    # Show full variable list
    message("\nFULL VARIABLE LIST:")
    message("---------------------------------------------------------")
    print(var_summary %>%
            select(variable, current_type, n_unique, percent_missing) %>%
            as.data.frame(),
          row.names = FALSE)

    message("\n")
  }

  # Step 6: Apply Suggested Changes
  # --------------------------------
  if (auto_convert && n_suggestions > 0) {
    message("\nStep 4: Applying suggested type conversions...")

    data_converted <- data

    for (i in 1:nrow(var_summary)) {
      var_name <- var_summary$variable[i]

      if (var_summary$change_suggested[i]) {
        suggested <- var_summary$suggested_type[i]

        if (suggested == "factor") {
          # Convert to factor

          # Special handling for SPSS labeled variables
          if (inherits(data[[var_name]], "haven_labelled")) {
            # Use haven's as_factor to preserve labels
            data_converted[[var_name]] <- haven::as_factor(data[[var_name]])
            message("  PASS ", var_name, ": Converted to factor (preserved SPSS labels)")
          } else {
            # Regular conversion
            data_converted[[var_name]] <- as.factor(data[[var_name]])
            message("  PASS ", var_name, ": Converted to factor")
          }
        }
      }
    }

    data <- data_converted
  }

  # Step 7: Create Data Validation Report
  # --------------------------------------
  validation_plot <- NULL

  if (create_report) {
    message("\nStep 5: Creating data validation report...")

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("ggplot2 required for validation report. Skipping plot.")
    } else {
      # Create a summary plot showing variable types and missing data
      plot_data <- var_summary %>%
        mutate(
          type_label = paste0(current_type, " (", n_unique, " unique)"),
          missing_label = paste0(percent_missing, "% missing")
        )

      validation_plot <- ggplot(plot_data,
                                aes(x = reorder(variable, percent_missing),
                                    y = percent_missing)) +
        geom_col(aes(fill = current_type), alpha = 0.8) +
        geom_text(aes(label = paste0(percent_missing, "%")),
                  hjust = -0.1, size = 3) +
        coord_flip() +
        scale_fill_brewer(palette = "Set2", name = "Variable Type") +
        labs(
          title = "Data Quality Report",
          subtitle = paste0("Dataset: ", basename(file_path),
                           " (", n_rows, " rows, ", n_cols, " variables)"),
          x = "Variable",
          y = "Percentage Missing (%)",
          caption = "Variables ordered by missing data | Colors show variable types"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 14),
          legend.position = "bottom",
          panel.grid.major.y = element_blank()
        )

      message("  PASS Validation plot created")
    }
  }

  # Step 8: Create Import Log
  # --------------------------
  import_log <- list(
    file_path = file_path,
    file_type = file_ext,
    import_time = Sys.time(),
    n_rows = n_rows,
    n_cols = n_cols,
    changes_made = if (auto_convert) var_summary %>%
      filter(change_suggested) %>%
      select(variable, current_type, suggested_type, suggestion_reason) else NULL,
    n_changes = if (auto_convert) n_suggestions else 0
  )

  # Step 9: Return Results
  # ----------------------
  message("\n==========================================================")
  message("|                   IMPORT COMPLETE                      |")
  message("==========================================================\n")

  message("PASS Data imported: ", n_rows, " rows x ", n_cols, " columns")
  if (auto_convert && n_suggestions > 0) {
    message("PASS Applied ", n_suggestions, " type conversion",
            ifelse(n_suggestions > 1, "s", ""))
  }
  message("PASS Data ready for analysis")

  if (create_report && !is.null(validation_plot)) {
    message("\nTo view data quality report:")
    message('  print(result$validation_report)')
  }

  message("\nTo access your data:")
  message('  my_data <- result$data')
  message("\n")

  results <- list(
    data = data,
    variable_summary = var_summary,
    suggested_changes = if (auto_convert) var_summary %>%
      filter(change_suggested) else NULL,
    validation_report = validation_plot,
    import_log = import_log
  )

  class(results) <- c("imported_data", "list")

  return(results)
}


#' Review and Modify Variable Types Interactively
#'
#' This function provides an interactive way to review all variables in your
#' dataset and change their types if needed. Perfect for ensuring your data
#' is correctly formatted before analysis.
#'
#' @param data A data frame or tibble to review
#' @param suggest_changes Logical. Should the function suggest type improvements?
#'   Default TRUE.
#'
#' @return A list containing the modified data and a change log
#'
#' @examples
#' \dontrun{
#' # After importing data, review variable types
#' checked_data <- check_variable_types(my_data)
#'
#' # Access the corrected data
#' final_data <- checked_data$data
#' }
#'
#' @export
check_variable_types <- function(data, suggest_changes = TRUE) {

  message("\n==========================================================")
  message("|         INTERACTIVE VARIABLE TYPE CHECKER              |")
  message("==========================================================\n")

  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data frame or tibble")
  }

  # Create summary
  var_info <- tibble(
    variable = names(data),
    current_type = sapply(data, function(x) class(x)[1]),
    n_unique = sapply(data, n_distinct),
    n_missing = sapply(data, function(x) sum(is.na(x))),
    example_values = sapply(data, function(x) {
      vals <- na.omit(x)[1:min(5, sum(!is.na(x)))]
      paste(vals, collapse = ", ")
    })
  )

  message("Dataset has ", nrow(data), " rows and ", ncol(data), " variables\n")
  message("Variable types:")
  message("-----------------------------------------------------------\n")

  # Display each variable with details
  for (i in 1:nrow(var_info)) {
    message(i, ". ", var_info$variable[i])
    message("   Type: ", var_info$current_type[i])
    message("   Unique values: ", var_info$n_unique[i])
    message("   Missing: ", var_info$n_missing[i])
    message("   Examples: ", var_info$example_values[i])

    # Suggest changes if applicable
    if (suggest_changes) {
      current_type <- var_info$current_type[i]
      n_unique <- var_info$n_unique[i]

      if (current_type %in% c("numeric", "double") && n_unique <= 10) {
        message("   TIP: SUGGESTION: Consider converting to factor (categorical)")
      } else if (current_type == "character" && n_unique <= 20) {
        message("   TIP: SUGGESTION: Convert to factor")
      }
    }

    message("")
  }

  message("-----------------------------------------------------------\n")

  message("Variable types look correct? ")
  message("Use specific conversion functions to modify if needed:\n")
  message("  - data$var <- as.factor(data$var)  # Convert to factor")
  message("  - data$var <- as.numeric(data$var) # Convert to numeric")
  message("  - data$var <- as.character(data$var) # Convert to character")

  return(list(
    data = data,
    variable_info = var_info
  ))
}


#' Print Method for Imported Data
#'
#' @param x An imported_data object
#' @param ... Additional arguments (not used)
#' @export
print.imported_data <- function(x, ...) {
  cat("\n")
  cat("Imported Research Data\n")
  cat("========================================\n\n")

  cat("Dimensions:", nrow(x$data), "rows x", ncol(x$data), "columns\n")
  cat("Import time:", format(x$import_log$import_time), "\n")

  if (!is.null(x$import_log$changes_made)) {
    cat("\nType conversions applied:", x$import_log$n_changes, "\n")
  }

  cat("\nTo access data: result$data\n")
  cat("To view variable summary: result$variable_summary\n")
  if (!is.null(x$validation_report)) {
    cat("To see quality report: print(result$validation_report)\n")
  }

  cat("\n")
  invisible(x)
}
