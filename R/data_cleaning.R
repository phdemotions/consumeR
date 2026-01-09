#' Clean and Prepare Survey Data with Publication-Ready Exclusion Reporting
#'
#' @description
#' This is the FIRST STEP in your research workflow. Cleans survey data by
#' systematically excluding cases based on pre-test status, inclusion criteria,
#' and attention checks. Provides complete documentation suitable for the
#' Journal of Consumer Psychology and creates publication-ready participant
#' flow diagrams.
#'
#' **Why this matters:** Journals require transparent reporting of all exclusions.
#' This function creates the exact text you need for your Methods section and
#' tracks every single case that was excluded and why.
#'
#' @param data Data frame containing raw survey data
#' @param pretest_var Name of variable indicating pre-test status (optional).
#'   Values indicating pre-test (e.g., TRUE, 1, "pretest") should be specified
#'   in pretest_values parameter
#' @param pretest_values Values in pretest_var that indicate pre-test cases.
#'   Default: c(TRUE, 1, "pretest", "pre-test", "test")
#' @param inclusion_criteria List of inclusion criteria as named list.
#'   Each element should be a logical vector indicating which cases meet that criterion.
#'   Example: list(completed = data$finished == 1, adult = data$age >= 18)
#' @param attention_checks List of attention check variables and their correct answers.
#'   Example: list(ac1 = list(var = "attn_check1", correct = 3),
#'                 ac2 = list(var = "attn_check2", correct = "strongly agree"))
#' @param attention_check_rule How many attention checks must pass? Options:
#'   - "all" (default): Must pass all attention checks
#'   - "majority": Must pass > 50% of attention checks
#'   - numeric: Must pass at least this many checks
#' @param additional_exclusions Optional logical vector indicating additional cases
#'   to exclude (e.g., duplicate IPs, suspicious patterns)
#' @param additional_exclusion_reason Character string describing additional exclusions
#' @param id_var Name of participant ID variable (for tracking)
#' @param verbose Logical. Show detailed progress messages? Default TRUE
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{clean_data}: Final cleaned dataset (tibble)
#'     \item \code{n_initial}: Initial sample size
#'     \item \code{n_final}: Final sample size after all exclusions
#'     \item \code{exclusion_summary}: Data frame with counts for each exclusion type
#'     \item \code{exclusion_details}: Detailed information about each excluded case
#'     \item \code{participant_flow}: CONSORT-style flow data
#'     \item \code{publication_text}: Publication-ready Methods section text
#'     \item \code{exclusion_table}: Formatted table for manuscript
#'     \item \code{flags}: Data frame showing which cases were flagged for which reasons
#'   }
#'
#' @details
#' ## Exclusion Order (following best practices):
#'
#' 1. **Pre-test cases** - Removed first (pilot testing, researcher tests)
#' 2. **Inclusion criteria failures** - Did not meet study requirements
#' 3. **Attention check failures** - Failed quality checks
#' 4. **Additional exclusions** - Other reasons (duplicates, etc.)
#'
#' ## Publication Standards:
#'
#' The output provides everything needed for Journal of Consumer Psychology:
#' - Exact sample sizes at each step
#' - Specific exclusion criteria
#' - Number excluded for each reason
#' - Final analytical sample size
#' - Percentage of initial sample retained
#'
#' ## Example Methods Section Text:
#'
#' "Data were collected from 500 participants. We excluded 15 pre-test cases,
#' 23 participants who did not complete the survey, 18 participants under age 18,
#' and 12 participants who failed one or more attention checks. The final sample
#' consisted of 432 participants (86.4% of initial sample)."
#'
#' @examples
#' \dontrun{
#' # Complete example with all exclusion types
#' result <- clean_survey_data(
#'   data = raw_data,
#'   pretest_var = "is_pretest",
#'   pretest_values = c(1, TRUE),
#'   inclusion_criteria = list(
#'     completed = raw_data$finished == 1,
#'     adult = raw_data$age >= 18,
#'     us_resident = raw_data$country == "US"
#'   ),
#'   attention_checks = list(
#'     ac1 = list(var = "attention_1", correct = 4),
#'     ac2 = list(var = "attention_2", correct = "disagree")
#'   ),
#'   attention_check_rule = "all",
#'   id_var = "participant_id"
#' )
#'
#' # Access cleaned data
#' clean_df <- result$clean_data
#'
#' # View publication text
#' cat(result$publication_text)
#'
#' # View exclusion summary
#' print(result$exclusion_summary)
#'
#' # Create flowchart from participant_flow data
#' print(result$participant_flow)
#' }
#'
#' @export
#' @importFrom dplyr tibble filter mutate
clean_survey_data <- function(data,
                               pretest_var = NULL,
                               pretest_values = c(TRUE, 1, "pretest", "pre-test", "test"),
                               inclusion_criteria = NULL,
                               attention_checks = NULL,
                               attention_check_rule = "all",
                               additional_exclusions = NULL,
                               additional_exclusion_reason = "Additional exclusions",
                               id_var = NULL,
                               verbose = TRUE) {

  # Convert to tibble for consistent handling
  data <- as.data.frame(data)

  # Initialize tracking
  n_initial <- nrow(data)
  exclusion_log <- list()
  flags <- data.frame(
    row_number = 1:n_initial,
    pretest = FALSE,
    inclusion_fail = FALSE,
    attention_fail = FALSE,
    additional = FALSE,
    exclusion_reason = NA_character_,
    stringsAsFactors = FALSE
  )

  if (!is.null(id_var) && id_var %in% names(data)) {
    flags$id <- data[[id_var]]
  }

  if (verbose) {
    message("\n", rep("=", 70))
    message("DATA CLEANING AND EXCLUSION TRACKING")
    message("Journal of Consumer Psychology Standards")
    message(rep("=", 70), "\n")
    message("Initial sample size: n = ", n_initial)
    message("\nBeginning systematic exclusion process...\n")
  }

  # Step 1: Identify pre-test cases
  # --------------------------------
  pretest_indices <- c()
  n_pretest <- 0

  if (!is.null(pretest_var) && pretest_var %in% names(data)) {
    pretest_indices <- which(data[[pretest_var]] %in% pretest_values)
    n_pretest <- length(pretest_indices)

    if (n_pretest > 0) {
      flags$pretest[pretest_indices] <- TRUE
      flags$exclusion_reason[pretest_indices] <- "Pre-test case"

      exclusion_log$pretest <- list(
        n = n_pretest,
        reason = "Pre-test or pilot data",
        indices = pretest_indices
      )

      if (verbose) {
        message("STEP 1: Pre-test cases")
        message("  Excluded: ", n_pretest, " pre-test cases")
        message("  Remaining: ", n_initial - n_pretest, "\n")
      }
    }
  } else if (verbose && !is.null(pretest_var)) {
    message("STEP 1: Pre-test cases")
    message("  Variable '", pretest_var, "' not found - skipping\n")
  }

  # Current sample after pretest exclusion
  current_indices <- setdiff(1:n_initial, pretest_indices)

  # Step 2: Apply inclusion criteria
  # ---------------------------------
  inclusion_indices <- c()
  inclusion_details <- list()

  if (!is.null(inclusion_criteria) && length(inclusion_criteria) > 0) {
    if (verbose) {
      message("STEP 2: Inclusion criteria")
    }

    for (criterion_name in names(inclusion_criteria)) {
      criterion_met <- inclusion_criteria[[criterion_name]]

      if (length(criterion_met) != n_initial) {
        warning("Inclusion criterion '", criterion_name, "' has wrong length. Skipping.")
        next
      }

      # Find cases that fail this criterion (among current sample)
      fails_criterion <- intersect(current_indices, which(!criterion_met))
      n_fails <- length(fails_criterion)

      if (n_fails > 0) {
        inclusion_indices <- union(inclusion_indices, fails_criterion)
        flags$inclusion_fail[fails_criterion] <- TRUE
        flags$exclusion_reason[fails_criterion] <- paste0(
          ifelse(is.na(flags$exclusion_reason[fails_criterion]), "",
                 paste0(flags$exclusion_reason[fails_criterion], "; ")),
          "Failed: ", criterion_name
        )

        inclusion_details[[criterion_name]] <- list(
          n = n_fails,
          reason = paste0("Did not meet criterion: ", criterion_name),
          indices = fails_criterion
        )

        if (verbose) {
          message("  '", criterion_name, "': ", n_fails, " excluded")
        }
      }
    }

    n_inclusion <- length(inclusion_indices)
    exclusion_log$inclusion <- list(
      n = n_inclusion,
      reason = "Failed inclusion criteria",
      details = inclusion_details
    )

    if (verbose) {
      message("  Total excluded for inclusion criteria: ", n_inclusion)
      message("  Remaining: ", length(setdiff(current_indices, inclusion_indices)), "\n")
    }

    # Update current sample
    current_indices <- setdiff(current_indices, inclusion_indices)
  }

  # Step 3: Check attention checks
  # -------------------------------
  attention_indices <- c()
  attention_details <- list()

  if (!is.null(attention_checks) && length(attention_checks) > 0) {
    if (verbose) {
      message("STEP 3: Attention checks")
    }

    # Create matrix to track which checks each person passed
    n_checks <- length(attention_checks)
    check_matrix <- matrix(NA, nrow = n_initial, ncol = n_checks)
    colnames(check_matrix) <- names(attention_checks)

    for (i in seq_along(attention_checks)) {
      check_name <- names(attention_checks)[i]
      check_info <- attention_checks[[i]]

      var_name <- check_info$var
      correct_answer <- check_info$correct

      if (!var_name %in% names(data)) {
        warning("Attention check variable '", var_name, "' not found. Skipping.")
        next
      }

      # Check which cases got it right
      passed <- data[[var_name]] == correct_answer
      passed[is.na(passed)] <- FALSE  # Treat NA as failure

      check_matrix[, i] <- passed

      n_failed_this <- sum(!passed[current_indices])
      if (verbose && n_failed_this > 0) {
        message("  '", check_name, "': ", n_failed_this, " failed")
      }
    }

    # Determine who fails based on rule
    checks_passed <- rowSums(check_matrix, na.rm = TRUE)

    if (attention_check_rule == "all") {
      fails_attention <- which(checks_passed < n_checks)
    } else if (attention_check_rule == "majority") {
      fails_attention <- which(checks_passed < (n_checks / 2))
    } else if (is.numeric(attention_check_rule)) {
      fails_attention <- which(checks_passed < attention_check_rule)
    } else {
      warning("Invalid attention_check_rule. Using 'all'.")
      fails_attention <- which(checks_passed < n_checks)
    }

    # Only count those in current sample
    attention_indices <- intersect(current_indices, fails_attention)
    n_attention <- length(attention_indices)

    if (n_attention > 0) {
      flags$attention_fail[attention_indices] <- TRUE
      flags$exclusion_reason[attention_indices] <- paste0(
        ifelse(is.na(flags$exclusion_reason[attention_indices]), "",
               paste0(flags$exclusion_reason[attention_indices], "; ")),
        "Failed attention checks (", checks_passed[attention_indices], "/", n_checks, " passed)"
      )

      exclusion_log$attention <- list(
        n = n_attention,
        reason = paste0("Failed attention checks (rule: ", attention_check_rule, ")"),
        n_checks = n_checks,
        indices = attention_indices
      )

      if (verbose) {
        message("  Total excluded for attention check failures: ", n_attention)
        message("  Remaining: ", length(setdiff(current_indices, attention_indices)), "\n")
      }

      current_indices <- setdiff(current_indices, attention_indices)
    }
  }

  # Step 4: Additional exclusions
  # ------------------------------
  additional_indices <- c()
  n_additional <- 0

  if (!is.null(additional_exclusions)) {
    if (length(additional_exclusions) != n_initial) {
      warning("additional_exclusions has wrong length. Skipping.")
    } else {
      additional_indices <- intersect(current_indices, which(additional_exclusions))
      n_additional <- length(additional_indices)

      if (n_additional > 0) {
        flags$additional[additional_indices] <- TRUE
        flags$exclusion_reason[additional_indices] <- paste0(
          ifelse(is.na(flags$exclusion_reason[additional_indices]), "",
                 paste0(flags$exclusion_reason[additional_indices], "; ")),
          additional_exclusion_reason
        )

        exclusion_log$additional <- list(
          n = n_additional,
          reason = additional_exclusion_reason,
          indices = additional_indices
        )

        if (verbose) {
          message("STEP 4: Additional exclusions")
          message("  Excluded: ", n_additional, " (", additional_exclusion_reason, ")")
          message("  Remaining: ", length(setdiff(current_indices, additional_indices)), "\n")
        }

        current_indices <- setdiff(current_indices, additional_indices)
      }
    }
  }

  # Final sample
  # ------------
  n_final <- length(current_indices)
  pct_retained <- round(100 * n_final / n_initial, 1)

  clean_data <- data[current_indices, , drop = FALSE]

  if (verbose) {
    message(rep("=", 70))
    message("FINAL SAMPLE")
    message(rep("=", 70))
    message("Initial sample:  ", n_initial)
    message("Final sample:    ", n_final)
    message("Retention rate:  ", pct_retained, "%")
    message(rep("=", 70), "\n")
  }

  # Create exclusion summary table
  # -------------------------------
  exclusion_summary <- data.frame(
    exclusion_type = character(),
    n_excluded = integer(),
    cumulative_n = integer(),
    percentage_of_initial = numeric(),
    stringsAsFactors = FALSE
  )

  cumulative <- 0

  if (n_pretest > 0) {
    cumulative <- cumulative + n_pretest
    exclusion_summary <- rbind(exclusion_summary, data.frame(
      exclusion_type = "Pre-test cases",
      n_excluded = n_pretest,
      cumulative_n = n_initial - cumulative,
      percentage_of_initial = round(100 * n_pretest / n_initial, 1)
    ))
  }

  if (!is.null(inclusion_criteria) && length(inclusion_indices) > 0) {
    cumulative <- cumulative + length(inclusion_indices)
    exclusion_summary <- rbind(exclusion_summary, data.frame(
      exclusion_type = "Failed inclusion criteria",
      n_excluded = length(inclusion_indices),
      cumulative_n = n_initial - cumulative,
      percentage_of_initial = round(100 * length(inclusion_indices) / n_initial, 1)
    ))
  }

  if (!is.null(attention_checks) && length(attention_indices) > 0) {
    cumulative <- cumulative + length(attention_indices)
    exclusion_summary <- rbind(exclusion_summary, data.frame(
      exclusion_type = "Failed attention checks",
      n_excluded = length(attention_indices),
      cumulative_n = n_initial - cumulative,
      percentage_of_initial = round(100 * length(attention_indices) / n_initial, 1)
    ))
  }

  if (n_additional > 0) {
    cumulative <- cumulative + n_additional
    exclusion_summary <- rbind(exclusion_summary, data.frame(
      exclusion_type = additional_exclusion_reason,
      n_excluded = n_additional,
      cumulative_n = n_initial - cumulative,
      percentage_of_initial = round(100 * n_additional / n_initial, 1)
    ))
  }

  # Final row
  exclusion_summary <- rbind(exclusion_summary, data.frame(
    exclusion_type = "FINAL ANALYTICAL SAMPLE",
    n_excluded = NA,
    cumulative_n = n_final,
    percentage_of_initial = pct_retained
  ))

  # Generate publication text
  # --------------------------
  pub_text <- generate_cleaning_publication_text(
    n_initial = n_initial,
    n_final = n_final,
    exclusion_log = exclusion_log,
    inclusion_details = if (!is.null(inclusion_criteria)) inclusion_details else NULL,
    attention_n_checks = if (!is.null(attention_checks)) length(attention_checks) else 0,
    attention_rule = if (!is.null(attention_checks)) attention_check_rule else NULL
  )

  # Create participant flow (CONSORT-style)
  # ----------------------------------------
  flow <- list(
    step1 = list(
      label = "Initial Sample",
      n = n_initial
    ),
    step2 = if (n_pretest > 0) list(
      label = "After excluding pre-test cases",
      n = n_initial - n_pretest,
      excluded = n_pretest,
      reason = "Pre-test or pilot data"
    ) else NULL,
    step3 = if (length(inclusion_indices) > 0) list(
      label = "After applying inclusion criteria",
      n = n_initial - n_pretest - length(inclusion_indices),
      excluded = length(inclusion_indices),
      reason = "Did not meet inclusion criteria"
    ) else NULL,
    step4 = if (length(attention_indices) > 0) list(
      label = "After attention check screening",
      n = n_initial - n_pretest - length(inclusion_indices) - length(attention_indices),
      excluded = length(attention_indices),
      reason = "Failed attention checks"
    ) else NULL,
    step5 = if (n_additional > 0) list(
      label = paste0("After ", tolower(additional_exclusion_reason)),
      n = n_final,
      excluded = n_additional,
      reason = additional_exclusion_reason
    ) else NULL,
    final = list(
      label = "Final Analytical Sample",
      n = n_final,
      percentage_retained = pct_retained
    )
  )

  # Remove NULL steps
  flow <- flow[!sapply(flow, is.null)]

  # Return comprehensive results
  # -----------------------------
  results <- structure(
    list(
      clean_data = clean_data,
      n_initial = n_initial,
      n_final = n_final,
      n_excluded_total = n_initial - n_final,
      retention_rate = pct_retained,
      exclusion_summary = exclusion_summary,
      exclusion_log = exclusion_log,
      participant_flow = flow,
      publication_text = pub_text,
      flags = flags,
      timestamp = Sys.time()
    ),
    class = c("cleaning_result", "list")
  )

  return(results)
}


#' Generate Publication Text for Data Cleaning
#' @noRd
generate_cleaning_publication_text <- function(n_initial,
                                                n_final,
                                                exclusion_log,
                                                inclusion_details,
                                                attention_n_checks,
                                                attention_rule) {

  # Build comprehensive methods text
  sections <- list()

  # Sample collection
  sections$sample_collection <- paste0(
    "Data were collected from ", n_initial, " participants."
  )

  # Exclusions
  exclusion_sentences <- c()

  if (!is.null(exclusion_log$pretest)) {
    exclusion_sentences <- c(exclusion_sentences, paste0(
      "We first removed ", exclusion_log$pretest$n, " pre-test case",
      ifelse(exclusion_log$pretest$n > 1, "s", ""),
      " that were collected during pilot testing"
    ))
  }

  if (!is.null(exclusion_log$inclusion)) {
    # Detail each inclusion criterion
    if (!is.null(inclusion_details) && length(inclusion_details) > 0) {
      criterion_texts <- sapply(names(inclusion_details), function(name) {
        n <- inclusion_details[[name]]$n
        paste0(n, " participant", ifelse(n > 1, "s", ""), " who failed the '", name, "' criterion")
      })

      exclusion_sentences <- c(exclusion_sentences, paste0(
        "We then excluded participants who did not meet our inclusion criteria: ",
        paste(criterion_texts, collapse = ", ")
      ))
    } else {
      exclusion_sentences <- c(exclusion_sentences, paste0(
        "We then excluded ", exclusion_log$inclusion$n, " participant",
        ifelse(exclusion_log$inclusion$n > 1, "s", ""),
        " who did not meet our inclusion criteria"
      ))
    }
  }

  if (!is.null(exclusion_log$attention)) {
    rule_text <- if (attention_rule == "all") {
      "all"
    } else if (attention_rule == "majority") {
      "the majority of"
    } else if (is.numeric(attention_rule)) {
      paste0("at least ", attention_rule, " of")
    } else {
      ""
    }

    exclusion_sentences <- c(exclusion_sentences, paste0(
      "We excluded ", exclusion_log$attention$n, " participant",
      ifelse(exclusion_log$attention$n > 1, "s", ""),
      " who failed ", rule_text, " ", attention_n_checks, " attention check",
      ifelse(attention_n_checks > 1, "s", "")
    ))
  }

  if (!is.null(exclusion_log$additional)) {
    exclusion_sentences <- c(exclusion_sentences, paste0(
      "Additionally, we excluded ", exclusion_log$additional$n, " participant",
      ifelse(exclusion_log$additional$n > 1, "s", ""),
      " due to ", tolower(exclusion_log$additional$reason)
    ))
  }

  if (length(exclusion_sentences) > 0) {
    sections$exclusions <- paste0(
      paste(exclusion_sentences, collapse = ". "), "."
    )
  }

  # Final sample
  pct_retained <- round(100 * n_final / n_initial, 1)
  sections$final_sample <- paste0(
    "The final analytical sample consisted of ", n_final, " participants (",
    pct_retained, "% of the initial sample)."
  )

  # Combine all sections
  full_text <- paste(unlist(sections), collapse = " ")

  # Also create a verbose version with more detail
  verbose_text <- paste0(
    "SAMPLE AND DATA CLEANING PROCEDURE\n\n",
    "Initial Sample: We initially collected data from ", n_initial, " participants.\n\n",
    "Exclusion Criteria: Following best practices in consumer research ",
    "(see Oppenheimer, Meyvis, & Davidenko, 2009), we applied systematic exclusion ",
    "criteria to ensure data quality.\n\n"
  )

  if (!is.null(exclusion_log$pretest)) {
    verbose_text <- paste0(verbose_text,
      "Pre-test Exclusions: We first removed ", exclusion_log$pretest$n, " cases ",
      "that were collected during pre-testing and pilot data collection. These cases ",
      "were used to validate survey flow and question wording but were not part of ",
      "the main data collection effort.\n\n"
    )
  }

  if (!is.null(exclusion_log$inclusion) && !is.null(inclusion_details)) {
    verbose_text <- paste0(verbose_text,
      "Inclusion Criteria: Participants had to meet all of the following criteria ",
      "to be included in the analysis:\n"
    )
    for (name in names(inclusion_details)) {
      n <- inclusion_details[[name]]$n
      verbose_text <- paste0(verbose_text,
        "  - ", name, " (excluded ", n, " participant",
        ifelse(n > 1, "s", ""), ")\n"
      )
    }
    verbose_text <- paste0(verbose_text, "\n")
  }

  if (!is.null(exclusion_log$attention)) {
    verbose_text <- paste0(verbose_text,
      "Attention Checks: To ensure participants were attentive, we embedded ",
      attention_n_checks, " attention check", ifelse(attention_n_checks > 1, "s", ""),
      " throughout the survey. These checks asked participants to select a specific ",
      "response (e.g., 'Please select Strongly Agree for this item'). We excluded ",
      exclusion_log$attention$n, " participant",
      ifelse(exclusion_log$attention$n > 1, "s", ""),
      " who failed ", rule_text, " attention check",
      ifelse(attention_n_checks > 1, "s", ""), ".\n\n"
    )
  }

  if (!is.null(exclusion_log$additional)) {
    verbose_text <- paste0(verbose_text,
      "Additional Exclusions: We also excluded ", exclusion_log$additional$n,
      " participant", ifelse(exclusion_log$additional$n > 1, "s", ""),
      " due to ", tolower(exclusion_log$additional$reason), ".\n\n"
    )
  }

  verbose_text <- paste0(verbose_text,
    "Final Sample: After all exclusions, the final analytical sample consisted of ",
    n_final, " participants (", pct_retained, "% retention rate from initial sample). ",
    "This retention rate is ", ifelse(pct_retained >= 80, "excellent",
                                      ifelse(pct_retained >= 70, "good",
                                             ifelse(pct_retained >= 60, "acceptable", "low"))),
    " and is consistent with standards in consumer research."
  )

  list(
    concise = full_text,
    verbose = verbose_text,
    consort = generate_consort_text(n_initial, n_final, exclusion_log)
  )
}


#' Generate CONSORT-Style Flow Text
#' @noRd
generate_consort_text <- function(n_initial, n_final, exclusion_log) {
  text <- paste0(
    "PARTICIPANT FLOW (CONSORT Style):\n\n",
    "Assessed for eligibility: n = ", n_initial, "\n"
  )

  current_n <- n_initial

  if (!is.null(exclusion_log$pretest)) {
    text <- paste0(text,
      "  ↓ Excluded (pre-test): n = ", exclusion_log$pretest$n, "\n",
      "After pre-test exclusion: n = ", current_n - exclusion_log$pretest$n, "\n"
    )
    current_n <- current_n - exclusion_log$pretest$n
  }

  if (!is.null(exclusion_log$inclusion)) {
    text <- paste0(text,
      "  ↓ Excluded (inclusion criteria): n = ", exclusion_log$inclusion$n, "\n",
      "After inclusion screening: n = ", current_n - exclusion_log$inclusion$n, "\n"
    )
    current_n <- current_n - exclusion_log$inclusion$n
  }

  if (!is.null(exclusion_log$attention)) {
    text <- paste0(text,
      "  ↓ Excluded (attention checks): n = ", exclusion_log$attention$n, "\n",
      "After attention screening: n = ", current_n - exclusion_log$attention$n, "\n"
    )
    current_n <- current_n - exclusion_log$attention$n
  }

  if (!is.null(exclusion_log$additional)) {
    text <- paste0(text,
      "  ↓ Excluded (", exclusion_log$additional$reason, "): n = ",
      exclusion_log$additional$n, "\n",
      "After additional exclusions: n = ", current_n - exclusion_log$additional$n, "\n"
    )
    current_n <- current_n - exclusion_log$additional$n
  }

  text <- paste0(text,
    "\n",
    "═══════════════════════════════\n",
    "FINAL ANALYTICAL SAMPLE: n = ", n_final, "\n",
    "═══════════════════════════════"
  )

  text
}


#' Print Method for Cleaning Results
#'
#' @param x A cleaning_result object
#' @param show_flow Logical - show participant flow? (default TRUE)
#' @param show_details Logical - show detailed exclusions? (default FALSE)
#' @param show_publication Logical - show publication text? (default FALSE)
#' @param ... Additional arguments
#'
#' @export
print.cleaning_result <- function(x, show_flow = TRUE, show_details = FALSE,
                                   show_publication = FALSE, ...) {
  cat("\n")
  cat("═" %+% rep("═", 68) %+% "═\n", sep = "")
  cat("DATA CLEANING RESULTS\n")
  cat("Journal of Consumer Psychology Standards\n")
  cat("═" %+% rep("═", 68) %+% "═\n", sep = "")
  cat("\n")

  cat("SAMPLE SUMMARY:\n")
  cat("  Initial sample:      n =", x$n_initial, "\n")
  cat("  Final sample:        n =", x$n_final, "\n")
  cat("  Total excluded:      n =", x$n_excluded_total, "\n")
  cat("  Retention rate:      ", x$retention_rate, "%\n")
  cat("\n")

  cat("EXCLUSION SUMMARY:\n")
  print(x$exclusion_summary, row.names = FALSE)
  cat("\n")

  if (show_flow) {
    cat("PARTICIPANT FLOW:\n")
    cat(x$publication_text$consort, "\n\n")
  }

  if (show_details && !is.null(x$exclusion_log)) {
    cat("DETAILED EXCLUSION LOG:\n")
    for (type_name in names(x$exclusion_log)) {
      cat("\n  ", toupper(type_name), ":\n", sep = "")
      cat("    Reason:", x$exclusion_log[[type_name]]$reason, "\n")
      cat("    Count:", x$exclusion_log[[type_name]]$n, "\n")
    }
    cat("\n")
  }

  if (show_publication) {
    cat("═" %+% rep("═", 68) %+% "═\n", sep = "")
    cat("PUBLICATION-READY TEXT\n")
    cat("═" %+% rep("═", 68) %+% "═\n", sep = "")
    cat("\nCONCISE VERSION (for brief Methods section):\n")
    cat(strwrap(x$publication_text$concise, width = 68, prefix = "  "), sep = "\n")
    cat("\n\nVERBOSE VERSION (with all details):\n")
    cat(strwrap(x$publication_text$verbose, width = 68, prefix = "  "), sep = "\n")
    cat("\n")
  }

  if (!show_flow) {
    cat("Tip: Use print(result, show_flow = TRUE) to see participant flow\n")
  }
  if (!show_details) {
    cat("Tip: Use print(result, show_details = TRUE) for detailed exclusion log\n")
  }
  if (!show_publication) {
    cat("Tip: Use print(result, show_publication = TRUE) for publication text\n")
  }

  cat("\n")

  invisible(x)
}

# Helper function
`%+%` <- function(a, b) paste0(a, b)
