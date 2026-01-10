#' Clean and Prepare Survey Data with Publication-Ready Exclusion Reporting
#'
#' @description
#' **This is STEP 1 in your research workflow.** Cleans survey data by systematically
#' excluding cases and creates publication-ready participant flow documentation.
#'
#' **What this function does:**
#' - Removes pre-test/pilot data (if applicable)
#' - Excludes participants who don't meet your inclusion criteria
#' - Removes participants who failed attention checks
#' - Tracks every exclusion with detailed reasons
#' - Generates publication text for your Methods section
#'
#' @param data Data frame with your raw survey data (usually from Qualtrics, MTurk, etc.)
#'
#' @param pretest_var **Column name** that marks pre-test cases (or NULL if no pre-tests).
#'
#'   **What this is:** The name of the column in your data that identifies pre-test responses.
#'   Pre-tests are responses you collected BEFORE your main study (pilot testing, your own
#'   test responses, etc.). These shouldn't be counted in your sample size.
#'
#'   **Real examples:**
#'   - Qualtrics often creates: `"Status"` (values: "Survey Preview" = pre-test)
#'   - Or you might have: `"is_pilot"`, `"test_response"`, `"preview"`
#'   - MTurk: You might mark your own tests with a specific worker ID
#'
#'   **How to find it:**
#'   Look at your column names: `names(raw_data)` or `colnames(raw_data)`
#'
#'   **If you don't have pre-tests:** Just use `pretest_var = NULL` (the default)
#'
#' @param pretest_values **Values** in the pretest_var column that indicate it's a pre-test.
#'
#'   **What this is:** What values in that column mean "this is a pre-test"?
#'
#'   **Real examples:**
#'   - If using Qualtrics Status: `pretest_values = "Survey Preview"`
#'   - If you have a TRUE/FALSE column: `pretest_values = c(TRUE, 1, "yes")`
#'   - If you marked tests with special ID: `pretest_values = "RESEARCHER_TEST"`
#'
#'   **Default:** c(TRUE, 1, "pretest", "pre-test", "test") - catches common cases
#'
#' @param date_var **Column name** containing response dates/timestamps (or NULL).
#'
#'   **What this is:** The column that shows when each response was collected.
#'   Use this if you want to exclude responses collected before a certain date.
#'
#'   **Real examples:**
#'   - Qualtrics: `"StartDate"`, `"EndDate"`, or `"RecordedDate"`
#'   - MTurk: `"SubmitTime"` or `"AcceptTime"`
#'   - Your own: Whatever timestamp column you have
#'
#'   **How to find it:**
#'   Look for date/time columns: `names(raw_data)` - often contains "Date" or "Time"
#'
#'   **Note:** Works with most date formats (automatically converted)
#'
#' @param pretest_before_date **Cutoff date** - exclude responses collected before this date (or NULL).
#'
#'   **What this is:** Any responses collected BEFORE this date will be treated as pre-tests.
#'   Useful when you did pilot testing before your main data collection.
#'
#'   **Real examples:**
#'   ```r
#'   # If you started real data collection on March 15, 2024:
#'   pretest_before_date = "2024-03-15"
#'
#'   # Or use a Date object:
#'   pretest_before_date = as.Date("2024-03-15")
#'
#'   # With specific time (if you have timestamps):
#'   pretest_before_date = as.POSIXct("2024-03-15 09:00:00")
#'   ```
#'
#'   **Date formats accepted:**
#'   - "YYYY-MM-DD" (recommended): `"2024-03-15"`
#'   - Date objects: `as.Date("2024-03-15")`
#'   - POSIXct timestamps: `as.POSIXct("2024-03-15 09:00:00")`
#'
#'   **How to use with `date_var`:**
#'   Both must be provided together. The function will exclude any rows where
#'   `date_var < pretest_before_date`
#'
#'   **Can combine with `pretest_var`:** If you provide both date-based and column-based
#'   pre-test criteria, responses matching EITHER will be removed
#'
#' @param inclusion_criteria **Named list** of requirements participants must meet.
#'
#'   **What this is:** Rules that determine if someone should be INCLUDED in your analysis.
#'   Create a list where each element is a TRUE/FALSE check.
#'
#'   **Structure:** `list(criterion_name = TRUE/FALSE for each row)`
#'
#'   **Real examples:**
#'
#'   ```r
#'   # Example 1: Basic Qualtrics survey
#'   inclusion_criteria = list(
#'     completed = raw_data$Finished == 1,              # Qualtrics completion flag
#'     adult = raw_data$age >= 18,                      # Age requirement
#'     consented = raw_data$consent == "I agree"        # Gave consent
#'   )
#'
#'   # Example 2: MTurk study
#'   inclusion_criteria = list(
#'     completed = raw_data$status == "Submitted",
#'     approval_rate = raw_data$approval >= 95,         # MTurk 95%+ approval
#'     min_hits = raw_data$total_hits >= 100,           # At least 100 HITs
#'     us_only = raw_data$location == "US"
#'   )
#'
#'   # Example 3: Just want people who finished
#'   inclusion_criteria = list(
#'     completed = raw_data$Finished == 1
#'   )
#'   ```
#'
#'   **How to figure out column names:**
#'   - Run: `names(raw_data)` to see all columns
#'   - Look for: "Finished", "Status", "Progress", etc.
#'   - Check what values mean completion: `table(raw_data$Finished)`
#'
#' @param attention_checks **Named list** of attention check questions and correct answers.
#'
#'   **What this is:** Questions where you told participants "Select [specific answer]".
#'   These catch people who aren't paying attention.
#'
#'   **Structure:** Each attention check needs:
#'   - `var`: The column name for that question
#'   - `correct`: The correct answer
#'
#'   **Real examples:**
#'
#'   ```r
#'   # Example 1: Likert scale attention checks (1-7 scale)
#'   attention_checks = list(
#'     ac1 = list(var = "Q5_1", correct = 7),           # "Select Strongly Agree (7)"
#'     ac2 = list(var = "Q12_1", correct = 1)           # "Select Strongly Disagree (1)"
#'   )
#'
#'   # Example 2: Text-based attention checks
#'   attention_checks = list(
#'     ac1 = list(var = "attention_1", correct = "blue"),       # "Select 'blue'"
#'     ac2 = list(var = "attention_2", correct = "strongly agree")  # Exact text match
#'   )
#'
#'   # Example 3: Multiple choice (numeric coding)
#'   attention_checks = list(
#'     ac1 = list(var = "AC_Q3", correct = 4)           # Option 4 was correct
#'   )
#'   ```
#'
#'   **How to find column names:**
#'   - In Qualtrics: Questions are often "Q1", "Q2", "Q3", etc.
#'   - Look at: `names(raw_data)` to find your attention check columns
#'   - Check what values people selected: `table(raw_data$Q5_1)`
#'
#'   **What's the correct answer?**
#'   - Look at your survey - what did you tell people to select?
#'   - Check your data: `unique(raw_data$Q5_1)` to see possible values
#'
#' @param attention_check_rule **How many** attention checks must someone pass?
#'
#'   **Options:**
#'   - `"all"` (default and recommended): Must pass EVERY attention check
#'   - `"majority"`: Must pass more than half (e.g., 2 out of 3)
#'   - A number: Must pass at least this many (e.g., `2` means pass at least 2)
#'
#'   **Examples:**
#'   - If you have 3 attention checks and want to be strict: `"all"`
#'   - If you want to be lenient (allow 1 failure): `2` (must pass 2 out of 3)
#'   - If you have many checks and want majority: `"majority"`
#'
#'   **Recommendation:** Use `"all"` - it's most common in publications
#'
#' @param additional_exclusions **TRUE/FALSE vector** for any other exclusions.
#'
#'   **What this is:** Any other reason to exclude someone (duplicates, bots, etc.)
#'
#'   **Real examples:**
#'
#'   ```r
#'   # Exclude duplicate IP addresses
#'   additional_exclusions = duplicated(raw_data$IPAddress)
#'
#'   # Exclude people who finished too fast (< 2 minutes)
#'   additional_exclusions = raw_data$Duration_seconds < 120
#'
#'   # Exclude based on multiple criteria
#'   additional_exclusions = duplicated(raw_data$IPAddress) |
#'                          raw_data$Duration_seconds < 120
#'   ```
#'
#' @param additional_exclusion_reason **Text description** of those exclusions.
#'
#'   **Examples:**
#'   - `"Duplicate IP addresses"`
#'   - `"Completed survey too quickly (< 2 minutes)"`
#'   - `"Failed quality checks"`
#'
#' @param rename_vars **Named list** for renaming variables (or NULL).
#'
#'   **What this is:** Renames columns from Qualtrics' random names to meaningful names.
#'
#'   **Real examples:**
#'   ```r
#'   # Qualtrics often names questions Q1, Q2, Q3...
#'   # Give them meaningful names:
#'   rename_vars = list(
#'     brand_attitude = "Q1",           # Rename Q1 to brand_attitude
#'     purchase_intent = "Q2",          # Rename Q2 to purchase_intent
#'     age = "Q17",                     # Rename Q17 to age
#'     condition = "FL_16_DO"           # Rename Qualtrics flow to condition
#'   )
#'   ```
#'
#'   **Structure:** `list(new_name = "old_name")`
#'   - Left side = what you WANT to call it
#'   - Right side = what Qualtrics CURRENTLY calls it
#'
#'   **How to find old names:** Run `names(raw_data)` to see current column names
#'
#' @param recode_vars **Named list** for recoding variable values (or NULL).
#'
#'   **What this is:** Changes coded values to meaningful labels.
#'   Useful for converting numeric codes to text labels or fixing typos.
#'
#'   **Real examples:**
#'   ```r
#'   # Example 1: Recode numeric condition codes to text labels
#'   recode_vars = list(
#'     condition = c("1" = "control", "2" = "treatment_A", "3" = "treatment_B")
#'   )
#'
#'   # Example 2: Fix inconsistent text responses
#'   recode_vars = list(
#'     gender = c("m" = "Male", "M" = "Male", "male" = "Male",
#'                "f" = "Female", "F" = "Female", "female" = "Female")
#'   )
#'
#'   # Example 3: Recode multiple variables
#'   recode_vars = list(
#'     condition = c("1" = "control", "2" = "treatment"),
#'     consent = c("1" = "yes", "2" = "no"),
#'     likert_scale = c("1" = "Strongly Disagree", "2" = "Disagree",
#'                      "3" = "Neutral", "4" = "Agree", "5" = "Strongly Agree")
#'   )
#'   ```
#'
#'   **Structure:** `list(var_name = c("old" = "new", "old2" = "new2"))`
#'
#'   **Note:** Use the NEW names if you're also using `rename_vars`
#'
#' @param id_var **Column name** with participant IDs (for tracking).
#'
#'   **Common names:**
#'   - Qualtrics: `"ResponseId"` or `"ResponseID"`
#'   - MTurk: `"WorkerId"` or `"worker_id"`
#'   - Prolific: `"PROLIFIC_PID"`
#'   - Your own: Whatever you named it
#'
#'   **To find it:** Run `names(raw_data)` and look for ID column
#'
#' @param verbose Show detailed progress messages? (TRUE = yes, see what's happening)
#'
#' @return A list with:
#'   - `clean_data`: Your cleaned dataset (use this for analysis!)
#'   - `n_initial`: Initial sample size (AFTER removing pre-tests)
#'   - `n_final`: Final sample size
#'   - `exclusion_summary`: Table of exclusions (for your manuscript)
#'   - `publication_text`: Copy-paste into your Methods section
#'   - `flags`: Which participants were excluded for which reasons
#'   - `renamed_vars`: Which variables were renamed (old → new)
#'   - `recoded_vars`: Which variables had values recoded
#'
#' @section For Novice Researchers:
#'
#' **Confused about what to put in each parameter?** Here's the easiest approach:
#'
#' 1. **First, look at your column names:**
#'    ```r
#'    names(raw_data)  # Shows all column names
#'    ```
#'
#' 2. **Start simple - just exclude incomplete responses:**
#'    ```r
#'    result <- clean_survey_data(
#'      data = raw_data,
#'      inclusion_criteria = list(
#'        completed = raw_data$Finished == 1  # Qualtrics completion column
#'      )
#'    )
#'    ```
#'
#' 3. **Add attention checks (if you have them):**
#'    ```r
#'    # First, figure out which columns are your attention checks
#'    names(raw_data)  # Look for your attention check question numbers
#'
#'    result <- clean_survey_data(
#'      data = raw_data,
#'      inclusion_criteria = list(
#'        completed = raw_data$Finished == 1
#'      ),
#'      attention_checks = list(
#'        # Replace Q5 with your actual question number
#'        ac1 = list(var = "Q5", correct = 7)  # Whatever answer was correct
#'      )
#'    )
#'    ```
#'
#' 4. **Add more criteria as needed:**
#'    ```r
#'    result <- clean_survey_data(
#'      data = raw_data,
#'      inclusion_criteria = list(
#'        completed = raw_data$Finished == 1,
#'        adult = raw_data$age >= 18,
#'        consented = raw_data$consent == "I agree"
#'      ),
#'      attention_checks = list(
#'        ac1 = list(var = "Q5", correct = 7),
#'        ac2 = list(var = "Q12", correct = "agree")
#'      ),
#'      id_var = "ResponseId"
#'    )
#'    ```
#'
#' **Still confused?** Common issues:
#' - **Don't know column names?** Run: `names(raw_data)` or `colnames(raw_data)`
#' - **Don't know what values mean completion?** Run: `table(raw_data$Finished)`
#' - **Don't have attention checks?** Just leave out the `attention_checks` parameter
#' - **Don't have pre-tests?** Leave out `pretest_var` (it's NULL by default)
#' - **Want to remove pre-tests by date instead?** Use `date_var` + `pretest_before_date`
#'   (e.g., if you started real data collection on March 1, exclude anything before that date)
#'
#' @examples
#' \dontrun{
#' # ========================================================================
#' # EXAMPLE 1: Minimal - Just exclude incomplete responses
#' # ========================================================================
#'
#' result <- clean_survey_data(
#'   data = raw_data,
#'   inclusion_criteria = list(
#'     completed = raw_data$Finished == 1
#'   )
#' )
#'
#' # ========================================================================
#' # EXAMPLE 2: Typical Qualtrics study with attention checks
#' # ========================================================================
#'
#' result <- clean_survey_data(
#'   data = raw_data,
#'
#'   # Qualtrics marks preview responses in the Status column
#'   pretest_var = "Status",
#'   pretest_values = "Survey Preview",
#'
#'   # Requirements to be included
#'   inclusion_criteria = list(
#'     completed = raw_data$Finished == 1,        # Completed survey
#'     adult = raw_data$age >= 18,                 # 18 or older
#'     consented = raw_data$consent == "I agree"   # Gave consent
#'   ),
#'
#'   # Attention checks (replace Q5, Q12 with your question numbers)
#'   attention_checks = list(
#'     ac1 = list(var = "Q5", correct = 7),       # Told them to select 7
#'     ac2 = list(var = "Q12", correct = 1)       # Told them to select 1
#'   ),
#'   attention_check_rule = "all",  # Must pass both
#'
#'   id_var = "ResponseId"  # Qualtrics ID column
#' )
#'
#' # ========================================================================
#' # EXAMPLE 2B: Date-based pre-test removal
#' # ========================================================================
#'
#' # If you ran pre-tests in February and started real data collection March 1
#' result <- clean_survey_data(
#'   data = raw_data,
#'
#'   # Remove anything collected before March 1, 2024
#'   date_var = "StartDate",                       # Qualtrics start date column
#'   pretest_before_date = "2024-03-01",           # Cutoff date
#'
#'   # Rest of your criteria
#'   inclusion_criteria = list(
#'     completed = raw_data$Finished == 1,
#'     adult = raw_data$age >= 18
#'   ),
#'
#'   attention_checks = list(
#'     ac1 = list(var = "Q5", correct = 7)
#'   ),
#'
#'   id_var = "ResponseId"
#' )
#'
#' # ========================================================================
#' # EXAMPLE 2C: Combine column-based AND date-based pre-test removal
#' # ========================================================================
#'
#' # Remove BOTH "Survey Preview" responses AND anything before March 1
#' result <- clean_survey_data(
#'   data = raw_data,
#'
#'   # Column-based: Qualtrics preview responses
#'   pretest_var = "Status",
#'   pretest_values = "Survey Preview",
#'
#'   # Date-based: Anything before March 1
#'   date_var = "StartDate",
#'   pretest_before_date = "2024-03-01",
#'
#'   # Other criteria...
#'   inclusion_criteria = list(
#'     completed = raw_data$Finished == 1
#'   ),
#'
#'   id_var = "ResponseId"
#' )
#'
#' # Get your cleaned data
#' clean_df <- result$clean_data
#'
#' # Copy this into your Methods section
#' cat(result$publication_text$concise)
#'
#' # ========================================================================
#' # EXAMPLE 3: MTurk study
#' # ========================================================================
#'
#' result <- clean_survey_data(
#'   data = raw_data,
#'
#'   # Mark your own test responses
#'   pretest_var = "WorkerId",
#'   pretest_values = "YOUR_WORKER_ID_HERE",
#'
#'   # MTurk quality criteria
#'   inclusion_criteria = list(
#'     submitted = raw_data$AssignmentStatus == "Submitted",
#'     approval = raw_data$ApprovalRate >= 95,
#'     min_hits = raw_data$TotalHITS >= 100,
#'     us_location = raw_data$CountryOfResidence == "US"
#'   ),
#'
#'   # Your attention checks
#'   attention_checks = list(
#'     ac1 = list(var = "attention_1", correct = "blue"),
#'     ac2 = list(var = "attention_2", correct = 4)
#'   ),
#'
#'   # Also exclude duplicates and speeders
#'   additional_exclusions = duplicated(raw_data$WorkerId) |
#'                          raw_data$duration_sec < 120,
#'   additional_exclusion_reason = "Duplicate worker IDs or completed too quickly",
#'
#'   id_var = "WorkerId"
#' )
#'
#' # ========================================================================
#' # EXAMPLE 4: No attention checks, just basic cleaning
#' # ========================================================================
#'
#' result <- clean_survey_data(
#'   data = raw_data,
#'   inclusion_criteria = list(
#'     completed = raw_data$Finished == 1,
#'     consented = raw_data$consent_given == TRUE
#'   )
#' )
#' # That's it! No attention checks needed.
#'
#' # ========================================================================
#' # EXAMPLE 5: Rename variables and recode values (VERY USEFUL!)
#' # ========================================================================
#'
#' # First, check what Qualtrics named your columns
#' names(raw_data)
#' # [1] "ResponseId" "Q1" "Q2" "Q3" "Q17" "FL_16_DO" "Finished" ...
#'
#' result <- clean_survey_data(
#'   data = raw_data,
#'
#'   # Rename Qualtrics' random names to meaningful names
#'   rename_vars = list(
#'     brand_attitude = "Q1",          # Q1 → brand_attitude
#'     purchase_intent = "Q2",         # Q2 → purchase_intent
#'     satisfaction = "Q3",            # Q3 → satisfaction
#'     age = "Q17",                    # Q17 → age
#'     condition = "FL_16_DO"          # Flow field → condition
#'   ),
#'
#'   # Recode numeric codes to meaningful labels
#'   # IMPORTANT: Use the NEW names (after renaming)
#'   recode_vars = list(
#'     condition = c("1" = "control", "2" = "treatment_A", "3" = "treatment_B"),
#'     age = c("1" = "18-24", "2" = "25-34", "3" = "35-44", "4" = "45-54", "5" = "55+")
#'   ),
#'
#'   # Now use the NEW names in your inclusion criteria
#'   inclusion_criteria = list(
#'     completed = raw_data$Finished == 1
#'   ),
#'
#'   id_var = "ResponseId"
#' )
#'
#' # Your clean data now has meaningful variable names and labels!
#' names(result$clean_data)
#' # [1] "ResponseId" "brand_attitude" "purchase_intent" "satisfaction" "age" "condition" ...
#'
#' table(result$clean_data$condition)
#' # control  treatment_A  treatment_B
#' #     150          145          143
#'
#' }
#'
#' @export
#' @importFrom dplyr tibble filter mutate rename
clean_survey_data <- function(data,
                               pretest_var = NULL,
                               pretest_values = c(TRUE, 1, "pretest", "pre-test", "test"),
                               date_var = NULL,
                               pretest_before_date = NULL,
                               inclusion_criteria = NULL,
                               attention_checks = NULL,
                               attention_check_rule = "all",
                               additional_exclusions = NULL,
                               additional_exclusion_reason = "Additional exclusions",
                               rename_vars = NULL,
                               recode_vars = NULL,
                               id_var = NULL,
                               verbose = TRUE) {

  # ===========================================================================
  # STEP 0: SETUP
  # ===========================================================================
  # What we're doing: Prepare the data and set up tracking variables
  # Why: Need to convert to data frame and create tracking for exclusions

  # Convert to data frame for consistent handling (works with tibbles too)
  data <- as.data.frame(data)

  # Get total rows BEFORE any exclusions (including pre-tests)
  n_total_rows <- nrow(data)

  # Initialize tracking
  exclusion_log <- list()

  # Create flags data frame - tracks which rows get excluded for which reasons
  # One row for each observation in the original data
  flags <- data.frame(
    row_number = 1:n_total_rows,
    pretest = FALSE,              # Will mark TRUE if it's a pre-test
    inclusion_fail = FALSE,       # Will mark TRUE if fails inclusion criteria
    attention_fail = FALSE,       # Will mark TRUE if fails attention checks
    additional = FALSE,           # Will mark TRUE for other exclusions
    exclusion_reason = NA_character_,  # Text description of why excluded
    stringsAsFactors = FALSE
  )

  # If user provided ID variable, add it to flags for tracking
  if (!is.null(id_var) && id_var %in% names(data)) {
    flags$id <- data[[id_var]]
  }

  # ===========================================================================
  # STEP 0.5: RENAME VARIABLES AND RECODE VALUES
  # ===========================================================================
  # What we're doing: Clean up variable names and recode values
  # Why: Qualtrics uses random names (Q1, Q2, FL_16_DO) and numeric codes.
  #      We rename these to meaningful names and recode values to labels.
  #
  # This happens FIRST so all subsequent operations use the clean names.

  renamed_vars <- character(0)
  recoded_vars <- character(0)

  # --- RENAMING VARIABLES ---
  if (!is.null(rename_vars) && length(rename_vars) > 0) {
    if (verbose) {
      message("\n", rep("=", 70))
      message("VARIABLE RENAMING")
      message(rep("=", 70), "\n")
    }

    for (new_name in names(rename_vars)) {
      old_name <- rename_vars[[new_name]]

      # Check if old column exists
      if (old_name %in% names(data)) {
        # Rename the column
        names(data)[names(data) == old_name] <- new_name
        renamed_vars <- c(renamed_vars, paste0(old_name, " → ", new_name))

        if (verbose) {
          message("  Renamed: '", old_name, "' → '", new_name, "'")
        }

        # Update references in other parameters if needed
        if (!is.null(pretest_var) && pretest_var == old_name) {
          pretest_var <- new_name
          if (verbose) message("    (Updated pretest_var reference)")
        }
        if (!is.null(date_var) && date_var == old_name) {
          date_var <- new_name
          if (verbose) message("    (Updated date_var reference)")
        }
        if (!is.null(id_var) && id_var == old_name) {
          id_var <- new_name
          if (verbose) message("    (Updated id_var reference)")
        }
      } else {
        warning("Cannot rename '", old_name, "' - column not found in data")
        if (verbose) {
          message("  ✗ Column '", old_name, "' not found")
          message("    Available columns: ", paste(names(data)[1:min(10, length(names(data)))], collapse = ", "), "...")
        }
      }
    }

    if (verbose && length(renamed_vars) > 0) {
      message("\nRenamed ", length(renamed_vars), " variable",
              ifelse(length(renamed_vars) > 1, "s", ""), "\n")
    }
  }

  # --- RECODING VALUES ---
  if (!is.null(recode_vars) && length(recode_vars) > 0) {
    if (verbose) {
      message(rep("=", 70))
      message("VALUE RECODING")
      message(rep("=", 70), "\n")
    }

    for (var_name in names(recode_vars)) {
      # Check if variable exists
      if (var_name %in% names(data)) {
        recode_map <- recode_vars[[var_name]]

        # Count how many values will be recoded
        values_to_recode <- as.character(data[[var_name]]) %in% names(recode_map)
        n_recoded <- sum(values_to_recode, na.rm = TRUE)

        if (n_recoded > 0) {
          # Perform the recoding
          data[[var_name]] <- as.character(data[[var_name]])
          for (old_val in names(recode_map)) {
            data[[var_name]][data[[var_name]] == old_val] <- recode_map[old_val]
          }

          recoded_vars <- c(recoded_vars, var_name)

          if (verbose) {
            message("  Recoded '", var_name, "': ", n_recoded, " value",
                    ifelse(n_recoded > 1, "s", ""), " changed")
            message("    Mapping: ", paste(names(recode_map), "→", recode_map, collapse = ", "))
          }
        } else if (verbose) {
          message("  '", var_name, "': No matching values found to recode")
        }
      } else {
        warning("Cannot recode '", var_name, "' - column not found in data")
        if (verbose) {
          message("  ✗ Column '", var_name, "' not found")
        }
      }
    }

    if (verbose && length(recoded_vars) > 0) {
      message("\nRecoded values in ", length(recoded_vars), " variable",
              ifelse(length(recoded_vars) > 1, "s", ""), "\n")
    }
  }

  # ===========================================================================
  # STEP 1: IDENTIFY AND REMOVE PRE-TEST CASES
  # ===========================================================================
  # What we're doing: Find rows that are pre-test/pilot data
  # Why: Pre-tests were collected BEFORE the study and shouldn't count in sample
  #
  # Pre-tests are things like:
  # - Your own test responses when building the survey
  # - Pilot data collected to test if questions work
  # - Preview responses in Qualtrics
  # - Responses flagged as "test" in your data
  # - Responses collected before your official data collection started
  #
  # IMPORTANT: These DON'T count toward your initial sample size!
  #
  # Two methods to identify pre-tests:
  # 1. Column-based: Use a column that flags pre-test responses (pretest_var)
  # 2. Date-based: Exclude anything collected before a certain date (date_var + pretest_before_date)
  # You can use either method alone, or combine both!

  pretest_indices <- c()
  n_pretest <- 0
  pretest_methods <- c()  # Track which methods were used

  # --- METHOD 1: Column-based pre-test identification ---
  if (!is.null(pretest_var) && pretest_var %in% names(data)) {
    # Find which rows have pre-test values in the pre-test variable
    # Example: If pretest_var = "Status" and pretest_values = "Survey Preview"
    #          This finds all rows where Status == "Survey Preview"
    pretest_indices_column <- which(data[[pretest_var]] %in% pretest_values)
    n_pretest_column <- length(pretest_indices_column)

    if (n_pretest_column > 0) {
      pretest_indices <- pretest_indices_column
      pretest_methods <- c(pretest_methods, "column-based")

      if (verbose) {
        message("\n", rep("=", 70))
        message("DATA CLEANING: Removing Pre-Test Data")
        message("(Pre-tests are NOT counted in your initial sample size)")
        message(rep("=", 70))
        message("\nMethod 1: Column-based identification")
        message("  Column: '", pretest_var, "'")
        message("  Looking for values: ", paste(pretest_values, collapse = ", "))
        message("  Found ", n_pretest_column, " pre-test cases\n")
      }
    }
  } else if (verbose && !is.null(pretest_var)) {
    # User specified a pre-test variable but it doesn't exist in data
    message("\nNote: Column '", pretest_var, "' not found in data - skipping column-based pre-test removal")
    message("Your column names are: ", paste(names(data)[1:min(10, length(names(data)))], collapse = ", "), "...")
  }

  # --- METHOD 2: Date-based pre-test identification ---
  if (!is.null(date_var) && !is.null(pretest_before_date)) {
    # Check if date column exists
    if (date_var %in% names(data)) {
      # Convert the date column to Date or POSIXct
      # This handles various date formats automatically
      date_column <- data[[date_var]]

      # Try to convert to Date/POSIXct if it's not already
      if (!inherits(date_column, c("Date", "POSIXct", "POSIXlt"))) {
        # Try to parse as date
        date_column <- tryCatch({
          as.POSIXct(date_column)
        }, error = function(e) {
          tryCatch({
            as.Date(date_column)
          }, error = function(e2) {
            warning("Could not convert '", date_var, "' to date format. Skipping date-based pre-test removal.")
            NULL
          })
        })
      }

      if (!is.null(date_column)) {
        # Convert the cutoff date to the same format
        cutoff_date <- if (inherits(pretest_before_date, c("Date", "POSIXct", "POSIXlt"))) {
          pretest_before_date
        } else {
          tryCatch({
            as.POSIXct(pretest_before_date)
          }, error = function(e) {
            tryCatch({
              as.Date(pretest_before_date)
            }, error = function(e2) {
              warning("Could not convert pretest_before_date to date format.")
              NULL
            })
          })
        }

        if (!is.null(cutoff_date)) {
          # Find rows where date < cutoff
          pretest_indices_date <- which(date_column < cutoff_date)
          n_pretest_date <- length(pretest_indices_date)

          if (n_pretest_date > 0) {
            # Combine with column-based indices (union - don't double count)
            pretest_indices <- union(pretest_indices, pretest_indices_date)
            pretest_methods <- c(pretest_methods, "date-based")

            if (verbose) {
              if (length(pretest_methods) == 1) {
                # First method being used
                message("\n", rep("=", 70))
                message("DATA CLEANING: Removing Pre-Test Data")
                message("(Pre-tests are NOT counted in your initial sample size)")
                message(rep("=", 70), "\n")
              }
              message("Method 2: Date-based identification")
              message("  Date column: '", date_var, "'")
              message("  Cutoff date: ", format(cutoff_date))
              message("  Found ", n_pretest_date, " responses before cutoff\n")
            }
          }
        }
      }
    } else {
      warning("Date column '", date_var, "' not found in data. Skipping date-based pre-test removal.")
      if (verbose) {
        message("Your date/time columns might be: ",
                paste(names(data)[grep("date|time|start|end|record", names(data), ignore.case = TRUE)],
                      collapse = ", "))
      }
    }
  } else if (!is.null(date_var) && is.null(pretest_before_date)) {
    warning("You provided 'date_var' but not 'pretest_before_date'. Both are needed for date-based pre-test removal.")
  } else if (is.null(date_var) && !is.null(pretest_before_date)) {
    warning("You provided 'pretest_before_date' but not 'date_var'. Both are needed for date-based pre-test removal.")
  }

  # --- FINALIZE PRE-TEST IDENTIFICATION ---
  n_pretest <- length(pretest_indices)

  if (n_pretest > 0) {
    # Mark these rows as pre-tests in our flags
    flags$pretest[pretest_indices] <- TRUE

    # Create detailed exclusion reason based on methods used
    if (length(pretest_methods) == 2) {
      reason_text <- "Pre-test case (column-based and date-based)"
    } else if ("column-based" %in% pretest_methods) {
      reason_text <- "Pre-test case (column-based)"
    } else {
      reason_text <- "Pre-test case (date-based)"
    }

    flags$exclusion_reason[pretest_indices] <- reason_text

    # Log this exclusion
    exclusion_log$pretest <- list(
      n = n_pretest,
      reason = "Pre-test or pilot data",
      methods = pretest_methods,
      indices = pretest_indices
    )

    if (verbose) {
      if (length(pretest_methods) > 1) {
        message("Combined total: ", n_pretest, " pre-test cases (using both methods)")
      }
      message("These will be removed before counting your sample.\n")
    }
  }

  # Remove pre-test cases from the data NOW
  # Everything after this point works with the non-pretest data
  if (n_pretest > 0) {
    data <- data[-pretest_indices, , drop = FALSE]
    # Renumber rows to match the new data
    row_nums_remaining <- (1:n_total_rows)[-pretest_indices]
  } else {
    row_nums_remaining <- 1:n_total_rows
  }

  # ===========================================================================
  # STEP 2: COUNT INITIAL SAMPLE SIZE (AFTER PRE-TESTS REMOVED)
  # ===========================================================================
  # What we're doing: Count how many participants we actually collected data from
  # Why: This is your "n = X" that you report in your methods section
  #
  # IMPORTANT: We count AFTER removing pre-tests because pre-tests aren't
  # real participants - they're test runs done before the study began

  n_initial <- nrow(data)  # This is your real starting sample size

  if (verbose) {
    message(rep("=", 70))
    message("INITIAL SAMPLE SIZE")
    message(rep("=", 70))
    message("Starting with ", n_initial, " participants")
    if (n_pretest > 0) {
      message("(", n_pretest, " pre-test cases were removed first)")
    }
    message("\nNow beginning systematic exclusion process...")
    message(rep("=", 70), "\n")
  }

  # Track which rows we're currently keeping
  current_indices <- 1:n_initial

  # ===========================================================================
  # STEP 3: APPLY INCLUSION CRITERIA
  # ===========================================================================
  # What we're doing: Check if participants meet requirements to be included
  # Why: Studies have specific requirements (age, completion, location, etc.)
  #
  # Inclusion criteria are requirements participants MUST meet, like:
  # - Completed the survey (didn't quit halfway)
  # - Meet age requirement (e.g., 18+)
  # - From correct country (e.g., US only)
  # - Gave consent
  #
  # Each criterion is a TRUE/FALSE check for every participant

  inclusion_indices <- c()  # Will hold indices of people who FAIL criteria
  inclusion_details <- list()

  if (!is.null(inclusion_criteria) && length(inclusion_criteria) > 0) {
    if (verbose) {
      message("STEP 1: Checking Inclusion Criteria")
      message("-" %+% rep("-", 50), "\n")
    }

    for (criterion_name in names(inclusion_criteria)) {
      # Get the TRUE/FALSE vector for this criterion
      criterion_met <- inclusion_criteria[[criterion_name]]

      # Safety check: Make sure it has the right length
      # Should have one TRUE/FALSE for each row in ORIGINAL data (before pre-test removal)
      if (length(criterion_met) != n_total_rows) {
        warning("Inclusion criterion '", criterion_name, "' has wrong length (should be ",
                n_total_rows, ", got ", length(criterion_met), "). Skipping.")
        next
      }

      # Remove pre-test indices from the criterion check
      if (n_pretest > 0) {
        criterion_met <- criterion_met[-pretest_indices]
      }

      # Find participants who FAIL this criterion (among those not yet excluded)
      # We only check people in current_indices (haven't been excluded yet)
      fails_criterion <- intersect(current_indices, which(!criterion_met))
      n_fails <- length(fails_criterion)

      if (n_fails > 0) {
        # Add to our list of people failing inclusion criteria
        inclusion_indices <- union(inclusion_indices, fails_criterion)

        # Update flags with this specific failure reason
        # Note: We need to map back to original row numbers
        original_rows <- row_nums_remaining[fails_criterion]
        flags$inclusion_fail[original_rows] <- TRUE

        # Add to exclusion reason (might already have text from previous criteria)
        for (i in original_rows) {
          if (is.na(flags$exclusion_reason[i])) {
            flags$exclusion_reason[i] <- paste0("Failed: ", criterion_name)
          } else {
            flags$exclusion_reason[i] <- paste0(flags$exclusion_reason[i], "; Failed: ", criterion_name)
          }
        }

        # Log details about this criterion
        inclusion_details[[criterion_name]] <- list(
          n = n_fails,
          reason = paste0("Did not meet criterion: ", criterion_name),
          indices = fails_criterion
        )

        if (verbose) {
          message("  '", criterion_name, "': excluded ", n_fails, " participant",
                  ifelse(n_fails > 1, "s", ""))
        }
      } else if (verbose) {
        message("  '", criterion_name, "': all passed ✓")
      }
    }

    # Total number excluded for ANY inclusion criterion failure
    n_inclusion <- length(inclusion_indices)

    if (n_inclusion > 0) {
      exclusion_log$inclusion <- list(
        n = n_inclusion,
        reason = "Failed inclusion criteria",
        details = inclusion_details
      )

      if (verbose) {
        message("\n  Total excluded for inclusion criteria: ", n_inclusion)
        message("  Remaining: ", n_initial - n_inclusion, "\n")
      }

      # Update current indices - remove those who failed
      current_indices <- setdiff(current_indices, inclusion_indices)
    }
  }

  # ===========================================================================
  # STEP 4: CHECK ATTENTION CHECKS
  # ===========================================================================
  # What we're doing: See who failed attention check questions
  # Why: Attention checks catch people who aren't reading carefully
  #
  # Attention checks are questions like:
  # "To show you are paying attention, please select 'Strongly Agree'"
  # "Select the color blue from the options below"
  #
  # They have one obviously correct answer that you told people to select

  attention_indices <- c()
  attention_details <- list()

  if (!is.null(attention_checks) && length(attention_checks) > 0) {
    if (verbose) {
      message("STEP 2: Checking Attention Checks")
      message("-" %+% rep("-", 50), "\n")
    }

    n_checks <- length(attention_checks)

    # Create matrix to track pass/fail for each check
    # Rows = participants, Columns = attention checks
    check_matrix <- matrix(NA, nrow = n_initial, ncol = n_checks)
    colnames(check_matrix) <- names(attention_checks)

    # Check each attention check
    for (i in seq_along(attention_checks)) {
      check_name <- names(attention_checks)[i]
      check_info <- attention_checks[[i]]

      # Get the column name and correct answer
      var_name <- check_info$var
      correct_answer <- check_info$correct

      # Safety check: Does this column exist?
      if (!var_name %in% names(data)) {
        warning("Attention check variable '", var_name, "' not found in data. Skipping.")
        if (verbose) {
          message("  Warning: Column '", var_name, "' not found")
          message("  Your column names: ", paste(names(data)[1:min(5, length(names(data)))], collapse = ", "), "...")
        }
        next
      }

      # Check who got it right: Does their answer == correct answer?
      passed <- data[[var_name]] == correct_answer
      passed[is.na(passed)] <- FALSE  # Treat missing as failure

      # Store in matrix
      check_matrix[, i] <- passed

      # Count failures (among current sample only)
      n_failed_this <- sum(!passed[current_indices])
      if (verbose && n_failed_this > 0) {
        message("  '", check_name, "': ", n_failed_this, " failed")
      } else if (verbose) {
        message("  '", check_name, "': all passed ✓")
      }
    }

    # ===========================================================================
    # Determine who fails based on the rule
    # ===========================================================================
    # Count how many checks each person passed
    checks_passed <- rowSums(check_matrix, na.rm = TRUE)

    # Apply the rule to determine who fails
    if (attention_check_rule == "all") {
      # Must pass ALL checks
      fails_attention <- which(checks_passed < n_checks)
      rule_text <- "all"
    } else if (attention_check_rule == "majority") {
      # Must pass > 50%
      fails_attention <- which(checks_passed < (n_checks / 2))
      rule_text <- "the majority of"
    } else if (is.numeric(attention_check_rule)) {
      # Must pass at least this many
      fails_attention <- which(checks_passed < attention_check_rule)
      rule_text <- paste0("at least ", attention_check_rule, " of")
    } else {
      warning("Invalid attention_check_rule: '", attention_check_rule, "'. Using 'all'.")
      fails_attention <- which(checks_passed < n_checks)
      rule_text <- "all"
    }

    # Only count failures among current sample (not already excluded)
    attention_indices <- intersect(current_indices, fails_attention)
    n_attention <- length(attention_indices)

    if (n_attention > 0) {
      # Update flags
      original_rows <- row_nums_remaining[attention_indices]
      flags$attention_fail[original_rows] <- TRUE

      # Add to exclusion reason
      for (i in seq_along(original_rows)) {
        row_idx <- original_rows[i]
        n_passed <- checks_passed[attention_indices[i]]

        reason_text <- paste0("Failed attention checks (", n_passed, "/", n_checks, " passed)")

        if (is.na(flags$exclusion_reason[row_idx])) {
          flags$exclusion_reason[row_idx] <- reason_text
        } else {
          flags$exclusion_reason[row_idx] <- paste0(flags$exclusion_reason[row_idx], "; ", reason_text)
        }
      }

      # Log this exclusion
      exclusion_log$attention <- list(
        n = n_attention,
        reason = paste0("Failed attention checks (rule: ", attention_check_rule, ")"),
        n_checks = n_checks,
        rule = rule_text,
        indices = attention_indices
      )

      if (verbose) {
        message("\n  Total excluded for attention check failures: ", n_attention)
        message("  (Required: ", rule_text, " ", n_checks, " check", ifelse(n_checks > 1, "s", ""), ")")
        message("  Remaining: ", length(setdiff(current_indices, attention_indices)), "\n")
      }

      # Update current sample
      current_indices <- setdiff(current_indices, attention_indices)
    } else if (verbose) {
      message("\n  No failures - all passed the required attention checks ✓\n")
    }
  }

  # ===========================================================================
  # STEP 5: ADDITIONAL EXCLUSIONS
  # ===========================================================================
  # What we're doing: Apply any other exclusion criteria
  # Why: Catch things like duplicate IPs, speeders, bots, suspicious patterns

  additional_indices <- c()
  n_additional <- 0

  if (!is.null(additional_exclusions)) {
    if (verbose) {
      message("STEP 3: Additional Exclusions")
      message("-" %+% rep("-", 50), "\n")
    }

    # Safety check: Right length?
    if (length(additional_exclusions) != n_total_rows) {
      warning("additional_exclusions has wrong length (should be ", n_total_rows,
              ", got ", length(additional_exclusions), "). Skipping.")
    } else {
      # Remove pre-test indices if applicable
      if (n_pretest > 0) {
        additional_exclusions <- additional_exclusions[-pretest_indices]
      }

      # Find who gets excluded (among current sample only)
      additional_indices <- intersect(current_indices, which(additional_exclusions))
      n_additional <- length(additional_indices)

      if (n_additional > 0) {
        # Update flags
        original_rows <- row_nums_remaining[additional_indices]
        flags$additional[original_rows] <- TRUE

        # Add to exclusion reason
        for (row_idx in original_rows) {
          if (is.na(flags$exclusion_reason[row_idx])) {
            flags$exclusion_reason[row_idx] <- additional_exclusion_reason
          } else {
            flags$exclusion_reason[row_idx] <- paste0(flags$exclusion_reason[row_idx],
                                                       "; ", additional_exclusion_reason)
          }
        }

        # Log this
        exclusion_log$additional <- list(
          n = n_additional,
          reason = additional_exclusion_reason,
          indices = additional_indices
        )

        if (verbose) {
          message("  Excluded: ", n_additional, " (", additional_exclusion_reason, ")")
          message("  Remaining: ", length(setdiff(current_indices, additional_indices)), "\n")
        }

        # Update current sample
        current_indices <- setdiff(current_indices, additional_indices)
      } else if (verbose) {
        message("  No additional exclusions needed ✓\n")
      }
    }
  }

  # ===========================================================================
  # STEP 6: CREATE FINAL CLEAN DATASET
  # ===========================================================================
  # What we're doing: Extract only the rows that passed all checks
  # Why: This is your analysis dataset!

  n_final <- length(current_indices)
  pct_retained <- round(100 * n_final / n_initial, 1)

  clean_data <- data[current_indices, , drop = FALSE]

  if (verbose) {
    message(rep("=", 70))
    message("CLEANING COMPLETE!")
    message(rep("=", 70))
    message("Initial sample:     ", n_initial, " participants")
    if (n_pretest > 0) {
      message("(Pre-test cases:    ", n_pretest, " removed before counting)")
    }
    message("Final sample:       ", n_final, " participants")
    message("Excluded:           ", n_initial - n_final, " participants")
    message("Retention rate:     ", pct_retained, "%")
    message(rep("=", 70), "\n")
  }

  # ===========================================================================
  # STEP 7: CREATE EXCLUSION SUMMARY TABLE
  # ===========================================================================
  # What we're doing: Make a nice table showing all exclusions
  # Why: Perfect for Table 1 in your manuscript

  exclusion_summary <- data.frame(
    exclusion_type = character(),
    n_excluded = integer(),
    cumulative_n = integer(),
    percentage_of_initial = numeric(),
    stringsAsFactors = FALSE
  )

  cumulative <- 0

  # Note: Pre-tests are NOT included in exclusion summary because they weren't
  # part of the initial sample. They were removed BEFORE counting began.

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

  # ===========================================================================
  # STEP 8: GENERATE PUBLICATION TEXT
  # ===========================================================================
  # What we're doing: Create text you can copy into your manuscript
  # Why: Saves you time and ensures you report everything correctly

  pub_text <- generate_cleaning_publication_text(
    n_initial = n_initial,
    n_final = n_final,
    n_pretest = n_pretest,
    exclusion_log = exclusion_log,
    inclusion_details = if (!is.null(inclusion_criteria)) inclusion_details else NULL,
    attention_n_checks = if (!is.null(attention_checks)) length(attention_checks) else 0,
    attention_rule = if (!is.null(attention_checks)) attention_check_rule else NULL
  )

  # ===========================================================================
  # STEP 9: CREATE PARTICIPANT FLOW
  # ===========================================================================
  # What we're doing: CONSORT-style flow diagram data
  # Why: Shows the flow of participants through your study

  flow <- list(
    step1 = list(
      label = "Initial Sample (after removing pre-tests)",
      n = n_initial,
      note = if (n_pretest > 0) paste0(n_pretest, " pre-test cases removed first") else NULL
    )
  )

  step_num <- 2
  if (length(inclusion_indices) > 0) {
    flow[[paste0("step", step_num)]] <- list(
      label = "After applying inclusion criteria",
      n = n_initial - length(inclusion_indices),
      excluded = length(inclusion_indices),
      reason = "Did not meet inclusion criteria"
    )
    step_num <- step_num + 1
  }

  if (length(attention_indices) > 0) {
    flow[[paste0("step", step_num)]] <- list(
      label = "After attention check screening",
      n = n_initial - length(inclusion_indices) - length(attention_indices),
      excluded = length(attention_indices),
      reason = "Failed attention checks"
    )
    step_num <- step_num + 1
  }

  if (n_additional > 0) {
    flow[[paste0("step", step_num)]] <- list(
      label = paste0("After ", tolower(additional_exclusion_reason)),
      n = n_final,
      excluded = n_additional,
      reason = additional_exclusion_reason
    )
  }

  flow$final <- list(
    label = "Final Analytical Sample",
    n = n_final,
    percentage_retained = pct_retained
  )

  # ===========================================================================
  # STEP 10: RETURN EVERYTHING
  # ===========================================================================

  results <- structure(
    list(
      clean_data = clean_data,
      n_initial = n_initial,
      n_final = n_final,
      n_excluded_total = n_initial - n_final,
      n_pretest = n_pretest,  # Reported separately
      retention_rate = pct_retained,
      exclusion_summary = exclusion_summary,
      exclusion_log = exclusion_log,
      participant_flow = flow,
      publication_text = pub_text,
      flags = flags,
      renamed_vars = renamed_vars,  # Track which variables were renamed
      recoded_vars = recoded_vars,  # Track which variables had values recoded
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
                                                n_pretest,
                                                exclusion_log,
                                                inclusion_details,
                                                attention_n_checks,
                                                attention_rule) {

  # Build comprehensive methods text
  sections <- list()

  # Sample collection - NOTE: n_initial is AFTER pre-test removal
  if (n_pretest > 0) {
    sections$sample_collection <- paste0(
      "We collected data from ", n_initial, " participants. ",
      "(An additional ", n_pretest, " pre-test response", ifelse(n_pretest > 1, "s were", " was"),
      " collected during pilot testing and removed prior to analysis.)"
    )
  } else {
    sections$sample_collection <- paste0(
      "Data were collected from ", n_initial, " participants."
    )
  }

  # Exclusions
  exclusion_sentences <- c()

  if (!is.null(exclusion_log$inclusion)) {
    # Detail each inclusion criterion
    if (!is.null(inclusion_details) && length(inclusion_details) > 0) {
      criterion_texts <- sapply(names(inclusion_details), function(name) {
        n <- inclusion_details[[name]]$n
        paste0(n, " participant", ifelse(n > 1, "s", ""), " who failed the '", name, "' criterion")
      })

      exclusion_sentences <- c(exclusion_sentences, paste0(
        "We excluded participants who did not meet our inclusion criteria: ",
        paste(criterion_texts, collapse = ", ")
      ))
    } else {
      exclusion_sentences <- c(exclusion_sentences, paste0(
        "We excluded ", exclusion_log$inclusion$n, " participant",
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
      "We also excluded ", exclusion_log$additional$n, " participant",
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
    "SAMPLE AND DATA CLEANING PROCEDURE\n\n"
  )

  if (n_pretest > 0) {
    verbose_text <- paste0(verbose_text,
      "Pre-test Data: Prior to the main data collection, we conducted ", n_pretest,
      " pre-test response", ifelse(n_pretest > 1, "s", ""),
      " during pilot testing to validate survey flow and question wording. ",
      "These pre-test cases were removed from the dataset before analysis and are not ",
      "included in the reported sample size.\n\n"
    )
  }

  verbose_text <- paste0(verbose_text,
    "Initial Sample: We collected data from ", n_initial, " participants for the main study.\n\n"
  )

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
      ifelse(attention_n_checks > 1, "s", ""), " (following Oppenheimer et al., 2009).\n\n"
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
    n_final, " participants (", pct_retained, "% retention rate from initial sample",
    ifelse(n_pretest > 0, ", not counting pre-tests", ""), "). ",
    "This retention rate is ", ifelse(pct_retained >= 80, "excellent",
                                      ifelse(pct_retained >= 70, "good",
                                             ifelse(pct_retained >= 60, "acceptable", "low"))),
    " and is consistent with standards in consumer research."
  )

  # CONSORT text
  consort_text <- paste0(
    "PARTICIPANT FLOW (CONSORT Style):\n\n"
  )

  if (n_pretest > 0) {
    consort_text <- paste0(consort_text,
      "PRE-TEST RESPONSES (excluded before counting): n = ", n_pretest, "\n",
      "  ↓\n"
    )
  }

  consort_text <- paste0(consort_text,
    "Initial sample collected: n = ", n_initial, "\n"
  )

  current_n <- n_initial

  if (!is.null(exclusion_log$inclusion)) {
    consort_text <- paste0(consort_text,
      "  ↓ Excluded (inclusion criteria): n = ", exclusion_log$inclusion$n, "\n",
      "After inclusion screening: n = ", current_n - exclusion_log$inclusion$n, "\n"
    )
    current_n <- current_n - exclusion_log$inclusion$n
  }

  if (!is.null(exclusion_log$attention)) {
    consort_text <- paste0(consort_text,
      "  ↓ Excluded (attention checks): n = ", exclusion_log$attention$n, "\n",
      "After attention screening: n = ", current_n - exclusion_log$attention$n, "\n"
    )
    current_n <- current_n - exclusion_log$attention$n
  }

  if (!is.null(exclusion_log$additional)) {
    consort_text <- paste0(consort_text,
      "  ↓ Excluded (", exclusion_log$additional$reason, "): n = ",
      exclusion_log$additional$n, "\n",
      "After additional exclusions: n = ", current_n - exclusion_log$additional$n, "\n"
    )
    current_n <- current_n - exclusion_log$additional$n
  }

  consort_text <- paste0(consort_text,
    "\n",
    "═══════════════════════════════\n",
    "FINAL ANALYTICAL SAMPLE: n = ", n_final, "\n",
    "═══════════════════════════════"
  )

  list(
    concise = full_text,
    verbose = verbose_text,
    consort = consort_text
  )
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
  if (x$n_pretest > 0) {
    cat("  Pre-tests removed:   n =", x$n_pretest, "(not counted in initial sample)\n")
  }
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

