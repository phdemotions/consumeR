pkgname <- "consumeR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "consumeR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('consumeR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_composite")
### * add_composite

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: add_composite
### Title: Add Composite Score to Data Frame
### Aliases: add_composite

### ** Examples

df <- data.frame(
  q1 = c(7, 6, 5),
  q2 = c(6, 7, 4),
  q3 = c(7, 6, 5)
)

# Add composite column
df_with_composite <- add_composite(
  df,
  col_name = "satisfaction",
  items = c("q1", "q2", "q3")
)

# Result has new "satisfaction" column
df_with_composite$satisfaction




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("add_composite", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("alpha_table")
### * alpha_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: alpha_table
### Title: Batch Calculate Cronbach's Alpha for Multiple Scales
### Aliases: alpha_table

### ** Examples

## Not run: 
##D scales <- list(
##D   satisfaction = c("sat1", "sat2", "sat3", "sat4"),
##D   loyalty = c("loy1", "loy2", "loy3"),
##D   value = c("val1", "val2", "val3", "val4", "val5")
##D )
##D 
##D alpha_results <- alpha_table(customer_data, scales)
##D print(alpha_results)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("alpha_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyze_correlation")
### * analyze_correlation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyze_correlation
### Title: Perform Correlation Analysis with Comprehensive Diagnostics
### Aliases: analyze_correlation

### ** Examples

# Example 1: Data frame with variable names
data <- data.frame(
  customer_satisfaction = c(7, 8, 6, 9, 5, 8, 7, 6, 9, 8),
  purchase_intention = c(8, 9, 7, 9, 6, 8, 7, 7, 9, 8)
)
result <- analyze_correlation(data, "customer_satisfaction", "purchase_intention")

# Example 2: Two numeric vectors
x <- c(23, 45, 67, 34, 56, 78, 89, 12, 45, 67)
y <- c(34, 56, 78, 45, 67, 89, 90, 23, 56, 78)
result <- analyze_correlation(x, var2 = y)

# Example 3: Force Spearman (non-parametric)
result <- analyze_correlation(data, "customer_satisfaction",
                               "purchase_intention", method = "spearman")

# View publication text
print(result, show_publication = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyze_correlation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyze_regression")
### * analyze_regression

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyze_regression
### Title: Perform Linear Regression with Comprehensive Diagnostics
### Aliases: analyze_regression

### ** Examples

# Example 1: Simple regression
data <- data.frame(
  ad_spending = c(100, 200, 150, 300, 250, 400, 350, 500),
  sales = c(20, 35, 28, 48, 42, 65, 58, 80)
)
result <- analyze_regression(data, sales ~ ad_spending)

# Example 2: Multiple regression
data <- data.frame(
  satisfaction = c(7, 8, 6, 9, 5, 8, 7, 6, 9, 8),
  price = c(10, 8, 12, 7, 15, 9, 11, 13, 8, 10),
  quality = c(8, 9, 7, 9, 6, 8, 7, 6, 9, 8),
  service = c(7, 8, 6, 9, 5, 8, 7, 7, 9, 8)
)
result <- analyze_regression(data, satisfaction ~ price + quality + service)

# View publication text
print(result, show_publication = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyze_regression", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("assert_vars_present")
### * assert_vars_present

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: assert_vars_present
### Title: Assert Required Variables are Present
### Aliases: assert_vars_present

### ** Examples

## Not run: 
##D df <- data.frame(x = 1:5, y = 6:10)
##D 
##D # Success - no error
##D assert_vars_present(df, c("x", "y"), "my analysis")
##D 
##D # Error - missing variable
##D assert_vars_present(df, c("x", "z"), "my analysis")
##D # Error: Missing variables for my analysis: z
##D #   Available variables: x, y
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("assert_vars_present", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("assumption_checks")
### * assumption_checks

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: assumption_checks
### Title: Assumption Checks with Remediation Recommendations
### Aliases: assumption_checks

### ** Examples

## Not run: 
##D model <- lm(satisfaction ~ condition, data = df)
##D checks <- assumption_checks(model)
##D print(checks)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("assumption_checks", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calculate_alpha")
### * calculate_alpha

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calculate_alpha
### Title: Calculate Cronbach's Alpha for Scale Reliability
### Aliases: calculate_alpha

### ** Examples

# Example 1: Customer satisfaction scale
# Imagine we asked Cloud 9 customers three satisfaction questions
satisfaction_data <- data.frame(
  customer_id = 1:20,
  sat_overall = c(7,8,6,9,7,8,9,7,6,8,7,9,8,7,8,9,7,8,7,8),
  sat_recommend = c(8,9,7,9,8,9,9,8,7,9,8,9,9,8,9,9,8,9,8,9),
  sat_return = c(7,8,6,8,7,8,8,7,6,8,7,8,8,7,8,8,7,8,7,8)
)

# Calculate reliability
reliability <- calculate_alpha(
  data = satisfaction_data,
  items = c("sat_overall", "sat_recommend", "sat_return"),
  scale_name = "Customer Satisfaction"
)

# View results
cat(reliability$interpretation)
print(reliability$alpha)

# Example 2: With item labels for clarity
reliability <- calculate_alpha(
  data = satisfaction_data,
  items = c("sat_overall", "sat_recommend", "sat_return"),
  scale_name = "Customer Satisfaction",
  item_labels = c(
    sat_overall = "Overall satisfaction with Cloud 9",
    sat_recommend = "Would recommend to friends",
    sat_return = "Likelihood to return"
  )
)

# Example 3: With reverse-coded items
# Some items might be worded negatively (e.g., "I was dissatisfied")
mixed_data <- data.frame(
  happy_1 = c(7,8,9,7,8),      # Positive wording
  unhappy_2 = c(3,2,1,3,2),    # Negative wording (needs reversing)
  happy_3 = c(8,9,9,8,9)       # Positive wording
)

reliability <- calculate_alpha(
  data = mixed_data,
  items = c("happy_1", "unhappy_2", "happy_3"),
  scale_name = "Happiness",
  reverse_items = c("unhappy_2")  # Reverse the negative item
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calculate_alpha", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calculate_composite_reliability")
### * calculate_composite_reliability

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calculate_composite_reliability
### Title: Calculate Composite Reliability and Validity Measures
### Aliases: calculate_composite_reliability

### ** Examples

# Example: Employee engagement scale
engagement_data <- data.frame(
  engaged_1 = c(7,8,6,9,7,8,9,7,6,8,7,9,8,7,8,9,7,8,7,8),
  engaged_2 = c(8,9,7,9,8,9,9,8,7,9,8,9,9,8,9,9,8,9,8,9),
  engaged_3 = c(7,8,6,8,7,8,8,7,6,8,7,8,8,7,8,8,7,8,7,8),
  engaged_4 = c(8,8,7,9,8,9,9,8,7,9,8,9,9,8,9,9,8,9,8,9)
)

# Calculate comprehensive reliability
reliability <- calculate_composite_reliability(
  data = engagement_data,
  items = c("engaged_1", "engaged_2", "engaged_3", "engaged_4"),
  scale_name = "Employee Engagement"
)

# View results
print(reliability)
cat(reliability$interpretation)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calculate_composite_reliability", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calculate_summary_stats")
### * calculate_summary_stats

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calculate_summary_stats
### Title: Calculate Summary Statistics for Consumer Data
### Aliases: calculate_summary_stats

### ** Examples

# Example 1: Basic usage with sample consumer spending data
spending <- c(45.2, 67.8, 23.4, 89.1, 34.5, 56.7, 78.9, 12.3)
calculate_summary_stats(spending)

# Example 2: Data with missing values (automatically handled)
satisfaction <- c(7, 8, NA, 6, 9, 7, NA, 8, 7)
calculate_summary_stats(satisfaction)

# Example 3: With fewer statistics and more decimal places
prices <- c(19.99, 24.99, 29.99, 34.99, 39.99)
calculate_summary_stats(prices, include_all = FALSE, round_digits = 3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calculate_summary_stats", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_homogeneity_of_variance")
### * check_homogeneity_of_variance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_homogeneity_of_variance
### Title: Check Homogeneity of Variance (Homoscedasticity)
### Aliases: check_homogeneity_of_variance

### ** Examples

check_homogeneity_of_variance(c(rnorm(50, 5, 1), rnorm(50, 6, 1)),
                               rep(c("A", "B"), each = 50))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_homogeneity_of_variance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_normality")
### * check_normality

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_normality
### Title: Check Normality Assumption
### Aliases: check_normality

### ** Examples

check_normality(rnorm(100), "satisfaction_scores")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_normality", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_variable_types")
### * check_variable_types

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_variable_types
### Title: Review and Modify Variable Types Interactively
### Aliases: check_variable_types

### ** Examples

## Not run: 
##D # After importing data, review variable types
##D checked_data <- check_variable_types(my_data)
##D 
##D # Access the corrected data
##D final_data <- checked_data$data
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_variable_types", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("chisq_test")
### * chisq_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: chisq_test
### Title: Chi-Square Test of Independence with Effect Size
### Aliases: chisq_test

### ** Examples

## Not run: 
##D # From data frame
##D result <- chisq_test(
##D   data = customer_data,
##D   x = "condition",
##D   y = "purchased"
##D )
##D 
##D # From vectors
##D condition <- c(rep("A", 50), rep("B", 50))
##D purchased <- c(rep(c("yes", "no"), each = 25),
##D                rep(c("yes", "no"), c(35, 15)))
##D result <- chisq_test(x = condition, y = purchased)
##D 
##D print(result)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("chisq_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("clean_names_safe")
### * clean_names_safe

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: clean_names_safe
### Title: Clean Variable Names Safely
### Aliases: clean_names_safe

### ** Examples

## Not run: 
##D df <- data.frame(`First Name` = "John", `Last Name` = "Doe")
##D clean_names_safe(df)  # first_name, last_name (if janitor installed)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("clean_names_safe", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("clean_survey_data")
### * clean_survey_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: clean_survey_data
### Title: Clean and Prepare Survey Data with Publication-Ready Exclusion
###   Reporting
### Aliases: clean_survey_data

### ** Examples

## Not run: 
##D # ========================================================================
##D # EXAMPLE 1: Minimal - Just exclude incomplete responses
##D # ========================================================================
##D 
##D result <- clean_survey_data(
##D   data = raw_data,
##D   inclusion_criteria = list(
##D     completed = raw_data$Finished == 1
##D   )
##D )
##D 
##D # ========================================================================
##D # EXAMPLE 2: Typical Qualtrics study with attention checks
##D # ========================================================================
##D 
##D result <- clean_survey_data(
##D   data = raw_data,
##D 
##D   # Qualtrics marks preview responses in the Status column
##D   pretest_var = "Status",
##D   pretest_values = "Survey Preview",
##D 
##D   # Requirements to be included
##D   inclusion_criteria = list(
##D     completed = raw_data$Finished == 1,        # Completed survey
##D     adult = raw_data$age >= 18,                 # 18 or older
##D     consented = raw_data$consent == "I agree"   # Gave consent
##D   ),
##D 
##D   # Attention checks (replace Q5, Q12 with your question numbers)
##D   attention_checks = list(
##D     ac1 = list(var = "Q5", correct = 7),       # Told them to select 7
##D     ac2 = list(var = "Q12", correct = 1)       # Told them to select 1
##D   ),
##D   attention_check_rule = "all",  # Must pass both
##D 
##D   id_var = "ResponseId"  # Qualtrics ID column
##D )
##D 
##D # ========================================================================
##D # EXAMPLE 2B: Date-based pre-test removal
##D # ========================================================================
##D 
##D # If you ran pre-tests in February and started real data collection March 1
##D result <- clean_survey_data(
##D   data = raw_data,
##D 
##D   # Remove anything collected before March 1, 2024
##D   date_var = "StartDate",                       # Qualtrics start date column
##D   pretest_before_date = "2024-03-01",           # Cutoff date
##D 
##D   # Rest of your criteria
##D   inclusion_criteria = list(
##D     completed = raw_data$Finished == 1,
##D     adult = raw_data$age >= 18
##D   ),
##D 
##D   attention_checks = list(
##D     ac1 = list(var = "Q5", correct = 7)
##D   ),
##D 
##D   id_var = "ResponseId"
##D )
##D 
##D # ========================================================================
##D # EXAMPLE 2C: Combine column-based AND date-based pre-test removal
##D # ========================================================================
##D 
##D # Remove BOTH "Survey Preview" responses AND anything before March 1
##D result <- clean_survey_data(
##D   data = raw_data,
##D 
##D   # Column-based: Qualtrics preview responses
##D   pretest_var = "Status",
##D   pretest_values = "Survey Preview",
##D 
##D   # Date-based: Anything before March 1
##D   date_var = "StartDate",
##D   pretest_before_date = "2024-03-01",
##D 
##D   # Other criteria...
##D   inclusion_criteria = list(
##D     completed = raw_data$Finished == 1
##D   ),
##D 
##D   id_var = "ResponseId"
##D )
##D 
##D # Get your cleaned data
##D clean_df <- result$clean_data
##D 
##D # Copy this into your Methods section
##D cat(result$publication_text$concise)
##D 
##D # ========================================================================
##D # EXAMPLE 3: MTurk study
##D # ========================================================================
##D 
##D result <- clean_survey_data(
##D   data = raw_data,
##D 
##D   # Mark your own test responses
##D   pretest_var = "WorkerId",
##D   pretest_values = "YOUR_WORKER_ID_HERE",
##D 
##D   # MTurk quality criteria
##D   inclusion_criteria = list(
##D     submitted = raw_data$AssignmentStatus == "Submitted",
##D     approval = raw_data$ApprovalRate >= 95,
##D     min_hits = raw_data$TotalHITS >= 100,
##D     us_location = raw_data$CountryOfResidence == "US"
##D   ),
##D 
##D   # Your attention checks
##D   attention_checks = list(
##D     ac1 = list(var = "attention_1", correct = "blue"),
##D     ac2 = list(var = "attention_2", correct = 4)
##D   ),
##D 
##D   # Also exclude duplicates and speeders
##D   additional_exclusions = duplicated(raw_data$WorkerId) |
##D                          raw_data$duration_sec < 120,
##D   additional_exclusion_reason = "Duplicate worker IDs or completed too quickly",
##D 
##D   id_var = "WorkerId"
##D )
##D 
##D # ========================================================================
##D # EXAMPLE 4: No attention checks, just basic cleaning
##D # ========================================================================
##D 
##D result <- clean_survey_data(
##D   data = raw_data,
##D   inclusion_criteria = list(
##D     completed = raw_data$Finished == 1,
##D     consented = raw_data$consent_given == TRUE
##D   )
##D )
##D # That's it! No attention checks needed.
##D 
##D # ========================================================================
##D # EXAMPLE 5: Rename variables and recode values (VERY USEFUL!)
##D # ========================================================================
##D 
##D # First, check what Qualtrics named your columns
##D names(raw_data)
##D # [1] "ResponseId" "Q1" "Q2" "Q3" "Q17" "FL_16_DO" "Finished" ...
##D 
##D result <- clean_survey_data(
##D   data = raw_data,
##D 
##D   # Rename Qualtrics' random names to meaningful names
##D   rename_vars = list(
##D     brand_attitude = "Q1",          # Q1 -> brand_attitude
##D     purchase_intent = "Q2",         # Q2 -> purchase_intent
##D     satisfaction = "Q3",            # Q3 -> satisfaction
##D     age = "Q17",                    # Q17 -> age
##D     condition = "FL_16_DO"          # Flow field -> condition
##D   ),
##D 
##D   # Recode numeric codes to meaningful labels
##D   # IMPORTANT: Use the NEW names (after renaming)
##D   recode_vars = list(
##D     condition = c("1" = "control", "2" = "treatment_A", "3" = "treatment_B"),
##D     age = c("1" = "18-24", "2" = "25-34", "3" = "35-44", "4" = "45-54", "5" = "55+")
##D   ),
##D 
##D   # Now use the NEW names in your inclusion criteria
##D   inclusion_criteria = list(
##D     completed = raw_data$Finished == 1
##D   ),
##D 
##D   id_var = "ResponseId"
##D )
##D 
##D # Your clean data now has meaningful variable names and labels!
##D names(result$clean_data)
##D # [1] "ResponseId" "brand_attitude" "purchase_intent" "satisfaction" "age" "condition" ...
##D 
##D table(result$clean_data$condition)
##D # control  treatment_A  treatment_B
##D #     150          145          143
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("clean_survey_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cohens_d_table")
### * cohens_d_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cohens_d_table
### Title: Cohen's d Effect Size with Confidence Intervals
### Aliases: cohens_d_table

### ** Examples

## Not run: 
##D df <- data.frame(
##D   satisfaction = c(5, 6, 7, 8, 3, 4, 5, 6),
##D   condition = rep(c("treatment", "control"), each = 4)
##D )
##D 
##D effect_size <- cohens_d_table(df, "satisfaction", "condition")
##D print(effect_size)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cohens_d_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compare_cfa_models")
### * compare_cfa_models

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compare_cfa_models
### Title: Compare Nested CFA Models
### Aliases: compare_cfa_models

### ** Examples

## Not run: 
##D # Fit nested models
##D model_1factor <- "general =~ q1 + q2 + q3 + q4"
##D model_2factor <- "
##D   factor1 =~ q1 + q2
##D   factor2 =~ q3 + q4
##D "
##D 
##D fit1 <- run_cfa(model_1factor, data = df)
##D fit2 <- run_cfa(model_2factor, data = df)
##D 
##D # Compare
##D comparison <- compare_cfa_models(fit1, fit2)
##D print(comparison)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compare_cfa_models", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compare_groups_anova")
### * compare_groups_anova

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compare_groups_anova
### Title: Perform One-Way ANOVA with Comprehensive Diagnostics
### Aliases: compare_groups_anova

### ** Examples

# Example 1: Vector input
satisfaction <- c(5, 6, 7, 8, 7, 6,    # Group A
                  3, 4, 5, 4, 3, 4,    # Group B
                  7, 8, 9, 8, 9, 8)    # Group C
condition <- factor(rep(c("A", "B", "C"), each = 6))
result <- compare_groups_anova(satisfaction, condition)

# Example 2: Data frame with formula
data <- data.frame(
  score = c(45, 67, 54, 78, 56, 34, 56, 78, 89, 67),
  group = factor(rep(c("Control", "Treatment A", "Treatment B"), c(3, 4, 3)))
)
result <- compare_groups_anova(data, formula = score ~ group)

# View publication text
print(result, show_publication = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compare_groups_anova", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compare_ols_robust")
### * compare_ols_robust

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compare_ols_robust
### Title: Compare OLS and Robust Standard Errors
### Aliases: compare_ols_robust

### ** Examples

## Not run: 
##D model <- lm(y ~ x1 + x2, data = df)
##D comparison <- compare_ols_robust(model)
##D print(comparison)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compare_ols_robust", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compare_sem_models")
### * compare_sem_models

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compare_sem_models
### Title: Compare SEM Models
### Aliases: compare_sem_models

### ** Examples

## Not run: 
##D model1 <- run_sem(syntax1, data = mydata)
##D model2 <- run_sem(syntax2, data = mydata)
##D 
##D compare_sem_models(model1, model2,
##D                    model_names = c("Partial mediation", "Full mediation"))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compare_sem_models", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("consumer_survey")
### * consumer_survey

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: consumer_survey
### Title: Consumer Survey Example Dataset
### Aliases: consumer_survey
### Keywords: datasets

### ** Examples

# Load the dataset
data(consumer_survey)

# View structure
str(consumer_survey)

# Basic summary
summary(consumer_survey)

# Group comparison
library(dplyr)
consumer_survey %>%
  group_by(flyer_group) %>%
  summarise(
    n = n(),
    mean_spending = mean(spending, na.rm = TRUE),
    median_satisfaction = median(satisfaction, na.rm = TRUE)
  )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("consumer_survey", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("correlation_table")
### * correlation_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: correlation_table
### Title: Correlation Table with Confidence Intervals
### Aliases: correlation_table

### ** Examples

## Not run: 
##D df <- data.frame(
##D   satisfaction = rnorm(100),
##D   loyalty = rnorm(100),
##D   value = rnorm(100)
##D )
##D 
##D cors <- correlation_table(df, c("satisfaction", "loyalty", "value"))
##D print(cors)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("correlation_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_analysis_report")
### * create_analysis_report

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_analysis_report
### Title: Create a Transparent Analysis Report
### Aliases: create_analysis_report

### ** Examples

# Example 1: Simple analysis of a single variable
spending <- c(45.2, 67.8, 23.4, 89.1, 34.5, 56.7, 78.9, 12.3, 91.2, 43.5)
create_analysis_report(spending, title = "Consumer Spending Analysis")

# Example 2: Analysis with a data frame
consumer_data <- data.frame(
  spending = c(45, 67, 23, 89, 34, 56, 78, 12, 91, 43),
  satisfaction = c(7, 8, 6, 9, 7, 8, 9, 5, 9, 7)
)
create_analysis_report(consumer_data, variable = "satisfaction",
                      title = "Customer Satisfaction Analysis")

# Example 3: Group comparison
study_data <- data.frame(
  purchase_amount = c(45, 67, 23, 89, 34, 56, 78, 12, 91, 43,
                     34, 45, 29, 56, 41, 39, 49, 31, 52, 38),
  condition = c(rep("Treatment", 10), rep("Control", 10))
)
create_analysis_report(study_data,
                      variable = "purchase_amount",
                      group_var = "condition",
                      title = "Treatment Effect Analysis")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_analysis_report", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("efa_diagnostics")
### * efa_diagnostics

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: efa_diagnostics
### Title: EFA Diagnostics: KMO, Bartlett's Test, Parallel Analysis
### Aliases: efa_diagnostics

### ** Examples

## Not run: 
##D # Prepare item data
##D items <- customer_survey[, c("q1", "q2", "q3", "q4", "q5", "q6")]
##D 
##D # Run diagnostics
##D diagnostics <- efa_diagnostics(items)
##D print(diagnostics)
##D 
##D # Check suitability
##D if (diagnostics$suitable) {
##D   # Proceed with EFA
##D   efa_result <- perform_efa(items, n_factors = diagnostics$parallel$nfact)
##D }
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("efa_diagnostics", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("emmeans_contrasts")
### * emmeans_contrasts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: emmeans_contrasts
### Title: Planned Contrasts and Simple Effects via emmeans
### Aliases: emmeans_contrasts

### ** Examples

## Not run: 
##D # Fit ANOVA
##D model <- lm(satisfaction ~ condition, data = df)
##D 
##D # Pairwise comparisons with Tukey adjustment
##D contrasts <- emmeans_contrasts(model, specs = "condition")
##D 
##D # Treatment vs control
##D contrasts <- emmeans_contrasts(
##D   model,
##D   specs = "condition",
##D   contrasts = "trt.vs.ctrl",
##D   ref = "control"
##D )
##D 
##D # Simple effects (condition at each time point)
##D model2 <- lm(satisfaction ~ condition * time, data = df)
##D simple <- emmeans_contrasts(
##D   model2,
##D   specs = "condition",
##D   by = "time"
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("emmeans_contrasts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fisher_exact_test")
### * fisher_exact_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fisher_exact_test
### Title: Fisher's Exact Test for Small Samples
### Aliases: fisher_exact_test

### ** Examples

## Not run: 
##D # Small sample 2x2 table
##D result <- fisher_exact_test(
##D   data = df,
##D   x = "treatment",
##D   y = "response"
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fisher_exact_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("format_est_ci")
### * format_est_ci

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: format_est_ci
### Title: Format Estimates with Confidence Intervals
### Aliases: format_est_ci

### ** Examples

# Single value
format_est_ci(2.34, 1.23, 3.45)  # "2.34 [1.23, 3.45]"

# Multiple values
format_est_ci(
  est = c(2.34, 5.67),
  lo = c(1.23, 4.56),
  hi = c(3.45, 6.78)
)

# More decimal places
format_est_ci(2.3456, 1.2345, 3.4567, digits = 3)

# As percentages
format_est_ci(0.234, 0.123, 0.345, percent = TRUE)  # "23.4% [12.3%, 34.5%]"




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("format_est_ci", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("format_n")
### * format_n

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: format_n
### Title: Format Sample Sizes for Publication
### Aliases: format_n

### ** Examples

# Total sample
format_n(150)  # "N = 150"

# Subgroup sample
format_n(45, type = "n")  # "n = 45"

# Multiple values
format_n(c(100, 50), type = "n")  # c("n = 100", "n = 50")

# Without label
format_n(150, include_label = FALSE)  # "150"




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("format_n", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("format_p")
### * format_p

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: format_p
### Title: Format P-Values for Publication
### Aliases: format_p

### ** Examples

# APA style (no leading zero)
format_p(0.042)  # "p = .042"
format_p(0.0001) # "p < .001"
format_p(c(0.042, 0.0001, 0.523))

# Plain style (with leading zero)
format_p(0.042, style = "plain")  # "p = 0.042"

# More decimal places
format_p(0.04237, digits = 4)  # "p = .0424"




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("format_p", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("friedman_test")
### * friedman_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: friedman_test
### Title: Friedman Test
### Aliases: friedman_test

### ** Examples

set.seed(42)
df <- expand.grid(
  subject = 1:30,
  time = c("T1", "T2", "T3")
)
df$rating <- rpois(nrow(df), lambda = 4 + as.numeric(factor(df$time)))

result <- friedman_test(df, outcome = "rating", time = "time", subject = "subject")
print(result)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("friedman_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("icc_calculate")
### * icc_calculate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: icc_calculate
### Title: Calculate Intraclass Correlation Coefficient
### Aliases: icc_calculate

### ** Examples

## Not run: 
##D # Calculate ICC from unconditional model
##D icc <- icc_calculate(satisfaction ~ (1 | store_id), data = mydata)
##D print(icc)
##D 
##D # Or from fitted model
##D model <- run_mlm(satisfaction ~ price + (1 | store_id), data = mydata)
##D icc <- icc_calculate(model)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("icc_calculate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("import_research_data")
### * import_research_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: import_research_data
### Title: Import Data from CSV or SPSS with Interactive Variable Type
###   Checking
### Aliases: import_research_data

### ** Examples

## Not run: 
##D # Example 1: Import CSV file (interactive)
##D my_data <- import_research_data("survey_data.csv")
##D # Shows summary, asks for confirmation
##D # Returns cleaned data ready for analysis
##D 
##D # Example 2: Import SPSS file (non-interactive)
##D my_data <- import_research_data(
##D   "study1_data.sav",
##D   interactive = FALSE,
##D   create_report = TRUE
##D )
##D 
##D # Example 3: Access the imported data
##D result <- import_research_data("data.csv")
##D clean_data <- result$data  # The cleaned tibble
##D var_info <- result$variable_summary  # Info about variables
##D plot(result$validation_report)  # View quality report
##D 
##D # Example 4: Manual type specification
##D result <- import_research_data("data.csv")
##D # Then use check_variable_types() to review and modify
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("import_research_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("johnson_neyman")
### * johnson_neyman

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: johnson_neyman
### Title: Johnson-Neyman Technique for Regions of Significance
### Aliases: johnson_neyman

### ** Examples

# Price sensitivity moderated by brand loyalty
set.seed(42)
n <- 200
df <- data.frame(
  price = rnorm(n, 50, 10),
  loyalty = rnorm(n, 5, 1.5),
  purchase = rnorm(n, 5, 1)
)
df$purchase <- df$purchase - 0.1 * df$price + 0.3 * df$loyalty +
               0.05 * df$price * df$loyalty

model <- lm(purchase ~ price * loyalty, data = df)

# Find regions of significance
jn <- johnson_neyman(model, focal = "price", moderator = "loyalty")
print(jn)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("johnson_neyman", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("kruskal_wallis_test")
### * kruskal_wallis_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: kruskal_wallis_test
### Title: Kruskal-Wallis Test
### Aliases: kruskal_wallis_test

### ** Examples

set.seed(42)
df <- data.frame(
  rating = c(rpois(40, 3), rpois(40, 5), rpois(40, 4)),
  product = rep(c("A", "B", "C"), each = 40)
)

result <- kruskal_wallis_test(df, outcome = "rating", group = "product")
print(result)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("kruskal_wallis_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("logistic_assumptions")
### * logistic_assumptions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: logistic_assumptions
### Title: Check Logistic Regression Assumptions
### Aliases: logistic_assumptions

### ** Examples

# Fit model
set.seed(42)
df <- data.frame(
  purchased = sample(0:1, 150, replace = TRUE),
  price = rnorm(150, 50, 10),
  quality = rnorm(150, 5, 1),
  age = sample(25:65, 150, replace = TRUE)
)
model <- run_logistic(purchased ~ price + quality + age, data = df)

# Check assumptions
checks <- logistic_assumptions(model, data = df)
print(checks)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("logistic_assumptions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("make_table_md")
### * make_table_md

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make_table_md
### Title: Create Markdown Table
### Aliases: make_table_md

### ** Examples

## Not run: 
##D # Simple table
##D df <- data.frame(
##D   Variable = c("Age", "Income", "Satisfaction"),
##D   Mean = c(34.5, 65000, 7.2),
##D   SD = c(8.3, 15000, 1.4)
##D )
##D make_table_md(df, caption = "Descriptive Statistics")
##D 
##D # Custom alignment
##D make_table_md(df, align = c("l", "r", "r"))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make_table_md", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("make_table_tsv")
### * make_table_tsv

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make_table_tsv
### Title: Create TSV Table for Copy-Paste
### Aliases: make_table_tsv

### ** Examples

## Not run: 
##D # Create table
##D df <- data.frame(
##D   Variable = c("Age", "Income", "Satisfaction"),
##D   Mean = c(34.5, 65000, 7.2),
##D   SD = c(8.3, 15000, 1.4)
##D )
##D 
##D # Print as TSV
##D make_table_tsv(df)
##D 
##D # Copy the output and paste into Excel/Sheets
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make_table_tsv", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mann_whitney_test")
### * mann_whitney_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mann_whitney_test
### Title: Mann-Whitney U Test (Wilcoxon Rank-Sum Test)
### Aliases: mann_whitney_test

### ** Examples

# Compare purchase intention between groups
set.seed(42)
df <- data.frame(
  intention = c(rpois(50, 3), rpois(50, 5)),  # Non-normal
  condition = rep(c("Control", "Treatment"), each = 50)
)

result <- mann_whitney_test(df, outcome = "intention", group = "condition")
print(result)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mann_whitney_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mcnemar_test")
### * mcnemar_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mcnemar_test
### Title: McNemar's Test for Paired Categorical Data
### Aliases: mcnemar_test

### ** Examples

## Not run: 
##D # Before-after purchase behavior
##D result <- mcnemar_test(
##D   data = df,
##D   var1 = "purchased_before",
##D   var2 = "purchased_after"
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mcnemar_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mediation_parallel")
### * mediation_parallel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mediation_parallel
### Title: Parallel Mediation Analysis
### Aliases: mediation_parallel

### ** Examples

## Not run: 
##D # Two parallel mediators
##D result <- mediation_parallel(
##D   data = consumer_data,
##D   x = "brand_reputation",
##D   mediators = c("perceived_quality", "brand_trust"),
##D   y = "purchase_intention",
##D   boot_samples = 5000,
##D   seed = 123
##D )
##D print(result)
##D 
##D # Three mediators with covariates
##D result2 <- mediation_parallel(
##D   data = mydata,
##D   x = "ad_exposure",
##D   mediators = c("awareness", "attitude", "intention"),
##D   y = "behavior",
##D   covariates = c("age", "gender"),
##D   boot_samples = 5000
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mediation_parallel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mediation_serial")
### * mediation_serial

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mediation_serial
### Title: Serial Mediation Analysis
### Aliases: mediation_serial

### ** Examples

## Not run: 
##D # Two-mediator serial model
##D result <- mediation_serial(
##D   data = consumer_data,
##D   x = "advertising",
##D   mediators = c("awareness", "attitude"),
##D   y = "purchase",
##D   boot_samples = 5000,
##D   seed = 123
##D )
##D print(result)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mediation_serial", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mediation_simple")
### * mediation_simple

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mediation_simple
### Title: Simple Mediation Analysis with Bootstrap Confidence Intervals
### Aliases: mediation_simple

### ** Examples

# Brand attitude mediates price -> purchase intention
set.seed(42)
n <- 200
df <- data.frame(
  price_perception = rnorm(n, 5, 1),
  brand_attitude = rnorm(n, 5, 1),
  purchase_intention = rnorm(n, 5, 1)
)
# Create mediation relationship
df$brand_attitude <- df$brand_attitude + 0.5 * df$price_perception
df$purchase_intention <- df$purchase_intention +
  0.3 * df$price_perception + 0.6 * df$brand_attitude

# Run mediation analysis
result <- mediation_simple(
  data = df,
  x = "price_perception",
  m = "brand_attitude",
  y = "purchase_intention",
  boot_samples = 1000,  # Use 5000+ for publications
  seed = 42
)

print(result)
summary(result)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mediation_simple", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mlm_assumptions")
### * mlm_assumptions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mlm_assumptions
### Title: Check MLM Assumptions
### Aliases: mlm_assumptions

### ** Examples

## Not run: 
##D model <- run_mlm(y ~ x + (1 | group), data = mydata)
##D assumptions <- mlm_assumptions(model)
##D print(assumptions$summary)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mlm_assumptions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("moderated_mediation")
### * moderated_mediation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: moderated_mediation
### Title: Moderated Mediation Analysis (Hayes PROCESS Models 7, 8, 14)
### Aliases: moderated_mediation

### ** Examples

# Model 7: Trust mediates CSR -> loyalty, moderated by involvement
set.seed(42)
n <- 200
df <- data.frame(
  csr = rnorm(n),
  trust = rnorm(n),
  loyalty = rnorm(n),
  involvement = rnorm(n)
)
# Create moderated mediation structure
df$trust <- df$trust + 0.5 * df$csr
df$loyalty <- df$loyalty + 0.3 * df$trust +
  0.4 * df$involvement + 0.2 * df$trust * df$involvement

result <- moderated_mediation(
  data = df,
  x = "csr",
  m = "trust",
  y = "loyalty",
  moderator = "involvement",
  model = "7",
  boot_samples = 1000,  # Use 5000+ for publication
  seed = 42
)
print(result)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("moderated_mediation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("odds_ratio_table")
### * odds_ratio_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: odds_ratio_table
### Title: Odds Ratio with Confidence Interval
### Aliases: odds_ratio_table

### ** Examples

## Not run: 
##D # Treatment effect on success
##D result <- odds_ratio_table(
##D   data = df,
##D   x = "treatment",  # Exposed vs unexposed
##D   y = "success"     # Outcome
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("odds_ratio_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("perform_efa")
### * perform_efa

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: perform_efa
### Title: Perform Exploratory Factor Analysis with Beautiful
###   Visualizations
### Aliases: perform_efa

### ** Examples

## Not run: 
##D # Example: Analyze customer experience survey
##D # Suppose we have 9 items that might measure 3 dimensions
##D 
##D library(dplyr)
##D library(ggplot2)
##D 
##D # Run EFA to discover factor structure
##D efa_results <- perform_efa(
##D   data = customer_data,
##D   items = c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"),
##D   item_labels = c(
##D     q1 = "Store cleanliness",
##D     q2 = "Product selection",
##D     q3 = "Staff friendliness",
##D     # ... etc
##D   )
##D )
##D 
##D # View the scree plot
##D print(efa_results$scree_plot)
##D 
##D # View loading heatmap
##D print(efa_results$loading_plot)
##D 
##D # Get interpretation
##D cat(efa_results$interpretation)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("perform_efa", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pseudo_r2")
### * pseudo_r2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pseudo_r2
### Title: Calculate Pseudo R-squared for Logistic Regression
### Aliases: pseudo_r2

### ** Examples

set.seed(42)
df <- data.frame(
  outcome = sample(0:1, 100, replace = TRUE),
  x1 = rnorm(100),
  x2 = rnorm(100)
)
model <- run_logistic(outcome ~ x1 + x2, data = df)
pseudo_r2(model)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pseudo_r2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("reverse_score_likert")
### * reverse_score_likert

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: reverse_score_likert
### Title: Reverse Score Likert-Type Items
### Aliases: reverse_score_likert

### ** Examples

# Basic usage (1-7 scale)
responses <- c(1, 2, 3, 4, 5, 6, 7)
reverse_score_likert(responses)  # c(7, 6, 5, 4, 3, 2, 1)

# 1-5 scale
responses_5pt <- c(1, 2, 3, 4, 5)
reverse_score_likert(responses_5pt, min = 1, max = 5)  # c(5, 4, 3, 2, 1)

# Handles NA
responses_na <- c(1, NA, 3, 5, NA, 7)
reverse_score_likert(responses_na)  # c(7, NA, 5, 3, NA, 1)

# With out-of-range values (strict = FALSE)
responses_bad <- c(1, 2, 8, 4, 5)  # 8 is out of range
reverse_score_likert(responses_bad, strict = FALSE)
# Warning issued, 8 preserved




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("reverse_score_likert", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rm_pairwise")
### * rm_pairwise

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rm_pairwise
### Title: Pairwise Comparisons for Repeated Measures ANOVA
### Aliases: rm_pairwise

### ** Examples

## Not run: 
##D # After significant RM ANOVA
##D # Create example data
##D set.seed(42)
##D df <- expand.grid(
##D   id = 1:30,
##D   time = c("Before", "During", "After")
##D )
##D df$satisfaction <- rnorm(nrow(df), mean = 5 + as.numeric(factor(df$time)), sd = 1)
##D 
##D result <- run_rm_anova(df, outcome = "satisfaction", within = "time", subject = "id")
##D pairwise <- rm_pairwise(result, factor = "time")
##D print(pairwise)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rm_pairwise", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("row_sd")
### * row_sd

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: row_sd
### Title: Calculate Row-Wise Standard Deviation
### Aliases: row_sd

### ** Examples

# Basic usage
df <- data.frame(
  q1 = c(5, 7, 1, 4),
  q2 = c(6, 7, 1, 5),
  q3 = c(5, 7, 1, 3)
)
row_sd(df)  # SD for each row

# Specific items
row_sd(df, items = c("q1", "q2"))

# With missing values
df_na <- data.frame(
  q1 = c(5, NA, 1),
  q2 = c(6, 7, NA),
  q3 = c(5, 7, 1)
)
row_sd(df_na, na.rm = TRUE)

# Detect straight-lining
straight_liner <- data.frame(q1 = 7, q2 = 7, q3 = 7)
row_sd(straight_liner)  # Returns 0




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("row_sd", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("run_anova")
### * run_anova

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: run_anova
### Title: Run ANOVA with Type II or Type III Sums of Squares
### Aliases: run_anova

### ** Examples

## Not run: 
##D # Fit model
##D model <- lm(satisfaction ~ condition * time, data = df)
##D 
##D # Type II SS (default)
##D anova_result <- run_anova(model)
##D 
##D # Type III SS
##D anova_result <- run_anova(model, type = "III")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("run_anova", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("run_cfa")
### * run_cfa

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: run_cfa
### Title: Run Confirmatory Factor Analysis (CFA)
### Aliases: run_cfa

### ** Examples

## Not run: 
##D # Define CFA model
##D model <- "
##D   # Latent variables
##D   satisfaction =~ sat1 + sat2 + sat3
##D   loyalty =~ loy1 + loy2 + loy3
##D 
##D   # Covariance
##D   satisfaction ~~ loyalty
##D "
##D 
##D # Run CFA
##D fit <- run_cfa(model, data = customer_survey)
##D 
##D # Get fit indices
##D fit_indices <- tidy_cfa_fit(fit)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("run_cfa", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("run_logistic")
### * run_logistic

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: run_logistic
### Title: Run Logistic Regression for Binary Outcomes
### Aliases: run_logistic

### ** Examples

# Purchase decision based on price and quality perceptions
set.seed(42)
n <- 200
df <- data.frame(
  purchased = sample(0:1, n, replace = TRUE, prob = c(0.4, 0.6)),
  price = rnorm(n, 50, 10),
  quality = rnorm(n, 5, 1),
  age = sample(18:65, n, replace = TRUE)
)

# Fit logistic regression
model <- run_logistic(purchased ~ price + quality + age, data = df)
summary(model)

# Get publication-ready results with odds ratios
results <- tidy_logistic(model)
print(results)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("run_logistic", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("run_mlm")
### * run_mlm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: run_mlm
### Title: Multi-Level Modeling (MLM/HLM)
### Aliases: run_mlm

### ** Examples

## Not run: 
##D # Random intercept model: Satisfaction nested within stores
##D model1 <- run_mlm(
##D   satisfaction ~ price + quality + (1 | store_id),
##D   data = consumer_data
##D )
##D print(model1)
##D 
##D # Random slope model: Price effect varies by store
##D model2 <- run_mlm(
##D   satisfaction ~ price + quality + (price | store_id),
##D   data = consumer_data
##D )
##D 
##D # Crossed random effects: Consumers and products
##D model3 <- run_mlm(
##D   rating ~ brand + price + (1 | consumer_id) + (1 | product_id),
##D   data = rating_data
##D )
##D 
##D # Extract results
##D tidy_mlm(model1)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("run_mlm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("run_rm_anova")
### * run_rm_anova

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: run_rm_anova
### Title: Repeated Measures ANOVA
### Aliases: run_rm_anova

### ** Examples

## Not run: 
##D # Within-subjects design: Satisfaction across 3 time points
##D set.seed(42)
##D df <- expand.grid(
##D   subject = 1:30,
##D   time = c("Before", "During", "After")
##D )
##D df$satisfaction <- rnorm(nrow(df), mean = 5 + as.numeric(factor(df$time)), sd = 1)
##D 
##D result <- run_rm_anova(
##D   data = df,
##D   outcome = "satisfaction",
##D   within = "time",
##D   subject = "subject"
##D )
##D print(result)
##D 
##D # Mixed design: Time (within) x Condition (between)
##D df$condition <- rep(c("Control", "Treatment"), each = 45)
##D result <- run_rm_anova(
##D   data = df,
##D   outcome = "satisfaction",
##D   within = "time",
##D   between = "condition",
##D   subject = "subject"
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("run_rm_anova", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("run_sem")
### * run_sem

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: run_sem
### Title: Structural Equation Modeling (SEM) Path Analysis
### Aliases: run_sem

### ** Examples

## Not run: 
##D # Simple mediation model
##D model <- "
##D   # Direct effects
##D   satisfaction ~ b*quality + c*price
##D   quality ~ a*brand_reputation
##D 
##D   # Indirect effect
##D   indirect := a * b
##D "
##D 
##D result <- run_sem(model, data = consumer_survey, se = "bootstrap",
##D                   bootstrap = 5000, seed = 123)
##D print(result)
##D 
##D # Path model with multiple outcomes
##D model2 <- c(
##D   "purchase_intent ~ attitude + subjective_norm",
##D   "attitude ~ ad_exposure + prior_belief",
##D   "subjective_norm ~ ad_exposure"
##D )
##D 
##D result2 <- run_sem(model2, data = mydata)
##D tidy_sem(result2)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("run_sem", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simple_slopes")
### * simple_slopes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simple_slopes
### Title: Simple Slopes Analysis for Interaction Effects
### Aliases: simple_slopes

### ** Examples

# Price sensitivity moderated by brand loyalty
set.seed(42)
n <- 150
df <- data.frame(
  price = rnorm(n, 50, 10),
  loyalty = rnorm(n, 5, 1.5),
  purchase = rnorm(n, 5, 1)
)
# Interaction: price effect depends on loyalty
df$purchase <- df$purchase - 0.1 * df$price + 0.3 * df$loyalty +
               0.05 * df$price * df$loyalty

# Fit model with interaction
model <- lm(purchase ~ price * loyalty, data = df)

# Probe interaction with simple slopes
slopes <- simple_slopes(model, focal = "price", moderator = "loyalty")
print(slopes)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simple_slopes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_cfa_fit")
### * tidy_cfa_fit

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_cfa_fit
### Title: Extract Tidy CFA Fit Indices
### Aliases: tidy_cfa_fit

### ** Examples

## Not run: 
##D # Run CFA
##D model <- "satisfaction =~ q1 + q2 + q3"
##D fit <- run_cfa(model, data = df)
##D 
##D # Extract fit indices
##D fit_indices <- tidy_cfa_fit(fit)
##D print(fit_indices)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_cfa_fit", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_lm_robust")
### * tidy_lm_robust

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_lm_robust
### Title: Tidy Regression Results with Robust Standard Errors
### Aliases: tidy_lm_robust

### ** Examples

## Not run: 
##D # Fit model
##D model <- lm(satisfaction ~ price + quality + service, data = df)
##D 
##D # Check for heteroscedasticity
##D assumptions <- assumption_checks(model)
##D 
##D # If violated, use robust SEs
##D robust_results <- tidy_lm_robust(model, hc_type = "HC3")
##D print(robust_results)
##D 
##D # Compare to OLS
##D ols_results <- broom::tidy(model, conf.int = TRUE)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_lm_robust", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_logistic")
### * tidy_logistic

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_logistic
### Title: Tidy Logistic Regression Results with Odds Ratios
### Aliases: tidy_logistic

### ** Examples

# Fit model
set.seed(42)
df <- data.frame(
  purchased = sample(0:1, 100, replace = TRUE),
  price = rnorm(100, 50, 10),
  quality = rnorm(100, 5, 1)
)
model <- run_logistic(purchased ~ price + quality, data = df)

# Get odds ratios with CIs
results <- tidy_logistic(model)
print(results)

# Get log-odds (coefficient scale)
results_logodds <- tidy_logistic(model, exponentiate = FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_logistic", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_mlm")
### * tidy_mlm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_mlm
### Title: Extract Publication-Ready MLM Results
### Aliases: tidy_mlm

### ** Examples

## Not run: 
##D model <- run_mlm(y ~ x + (1 | group), data = mydata)
##D tidy_mlm(model)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_mlm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_sem")
### * tidy_sem

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_sem
### Title: Extract Publication-Ready SEM Results
### Aliases: tidy_sem

### ** Examples

## Not run: 
##D result <- run_sem(model, data = mydata)
##D tidy_sem(result)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_sem", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wilcoxon_signed_rank_test")
### * wilcoxon_signed_rank_test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wilcoxon_signed_rank_test
### Title: Wilcoxon Signed-Rank Test
### Aliases: wilcoxon_signed_rank_test

### ** Examples

set.seed(42)
df <- data.frame(
  pre = rpois(50, 4),
  post = rpois(50, 6)
)

result <- wilcoxon_signed_rank_test(df, before = "pre", after = "post")
print(result)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wilcoxon_signed_rank_test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_table")
### * write_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_table
### Title: Write Table to File
### Aliases: write_table

### ** Examples

## Not run: 
##D df <- data.frame(
##D   Variable = c("Age", "Income"),
##D   Mean = c(34.5, 65000),
##D   SD = c(8.3, 15000)
##D )
##D 
##D # Save as TSV
##D write_table(df, "results/descriptives.tsv")
##D 
##D # Save as markdown
##D write_table(df, "results/table1.md", format = "md")
##D 
##D # Save as RDS (preserves types)
##D write_table(df, "results/data.rds", format = "rds")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
