## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(consumeR)

## ----descriptive-example------------------------------------------------------
# Sample data: Consumer spending amounts in dollars
consumer_spending <- c(45.20, 67.80, 23.40, 89.10, 34.50,
                       56.70, 78.90, 12.30, 91.20, 43.50)

# Calculate summary statistics
stats <- calculate_summary_stats(consumer_spending)

# View results
print(stats)

## ----missing-data-------------------------------------------------------------
# Data with missing values (common in real research)
satisfaction_scores <- c(7, 8, NA, 6, 9, 7, NA, 8, 7, 9)

# The function removes NAs and reports how many
stats_with_missing <- calculate_summary_stats(satisfaction_scores)

## ----customize-stats----------------------------------------------------------
# Fewer statistics, more decimal places
calculate_summary_stats(
  consumer_spending,
  include_all = FALSE,  # Skip variance, range, IQR
  round_digits = 3      # More precision
)

## ----group-comparison---------------------------------------------------------
# Simulated A/B test data
# Treatment group: Received promotional email
treatment_purchases <- c(45.20, 67.80, 23.40, 89.10, 34.50,
                        56.70, 78.90, 12.30, 91.20, 43.50)

# Control group: No promotional email
control_purchases <- c(34.10, 45.20, 28.90, 56.30, 41.20,
                      38.70, 49.10, 31.40, 52.80, 38.90)

# Test if groups are different
comparison <- test_group_differences(treatment_purchases, control_purchases)

# View results
print(comparison$interpretation)

## ----test-selection-----------------------------------------------------------
# Automatic selection based on sample size
result_auto <- test_group_differences(treatment_purchases, control_purchases,
                                      test_type = "auto")

# Manual selection: Force t-test
result_ttest <- test_group_differences(treatment_purchases, control_purchases,
                                       test_type = "t.test")

# Manual selection: Force Wilcoxon (non-parametric)
result_wilcox <- test_group_differences(treatment_purchases, control_purchases,
                                        test_type = "wilcoxon")

## ----one-sided----------------------------------------------------------------
# Two-sided: Is there ANY difference? (default)
test_group_differences(treatment_purchases, control_purchases,
                      alternative = "two.sided")

# One-sided: Is treatment GREATER than control?
test_group_differences(treatment_purchases, control_purchases,
                      alternative = "greater")

# One-sided: Is treatment LESS than control?
test_group_differences(treatment_purchases, control_purchases,
                      alternative = "less")

## ----paired-------------------------------------------------------------------
# Weight before diet program
before <- c(180, 195, 210, 175, 188, 202)

# Weight after diet program (same individuals)
after <- c(175, 190, 205, 172, 185, 198)

# Paired test accounts for individual differences
test_group_differences(before, after, paired = TRUE)

## ----full-report--------------------------------------------------------------
# Create a comprehensive report
create_analysis_report(
  consumer_spending,
  title = "Consumer Spending Analysis - Study 1"
)

## ----report-groups------------------------------------------------------------
# Combine data into a data frame (common format)
study_data <- data.frame(
  purchase_amount = c(treatment_purchases, control_purchases),
  condition = c(rep("Treatment", length(treatment_purchases)),
               rep("Control", length(control_purchases)))
)

# Generate report with group comparison
create_analysis_report(
  data = study_data,
  variable = "purchase_amount",
  group_var = "condition",
  title = "Promotional Email Effectiveness Study"
)

## ----save-report, eval=FALSE--------------------------------------------------
# # Save report to a text file for sharing with reviewers
# create_analysis_report(
#   data = study_data,
#   variable = "purchase_amount",
#   group_var = "condition",
#   title = "Promotional Email Effectiveness Study",
#   report_file = "analysis_report.txt"
# )

## ----complete-workflow--------------------------------------------------------
# SCENARIO: Testing if a new store layout increases spending

# Step 1: Prepare data
old_layout_spending <- c(42.50, 38.20, 51.30, 45.80, 39.90,
                        47.60, 44.20, 41.30, 48.70, 43.50)

new_layout_spending <- c(48.30, 52.10, 61.20, 55.40, 58.70,
                        54.20, 59.80, 51.90, 63.40, 56.30)

# Step 2: Examine each group separately
cat("OLD LAYOUT STATISTICS:\n")
old_stats <- calculate_summary_stats(old_layout_spending)
print(old_stats)

cat("\n\nNEW LAYOUT STATISTICS:\n")
new_stats <- calculate_summary_stats(new_layout_spending)
print(new_stats)

# Step 3: Statistical comparison
cat("\n\nGROUP COMPARISON:\n")
comparison <- test_group_differences(old_layout_spending, new_layout_spending)
cat(comparison$interpretation, "\n")

# Step 4: Generate full report for reviewers
cat("\n\n")
cat(rep("=", 60), "\n", sep = "")
cat("FULL REPORT FOR PEER REVIEW\n")
cat(rep("=", 60), "\n\n", sep = "")

layout_data <- data.frame(
  spending = c(old_layout_spending, new_layout_spending),
  layout = c(rep("Old Layout", 10), rep("New Layout", 10))
)

create_analysis_report(
  data = layout_data,
  variable = "spending",
  group_var = "layout",
  title = "Store Layout Experiment Analysis"
)

## ----report-missing, eval=FALSE-----------------------------------------------
# # Good practice: Let the function report missing values
# calculate_summary_stats(data_with_nas)
# 
# # The automatic message tells reviewers exactly what was excluded

## ----document-decisions, eval=FALSE-------------------------------------------
# # Good: Explain why you chose a specific test
# result <- test_group_differences(
#   group1, group2,
#   test_type = "wilcoxon",  # Using non-parametric test because...
#   alternative = "greater"   # One-sided based on hypothesis that...
# )

## ----complete-reports, eval=FALSE---------------------------------------------
# # Good: Generate a report that includes everything
# create_analysis_report(
#   data = my_data,
#   variable = "outcome",
#   group_var = "treatment",
#   title = "Complete Analysis for Reviewers",
#   report_file = "reviewer_report.txt"
# )

## ----effect-size--------------------------------------------------------------
result <- test_group_differences(treatment_purchases, control_purchases)

cat("Mean difference:", result$difference, "\n")
cat("This means Treatment group spent $", result$difference,
    " more on average\n", sep = "")

## ----satisfaction-survey------------------------------------------------------
# Satisfaction ratings (1-10 scale)
satisfaction <- c(8, 7, 9, 6, 8, 7, 9, 8, 7, 6, 9, 8, 7, 8, 9)

stats <- calculate_summary_stats(satisfaction)
cat("Average satisfaction:", stats$mean, "out of 10\n")
cat("Median satisfaction:", stats$median, "out of 10\n")
cat("Standard deviation:", stats$sd, "\n")

## ----price-sensitivity--------------------------------------------------------
# Purchases at low price point
low_price_purchases <- c(12, 15, 11, 14, 13, 16, 12, 14, 15, 13)

# Purchases at high price point
high_price_purchases <- c(8, 6, 9, 7, 8, 6, 7, 9, 8, 7)

test_group_differences(low_price_purchases, high_price_purchases,
                      alternative = "greater")

## ----longitudinal-------------------------------------------------------------
# Customer engagement before campaign
engagement_before <- c(45, 52, 48, 51, 49, 53, 47, 50, 46, 54)

# Same customers after campaign
engagement_after <- c(52, 58, 54, 57, 55, 60, 53, 56, 52, 61)

test_group_differences(engagement_before, engagement_after,
                      paired = TRUE,
                      alternative = "less")  # Testing if AFTER is greater

## ----troubleshoot-1, error=TRUE, eval=FALSE-----------------------------------
try({
# # Problem: Data is not numeric
# test_group_differences(c("high", "low"), c("medium", "low"))
# 
# # Solution: Use numeric data
# test_group_differences(c(10, 5), c(7, 5))
})

## ----troubleshoot-2, error=TRUE, eval=FALSE-----------------------------------
try({
# # Problem: Too few data points
# test_group_differences(c(5), c(3, 4))
# 
# # Solution: Collect more data or use different analysis
})

## ----troubleshoot-3-----------------------------------------------------------
# Data with NAs - function handles automatically
data_with_gaps <- c(5, NA, 7, 8, NA, 6, 9)
calculate_summary_stats(data_with_gaps)
# Note the message about removed values

## ----citation, eval=FALSE-----------------------------------------------------
# citation("consumeR")

