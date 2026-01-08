# 📊 Data Import & Variable Type Checking Guide

## New Functions for Data Management

The consumeR package now includes powerful, user-friendly functions for importing and validating your data from CSV and SPSS files.

---

## Why These Functions?

When you import research data, several things can go wrong:
- **Wrong variable types**: Numbers stored as text, categories as numbers
- **Lost SPSS labels**: Value labels and variable labels disappear
- **Hidden issues**: Missing data patterns not immediately visible
- **No documentation**: No record of what was imported and how

Our new functions solve all of these problems with complete transparency!

---

## Function 1: `import_research_data()`

### What It Does

1. ✅ **Imports** CSV or SPSS (.sav) files
2. ✅ **Detects** variable types automatically
3. ✅ **Suggests** improvements (e.g., "this should be a factor")
4. ✅ **Converts** types intelligently
5. ✅ **Creates** a visual quality report
6. ✅ **Documents** everything for peer review

### Basic Usage

```r
library(consumeR)

# Import a CSV file (interactive mode)
result <- import_research_data("my_survey_data.csv")

# Access your cleaned data
my_data <- result$data

# View variable summary
View(result$variable_summary)

# See the quality report
print(result$validation_report)
```

### Import SPSS Files

```r
# Import SPSS file - preserves all labels!
result <- import_research_data("study1_data.sav")

# SPSS value labels are automatically converted to R factors
# Variable labels are preserved
my_data <- result$data
```

### What You'll See (Interactive Mode)

```
╔════════════════════════════════════════════════════════╗
║        TRANSPARENT DATA IMPORT & TYPE CHECKING         ║
╚════════════════════════════════════════════════════════╝

File: survey_data.csv
Type: CSV file

Step 1: Importing data...
  ✓ CSV file imported successfully
  Dimensions: 200 rows × 15 columns

Step 2: Analyzing variable types...

Step 3: Detecting potential type improvements...
  ✓ Found 3 suggested improvements

╔════════════════════════════════════════════════════════╗
║              VARIABLE TYPE SUMMARY                     ║
╚════════════════════════════════════════════════════════╝

Current variable types:
  numeric: 8 variables
  character: 5 variables
  logical: 2 variables

SUGGESTED CHANGES:
─────────────────────────────────────────────────────────

1. gender
   Current: character
   Suggested: factor
   Reason: Text with 2 categories - convert to factor
   Examples: Male, Female, Male

2. condition
   Current: character
   Suggested: factor
   Reason: Text with 2 categories - convert to factor
   Examples: Treatment, Control, Treatment

3. satisfaction_rating
   Current: numeric
   Suggested: factor
   Reason: Only 5 unique values - likely categorical
   Examples: 1, 2, 3

─────────────────────────────────────────────────────────

FULL VARIABLE LIST:
─────────────────────────────────────────────────────────
      variable current_type n_unique percent_missing
1  customer_id      numeric      200             0.0
2       gender    character        2             0.0
3    condition    character        2             0.0
4          age      numeric       45             2.5
...

Step 4: Applying suggested type conversions...
  ✓ gender: Converted to factor
  ✓ condition: Converted to factor
  ✓ satisfaction_rating: Converted to factor

Step 5: Creating data validation report...
  ✓ Validation plot created

╔════════════════════════════════════════════════════════╗
║                   IMPORT COMPLETE                      ║
╚════════════════════════════════════════════════════════╝

✓ Data imported: 200 rows × 15 columns
✓ Applied 3 type conversions
✓ Data ready for analysis

To view data quality report:
  print(result$validation_report)

To access your data:
  my_data <- result$data
```

---

## Function 2: `check_variable_types()`

### What It Does

After importing data (or anytime), you can review and modify variable types interactively.

### Usage

```r
# Review all variables in your dataset
checked <- check_variable_types(my_data)

# See detailed information about each variable
View(checked$variable_info)
```

### What You'll See

```
╔════════════════════════════════════════════════════════╗
║         INTERACTIVE VARIABLE TYPE CHECKER              ║
╚════════════════════════════════════════════════════════╝

Dataset has 200 rows and 15 variables

Variable types:
───────────────────────────────────────────────────────────

1. customer_id
   Type: numeric
   Unique values: 200
   Missing: 0
   Examples: 1, 2, 3, 4, 5

2. gender
   Type: factor
   Unique values: 2
   Missing: 0
   Examples: Male, Female, Male, Female, Male

3. age
   Type: numeric
   Unique values: 45
   Missing: 5
   Examples: 25, 34, 45, 23, 56
   💡 SUGGESTION: Consider if this should be binned into age groups

4. satisfaction_q1
   Type: numeric
   Unique values: 7
   Missing: 2
   Examples: 5, 6, 7, 5, 6
   💡 SUGGESTION: Consider converting to factor (categorical)

...
```

---

## Complete Workflow Example

Here's a typical research workflow using the new functions:

```r
library(consumeR)
library(dplyr)

# Step 1: Import your data
result <- import_research_data("cloud9_survey.csv")

# Step 2: Access the cleaned data
survey_data <- result$data

# Step 3: Check variable summary
print(result$variable_summary)

# Step 4: View quality report (missing data patterns)
print(result$validation_report)

# Step 5: Verify specific variables if needed
check_variable_types(survey_data)

# Step 6: Make manual adjustments if needed
survey_data <- survey_data %>%
  mutate(
    # Convert Likert scales to ordered factors
    satisfaction = factor(satisfaction,
                         levels = 1:7,
                         ordered = TRUE),
    # Create age groups
    age_group = cut(age,
                   breaks = c(0, 25, 35, 50, 100),
                   labels = c("18-25", "26-35", "36-50", "50+"))
  )

# Step 7: Now ready for analysis!
calculate_summary_stats(survey_data$satisfaction)
```

---

## Working with SPSS Files

### Why SPSS Support Matters

SPSS files (.sav) contain valuable metadata:
- **Value labels**: 1 = "Male", 2 = "Female"
- **Variable labels**: Descriptive names for each variable
- **Missing value codes**: Specific codes for different types of missing data

Our function preserves all of this!

### Example: SPSS Import

```r
# Import SPSS file
result <- import_research_data("dissertation_data.sav")

# SPSS labels are automatically converted
spss_data <- result$data

# Example: If SPSS file had gender coded as 1/2 with labels
# After import, it's a factor with levels "Male" and "Female"
table(spss_data$gender)
#> Female   Male
#>    102     98

# Variable labels are preserved (if available)
# Access them with:
attr(spss_data$gender, "label")
```

---

## Non-Interactive Mode

For scripts or automated workflows:

```r
# Import without prompts
result <- import_research_data(
  "data.csv",
  interactive = FALSE,    # No prompts
  auto_convert = TRUE,    # Still apply smart conversions
  create_report = TRUE    # Generate quality report
)

# Automatically applied conversions are logged
result$import_log$changes_made
```

---

## Data Validation Report

The validation report is a ggplot2 object showing:
- **Missing data** for each variable
- **Variable types** (color-coded)
- **Data quality** at a glance

```r
# View the report
print(result$validation_report)

# Customize it (it's a ggplot2 object!)
result$validation_report +
  labs(title = "My Custom Title") +
  theme_bw()

# Save to file
ggsave("data_quality_report.png",
       result$validation_report,
       width = 10, height = 8)
```

---

## What Gets Documented for Peer Review

The function creates a complete audit trail:

```r
# View import log
result$import_log

#> $file_path
#> [1] "survey_data.csv"
#>
#> $file_type
#> [1] "csv"
#>
#> $import_time
#> [1] "2026-01-08 17:30:45 EST"
#>
#> $n_rows
#> [1] 200
#>
#> $n_cols
#> [1] 15
#>
#> $changes_made
#> # A tibble: 3 × 4
#>   variable      current_type suggested_type suggestion_reason
#>   <chr>         <chr>        <chr>          <chr>
#> 1 gender        character    factor         "Text with 2 categories..."
#> 2 condition     character    factor         "Text with 2 categories..."
#> 3 satisfaction  numeric      factor         "Only 5 unique values..."
```

This documentation can go directly into your methods section!

---

## Common Use Cases

### Use Case 1: First Time Importing Survey Data

```r
# Cloud 9 customer satisfaction survey
result <- import_research_data("cloud9_satisfaction.csv")

# Review what was changed
result$variable_summary %>%
  filter(change_suggested) %>%
  select(variable, current_type, suggested_type, suggestion_reason)

# Use the cleaned data
customer_data <- result$data
```

### Use Case 2: SPSS from Qualtrics/SurveyMonkey

```r
# Import SPSS file from Qualtrics
result <- import_research_data("qualtrics_export.sav")

# All Qualtrics coding is preserved
# Multiple choice questions become factors
# Likert scales keep their labels
qualtrics_data <- result$data
```

### Use Case 3: Checking Data from Collaborator

```r
# Someone sent you data - verify it's correct
result <- import_research_data("collaborator_data.csv")

# View quality report
print(result$validation_report)

# Check for issues
result$variable_summary %>%
  filter(percent_missing > 10)  # Variables with >10% missing

# Verify types
check_variable_types(result$data)
```

### Use Case 4: Reproducible Analysis Script

```r
# At the top of your analysis script
result <- import_research_data(
  "study1_data.csv",
  interactive = FALSE,
  create_report = TRUE
)

# Save the import log for documentation
saveRDS(result$import_log, "data_import_log.rds")

# Save quality report
ggsave("figures/data_quality.png", result$validation_report)

# Use the data
study_data <- result$data

# Continue with analysis...
calculate_alpha(study_data, items = c("sat_1", "sat_2", "sat_3"))
```

---

## Tips for Best Practices

### 1. Always Create a Report

```r
result <- import_research_data(
  "data.csv",
  create_report = TRUE  # Always do this!
)

# Include the report in your paper's supplementary materials
ggsave("supplementary/data_quality.png", result$validation_report)
```

### 2. Document Type Conversions

```r
# The function creates a log of all changes
conversions <- result$import_log$changes_made

# Include this in your methods section:
# "Variables were converted from their import types as follows:
#  - gender: character → factor (2 categories)
#  - condition: character → factor (2 categories)
#  - satisfaction: numeric → factor (5 categories)"
```

### 3. Verify Critical Variables

```r
# After import, double-check key variables
result <- import_research_data("data.csv")

# Verify the manipulation check
table(result$data$manipulation_check)

# Verify exclusion criteria
result$data %>%
  filter(attention_check == "Failed") %>%
  nrow()
```

### 4. Handle Reverse-Coded Items

```r
# Import data first
result <- import_research_data("data.csv")
survey_data <- result$data

# Then reverse-code items
survey_data <- survey_data %>%
  mutate(
    # Reverse 7-point Likert scale
    satisfaction_2_reversed = 8 - satisfaction_2
  )

# Now use in reliability analysis
calculate_alpha(
  survey_data,
  items = c("satisfaction_1", "satisfaction_2_reversed", "satisfaction_3"),
  reverse_items = NULL  # Already reversed above
)
```

---

## Integration with Other consumeR Functions

The imported data works seamlessly with all other consumeR functions:

```r
# Step 1: Import
result <- import_research_data("study_data.csv")
data <- result$data

# Step 2: Descriptive stats
calculate_summary_stats(data$spending)

# Step 3: Group comparison
test_group_differences(
  data$spending[data$condition == "Treatment"],
  data$spending[data$condition == "Control"]
)

# Step 4: Reliability analysis
calculate_alpha(
  data,
  items = c("engage_1", "engage_2", "engage_3", "engage_4"),
  scale_name = "Employee Engagement"
)

# Step 5: Factor analysis
perform_efa(
  data,
  items = c("q1", "q2", "q3", "q4", "q5", "q6")
)

# Step 6: Complete report
create_analysis_report(
  data,
  variable = "spending",
  group_var = "condition",
  title = "Study 1 Results"
)
```

---

## Troubleshooting

### Q: Function says package 'haven' is required for SPSS files

**A:** Install it:
```r
install.packages("haven")
```

### Q: My dates aren't being recognized

**A:** Import first, then convert:
```r
result <- import_research_data("data.csv")
data <- result$data %>%
  mutate(date_collected = as.Date(date_collected, format = "%Y-%m-%d"))
```

### Q: I want to manually specify types during import

**A:** Import first, then modify:
```r
result <- import_research_data("data.csv", auto_convert = FALSE)
data <- result$data %>%
  mutate(
    gender = as.factor(gender),
    age_group = factor(age_group, levels = c("Young", "Middle", "Old"), ordered = TRUE)
  )
```

---

## What Makes This Gold Standard for Reproducibility

### 1. **Explicit Variable Mapping**
Every variable's type is documented:
```r
result$variable_summary
# Shows exactly what each variable is
```

### 2. **Complete Audit Trail**
```r
result$import_log
# Records when, what, and how data was imported
```

### 3. **Visual Documentation**
```r
result$validation_report
# Publication-ready plot of data quality
```

### 4. **Plain English Explanations**
```
suggestion_reason: "Text with 2 categories - convert to factor"
```
Reviewers understand exactly why changes were made!

### 5. **Tidyverse Principles**
All data returned as tibbles, all reports as ggplot2 objects - modern, readable R code.

---

## Summary

The new data import functions provide:

✅ **Easy import** from CSV and SPSS
✅ **Smart type detection** and conversion
✅ **Visual quality reports** with ggplot2
✅ **Complete documentation** for methods sections
✅ **Tidyverse integration** for modern workflows
✅ **Gold-standard reproducibility** with full audit trail

**Next**: Just import your data and you're ready to analyze!

```r
result <- import_research_data("my_data.csv")
my_data <- result$data
# Ready to go!
```
