# 🔬 Complete Research Workflow Guide

## From Data Import to Publication-Ready Analysis

This guide shows you the **logical, step-by-step workflow** for analyzing consumer research data using the consumeR package. Follow these steps in order for a complete, reproducible analysis.

---

## 📋 Table of Contents

1. [Step 1: Import & Validate Data](#step-1-import--validate-data)
2. [Step 2: Check & Clean Variables](#step-2-check--clean-variables)
3. [Step 3: Descriptive Statistics](#step-3-descriptive-statistics)
4. [Step 4: Group Comparisons](#step-4-group-comparisons)
5. [Step 5: Reliability Analysis](#step-5-reliability-analysis)
6. [Step 6: Factor Analysis](#step-6-factor-analysis)
7. [Step 7: Complete Reporting](#step-7-complete-reporting)
8. [Complete Example Workflow](#complete-example-workflow)

---

## Step 1: Import & Validate Data

**Goal**: Load your data and ensure it's properly formatted

**Function**: `import_research_data()`

### What Happens Here:
- ✅ Import CSV or SPSS (.sav) files
- ✅ **Automatically clean column names** (lowercase, no spaces, R-friendly)
- ✅ Detect variable types
- ✅ Get suggestions for type improvements
- ✅ Create data quality report
- ✅ Generate complete audit trail

### Basic Example:

```r
library(consumeR)
library(dplyr)

# Import your data
result <- import_research_data("study1_data.csv")

# Access cleaned data (column names automatically cleaned!)
study_data <- result$data

# View variable summary
View(result$variable_summary)

# Check data quality report
print(result$validation_report)

# Save quality report for supplementary materials
ggsave("supplementary/data_quality.png", result$validation_report)
```

### SPSS Files:

```r
# Import SPSS file - preserves all labels!
result <- import_research_data("qualtrics_export.sav")

# SPSS value labels become factors
# Variable labels are preserved
study_data <- result$data
```

**📖 Full Guide**: See [DATA_IMPORT_GUIDE.md](DATA_IMPORT_GUIDE.md)

---

## Step 2: Check & Clean Variables

**Goal**: Verify each variable is the correct type

**Function**: `check_variable_types()`

### What Happens Here:
- ✅ Review all variables interactively
- ✅ See example values for each
- ✅ Get suggestions for improvements
- ✅ Check missing data patterns
- ✅ Verify unique values

### Example:

```r
# Review all variables
checked <- check_variable_types(study_data)

# Make any manual adjustments needed
study_data <- study_data %>%
  mutate(
    # Convert Likert scales to ordered factors
    satisfaction = factor(satisfaction,
                         levels = 1:7,
                         ordered = TRUE),

    # Create age groups if needed
    age_group = cut(age,
                   breaks = c(0, 25, 35, 50, 100),
                   labels = c("18-25", "26-35", "36-50", "50+"))
  )
```

**💡 Tip**: Column names are already cleaned from Step 1, so they'll be lowercase with underscores instead of spaces!

**📖 Full Guide**: See [DATA_IMPORT_GUIDE.md](DATA_IMPORT_GUIDE.md#function-2-check_variable_types)

---

## Step 3: Descriptive Statistics

**Goal**: Understand your data with basic summary statistics

**Function**: `calculate_summary_stats()`

### What You Get:
- Mean, median, standard deviation
- Min, max, range
- Quartiles (Q1, Q3)
- IQR, variance
- Sample size
- Missing data count

### Example:

```r
# Descriptive stats for spending
spending_stats <- calculate_summary_stats(study_data$spending)

# For your paper:
# M = 58.45, SD = 25.30, N = 200
print(spending_stats$mean)
print(spending_stats$sd)
print(spending_stats$n)

# Satisfaction ratings
satisfaction_stats <- calculate_summary_stats(study_data$satisfaction)
```

**Output Example:**
```
─────────────────────────────────────────────────
 TRANSPARENT SUMMARY STATISTICS
─────────────────────────────────────────────────
Variable: spending

Sample Size:
  N (valid): 200
  N (missing): 0

Central Tendency:
  Mean: 58.45
  Median: 55.00

Variability:
  SD: 25.30
  Variance: 640.09
  IQR: 35.50
  Range: 120.00
```

**📖 Full Guide**: See [QUICKSTART.md](QUICKSTART.md#1-descriptive-statistics)

---

## Step 4: Group Comparisons

**Goal**: Test if groups differ significantly

**Function**: `test_group_differences()`

### What You Get:
- Automatic test selection (t-test or Wilcoxon)
- Normality check
- Effect size (Cohen's d)
- Mean difference with confidence interval
- Plain English interpretation

### Example:

```r
# H1: Treatment group spends more than control
h1_test <- test_group_differences(
  study_data$spending[study_data$condition == "treatment"],
  study_data$spending[study_data$condition == "control"],
  alternative = "greater",
  group1_name = "Treatment",
  group2_name = "Control"
)

# For your paper
cat(h1_test$interpretation)

# Report: t(198) = 3.45, p < .001, d = 0.68
print(h1_test$test_statistic)
print(h1_test$p_value)
print(h1_test$effect_size)
```

**Output Example:**
```
INTERPRETATION: The Treatment group (M = 65.30) had
SIGNIFICANTLY HIGHER values than the Control group
(M = 51.60). This difference was statistically
significant (p = 0.001) with a MEDIUM effect size
(Cohen's d = 0.68).
```

**📖 Full Guide**: See [QUICKSTART.md](QUICKSTART.md#2-group-comparisons)

---

## Step 5: Reliability Analysis

**Goal**: Validate your measurement scales

**Functions**: `calculate_alpha()`, `calculate_composite_reliability()`

### What You Get:
- Cronbach's alpha with interpretation
- Item-total correlations
- Alpha-if-item-deleted
- Composite Reliability (CR)
- Average Variance Extracted (AVE)
- Quality assessment vs. thresholds

### Example - Cronbach's Alpha:

```r
# Check reliability of satisfaction scale
satisfaction_alpha <- calculate_alpha(
  data = study_data,
  items = c("sat_1", "sat_2", "sat_3", "sat_4"),
  scale_name = "Customer Satisfaction",
  item_labels = c(
    sat_1 = "Overall satisfaction (Likert 1-7, Q1)",
    sat_2 = "Would recommend (Likert 1-7, Q2)",
    sat_3 = "Meets expectations (Likert 1-7, Q3)",
    sat_4 = "Value for money (Likert 1-7, Q4)"
  )
)

# For your paper: α = 0.89
print(satisfaction_alpha$alpha)
```

### Example - Composite Reliability:

```r
# Advanced reliability measures
satisfaction_cr <- calculate_composite_reliability(
  data = study_data,
  items = c("sat_1", "sat_2", "sat_3", "sat_4"),
  scale_name = "Customer Satisfaction"
)

# For your paper: CR = 0.90, AVE = 0.68
print(satisfaction_cr$composite_reliability)
print(satisfaction_cr$ave)
```

**Output Example:**
```
INTERPRETATION: This scale demonstrates EXCELLENT reliability.
- Cronbach's α = 0.89 (exceeds 0.70 threshold)
- CR = 0.90 (exceeds 0.70 threshold)
- AVE = 0.68 (exceeds 0.50 threshold)

The construct explains 68% of item variance. This scale is
suitable for research and should be accepted by reviewers.
```

**📖 Full Guide**: See [NEW_FEATURES_SUMMARY.md](NEW_FEATURES_SUMMARY.md)

---

## Step 6: Factor Analysis

**Goal**: Explore underlying factor structure

**Function**: `perform_efa()`

### What You Get:
- Optimal number of factors (Kaiser criterion)
- Factor loadings for each item
- **Beautiful ggplot2 visualizations**:
  - Scree plot (publication-ready)
  - Loading heatmap with color gradients
- Variance explained by each factor
- Item-to-factor assignment recommendations

### Example:

```r
# Explore factor structure of 12 survey items
efa_results <- perform_efa(
  data = study_data,
  items = c("q_1", "q_2", "q_3", "q_4", "q_5", "q_6",
           "q_7", "q_8", "q_9", "q_10", "q_11", "q_12"),
  rotation = "varimax",
  create_plots = TRUE
)

# View results
print(efa_results)

# Save plots for paper
ggsave("figures/scree_plot.png", efa_results$scree_plot,
       width = 8, height = 6)
ggsave("figures/factor_loadings.png", efa_results$loading_plot,
       width = 10, height = 8)

# Get factor loadings for table
loadings_table <- efa_results$factor_loadings %>%
  filter(abs_loading > 0.40) %>%
  select(item, factor, loading)

View(loadings_table)
```

**Output Includes:**
- Scree plot showing eigenvalues
- Heatmap of factor loadings
- Tidy tibble of all loadings
- Variance explained breakdown

**📖 Full Guide**: See [NEW_FEATURES_SUMMARY.md](NEW_FEATURES_SUMMARY.md#2-exploratory-factor-analysis-efa)

---

## Step 7: Complete Reporting

**Goal**: Generate comprehensive analysis report

**Function**: `create_analysis_report()`

### What You Get:
- Complete descriptive statistics
- Group comparison results
- Visual summaries (boxplot, histogram)
- Statistical test results
- Plain English interpretations
- Everything needed for methods section

### Example:

```r
# Generate complete report
create_analysis_report(
  data = study_data,
  variable = "spending",
  group_var = "condition",
  title = "Study 1: Treatment Effect on Customer Spending",
  report_file = "analysis/study1_report.txt"
)

# Report is saved and also displayed in console
```

**Report Includes:**
- Study title and timestamp
- Sample sizes by group
- Descriptive statistics table
- Normality test results
- Group comparison test
- Effect size
- Interpretation

**📖 Full Guide**: See [EXAMPLES_GUIDE.md](EXAMPLES_GUIDE.md)

---

## Complete Example Workflow

Here's a full analysis from start to finish:

```r
library(consumeR)
library(dplyr)
library(ggplot2)

# ══════════════════════════════════════════════════════════
# STEP 1: IMPORT & VALIDATE DATA
# ══════════════════════════════════════════════════════════

result <- import_research_data(
  "qualtrics_export.sav",  # SPSS file from Qualtrics
  interactive = TRUE,
  clean_names = TRUE,      # Automatically clean column names!
  create_report = TRUE
)

# Access cleaned data (column names are now R-friendly!)
study_data <- result$data

# Save quality report for supplementary materials
ggsave("supplements/data_quality.png", result$validation_report,
       width = 10, height = 8)

# Document import process for methods section
saveRDS(result$import_log, "analysis/import_log.rds")


# ══════════════════════════════════════════════════════════
# STEP 2: CHECK & VERIFY VARIABLES
# ══════════════════════════════════════════════════════════

# Review variables
checked <- check_variable_types(study_data)

# Make manual adjustments if needed
study_data <- study_data %>%
  mutate(
    satisfaction = factor(satisfaction, levels = 1:7, ordered = TRUE)
  )


# ══════════════════════════════════════════════════════════
# STEP 3: DESCRIPTIVE STATISTICS
# ══════════════════════════════════════════════════════════

# Spending descriptives
spending_stats <- calculate_summary_stats(study_data$spending)

# For paper: M = 58.45, SD = 25.30, N = 200
print(spending_stats$mean)
print(spending_stats$sd)


# ══════════════════════════════════════════════════════════
# STEP 4: GROUP COMPARISONS (H1)
# ══════════════════════════════════════════════════════════

# H1: Treatment group spends more than control
h1_test <- test_group_differences(
  study_data$spending[study_data$condition == "treatment"],
  study_data$spending[study_data$condition == "control"],
  alternative = "greater",
  group1_name = "Treatment",
  group2_name = "Control"
)

# For paper: t(198) = 3.45, p < .001, d = 0.68
cat(h1_test$interpretation)


# ══════════════════════════════════════════════════════════
# STEP 5: RELIABILITY ANALYSIS
# ══════════════════════════════════════════════════════════

# Check Cronbach's alpha for satisfaction scale
satisfaction_alpha <- calculate_alpha(
  data = study_data,
  items = c("sat_1", "sat_2", "sat_3", "sat_4"),
  scale_name = "Customer Satisfaction",
  item_labels = c(
    sat_1 = "Overall satisfaction",
    sat_2 = "Would recommend",
    sat_3 = "Meets expectations",
    sat_4 = "Value for money"
  )
)

# Check composite reliability
satisfaction_cr <- calculate_composite_reliability(
  data = study_data,
  items = c("sat_1", "sat_2", "sat_3", "sat_4"),
  scale_name = "Customer Satisfaction"
)

# For paper: α = 0.89, CR = 0.90, AVE = 0.68
print(satisfaction_alpha$alpha)
print(satisfaction_cr$composite_reliability)
print(satisfaction_cr$ave)


# ══════════════════════════════════════════════════════════
# STEP 6: FACTOR ANALYSIS (EXPLORATORY)
# ══════════════════════════════════════════════════════════

# Explore underlying factor structure
efa_results <- perform_efa(
  data = study_data,
  items = c("q_1", "q_2", "q_3", "q_4", "q_5", "q_6",
           "q_7", "q_8", "q_9", "q_10", "q_11", "q_12"),
  rotation = "varimax",
  create_plots = TRUE
)

# Save plots for paper
ggsave("figures/scree_plot.png", efa_results$scree_plot,
       width = 8, height = 6)
ggsave("figures/factor_loadings.png", efa_results$loading_plot,
       width = 10, height = 8)

# Get factor loadings for table
loadings_table <- efa_results$factor_loadings %>%
  filter(abs_loading > 0.40) %>%
  select(item, factor, loading)


# ══════════════════════════════════════════════════════════
# STEP 7: COMPLETE ANALYSIS REPORT
# ══════════════════════════════════════════════════════════

create_analysis_report(
  data = study_data,
  variable = "spending",
  group_var = "condition",
  title = "Study 1: Treatment Effect on Spending",
  report_file = "analysis/study1_report.txt"
)

# ══════════════════════════════════════════════════════════
# DONE! Everything documented and ready for publication
# ══════════════════════════════════════════════════════════
```

---

## 📚 Related Documentation

### For Beginners:
1. **[README_FIRST.md](README_FIRST.md)** - Start here!
2. **[QUICKSTART.md](QUICKSTART.md)** - 5-minute introduction
3. **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - One-page cheat sheet

### Detailed Guides (By Analysis Step):
1. **[DATA_IMPORT_GUIDE.md](DATA_IMPORT_GUIDE.md)** - Data import & validation (Steps 1-2)
2. **[EXAMPLES_GUIDE.md](EXAMPLES_GUIDE.md)** - Descriptive stats & comparisons (Steps 3-4)
3. **[NEW_FEATURES_SUMMARY.md](NEW_FEATURES_SUMMARY.md)** - Reliability & factor analysis (Steps 5-6)

### Advanced Topics:
4. **[COMPLETE_FEATURES_UPDATE.md](COMPLETE_FEATURES_UPDATE.md)** - Complete feature overview
5. **[PACKAGE_OVERVIEW.md](PACKAGE_OVERVIEW.md)** - Technical package details
6. **[DEVELOPMENT.md](DEVELOPMENT.md)** - Package development guide

### GitHub & Installation:
7. **[GITHUB_SETUP.md](GITHUB_SETUP.md)** - Upload to GitHub
8. **[UPLOAD_TO_GITHUB.md](UPLOAD_TO_GITHUB.md)** - Simplified upload

---

## 💡 Key Features

### ✅ Automatic Name Cleaning
Column names are automatically cleaned using `janitor::clean_names()`:
- **Before**: "Customer ID", "Satisfaction Rating (1-7)", "Would Recommend?"
- **After**: `customer_id`, `satisfaction_rating_1_7`, `would_recommend`

All lowercase, no spaces, no special characters - R-friendly!

### ✅ Tidyverse Integration
All functions use tidyverse principles:
- Data as tibbles
- Pipeable workflows
- ggplot2 visualizations
- dplyr data manipulation

### ✅ Publication-Ready Output
- ggplot2 plots ready for journals
- Plain English interpretations
- Complete statistical reporting
- Reproducible audit trails

### ✅ Gold-Standard Reproducibility
- Explicit variable mapping
- Complete documentation
- Visual aids
- Transparent calculations

---

## 🎯 Tips for Success

### 1. Always Start with Data Import
Use `import_research_data()` even for CSV files - it catches issues early!

### 2. Save Everything
```r
# Save import log
saveRDS(result$import_log, "logs/import_log.rds")

# Save quality report
ggsave("reports/data_quality.png", result$validation_report)

# Save all plots
ggsave("figures/scree.png", efa_results$scree_plot)
```

### 3. Document as You Go
Each function creates documentation - use it in your methods section!

### 4. Check Reliability Early
Don't wait until peer review - check α, CR, and AVE during analysis.

### 5. Use Themed Examples
The package includes Cloud 9 themed data - perfect for learning!

```r
# Load example data
data("cloud9_customers")

# Try the workflow with example data first
result <- import_research_data(cloud9_customers)
```

---

## 📊 Summary

The consumeR package provides a **complete workflow** for consumer research:

1. **Import** → Automatic name cleaning + type detection
2. **Validate** → Variable checking + quality reports
3. **Describe** → Transparent summary statistics
4. **Compare** → Group differences with effect sizes
5. **Reliability** → α, CR, AVE with interpretations
6. **Factor Analysis** → EFA with beautiful visualizations
7. **Report** → Complete analysis documentation

**Everything uses tidyverse principles for maximum readability!**

---

**Need Help?**
- Quick questions: See [QUICK_REFERENCE.md](QUICK_REFERENCE.md)
- Just starting: See [QUICKSTART.md](QUICKSTART.md)
- Specific topics: See guides listed above
- Function help: `?function_name` (e.g., `?calculate_alpha`)

---

**Ready to start?**

```r
library(consumeR)
result <- import_research_data("your_data.csv")
# You're off! Follow the steps above 🚀
```
