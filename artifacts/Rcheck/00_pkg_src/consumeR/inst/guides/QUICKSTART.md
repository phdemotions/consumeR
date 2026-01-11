# Quick Start Guide for consumeR

This guide will get you up and running with the consumeR package in 5 minutes.

## Installation & Setup

### Option 1: If you're developing the package

1. Open R or RStudio
2. Set working directory to the package folder:
   ```r
   setwd("path/to/consumeR")
   ```

3. Run the setup script:
   ```r
   source("SETUP.R")
   ```

This will:
- Install required development packages
- Generate documentation
- Create example dataset
- Run package checks

### Option 2: If you just want to use the package

```r
# Install from the package directory
setwd("path/to/consumeR")
install.packages(".", repos = NULL, type = "source")

# Then load it
library(consumeR)
```

## Your First Analysis (2 minutes)

### Example 1: Summary Statistics

```r
library(consumeR)

# Your data
customer_spending <- c(45.20, 67.80, 23.40, 89.10, 34.50,
                      56.70, 78.90, 12.30, 91.20, 43.50)

# Get statistics
stats <- calculate_summary_stats(customer_spending)
print(stats)
```

**Output:**
```
$n
[1] 10

$mean
[1] 54.26

$median
[1] 51.1

$sd
[1] 25.97
...
```

### Example 2: Compare Two Groups

```r
# Treatment vs Control
treatment <- c(65, 72, 68, 71, 69, 70, 73, 67, 71, 69)
control <- c(58, 62, 60, 59, 61, 63, 59, 62, 60, 61)

# Test for difference
result <- test_group_differences(treatment, control)

# See the interpretation
cat(result$interpretation)
```

**Output:**
```
The groups are significantly different (p = 0.0001).
Group 1 has a higher mean (69.50) than Group 2 (60.50),
with a difference of 9.00.
```

### Example 3: Full Report for Reviewers

```r
# Using the built-in example dataset
consumer_survey

# Generate complete analysis report
create_analysis_report(
  data = consumer_survey,
  variable = "spending",
  group_var = "group",
  title = "Consumer Spending Analysis"
)
```

This prints a comprehensive report including:
- Data overview
- Descriptive statistics
- Group comparison results
- Methodological notes

## Key Features in 30 Seconds

1. **Transparent Code**: Every function has extensive comments
   ```r
   # View the source code
   calculate_summary_stats
   ```

2. **Plain English Results**: No cryptic statistical output
   ```r
   result$interpretation  # Human-readable explanation
   ```

3. **Automatic Missing Value Handling**: Just works
   ```r
   data_with_nas <- c(1, 2, NA, 4, 5)
   calculate_summary_stats(data_with_nas)
   # Automatically removes NAs and tells you
   ```

4. **Smart Test Selection**: Chooses the right test automatically
   ```r
   test_group_differences(group1, group2, test_type = "auto")
   ```

## Common Use Cases

### Use Case 1: Simple Descriptive Stats for Paper

```r
satisfaction_scores <- c(7, 8, 9, 6, 8, 7, 9, 8, 7, 6, 9, 8)
stats <- calculate_summary_stats(satisfaction_scores)

# For your paper:
cat(sprintf("Mean satisfaction was %.2f (SD = %.2f, range = %d-%d)",
            stats$mean, stats$sd, stats$min, stats$max))
```

### Use Case 2: A/B Test Analysis

```r
# Your A/B test data
version_a <- c(0.12, 0.15, 0.11, 0.14, 0.13)
version_b <- c(0.18, 0.19, 0.17, 0.20, 0.18)

# Test and report
result <- test_group_differences(version_a, version_b)
cat("P-value:", result$p_value, "\n")
cat("Significant:", result$significant, "\n")
cat(result$interpretation)
```

### Use Case 3: Complete Analysis for Reviewers

```r
# Your study data
study_data <- data.frame(
  outcome = c(group1_values, group2_values),
  condition = c(rep("Treatment", length(group1_values)),
               rep("Control", length(group2_values)))
)

# Generate report and save to file
create_analysis_report(
  data = study_data,
  variable = "outcome",
  group_var = "condition",
  title = "Study 1: Treatment Effect Analysis",
  report_file = "study1_analysis.txt"
)
```

## Getting Help

### Function Documentation
```r
?calculate_summary_stats
?test_group_differences
?create_analysis_report
```

### Comprehensive Tutorial
```r
vignette("getting-started", package = "consumeR")
```

### View Source Code
All functions have extensive inline comments:
```r
# Just type the function name without parentheses
calculate_summary_stats
```

## Next Steps

1. **Try the examples** - Run the code above with your own data
2. **Read the vignette** - `vignette("getting-started")`
3. **Check the README** - More examples and philosophy
4. **Explore the code** - See how transparent it is!

## For Package Development

If you're contributing to the package:

1. **Read DEVELOPMENT.md** - Complete development guide
2. **Run tests**: `devtools::test()`
3. **Check package**: `devtools::check()`
4. **Build**: `devtools::build()`

## Tips for Reviewers

If you're reviewing a paper that uses this package:

1. All code is transparent - view any function's source
2. Results are reproducible - run the same code
3. Methods are documented - check inline comments
4. Assumptions are stated - read function documentation

## Troubleshooting

### Error: "Package not found"
```r
# Make sure you're in the right directory
setwd("path/to/consumeR")
# Then install
install.packages(".", repos = NULL, type = "source")
```

### Error: "Function not found"
```r
# Load the library first
library(consumeR)
```

### Need to rebuild documentation?
```r
library(devtools)
document()
```

## Summary

consumeR makes transparent analysis easy:
- ✅ Clear, readable code
- ✅ Plain English results
- ✅ Automatic best practices
- ✅ Perfect for peer review

**Time to first analysis: < 5 minutes**

Start with `calculate_summary_stats()` and go from there!
