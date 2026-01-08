# consumeR Examples Guide
## Real-World Retail Scenarios (Cloud 9 / Dunder Mifflin Style)

This guide shows how to use consumeR with fun, relatable examples inspired by Superstore and The Office.

---

## The Dataset: Cloud 9 Customer Survey

Our example dataset tracks 100 Cloud 9 customers during a promotional flyer campaign:

- **50 customers** received a flyer with special deals ("Got Flyer")
- **50 customers** shopped without seeing the flyer ("No Flyer")

We tracked:
- **Spending**: How much they bought ($)
- **Satisfaction**: How happy they were (1-10 scale)
- **Loyalty Score**: How likely they'll return (0-100)

The dataset includes familiar names like Amy Sosa, Jim Halpert, Dwight Schrute, and more!

---

## Example 1: "Did the Flyer Actually Work?"

**Scenario**: Glenn (the store manager) wants to know if those expensive promotional flyers were worth it.

```r
library(consumeR)

# Load the Cloud 9 customer data
data(consumer_survey)

# Look at the first few customers
head(consumer_survey)
#   customer_id customer_name flyer_group spending satisfaction loyalty_score
# 1           1     Amy Sosa   Got Flyer    73.52            9          78.3
# 2           2  Jonah Simms   Got Flyer    89.21            8          85.1
# 3           3 Garrett McNeill Got Flyer    45.67            7          62.4

# How much did customers spend on average?
all_spending_stats <- calculate_summary_stats(consumer_survey$spending)
print(all_spending_stats)
# $mean: 58.45 (average spending across all customers)
# $median: 56.20
# $sd: 25.30 (quite a range in spending!)
```

**What this tells us**: Customers spent an average of about $58, but there's a lot of variation (some bought bulk toilet paper, others just grabbed a snack).

---

## Example 2: "Comparing Flyer vs No Flyer Customers"

**Scenario**: Dina (assistant manager) wants statistical proof that the flyer increased sales.

```r
# Split spending by whether they got the flyer
flyer_customers <- consumer_survey$spending[consumer_survey$flyer_group == "Got Flyer"]
no_flyer_customers <- consumer_survey$spending[consumer_survey$flyer_group == "No Flyer"]

# Compare the two groups
flyer_test <- test_group_differences(flyer_customers, no_flyer_customers)

# See the plain English interpretation
cat(flyer_test$interpretation)
# "The groups are significantly different (p = 0.0234).
#  Group 1 has a higher mean (65.32) than Group 2 (51.58),
#  with a difference of 13.74."

# Check if it's statistically significant
print(flyer_test$significant)
# TRUE - Yes! The flyer made a real difference!

# What test was used?
print(flyer_test$test_used)
# "Wilcoxon rank-sum test" - automatically chosen because of sample size
```

**What this tells us**: Customers who got the flyer spent about $13.74 more on average. This is statistically significant, so Glenn can justify the flyer budget!

---

## Example 3: "Customer Satisfaction Analysis"

**Scenario**: Jonah wants to know if the promotional flyer made customers happier (or annoyed them with ads).

```r
# Look at satisfaction scores
satisfaction_stats <- calculate_summary_stats(consumer_survey$satisfaction)
print(satisfaction_stats)

# Overall satisfaction is pretty good (mean around 7.5/10)
# But did the flyer group feel differently?

flyer_satisfaction <- consumer_survey$satisfaction[consumer_survey$flyer_group == "Got Flyer"]
no_flyer_satisfaction <- consumer_survey$satisfaction[consumer_survey$flyer_group == "No Flyer"]

satisfaction_comparison <- test_group_differences(
  flyer_satisfaction,
  no_flyer_satisfaction,
  alternative = "greater"  # Testing if flyer group is MORE satisfied
)

cat(satisfaction_comparison$interpretation)
```

**What this tells us**: Did getting deals make customers happier? The test tells us!

---

## Example 4: "The Complete Report for Corporate"

**Scenario**: Cloud 9 corporate (Jeff and the suits) want a full analysis. Glenn needs a professional report.

```r
# Generate a comprehensive, publication-ready report
create_analysis_report(
  data = consumer_survey,
  variable = "spending",
  group_var = "flyer_group",
  title = "Cloud 9 Promotional Flyer Campaign Analysis - Q4 2025",
  report_file = "flyer_campaign_results.txt"
)
```

**Output** (saved to file):
```
==================================================
Cloud 9 Promotional Flyer Campaign Analysis - Q4 2025
==================================================
Generated on: 2026-01-08

SECTION 1: DATA OVERVIEW
--------------------------------------------------
Variable analyzed: spending
Total observations: 100
Valid observations: 99
Missing values: 1 (1.0%)

SECTION 2: DESCRIPTIVE STATISTICS
--------------------------------------------------
Sample size (n): 99
Central Tendency Measures:
  Mean: 58.45
  Median: 56.20

Variability Measures:
  Standard Deviation: 25.30
  ...

SECTION 3: GROUP COMPARISON
--------------------------------------------------
Grouping variable: flyer_group
Groups compared: Got Flyer vs No Flyer

Group 1 (Got Flyer):
  n = 50
  Mean = 65.32

Group 2 (No Flyer):
  n = 49
  Mean = 51.58

Statistical Test Results:
  Test used: Wilcoxon rank-sum test
  P-value: 0.0234
  Significant at alpha = 0.05: TRUE
  Mean difference: 13.74

Interpretation:
  The groups are significantly different (p = 0.0234).
  Group 1 has a higher mean (65.32) than Group 2 (51.58),
  with a difference of 13.74.

SECTION 4: METHODOLOGICAL NOTES
--------------------------------------------------
For complete transparency and reproducibility...
```

**What this tells us**: A complete, transparent report that corporate can review. Perfect for peer review!

---

## Example 5: "Loyalty Score - Will They Come Back?"

**Scenario**: Mateo is tracking which customers will return. He wants to know if the flyer created loyal shoppers.

```r
# Check loyalty scores for both groups
flyer_loyalty <- consumer_survey$loyalty_score[consumer_survey$flyer_group == "Got Flyer"]
no_flyer_loyalty <- consumer_survey$loyalty_score[consumer_survey$flyer_group == "No Flyer"]

# Compare loyalty
loyalty_test <- test_group_differences(flyer_loyalty, no_flyer_loyalty)

# Get detailed stats for flyer customers
flyer_loyalty_stats <- calculate_summary_stats(flyer_loyalty, include_all = TRUE)

cat("Flyer customer loyalty stats:\n")
cat("Average loyalty:", flyer_loyalty_stats$mean, "\n")
cat("Range:", flyer_loyalty_stats$min, "to", flyer_loyalty_stats$max, "\n")
cat("Standard deviation:", flyer_loyalty_stats$sd, "\n")
```

**What this tells us**: Are flyer customers more likely to become regular shoppers? The numbers reveal the answer!

---

## Example 6: "Who Spent the Most?"

**Scenario**: Cheyenne is curious about who the big spenders were.

```r
# Find the top spenders
consumer_survey_clean <- consumer_survey[!is.na(consumer_survey$spending), ]
top_spenders <- consumer_survey_clean[order(-consumer_survey_clean$spending), ][1:10, ]

cat("Top 10 Spenders at Cloud 9:\n")
print(top_spenders[, c("customer_name", "flyer_group", "spending")])

# Calculate stats for just the top spenders
top_spender_amounts <- top_spenders$spending
calculate_summary_stats(top_spender_amounts)
```

**What this tells us**: Who's buying bulk items? How much did they spend? Were they influenced by the flyer?

---

## Example 7: "Before and After - A Paired Test Example"

**Scenario**: Imagine we tracked the same customers' spending before and after the flyer campaign.

```r
# Simulated example: Same 20 customers, measured twice
before_flyer <- c(45, 52, 48, 51, 49, 53, 47, 50, 46, 54,
                 55, 49, 51, 48, 52, 50, 53, 47, 49, 51)

after_flyer <- c(52, 58, 54, 57, 55, 60, 53, 56, 52, 61,
                59, 55, 57, 54, 58, 56, 60, 53, 55, 57)

# Paired test (same people, measured twice)
campaign_effect <- test_group_differences(
  before_flyer,
  after_flyer,
  paired = TRUE,
  alternative = "less"  # Testing if AFTER is greater
)

cat(campaign_effect$interpretation)
```

**What this tells us**: Did individual customers change their behavior? Paired tests account for individual differences.

---

## Example 8: "Missing Data - Handling Survey Non-Response"

**Scenario**: Some customers didn't complete the survey. The package handles this automatically.

```r
# The dataset has some missing values (just like real surveys)
cat("Missing data summary:\n")
print(colSums(is.na(consumer_survey)))

# When you run analyses, missing values are automatically handled
# and the function tells you exactly what was excluded
spending_analysis <- calculate_summary_stats(consumer_survey$spending)
# Note: 1 missing value(s) removed from calculations.

# This transparency is crucial for peer review!
```

**What this tells us**: Real data has missing values. consumeR handles them transparently and tells you exactly what happened.

---

## Example 9: "Quick Stats for a Meeting"

**Scenario**: You have 2 minutes before a meeting with corporate. Quick stats needed!

```r
library(consumeR)
data(consumer_survey)

# One-liner for quick stats
stats <- calculate_summary_stats(consumer_survey$spending)

# Present in meeting:
cat(sprintf("Average customer spending: $%.2f\n", stats$mean))
cat(sprintf("Typical range: $%.2f to $%.2f\n", stats$q25, stats$q75))
cat(sprintf("Some customers spent as much as: $%.2f\n", stats$max))
```

**What this tells us**: Fast, accurate numbers for quick decisions.

---

## Example 10: "A/B Testing - Web vs In-Store Flyers"

**Scenario**: What if Cloud 9 tested digital flyers (email) vs physical flyers (in-store)?

```r
# Modify the dataset for this example
consumer_survey$flyer_type <- ifelse(
  consumer_survey$flyer_group == "Got Flyer",
  sample(c("Digital", "Physical"), sum(consumer_survey$flyer_group == "Got Flyer"),
         replace = TRUE),
  "None"
)

# Compare digital vs physical flyers
digital_spending <- consumer_survey$spending[consumer_survey$flyer_type == "Digital"]
physical_spending <- consumer_survey$spending[consumer_survey$flyer_type == "Physical"]

flyer_type_test <- test_group_differences(digital_spending, physical_spending)
cat(flyer_type_test$interpretation)
```

**What this tells us**: Which flyer type is more effective? Data-driven decision making!

---

## Why These Examples Work for Teaching

1. **Relatable Context**: Everyone knows retail stores and can imagine these scenarios
2. **Familiar Characters**: Fans of Superstore/The Office connect with the names
3. **Real Decisions**: These are actual business questions retail managers face
4. **Clear Outcomes**: Easy to understand what the numbers mean
5. **Transparent Process**: Every step is explained in plain English

---

## Tips for Creating Your Own Examples

When using consumeR with your own data:

```r
# 1. Load your data
my_data <- read.csv("my_consumer_study.csv")

# 2. Get quick overview
calculate_summary_stats(my_data$outcome_variable)

# 3. Compare groups if applicable
test_group_differences(group_a, group_b)

# 4. Generate full report for reviewers
create_analysis_report(
  data = my_data,
  variable = "outcome_variable",
  group_var = "treatment_group",
  title = "My Study Results",
  report_file = "analysis_for_reviewers.txt"
)
```

---

## Next Steps

- Try these examples yourself: `data(consumer_survey)`
- Read the full vignette: `vignette("getting-started")`
- Explore the source code: it's extensively commented!
- Adapt examples to your own retail/consumer research

---

**Remember**: Every function in consumeR is designed to be so transparent that even Cheyenne (from Superstore) could understand what's happening. And that's the point!
