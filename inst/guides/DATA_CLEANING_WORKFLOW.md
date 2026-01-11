# Data Cleaning Workflow for Journal of Consumer Psychology

## Overview

**THIS IS STEP 1** in your research workflow. Before running any statistical analyses, you must properly clean your data and document all exclusions.

The Journal of Consumer Psychology (and all major journals) requires complete transparency about:
- How many participants you initially collected
- Who was excluded and why
- Your final analytical sample size
- The percentage of participants retained

The `consumeR` package makes this process explicit, transparent, and publication-ready.

---

## Why This Matters

### From JCP Editorial Policies:

> "Authors must report how they determined their sample size, all data exclusions (if any), all manipulations, and all measures in the study." - JCP Author Guidelines

> "For data cleaning, authors should explicitly state the criteria for excluding participants and report the number of participants excluded at each step." - Transparency and Openness Promotion (TOP) Guidelines

### Common Manuscript Rejections:

❌ "The authors did not report how many participants were excluded for attention check failures."

❌ "It's unclear whether pre-test data was included in the final analysis."

❌ "The exclusion criteria were applied post-hoc and are not justified."

✅ With `consumeR`, you get publication-ready text that addresses all of these concerns.

---

## The Complete Workflow

### Step 1: Import Your Raw Data

```r
library(consumeR)

# Import your survey data
raw_data <- read.csv("qualtrics_export.csv")

# Or use the package's import function
raw_data <- import_research_data("qualtrics_export.csv")$data
```

### Step 2: Clean and Document Exclusions

```r
# Define your cleaning criteria
cleaning_result <- clean_survey_data(
  data = raw_data,

  # Pre-test cases (pilot data, researcher tests)
  pretest_var = "is_pretest",
  pretest_values = c(1, TRUE, "test"),

  # Inclusion criteria (must meet ALL)
  inclusion_criteria = list(
    completed = raw_data$Finished == 1,          # Completed survey
    adult = raw_data$age >= 18,                   # Age 18+
    us_resident = raw_data$country == "US",       # US only
    consent = raw_data$consent == "I agree"       # Provided consent
  ),

  # Attention checks
  attention_checks = list(
    ac1 = list(var = "attention_check_1", correct = 4),
    ac2 = list(var = "attention_check_2", correct = "strongly disagree"),
    ac3 = list(var = "attention_check_3", correct = 2)
  ),
  attention_check_rule = "all",  # Must pass ALL checks

  # Additional exclusions (optional)
  additional_exclusions = duplicated(raw_data$IPAddress),
  additional_exclusion_reason = "Duplicate IP addresses",

  # ID variable for tracking
  id_var = "ResponseId"
)
```

### Step 3: Review the Results

```r
# View summary
print(cleaning_result)

# View publication text
print(cleaning_result, show_publication = TRUE)

# View detailed flow
print(cleaning_result, show_flow = TRUE, show_details = TRUE)
```

### Step 4: Extract Clean Data

```r
# Get your clean dataset
clean_data <- cleaning_result$clean_data

# Now proceed with your analyses using the clean data
result <- test_group_differences(
  clean_data$satisfaction[clean_data$condition == "Treatment"],
  clean_data$satisfaction[clean_data$condition == "Control"]
)
```

---

## What You Get

### 1. Exclusion Summary Table

Perfect for a table in your manuscript:

| Exclusion Type | N Excluded | Remaining N | % of Initial |
|----------------|------------|-------------|--------------|
| Pre-test cases | 15 | 485 | 3.0% |
| Failed inclusion criteria | 23 | 462 | 4.6% |
| Failed attention checks | 28 | 434 | 5.6% |
| Duplicate IP addresses | 2 | 432 | 0.4% |
| **FINAL ANALYTICAL SAMPLE** | - | **432** | **86.4%** |

Access with: `cleaning_result$exclusion_summary`

### 2. Publication-Ready Text

**Concise version** (for tight word limits):

> "Data were collected from 500 participants. We excluded 15 pre-test cases that were collected during pilot testing. We then excluded participants who did not meet our inclusion criteria: 8 participants who failed the 'completed' criterion, 7 participants who failed the 'adult' criterion, 5 participants who failed the 'us_resident' criterion, 3 participants who failed the 'consent' criterion. We excluded 28 participants who failed all 3 attention checks. Additionally, we excluded 2 participants due to duplicate IP addresses. The final analytical sample consisted of 432 participants (86.4% of the initial sample)."

**Verbose version** (with full details):

> "SAMPLE AND DATA CLEANING PROCEDURE
>
> Initial Sample: We initially collected data from 500 participants.
>
> Exclusion Criteria: Following best practices in consumer research (see Oppenheimer, Meyvis, & Davidenko, 2009), we applied systematic exclusion criteria to ensure data quality.
>
> Pre-test Exclusions: We first removed 15 cases that were collected during pre-testing and pilot data collection. These cases were used to validate survey flow and question wording but were not part of the main data collection effort.
>
> Inclusion Criteria: Participants had to meet all of the following criteria to be included in the analysis:
>   - completed (excluded 8 participants)
>   - adult (excluded 7 participants)
>   - us_resident (excluded 5 participants)
>   - consent (excluded 3 participants)
>
> Attention Checks: To ensure participants were attentive, we embedded 3 attention checks throughout the survey. These checks asked participants to select a specific response (e.g., 'Please select Strongly Agree for this item'). We excluded 28 participants who failed all attention checks.
>
> Additional Exclusions: We also excluded 2 participants due to duplicate IP addresses.
>
> Final Sample: After all exclusions, the final analytical sample consisted of 432 participants (86.4% retention rate from initial sample). This retention rate is excellent and is consistent with standards in consumer research."

Access with: `cleaning_result$publication_text$concise` or `$verbose`

### 3. CONSORT-Style Flow Diagram

```
PARTICIPANT FLOW (CONSORT Style):

Assessed for eligibility: n = 500
  ↓ Excluded (pre-test): n = 15
After pre-test exclusion: n = 485
  ↓ Excluded (inclusion criteria): n = 23
After inclusion screening: n = 462
  ↓ Excluded (attention checks): n = 28
After attention screening: n = 434
  ↓ Excluded (Duplicate IP addresses): n = 2
After additional exclusions: n = 432

═══════════════════════════════
FINAL ANALYTICAL SAMPLE: n = 432
═══════════════════════════════
```

Access with: `cleaning_result$publication_text$consort`

### 4. Individual Case Flags

Track which specific cases were excluded for which reasons:

```r
# See all flags
head(cleaning_result$flags)

# row_number    id  pretest  inclusion_fail  attention_fail  additional  exclusion_reason
#          1  R_1    FALSE           FALSE           FALSE       FALSE  <NA>
#          2  R_2     TRUE           FALSE           FALSE       FALSE  Pre-test case
#          3  R_3    FALSE            TRUE           FALSE       FALSE  Failed: completed
#          4  R_4    FALSE           FALSE            TRUE       FALSE  Failed attention checks (1/3 passed)
#          5  R_5    FALSE           FALSE           FALSE        TRUE  Duplicate IP addresses

# Export for supplementary materials
write.csv(cleaning_result$flags, "exclusion_flags.csv", row.names = FALSE)
```

---

## Common Exclusion Scenarios

### Scenario 1: Basic Survey with Attention Checks

```r
result <- clean_survey_data(
  data = raw_data,
  inclusion_criteria = list(
    completed = raw_data$Finished == 1
  ),
  attention_checks = list(
    ac1 = list(var = "AC_1", correct = 3),
    ac2 = list(var = "AC_2", correct = "disagree")
  ),
  attention_check_rule = "all"
)
```

### Scenario 2: MTurk Study with Quality Filters

```r
result <- clean_survey_data(
  data = raw_data,
  pretest_var = "preview",
  pretest_values = TRUE,
  inclusion_criteria = list(
    completed = raw_data$Finished == 1,
    approval_rate = raw_data$mturk_approval >= 95,
    min_hits = raw_data$mturk_hits >= 100,
    us_only = raw_data$location == "US"
  ),
  attention_checks = list(
    ac1 = list(var = "attn_1", correct = 4),
    ac2 = list(var = "attn_2", correct = "strongly agree")
  ),
  additional_exclusions = raw_data$duration < 120,  # Too fast (< 2 min)
  additional_exclusion_reason = "Completed survey too quickly (< 2 minutes)"
)
```

### Scenario 3: Longitudinal Study (Time 1)

```r
result <- clean_survey_data(
  data = t1_data,
  pretest_var = "test_run",
  inclusion_criteria = list(
    completed = t1_data$Finished == 1,
    adult = t1_data$age >= 18,
    consent = t1_data$consent == 1
  ),
  attention_checks = list(
    ac1 = list(var = "attention_1", correct = 2)
  ),
  id_var = "participant_id"
)

# Save participant IDs for Time 2 recruitment
retained_ids <- result$clean_data$participant_id
write.csv(retained_ids, "t1_retained_ids.csv", row.names = FALSE)
```

### Scenario 4: Lenient Attention Check Rule

Sometimes you want to allow 1 failure out of 3 checks:

```r
result <- clean_survey_data(
  data = raw_data,
  attention_checks = list(
    ac1 = list(var = "AC_1", correct = 3),
    ac2 = list(var = "AC_2", correct = "agree"),
    ac3 = list(var = "AC_3", correct = 5)
  ),
  attention_check_rule = 2  # Must pass at least 2 out of 3
)

# Or require majority:
# attention_check_rule = "majority"
```

---

## Best Practices

### 1. **Pre-Register Your Exclusion Criteria**

Define your exclusion criteria **before** data collection:
- What makes someone a pre-test case?
- What are your inclusion criteria?
- How many attention checks will you use?
- What's your attention check rule (all vs. majority)?

### 2. **Be Conservative with Exclusions**

Only exclude participants for:
- ✅ Legitimate data quality reasons (inattention, didn't meet inclusion criteria)
- ✅ Pre-specified criteria
- ❌ NOT because their data "doesn't support your hypothesis"
- ❌ NOT for post-hoc reasons

### 3. **Document Everything**

The `consumeR` package does this for you, but manually note:
- Why you chose each inclusion criterion
- Why you set your attention check rule
- Any additional exclusions and their justification

### 4. **Report Retention Rates**

Most consumer research studies should retain **70-90%** of participants. If your retention is lower:
- ✅ Acknowledge it in your limitations
- ✅ Discuss potential selection bias
- ✅ Consider whether criteria were too strict

### 5. **Sensitivity Analyses**

If you excluded many participants, run sensitivity analyses:

```r
# Main analysis (strict criteria)
main_result <- clean_survey_data(
  data = raw_data,
  attention_check_rule = "all"  # Must pass all
)

# Sensitivity analysis (lenient criteria)
sensitivity_result <- clean_survey_data(
  data = raw_data,
  attention_check_rule = "majority"  # Just need >50%
)

# Compare results
cat("Main analysis: n =", main_result$n_final, "\n")
cat("Sensitivity analysis: n =", sensitivity_result$n_final, "\n")
```

---

## Integration with Statistical Analyses

After cleaning, use the clean data for all analyses:

```r
# Step 1: Clean data
cleaning <- clean_survey_data(
  data = raw_data,
  inclusion_criteria = list(completed = raw_data$Finished == 1),
  attention_checks = list(ac1 = list(var = "AC1", correct = 3)),
  id_var = "ResponseId"
)

# Extract clean data
df <- cleaning$clean_data

# Step 2: Descriptive statistics
desc <- calculate_summary_stats(df$satisfaction)

# Step 3: Group comparison
comp <- test_group_differences(
  df$satisfaction[df$condition == "Treatment"],
  df$satisfaction[df$condition == "Control"]
)
print(comp, show_publication = TRUE)

# Step 4: ANOVA
anova_result <- compare_groups_anova(
  df,
  formula = satisfaction ~ condition
)

# Step 5: Regression
reg <- analyze_regression(
  df,
  satisfaction ~ price + quality + service
)

# Step 6: Correlation
cor <- analyze_correlation(df, "satisfaction", "loyalty")
```

---

## Putting It All Together: Example Methods Section

Here's how you'd write your Methods section using all the output:

> **Participants and Procedure**
>
> Data were collected from 500 participants via Amazon Mechanical Turk. We excluded 15 pre-test cases that were collected during pilot testing. We then excluded participants who did not meet our inclusion criteria: 23 participants who did not complete the survey, 18 participants under age 18, and 12 participants who failed one or more of our three attention checks (e.g., "Please select 'Strongly Agree' for this item"). The final analytical sample consisted of 432 participants (86.4% of the initial sample; Mage = 34.2, SD = 11.5; 52% female).
>
> Participants were randomly assigned to one of two conditions: treatment (n = 217) or control (n = 215). [Continue with procedure description...]
>
> **Measures**
>
> *Customer Satisfaction.* [...]
>
> **Data Analysis**
>
> Data normality was assessed using the Shapiro-Wilk test. Both groups met the normality assumption (Treatment: W = 0.97, p = .23; Control: W = 0.98, p = .41). Homogeneity of variance was assessed using Levene's test and was satisfied (F(1, 430) = 1.45, p = .23). An independent samples t-test was conducted to compare satisfaction between conditions.
>
> **Results**
>
> The independent samples t-test revealed a statistically significant difference between conditions (t(430) = 3.24, p = .001). The treatment condition (M = 5.72, SD = 1.14) reported higher satisfaction than the control condition (M = 5.21, SD = 1.09), 95% CI [0.20, 0.82], Cohen's d = 0.45, representing a small to medium effect size.

**Every sentence came from `consumeR` package output!**

---

## Troubleshooting

### Issue: Too many exclusions

**Problem:** Retention rate < 60%

**Solutions:**
- Review your inclusion criteria - are they too strict?
- Check attention checks - are they confusing or ambiguous?
- Consider pilot testing to refine measures
- Document in limitations and run sensitivity analyses

### Issue: No participants excluded

**Problem:** 100% retention rate

**Check:**
- Are your attention checks working? (test them manually)
- Do you have pre-test data mixed in?
- Review individual responses - are some clearly inattentive?

### Issue: Missing variable names

**Problem:** "Variable 'AC_1' not found in data"

**Solutions:**
```r
# Check your column names
names(raw_data)

# Column names from Qualtrics often have prefixes
# Look for: Q1_1, AC_1_1, etc.

# Update your cleaning code with correct names
attention_checks = list(
  ac1 = list(var = "Q23_1", correct = 3)  # Use actual column name
)
```

---

## Quick Reference

```r
# Minimal example (just attention checks)
result <- clean_survey_data(
  data = raw_data,
  attention_checks = list(
    ac1 = list(var = "AC1", correct = 3)
  )
)

# Full example (all features)
result <- clean_survey_data(
  data = raw_data,
  pretest_var = "preview",
  pretest_values = TRUE,
  inclusion_criteria = list(
    completed = raw_data$Finished == 1,
    adult = raw_data$age >= 18
  ),
  attention_checks = list(
    ac1 = list(var = "AC1", correct = 3),
    ac2 = list(var = "AC2", correct = "agree")
  ),
  attention_check_rule = "all",
  id_var = "ResponseId"
)

# Access outputs
clean_df <- result$clean_data
cat(result$publication_text$concise)
print(result$exclusion_summary)
```

---

## References

Key papers on data quality and exclusions:

- Oppenheimer, D. M., Meyvis, T., & Davidenko, N. (2009). Instructional manipulation checks: Detecting satisficing to increase statistical power. *Journal of Experimental Social Psychology, 45*(4), 867-872.

- Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in survey data. *Psychological Methods, 17*(3), 437.

- Simmons, J. P., Nelson, L. D., & Simonsohn, U. (2011). False-positive psychology: Undisclosed flexibility in data collection and analysis allows presenting anything as significant. *Psychological Science, 22*(11), 1359-1366.

- Peer, E., Brandimarte, L., Samat, S., & Acquisti, A. (2017). Beyond the Turk: Alternative platforms for crowdsourcing behavioral research. *Journal of Experimental Social Psychology, 70*, 153-163.

---

**Remember: Data cleaning is NOT optional. It's a critical part of transparent, reproducible research.**

The `consumeR` package makes it easy to do it right the first time, with complete documentation for peer review.
