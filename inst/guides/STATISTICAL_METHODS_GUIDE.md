# Complete Guide to Statistical Methods in consumeR

## Overview

The `consumeR` package now provides comprehensive statistical analysis functions with:

- **Explicit assumption checking** - Every test's assumptions are checked and reported
- **Publication-ready text** - Gold-standard text suitable for journals like Journal of Consumer Psychology
- **Detailed explanations** - Verbose output that helps novices understand what each test does
- **Effect sizes** - Appropriate effect size measures for every test
- **Complete transparency** - Full documentation of every step for peer review

## Table of Contents

1. [Comparing Two Groups](#comparing-two-groups)
2. [Comparing Multiple Groups (ANOVA)](#comparing-multiple-groups-anova)
3. [Linear Regression](#linear-regression)
4. [Correlation Analysis](#correlation-analysis)
5. [Understanding Assumptions](#understanding-assumptions)
6. [Using Publication Text](#using-publication-text)

---

## Comparing Two Groups

### Function: `test_group_differences()`

Compare means between two groups using t-tests or non-parametric alternatives.

**When to use:**
- You have one continuous outcome variable
- You have two groups to compare
- Examples: Treatment vs. Control, Before vs. After, Group A vs. Group B

### Basic Example

```r
library(consumeR)

# Create example data
treatment <- c(7.2, 8.1, 6.9, 7.8, 8.5, 7.3, 8.2, 7.6)
control <- c(5.4, 6.2, 5.8, 6.1, 5.9, 6.3, 5.7, 6.0)

# Run the test (automatic method selection with assumption checks)
result <- test_group_differences(treatment, control)

# View results
print(result)
```

### What You Get

**Standard Output:**
- Test used (Student's t-test, Welch's t-test, or Wilcoxon)
- Sample sizes
- Descriptive statistics (M, SD for each group)
- Test statistic, df, and p-value
- Effect size (Cohen's d) with interpretation
- Plain English interpretation

**Assumption Checks** (shown with `print(result, show_assumptions = TRUE)`):
- Normality for both groups (Shapiro-Wilk test)
- Homogeneity of variance (Levene's test)
- Independence (design-based assessment)
- For EACH assumption: interpretation, recommendation, and verbose explanation

**Publication Text** (shown with `print(result, show_publication = TRUE)`):
```
ASSUMPTIONS:
Data normality was assessed using the Shapiro-Wilk test. The Group 1
variable met the normality assumption (W = 0.9234, p = 0.4521). The
Group 2 variable met the normality assumption (W = 0.9455, p = 0.6213).
Homogeneity of variance was assessed using Levene's test. The assumption
was met (F(1, 14) = 1.234, p = 0.2851), indicating equal variances
across groups.

METHODS:
An independent samples t-test was conducted to compare means between
groups. The test assumes normality of distributions and homogeneity of
variance.

RESULTS:
The independent samples t-test revealed a statistically significant
difference between groups (t(14) = 3.45, p = .004). Group 1 (M = 7.70,
SD = 0.52) exceeded Group 2 (M = 5.93, SD = 0.31), 95% CI [0.62, 2.92],
Cohen's d = 4.15 (large effect size).

INTERPRETATION:
These results provide evidence of a statistically significant effect.
```

### Advanced Options

```r
# Force a specific test
result <- test_group_differences(group1, group2, test_type = "wilcoxon")

# One-sided test
result <- test_group_differences(group1, group2, alternative = "greater")

# Paired samples (e.g., before/after)
before <- c(65, 72, 68, 71, 69, 70)
after <- c(62, 68, 65, 69, 66, 67)
result <- test_group_differences(before, after, paired = TRUE)

# Disable assumption checking for speed
result <- test_group_differences(group1, group2, check_assumptions = FALSE)

# Quiet mode (no messages)
result <- test_group_differences(group1, group2, verbose = FALSE)
```

---

## Comparing Multiple Groups (ANOVA)

### Function: `compare_groups_anova()`

Compare means across three or more groups using Analysis of Variance.

**When to use:**
- You have one continuous outcome variable
- You have three or more groups to compare
- Examples: Comparing multiple treatments, conditions, or market segments

### Basic Example with Vector Input

```r
# Create example data
satisfaction <- c(5, 6, 7, 8, 7, 6,    # Condition A
                  3, 4, 5, 4, 3, 4,    # Condition B
                  7, 8, 9, 8, 9, 8)    # Condition C

condition <- factor(rep(c("A", "B", "C"), each = 6))

# Run ANOVA
result <- compare_groups_anova(satisfaction, condition)

# View results
print(result)
```

### Example with Data Frame and Formula

```r
# Data frame approach (recommended)
data <- data.frame(
  satisfaction = c(5, 6, 7, 8, 7, 6, 3, 4, 5, 4, 3, 4, 7, 8, 9, 8, 9, 8),
  condition = factor(rep(c("A", "B", "C"), each = 6))
)

result <- compare_groups_anova(data, formula = satisfaction ~ condition)

# View with post-hoc tests
print(result, show_posthoc = TRUE)
```

### What You Get

**Standard Output:**
- ANOVA type (standard or Welch's)
- Descriptive statistics for each group (n, mean, SD, min, max)
- F-statistic, degrees of freedom, p-value
- Effect size (η² - eta-squared) with interpretation
- Post-hoc test results (if significant)
- Plain English interpretation

**Post-Hoc Tests:**
- Automatically performed if overall ANOVA is significant
- Tukey HSD (default) or Bonferroni correction
- Shows which specific groups differ from each other
- Adjusted p-values to control for multiple comparisons

**Publication Text Example:**
```
ASSUMPTIONS:
The data were collected using a independent groups ANOVA design,
ensuring independence of observations. Data normality was assessed
using the Shapiro-Wilk test. The ANOVA residuals variable met the
normality assumption (W = 0.9521, p = 0.4234). Homogeneity of
variance was assessed using Levene's test. The assumption was met
(F(2, 15) = 1.123, p = 0.3521), indicating equal variances across
groups.

METHODS:
A one-way analysis of variance (ANOVA) was conducted to examine
differences across groups. ANOVA assumes normality of residuals,
homogeneity of variance across groups, and independence of
observations.

RESULTS:
The ANOVA revealed a statistically significant effect (F(2, 15) =
23.45, p < .001), η² = 0.758.

INTERPRETATION:
These results provide evidence of a statistically significant effect.

ADDITIONAL NOTES:
Post-hoc comparisons were conducted using Tukey's HSD test to
control for Type I error inflation.
```

### Advanced Options

```r
# Use Welch's ANOVA (robust to unequal variances)
result <- compare_groups_anova(data, formula = y ~ group, use_welch = TRUE)

# Use Bonferroni post-hoc tests
result <- compare_groups_anova(data, formula = y ~ group, post_hoc = "bonferroni")

# Skip post-hoc tests
result <- compare_groups_anova(data, formula = y ~ group, post_hoc = "none")

# Use different alpha level
result <- compare_groups_anova(data, formula = y ~ group, alpha = 0.01)
```

---

## Linear Regression

### Function: `analyze_regression()`

Examine linear relationships between variables, with single or multiple predictors.

**When to use:**
- You want to predict an outcome from one or more predictors
- You want to understand which variables influence an outcome
- Examples: Does ad spending predict sales? Do price, quality, and service predict satisfaction?

### Simple Regression (One Predictor)

```r
# Create example data
data <- data.frame(
  ad_spending = c(100, 200, 150, 300, 250, 400, 350, 500),
  sales = c(20, 35, 28, 48, 42, 65, 58, 80)
)

# Run regression
result <- analyze_regression(data, sales ~ ad_spending)

# View results
print(result)
```

### Multiple Regression (Multiple Predictors)

```r
# Create example data
data <- data.frame(
  satisfaction = c(7, 8, 6, 9, 5, 8, 7, 6, 9, 8),
  price = c(10, 8, 12, 7, 15, 9, 11, 13, 8, 10),
  quality = c(8, 9, 7, 9, 6, 8, 7, 6, 9, 8),
  service = c(7, 8, 6, 9, 5, 8, 7, 7, 9, 8)
)

# Run regression with multiple predictors
result <- analyze_regression(data, satisfaction ~ price + quality + service)

# View results
print(result)

# View with assumption checks
print(result, show_assumptions = TRUE)
```

### What You Get

**Standard Output:**
- Model type (Simple or Multiple regression)
- Number of observations and predictors
- R² and Adjusted R² with variance explained percentage
- Overall model F-test and significance
- RMSE (Root Mean Squared Error)
- Coefficients table with:
  - Estimate (b weight)
  - Standard error
  - t-value
  - p-value
  - Significance stars
- Plain English interpretation

**Assumption Checks:**
1. **Independence** - Design-based assessment
2. **Normality of residuals** - Shapiro-Wilk test
3. **Homoscedasticity** - Breusch-Pagan test for constant variance
4. **Multicollinearity** - VIF (Variance Inflation Factor) for multiple regression

Each assumption includes:
- Test result
- Interpretation
- Recommendation
- Publication-ready text
- Verbose explanation for learning

**Publication Text Example:**
```
ASSUMPTIONS:
The data were collected using a cross-sectional regression, ensuring
independence of observations. Data normality was assessed using the
Shapiro-Wilk test. The regression residuals variable met the normality
assumption (W = 0.9423, p = 0.5821). Homoscedasticity was assessed
using the Breusch-Pagan test. The assumption was met (χ² = 2.31, p =
0.1285), indicating constant error variance. Multicollinearity was
assessed using Variance Inflation Factors (VIF). All VIF values were
below 5, indicating acceptable multicollinearity.

METHODS:
Linear regression analysis was conducted to examine the relationship
between the predictor variable(s) and the outcome variable. Regression
assumes linearity, independence of residuals, homoscedasticity of
residuals, and normality of residuals.

ADDITIONAL NOTES:
The regression model included 3 predictor(s) and was based on 10
observations. RMSE = 0.823.
```

### Understanding the Coefficients

Each coefficient tells you:
- **Estimate**: How much the outcome changes for a 1-unit increase in the predictor
- **p-value**: Whether this predictor significantly predicts the outcome
- **Significance stars**: Quick visual indicator of significance level

Example interpretation:
> "For every $1 increase in ad spending, sales increased by $0.15 (b = 0.15, p < .001)."

### Advanced Options

```r
# Disable assumption checking
result <- analyze_regression(data, y ~ x, check_assumptions = FALSE)

# Disable plots
result <- analyze_regression(data, y ~ x, create_plots = FALSE)

# Use stricter alpha level
result <- analyze_regression(data, y ~ x, alpha = 0.01)

# Access the full lm() model object
lm_model <- result$lm_model
summary(lm_model)
plot(lm_model)  # Base R diagnostic plots
```

---

## Correlation Analysis

### Function: `analyze_correlation()`

Examine the strength and direction of linear relationships between two variables.

**When to use:**
- You want to know if two variables are related
- You want to measure the strength of a relationship
- Examples: Are customer satisfaction and loyalty related? Do price and quality correlate?

### Basic Example with Data Frame

```r
# Create example data
data <- data.frame(
  customer_satisfaction = c(7, 8, 6, 9, 5, 8, 7, 6, 9, 8),
  purchase_intention = c(8, 9, 7, 9, 6, 8, 7, 7, 9, 8)
)

# Calculate correlation
result <- analyze_correlation(data, "customer_satisfaction", "purchase_intention")

# View results
print(result)
```

### Example with Two Vectors

```r
x <- c(23, 45, 67, 34, 56, 78, 89, 12, 45, 67)
y <- c(34, 56, 78, 45, 67, 89, 90, 23, 56, 78)

result <- analyze_correlation(x, var2 = y)
```

### What You Get

**Standard Output:**
- Correlation method used (Pearson or Spearman)
- Correlation coefficient (r or ρ)
- 95% Confidence interval (for Pearson)
- p-value and significance
- r² (shared variance)
- Strength interpretation (negligible, weak, moderate, strong, very strong)
- Direction (positive, negative, near-zero)
- Plain English interpretation

**Assumption Checks:**
- Normality for both variables (for Pearson)
- Outlier detection
- Note about linearity (should check with scatterplot)

**Interpreting Correlation Strength:**
- |r| < 0.10: Negligible
- |r| 0.10 - 0.30: Weak
- |r| 0.30 - 0.50: Moderate
- |r| 0.50 - 0.70: Strong
- |r| 0.70 - 1.00: Very strong

**Publication Text Example:**
```
ASSUMPTIONS:
Data normality was assessed using the Shapiro-Wilk test. The
customer_satisfaction variable met the normality assumption (W =
0.9234, p = 0.3821). The purchase_intention variable met the
normality assumption (W = 0.9455, p = 0.5213). No major outliers
detected (|z| > 3 criterion).

METHODS:
Pearson's correlation coefficient was calculated to assess the linear
relationship between variables. This test assumes bivariate normality.

ADDITIONAL NOTES:
Correlation strength was interpreted using Cohen's (1988) guidelines.
The correlation coefficient of 0.78 indicates that the variables share
approximately 60.8% common variance (r² = 0.608).
```

### Choosing Pearson vs. Spearman

**Pearson correlation** (default when assumptions met):
- Assumes linear relationship
- Assumes both variables are normally distributed
- Most powerful when assumptions are met
- Use for interval/ratio data

**Spearman correlation** (automatically used when assumptions violated):
- Non-parametric (doesn't assume normality)
- Measures monotonic relationships (not just linear)
- Robust to outliers
- Use for ordinal data or when normality violated

### Advanced Options

```r
# Force Spearman correlation
result <- analyze_correlation(data, "var1", "var2", method = "spearman")

# Force Pearson correlation
result <- analyze_correlation(data, "var1", "var2", method = "pearson")

# Disable assumption checking
result <- analyze_correlation(data, "var1", "var2", check_assumptions = FALSE)

# Use different alpha level
result <- analyze_correlation(data, "var1", "var2", alpha = 0.01)
```

### Creating Scatterplots

Always visualize correlations!

```r
# Basic scatterplot
plot(data$customer_satisfaction, data$purchase_intention,
     xlab = "Customer Satisfaction",
     ylab = "Purchase Intention",
     main = "Correlation between Satisfaction and Intention")

# Add regression line
abline(lm(data$purchase_intention ~ data$customer_satisfaction), col = "red")

# With ggplot2
library(ggplot2)
ggplot(data, aes(x = customer_satisfaction, y = purchase_intention)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Customer Satisfaction and Purchase Intention",
       x = "Customer Satisfaction",
       y = "Purchase Intention")
```

---

## Understanding Assumptions

Every statistical test makes assumptions. This package explicitly checks these assumptions and provides detailed guidance.

### Normality

**What it means:** Data (or residuals) follow a bell-shaped distribution

**How it's tested:** Shapiro-Wilk test

**What you get:**
- Test statistic (W) and p-value
- Interpretation: Met or violated
- Recommendation: What to do if violated
- Verbose explanation: What normality means, why it matters, and alternatives

**If violated:**
- Use non-parametric tests (Wilcoxon, Spearman, Kruskal-Wallis)
- Transform data (log, square root)
- With large samples (n > 30), parametric tests are often robust

### Homogeneity of Variance (Homoscedasticity)

**What it means:** Groups have similar variability (spread)

**How it's tested:** Levene's test (for ANOVA/t-tests) or Breusch-Pagan test (for regression)

**If violated:**
- Use Welch's t-test or Welch's ANOVA (automatic in this package)
- Use robust standard errors for regression
- Transform data

### Independence

**What it means:** Each observation is independent; no clustering or repeated measures

**How it's assessed:** Based on research design (not testable statistically)

**If violated:**
- Use repeated measures ANOVA
- Use paired t-tests
- Use mixed effects models

### Multicollinearity (Regression)

**What it means:** Predictor variables are highly correlated with each other

**How it's tested:** Variance Inflation Factor (VIF)

**Threshold:** VIF < 5 is acceptable

**If violated:**
- Remove one of the correlated predictors
- Combine correlated predictors
- Use ridge regression

---

## Using Publication Text

Every function generates publication-ready text suitable for top-tier journals.

### How to Access

```r
# Run your analysis
result <- test_group_differences(group1, group2)

# Show publication text
print(result, show_publication = TRUE)
```

### What You Get

**Four sections:**

1. **ASSUMPTIONS:** Text describing which assumptions were tested and results
2. **METHODS:** Description of the statistical method used
3. **RESULTS:** Statistical results with test statistics, p-values, effect sizes
4. **INTERPRETATION:** Brief interpretation of findings

### Using in Your Manuscript

**Copy and paste** the sections into your manuscript. Then:

1. **Integrate with your narrative:** Weave the sentences into your writing
2. **Select appropriate detail:** Choose the level of detail your journal requires
3. **Customize as needed:** Modify to match your specific research context
4. **Add context:** Include your research question and theoretical framework

### Example Integration

**Methods Section:**
> "Customer satisfaction was measured using a 7-point Likert scale. To compare satisfaction between the treatment and control groups, an independent samples t-test was conducted. Data normality was assessed using the Shapiro-Wilk test. Both groups met the normality assumption (Treatment: W = 0.92, p = .45; Control: W = 0.95, p = .62). Homogeneity of variance was assessed using Levene's test and was satisfied (F(1, 14) = 1.23, p = .29)."

**Results Section:**
> "The independent samples t-test revealed a statistically significant difference between groups (t(14) = 3.45, p = .004). The treatment group (M = 7.70, SD = 0.52) reported higher satisfaction than the control group (M = 5.93, SD = 0.31), 95% CI [0.62, 2.92], Cohen's d = 4.15, representing a large effect size."

---

## Complete Workflow Example

Here's a complete analysis workflow for a consumer research study:

```r
library(consumeR)

# 1. Load your data
data <- read.csv("customer_survey.csv")

# 2. Descriptive statistics
desc_stats <- calculate_summary_stats(data$satisfaction)
print(desc_stats)

# 3. Compare groups
comparison <- test_group_differences(
  data$satisfaction[data$condition == "Treatment"],
  data$satisfaction[data$condition == "Control"]
)
print(comparison, show_publication = TRUE)

# 4. Multiple group comparison
anova_result <- compare_groups_anova(
  data,
  formula = satisfaction ~ segment
)
print(anova_result, show_assumptions = TRUE)

# 5. Regression analysis
regression <- analyze_regression(
  data,
  satisfaction ~ price + quality + service
)
print(regression, show_publication = TRUE)

# 6. Correlation analysis
correlation <- analyze_correlation(
  data,
  "satisfaction",
  "loyalty"
)
print(correlation)

# 7. Create scatterplot
plot(data$satisfaction, data$loyalty)
abline(lm(data$loyalty ~ data$satisfaction), col = "red")
```

---

## Tips for Novice Users

1. **Always check assumptions first:** Use `show_assumptions = TRUE` to understand if your test is appropriate

2. **Read the verbose explanations:** They're designed to teach you what each test does and when to use it

3. **Use automatic method selection:** Set `method = "auto"` (the default) to let the package choose the right test

4. **Start simple:** Begin with t-tests and correlations before moving to ANOVA and regression

5. **Visualize your data:** Always create plots to understand your data before running tests

6. **Check sample size:** Small samples may not have enough power to detect effects

7. **Don't p-hack:** Running many tests increases false positives. Plan your analyses in advance

8. **Report effect sizes:** Statistical significance ≠ practical significance. Effect sizes matter!

9. **Save your publication text:** Copy it immediately - you'll need it for your paper

10. **Understand what tests answer:**
    - t-test: "Are two group means different?"
    - ANOVA: "Are multiple group means different?"
    - Regression: "Does X predict Y?"
    - Correlation: "Are X and Y related?"

---

## Quick Reference Table

| Research Question | Function | Example |
|------------------|----------|---------|
| Compare 2 groups | `test_group_differences()` | Treatment vs. Control |
| Compare 3+ groups | `compare_groups_anova()` | Three market segments |
| Predict outcome | `analyze_regression()` | Price, quality → satisfaction |
| Measure relationship | `analyze_correlation()` | Satisfaction & loyalty |
| Check normality | `check_normality()` | Is my data normal? |
| Check equal variances | `check_homogeneity_of_variance()` | Do groups have equal variance? |

---

## Getting Help

```r
# Function documentation
?test_group_differences
?compare_groups_anova
?analyze_regression
?analyze_correlation

# Package vignettes
browseVignettes("consumeR")

# Example datasets
data(consumer_survey)
?consumer_survey
```

---

## Citation

When using this package, please cite:

```
R package consumeR: Statistical methods for consumer research with explicit
assumptions and publication-ready output.
```

For specific methods, cite the original papers:

- **Cohen's d:** Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.).
- **Eta-squared:** Cohen, J. (1973). Eta-squared and partial eta-squared in fixed factor ANOVA designs.
- **VIF:** O'Brien, R. M. (2007). A caution regarding rules of thumb for variance inflation factors.

---

## Updates in This Version

### New Statistical Functions

✅ **Enhanced t-tests** with automatic Welch's correction
✅ **One-way ANOVA** with Welch's alternative and post-hoc tests
✅ **Linear regression** with multicollinearity checks
✅ **Correlation analysis** with automatic method selection

### New Features

✅ **Explicit assumption checking** for all tests
✅ **Publication-ready text** following APA 7th guidelines
✅ **Effect sizes** for all analyses
✅ **Verbose explanations** for educational purposes
✅ **Custom print methods** for clean output

### What Makes This Different

Most R packages give you test statistics. This package gives you:

1. **Complete transparency:** See exactly what assumptions were checked
2. **Educational content:** Learn what each test does and why
3. **Publication-ready output:** Copy-paste into your manuscript
4. **Intelligent defaults:** Automatic method selection based on assumption tests
5. **Novice-friendly:** Written for researchers who aren't statisticians

---

**Questions? Issues? Suggestions?**

This package is designed to make your research easier and more transparent. If you have questions or find issues, please let us know!
