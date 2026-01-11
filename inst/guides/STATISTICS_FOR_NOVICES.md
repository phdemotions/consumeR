# Statistics for Novice Researchers: A Complete Guide

## Welcome!

**You don't need to be a statistician to do rigorous research.** This guide explains every statistical concept in the consumeR package in plain English, with real examples.

---

## Table of Contents

1. [Understanding P-Values](#understanding-p-values)
2. [What Are Effect Sizes?](#what-are-effect-sizes)
3. [Understanding Assumptions](#understanding-assumptions)
4. [Choosing the Right Test](#choosing-the-right-test)
5. [Reading Statistical Output](#reading-statistical-output)
6. [Common Mistakes to Avoid](#common-mistakes-to-avoid)
7. [Glossary](#glossary)

---

## Understanding P-Values

### What is a p-value?

**Simple answer:** The p-value tells you how likely your results are due to random chance.

**Analogy:** Imagine flipping a coin 100 times and getting 75 heads. That seems suspicious, right? The p-value tells you: "If this were a fair coin, what's the probability of getting 75 or more heads?" If that probability is very low (p < .05), you'd conclude the coin is probably unfair.

### The Magic Number: 0.05

- **p < 0.05**: Your result is "statistically significant" ✓
  - Less than 5% chance this happened by random luck
  - You can be reasonably confident there's a real effect

- **p ≥ 0.05**: Your result is "not statistically significant" ✗
  - More than 5% chance this happened by random luck
  - You can't rule out that it's just noise

### Common Misconceptions

❌ **WRONG:** "p = .03 means there's a 97% chance my hypothesis is true"
✅ **RIGHT:** "p = .03 means if there were NO effect, I'd only see results this extreme 3% of the time"

❌ **WRONG:** "p = .06 means my hypothesis is false"
✅ **RIGHT:** "p = .06 means I don't have enough evidence to reject the null hypothesis at the conventional .05 level"

❌ **WRONG:** "p = .001 means a bigger effect than p = .04"
✅ **RIGHT:** "Both are significant, but effect SIZE (not p-value) tells you how big the difference is"

### How consumeR Reports P-Values

```r
result <- test_group_differences(group1, group2)
result$p_value
# 0.0234

result$significant
# TRUE (because 0.0234 < 0.05)
```

**In your manuscript:**
> "Groups differed significantly (p = .023)"  or "p < .05" if very small

---

## What Are Effect Sizes?

### The Problem with P-Values Alone

**Scenario 1:** You have 1,000,000 participants. Groups differ by 0.1 points on a 7-point scale.
- **P-value:** p < .001 ✓ Significant!
- **Practical importance:** Basically zero. Who cares about 0.1 points?

**Scenario 2:** You have 20 participants. Groups differ by 2 points on a 7-point scale.
- **P-value:** p = .08 ✗ Not significant
- **Practical importance:** Could be huge! But you don't have enough data to prove it.

**The lesson:** P-values tell you if an effect EXISTS. Effect sizes tell you if it MATTERS.

### Cohen's d (for t-tests)

**What it means:** How many standard deviations apart are the two group means?

**Interpretation:**
- **d = 0.2**: Small effect (groups overlap a lot)
- **d = 0.5**: Medium effect (noticeable difference)
- **d = 0.8**: Large effect (groups clearly distinct)

**Real-world analogy:**
- **d = 0.2**: Like comparing heights of 13-year-olds vs. 13.5-year-olds
- **d = 0.5**: Like comparing heights of 13-year-olds vs. 15-year-olds
- **d = 0.8**: Like comparing heights of 13-year-olds vs. 18-year-olds

**consumeR output:**
```r
result$effect_size
# 0.65

result$effect_interpretation
# "medium"
```

**In your manuscript:**
> "The treatment group showed higher satisfaction than control (Cohen's d = 0.65, a medium effect size)"

### Eta-Squared (η²) for ANOVA

**What it means:** What percentage of variance is explained by your groups?

**Interpretation:**
- **η² = 0.01**: Small effect (1% of variance explained)
- **η² = 0.06**: Medium effect (6% of variance explained)
- **η² = 0.14**: Large effect (14% of variance explained)

**Real-world meaning:**
If η² = 0.10, that means 10% of the differences in your outcome variable are explained by which group people are in, and 90% is due to other factors (individual differences, measurement error, etc.).

### R² for Regression

**What it means:** How much of the variation in Y can be predicted from X?

**Interpretation:**
- **R² = 0.10**: Your predictors explain 10% of variance
- **R² = 0.30**: Your predictors explain 30% of variance (pretty good!)
- **R² = 0.50**: Your predictors explain 50% of variance (excellent!)

**Example:**
If you're predicting customer satisfaction (Y) from price, quality, and service (X), and R² = 0.40, that means these three factors explain 40% of why some customers are more satisfied than others.

---

## Understanding Assumptions

### What Are Assumptions?

**Statistical tests make assumptions** - like rules that need to be (mostly) true for the test to work correctly.

**Analogy:** A recipe assumes you have an oven. If you don't have an oven, you need a different recipe. Same with stats - if assumptions aren't met, you need a different test.

### The Big Three Assumptions

#### 1. Independence

**What it means:** Each observation should be independent - knowing one person's score shouldn't tell you about another person's score.

**Violated when:**
- ✗ You measure the same person multiple times (use repeated measures tests)
- ✗ You have nested data (students within classrooms - use multilevel models)
- ✗ You have matched pairs (siblings, couples - use paired tests)

**How consumeR checks this:**
We can't test this statistically - it's about your research design. We ask YOU to think about whether observations are independent.

#### 2. Normality

**What it means:** Your data should follow a bell-shaped curve (normal distribution).

**Why it matters:** Many tests assume data is normally distributed. If it's not, the test might give you wrong answers.

**How to tell:**
```r
# consumeR checks this automatically
result <- test_group_differences(group1, group2)
result$assumptions$normality_group1

# Results:
# "The normality assumption is MET"  ✓
# or
# "The normality assumption is VIOLATED"  ✗
```

**What to do if violated:**
1. **Use non-parametric tests** (Wilcoxon instead of t-test)
   - consumeR does this automatically!
2. **Transform your data** (log, square root)
3. **If n > 30**, often okay anyway (Central Limit Theorem)

**Visual check:**
- Make a histogram - does it look roughly bell-shaped?
- Make a Q-Q plot - do points fall roughly on a line?

#### 3. Homogeneity of Variance

**What it means:** Groups should have similar "spread" (variability).

**Example:**
- Group A: scores range from 5-7 (very consistent)
- Group B: scores range from 1-10 (all over the place)
- **Problem!** The spreads are very different.

**How consumeR checks:**
```r
result$assumptions$homogeneity

# "The homogeneity of variance assumption is MET"  ✓
# or
# "The homogeneity of variance assumption is VIOLATED"  ✗
```

**What to do if violated:**
- Use **Welch's t-test** instead of Student's t-test
- consumeR automatically does this for you!

---

## Choosing the Right Test

### Decision Tree

**Start here:** What's your research question?

#### Comparing Two Groups

**Question:** "Are two groups different?"
**Examples:**
- Treatment vs. Control
- Before vs. After
- Men vs. Women

**Test:** `test_group_differences()`
```r
result <- test_group_differences(
  data$outcome[data$group == "Treatment"],
  data$outcome[data$group == "Control"]
)
```

**What it does:**
- Checks if assumptions are met
- Automatically picks the right test (t-test or Wilcoxon)
- Gives you publication-ready output

#### Comparing Three or More Groups

**Question:** "Do multiple groups differ?"
**Examples:**
- Low vs. Medium vs. High price
- Four different marketing messages
- Three market segments

**Test:** `compare_groups_anova()`
```r
result <- compare_groups_anova(
  data,
  formula = satisfaction ~ segment
)
```

**What it does:**
- Checks if ANY groups differ (F-test)
- If yes, tells you WHICH groups differ (post-hoc tests)
- Handles unequal variances (Welch's ANOVA)

#### Predicting an Outcome

**Question:** "What predicts my outcome?"
**Examples:**
- Do price, quality, and service predict satisfaction?
- Does ad spending predict sales?

**Test:** `analyze_regression()`
```r
result <- analyze_regression(
  data,
  satisfaction ~ price + quality + service
)
```

**What it does:**
- Tells you which predictors matter
- Shows how much variance is explained (R²)
- Checks all regression assumptions

#### Measuring Relationships

**Question:** "Are two variables related?"
**Examples:**
- Is satisfaction correlated with loyalty?
- Do price and quality go together?

**Test:** `analyze_correlation()`
```r
result <- analyze_correlation(
  data,
  "satisfaction",
  "loyalty"
)
```

**What it does:**
- Measures strength of relationship (r)
- Picks Pearson or Spearman based on normality
- Tells you if it's significant

---

## Reading Statistical Output

### Example: T-Test Output

```r
result <- test_group_differences(treatment, control)
print(result)
```

**What you'll see:**

```
═══════════════════════════════════════════════════════════
GROUP COMPARISON RESULTS
═══════════════════════════════════════════════════════════

Test Used: Student's t-test
Sample Sizes: Group 1 (n = 50), Group 2 (n = 50)

DESCRIPTIVE STATISTICS:
  Group 1: M = 7.45, SD = 1.23
  Group 2: M = 6.12, SD = 1.45
  Difference: 1.33, 95% CI [0.78, 1.88]

STATISTICAL RESULTS:
  Test statistic: 4.523
  Degrees of freedom: 98
  p-value: < .001
  Significant: YES (p < .05)

EFFECT SIZE:
  Cohen's d = 0.98 (large)

INTERPRETATION:
  The groups are significantly different (p < .001).
  Group 1 (M = 7.45, SD = 1.23) has a higher mean than
  Group 2 (M = 6.12, SD = 1.45), with a mean difference
  of 1.33. The effect size is large (Cohen's d = 0.98).
```

**How to read this:**

1. **Test Used** - Which test was run (t-test, Wilcoxon, etc.)
2. **Sample Sizes** - How many people in each group
3. **Descriptive Statistics:**
   - **M** = Mean (average)
   - **SD** = Standard Deviation (how spread out scores are)
   - **95% CI** = Confidence Interval (range where true difference likely falls)
4. **Statistical Results:**
   - **Test statistic** - Technical number (ignore unless asked)
   - **p-value** - Is it significant? (look for < .05)
5. **Effect Size** - How big is the difference? (small/medium/large)
6. **Interpretation** - Plain English summary

### What to Report in Your Manuscript

**Minimal reporting:**
> "Treatment group (M = 7.45, SD = 1.23) scored higher than control group (M = 6.12, SD = 1.45), t(98) = 4.52, p < .001, d = 0.98."

**With publication text:**
```r
print(result, show_publication = TRUE)
# Copy-paste the output into your manuscript!
```

---

## Common Mistakes to Avoid

### 1. P-Hacking

**What it is:** Running many tests until you find p < .05

**Example of p-hacking:**
```r
# DON'T DO THIS:
test_group_differences(satisfaction, condition)  # p = .08 (not sig)
test_group_differences(satisfaction, age_group)  # p = .12 (not sig)
test_group_differences(satisfaction, gender)     # p = .03 (sig!) ← Report this one
```

**Why it's bad:** If you run 20 tests, you'll get 1 significant result by chance (5% = 1 in 20)

**How to avoid:**
- Pre-register your hypotheses
- Only test what you said you'd test
- Report ALL tests you ran, not just significant ones

### 2. Ignoring Effect Sizes

**The mistake:**
> "We found a significant difference (p < .001)"

**Better:**
> "We found a significant difference (p < .001, d = 0.25)"

**Why it matters:** With huge samples, tiny meaningless differences can be "significant"

### 3. Assuming p > .05 Means "No Effect"

**The mistake:**
> "There was no effect (p = .08)"

**Better:**
> "There was no statistically significant effect (p = .08), though the effect size was small to medium (d = 0.35). The study may have been underpowered."

**Why:** Absence of evidence ≠ evidence of absence

### 4. Not Checking Assumptions

**The mistake:**
```r
# Just run the test without checking anything
t.test(group1, group2)
```

**Better:**
```r
# Let consumeR check assumptions for you
result <- test_group_differences(group1, group2)
print(result, show_assumptions = TRUE)
```

**Why:** If assumptions are violated, your p-values might be wrong

### 5. Confusing Correlation with Causation

**The mistake:**
> "Ice cream sales cause drowning deaths (r = .95, p < .001)"

**Reality:**
- Both increase in summer (confounding variable: temperature)
- Correlation ≠ causation

**Remember:**
- Correlation tells you variables go together
- Only experiments (with random assignment) can show causation

---

## Glossary

**Alpha (α):** The significance threshold, usually 0.05

**ANOVA:** Analysis of Variance - comparing 3+ groups

**Assumptions:** Requirements that must be (mostly) met for a test to work properly

**Central Limit Theorem:** With large samples (n > 30), means are normally distributed even if raw data isn't

**Cohen's d:** Effect size for t-tests (standardized mean difference)

**Confidence Interval (CI):** Range where the true value probably falls (usually 95% CI)

**Correlation (r):** Measures strength of relationship (-1 to +1)

**Degrees of Freedom (df):** Technical parameter related to sample size

**Effect Size:** How big/important an effect is (not just if it's significant)

**Eta-squared (η²):** Effect size for ANOVA (proportion of variance explained)

**Homogeneity of Variance:** Groups have similar spread/variability

**Hypothesis:**
- **Null hypothesis:** There's no effect
- **Alternative hypothesis:** There is an effect

**Independence:** Each observation is unrelated to others

**Normality:** Data follows a bell-shaped curve

**Non-parametric Test:** Test that doesn't assume normality (e.g., Wilcoxon)

**P-value:** Probability of getting results this extreme if null hypothesis is true

**Post-hoc Test:** Follow-up tests after ANOVA to see which groups differ

**Power:** Ability to detect an effect if one exists (higher is better)

**R² (R-squared):** Proportion of variance explained by predictors (regression)

**Regression:** Predicting an outcome from one or more predictors

**Residuals:** Differences between observed and predicted values

**Standard Deviation (SD):** Average distance from the mean (measure of spread)

**Standard Error (SE):** Uncertainty in an estimate (smaller is better)

**Statistical Significance:** p < .05 (conventionally)

**Test Statistic:** Technical value from test (t, F, r, etc.)

**T-test:** Comparing means of two groups

**Type I Error:** False positive (saying there's an effect when there isn't)

**Type II Error:** False negative (missing an effect that's really there)

**Welch's Test:** Version of t-test/ANOVA that doesn't assume equal variances

**Wilcoxon Test:** Non-parametric version of t-test

---

## Quick Reference Card

### When to Use Each Test

| Research Question | Test | consumeR Function |
|-------------------|------|-------------------|
| Compare 2 groups | t-test | `test_group_differences()` |
| Compare 3+ groups | ANOVA | `compare_groups_anova()` |
| Predict outcome | Regression | `analyze_regression()` |
| Measure relationship | Correlation | `analyze_correlation()` |

### Interpreting Effect Sizes

| Effect Size | Cohen's d | η² | R² |
|-------------|-----------|-----|-----|
| Small | 0.2 | 0.01 | 0.10 |
| Medium | 0.5 | 0.06 | 0.30 |
| Large | 0.8 | 0.14 | 0.50 |

### P-Value Guide

| P-value | Meaning | Symbol |
|---------|---------|--------|
| < .001 | Very strong evidence | *** |
| < .01 | Strong evidence | ** |
| < .05 | Significant | * |
| < .10 | Marginal | . |
| ≥ .10 | Not significant | ns |

---

## Getting Help

**Still confused?** That's normal! Statistics is hard.

**Resources:**
1. Read the verbose output: `print(result, show_assumptions = TRUE)`
2. Check the guide: `?test_group_differences`
3. Look at examples: All functions have working examples
4. Ask for help: Statistics consultants, your advisor, online forums

**Remember:** You don't need to understand every detail. The consumeR package does the technical stuff. You just need to:
1. Know which test answers your question
2. Understand the output
3. Report it correctly in your paper

**The package gives you MORE explanation than you need** - choose what makes sense for your level and your journal's requirements.

---

**Good luck with your research!** 🎉

