# consumeR: Statistical Analysis for Journal of Consumer Psychology Standards

## Package Philosophy

The `consumeR` package is specifically designed to help consumer researchers meet the rigorous reporting standards required by top-tier journals, particularly the **Journal of Consumer Psychology (JCP)**.

### Why This Package Exists

Most statistical software (R, SPSS, Python) gives you test statistics - but journals don't just want statistics. They want:

✅ **Complete transparency** about assumptions
✅ **Explicit documentation** of data cleaning
✅ **Publication-ready text** that can go directly into manuscripts
✅ **Effect sizes** for all analyses
✅ **Clear explanation** of methods for peer reviewers

**The Problem:** Researchers (especially those not trained in statistics) spend hours:
- Figuring out which statistical test to use
- Checking if assumptions are met
- Writing up methods and results sections
- Worrying if they're reporting everything correctly

**The Solution:** `consumeR` does all of this automatically, following Journal of Consumer Psychology standards.

---

## What Makes This Package Different

### 1. **Designed for Non-Statisticians**

Most researchers in consumer psychology are NOT statisticians. This package:
- Uses plain English explanations
- Automatically selects appropriate tests based on assumptions
- Provides verbose output so you can learn what each test does
- Gives you MORE text than you need, so you can select what fits your journal

### 2. **Publication-Ready Output**

Every function generates text that follows:
- APA 7th edition style
- Journal of Consumer Psychology guidelines
- Transparency and Openness Promotion (TOP) standards
- CONSORT reporting guidelines (for sample flow)

**You can literally copy-paste the output into your manuscript.**

### 3. **Complete Assumption Checking**

Every statistical test automatically checks its assumptions:
- Normality (Shapiro-Wilk test)
- Homogeneity of variance (Levene's test, Breusch-Pagan test)
- Independence (design-based assessment)
- Multicollinearity (VIF for regression)

For EACH assumption, you get:
- Test result (met or violated)
- Plain English interpretation
- Specific recommendation
- Publication-ready sentence
- Verbose explanation (for learning)

### 4. **Explicit Methods**

Unlike other packages that hide details, `consumeR` makes everything explicit:
- Which test was used (Student's t, Welch's t, Wilcoxon, etc.)
- Why that test was selected
- What assumptions were checked
- What the results mean in practical terms

### 5. **Data Cleaning as Priority**

Data cleaning is THE FIRST STEP, not an afterthought:
- Track pre-test exclusions
- Document inclusion criteria failures
- Report attention check results
- Generate CONSORT-style participant flow
- Create publication-ready sample description

---

## Journal of Consumer Psychology Requirements

### From JCP Author Guidelines:

> **Data Collection and Analysis**
>
> "Authors must report how they determined their sample size, all data exclusions (if any), all manipulations, and all measures in the study. Authors must also report all conditions and data manipulations. If any data or conditions have been excluded from analyses, this must be clearly stated and justified."

> **Statistical Reporting**
>
> "In addition to reporting the results of null hypothesis significance tests (e.g., t, F), authors should report effect sizes (e.g., Cohen's d, eta-squared) and confidence intervals where appropriate."

> **Reproducibility**
>
> "Authors are encouraged to make their data and analysis scripts publicly available. The journal encourages the use of reproducible analysis workflows."

### How `consumeR` Helps:

| JCP Requirement | How consumeR Helps |
|-----------------|-------------------|
| Report all exclusions | `clean_survey_data()` tracks every exclusion with reasons |
| Report sample size determination | Publication text includes initial and final sample sizes |
| Report all manipulations | Documentation helpers for condition assignment |
| Report effect sizes | Every test includes appropriate effect size (d, η², r²) |
| Report confidence intervals | CIs included for all applicable tests |
| Reproducibility | All analysis parameters explicitly documented |
| Assumption checking | Every test checks and reports assumptions |

---

## Typical JCP Manuscript Structure (with consumeR)

### Method Section

**Participants**

```r
# Use consumeR to clean data and generate this text
cleaning <- clean_survey_data(
  data = raw_data,
  pretest_var = "is_pretest",
  inclusion_criteria = list(
    completed = raw_data$Finished == 1,
    adult = raw_data$age >= 18
  ),
  attention_checks = list(
    ac1 = list(var = "AC1", correct = 3),
    ac2 = list(var = "AC2", correct = "agree")
  )
)

# Copy this into your manuscript:
cat(cleaning$publication_text$verbose)
```

**Output:**
> "Data were collected from 500 participants via Amazon Mechanical Turk. We excluded 15 pre-test cases, 23 participants who did not complete the survey, 18 participants under age 18, and 28 participants who failed one or more attention checks. The final analytical sample consisted of 432 participants (86.4% of initial sample; Mage = 34.2, SD = 11.5; 52% female)."

**Procedure**

[Describe your procedure...]

**Measures**

[Describe your measures, then check reliability:]

```r
# Check scale reliability
alpha_result <- calculate_alpha(
  data = df,
  items = c("sat1", "sat2", "sat3", "sat4"),
  scale_name = "Customer Satisfaction"
)

print(alpha_result)
# "The Customer Satisfaction scale demonstrated excellent reliability (α = .92, 95% CI [.90, .94])..."
```

### Results Section

**Preliminary Analyses**

```r
# Check assumptions
t_result <- test_group_differences(
  df$satisfaction[df$condition == "Treatment"],
  df$satisfaction[df$condition == "Control"]
)

# Copy assumption text:
print(t_result, show_assumptions = TRUE)
```

**Output:**
> "Data normality was assessed using the Shapiro-Wilk test. Both groups met the normality assumption (Treatment: W = 0.97, p = .23; Control: W = 0.98, p = .41). Homogeneity of variance was assessed using Levene's test (F(1, 430) = 1.45, p = .23)."

**Main Analyses**

```r
# Run analysis and get publication text
print(t_result, show_publication = TRUE)
```

**Output:**
> "An independent samples t-test revealed a statistically significant difference between conditions (t(430) = 3.24, p = .001). The treatment condition (M = 5.72, SD = 1.14) exceeded the control condition (M = 5.21, SD = 1.09), 95% CI [0.20, 0.82], Cohen's d = 0.45, representing a small to medium effect size."

---

## Key Standards Met

### 1. **Transparency Standards**

✅ All data exclusions documented
✅ All assumption violations reported
✅ All statistical decisions explained
✅ Complete audit trail from raw to clean data

### 2. **APA 7th Edition**

✅ Correct statistical notation (M, SD, t, F, p, d, η², r)
✅ Proper p-value formatting (p < .001, p = .045)
✅ Confidence intervals reported
✅ Effect sizes with interpretation

### 3. **Statistical Best Practices**

✅ Assumption checking before analysis
✅ Appropriate test selection
✅ Effect sizes over p-values
✅ Multiple comparison corrections (ANOVA post-hocs)
✅ Robust alternatives when assumptions violated (Welch's tests)

### 4. **Reproducibility**

✅ Complete parameter documentation
✅ Explicit test selection criteria
✅ Seed setting for random processes
✅ Version tracking

---

## Common JCP Reviewer Comments (and How consumeR Addresses Them)

### "Please report effect sizes"

❌ **Without consumeR:**
> "The groups differed significantly (t = 3.24, p < .001)."

✅ **With consumeR:**
> "The groups differed significantly (t(430) = 3.24, p < .001, Cohen's d = 0.45)."

### "Were assumptions checked?"

❌ **Without consumeR:**
> "We conducted a t-test..."
> *Reviewer: "Did you check normality? Homogeneity of variance?"*

✅ **With consumeR:**
> "Data normality was assessed using the Shapiro-Wilk test. Both groups met the normality assumption (Treatment: W = 0.97, p = .23; Control: W = 0.98, p = .41). Homogeneity of variance was assessed using Levene's test (F(1, 430) = 1.45, p = .23). We conducted an independent samples t-test..."

### "How many participants were excluded?"

❌ **Without consumeR:**
> "Data were collected from 432 participants."
> *Reviewer: "How many did you originally collect? Who was excluded?"*

✅ **With consumeR:**
> "Data were collected from 500 participants. We excluded 15 pre-test cases, 23 participants who did not complete the survey, 18 participants under age 18, and 28 participants who failed attention checks. The final sample consisted of 432 participants (86.4% retention)."

### "Why did you use this test?"

❌ **Without consumeR:**
> "We used Welch's t-test."
> *Reviewer: "Why Welch's instead of Student's?"*

✅ **With consumeR:**
> "Due to violation of the homogeneity of variance assumption (Levene's F(1, 430) = 4.52, p = .034), we employed Welch's t-test, which does not assume equal variances."

---

## Package Functions Overview

### Data Cleaning (STEP 1 - Do This First!)

| Function | Purpose | JCP Standard |
|----------|---------|--------------|
| `clean_survey_data()` | Clean data with exclusion tracking | Complete participant flow reporting |
| `import_research_data()` | Import CSV/SPSS with validation | Data provenance documentation |

### Descriptive Statistics

| Function | Purpose | JCP Standard |
|----------|---------|--------------|
| `calculate_summary_stats()` | Comprehensive descriptives | M, SD, quartiles, range |
| `calculate_alpha()` | Cronbach's alpha reliability | Internal consistency reporting |
| `calculate_composite_reliability()` | CR and AVE | Advanced reliability metrics |

### Group Comparisons

| Function | Purpose | JCP Standard |
|----------|---------|--------------|
| `test_group_differences()` | Compare 2 groups (t-test/Wilcoxon) | Assumption checks, effect sizes, CIs |
| `compare_groups_anova()` | Compare 3+ groups (ANOVA) | Post-hoc tests, eta-squared, assumptions |

### Regression and Correlation

| Function | Purpose | JCP Standard |
|----------|---------|--------------|
| `analyze_regression()` | Linear regression | R², coefficients table, VIF, diagnostics |
| `analyze_correlation()` | Pearson/Spearman correlation | r, r², CIs, strength interpretation |

### Factor Analysis

| Function | Purpose | JCP Standard |
|----------|---------|--------------|
| `perform_efa()` | Exploratory factor analysis | Factor loadings, variance explained, plots |

---

## Complete Workflow Example

Here's a complete analysis workflow meeting JCP standards:

```r
library(consumeR)

# ============================================================
# STEP 1: DATA CLEANING (ALWAYS FIRST!)
# ============================================================

cleaning <- clean_survey_data(
  data = raw_data,
  pretest_var = "preview",
  inclusion_criteria = list(
    completed = raw_data$Finished == 1,
    adult = raw_data$age >= 18,
    us_only = raw_data$country == "US"
  ),
  attention_checks = list(
    ac1 = list(var = "AC_1", correct = 3),
    ac2 = list(var = "AC_2", correct = "agree")
  ),
  id_var = "ResponseId"
)

# Get publication text for Methods section
cat(cleaning$publication_text$concise)

# Extract clean data
df <- cleaning$clean_data

# ============================================================
# STEP 2: RELIABILITY CHECKS
# ============================================================

# Check scale reliability
satisfaction_reliability <- calculate_alpha(
  df,
  items = c("sat1", "sat2", "sat3", "sat4"),
  scale_name = "Satisfaction"
)
print(satisfaction_reliability)

# ============================================================
# STEP 3: DESCRIPTIVE STATISTICS
# ============================================================

desc_treatment <- calculate_summary_stats(df$satisfaction[df$condition == "Treatment"])
desc_control <- calculate_summary_stats(df$satisfaction[df$condition == "Control"])

# ============================================================
# STEP 4: MAIN ANALYSIS
# ============================================================

main_result <- test_group_differences(
  df$satisfaction[df$condition == "Treatment"],
  df$satisfaction[df$condition == "Control"]
)

# View complete results with assumptions and publication text
print(main_result, show_assumptions = TRUE, show_publication = TRUE)

# ============================================================
# STEP 5: COPY-PASTE INTO MANUSCRIPT
# ============================================================

# All the text you need is in:
# - cleaning$publication_text$concise (Participants section)
# - satisfaction_reliability (Measures section)
# - main_result$publication_text (Results section)
```

**That's it! You now have everything needed for a JCP-quality Methods and Results section.**

---

## Tips for JCP Submission

### 1. **Pre-register When Possible**

consumeR makes it easy to pre-register your analysis plan:
- Specify your exclusion criteria
- Define your statistical approach
- Document your alpha level
- State your effect size expectations

### 2. **Report Assumption Violations**

If assumptions are violated, `consumeR` automatically:
- Detects the violation
- Recommends the appropriate alternative (e.g., Welch's t-test)
- Provides publication text explaining the decision

### 3. **Include Effect Sizes**

Every test in `consumeR` includes:
- Cohen's d (for t-tests)
- Eta-squared (for ANOVA)
- R-squared (for regression)
- r and r² (for correlation)

With interpretation: negligible, small, medium, large

### 4. **Show Your Work**

The verbose output explains:
- What each assumption means
- Why it matters
- What happens if it's violated
- How to interpret the test

Choose the level of detail appropriate for your journal.

### 5. **Supplement with Raw Output**

For ultimate transparency, you can include:
```r
# Include in supplementary materials
write.csv(cleaning$flags, "participant_exclusions.csv")
write.csv(cleaning$exclusion_summary, "exclusion_summary.csv")
saveRDS(main_result, "analysis_results.rds")
```

---

## Getting Help

### Documentation

```r
# Function help
?clean_survey_data
?test_group_differences
?compare_groups_anova

# Workflow guides
browseVignettes("consumeR")
```

### Guides

- **DATA_CLEANING_WORKFLOW.md** - Complete data cleaning guide
- **STATISTICAL_METHODS_GUIDE.md** - All statistical tests
- **EXAMPLES_GUIDE.md** - Real-world examples

### Common Questions

**Q: Do I need to know statistics to use this package?**
A: No! The package is designed for non-statisticians. The verbose output teaches you what each test does.

**Q: Can I use this for journals other than JCP?**
A: Yes! The output follows APA 7th edition and is suitable for any psychology/marketing journal (JCR, JMR, JPSP, etc.)

**Q: What if my journal has different requirements?**
A: The package gives you MORE detail than most journals need. Select the appropriate sentences for your specific journal.

**Q: Can I customize the output?**
A: Yes! All functions have parameters to control output verbosity, assumption checking, and text generation.

---

## Citation

When using this package in publications:

```
We used the consumeR package (version X.X.X) for all statistical analyses,
which provides publication-ready output following Journal of Consumer
Psychology standards.
```

---

## Package Design Principles

1. **Transparency First** - Every decision is documented and explained
2. **Novice-Friendly** - Assumes user is NOT a statistician
3. **Publication-Ready** - Output can be directly used in manuscripts
4. **Conservative** - Warns when assumptions are questionable
5. **Educational** - Teaches statistics while conducting analyses
6. **Reproducible** - Complete documentation of all parameters and decisions

---

## Updates and Contributions

This package is actively maintained to stay current with JCP guidelines and statistical best practices.

**Planned features:**
- Mediation analysis
- Moderation analysis
- Mixed effects models
- Bayesian alternatives
- Power analysis
- Sample size calculators

---

**Remember: Good statistics isn't just about getting p-values. It's about transparency, reproducibility, and helping readers understand your findings.**

The `consumeR` package makes it easy to meet the highest standards.
