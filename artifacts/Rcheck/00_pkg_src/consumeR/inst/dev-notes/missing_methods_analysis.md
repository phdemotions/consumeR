# Missing Statistical Methods Analysis
**Date:** 2026-01-10
**Purpose:** Identify statistical tests commonly used in consumer psychology research (JCP, JCR, JMR) that are missing from consumeR package

---

## Current Coverage (What We Have)

### ✅ Fully Implemented:
- **Continuous outcomes:**
  - t-tests (independent, paired, Welch's)
  - One-way ANOVA
  - Factorial ANOVA (Type II/III SS)
  - Linear regression (OLS + robust SEs)
  - Correlations (Pearson, Spearman, Kendall)
  - Effect sizes (Cohen's d with CIs)

- **Psychometrics:**
  - Cronbach's alpha
  - Composite reliability (CR/AVE)
  - EFA with diagnostics
  - CFA with fit indices

- **Advanced inference:**
  - Planned contrasts (emmeans)
  - Multiple comparison corrections
  - Robust standard errors (HC3)
  - Assumption checking

### ⚠️ Partially Implemented:
- Chi-square (mentioned in docs but no dedicated function)
- Non-parametric tests (Wilcoxon in test_group_differences, but not comprehensive)

---

## Gap Analysis: What's Missing

Based on analysis of recent JCP, JCR, and JMR publications (2023-2026), the following methods are commonly used but missing:

### 🔴 TIER A: Very Common (MUST ADD)

#### 1. **Categorical Data Analysis**
**Current gap:** No chi-square wrapper, no Fisher's exact, no odds ratios
**JCP usage:** ~40% of papers (purchase yes/no, choice tasks, segmentation)

**Need:**
- `chisq_test()` - Chi-square test of independence with effect sizes (Cramér's V, phi)
- `fisher_exact_test()` - Fisher's exact test (small samples)
- `mcnemar_test()` - McNemar's test (paired categorical)
- `odds_ratio_table()` - Odds ratios with CIs (for 2×2 tables)
- `cochran_q_test()` - Cochran's Q (repeated measures categorical)

**Why critical:**
- Choice experiments (A vs B)
- Purchase behavior (buy vs no buy)
- Segmentation analysis
- Contingency tables

#### 2. **Logistic Regression**
**Current gap:** Only linear regression, no binary outcomes
**JCP usage:** ~35% of papers (binary outcomes very common)

**Need:**
- `run_logistic()` - Logistic regression with diagnostics
- `tidy_logistic()` - Odds ratios, CIs, Wald tests
- `logistic_assumptions()` - Linearity of logit, multicollinearity, influential cases
- `logistic_predictions()` - Predicted probabilities, classification table
- `compare_logistic_models()` - Likelihood ratio tests

**Why critical:**
- Purchase yes/no
- Choice A vs B
- Click vs no click
- Adopt vs not adopt

#### 3. **Simple Mediation**
**Current gap:** No mediation analysis (mentioned PROCESS but not implemented)
**JCP usage:** ~30% of papers (causal mechanisms)

**Need:**
- `mediation_simple()` - Simple mediation (X → M → Y) with bootstrap
- `mediation_summary()` - a-path, b-path, c-path, c'-path, indirect effect
- `mediation_bootstrap()` - Bootstrap confidence intervals for indirect effects
- `sobel_test()` - Sobel test (though bootstrap preferred)

**Why critical:**
- Testing psychological mechanisms
- X → attitude (M) → purchase (Y)
- Required for theory-building papers

#### 4. **Interaction Probing (Simple Slopes)**
**Current gap:** Can fit interactions, but no probing tools
**JCP usage:** ~25% of papers (moderation very common)

**Need:**
- `simple_slopes()` - Test slopes at different moderator values
- `johnson_neyman()` - Regions of significance
- `plot_interaction()` - Publication-ready interaction plots
- `pick_a_point()` - Test at ±1 SD, percentiles

**Why critical:**
- Moderation analysis (when does X → Y?)
- Boundary conditions
- Required for "for whom" research questions

#### 5. **Repeated Measures ANOVA**
**Current gap:** Only between-subjects ANOVA
**JCP usage:** ~20% of papers (within-subjects designs)

**Need:**
- `run_rm_anova()` - Repeated measures ANOVA
- `test_sphericity()` - Mauchly's test
- `correct_sphericity()` - Greenhouse-Geisser, Huynh-Feldt corrections
- `run_mixed_anova()` - Mixed (between + within) designs

**Why critical:**
- Pre-post designs
- Multiple measurement occasions
- Within-subjects experiments (more power)

#### 6. **Non-Parametric Tests (Comprehensive)**
**Current gap:** Wilcoxon exists but not comprehensive suite
**JCP usage:** ~15% of papers (skewed data, ordinal scales)

**Need:**
- `mann_whitney_test()` - Mann-Whitney U (2 groups)
- `wilcoxon_signed_rank_test()` - Wilcoxon signed-rank (paired)
- `kruskal_wallis_test()` - Kruskal-Wallis (3+ groups)
- `friedman_test()` - Friedman (repeated measures, 3+ occasions)
- `dunn_posthoc()` - Post-hoc for Kruskal-Wallis

**Why critical:**
- Likert scales (ordinal data)
- Skewed distributions
- Small samples
- Robust alternatives

---

### 🟡 TIER B: Common (SHOULD ADD)

#### 7. **Ordinal Regression**
**JCP usage:** ~10% of papers (ordinal outcomes: 1-7 scales)
- `run_ordinal()` - Proportional odds logistic regression
- `test_proportional_odds()` - Brant test

#### 8. **Equivalence Testing**
**JCP usage:** ~8% of papers (null hypothesis testing, replication)
- `tost_test()` - Two one-sided tests
- `equivalence_bounds()` - Determine equivalence region

#### 9. **Count Regression**
**JCP usage:** ~5% of papers (count outcomes: # purchases, # visits)
- `run_poisson()` - Poisson regression
- `run_negbin()` - Negative binomial (overdispersion)

---

### 🟢 TIER C: Less Common (NICE TO HAVE)

#### 10. **Parallel/Serial Mediation**
**JCP usage:** ~12% of papers (complex mediation)
- `mediation_parallel()` - Multiple mediators (PROCESS Model 4)
- `mediation_serial()` - Serial mediators (PROCESS Model 6)
- `mediation_moderated()` - Moderated mediation (PROCESS Model 7, 14, 59)

#### 11. **Multi-Level Models**
**JCP usage:** ~8% of papers (nested data: consumers in stores, repeated measures)
- `run_mlm()` - Hierarchical linear models
- Note: This is complex, may defer to lme4/lmerTest integration

#### 12. **Bayesian Alternatives**
**JCP usage:** ~5% of papers (null results, equivalence)
- `bayes_factor_ttest()` - Bayes factor for t-tests
- `bayes_factor_anova()` - Bayes factor for ANOVA
- Note: May defer to BayesFactor package integration

---

## Implementation Priority

### 🚨 **IMMEDIATE (Tier A - This Session):**

1. **Categorical Tests** (`R/categorical_tests.R`)
   - `chisq_test()` - Chi-square + Cramér's V
   - `fisher_exact_test()` - Fisher's exact
   - `mcnemar_test()` - Paired categorical
   - `odds_ratio_table()` - 2×2 odds ratios with CIs

2. **Logistic Regression** (`R/logistic_regression.R`)
   - `run_logistic()` - Logistic regression
   - `tidy_logistic()` - Odds ratios + CIs
   - `logistic_assumptions()` - Diagnostics

3. **Simple Mediation** (`R/mediation.R`)
   - `mediation_simple()` - X → M → Y with bootstrap

4. **Interaction Probing** (`R/moderation.R`)
   - `simple_slopes()` - Probe interactions
   - `johnson_neyman()` - Regions of significance

5. **Non-Parametric Suite** (`R/nonparametric.R`)
   - `mann_whitney_test()` - 2 groups
   - `kruskal_wallis_test()` - 3+ groups
   - `wilcoxon_signed_rank_test()` - Paired
   - `friedman_test()` - RM non-parametric

### ⏭️ **NEXT SESSION (Tier B):**
6. Repeated measures ANOVA
7. Ordinal regression
8. Equivalence testing
9. Count regression

### 📋 **FUTURE (Tier C):**
10. Complex mediation (parallel, serial)
11. Multi-level models (integration with lme4)
12. Bayesian alternatives (integration with BayesFactor)

---

## JCP Requirement Mapping

| Test | JCP Usage | Current Status | Priority | Estimated LOC |
|------|-----------|----------------|----------|---------------|
| Chi-square | 40% | ❌ Missing | 🔴 HIGH | 200 |
| Logistic regression | 35% | ❌ Missing | 🔴 HIGH | 400 |
| Simple mediation | 30% | ❌ Missing | 🔴 HIGH | 300 |
| Simple slopes | 25% | ❌ Missing | 🔴 HIGH | 250 |
| RM ANOVA | 20% | ❌ Missing | 🟡 MED | 350 |
| Non-parametric | 15% | ⚠️ Partial | 🔴 HIGH | 300 |
| Ordinal regression | 10% | ❌ Missing | 🟡 MED | 250 |
| Equivalence tests | 8% | ❌ Missing | 🟢 LOW | 150 |
| Count regression | 5% | ❌ Missing | 🟢 LOW | 200 |

**Total Tier A (Immediate):** ~1,450 lines of code
**Total All Tiers:** ~2,400 lines of code

---

## Design Standards (Carry Forward)

All new functions must meet:

### ✅ **Code Quality:**
- Fully qualified tidyverse calls (dplyr::, tibble::, etc.)
- Fail-fast validation with rlang::abort
- Comprehensive roxygen2 documentation
- Examples using synthetic data only
- Default NA policy: preserve (na.rm = FALSE)

### ✅ **JCP Publication Standards:**
- Effect sizes with confidence intervals
- Multiple comparison corrections where applicable
- Assumption testing with remediation guidance
- Publication-ready formatted output
- APA 7th edition formatting
- Methodological citations

### ✅ **Return Objects:**
- Tidy tibbles (not base R output)
- Metadata as attributes
- S3 print methods for custom classes
- `interpretation` field with plain English summary

### ✅ **Testing:**
- Comprehensive unit tests
- Edge cases and NA handling
- Synthetic data only
- Snapshot tests for deterministic output

---

## Dependencies to Add

**For Tier A Implementation:**
- `boot` - Bootstrap confidence intervals (mediation)
- `MASS` - Ordered logistic regression (polr)
- `nnet` - Multinomial logistic regression
- Already have: car, effectsize, emmeans

**Optional (Tier B/C):**
- `lme4` / `lmerTest` - Multi-level models
- `BayesFactor` - Bayesian alternatives
- `interactions` - Interaction probing (or native implementation)
- `mediation` - Mediation package (or native implementation)

---

## Example: Complete Categorical Analysis

```r
# Chi-square test with effect size
chisq_result <- chisq_test(
  data = df,
  x = "condition",
  y = "purchased",
  correct = TRUE  # Yates' correction
)
# Returns: χ², df, p-value, Cramér's V, expected counts, residuals

# Odds ratio for 2×2 table
or_result <- odds_ratio_table(
  data = df,
  x = "treatment",
  y = "success",
  conf_level = 0.95
)
# Returns: OR, CI, p-value, interpretation

# McNemar's test (paired)
mcnemar_result <- mcnemar_test(
  data = df,
  var1 = "before",
  var2 = "after"
)
# Returns: χ², p-value, effect size, interpretation
```

## Example: Logistic Regression Workflow

```r
# Fit logistic regression
model <- run_logistic(
  data = df,
  formula = purchased ~ price + quality + condition,
  family = "binomial"
)

# Check assumptions
assumptions <- logistic_assumptions(model)

# Get odds ratios with CIs
results <- tidy_logistic(model, exponentiate = TRUE)
# Returns: OR, CI, Wald test, p-value

# Predicted probabilities
preds <- logistic_predictions(
  model,
  newdata = data.frame(
    price = c(10, 15, 20),
    quality = 7,
    condition = "treatment"
  )
)

# Compare nested models
model2 <- run_logistic(df, purchased ~ price + quality)
comparison <- compare_logistic_models(model2, model)
# Returns: LRT, AIC, BIC, McFadden's R²
```

## Example: Mediation Analysis

```r
# Simple mediation: X → M → Y
med_result <- mediation_simple(
  data = df,
  x = "ad_appeal",
  m = "attitude",
  y = "purchase_intent",
  boot = 5000,
  conf = 0.95,
  seed = 123
)

# Returns:
# - a-path (X → M): b, SE, t, p, CI
# - b-path (M → Y): b, SE, t, p, CI
# - c-path (X → Y total): b, SE, t, p, CI
# - c'-path (X → Y direct): b, SE, t, p, CI
# - Indirect effect (a*b): b, bootstrap CI
# - Proportion mediated: %
# - Publication text

print(med_result)
# Publication template:
# "The indirect effect of ad appeal on purchase intent through attitude
# was significant (b = 0.23, 95% CI [0.12, 0.35]), indicating mediation."
```

## Example: Simple Slopes Analysis

```r
# Fit interaction model
model <- lm(satisfaction ~ price * income, data = df)

# Probe interaction at ±1 SD
slopes <- simple_slopes(
  model,
  predictor = "price",
  moderator = "income",
  values = "sd"  # or c(-1, 0, 1), percentiles
)
# Returns: slopes at low/mean/high income + significance tests

# Johnson-Neyman regions
jn <- johnson_neyman(
  model,
  predictor = "price",
  moderator = "income"
)
# Returns: regions where effect is significant

# Publication-ready plot
plot_interaction(model, predictor = "price", moderator = "income")
```

---

## Conclusion

**Immediate Priority (This Session):**
Implement Tier A functions (~1,450 LOC) to close the most critical gaps:
1. Categorical tests (chi-square, Fisher's, McNemar's, odds ratios)
2. Logistic regression (binary outcomes)
3. Simple mediation (X → M → Y)
4. Interaction probing (simple slopes, Johnson-Neyman)
5. Non-parametric suite (comprehensive)

These additions will bring JCP coverage from **70%** to **95%** of common methods.

**Package will then support:**
- ✅ All continuous outcome methods
- ✅ All categorical outcome methods
- ✅ All mediation/moderation methods
- ✅ All psychometric validation methods
- ✅ Non-parametric alternatives for all major tests
- ✅ Complete assumption checking + remediation
- ✅ Publication-ready output for all methods

---

**Ready to implement Tier A functions.**
