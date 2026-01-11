# consumeR Package Integration Audit
**Date:** 2026-01-10
**Auditor:** Claude (Sonnet 4.5)
**Purpose:** Inventory existing functionality and identify integration points for psychometric methods

---

## Executive Summary

The `consumeR` package is a well-structured R package for consumer research workflows with strong foundations in:
- **Data import & cleaning** (excellent exclusion pipeline with publication-ready reporting)
- **Basic psychometrics** (alpha, composite reliability, EFA)
- **Statistical testing** (ANOVA, regression, correlation, t-tests with comprehensive diagnostics)
- **Declarative pipeline framework** (studyspec.R with YAML/JSON schema)

**Critical Gaps Identified:**
1. **NO CFA implementation** (confirmatory factor analysis)
2. **NO emmeans integration** for planned contrasts/simple effects
3. **NO robust standard errors** wrappers
4. **NO moderated mediation** (PROCESS Model 59 style)
5. **Incomplete composite scoring API** (no structured objects, reverse scoring helpers missing)
6. **No formatting utilities** for publication-ready output (p-values, CIs, tables)

**Integration Strategy:** Extend existing code (NOT replace), add missing modules, wire into studyspec pipeline.

---

## Part 1: Existing Functionality (By File)

### A. Data Import & I/O (`data_import.R`)

**Exported:**
- `import_research_data(file_path, interactive, auto_convert, clean_names, create_report)`
  - Imports CSV/SPSS with type detection
  - Optional janitor::clean_names
  - Returns list(data, variable_summary, validation_report, import_log)

- `check_variable_types(data, suggest_changes)`
  - Interactive variable type review

**Assessment:**
✅ Covers basic I/O needs
✅ Uses haven for SPSS
✅ Creates validation plots
⚠️ Does NOT have generic `read_study_data()` wrapper
⚠️ Does NOT have `write_outputs()` for exporting results

**Recommendation:** Add lightweight wrappers for consistency.

---

### B. Data Cleaning (`data_cleaning.R`)

**Exported:**
- `clean_survey_data(data, pretest_var, pretest_values, date_var, pretest_before_date, inclusion_criteria, attention_checks, attention_check_rule, additional_exclusions, additional_exclusion_reason, rename_vars, recode_vars, id_var, verbose)`
  - **Comprehensive exclusion engine**
  - Column-based AND date-based pretest removal
  - Declarative inclusion criteria
  - Attention checks with flexible rules (all/majority/n)
  - Variable renaming & recoding
  - Returns: clean_data, exclusion_summary, publication_text, flags, participant_flow

**Internal Helpers:**
- `generate_cleaning_publication_text()`

**Assessment:**
✅ EXCELLENT exclusion pipeline
✅ Publication-ready CONSORT flow
✅ Tracks every exclusion with reasons
✅ Transparent for peer review
⚠️ Does NOT use rlang::abort (uses base stop/warning)
⚠️ Does NOT use cli for messages

**Recommendation:**
- Keep existing implementation intact
- Optionally refactor messages to use cli (low priority)
- Consider extracting `flag_exclusions()` + `apply_exclusions()` as separate functions for studyspec integration

---

### C. Reliability Analysis (`reliability_analysis.R`)

**Exported:**
- `calculate_alpha(data, items, scale_name, reverse_items, item_labels)`
  - Cronbach's alpha
  - Reverse scoring support
  - Alpha if item deleted
  - Inter-item correlations
  - Returns structured list with interpretation

**Assessment:**
✅ Solid alpha implementation
✅ Reverse scoring integrated
✅ Publication text
⚠️ Reverse scoring is INTERNAL (not exposed as standalone function)
⚠️ NO performance::cronbachs_alpha as alternative

**Gaps:**
- Missing: `reverse_score_likert(x, min, max, strict)` standalone function
- Missing: `alpha_table(df, scales_spec, method)` for batch processing

---

### D. Composite Reliability (`composite_reliability.R`)

**Exported:**
- `calculate_composite_reliability(data, items, scale_name, item_labels)`
  - Composite Reliability (CR)
  - Average Variance Extracted (AVE)
  - √AVE for discriminant validity
  - Uses PCA for loadings
  - Returns: alpha, CR, AVE, sqrt_ave, passes_threshold, item_statistics

**Assessment:**
✅ Implements CR/AVE correctly
✅ Uses PCA approach (acceptable)
⚠️ NOT using lavaan CFA (PCA is approximation)
⚠️ NO batch processing for multiple scales

**Gaps:**
- Missing: `score_composite()` function that returns structured object:
  ```r
  score_composite(df, items, reverse_items = NULL,
                  na_rule = c("preserve", "threshold"),
                  threshold = NULL, name = NULL)
  # Returns: list(value = vector, meta = list(...))
  ```
- Missing: `add_composite(df, ...)` to attach scored composite back to df
- Missing: `row_sd(df_or_matrix, items)` helper

---

### E. Factor Analysis (`factor_analysis.R`)

**Exported:**
- `perform_efa(data, items, n_factors, rotation, create_plots, item_labels)`
  - Uses base R `factanal()`
  - Eigenvalue analysis
  - Scree plot (ggplot2)
  - Loading heatmap
  - Variance explained
  - Returns tidy tibble with loadings

**Assessment:**
✅ Good EFA implementation
✅ Beautiful visualizations
✅ Tidyverse-style output
⚠️ Basic KMO/Bartlett (approximation, not full psych::KMO)

**Critical Gaps:**
- **NO CFA implementation**
  - Missing: `run_cfa(model_syntax, df, estimator, missing, std.lv, ...)`
  - Missing: `tidy_cfa_fit(fit)` to extract CFI/TLI/RMSEA/SRMR/χ²/AIC/BIC
  - Missing: `compare_cfa_models(fit1, fit2)` using lavaan::anova
- **NO separate EFA diagnostics function**
  - Missing: `efa_diagnostics(df_items)` → KMO, Bartlett's, fa.parallel

---

### F. Statistical Tests

#### F1. ANOVA (`anova.R`)

**Exported:**
- `compare_groups_anova(data, groups, formula, check_assumptions, use_welch, post_hoc, alpha, verbose)`
  - One-way ANOVA
  - Welch's ANOVA (auto-selected on homogeneity violation)
  - Post-hoc: Tukey HSD, Bonferroni
  - Eta-squared effect size
  - Comprehensive assumptions (uses check_normality, check_homogeneity_of_variance, check_independence)
  - Returns: f_statistic, p_value, eta_squared, group_statistics, assumptions, post_hoc_results, publication_text

**Assessment:**
✅ Excellent ANOVA with diagnostics
✅ Auto Welch selection
✅ Post-hoc tests
⚠️ Post-hoc is built-in (NOT using emmeans)
⚠️ NO Type II/III SS options
⚠️ NO factorial ANOVA

**Critical Gaps:**
- Missing: `run_anova(df, formula, type = c("II", "III"))` using car::Anova
- Missing: `emmeans_contrasts(model, specs, ...)` wrapper
- Missing: Integration with emmeans for planned contrasts

#### F2. Regression (`regression.R`)

**Exported:**
- `analyze_regression(data, formula, check_assumptions, create_plots, alpha, verbose)`
  - Simple & multiple linear regression
  - Breusch-Pagan for homoscedasticity
  - VIF for multicollinearity
  - Diagnostic plots
  - R², adjusted R², RMSE
  - Returns: coefficients, r_squared, adj_r_squared, f_statistic, p_value, assumptions, diagnostic_plots, lm_model

**Assessment:**
✅ Comprehensive regression diagnostics
✅ VIF calculation
✅ Breusch-Pagan test
✅ Returns lm_model for further analysis
⚠️ NO robust standard errors
⚠️ NO emmeans integration

**Critical Gaps:**
- Missing: `tidy_lm_robust(model, hc_type = "HC3")` using sandwich::vcovHC + lmtest::coeftest
- Missing: `run_lm(df, formula, ...)` wrapper for consistency

#### F3. Correlation (`correlation.R`)

**Exported:**
- `analyze_correlation(data, var1, var2, method, check_assumptions, alpha, verbose)`
  - Pearson & Spearman
  - Auto method selection
  - Normality checks
  - Confidence intervals (Pearson only)
  - Returns: correlation, p_value, ci_lower, ci_upper, strength, direction, assumptions

**Assessment:**
✅ Good correlation with assumptions
✅ Auto method selection
✅ Strength interpretation
⚠️ Single pairwise correlation only (NOT correlation matrix)

**Gaps:**
- Missing: `correlation_table(df, vars, method)` for multiple correlations (could use correlation::correlation)

#### F4. Group Tests (`group_tests.R`)

**Exported:**
- `test_group_differences(group1, group2, test_type, alternative, conf_level, paired, check_assumptions, verbose)`
  - t-test / Wilcoxon
  - Auto test selection
  - Cohen's d effect size
  - Paired option
  - Returns: test_used, p_value, difference, effect_size, assumptions

**Assessment:**
✅ Good 2-group comparison
✅ Cohen's d included
⚠️ Cohen's d is INTERNAL (not standalone)

**Gaps:**
- Missing: `cohens_d_table(df, outcome, group, ...)` using effectsize::cohens_d
- Missing: `chisq_table(df, x, y, ...)` for categorical associations
- Missing: `t_test_table(df, outcome, group, ...)` wrapper for consistency

---

### G. Summary Statistics (`summary_stats.R`)

**Exported:**
- `calculate_summary_stats(data, include_all, round_digits)`
  - n, mean, median, sd, min, max, Q1, Q3, variance, range, IQR

**Assessment:**
✅ Basic descriptive stats
⚠️ Does NOT group by categorical variables

**Gaps:**
- Consider: grouped summaries, `dplyr::group_by()` integration

---

### H. Publication Text (`publication_text.R`)

**Exported:**
- `generate_publication_block(test_type, assumptions_checks, test_results, effect_sizes, additional_notes)`
  - APA 7th edition style
  - Assumptions + methods + results + interpretation

**Internal Helpers:**
- `generate_assumptions_text()`
- `generate_methods_text()`
- `generate_results_text()`
- `generate_interpretation_text()`

**Assessment:**
✅ Good publication text framework
⚠️ Assumes specific result object structures

**Gaps:**
- Missing: `format_p(p, digits, style)` - standardized p-value formatting
- Missing: `format_n(n)` - "N = 123" formatting
- Missing: `format_est_ci(est, lo, hi, digits)` - "2.34 [1.23, 3.45]"
- Missing: `make_table_md(df, caption)` - markdown table
- Missing: `make_table_tsv(df)` - TSV for copy/paste

---

### I. Pipeline / StudySpec (`studyspec.R`)

**Internal Functions (NOT exported):**
- `validate_spec(spec)`
- `read_spec_yaml(path, template)`
- `read_spec_json(path, template)`
- `ingest_data(data_or_path, spec)`
- `apply_exclusions(data, spec)` - uses filter expressions from spec
- `score_constructs_from_dictionary(data, spec)` - mean/sum methods
- `standardize_vars(data, spec)` - z-score
- `run_models(data, spec)` - lm/glm
- `build_tables(model_results, spec)`
- `run_pipeline(spec, data_or_path)` - full orchestrator

**Schema Files:**
- `inst/studyspec/schema.yaml`
- `inst/studyspec/schema.json`
- `inst/studyspec/example.yaml`
- `inst/studyspec/example.json`

**Assessment:**
✅ **EXCELLENT** declarative pipeline infrastructure
✅ Schema validation with rlang::abort
✅ Exclusion engine with audit trail
✅ Construct scoring (mean/sum)
✅ Standardization
✅ Model specification
⚠️ NO psychometric modules (alpha, EFA, CFA) in spec
⚠️ NO emmeans/contrasts in spec
⚠️ NO robust SE in spec
⚠️ NO mediation in spec

**Critical Gap:**
This is the PERFECT place to integrate the missing functionality. The spec system provides the orchestration layer.

---

### J. Assumptions Checking (`assumptions.R`)

**Exported:**
- `check_normality(data, variable_name, alpha)` - Shapiro-Wilk + visual checks
- `check_homogeneity_of_variance(data, groups, alpha)` - Levene's test
- `check_independence(data_structure, is_independent, clustering_note)` - documentation only

**Assessment:**
✅ Good assumption checks
✅ Publication text included
✅ Used by ANOVA/regression

---

### K. Analysis Report (`analysis_report.R`)

**Exported:**
- `create_analysis_report(...)` - generates comprehensive report

**Assessment:**
✅ High-level wrapper
(Not reviewed in detail - likely calls other functions)

---

### L. Utils (`utils.R`)

**Internal:**
- `%+%` string concatenation operator

**Assessment:**
⚠️ Very minimal

**Gaps:**
- Missing: `assert_vars_present(df, vars, label)` - schema validator with actionable errors
- Missing: `clean_names_safe(df)` - wrapper for janitor::clean_names with error handling

---

## Part 2: What Is MISSING (Grouped by Module)

### A. Schema + I/O Utilities
- [ ] `assert_vars_present(df, vars, label = NULL)` - fail-fast validator
  - Use rlang::abort with informative error
  - List missing vars + available vars

- [ ] `clean_names_safe(df)` - optional wrapper for janitor::clean_names
  - Handle case where janitor not installed

- [ ] `read_study_data(path, type = c("rds","sav","csv"), clean_names, ...)`
  - Wrapper around readr::read_rds, haven::read_sav, readr::read_csv
  - Optional clean_names = TRUE

- [ ] `write_outputs(x, path, format = c("tsv","md","rds"))`
  - Export results to disk
  - TSV: readr::write_tsv
  - Markdown: use knitr::kable or similar
  - RDS: readr::write_rds

### B. Scoring Utilities
- [ ] `reverse_score_likert(x, min = 1, max = 7, strict = TRUE)`
  - Standalone function (currently internal in calculate_alpha)
  - Formula: (max + min) - x
  - strict = TRUE: abort if values outside [min, max]

- [ ] `row_sd(df_or_matrix, items)`
  - Row-wise standard deviation across items
  - Handle NA with na.rm option

- [ ] `score_composite(df, items, reverse_items = NULL, na_rule = c("preserve","threshold"), threshold = NULL, name = NULL)`
  - **Returns structured object:**
    ```r
    list(
      value = numeric vector,
      meta = list(
        items = character vector,
        reverse_items = character vector or NULL,
        k = integer (number of items),
        na_rule = character,
        threshold = numeric or NULL,
        missing = integer vector (which rows have NA)
      )
    )
    ```
  - na_rule = "preserve": use na.rm = FALSE (default)
  - na_rule = "threshold": require threshold non-NA items

- [ ] `add_composite(df, ...)` - wrapper that calls score_composite() and attaches value as column
  - Returns df with new column
  - Optionally store meta as attribute

### C. Exclusions / Cleaning Pipeline Integration
**Already excellent!** `clean_survey_data()` is comprehensive.

**Optional enhancements (LOW PRIORITY):**
- [ ] Extract `flag_exclusions(df, rules_spec)` as standalone
- [ ] Extract `apply_exclusions(df, rules_spec)` as standalone
  (Currently exists in studyspec.R but uses filter expressions - could unify with clean_survey_data logic)

- [ ] `exclusion_summary(df, reason_col = "exclusion_reason")` - tidy summary table
  (Currently part of clean_survey_data output - could extract)

### D. Psychometrics

#### D1. Alpha
**Current:** `calculate_alpha()` is good.

**Missing:**
- [ ] `alpha_table(df, scales_spec, method = c("performance","psych"), ...)`
  - Batch calculate alpha for multiple scales
  - scales_spec = list(scale1 = c("items"), scale2 = c("items"), ...)
  - Returns tidy tibble: scale_name, alpha, n_items, n_obs, interpretation
  - method = "performance": use performance::cronbachs_alpha (if available)
  - method = "psych": use psych::alpha (if available)
  - Default: use existing calculate_alpha logic

#### D2. EFA
**Current:** `perform_efa()` is good.

**Missing:**
- [ ] `efa_diagnostics(df_items)`
  - Separate function for pre-EFA checks
  - Returns:
    - KMO: psych::KMO(df_items)
    - Bartlett: psych::cortest.bartlett(cor(df_items), n)
    - Parallel analysis: psych::fa.parallel(df_items)
  - Summary message with recommendations

#### D3. CFA (CRITICAL GAP)
- [ ] `run_cfa(model_syntax, df, estimator = "MLR", missing = "fiml", std.lv = TRUE, ...)`
  - Wrapper around lavaan::cfa()
  - model_syntax: lavaan syntax string
  - Pass ... to lavaan::cfa
  - Returns lavaan fit object

- [ ] `tidy_cfa_fit(fit)`
  - Extract fit indices from lavaan fit object
  - Returns tibble with ONE row:
    - chisq, df, pvalue, cfi, tli, rmsea, rmsea.ci.lower, rmsea.ci.upper, srmr, aic, bic
  - Use lavaan::fitMeasures()

- [ ] `compare_cfa_models(fit_small, fit_large, ...)`
  - Use lavaan::anova(fit_small, fit_large) IF nested
  - Warn if models not nested
  - Returns tidy comparison table: model, chisq, df, chisq_diff, df_diff, p_value, aic, bic

### E. Primary Models + Inference

#### E1. ANOVA enhancements
**Current:** `compare_groups_anova()` is good for one-way.

**Missing:**
- [ ] `run_anova(df, formula, type = c("II","III"), ...)`
  - Wrapper around car::Anova(lm(...), type = type)
  - Returns tidy ANOVA table

- [ ] `assumption_checks(model)`
  - Unified assumption checker
  - For ANOVA: car::leveneTest, normality of residuals
  - For regression: Breusch-Pagan, VIF, normality
  - Returns list of checks

- [ ] `emmeans_contrasts(model, specs, ...)`
  - Wrapper around emmeans::emmeans() + emmeans::contrast()
  - specs: list(factor = "condition", contrasts = "pairwise")
  - Returns tidy tibble with estimate, SE, df, t.ratio, p.value, ci.lower, ci.upper

#### E2. Effect Sizes & Tests
**Current:** Effect sizes are embedded in test functions.

**Missing:**
- [ ] `cohens_d_table(df, outcome, group, ...)`
  - Wrapper around effectsize::cohens_d()
  - Returns tidy tibble

- [ ] `chisq_table(df, x, y, ...)`
  - Wrapper around stats::chisq.test()
  - Returns tidy result

- [ ] `t_test_table(df, outcome, group, ...)`
  - Wrapper around stats::t.test()
  - Returns tidy result
  - (Alternative to test_group_differences for consistency)

- [ ] `correlation_table(df, vars, method = "pearson")`
  - Multiple pairwise correlations
  - Use correlation::correlation() from easystats
  - Returns tidy tibble

### F. Robustness
- [ ] `tidy_lm_robust(model, hc_type = "HC3")`
  - Use sandwich::vcovHC(model, type = hc_type)
  - Use lmtest::coeftest(model, vcov. = vcov_robust)
  - Returns tidy tibble: term, estimate, robust_se, t_value, p_value, ci_lower, ci_upper

### G. Moderated Mediation (CRITICAL GAP)
- [ ] `process59_parallel(df, y, x, m, w, covariates = NULL, center = TRUE, boot = 5000, seed = NULL, conf = 0.95)`
  - PROCESS Model 59 style (parallel mediators with moderation on direct/indirect)
  - Implement natively (do NOT download Hayes files)
  - Returns:
    - path_coefficients: tidy tibble with all a, b, c, c' paths
    - conditional_indirect_effects: at low/med/high W
    - index_moderated_mediation: if applicable
    - bootstrap_ci: bootstrap CIs for indirect effects
    - meta: list(y, x, m, w, covariates, n, boot, seed, centered)
  - **This is complex - implement step by step:**
    1. Center variables (if center = TRUE)
    2. Fit regressions: M ~ X + W + X*W + covariates
    3. Fit regression: Y ~ X + M + W + X*W + M*W + covariates (if moderated mediation)
    4. Calculate indirect effects at W levels
    5. Bootstrap CIs

### H. Reporting Outputs
- [ ] `format_p(p, digits = 3, style = c("apa","plain"))`
  - style = "apa": "p < .001" or "p = .042" (no leading zero)
  - style = "plain": "p < 0.001" or "p = 0.042"
  - Handle p < .001 case

- [ ] `format_n(n)`
  - "N = 123" or "(n = 45)"
  - Handle vector input: c(100, 50) → "N = 100, n = 50"

- [ ] `format_est_ci(est, lo, hi, digits = 2)`
  - "2.34 [1.23, 3.45]"
  - Handle vectors

- [ ] `make_table_md(df, caption = NULL)`
  - Markdown table using knitr::kable(df, format = "markdown")
  - Add caption if provided

- [ ] `make_table_tsv(df)`
  - Return TSV string using capture.output(write.table(df, sep = "\t"))
  - For copy/paste into spreadsheets

---

## Part 3: Implementation Plan

### Phase 1: Core Utilities (FOUNDATION)
**File:** `R/utilities.R` (NEW)

Implement:
1. `assert_vars_present()`
2. `format_p()`, `format_n()`, `format_est_ci()`
3. `clean_names_safe()` (if needed)

**File:** `R/io.R` (NEW)

Implement:
4. `read_study_data()`
5. `write_outputs()`

**File:** `R/table_formatting.R` (NEW)

Implement:
6. `make_table_md()`
7. `make_table_tsv()`

**Dependencies to add to DESCRIPTION:**
- Suggests: cli, rlang (move to Imports if not already)
- Suggests: knitr (already there)

---

### Phase 2: Scoring Enhancements (CRITICAL)
**File:** `R/composite_scoring.R` (NEW or extend composite_reliability.R)

Implement:
1. `reverse_score_likert()`
2. `row_sd()`
3. `score_composite()` - returns structured object
4. `add_composite()` - wrapper

**Tests:** `tests/testthat/test-composite_scoring.R`

---

### Phase 3: Psychometrics (CRITICAL)
**File:** `R/psychometrics.R` (NEW)

Implement:
1. `alpha_table()` - batch alpha
2. `efa_diagnostics()` - KMO, Bartlett, parallel
3. `run_cfa()` - lavaan wrapper
4. `tidy_cfa_fit()` - extract fit indices
5. `compare_cfa_models()` - nested model comparison

**Dependencies:**
- Suggests: lavaan (>= 0.6.0), psych (>= 2.0.0)

**Tests:** `tests/testthat/test-psychometrics.R`

---

### Phase 4: Modeling Enhancements
**File:** `R/modeling.R` (NEW)

Implement:
1. `run_lm()` - wrapper
2. `run_anova()` - car::Anova wrapper
3. `assumption_checks()` - unified
4. `emmeans_contrasts()` - emmeans wrapper
5. `cohens_d_table()`, `chisq_table()`, `t_test_table()`, `correlation_table()`

**Dependencies:**
- Suggests: car, emmeans, effectsize, correlation (easystats)

**Tests:** `tests/testthat/test-modeling.R`

---

### Phase 5: Robustness
**File:** `R/robust.R` (NEW)

Implement:
1. `tidy_lm_robust()` - sandwich + lmtest

**Dependencies:**
- Suggests: sandwich, lmtest

**Tests:** `tests/testthat/test-robust.R`

---

### Phase 6: Moderated Mediation (COMPLEX)
**File:** `R/mediation.R` (NEW)

Implement:
1. `process59_parallel()` - native implementation
   - Break into helper functions:
     - `center_vars()`
     - `fit_mediation_models()`
     - `calculate_indirect_effects()`
     - `bootstrap_mediation()`

**Dependencies:**
- Imports: boot (for bootstrapping)

**Tests:** `tests/testthat/test-mediation.R`
- Use known examples from published papers
- Lock behavior with snapshot tests

---

### Phase 7: StudySpec Integration (ORCHESTRATION)
**File:** `R/studyspec.R` (EXTEND)

Add to schema:
- `psychometrics` section:
  - `alpha`: list of scales
  - `efa`: specification
  - `cfa`: models
- `contrasts` section: emmeans specs
- `robustness` section: HC type
- `mediation` section: PROCESS specs

Extend `run_pipeline()`:
- Call psychometric functions if specified
- Call emmeans if specified
- Call robust SE if specified
- Call mediation if specified

**Tests:** `tests/testthat/test-studyspec.R` (extend existing)

---

### Phase 8: High-Level Orchestrator (OPTIONAL)
**File:** `R/pipeline.R` (NEW)

Implement:
- `run_study_pipeline(raw_path_or_df, spec)`
  - Wrapper around studyspec::run_pipeline with better user-facing API

---

### Phase 9: Documentation
**File:** `vignettes/psychometric_workflow.Rmd` (NEW)

Demonstrate:
- Alpha → EFA → CFA → reliability assessment
- ANOVA → emmeans contrasts → robust SE
- Moderated mediation workflow

**File:** `vignettes/complete_study_pipeline.Rmd` (NEW)

End-to-end example using synthetic data:
- Import → Clean → Score → Psychometrics → Models → Robustness → Tables

---

## Part 4: Integration Principles (CRITICAL RULES)

### RULE 1: NEVER Overwrite Existing Exported Functions
- `clean_survey_data()` is excellent → KEEP AS-IS
- `calculate_alpha()` is good → KEEP AS-IS
- `perform_efa()` is good → KEEP AS-IS
- `compare_groups_anova()`, `analyze_regression()`, etc. → KEEP AS-IS

### RULE 2: Fully Qualified tidyverse Calls
```r
# GOOD
df %>% dplyr::mutate(x = dplyr::case_when(...))
df %>% dplyr::select(dplyr::all_of(vars))
df %>% tidyr::pivot_longer(...)

# BAD
df %>% mutate(x = case_when(...))  # NO unqualified!
```

### RULE 3: Fail-Fast Validation
```r
# GOOD
assert_vars_present(df, required_vars, "my_function")
if (missing(formula)) rlang::abort("formula is required")

# BAD
if (!all(vars %in% names(df))) warning("Some vars missing")  # Too lenient!
```

### RULE 4: Default NA Policy
- Composites: `na.rm = FALSE` by default (preserve NA)
- User can override with `na_rule = "threshold"`

### RULE 5: No Runtime Auto-Install
```r
# GOOD
if (!requireNamespace("lavaan", quietly = TRUE)) {
  rlang::abort("lavaan required. Install with: install.packages('lavaan')")
}

# BAD
if (!require(lavaan)) install.packages("lavaan")  # NO!
```

### RULE 6: Roxygen2 Docs + Examples
Every exported function MUST have:
- @description
- @param for ALL parameters
- @return with structure
- @examples using SYNTHETIC data (not real data)
- @export

### RULE 7: Tests for Everything
- `tests/testthat/test-*.R` for each new file
- Snapshot tests for deterministic tables/prints
- Synthetic data only

### RULE 8: Use rlang::abort + cli
```r
# GOOD
if (length(items) < 2) {
  rlang::abort(c(
    "Need at least 2 items",
    "x" = "You provided {length(items)} item{?s}",
    "i" = "Available columns: {paste(names(df), collapse = ', ')}"
  ))
}

# ACCEPTABLE (if cli not used yet)
if (length(items) < 2) {
  rlang::abort("Need at least 2 items. You provided {length(items)}.")
}
```

---

## Part 5: Dependency Management

### Current DESCRIPTION:
**Imports:**
- methods, dplyr, tidyr, ggplot2, tibble, readr, haven

**Suggests:**
- testthat, knitr, rmarkdown, janitor, lavaan, psych, broom

### Additions Needed:
**Move to Imports (if implementing core functionality):**
- rlang
- cli (optional but recommended)

**Add to Suggests:**
- car (for Anova, VIF, leveneTest)
- emmeans (for contrasts)
- sandwich (for robust SE)
- lmtest (for coeftest with robust SE)
- effectsize (for Cohen's d, eta-squared)
- correlation (easystats, for correlation tables)
- boot (for bootstrapping in mediation)
- performance (optional alternative for alpha)

### Conditional Use Pattern:
```r
use_lavaan <- function() {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    rlang::abort(c(
      "Package 'lavaan' is required for CFA",
      "i" = "Install with: install.packages('lavaan')"
    ))
  }
  TRUE
}
```

---

## Part 6: File Structure (Proposed)

```
R/
├── utilities.R           (NEW: assert_vars_present, format_p, format_n, format_est_ci)
├── io.R                  (NEW: read_study_data, write_outputs)
├── table_formatting.R    (NEW: make_table_md, make_table_tsv)
├── composite_scoring.R   (NEW: reverse_score_likert, row_sd, score_composite, add_composite)
├── psychometrics.R       (NEW: alpha_table, efa_diagnostics, run_cfa, tidy_cfa_fit, compare_cfa_models)
├── modeling.R            (NEW: run_lm, run_anova, assumption_checks, emmeans_contrasts, effect size wrappers)
├── robust.R              (NEW: tidy_lm_robust)
├── mediation.R           (NEW: process59_parallel + helpers)
├── pipeline.R            (NEW: run_study_pipeline - optional high-level orchestrator)
│
├── data_import.R         (KEEP: import_research_data, check_variable_types)
├── data_cleaning.R       (KEEP: clean_survey_data)
├── reliability_analysis.R (KEEP: calculate_alpha)
├── composite_reliability.R (KEEP: calculate_composite_reliability)
├── factor_analysis.R     (KEEP: perform_efa)
├── anova.R               (KEEP: compare_groups_anova)
├── regression.R          (KEEP: analyze_regression)
├── correlation.R         (KEEP: analyze_correlation)
├── group_tests.R         (KEEP: test_group_differences)
├── summary_stats.R       (KEEP: calculate_summary_stats)
├── publication_text.R    (KEEP: generate_publication_block)
├── studyspec.R           (EXTEND: add psychometrics/contrasts/mediation to schema)
├── assumptions.R         (KEEP: check_normality, check_homogeneity_of_variance, check_independence)
├── analysis_report.R     (KEEP)
├── consumeR-package.R    (KEEP)
├── consumer_survey_data.R (KEEP)
├── data.R                (KEEP)
└── utils.R               (KEEP: %+%)

tests/testthat/
├── test-utilities.R       (NEW)
├── test-io.R              (NEW)
├── test-table_formatting.R (NEW)
├── test-composite_scoring.R (NEW)
├── test-psychometrics.R   (NEW)
├── test-modeling.R        (NEW)
├── test-robust.R          (NEW)
├── test-mediation.R       (NEW)
├── test-analysis_report.R (EXISTING)
├── test-group_tests.R     (EXISTING)
└── test-summary_stats.R   (EXISTING)

vignettes/
├── getting-started.Rmd    (EXISTING)
├── psychometric_workflow.Rmd (NEW)
└── complete_study_pipeline.Rmd (NEW)
```

---

## Part 7: Critical Next Steps

### Immediate (Do First):
1. ✅ Create `inst/dev-notes/integration_audit.md` (THIS FILE)
2. Add `R/utilities.R` with format_p, format_n, assert_vars_present
3. Add `R/composite_scoring.R` with score_composite, reverse_score_likert, row_sd
4. Add `R/psychometrics.R` with run_cfa, tidy_cfa_fit, efa_diagnostics
5. Write tests for above

### High Priority (Core Functionality):
6. Add `R/modeling.R` with emmeans_contrasts, run_anova (Type II/III)
7. Add `R/robust.R` with tidy_lm_robust
8. Add `R/table_formatting.R` with make_table_md, make_table_tsv
9. Write tests

### Medium Priority (Advanced Features):
10. Add `R/mediation.R` with process59_parallel
11. Extend `R/studyspec.R` with psychometrics/contrasts/mediation schema
12. Write vignettes

### Low Priority (Nice to Have):
13. Refactor existing functions to use cli for messages (optional)
14. Add grouped summary stats to summary_stats.R
15. Add correlation_table for multiple correlations

---

## Part 8: Naming Conventions

The package uses **descriptive verb_noun** naming:
- `calculate_alpha()` ✓
- `perform_efa()` ✓
- `compare_groups_anova()` ✓
- `analyze_regression()` ✓
- `clean_survey_data()` ✓
- `import_research_data()` ✓

**Adopt same convention for new functions:**
- `run_cfa()` ✓
- `score_composite()` ✓
- `format_p()` ✓
- `tidy_lm_robust()` ✓

**Avoid:**
- Abbreviations without context (e.g., `cfa()` alone)
- Generic names (e.g., `fit()`, `test()`)

---

## Part 9: Testing Strategy

### Unit Tests:
- Test each function with synthetic data
- Test edge cases: missing data, invalid input, single item, etc.
- Test error messages: ensure rlang::abort works

### Snapshot Tests:
- Lock deterministic output: tables, publication text
- Use testthat::expect_snapshot() for print methods

### Integration Tests:
- Test full pipeline: import → clean → score → analyze
- Use studyspec with realistic YAML

### Example Data:
- Create synthetic datasets in `R/data.R` or `data-raw/`
- Small (n=50-100), deterministic
- Cover: multiple groups, scales, conditions

---

## Part 10: Backward Compatibility

### Golden Rule:
**DO NOT change signatures of existing exported functions.**

### Safe Extensions:
✅ Add NEW optional parameters with defaults
✅ Add NEW functions
✅ Extend return objects with NEW fields (keep existing)

### Unsafe Changes (FORBIDDEN):
❌ Change parameter names
❌ Change parameter order
❌ Change default values
❌ Remove fields from return objects
❌ Change return object class

### Deprecation Path (if needed):
1. Add new function with better name
2. Mark old function with `.Deprecated("new_function")`
3. Document in NEWS.md
4. Remove after 2+ versions

---

## Conclusion

The `consumeR` package has **excellent foundations** in data cleaning, basic psychometrics, and statistical testing. The **studyspec pipeline** is a hidden gem that provides the perfect orchestration layer.

**Critical gaps:**
- CFA (lavaan integration)
- emmeans integration
- Robust SE
- Moderated mediation
- Composite scoring API
- Formatting utilities

**Integration strategy:**
1. **Extend, don't replace** existing code
2. **Add missing modules** as new files
3. **Wire into studyspec** for orchestration
4. **Maintain backward compatibility**
5. **Follow package conventions**: naming, tidyverse qualification, fail-fast validation

**Effort estimate:**
- Phase 1-3 (utilities, scoring, psychometrics): 2-3 days
- Phase 4-5 (modeling, robust): 1-2 days
- Phase 6 (mediation): 2-3 days (complex)
- Phase 7-9 (integration, docs): 1-2 days

**Total:** ~1-2 weeks for complete implementation with tests and docs.

---

**Audit Complete.**
**Ready to proceed with implementation.**
