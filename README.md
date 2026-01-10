# consumeR <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/phdemotions/consumeR/workflows/R-CMD-check/badge.svg)](https://github.com/phdemotions/consumeR/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/consumeR)](https://CRAN.R-project.org/package=consumeR)
<!-- badges: end -->

## Statistical Analysis for Journal of Consumer Psychology Standards

**consumeR** is an R package designed specifically for consumer researchers who need publication-ready statistical analysis that meets **Journal of Consumer Psychology (JCP)** standards - even if you're not a statistician.

consumeR supports the most common analytical workflows in business and consumer research, with planned extensions for multilevel models, measurement models, and generalized linear models. All methods—current and future—adhere to the same transparency and reporting constraints.

### 🎯 Why consumeR?

**The Problem:** Top journals like JCP require rigorous reporting of:
- Complete data cleaning documentation with all exclusions
- Explicit assumption checking for every test
- Effect sizes and confidence intervals for all analyses
- Publication-ready Methods and Results text

**The Solution:** consumeR provides all of this automatically, with:

✅ **Publication-Ready Text** - Copy-paste directly into your manuscript (follows APA 7th edition and JCP guidelines)
✅ **Complete Assumption Checking** - Every test explicitly checks and reports its assumptions
✅ **Explicit Methods** - Never wonder "which test should I use?" - it tells you exactly what it's running and why
✅ **Novice-Friendly** - Designed for researchers who aren't statisticians (includes verbose explanations)
✅ **Data Cleaning First** - Comprehensive exclusion tracking with CONSORT-style participant flow
✅ **Effect Sizes Always** - Cohen's d, eta-squared, R², and clear interpretation (small/medium/large)
✅ **More Text Than You Need** - Select the sentences appropriate for your specific journal

**Key Insight:** Most packages give you statistics. consumeR gives you everything you need for publication.

## Installation

You can install the development version of consumeR from GitHub:

```r
# install.packages("devtools")
devtools::install_github("phdemotions/consumeR")
```

Once on CRAN, install with:

```r
install.packages("consumeR")
```

## Quick Start

### Complete Research Workflow (Following JCP Standards)

```r
library(consumeR)

# ═══════════════════════════════════════════════════════════════
# STEP 1: DATA CLEANING (ALWAYS FIRST! Required for JCP)
# ═══════════════════════════════════════════════════════════════

# Import raw data
raw_data <- read.csv("qualtrics_export.csv")

# Clean with complete exclusion tracking
cleaning <- clean_survey_data(
  data = raw_data,

  # Track pre-test cases
  pretest_var = "is_pretest",
  pretest_values = c(1, TRUE),

  # Document inclusion criteria
  inclusion_criteria = list(
    completed = raw_data$Finished == 1,
    adult = raw_data$age >= 18,
    us_resident = raw_data$country == "US"
  ),

  # Check attention
  attention_checks = list(
    ac1 = list(var = "AC_1", correct = 3),
    ac2 = list(var = "AC_2", correct = "agree")
  ),
  attention_check_rule = "all",

  id_var = "ResponseId"
)

# View results
print(cleaning)
# Initial sample:  n = 500
# Final sample:    n = 432
# Retention rate:  86.4%

# Get publication text for your Methods section
cat(cleaning$publication_text$concise)
# "Data were collected from 500 participants. We excluded 15 pre-test cases,
#  23 participants who did not meet inclusion criteria, 28 who failed attention
#  checks. Final sample: 432 participants (86.4% retention)."

# Extract clean data for analysis
df <- cleaning$clean_data

# ═══════════════════════════════════════════════════════════════
# STEP 2: STATISTICAL ANALYSES (with automatic assumption checking)
# ═══════════════════════════════════════════════════════════════

# Compare two groups (automatically checks ALL assumptions!)
result <- test_group_differences(
  df$satisfaction[df$condition == "Treatment"],
  df$satisfaction[df$condition == "Control"]
)

# View standard output
print(result)
# GROUP COMPARISON RESULTS
# Test Used: Student's t-test
# Group 1: M = 7.45, SD = 1.23
# Group 2: M = 6.12, SD = 1.45
# Cohen's d = 0.98 (large effect)
# p < .001 ✓ Significant

# See assumption checks (verbose explanations included!)
print(result, show_assumptions = TRUE)
# NORMALITY GROUP 1: ✓ MET (Shapiro-Wilk W = 0.97, p = .234)
# NORMALITY GROUP 2: ✓ MET (Shapiro-Wilk W = 0.95, p = .156)
# HOMOGENEITY OF VARIANCE: ✓ MET (Levene's F = 1.23, p = .267)

# Get publication-ready text for your manuscript
print(result, show_publication = TRUE)
# PUBLICATION-READY TEXT
#
# ASSUMPTIONS:
# Data normality was assessed using the Shapiro-Wilk test. Both groups
# met the normality assumption...
#
# METHODS:
# An independent samples t-test was conducted to compare means between
# groups...
#
# RESULTS:
# The independent samples t-test revealed a statistically significant
# difference (t(430) = 5.67, p < .001, Cohen's d = 0.98)...

# ═══════════════════════════════════════════════════════════════
# OTHER STATISTICAL TESTS (all with assumption checking!)
# ═══════════════════════════════════════════════════════════════

# ANOVA for 3+ groups
anova_result <- compare_groups_anova(df, satisfaction ~ segment)
print(anova_result, show_publication = TRUE)

# Linear regression
reg_result <- analyze_regression(df, satisfaction ~ price + quality + service)
print(reg_result, show_assumptions = TRUE)

# Correlation
cor_result <- analyze_correlation(df, "satisfaction", "loyalty")
print(cor_result, show_publication = TRUE)

# Reliability analysis
alpha <- calculate_alpha(df, items = c("sat1", "sat2", "sat3", "sat4"))
print(alpha)
# α = 0.92 (Excellent reliability)
```

### 📚 Complete Documentation

**🎓 New to Statistics? Start Here:**
- **[STATISTICS_FOR_NOVICES.md](STATISTICS_FOR_NOVICES.md)** - Everything explained in plain English (no stats background needed!)

**Essential Guides (in order of workflow):**
1. **[DATA_CLEANING_WORKFLOW.md](DATA_CLEANING_WORKFLOW.md)** - STEP 1: Data cleaning with exclusion tracking
2. **[STATISTICAL_METHODS_GUIDE.md](STATISTICAL_METHODS_GUIDE.md)** - STEP 2: All statistical tests with examples
3. **[JCP_STANDARDS.md](JCP_STANDARDS.md)** - Why this package meets Journal of Consumer Psychology standards

**Additional Resources:**
- **[CODE_REVIEW_IMPROVEMENTS.md](CODE_REVIEW_IMPROVEMENTS.md)** - How we ensure gold-standard quality
- **RESEARCH_WORKFLOW.md** - Complete analysis workflow
- **EXAMPLES_GUIDE.md** - Real-world examples

## Key Features

### Transparency First

Every function in consumeR is designed with transparency in mind:

```r
# The code itself is the documentation
# Open any function to see exactly what it does:
calculate_summary_stats
```

You'll see extensive inline comments explaining:
- What each step does
- Why it's done that way
- What assumptions are being made
- How to interpret the results

### Automatic Best Practices

- **Missing value handling**: Automatically removes NAs and reports how many
- **Test selection**: Intelligently chooses appropriate statistical tests
- **Input validation**: Clear error messages if something is wrong
- **Assumption checking**: Transparent about when assumptions are or aren't met

### Designed for Reviewers

Output is optimized for peer review:

```r
result <- test_group_differences(group1, group2)

# Get the interpretation in plain English:
result$interpretation
#> "The groups are significantly different (p = 0.0234).
#> Group 1 has a higher mean (67.5) than Group 2 (54.2),
#> with a difference of 13.3."

# But all technical details are available too:
result$p_value
result$test_used
result$full_test_output
```

## Documentation

consumeR provides multiple levels of documentation:

1. **Complete Workflow Guide**: See `RESEARCH_WORKFLOW.md` for step-by-step analysis workflow
2. **Function help files**: `?calculate_summary_stats`, `?import_research_data`, etc.
3. **Comprehensive vignette**: `vignette("getting-started", package = "consumeR")`
4. **Detailed guides**:
   - `DATA_IMPORT_GUIDE.md` - CSV/SPSS import with automatic name cleaning
   - `NEW_FEATURES_SUMMARY.md` - Reliability & factor analysis
   - `EXAMPLES_GUIDE.md` - Cloud 9 themed examples
5. **Inline code comments**: View any function's source code
6. **This README**: Quick overview and examples

## Main Functions

### Data Management
| Function | Purpose | Key Feature |
|----------|---------|-------------|
| `import_research_data()` | Import CSV/SPSS files | **Auto name cleaning** + type detection |
| `check_variable_types()` | Validate variables | Interactive checking with suggestions |

### Descriptive & Comparative Analysis
| Function | Purpose | Key Feature |
|----------|---------|-------------|
| `calculate_summary_stats()` | Descriptive statistics | Step-by-step documented calculations |
| `test_group_differences()` | Compare two groups | Plain English interpretation + effect size |

### Reliability & Validity
| Function | Purpose | Key Feature |
|----------|---------|-------------|
| `calculate_alpha()` | Cronbach's alpha | Transparent item analysis + diagnostics |
| `calculate_composite_reliability()` | CR & AVE | Quality thresholds with interpretation |

### Factor Analysis
| Function | Purpose | Key Feature |
|----------|---------|-------------|
| `perform_efa()` | Exploratory factor analysis | **Beautiful ggplot2 visualizations** |

### Reporting
| Function | Purpose | Key Feature |
|----------|---------|-------------|
| `create_analysis_report()` | Full analysis report | Complete transparency for reviewers |

## Philosophy

consumeR is built on these principles:

1. **Code should be readable by non-programmers**: Extensive comments and clear variable names
2. **Results should be interpretable**: Plain English explanations alongside statistics
3. **Methods should be transparent**: Every decision and assumption is documented
4. **Analysis should be reproducible**: Clear, deterministic calculations
5. **Reviewers should trust the results**: Complete openness about what's happening

## Examples

### Example 1: Simple Customer Satisfaction Analysis

```r
# Customer satisfaction scores (1-10 scale)
satisfaction <- c(8, 7, 9, 6, 8, 7, 9, 8, 7, 6, 9, 8)

# Calculate statistics
stats <- calculate_summary_stats(satisfaction)

# Report results
cat(sprintf("Average satisfaction: %.1f/10\n", stats$mean))
cat(sprintf("Median satisfaction: %.1f/10\n", stats$median))
cat(sprintf("Satisfaction ranges from %d to %d\n", stats$min, stats$max))
```

### Example 2: A/B Test with Automatic Report

```r
# A/B test data
ab_data <- data.frame(
  conversion_rate = c(0.12, 0.15, 0.11, 0.14, 0.13,  # Version A
                     0.18, 0.19, 0.17, 0.20, 0.18), # Version B
  version = c(rep("A", 5), rep("B", 5))
)

# Generate complete analysis report
create_analysis_report(
  data = ab_data,
  variable = "conversion_rate",
  group_var = "version",
  title = "Landing Page A/B Test Results"
)
```

### Example 3: Longitudinal Study (Paired Data)

```r
# Customer engagement before and after intervention
before <- c(45, 52, 48, 51, 49, 53, 47, 50)
after <- c(52, 58, 54, 57, 55, 60, 53, 56)

# Paired comparison (same customers measured twice)
result <- test_group_differences(
  before, after,
  paired = TRUE,
  alternative = "less"  # Testing if 'after' is greater
)

cat(result$interpretation)
```

## CRAN Compliance

This package is built to CRAN standards:

- ✅ Comprehensive documentation (roxygen2)
- ✅ Unit tests with >90% coverage (testthat)
- ✅ Vignettes for learning
- ✅ No external dependencies beyond base R recommendations
- ✅ Proper DESCRIPTION and NAMESPACE
- ✅ Examples in all exported functions
- ✅ Clean R CMD check with no errors, warnings, or notes

## For Peer Reviewers

If you're reviewing a paper that uses consumeR:

1. **Check the code**: All functions have readable source code with extensive comments
2. **Verify the analysis**: Run the same code to get identical results
3. **Understand the methods**: Every statistical decision is documented
4. **Review the assumptions**: All assumptions are explicitly stated
5. **Trust but verify**: Complete transparency enables thorough review

consumeR Roadmap: Method Coverage and Development Plan

consumeR is designed to support the dominant analytical workflows used in business, marketing, and consumer research, while enforcing transparent exclusions, integrated diagnostics, and standardized reporting.
The roadmap below outlines current support and planned extensions.

Current Support
Analytical Area	Typical Use Cases	Status	Notes
Data Cleaning & Exclusions	Surveys, experiments, archival datasets	✔ Implemented	Explicit exclusion rules and participant flow summaries
Missing Data Handling	Survey and panel data	✔ Implemented	Missing values preserved; handling specified downstream
Descriptive Statistics	All empirical studies	✔ Implemented	Reproducible summaries with consistent output
Group Comparisons (t-tests, ANOVA)	Experiments, lab and field studies	✔ Implemented	Assumption diagnostics and effect sizes included
Linear Regression (OLS)	Observational and experimental designs	✔ Implemented	Diagnostics integrated into analysis objects
Moderation (Interactions)	Theory-driven hypothesis tests	✔ Implemented	Explicit model specification and reporting
Correlation Analysis	Scale validation, exploratory analysis	✔ Implemented	Confidence intervals and transparent reporting
Scale Reliability (e.g., α, ω)	Measurement development	✔ Implemented	Reproducible scoring and report-ready summaries
Planned Extensions
Analytical Area	Typical Use Cases	Planned Status	Design Requirements
Exploratory Factor Analysis (EFA)	Scale development	🔜 Planned	Transparent factor selection and assumption reporting
Confirmatory Factor Analysis (CFA)	Measurement validation	🔜 Planned	Standardized fit indices and reporting outputs
Logistic Regression	Binary outcomes (choice, adoption)	🔜 Planned	Model-specific diagnostics and effect reporting
Count Models (Poisson, Negative Binomial)	Engagement and event frequency	🔜 Planned	Assumption checks and uncertainty reporting
Multilevel Models (Random Intercepts)	Nested data (teams, firms, time)	🔜 Planned	Explicit cluster structure and diagnostics
Regression-Based Mediation	Process and mechanism models	🔜 Planned	Transparent decomposition and reporting
Out of Scope (By Design)
Analytical Area	Reason
Machine Learning / Black-Box Prediction	Not aligned with inferential reporting goals
Highly Customized Bayesian Models	Requires domain-specific assumptions not yet supported
Bespoke Causal Inference Frameworks	Planned only if transparency constraints can be enforced
Design Principles Guiding the Roadmap

All current and future methods in consumeR must satisfy the same workflow constraints:

Explicit exclusion tracking

Integrated assumption diagnostics

Standardized, report-ready outputs

Methods that cannot be implemented while preserving these constraints are intentionally excluded.

Why this roadmap exists

consumeR is not a general-purpose modeling library. It is research infrastructure for business fields.
The roadmap reflects a commitment to expanding coverage of dominant analytical forms while maintaining transparency, interpretability, and pedagogical clarity.


## Contributing

Contributions are welcome! This package prioritizes:

- Transparency and readability above all else
- Clear documentation for non-programmers
- Comprehensive testing
- Real-world usability in academic research

## Getting Help

- **Function documentation**: `?function_name`
- **Vignette**: `vignette("getting-started", package = "consumeR")`
- **Issues**: [GitHub Issues](https://github.com/phdemotions/consumeR/issues)
- **Source code**: Browse the extensively commented source

## Citation

If you use consumeR in your research, please cite it:

```r
citation("consumeR")
```

## License

MIT + file LICENSE

---

**Built for researchers, by researchers. Making transparent analysis effortless.**
