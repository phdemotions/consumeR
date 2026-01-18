# consumeR

<!-- badges: start -->
[![R-CMD-check](https://github.com/phdemotions/consumeR/workflows/R-CMD-check/badge.svg)](https://github.com/phdemotions/consumeR/actions)
<!-- badges: end -->

## Transparent Analytical Workflows for Consumer Research

`consumeR` is an R package designed for researchers in marketing and consumer psychology who want to conduct transparent, reproducible statistical analyses. It provides structured workflows that document data cleaning steps, check statistical assumptions, and generate clear reports—all while maintaining readability for reviewers and collaborators who may not be expert programmers.

### Who is this for?

consumeR supports the most common analytical workflows in business and consumer research, with planned extensions for multilevel models, measurement models, and generalized linear models. All methods—current and future—adhere to the same transparency and reporting constraints.

### Why consumeR?

This package is for researchers who:
- Need to document their analytical decisions clearly for peer review
- Want built-in assumption checking rather than manual validation
- Prefer readable code with explicit steps over complex one-liners
- Value transparency and reproducibility in their research workflow

**Not required**: Deep R programming expertise. Functions are designed to be accessible to researchers still learning R.

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("phdemotions/consumeR")
```

## Quick Start

### Not Sure Which Test to Use?

The package includes a helper for beginners:

```r
library(consumeR)
data(consumer_survey)

# Get personalized test recommendation
quick_test_help(
  data = consumer_survey,
  outcome = "spending",        # What you're measuring
  predictor = "flyer_group"    # What you think affects it
)

# This will:
# 1. Tell you which test to use
# 2. Explain WHY that test is appropriate
# 3. Give you ready-to-run code
# 4. List the next steps
```

### Basic Examples

The package includes a built-in example dataset (`consumer_survey`) for trying out functions:

```r
library(consumeR)

# Load built-in example data
data(consumer_survey)
head(consumer_survey)

# Example 1: Descriptive statistics
stats <- calculate_summary_stats(consumer_survey$satisfaction)
print(stats)

# Example 2: Compare two groups
library(dplyr)
flyer_group <- consumer_survey %>% filter(flyer_group == "Got Flyer")
no_flyer_group <- consumer_survey %>% filter(flyer_group == "No Flyer")

result <- test_group_differences(
  flyer_group$spending,
  no_flyer_group$spending
)
print(result)

# Example 3: Check assumptions explicitly
print(result, show_assumptions = TRUE)
```

### Data Cleaning with Exclusion Tracking

One of the package's core features is transparent participant exclusion tracking:

```r
# Start with raw data (your actual survey data)
# This example shows the structure; you would use your own data
cleaned <- clean_survey_data(
  data = raw_data,
  attention_checks = list(
    ac1 = list(var = "AttentionCheck1", correct = "correct_answer")
  ),
  id_var = "ResponseId"
)

# View exclusion summary
print(cleaned)

# Access cleaned data
final_data <- cleaned$clean_data
```

## Feature Overview

### Core Capabilities

consumeR provides comprehensive support for common research workflows:

| Analysis Category | Included Methods |
|:-----------------|:-----------------|
| **Data Management** | CSV/SPSS import, variable type checking, name standardization |
| **Data Cleaning** | Exclusion tracking, attention check validation, missing data reports |
| **Descriptive Statistics** | Summary statistics, correlation matrices |
| **Group Comparisons** | t-tests, ANOVA, repeated measures ANOVA, post-hoc contrasts |
| **Regression** | OLS with diagnostics, robust standard errors, logistic regression |
| **Reliability & Validity** | Cronbach's alpha, composite reliability, AVE |
| **Factor Analysis** | EFA with diagnostics, CFA with fit indices |
| **Mediation & Moderation** | Simple/parallel/serial mediation, simple slopes, Johnson-Neyman |
| **Multilevel Models** | Random-intercept models, ICC, assumption diagnostics |
| **Structural Equation Modeling** | Path analysis, indirect effects, model comparison |
| **Categorical Analysis** | Chi-square, Fisher's exact, McNemar's test, odds ratios |
| **Non-Parametric Tests** | Mann-Whitney, Kruskal-Wallis, Wilcoxon, Friedman |
| **Effect Sizes** | Cohen's d, Cramér's V, partial η², odds ratios with CIs |

### Potential Future Extensions

Features that could be added based on user needs:

- Count models (Poisson, negative binomial regression)
- Advanced SEM features (latent growth models, MIMIC models)
- Additional multilevel extensions (cross-classified models, growth curves)
- Propensity score matching for observational studies
- Sensitivity analyses for missing data

**Community-driven**: Future development prioritizes features requested by active users.

### Out of Scope

The following are explicitly **not planned** as they fall outside the package's design goals:

- Machine learning / predictive modeling (use `tidymodels`, `caret`, or `mlr3`)
- Bayesian inference (use `brms`, `rstanarm`, or `rethinking`)
- Time series analysis (use `forecast` or `fable`)
- Spatial statistics (use `sf` or `spatstat`)
- Network analysis (use `igraph` or `tidygraph`)

**Design philosophy**: consumeR focuses on transparent, assumption-based inference common in experimental and survey-based consumer research.

## Key Features

### 1. Explicit Assumption Checking

Instead of manually running diagnostic tests, consumeR functions check assumptions automatically and report results clearly:

```r
result <- analyze_regression(df, outcome ~ predictor1 + predictor2)

# Assumptions are tested and reported
print(result$assumptions)
```

### 2. Readable Code

Functions prioritize clarity over brevity. Open any function to see what it does:

```r
# View source code (all extensively commented)
calculate_summary_stats
```

### 3. Documentation for Learning

Help files include detailed explanations aimed at researchers learning statistics:

```r
?test_group_differences
?calculate_alpha
```

## Documentation

- **Function documentation**: Use `?function_name` in R
- **Vignette**: `vignette("getting-started", package = "consumeR")`
- **Examples**: All functions include runnable examples
- **Source code**: Extensively commented for learning

## Example: Complete Workflow

Here's a minimal analysis workflow from start to finish:

```r
library(consumeR)
data(consumer_survey)

# 1. Describe your data
summary_stats <- calculate_summary_stats(consumer_survey$spending)
print(summary_stats)

# 2. Test a hypothesis
library(dplyr)
treatment <- consumer_survey %>%
  filter(flyer_group == "Got Flyer") %>%
  pull(spending)

control <- consumer_survey %>%
  filter(flyer_group == "No Flyer") %>%
  pull(spending)

test_result <- test_group_differences(treatment, control)
print(test_result)

# 3. Check assumptions
print(test_result, show_assumptions = TRUE)

# 4. Calculate effect size
# (automatically included in test_result)
test_result$effect_size
```

## Philosophy

consumeR is built on these principles:

1. **Transparency**: Every analytical decision should be explicit and documented
2. **Assumption-based inference**: Statistical tests have assumptions; check them
3. **Accessibility**: Code should be readable by reviewers with varied programming experience
4. **Reproducibility**: Clear workflows enable exact replication
5. **Learning-friendly**: Documentation teaches concepts, not just syntax

## Citation

If you use consumeR in your research, please cite it:

```r
citation("consumeR")
```

**Suggested citation:**

> Gonzales, J. (2026). *consumeR: Transparent and Reproducible Consumer Research Analysis*. R package version 0.1.0. https://github.com/phdemotions/consumeR

Once published, a Zenodo DOI will be available for archival citation.

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](.github/CONTRIBUTING.md) for detailed guidelines on:

- Reporting bugs and suggesting enhancements
- Submitting pull requests
- Development setup and testing
- Code style requirements

For quick contributions:
- Prioritize readability and documentation
- Include tests for new functions
- Follow the existing code style (fully qualified function calls, roxygen2 docs)
- Run `devtools::check()` before submitting

## Getting Help

- **Issues**: [GitHub Issues](https://github.com/phdemotions/consumeR/issues)
- **Questions**: Open a discussion on GitHub
- **Function help**: `?function_name` in R

## License

MIT License. See LICENSE file for details.
