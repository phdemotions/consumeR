# consumeR

<!-- badges: start -->
[![R-CMD-check](https://github.com/phdemotions/consumeR/workflows/R-CMD-check/badge.svg)](https://github.com/phdemotions/consumeR/actions)
<!-- badges: end -->

## Transparent Analytical Workflows for Consumer Research

`consumeR` is an R package designed for researchers in marketing and consumer psychology who want to conduct transparent, reproducible statistical analyses. It provides structured workflows that document data cleaning steps, check statistical assumptions, and generate clear reports—all while maintaining readability for reviewers and collaborators who may not be expert programmers.

### Who is this for?

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

## Roadmap

### Currently Supported

consumeR provides functions for transparent analytical workflows:

| Category | Functions |
|----------|-----------|
| **Data Management** | Import (CSV, SPSS), variable checking, name cleaning |
| **Data Cleaning** | Exclusion tracking, attention check validation, missing data reporting |
| **Descriptive Statistics** | Summary statistics, correlation matrices |
| **Group Comparisons** | t-tests, ANOVA, repeated measures ANOVA, post-hoc contrasts |
| **Regression** | OLS with assumption checks, robust standard errors, logistic regression |
| **Reliability & Validity** | Cronbach's alpha, composite reliability, AVE |
| **Factor Analysis** | Exploratory (EFA) with diagnostics, Confirmatory (CFA) with fit indices |
| **Mediation & Moderation** | Simple, parallel, serial, and moderated mediation; simple slopes; Johnson-Neyman |
| **Multilevel Models** | Random-intercept models, ICC calculation, assumption diagnostics |
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

**Design philosophy

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

Once the Zenodo DOI is minted, citations will include:

> Gonzales, J. (2026). consumeR: Transparent and Reproducible Consumer Research Analysis. R package version 0.1.0. DOI: 10.5281/zenodo.XXXXXXX

## Contributing

Contributions are welcome! Please:

- Prioritize readability and documentation
- Include tests for new functions
- Follow the existing code style
- Update documentation

## Getting Help

- **Issues**: [GitHub Issues](https://github.com/phdemotions/consumeR/issues)
- **Questions**: Open a discussion on GitHub
- **Function help**: `?function_name` in R

## License

MIT License. See LICENSE file for details.
