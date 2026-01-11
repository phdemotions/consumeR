---
title: 'consumeR: Transparent analytical workflows for consumer research'
tags:
  - R
  - statistics
  - consumer research
  - reproducibility
  - transparency
authors:
  - name: Josh Gonzales
    orcid: 0000-0001-8633-3380
    affiliation: 1
    corresponding: true
affiliations:
  - name: Independent Researcher
    index: 1
date: 10 January 2026
bibliography: paper.bib
---

# Summary

Transparency and reproducibility are increasingly recognized as essential to credible quantitative research [@munafò2017manifesto; @simmons2011false]. In consumer research specifically, where experimental and survey designs predominate, researchers must document data exclusions, justify analytical choices, and verify statistical assumptions before interpreting results [@wasserstein2016asa; @deer2025marketing]. Yet standard statistical software often leaves these steps implicit, requiring researchers to manually document each decision, remember to check assumptions for every test, and cobble together multiple packages to complete a single analysis. `consumeR` addresses this workflow gap by providing structured, documented analysis pipelines that make analytical decisions explicit, assumption checking automatic, and exclusion tracking transparent—while maintaining code readability for researchers with varying programming experience.

# Statement of Need

Consumer researchers working in R face a common challenge: standard packages provide statistical tests but do not enforce or facilitate the documentation practices required for transparent reporting. A researcher conducting a simple between-subjects experiment must remember to: (1) document all participant exclusions with counts and reasons, (2) test normality assumptions before choosing parametric versus non-parametric tests, (3) check homogeneity of variance, (4) report effect sizes with confidence intervals, (5) justify the choice of statistical test, and (6) generate publication-ready text that synthesizes all of this information. Each step requires knowledge of which function to call from which package, how to interpret diagnostic output, and how to format results for a Methods or Results section. This burden is particularly acute for researchers still developing their programming skills [@shmueli2010explain].

`consumeR` addresses three specific gaps in the consumer research workflow:

1. **Structured exclusion tracking**: Following CONSORT-style reporting [@schulz2010consort], the package's `clean_survey_data()` function documents every exclusion step (pre-test removal, failed attention checks, unmet inclusion criteria) and generates summary statistics suitable for direct inclusion in a Methods section. This workflow is common in consumer research but not automated in existing R packages.

2. **Automatic assumption checking**: Statistical tests have assumptions (normality, homogeneity of variance, independence). Rather than requiring researchers to manually run diagnostic tests and interpret the results, `consumeR` functions automatically check assumptions and report violations clearly in the output object. For example, `test_group_differences()` automatically runs Shapiro-Wilk tests for normality and Levene's test for homogeneity, storing results in the output and switching to non-parametric alternatives when assumptions are violated (with clear documentation of the decision).

3. **Pedagogical design for learning**: Functions prioritize code readability and explicit documentation over computational efficiency. Every function includes extensive inline comments explaining the statistical logic, help files provide conceptual explanations alongside syntax, and error messages guide users toward solutions. This design supports researchers learning R while conducting rigorous analyses, addressing calls for more accessible statistical computing tools [@deer2025marketing].

The package does not introduce novel statistical methods. Instead, it structures existing, well-validated methods into workflows that align with transparency and reproducibility recommendations in recent methodological literature.

# Design and Architecture

`consumeR` extends the R statistical ecosystem rather than duplicating it. The package builds on established foundations:

- **Statistical computation**: Leverages `stats` (base R), `car`, `emmeans`, `lavaan`, `lme4`, and `psych` for core calculations
- **Data manipulation**: Uses `dplyr`, `tidyr`, and `tibble` from the tidyverse for consistent data structures
- **Effect sizes**: Integrates `effectsize` and `correlation` packages
- **Output format**: Returns tidy tibbles compatible with `broom`, `gt`, and `knitr` workflows

**Key architectural decisions**:

1. **Wrapper functions with added structure**: Rather than replacing existing packages, `consumeR` provides structured wrappers that enforce best practices. For example, `run_mlm()` wraps `lme4::lmer()` but adds automatic ICC calculation, assumption checking, and publication-ready output formatting.

2. **Explicit over implicit**: Functions make all analytical decisions visible. When `test_group_differences()` switches from a t-test to Mann-Whitney due to failed normality assumptions, this decision is documented in the output and explained in a message to the user.

3. **Reproducibility by default**: All functions that use random processes (bootstrap resampling, permutation tests) accept a `seed` parameter and document the random number generator state in output objects.

4. **Tidy return objects**: All analysis functions return structured lists with class attributes, enabling method dispatch for `print()`, `summary()`, and `plot()` generics. This makes results machine-readable for downstream analysis while maintaining human-readable formatting.

This architecture balances accessibility for novice users with extensibility for advanced users who may want to extract components for custom analyses.

# Functionality and Research Applications

`consumeR` provides functions across the consumer research workflow, organized by typical analysis progression:

**Data preparation**:
- `clean_survey_data()`: CONSORT-style exclusion tracking with participant flow documentation
- `import_research_data()`: Import from common formats (CSV, SPSS) with automatic variable type detection
- `score_composite()`: Create scale scores with metadata tracking and missing data rules

**Assumption testing and diagnostics**:
- `assumption_checks()`: Comprehensive regression diagnostics (normality, homoscedasticity, multicollinearity)
- `logistic_assumptions()`: Logistic regression diagnostics (separation, influential cases, linearity)
- `mlm_assumptions()`: Multilevel model diagnostics (level-1 and level-2 residual normality)

**Inferential analyses**:
- Group comparisons: `test_group_differences()`, `run_anova()`, `run_rm_anova()`, `emmeans_contrasts()`
- Regression: `analyze_regression()`, `run_logistic()`, `tidy_lm_robust()` (with HC3 robust standard errors)
- Mediation: `mediation_simple()`, `mediation_parallel()`, `mediation_serial()`, `moderated_mediation()`
- Factor analysis: `perform_efa()`, `run_cfa()`, with integrated diagnostics (KMO, Bartlett's test, fit indices)
- Multilevel models: `run_mlm()`, `icc_calculate()`, supporting random intercepts and slopes
- Structural equations: `run_sem()`, wrapping `lavaan` with streamlined output formatting

**Categorical and non-parametric methods**:
- `chisq_test()`, `fisher_exact_test()`, `mcnemar_test()` with effect sizes (Cramér's V, odds ratios)
- `mann_whitney_test()`, `kruskal_wallis_test()`, `wilcoxon_signed_rank_test()`, `friedman_test()`

All functions return effect sizes with confidence intervals following current reporting standards [@wasserstein2016asa], and many generate publication-ready text snippets that can be included directly in manuscripts.

**Example workflow**:

```r
library(consumeR)

# 1. Clean data with exclusion tracking
cleaned <- clean_survey_data(
  data = raw_survey,
  attention_checks = list(ac1 = list(var = "AC1", correct = 3)),
  id_var = "ResponseId"
)

# 2. Test hypothesis with automatic assumption checking
result <- test_group_differences(
  cleaned$clean_data$satisfaction[cleaned$clean_data$condition == "Treatment"],
  cleaned$clean_data$satisfaction[cleaned$clean_data$condition == "Control"]
)

# 3. View results with assumptions
print(result, show_assumptions = TRUE)
```

This workflow documents all analytical decisions and assumption checks without requiring the researcher to manually implement each step.

# Community and Development

The package follows open development practices:

- **Version control**: Full development history publicly available on GitHub
- **Testing**: Comprehensive test suite with >230 test cases using `testthat`
- **Continuous integration**: GitHub Actions CI tests package on macOS, Windows, and Linux with multiple R versions
- **Documentation**: All exported functions have complete help files with runnable examples
- **Contribution pathways**: Issues and pull requests welcome; code style guidelines documented

Installation is straightforward via GitHub:

```r
remotes::install_github("phdemotions/consumeR")
```

The package is designed for easy testing by new users through built-in example datasets (`consumer_survey`) that enable all examples to run without external data files.

# Impact and Adoption Potential

`consumeR` addresses a documented need in consumer research methodology: the gap between statistical best practices and practical implementation [@deer2025marketing]. By automating assumption checking, exclusion tracking, and effect size reporting, the package reduces the cognitive burden on researchers and decreases the likelihood of overlooking critical analytical steps.

The package is likely to be cited by researchers in consumer psychology, marketing, and related fields who value transparency and reproducibility. Its pedagogical design supports teaching statistical methods in graduate courses, and its emphasis on readable code facilitates peer review and replication efforts.

While the package is newly released, its design directly responds to methodological recommendations from recent high-profile publications in consumer research and psychological science [@deer2025marketing; @munafò2017manifesto], suggesting strong alignment with current community priorities.

# Acknowledgments

Development of this package was informed by ongoing discussions in the open science community regarding transparency and reproducibility in psychological and marketing research.

# References
