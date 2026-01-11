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
affiliations:
  - name: Independent Researcher
    index: 1
date: 10 January 2026
bibliography: paper.bib
---

# Summary

Transparency and reproducibility are increasingly recognized as essential to credible quantitative research [@munafò2017manifesto; @simmons2011false]. In consumer research specifically, where experimental and survey designs predominate, researchers must document data exclusions, justify analytical choices, and verify statistical assumptions [@wasserstein2016asa]. Yet standard statistical software often leaves these steps implicit, requiring researchers to manually document each decision and assumption check. `consumeR` addresses this gap by providing structured workflows that make analytical decisions explicit, assumption checking automatic, and exclusion tracking transparent—while maintaining code readability for researchers with varying programming experience.

# Statement of Need

Consumer researchers working in R face a common challenge: standard packages provide statistical tests but do not enforce or facilitate the documentation practices required for transparent reporting. Researchers must manually track participant exclusions, remember to check assumptions for each test, and document every analytical decision for peer review. This burden is particularly acute for researchers still developing their programming skills, who may struggle to implement best practices while learning statistical software.

`consumeR` addresses three specific needs in this workflow:

1. **Structured exclusion tracking**: Following CONSORT-style reporting [@schulz2010consort], the package documents every data exclusion step (pre-test removal, attention check failures, inclusion criteria) and generates summary statistics for Methods sections.

2. **Automatic assumption checking**: Statistical tests have assumptions (normality, homogeneity of variance, independence). Rather than requiring researchers to manually run diagnostic tests, `consumeR` functions check assumptions automatically and report violations clearly, enabling informed decisions about parametric versus non-parametric approaches.

3. **Pedagogical design**: Functions prioritize readability and explicit steps over computational efficiency. Code is extensively commented, help files explain statistical concepts, and error messages guide users toward solutions. This design supports researchers learning R while conducting rigorous analyses.

The package does not introduce novel statistical methods. Instead, it structures existing methods (t-tests, ANOVA, regression, reliability analysis) into workflows that align with transparency recommendations in recent methodological literature [@shmueli2010explain; @deer2025marketing].

# Functionality

`consumeR` provides functions across the consumer research workflow:

- **Data cleaning**: `clean_survey_data()` tracks all exclusions (pre-test, attention checks, inclusion criteria) and generates CONSORT-style flow diagrams. Researchers document why each participant was excluded.

- **Assumption checking**: Functions like `test_group_differences()` and `analyze_regression()` automatically test assumptions (normality via Shapiro-Wilk, homogeneity via Levene's test) and report results in output objects. Users can display assumption checks with `print(result, show_assumptions = TRUE)`.

- **Effect sizes with interpretation**: All inferential tests return standardized effect sizes (Cohen's d for mean differences, partial η² for ANOVA, Cramér's V for categorical associations) with qualitative interpretations following Cohen's conventions.

- **Transparent calculations**: Functions use explicit, documented calculations rather than opaque wrappers. Researchers can view source code to understand exactly what statistical procedures are applied.

The package focuses on frequentist inference common in experimental consumer research: group comparisons (t-tests, ANOVA), correlations, reliability (Cronbach's α, composite reliability, AVE), and basic moderation analysis. It explicitly does not cover machine learning, Bayesian methods, or advanced structural equation modeling, maintaining a focused scope on assumption-based inference.

# Acknowledgments

Development of this package was informed by ongoing discussions in the open science community regarding transparency and reproducibility in psychological and marketing research.

# References
