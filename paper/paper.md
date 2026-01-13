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

# State of the field

Existing R packages provide statistical functions but not integrated transparency workflows. Base R `stats`, `psych` [@revelle2023psych], `lavaan` [@rosseel2012lavaan], and `lme4` [@bates2015lme4] excel at specific analyses but do not enforce assumption checking, exclusion tracking, or transparent reporting. Recent packages like `report` [@makowski2023report] and `easystats` [@lüdecke2022easystats] address post-analysis formatting but not workflow structure, exclusion documentation, or pedagogical scaffolding.

`consumeR` fills this gap with domain-specific, end-to-end workflow support for consumer research (surveys, experiments, mediation/moderation). Rather than contributing to general-purpose packages, it addresses consumer research's specific needs: CONSORT-style exclusion tracking, attention check documentation, and accessible workflows for researchers with varying programming skills. Its architectural contribution—organizing existing methods into transparent, reproducible pipelines—requires coordinated integration across multiple upstream packages, justifying a standalone implementation.

# Software Design

`consumeR` extends rather than duplicates the R ecosystem, building on `stats`, `car`, `emmeans`, `lavaan`, `lme4`, `psych`, tidyverse packages, and `effectsize`. The package provides structured wrappers that enforce best practices: functions automatically check assumptions and document violations (e.g., `test_group_differences()` switches from t-test to Mann-Whitney when normality fails, with clear reporting), return tidy tibbles compatible with `broom`/`knitr` workflows, and ensure reproducibility through `seed` parameters for random processes. This architecture prioritizes explicit documentation of analytical decisions over implicit automation, making the analysis workflow transparent while remaining accessible to users with varying programming experience.

# Functionality

`consumeR` provides functions across the consumer research workflow: data preparation (`clean_survey_data()` for CONSORT-style exclusion tracking, `score_composite()` for scale creation), assumption testing (`assumption_checks()`, `logistic_assumptions()`, `mlm_assumptions()`), and inferential analyses including group comparisons, regression (linear, logistic, multilevel), mediation/moderation models, factor analysis (EFA/CFA), and structural equation modeling. All functions return effect sizes with confidence intervals and support reproducible workflows through explicit documentation of analytical decisions. The package is available via GitHub (`remotes::install_github("phdemotions/consumeR")`) with comprehensive documentation and test coverage.

# Research Impact Statement

`consumeR` addresses a documented need in consumer research methodology: the gap between statistical best practices and practical implementation [@deer2025marketing]. By automating assumption checking, exclusion tracking, and effect size reporting, the package reduces the cognitive burden on researchers and decreases the likelihood of overlooking critical analytical steps.

The package is likely to be cited by researchers in consumer psychology, marketing, and related fields who value transparency and reproducibility. Its pedagogical design supports teaching statistical methods in graduate courses, and its emphasis on readable code facilitates peer review and replication efforts.

While the package is newly released, its design directly responds to methodological recommendations from recent high-profile publications in consumer research and psychological science [@deer2025marketing; @munafò2017manifesto], suggesting strong alignment with current community priorities.

# AI usage disclosure

Generative AI tools (ChatGPT, Claude) were used during development to assist with code documentation, error message phrasing, and initial drafts of help file examples. All AI-generated content was reviewed, tested, and revised by the author. Core software architecture, statistical implementations, and design decisions were made without AI assistance. This paper was written by the author without AI assistance.

# Acknowledgments

Development of this package was informed by ongoing discussions in the open science community regarding transparency and reproducibility in psychological and marketing research.

# References
