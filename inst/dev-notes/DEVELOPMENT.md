# Development Guide for consumeR

This document provides instructions for developing, testing, and preparing consumeR for CRAN submission.

## Setup

### 1. Install Required Packages

```r
install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown"))
```

### 2. Load the Package for Development

```r
library(devtools)
setwd("path/to/consumeR")
load_all()  # Load the package for testing
```

## Documentation

### Generate Documentation from Roxygen Comments

The package uses roxygen2 for documentation. After editing any roxygen comments in R files:

```r
library(roxygen2)
devtools::document()  # This updates NAMESPACE and man/ files
```

### Build Vignettes

```r
devtools::build_vignettes()
```

### Preview Documentation

```r
?calculate_summary_stats
?test_group_differences
?create_analysis_report
```

## Testing

### Run All Tests

```r
devtools::test()
```

### Check Test Coverage

```r
library(covr)
package_coverage()
```

Target: >90% coverage for CRAN submission

### Run Individual Test Files

```r
testthat::test_file("tests/testthat/test-summary_stats.R")
```

## Quality Checks

### Run R CMD check

This is the most important check for CRAN compliance:

```r
devtools::check()
```

**Goal**: 0 errors, 0 warnings, 0 notes

### Check on Different Platforms

Use GitHub Actions or rhub:

```r
# Check on CRAN platforms
library(rhub)
check_for_cran()

# Or use GitHub Actions (recommended)
# See .github/workflows/R-CMD-check.yaml
```

### Spell Check

```r
devtools::spell_check()
```

### Check Examples

Make sure all examples run:

```r
devtools::run_examples()
```

## Building the Package

### Build Source Package

```r
devtools::build()
```

This creates a `.tar.gz` file suitable for CRAN submission.

### Install Package Locally

```r
devtools::install()
```

## Pre-CRAN Submission Checklist

Before submitting to CRAN, ensure:

- [ ] `devtools::check()` returns 0 errors, 0 warnings, 0 notes
- [ ] All examples run successfully
- [ ] All tests pass
- [ ] Test coverage >90%
- [ ] Version number updated in DESCRIPTION
- [ ] NEWS.md updated with changes
- [ ] cran-comments.md updated
- [ ] Email address in DESCRIPTION is valid
- [ ] URLs in DESCRIPTION are valid
- [ ] LICENSE file is correct
- [ ] README.md is current
- [ ] Vignettes build successfully
- [ ] All functions have examples
- [ ] No references to local file paths
- [ ] No `\dontrun{}` in examples (use `\donttest{}` if needed)

## CRAN Submission Process

### 1. Final Check

```r
devtools::check(cran = TRUE)
devtools::build()
```

### 2. Submit to CRAN

Two options:

**Option A: Using devtools**
```r
devtools::submit_cran()
```

**Option B: Manual submission**
1. Build the package: `devtools::build()`
2. Go to https://cran.r-project.org/submit.html
3. Upload the `.tar.gz` file
4. Fill in maintainer information
5. Submit

### 3. Respond to CRAN Feedback

CRAN will likely have feedback. Common issues:

- **Email confirmation**: You'll receive an email asking to confirm submission
- **CRAN comments**: Reviewers may request changes
- **Timing**: Usually get response within 1-7 days

## Common Development Tasks

### Adding a New Function

1. Create the R file in `R/` directory
2. Add roxygen2 documentation
3. Export if public: `#' @export`
4. Add imports if needed: `#' @importFrom package function`
5. Create tests in `tests/testthat/test-newfunction.R`
6. Add examples to documentation
7. Run `devtools::document()` to update NAMESPACE
8. Run `devtools::check()` to verify

### Updating a Function

1. Modify the R file
2. Update roxygen documentation if needed
3. Update tests if behavior changed
4. Update NEWS.md with changes
5. Run `devtools::check()`

### Adding Dependencies

Only if absolutely necessary (consumeR minimizes dependencies):

1. Add to DESCRIPTION under `Imports:` or `Suggests:`
2. Use `@importFrom` in roxygen comments
3. Run `devtools::document()`

## Transparency Best Practices

Since consumeR is focused on transparency, follow these guidelines:

### Code Style

```r
# Good: Clear, step-by-step comments
# Step 1: Remove missing values
# We remove NA values to ensure all calculations work properly
data_clean <- data[!is.na(data)]

# Step 2: Calculate the mean
# Mean is the sum of all values divided by the count
mean_value <- sum(data_clean) / length(data_clean)
```

### Documentation

- Write for reviewers who may not be R experts
- Explain WHY, not just WHAT
- Include multiple examples showing different use cases
- Provide plain English interpretations

### Error Messages

```r
# Good: Clear, helpful error message
if (!is.numeric(data)) {
  stop("Error: 'data' must be a numeric vector. ",
       "You provided: ", class(data)[1])
}

# Bad: Cryptic error
if (!is.numeric(data)) stop("Invalid input")
```

## Version Numbering

Follow semantic versioning (MAJOR.MINOR.PATCH):

- **MAJOR** (1.0.0): Incompatible API changes
- **MINOR** (0.1.0): New functionality, backwards compatible
- **PATCH** (0.0.1): Bug fixes, backwards compatible

Current version: 0.1.0 (initial release)

## Getting Help

- R Packages book: https://r-pkgs.org/
- CRAN Repository Policy: https://cran.r-project.org/web/packages/policies.html
- Writing R Extensions: https://cran.r-project.org/doc/manuals/r-release/R-exts.html

## Contact

For questions about package development, see the repository issues page.
