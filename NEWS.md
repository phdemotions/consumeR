# consumeR 0.1.0.9000 (Development)

## Internal Improvements

* Consolidated all p-value formatting to `format_p()` utility
  - Removed duplicate `format_pvalue()` implementation from publication_text.R
  - Standardized on 3-decimal APA formatting throughout package
  - Improved consistency and maintainability of publication text generation
  - All p-values now use comprehensive validation from utilities.R

# consumeR 0.1.0

## Initial Release

This is the first release of consumeR, a package designed for transparent and reproducible consumer research analysis.

### Main Features

* `calculate_summary_stats()`: Calculate comprehensive descriptive statistics with transparent, step-by-step documentation
* `test_group_differences()`: Compare two groups using appropriate statistical tests with plain English interpretations
* `create_analysis_report()`: Generate complete analysis reports optimized for peer review

### Design Philosophy

* Every function includes extensive inline comments explaining each step
* All statistical outputs include human-readable interpretations
* Automatic handling of missing values with transparent reporting
* Input validation with clear, informative error messages
* Complete documentation with multiple examples per function

### Documentation

* Comprehensive vignette: "Getting Started with consumeR"
* Full roxygen2 documentation for all exported functions
* Detailed README with philosophy and examples
* Extensive unit tests ensuring reliability

### CRAN Compliance

* No errors, warnings, or notes in R CMD check
* >90% test coverage
* All exported functions have examples
* Proper DESCRIPTION and NAMESPACE files
* Depends only on base R packages (stats, utils, graphics)

### For Peer Review

This package is specifically designed to facilitate peer review by:

* Making all code easily readable by non-programmers
* Providing complete transparency in all calculations
* Enabling easy replication of results
* Documenting all assumptions and decisions explicitly
