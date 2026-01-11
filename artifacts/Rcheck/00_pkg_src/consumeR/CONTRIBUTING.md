# Contributing to consumeR

Thank you for your interest in contributing to consumeR! This document provides guidelines for contributing to the package.

## Ways to Contribute

### Reporting Bugs

If you find a bug, please open an issue on GitHub with:

1. **Reproducible Example:** Minimal code that reproduces the problem
2. **Expected Behavior:** What you thought should happen
3. **Actual Behavior:** What actually happened
4. **System Information:**
   - R version (from `sessionInfo()`)
   - Operating system
   - consumeR version

**Example:**
```r
# Reproducible example
library(consumeR)
data <- data.frame(x = c("A", "B"), y = c(1, 2))
chisq_test(data, x = "x", y = "y")  # Error occurs here

# R version
sessionInfo()
```

### Suggesting Enhancements

We welcome suggestions for new features or improvements! Please:

1. Check existing issues to avoid duplicates
2. Describe the use case and why it's needed
3. Provide examples of how it would be used
4. Consider whether it fits the package scope (consumer research)

### Contributing Code

We welcome pull requests! Here's how:

#### 1. Fork and Clone
```bash
# Fork the repo on GitHub, then:
git clone https://github.com/YOUR-USERNAME/consumeR.git
cd consumeR
```

#### 2. Create a Branch
```bash
git checkout -b feature/your-feature-name
```

#### 3. Make Your Changes

**Code Style:**
- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use `snake_case` for function and variable names
- Add comments explaining complex logic
- Use roxygen2 for documentation

**Testing:**
- Add tests for new features in `tests/testthat/`
- Ensure existing tests still pass: `devtools::test()`
- Aim for good test coverage of your additions

**Documentation:**
- Document all exported functions with roxygen2
- Include `@examples` with runnable code
- Update README.md if adding major features
- Add `@param` for all function parameters
- Add `@return` describing return value

**Example Function:**
```r
#' Short Description (One Line)
#'
#' Longer description explaining what the function does,
#' when to use it, and any important details.
#'
#' @param data Data frame containing variables
#' @param x Character. Name of first variable
#' @param y Character. Name of second variable
#'
#' @return A list with class "my_result" containing:
#'   \describe{
#'     \item{statistic}{Test statistic value}
#'     \item{p_value}{P-value from test}
#'     \item{interpretation}{Plain English explanation}
#'   }
#'
#' @examples
#' df <- data.frame(x = c("A", "B"), y = c(1, 2))
#' result <- my_function(df, x = "x", y = "y")
#' print(result)
#'
#' @export
my_function <- function(data, x, y) {
  # Your code here
}
```

#### 4. Check Your Changes

Before submitting, run:

```r
# Load development version
devtools::load_all()

# Run tests
devtools::test()

# Check documentation
devtools::document()

# Run R CMD check
devtools::check()
```

Fix any errors, warnings, or notes.

#### 5. Commit and Push

```bash
git add .
git commit -m "Brief description of changes"
git push origin feature/your-feature-name
```

#### 6. Submit Pull Request

1. Go to your fork on GitHub
2. Click "New Pull Request"
3. Describe your changes:
   - What problem does it solve?
   - How did you test it?
   - Any breaking changes?

### Contributing Documentation

Improvements to documentation are always welcome:

- Fix typos or unclear explanations
- Add examples to function help
- Improve vignettes
- Enhance README

Small documentation changes can be made directly via GitHub's web interface.

## Development Guidelines

### Package Philosophy

consumeR is designed for **transparency and accessibility**:

1. **User-Friendly:** Functions should be easy to use for researchers without extensive programming experience
2. **Transparent:** All analytical decisions should be documented in output
3. **Reproducible:** Random processes should accept seed parameters
4. **Educational:** Error messages should guide users toward solutions
5. **Publication-Ready:** Output should be suitable for direct inclusion in papers

### Function Design Principles

**DO:**
- ✅ Provide helpful error messages that explain what's wrong and how to fix it
- ✅ Return structured objects with class attributes for method dispatch
- ✅ Include interpretation text in output
- ✅ Check assumptions automatically when applicable
- ✅ Document all parameters with examples
- ✅ Use message() for informational output, warning() for concerns

**DON'T:**
- ❌ Silently fail or return NA without explanation
- ❌ Use print() for critical information (use message() or return it)
- ❌ Require users to remember to check assumptions manually
- ❌ Use technical jargon without explanation in messages
- ❌ Make functions that only work in specific edge cases

### Testing Guidelines

**Required for New Features:**
- Test normal usage with realistic data
- Test edge cases (missing data, single observation, etc.)
- Test error handling (wrong input types, missing variables)
- Test return value structure and types

**Example Test:**
```r
test_that("my_function works with basic input", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c(1, 2, 3, 4)
  )

  result <- my_function(df, x = "x", y = "y")

  expect_s3_class(result, "my_result")
  expect_type(result$statistic, "double")
  expect_true(result$statistic >= 0)
  expect_true(result$p_value >= 0 && result$p_value <= 1)
})

test_that("my_function validates inputs", {
  df <- data.frame(x = c("A", "B"), y = c(1, 2))

  expect_error(my_function(df, x = "missing", y = "y"), "not found")
  expect_error(my_function("not a df", x = "x", y = "y"), "data frame")
})
```

## Community Standards

### Code of Conduct

We are committed to providing a welcoming and inclusive environment:

- Use welcoming and inclusive language
- Be respectful of differing viewpoints and experiences
- Accept constructive criticism gracefully
- Focus on what is best for the community
- Show empathy towards other community members

### Getting Help

**For Users:**
- Check function documentation: `?function_name`
- Read the getting-started vignette
- Search existing GitHub issues
- Open a new issue if your question isn't answered

**For Contributors:**
- Discuss major changes in an issue before implementing
- Ask questions in pull request comments
- Email maintainer: joshgonzalesphd@gmail.com

### Review Process

Pull requests are reviewed for:

1. **Correctness:** Does the code work as intended?
2. **Tests:** Are there tests that pass?
3. **Documentation:** Is it documented clearly?
4. **Style:** Does it follow package conventions?
5. **Scope:** Does it fit the package purpose?

Reviews typically happen within a few days. Be patient and responsive to feedback!

## Recognition

Contributors will be:

- Added to the package DESCRIPTION as contributors (for substantial contributions)
- Acknowledged in release notes
- Thanked in commit messages

## Questions?

Feel free to:
- Open an issue for questions
- Email the maintainer: joshgonzalesphd@gmail.com
- Start a discussion on GitHub Discussions (if enabled)

Thank you for helping make consumeR better! 🎉
