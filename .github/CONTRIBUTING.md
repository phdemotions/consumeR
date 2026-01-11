# Contributing to consumeR

Thank you for your interest in contributing to consumeR! This document provides guidelines for contributing to the project.

## How to Contribute

### Reporting Bugs

If you find a bug, please open an issue on GitHub with:
- A clear, descriptive title
- Steps to reproduce the issue
- Expected vs. actual behavior
- Your R version and package version
- A minimal reproducible example (if applicable)

### Suggesting Enhancements

Enhancement suggestions are welcome! Please open an issue describing:
- The proposed feature or improvement
- Why it would be useful for consumer research workflows
- Example use cases

### Pull Requests

We welcome pull requests for bug fixes and enhancements. Please:

1. **Fork the repository** and create a new branch for your changes
2. **Follow the existing code style**:
   - Use fully qualified function calls (e.g., `dplyr::mutate()`)
   - Include roxygen2 documentation for new functions
   - Follow tidyverse style guide conventions
3. **Add tests** for new functionality in `tests/testthat/`
4. **Update documentation** if you change function behavior
5. **Run R CMD check** to ensure the package builds without errors
6. **Write clear commit messages** describing what changed and why

### Code of Conduct

This project follows standard open source community guidelines:
- Be respectful and constructive in discussions
- Focus on what's best for the community and the project
- Accept constructive criticism gracefully

## Development Setup

```r
# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2"))

# Clone the repository
# git clone https://github.com/phdemotions/consumeR.git
# cd consumeR

# Load the package for development
devtools::load_all()

# Run tests
devtools::test()

# Check package
devtools::check()
```

## Questions?

If you have questions about contributing, feel free to:
- Open an issue for discussion
- Contact the maintainer at joshgonzalesphd@gmail.com

## License

By contributing to consumeR, you agree that your contributions will be licensed under the MIT License.
