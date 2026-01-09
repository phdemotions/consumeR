# Setting Up Function Reference Documentation

This guide will help you create individual reference pages for each function in the consumeR package.

## Quick Start (Automated)

Run the provided setup script:

```r
# In R or RStudio, from the package root directory:
source("setup_documentation.R")
```

This will automatically:
- ✅ Install required packages (devtools, roxygen2, pkgdown)
- ✅ Generate .Rd files from your roxygen2 comments
- ✅ Build the pkgdown website
- ✅ Show you what to commit to git

## Manual Steps (If You Prefer)

### 1. Install Required Packages

```r
install.packages(c("devtools", "roxygen2", "pkgdown"))
```

### 2. Generate Documentation Files

From the package root directory in R:

```r
devtools::document()
```

This creates the `man/` directory with `.Rd` files for each function that has roxygen2 documentation (the `#'` comments in your R files).

### 3. Build the Website

```r
pkgdown::build_site()
```

This creates the `docs/` directory with your complete website, including individual reference pages for each function.

### 4. Preview Locally

Open `docs/index.html` in your browser to preview the site.

Check the function reference pages at `docs/reference/index.html`.

### 5. Commit and Push

```bash
git add man/
git add NAMESPACE
git add docs/
git commit -m "Add function reference documentation"
git push
```

## What Gets Generated

After running these steps, you'll have:

### In `man/` directory:
- `calculate_summary_stats.Rd` - Documentation for calculate_summary_stats()
- `test_group_differences.Rd` - Documentation for test_group_differences()
- `create_analysis_report.Rd` - Documentation for create_analysis_report()
- ... (one .Rd file for each exported function)

### In `docs/reference/` directory:
- Individual HTML pages for each function
- Example: `docs/reference/calculate_summary_stats.html`

### Live URLs (after pushing to GitHub):
- Main site: https://phdemotions.github.io/consumeR/
- Function index: https://phdemotions.github.io/consumeR/reference/
- Individual functions:
  - https://phdemotions.github.io/consumeR/reference/calculate_summary_stats.html
  - https://phdemotions.github.io/consumeR/reference/test_group_differences.html
  - etc.

## How It Works

1. **Roxygen2 Comments** (`#'` in your R files) → contain function documentation
2. **devtools::document()** → converts roxygen2 comments to `.Rd` files in `man/`
3. **pkgdown::build_site()** → converts `.Rd` files to HTML pages in `docs/`
4. **GitHub Pages** → serves `docs/` directory as a website

## Your Current Setup

✅ You already have:
- Excellent roxygen2 documentation in your R files
- `_pkgdown.yml` configuration file
- GitHub Pages URL configured

❌ You just need to generate:
- `man/` directory (documentation files)
- `docs/` directory (website HTML)

## Troubleshooting

### If devtools::document() fails:
- Check that all your roxygen2 comments are properly formatted
- Make sure all `@param` names match your function parameters
- Verify that `@export` is present for functions you want to be public

### If pkgdown::build_site() fails:
- Run `devtools::document()` first
- Check that `_pkgdown.yml` is valid YAML
- Make sure all referenced functions in `_pkgdown.yml` exist

### If functions don't appear on the website:
- Ensure functions have `@export` tag in their roxygen2 comments
- Make sure functions are listed in `_pkgdown.yml` under the `reference:` section
- Run `devtools::document()` again

## Updating Documentation

Whenever you change roxygen2 comments in your R files:

```r
# Regenerate documentation
devtools::document()

# Rebuild website
pkgdown::build_site()

# Commit and push
# git add man/ docs/
# git commit -m "Update documentation"
# git push
```

## Additional Resources

- [roxygen2 documentation](https://roxygen2.r-lib.org/)
- [pkgdown documentation](https://pkgdown.r-lib.org/)
- [R Packages book](https://r-pkgs.org/)
