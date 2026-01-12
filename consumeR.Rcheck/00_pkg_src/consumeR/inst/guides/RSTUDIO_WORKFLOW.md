# RStudio Package Development Workflow

This guide shows you how to work with the consumeR package in RStudio.

## Opening the Project

1. In RStudio: **File → Open Project...**
2. Navigate to the consumeR folder
3. Select `consumeR.Rproj`

RStudio will now recognize this as an R package and enable special package development features!

## What You Get with the .Rproj File

### Build Menu
You'll see a **Build** tab in RStudio with buttons for:
- **Install and Restart** - Install the package and load it
- **Check** - Run R CMD check on your package
- **Test** - Run all tests
- **Build Source Package** - Create a .tar.gz file
- **Build Binary Package** - Create a binary package

### Keyboard Shortcuts
- **Ctrl/Cmd + Shift + B** - Install and Restart
- **Ctrl/Cmd + Shift + E** - Check Package
- **Ctrl/Cmd + Shift + T** - Run Tests
- **Ctrl/Cmd + Shift + D** - Document (runs roxygen2)

### Automatic Roxygen2 Integration
When you build/install, RStudio will automatically run `devtools::document()` to update your documentation!

## Recommended Workflow

### 1. Initial Setup (First Time)

Open `consumeR.Rproj` in RStudio, then run:

```r
# Install development tools
install.packages(c("devtools", "roxygen2", "pkgdown", "usethis"))

# Generate documentation
devtools::document()

# Build the website
pkgdown::build_site()
```

### 2. Daily Development Workflow

```r
# 1. Load your package for testing
devtools::load_all()  # or press Ctrl/Cmd + Shift + L

# 2. Make changes to your R files
# ... edit R/some_function.R ...

# 3. Update documentation (if you changed roxygen2 comments)
devtools::document()  # or press Ctrl/Cmd + Shift + D

# 4. Test your changes
devtools::test()  # or press Ctrl/Cmd + Shift + T

# 5. Check the package
devtools::check()  # or press Ctrl/Cmd + Shift + E
```

### 3. Before Committing Changes

```r
# Update documentation
devtools::document()

# Rebuild website (if you want to preview)
pkgdown::build_site()

# Run full check
devtools::check()
```

Then commit in terminal/git pane:
```bash
git add .
git commit -m "Your commit message"
git push
```

## Quick Commands Cheat Sheet

### Package Development
```r
# Load package for interactive testing
devtools::load_all()           # Ctrl/Cmd + Shift + L

# Generate .Rd files from roxygen2
devtools::document()           # Ctrl/Cmd + Shift + D

# Install package locally
devtools::install()            # or Build → Install and Restart

# Run tests
devtools::test()               # Ctrl/Cmd + Shift + T

# Check package (R CMD check)
devtools::check()              # Ctrl/Cmd + Shift + E
```

### Documentation & Website
```r
# Build pkgdown website
pkgdown::build_site()

# Preview a single function's help page
?calculate_summary_stats

# View package help
?consumeR
```

### Dependency Management
```r
# Add a package to Imports
usethis::use_package("dplyr")

# Add a package to Suggests
usethis::use_package("testthat", type = "Suggests")
```

## RStudio Panes Setup

Recommended pane layout for package development:

1. **Source Editor** (top-left) - Your R files
2. **Console** (bottom-left) - Run commands
3. **Build Tab** (top-right) - Package build tools
4. **Files/Plots/Help** (bottom-right) - Browse files and help

## Tips for Package Development in RStudio

### 1. Use devtools::load_all() instead of library()
```r
# DON'T do this during development:
library(consumeR)

# DO this instead:
devtools::load_all()  # or Ctrl/Cmd + Shift + L
```
This loads your latest code without needing to reinstall the package.

### 2. Auto-format Your Code
- Select code → **Code → Reformat Code** (or Ctrl/Cmd + Shift + A)
- Uses the styler package for consistent formatting

### 3. Navigate Functions Easily
- **Ctrl/Cmd + .** - Go to file/function
- **Ctrl/Cmd + Click** - Jump to function definition
- **Alt + Shift + K** - View all keyboard shortcuts

### 4. Use the Build Pane
The Build pane shows you:
- Install and Restart - Builds and loads your package
- Check - Runs comprehensive package checks
- More → Document - Runs roxygen2 to update docs

### 5. Git Integration
RStudio has built-in Git support:
- **Git pane** - See changed files
- **Diff** - Review changes before committing
- **Commit** - Stage and commit changes
- **Push/Pull** - Sync with GitHub

## Troubleshooting

### "Could not find function" errors
- Run `devtools::load_all()` to load your latest code
- Make sure the function has `@export` in its roxygen2 comments
- Run `devtools::document()` to update NAMESPACE

### Documentation not updating
- Run `devtools::document()` manually
- Check that roxygen2 comments are properly formatted
- Restart R session: **Session → Restart R**

### Build fails
- Run `devtools::check()` to see detailed errors
- Make sure all package dependencies are installed
- Check that DESCRIPTION file has correct package versions

## Next Steps After Setup

1. ✅ Open `consumeR.Rproj` in RStudio
2. ✅ Run `source("setup_documentation.R")` to generate docs
3. ✅ Use `devtools::load_all()` to test your functions
4. ✅ Make changes to R files
5. ✅ Run `devtools::document()` when you update roxygen2 comments
6. ✅ Run `devtools::check()` before committing
7. ✅ Commit and push your changes

Happy package development! 🚀
