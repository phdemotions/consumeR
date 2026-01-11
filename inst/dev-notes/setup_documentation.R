#!/usr/bin/env Rscript
#' Setup Script for consumeR Package Documentation
#'
#' This script will:
#' 1. Check for required packages
#' 2. Generate .Rd files from roxygen2 comments
#' 3. Build the pkgdown website
#' 4. Show you what to commit to git
#'
#' Run this script from the package root directory:
#' Rscript setup_documentation.R

cat("\n=== consumeR Documentation Setup ===\n\n")

# Function to check and install packages
check_and_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    cat("✓", pkg, "is already installed\n")
  }
}

# Step 1: Check for required packages
cat("Step 1: Checking required packages...\n")
required_packages <- c("devtools", "roxygen2", "pkgdown")

for (pkg in required_packages) {
  check_and_install(pkg)
}

cat("\n")

# Step 2: Generate documentation from roxygen2 comments
cat("Step 2: Generating .Rd files from roxygen2 comments...\n")
cat("This will create the man/ directory with documentation for each function.\n\n")

tryCatch({
  devtools::document()
  cat("✓ Documentation generated successfully!\n\n")
}, error = function(e) {
  cat("✗ Error generating documentation:\n")
  cat(conditionMessage(e), "\n")
  stop("Documentation generation failed. Please fix the errors above.")
})

# Step 3: Check what was generated
cat("Step 3: Checking generated documentation...\n")
rd_files <- list.files("man", pattern = "\\.Rd$", full.names = FALSE)
if (length(rd_files) > 0) {
  cat("✓ Generated", length(rd_files), "documentation files:\n")
  for (f in sort(rd_files)) {
    cat("  -", f, "\n")
  }
  cat("\n")
} else {
  cat("✗ No .Rd files were generated. Check that your R files have roxygen2 comments.\n\n")
}

# Step 4: Build pkgdown site
cat("Step 4: Building pkgdown website...\n")
cat("This will create the docs/ directory with your website.\n\n")

tryCatch({
  pkgdown::build_site()
  cat("\n✓ Website built successfully!\n\n")
}, error = function(e) {
  cat("✗ Error building website:\n")
  cat(conditionMessage(e), "\n")
  stop("Website build failed. Please fix the errors above.")
})

# Step 5: Show what to do next
cat("\n=== Next Steps ===\n\n")
cat("1. Preview your site locally:\n")
cat("   Open: docs/index.html in your browser\n\n")

cat("2. Check the function reference pages:\n")
cat("   Open: docs/reference/index.html\n")
cat("   Each function will have its own page like:\n")
cat("   - docs/reference/calculate_summary_stats.html\n")
cat("   - docs/reference/test_group_differences.html\n\n")

cat("3. Commit the generated files to git:\n")
cat("   git add man/\n")
cat("   git add NAMESPACE\n")
cat("   git add docs/\n")
cat("   git commit -m 'Add function reference documentation'\n")
cat("   git push\n\n")

cat("4. Your live site will be available at:\n")
cat("   https://phdemotions.github.io/consumeR/\n\n")

cat("✓ Setup complete!\n\n")
