# Setup Script for consumeR Package Development
# Run this script in R to prepare the package for use and CRAN submission

cat("==============================================\n")
cat("consumeR Package Setup Script\n")
cat("==============================================\n\n")

# Check if required packages are installed
required_packages <- c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown")

cat("Step 1: Checking required packages...\n")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
} else {
  cat("All required packages are installed.\n")
}

cat("\nStep 2: Loading devtools...\n")
library(devtools)

cat("\nStep 3: Setting working directory to package root...\n")
# Make sure we're in the package directory
if (basename(getwd()) != "consumeR") {
  cat("Please set your working directory to the consumeR package root.\n")
  cat("Use: setwd('path/to/consumeR')\n")
} else {
  cat("Working directory is correct: ", getwd(), "\n")
}

cat("\nStep 4: Generating documentation from roxygen2 comments...\n")
devtools::document()

cat("\nStep 5: Creating Cloud 9 themed example dataset...\n")
# Run the data creation script to generate themed customer data
source("data-raw/create_data.R")
cat("Dataset created with fun Superstore/Office themed customer names!\n")

cat("\nStep 6: Running package check...\n")
cat("This may take a few minutes...\n\n")
check_result <- devtools::check()

cat("\n==============================================\n")
cat("Setup Complete!\n")
cat("==============================================\n\n")

cat("Next steps:\n")
cat("1. Review check results above (should be 0 errors, 0 warnings, 0 notes)\n")
cat("2. Test the package: devtools::test()\n")
cat("3. Build the package: devtools::build()\n")
cat("4. Install locally: devtools::install()\n")
cat("5. Try the examples in the README.md\n\n")

cat("For CRAN submission:\n")
cat("1. Run: devtools::check(cran = TRUE)\n")
cat("2. Run: devtools::build()\n")
cat("3. Submit the .tar.gz file to CRAN\n\n")

cat("See DEVELOPMENT.md for detailed instructions.\n")
