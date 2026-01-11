#' Quick Test Helper - Find the Right Statistical Test
#'
#' @description
#' A simple helper that looks at your data and tells you which test to use.
#' Perfect for beginners who aren't sure which statistical test is appropriate.
#'
#' @param data Data frame containing your variables
#' @param outcome Name of your outcome/dependent variable (what you're measuring)
#' @param predictor Name of your predictor/independent variable (what you think affects the outcome)
#'
#' @return Prints recommendation and returns it invisibly
#'
#' @examples
#' \dontrun{
#' # I want to know if advertising affects sales
#' data(consumer_survey)
#' quick_test_help(consumer_survey, outcome = "spending", predictor = "flyer_group")
#'
#' # I want to know if age relates to satisfaction
#' quick_test_help(consumer_survey, outcome = "satisfaction", predictor = "age")
#' }
#'
#' @export
quick_test_help <- function(data, outcome, predictor) {

  # Check inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!outcome %in% names(data)) {
    stop("Can't find '", outcome, "' in your data. Available: ", paste(names(data), collapse = ", "))
  }

  if (!predictor %in% names(data)) {
    stop("Can't find '", predictor, "' in your data. Available: ", paste(names(data), collapse = ", "))
  }

  # Get the actual data
  outcome_data <- data[[outcome]]
  predictor_data <- data[[predictor]]

  # Classify outcome
  outcome_is_numeric <- is.numeric(outcome_data)
  outcome_unique <- length(unique(na.omit(outcome_data)))

  # Classify predictor
  predictor_is_numeric <- is.numeric(predictor_data)
  predictor_unique <- length(unique(na.omit(predictor_data)))

  # Determine recommendation
  recommendation <- NULL

  # CASE 1: Numeric outcome, 2-group categorical predictor
  if (outcome_is_numeric && !predictor_is_numeric && predictor_unique == 2) {
    recommendation <- list(
      scenario = paste("Comparing", outcome, "between 2 groups (", predictor, ")"),
      test_name = "Independent t-test (or Mann-Whitney if data not normal)",
      function_to_use = "test_group_differences()",
      why = paste(
        "You're comparing a numeric outcome (", outcome, ") between 2 groups.",
        "A t-test checks if the averages differ. The function will automatically",
        "check if your data is normal and use Mann-Whitney test if needed."
      ),
      example_code = sprintf(
        'library(dplyr)\ngroup1 <- data %%>%% filter(%s == "%s")\ngroup2 <- data %%>%% filter(%s == "%s")\n\nresult <- test_group_differences(\n  group1$%s,\n  group2$%s\n)\nprint(result)',
        predictor,
        unique(na.omit(predictor_data))[1],
        predictor,
        unique(na.omit(predictor_data))[2],
        outcome,
        outcome
      ),
      next_steps = c(
        "1. Run the example code above",
        "2. Look at the p-value to see if groups differ",
        "3. Check the assumptions section to make sure the test is valid",
        "4. Report the results using the interpretation provided"
      )
    )
  }

  # CASE 2: Numeric outcome, 3+ group categorical predictor
  else if (outcome_is_numeric && !predictor_is_numeric && predictor_unique > 2) {
    recommendation <- list(
      scenario = paste("Comparing", outcome, "across", predictor_unique, "groups (", predictor, ")"),
      test_name = "One-Way ANOVA (or Kruskal-Wallis if data not normal)",
      function_to_use = "run_anova()",
      why = paste(
        "You're comparing a numeric outcome (", outcome, ") across", predictor_unique, "groups.",
        "ANOVA tests if at least one group differs from the others."
      ),
      example_code = sprintf(
        'result <- run_anova(\n  data = data,\n  outcome = "%s",\n  group = "%s"\n)\nprint(result)',
        outcome,
        predictor
      ),
      next_steps = c(
        "1. Run the ANOVA first to see if ANY groups differ",
        "2. If significant, run post-hoc tests to see WHICH groups differ",
        "3. Check assumptions (normality, equal variances)",
        "4. Report effect sizes along with p-values"
      )
    )
  }

  # CASE 3: Categorical outcome, categorical predictor
  else if (!outcome_is_numeric && !predictor_is_numeric) {
    recommendation <- list(
      scenario = paste("Testing if", outcome, "is related to", predictor),
      test_name = "Chi-Square Test of Independence",
      function_to_use = "chisq_test()",
      why = paste(
        "You're looking at whether two categorical variables are related.",
        "Chi-square tests if the distribution of", outcome, "differs across", predictor, "groups."
      ),
      example_code = sprintf(
        'result <- chisq_test(\n  data = data,\n  x = "%s",\n  y = "%s"\n)\nprint(result)',
        predictor,
        outcome
      ),
      next_steps = c(
        "1. Run the chi-square test",
        "2. Check if expected cell counts are >= 5 (shown in output)",
        "3. If cells < 5, use fisher_exact_test() instead",
        "4. Report Cramer's V effect size"
      )
    )
  }

  # CASE 4: Both numeric (correlation)
  else if (outcome_is_numeric && predictor_is_numeric) {
    recommendation <- list(
      scenario = paste("Testing if", outcome, "and", predictor, "are related"),
      test_name = "Correlation Analysis (Pearson or Spearman)",
      function_to_use = "analyze_correlation()",
      why = paste(
        "You're looking at the relationship between two numeric variables.",
        "Correlation tells you if they move together (positive), opposite (negative), or not at all."
      ),
      example_code = sprintf(
        'result <- analyze_correlation(\n  data = data,\n  vars = c("%s", "%s")\n)\nprint(result)',
        outcome,
        predictor
      ),
      next_steps = c(
        "1. Run the correlation",
        "2. Look at the correlation coefficient (r): closer to 1 or -1 means stronger",
        "3. Check the p-value to see if it's significant",
        "4. Make a scatterplot to visualize the relationship"
      )
    )
  }

  # CASE 5: Numeric predictor, categorical outcome (less common, but handle it)
  else if (!outcome_is_numeric && predictor_is_numeric) {
    recommendation <- list(
      scenario = paste("Predicting", outcome, "from", predictor),
      test_name = "Logistic Regression",
      function_to_use = "run_logistic()",
      why = paste(
        "You're predicting a categorical outcome (", outcome, ") from a numeric predictor (",
        predictor, "). Logistic regression estimates the probability of each outcome category."
      ),
      example_code = sprintf(
        'result <- run_logistic(\n  data = data,\n  outcome = "%s",\n  predictors = "%s"\n)\nprint(result)',
        outcome,
        predictor
      ),
      next_steps = c(
        "1. Run logistic regression",
        "2. Interpret odds ratios (OR > 1 means higher predictor increases probability)",
        "3. Check model fit statistics",
        "4. Report confidence intervals for odds ratios"
      )
    )
  }

  # Print formatted recommendation
  if (!is.null(recommendation)) {
    cat("\n")
    cat("============================================================\n")
    cat("  RECOMMENDED TEST FOR YOUR DATA\n")
    cat("============================================================\n\n")

    cat("YOUR SITUATION:\n")
    cat("  ", recommendation$scenario, "\n\n")

    cat("RECOMMENDED TEST:\n")
    cat("  ", recommendation$test_name, "\n\n")

    cat("USE THIS FUNCTION:\n")
    cat("  ", recommendation$function_to_use, "\n\n")

    cat("WHY THIS TEST?\n")
    wrapped <- strwrap(recommendation$why, width = 58, prefix = "   ")
    cat(paste(wrapped, collapse = "\n"), "\n\n")

    cat("COPY AND RUN THIS CODE:\n\n")
    code_lines <- strsplit(recommendation$example_code, "\n")[[1]]
    for (line in code_lines) {
      cat("   ", line, "\n")
    }
    cat("\n")

    cat("NEXT STEPS:\n")
    for (step in recommendation$next_steps) {
      cat("  ", step, "\n")
    }
    cat("\n")

    cat("============================================================\n")
    cat("Need more help? Type: ?", recommendation$function_to_use, "\n")
    cat("============================================================\n\n")

    invisible(recommendation)
  } else {
    cat("\n")
    cat("============================================================\n")
    cat("  HELP NEEDED\n")
    cat("============================================================\n\n")

    cat("I'm not sure which test to recommend for this combination.\n\n")

    cat("Your data:\n")
    cat("  Outcome:", outcome, "-", ifelse(outcome_is_numeric, "NUMERIC", "CATEGORICAL"),
        "(", outcome_unique, "unique values)\n")
    cat("  Predictor:", predictor, "-", ifelse(predictor_is_numeric, "NUMERIC", "CATEGORICAL"),
        "(", predictor_unique, "unique values)\n\n")

    cat("Try:\n")
    cat("  1. Post your question to stats.stackexchange.com\n")
    cat("  2. Email: joshgonzalesphd@gmail.com\n")
    cat("  3. Open an issue at: https://github.com/phdemotions/consumeR/issues\n\n")

    cat("============================================================\n\n")

    invisible(NULL)
  }
}
