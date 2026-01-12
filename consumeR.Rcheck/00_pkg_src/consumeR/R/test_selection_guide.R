#' Interactive Statistical Test Selection Guide
#'
#' @description
#' Helps users with no statistics experience choose the right test for their
#' research question. Asks simple questions about the data and research goal,
#' then recommends appropriate statistical tests with explanations.
#'
#' @param data Optional data frame to analyze
#' @param outcome Character. Name of the outcome/dependent variable (optional)
#' @param predictor Character. Name of the predictor/independent variable (optional)
#' @param interactive Logical. Run in interactive mode with prompts? Default TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{recommended_test}{Name of recommended statistical test}
#'     \item{function_name}{consumeR function to use}
#'     \item{explanation}{Why this test is appropriate}
#'     \item{assumptions}{What to check before running the test}
#'     \item{example_code}{Example code to run the test}
#'     \item{alternative_tests}{Other tests to consider}
#'   }
#'
#' @details
#' The guide asks about:
#' - What you're trying to find out (compare groups, find relationships, predict outcomes)
#' - How many groups you're comparing
#' - What type of data you have (numbers, categories, yes/no)
#' - Whether the same people are measured multiple times
#' - How many variables you're looking at
#'
#' Based on your answers, it recommends the appropriate test and explains why.
#'
#' @examples
#' \dontrun{
#' # Interactive mode - answers questions step by step
#' guide <- select_statistical_test()
#'
#' # Or provide variables upfront for quick recommendation
#' guide <- select_statistical_test(
#'   data = my_data,
#'   outcome = "purchase_amount",
#'   predictor = "ad_condition"
#' )
#'
#' # See the recommendation
#' print(guide)
#'
#' # Run the suggested code
#' eval(parse(text = guide$example_code))
#' }
#'
#' @export
select_statistical_test <- function(data = NULL,
                                    outcome = NULL,
                                    predictor = NULL,
                                    interactive = TRUE) {

  # If data and variables provided, analyze them first
  if (!is.null(data) && !is.null(outcome)) {
    outcome_type <- classify_variable_type(data[[outcome]])

    if (!is.null(predictor)) {
      predictor_type <- classify_variable_type(data[[predictor]])
      n_groups <- length(unique(data[[predictor]]))
    } else {
      predictor_type <- NULL
      n_groups <- NULL
    }
  } else {
    outcome_type <- NULL
    predictor_type <- NULL
    n_groups <- NULL
  }

  # If interactive, ask questions
  if (interactive && interactive()) {  # Check if in interactive session
    message("\n=== Statistical Test Selection Guide ===\n")
    message("Let's find the right test for your research question!\n")

    # Question 1: What are you trying to find out?
    message("\n1. What are you trying to find out?\n")
    message("   a) Compare groups (e.g., Do men and women spend differently?)")
    message("   b) Find relationships (e.g., Does price relate to quality?)")
    message("   c) Predict outcomes (e.g., Can we predict purchases from ads?)")
    message("   d) Test change over time (e.g., Did satisfaction increase?)")

    research_goal <- readline("Enter your choice (a/b/c/d): ")

    # Question 2: About the outcome variable
    if (is.null(outcome_type)) {
      message("\n2. What kind of outcome are you measuring?\n")
      message("   a) Numbers (e.g., amount spent, satisfaction rating 1-10)")
      message("   b) Categories (e.g., brand chosen, product type)")
      message("   c) Yes/No or True/False")

      outcome_choice <- readline("Enter your choice (a/b/c): ")
      outcome_type <- switch(outcome_choice,
                            "a" = "numeric",
                            "b" = "categorical",
                            "c" = "binary")
    }

    # Question 3: About groups (if comparing)
    if (research_goal == "a") {
      if (is.null(n_groups)) {
        message("\n3. How many groups are you comparing?\n")
        n_groups <- as.integer(readline("Enter number: "))
      }

      message("\n4. Are the same people measured in each group?\n")
      message("   (e.g., measuring the same customers before and after)")
      paired <- tolower(readline("Yes or No: ")) == "yes"

    } else {
      paired <- FALSE
    }

  } else {
    # Non-interactive or missing info
    research_goal <- "a"  # Default
    paired <- FALSE
  }

  # Determine recommendation based on answers
  recommendation <- determine_test_recommendation(
    research_goal = research_goal,
    outcome_type = outcome_type,
    predictor_type = predictor_type,
    n_groups = n_groups,
    paired = paired,
    data = data,
    outcome = outcome,
    predictor = predictor
  )

  # Format and return
  structure(
    recommendation,
    class = c("test_recommendation", "list")
  )
}


#' Classify Variable Type
#' @keywords internal
classify_variable_type <- function(x) {
  if (is.numeric(x)) {
    n_unique <- length(unique(na.omit(x)))
    if (n_unique == 2) {
      return("binary")
    } else if (n_unique < 10) {
      return("ordinal")  # Could be Likert scale
    } else {
      return("numeric")
    }
  } else if (is.factor(x) || is.character(x)) {
    n_unique <- length(unique(na.omit(x)))
    if (n_unique == 2) {
      return("binary")
    } else {
      return("categorical")
    }
  } else if (is.logical(x)) {
    return("binary")
  } else {
    return("unknown")
  }
}


#' Determine Test Recommendation
#' @keywords internal
determine_test_recommendation <- function(research_goal,
                                         outcome_type,
                                         predictor_type,
                                         n_groups,
                                         paired,
                                         data,
                                         outcome,
                                         predictor) {

  # COMPARING GROUPS
  if (research_goal == "a") {

    # Numeric outcome
    if (outcome_type == "numeric") {
      if (n_groups == 2 && !paired) {
        # Independent two-group comparison
        list(
          recommended_test = "Independent Samples t-test (or Wilcoxon if non-normal)",
          function_name = "test_group_differences()",
          explanation = paste(
            "You're comparing a numeric outcome between 2 independent groups.",
            "The t-test checks if the average differs between groups.",
            "The function will automatically check assumptions and use",
            "Wilcoxon test if the data aren't normally distributed."
          ),
          assumptions = c(
            "Independence: The two groups are separate people/items",
            "Normality: Data in each group should be roughly bell-shaped (tested automatically)",
            "Equal variance: Spread of data similar in both groups (tested automatically)"
          ),
          example_code = if (!is.null(data) && !is.null(outcome) && !is.null(predictor)) {
            sprintf('test_group_differences(data = %s, outcome = "%s", group = "%s")',
                   deparse(substitute(data)), outcome, predictor)
          } else {
            'test_group_differences(data = my_data, outcome = "amount_spent", group = "condition")'
          },
          alternative_tests = c(
            "mann_whitney_test() - If data are very skewed or have outliers",
            "run_anova() - If you want detailed variance analysis"
          )
        )

      } else if (n_groups == 2 && paired) {
        # Paired two-group comparison
        list(
          recommended_test = "Paired t-test (or Wilcoxon Signed-Rank if non-normal)",
          function_name = "test_group_differences() with paired=TRUE",
          explanation = paste(
            "You're comparing the same people at two time points or conditions.",
            "The paired t-test accounts for individual differences by looking at",
            "the change within each person."
          ),
          assumptions = c(
            "Paired observations: Each measurement in group 1 matches one in group 2",
            "Normality of differences: The change scores should be roughly normal",
            "Independence between pairs: Different people are independent"
          ),
          example_code = if (!is.null(data)) {
            sprintf('test_group_differences(before_scores, after_scores, paired = TRUE)')
          } else {
            'test_group_differences(before_scores, after_scores, paired = TRUE)'
          },
          alternative_tests = c(
            "wilcoxon_signed_rank_test() - If differences are very skewed",
            "run_rm_anova() - If you have more than 2 time points"
          )
        )

      } else if (n_groups > 2 && !paired) {
        # Multiple independent groups
        list(
          recommended_test = "One-Way ANOVA (or Kruskal-Wallis if non-normal)",
          function_name = "run_anova()",
          explanation = paste(
            "You're comparing a numeric outcome across 3+ independent groups.",
            "ANOVA tests if at least one group differs from the others.",
            "If significant, follow up with post-hoc tests to see which groups differ."
          ),
          assumptions = c(
            "Independence: Groups contain different people/items",
            "Normality: Data in each group roughly bell-shaped",
            "Equal variances: Similar spread across all groups"
          ),
          example_code = if (!is.null(data) && !is.null(outcome) && !is.null(predictor)) {
            sprintf('run_anova(data = %s, outcome = "%s", group = "%s")',
                   deparse(substitute(data)), outcome, predictor)
          } else {
            'run_anova(data = my_data, outcome = "satisfaction", group = "product_type")'
          },
          alternative_tests = c(
            "kruskal_wallis_test() - If data are very skewed or ordinal",
            "compare_groups_anova() - For detailed between-group comparisons"
          )
        )

      } else if (n_groups > 2 && paired) {
        # Repeated measures ANOVA
        list(
          recommended_test = "Repeated Measures ANOVA (or Friedman if non-normal)",
          function_name = "run_rm_anova()",
          explanation = paste(
            "You're comparing the same people across 3+ time points or conditions.",
            "Repeated measures ANOVA accounts for individual differences",
            "and is more powerful than regular ANOVA for this design."
          ),
          assumptions = c(
            "Paired observations: Same people measured at all time points",
            "Normality: Data should be roughly normal at each time point",
            "Sphericity: Variances of differences between all pairs similar"
          ),
          example_code = if (!is.null(data) && !is.null(outcome)) {
            'run_rm_anova(data = my_data, outcome = "satisfaction", within = "time", subject = "participant_id")'
          } else {
            'run_rm_anova(data = my_data, outcome = "satisfaction", within = "time", subject = "participant_id")'
          },
          alternative_tests = c(
            "friedman_test() - If data are very skewed or ordinal",
            "run_mlm() - If you have additional predictors or covariates"
          )
        )
      }

    # Categorical/Binary outcome
    } else if (outcome_type %in% c("categorical", "binary")) {
      if (n_groups == 2 && !paired) {
        list(
          recommended_test = "Chi-Square Test of Independence (or Fisher's Exact for small samples)",
          function_name = "chisq_test()",
          explanation = paste(
            "You're comparing categorical outcomes between 2 groups.",
            "The chi-square test checks if the distribution of categories",
            "differs between groups (e.g., do men and women choose different brands?)."
          ),
          assumptions = c(
            "Independence: Each observation belongs to only one cell",
            "Expected frequencies: At least 5 expected count in each cell",
            "Large enough sample: Generally n > 20 total"
          ),
          example_code = if (!is.null(data) && !is.null(outcome) && !is.null(predictor)) {
            sprintf('chisq_test(data = %s, x = "%s", y = "%s")',
                   deparse(substitute(data)), predictor, outcome)
          } else {
            'chisq_test(data = my_data, x = "gender", y = "brand_chosen")'
          },
          alternative_tests = c(
            "fisher_exact_test() - If any expected cell count < 5",
            "logistic regression - run_logistic() - If you want to control for other variables"
          )
        )
      } else if (paired) {
        list(
          recommended_test = "McNemar's Test",
          function_name = "mcnemar_test()",
          explanation = paste(
            "You're comparing categorical responses from the same people",
            "at two time points (e.g., purchased before vs. after campaign).",
            "McNemar's test focuses on those who changed their response."
          ),
          assumptions = c(
            "Paired observations: Same people measured twice",
            "Binary or categorical outcome",
            "Enough discordant pairs: At least 10 people who changed"
          ),
          example_code = 'mcnemar_test(data = my_data, var1 = "purchased_before", var2 = "purchased_after")',
          alternative_tests = c(
            "Cochran's Q test - For 3+ time points (not yet implemented)",
            "Logistic regression - For more complex designs"
          )
        )
      }
    }

  # RELATIONSHIPS
  } else if (research_goal == "b") {
    if (outcome_type == "numeric" && predictor_type == "numeric") {
      list(
        recommended_test = "Correlation Analysis",
        function_name = "analyze_correlation()",
        explanation = paste(
          "You're examining if two numeric variables are related",
          "(e.g., do higher prices relate to better quality ratings?).",
          "Correlation tells you the strength and direction of the relationship."
        ),
        assumptions = c(
          "Linearity: The relationship is straight-line, not curved",
          "No extreme outliers: Outliers can distort correlation",
          "Normality: For Pearson correlation (Spearman is more robust)"
        ),
        example_code = if (!is.null(data) && !is.null(outcome) && !is.null(predictor)) {
          sprintf('analyze_correlation(data = %s, vars = c("%s", "%s"))',
                 deparse(substitute(data)), outcome, predictor)
        } else {
          'analyze_correlation(data = my_data, vars = c("price", "quality_rating"))'
        },
        alternative_tests = c(
          "analyze_regression() - If you want to predict one variable from another",
          "run_sem() - For multiple related variables at once"
        )
      )
    }

  # PREDICTION
  } else if (research_goal == "c") {
    if (outcome_type == "numeric") {
      list(
        recommended_test = "Linear Regression",
        function_name = "analyze_regression()",
        explanation = paste(
          "You're predicting a numeric outcome from one or more predictors.",
          "Regression estimates how much the outcome changes for each unit",
          "of the predictor, controlling for other variables."
        ),
        assumptions = c(
          "Linearity: Relationship between predictors and outcome is linear",
          "Independence: Observations are independent",
          "Homoscedasticity: Consistent spread of errors across predictions",
          "Normality of residuals: Prediction errors are roughly normal"
        ),
        example_code = if (!is.null(data) && !is.null(outcome) && !is.null(predictor)) {
          sprintf('analyze_regression(data = %s, outcome = "%s", predictors = c("%s"))',
                 deparse(substitute(data)), outcome, predictor)
        } else {
          'analyze_regression(data = my_data, outcome = "purchase_amount", predictors = c("ad_exposure", "income"))'
        },
        alternative_tests = c(
          "run_mlm() - For nested/hierarchical data (e.g., customers within stores)",
          "run_sem() - For complex models with multiple outcomes"
        )
      )
    } else if (outcome_type %in% c("binary", "categorical")) {
      list(
        recommended_test = "Logistic Regression",
        function_name = "run_logistic()",
        explanation = paste(
          "You're predicting a yes/no or categorical outcome from predictors.",
          "Logistic regression estimates the probability of each outcome",
          "and provides odds ratios showing effect sizes."
        ),
        assumptions = c(
          "Independence: Observations are independent",
          "No perfect multicollinearity: Predictors not too highly correlated",
          "Linearity of log-odds: Predictors linearly related to log-odds",
          "Sufficient sample size: At least 10-15 events per predictor"
        ),
        example_code = 'run_logistic(data = my_data, outcome = "purchased", predictors = c("price", "ad_seen"))',
        alternative_tests = c(
          "chisq_test() - For simple 2x2 comparisons without covariates",
          "run_mlm() - For nested binary outcomes"
        )
      )
    }
  }

  # Default fallback
  list(
    recommended_test = "Contact a statistician",
    function_name = "NA",
    explanation = "Your research design is complex and would benefit from statistical consultation.",
    assumptions = c("None"),
    example_code = "# Please describe your research question to get a recommendation",
    alternative_tests = c("Consider posting to stats.stackexchange.com with details")
  )
}


#' Print Method for Test Recommendations
#' @param x A test_recommendation object
#' @param ... Additional arguments (unused)
#' @export
print.test_recommendation <- function(x, ...) {
  cat("\n")
  cat("===============================================================\n")
  cat("         RECOMMENDED STATISTICAL TEST                         \n")
  cat("===============================================================\n\n")

  cat("TEST:", x$recommended_test, "\n\n")

  cat("FUNCTION TO USE:", x$function_name, "\n\n")

  cat("WHY THIS TEST?\n")
  cat("  ", gsub("\n", "\n   ", x$explanation), "\n\n")

  cat("ASSUMPTIONS TO CHECK:\n")
  for (assumption in x$assumptions) {
    cat("  -", assumption, "\n")
  }
  cat("\n")

  cat("EXAMPLE CODE:\n")
  cat("  ", x$example_code, "\n\n")

  if (length(x$alternative_tests) > 0) {
    cat("ALTERNATIVES TO CONSIDER:\n")
    for (alt in x$alternative_tests) {
      cat("  -", alt, "\n")
    }
    cat("\n")
  }

  cat("---------------------------------------------------------------\n")
  cat("NEXT STEPS:\n")
  cat("  1. Make sure your data meets the assumptions listed above\n")
  cat("  2. Run the example code (modify variable names as needed)\n")
  cat("  3. Look at the output - it explains the results in plain English\n")
  cat("  4. If assumptions are violated, try an alternative test\n\n")

  invisible(x)
}
