#' Cloud 9 Customer Survey Data
#'
#' A beginner-friendly dataset containing simulated customer survey data from Cloud 9
#' (inspired by the TV show Superstore). This data represents a typical retail
#' consumer research study that's perfect for learning statistical analysis.
#'
#' @description
#' **What is this data?**
#'
#' Imagine Cloud 9 wanted to test if sending promotional flyers to customers would
#' increase their spending. They randomly gave flyers to 50 customers ("Got Flyer"
#' group) and compared them to 50 customers who shopped normally ("No Flyer" group).
#' This dataset contains the results.
#'
#' **Why use this data?**
#'
#' This is a perfect practice dataset because:
#' - It's based on a familiar scenario (retail shopping)
#' - It has fun character names from Superstore and The Office
#' - It demonstrates real research questions you'd encounter
#' - It's the right size (100 customers) for learning statistics
#' - All variables are clearly labeled and easy to understand
#'
#' @format A data frame with 100 rows (customers) and 6 variables (columns):
#' \describe{
#'   \item{customer_id}{Unique customer number from 1 to 100. Each customer
#'     appears exactly once in this dataset.}
#'   \item{customer_name}{Customer name (character). Includes fun references to
#'     characters from "Superstore" like Amy Sosa and Jonah Simms, plus "The Office"
#'     characters like Jim Halpert, Pam Beesly, and Dwight Schrute!}
#'   \item{flyer_group}{Which experimental group the customer was in: "Got Flyer"
#'     (received promotional materials) or "No Flyer" (normal shopping). This is
#'     your independent variable or grouping variable.}
#'   \item{spending}{How much money (in dollars) the customer spent during their
#'     visit. This is a continuous numeric variable, perfect for calculating means
#'     and doing t-tests.}
#'   \item{satisfaction}{Customer satisfaction rating on a scale from 1 (very
#'     dissatisfied) to 10 (very satisfied). This is an ordinal variable suitable
#'     for group comparisons.}
#'   \item{loyalty_score}{Customer loyalty score on a scale from 0 to 100, where
#'     higher scores mean the customer is more likely to return and recommend Cloud 9
#'     to friends. Another continuous variable for analysis.}
#' }
#'
#' @details
#' ## The Research Question
#'
#' **Does sending promotional flyers increase customer spending?**
#'
#' This is a classic marketing research question! Cloud 9 wanted to know if
#' the cost of printing and distributing flyers would be worth it by increasing
#' how much customers spend in the store.
#'
#' ## Study Design (How the Data Was Collected)
#'
#' - **Type of study**: Randomized controlled trial (RCT) - the gold standard!
#' - **Sample size**: 100 customers total
#' - **Treatment group**: 50 customers who "Got Flyer" with special deals
#' - **Control group**: 50 customers who shopped normally ("No Flyer")
#' - **Outcomes measured**: Spending (in dollars), Satisfaction (1-10), Loyalty (0-100)
#'
#' ## What to Expect in the Data
#'
#' The "Got Flyer" group tends to spend a bit more on average than the "No Flyer"
#' group, but there's overlap between groups (not everyone responds the same way).
#' This demonstrates a realistic **small-to-medium effect size**, which is common
#' in consumer research and marketing studies.
#'
#' ## How to Use This Dataset for Learning
#'
#' **Step 1: Load and explore**
#' ```r
#' data(consumer_survey)
#' head(consumer_survey)  # See first few rows
#' summary(consumer_survey)  # Get basic overview
#' ```
#'
#' **Step 2: Ask research questions**
#' - Did the flyer increase spending?
#' - Are customers more satisfied when they get a flyer?
#' - Does loyalty differ between groups?
#'
#' **Step 3: Analyze with consumeR functions**
#' - Use `calculate_summary_stats()` for descriptive statistics
#' - Use `test_group_differences()` to compare flyer vs. no-flyer
#' - Use `create_analysis_report()` for a complete write-up
#'
#' ## Fun Facts About the Data
#'
#' - Customer names include beloved characters from "Superstore" (Amy Sosa,
#'   Jonah Simms, Garrett McNeill, Dina Fox) and "The Office" (Jim Halpert,
#'   Pam Beesly, Dwight Schrute, Michael Scott)
#' - The data represents realistic shopping patterns you'd see at a real
#'   retail store - some customers spend a lot, others very little
#' - Satisfaction ratings follow real human behavior - most people are
#'   moderately satisfied (around 6-8), with a few very happy or unhappy outliers
#'
#' @examples
#' # ========================================
#' # GETTING STARTED: Load and Explore the Data
#' # ========================================
#'
#' # Load the Cloud 9 customer data
#' data(consumer_survey)
#'
#' # Look at the first few rows to see what the data looks like
#' head(consumer_survey)
#'
#' # Get a quick summary of all variables
#' summary(consumer_survey)
#'
#' # How many customers in each group?
#' table(consumer_survey$flyer_group)
#' # Result: 50 customers got the flyer, 50 did not
#'
#'
#' # ========================================
#' # EXAMPLE 1: Descriptive Statistics
#' # ========================================
#'
#' # How much did customers spend on average?
#' library(consumeR)
#' calculate_summary_stats(consumer_survey$spending)
#' # This shows mean, median, SD, and more for spending
#'
#' # What about satisfaction ratings?
#' calculate_summary_stats(consumer_survey$satisfaction)
#'
#'
#' # ========================================
#' # EXAMPLE 2: Compare Two Groups
#' # ========================================
#'
#' # Research Question: Did the flyer increase spending?
#'
#' # First, separate the two groups:
#' flyer_group <- consumer_survey$spending[consumer_survey$flyer_group == "Got Flyer"]
#' no_flyer_group <- consumer_survey$spending[consumer_survey$flyer_group == "No Flyer"]
#'
#' # Now compare them:
#' result <- test_group_differences(flyer_group, no_flyer_group)
#'
#' # See the results in plain English:
#' cat(result$interpretation)
#'
#'
#' # ========================================
#' # EXAMPLE 3: Complete Analysis Report
#' # ========================================
#'
#' # Generate a full analysis report for peer review
#' create_analysis_report(
#'   data = consumer_survey,
#'   variable = "spending",
#'   group_var = "flyer_group",
#'   title = "Cloud 9 Promotional Flyer Campaign Analysis"
#' )
#' # This creates a comprehensive report with statistics,
#' # group comparisons, and interpretations
#'
#'
#' # ========================================
#' # EXAMPLE 4: Analyze Satisfaction
#' # ========================================
#'
#' # Did customers who got the flyer feel more satisfied?
#' flyer_satisfaction <- consumer_survey$satisfaction[
#'   consumer_survey$flyer_group == "Got Flyer"
#' ]
#' no_flyer_satisfaction <- consumer_survey$satisfaction[
#'   consumer_survey$flyer_group == "No Flyer"
#' ]
#'
#' satisfaction_result <- test_group_differences(
#'   flyer_satisfaction,
#'   no_flyer_satisfaction
#' )
#' cat(satisfaction_result$interpretation)
#'
#'
#' # ========================================
#' # FUN EXPLORATION: Who Were Our Top Spenders?
#' # ========================================
#'
#' # Sort by spending (highest first) and look at top 10
#' top_spenders <- consumer_survey[order(-consumer_survey$spending), ]
#' head(top_spenders[, c("customer_name", "spending", "flyer_group")], 10)
#'
#' # Did the flyer help these top spenders? Check their group!
#'
#' @source
#' Simulated data created for the consumeR package. Inspired by:
#' - "Superstore" (NBC, 2015-2021) - Cloud 9 store setting and characters
#' - "The Office" (NBC, 2005-2013) - Dunder Mifflin characters
#'
#' The data was generated using rnorm() to create realistic retail patterns.
#' See data-raw/create_data.R for the complete simulation code.
#'
#' @keywords datasets
"consumer_survey"
