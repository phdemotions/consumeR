#' Cloud 9 Customer Survey Data
#'
#' A dataset containing simulated customer survey data from Cloud 9 (inspired by
#' the TV show Superstore). This data represents a typical retail consumer research
#' study comparing customers who received a promotional flyer ("Got Flyer") versus
#' those who didn't ("No Flyer").
#'
#' @format A data frame with 100 rows and 6 variables:
#' \describe{
#'   \item{customer_id}{Unique customer identifier (1-100)}
#'   \item{customer_name}{Customer name (character)}
#'   \item{flyer_group}{Whether customer received promotional flyer: "Got Flyer" or "No Flyer"}
#'   \item{spending}{Purchase amount in dollars during visit (numeric)}
#'   \item{satisfaction}{Customer satisfaction rating on 1-10 scale (integer)}
#'   \item{loyalty_score}{Customer loyalty/engagement score on 0-100 scale (numeric)}
#' }
#'
#' @details
#' This dataset uses relatable retail scenarios to demonstrate the package's
#' functionality. Think of it like analyzing customer behavior at Cloud 9
#' (Superstore) or tracking sales at Dunder Mifflin (The Office).
#'
#' ## Study Context
#' Imagine Cloud 9 ran a promotional campaign where some customers received
#' a flyer with special deals while shopping. This dataset tracks whether
#' the flyer influenced their spending, satisfaction, and loyalty.
#'
#' - **Got Flyer group** (n=50): Customers who received the promotional flyer
#' - **No Flyer group** (n=50): Regular shoppers who didn't get the flyer
#'
#' ## Variables Explained
#' - **spending**: Total purchase amount (like buying bulk toilet paper or organizing supplies)
#' - **satisfaction**: How happy they were (1=Angry Garrett, 10=Enthusiastic Cheyenne)
#' - **loyalty_score**: How likely to return (0=Never coming back, 100=Daily shopper)
#'
#' ## Example Scenarios
#' The data includes realistic retail situations:
#' - Impulse purchases (saw the flyer, bought extra items)
#' - Regular shoppers (same behavior with or without flyer)
#' - Price-sensitive customers (flyer had big impact)
#' - Loyal customers (high scores regardless of promotion)
#'
#' @examples
#' # Load the Cloud 9 customer data
#' data(consumer_survey)
#'
#' # View first few customers
#' head(consumer_survey)
#'
#' # How much did customers spend on average?
#' calculate_summary_stats(consumer_survey$spending)
#'
#' # Did the flyer increase spending?
#' flyer_spending <- consumer_survey$spending[consumer_survey$flyer_group == "Got Flyer"]
#' no_flyer_spending <- consumer_survey$spending[consumer_survey$flyer_group == "No Flyer"]
#' test_group_differences(flyer_spending, no_flyer_spending)
#'
#' # Complete analysis: Was the promotional flyer effective?
#' create_analysis_report(
#'   data = consumer_survey,
#'   variable = "spending",
#'   group_var = "flyer_group",
#'   title = "Cloud 9 Promotional Flyer Campaign Analysis"
#' )
#'
#' # Customer satisfaction comparison
#' flyer_satisfaction <- consumer_survey$satisfaction[consumer_survey$flyer_group == "Got Flyer"]
#' no_flyer_satisfaction <- consumer_survey$satisfaction[consumer_survey$flyer_group == "No Flyer"]
#' test_group_differences(flyer_satisfaction, no_flyer_satisfaction)
#'
#' @source Simulated data inspired by Superstore/The Office for the consumeR package
"consumer_survey"
