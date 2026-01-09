#' consumeR: Transparent and Reproducible Consumer Research Analysis
#'
#' The consumeR package provides transparent, well-documented functions for
#' consumer research analysis that facilitate reproducibility during peer review.
#' All functions are designed with clarity and readability in mind, making code
#' easy to understand for reviewers with varying levels of programming experience.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{calculate_summary_stats}}}{Calculate descriptive statistics
#'     with clear, step-by-step documentation}
#'   \item{\code{\link{test_group_differences}}}{Compare two groups with
#'     transparent statistical testing}
#'   \item{\code{\link{create_analysis_report}}}{Generate comprehensive,
#'     human-readable analysis reports}
#' }
#'
#' @section Design Philosophy:
#' The package is designed for maximum transparency in research:
#' \itemize{
#'   \item Every function includes extensive inline comments explaining each step
#'   \item Input validation with clear error messages
#'   \item Automatic handling and reporting of missing values
#'   \item Plain English interpretations of statistical results
#'   \item Complete documentation with multiple examples
#' }
#'
#' @docType package
#' @name consumeR-package
#' @aliases consumeR
#' @import stats
#' @import graphics
#' @import grDevices
#' @import utils
#' @import methods
NULL
