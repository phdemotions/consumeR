#' Utility Functions for consumeR Package
#'
#' @description
#' Internal utility functions used across the consumeR package.
#'
#' @name utils
#' @keywords internal
NULL


#' String Concatenation Operator
#'
#' @description
#' Internal operator for concatenating strings. Used in print methods
#' throughout the package.
#'
#' @param a First string
#' @param b Second string
#' @return Concatenated string
#' @keywords internal
#' @noRd
`%+%` <- function(a, b) paste0(a, b)
