#' APA7 Template System
#'
#' Provides standardized APA7-compliant text templates for all analysis types.
#' Uses glue-based interpolation for type-safe rendering.
#'
#' @family infrastructure
#' @keywords internal

#' APA7 Text Templates (Internal)
#'
#' Not exported. Contains templates for methods, results, assumptions, and
#' interpretation sections.
#'
#' @keywords internal
.apa7_templates <- list(
  t_test = list(
    methods = "An independent samples t-test was conducted to compare {dv_name} between {group1} and {group2}.",
    results = "The test {significance_text} (t({df}) = {t_stat}, {p_text}, {ci_text}).",
    assumptions = "Assumptions of normality and homogeneity of variance were assessed.",
    interpretation = "{group_higher} had {direction} {dv_name} than {group_lower}."
  ),

  welch_t = list(
    methods = "A Welch's t-test (not assuming equal variances) was conducted to compare {dv_name} between {group1} and {group2}.",
    results = "The test {significance_text} (t({df}) = {t_stat}, {p_text}, {ci_text}).",
    assumptions = "Welch's correction was applied due to unequal variances.",
    interpretation = "{group_higher} had {direction} {dv_name} than {group_lower}."
  ),

  correlation = list(
    methods = "A {method} correlation was computed to assess the relationship between {var1} and {var2}.",
    results = "The correlation {significance_text} ({statistic_label}({df}) = {r}, {p_text}{ci_text}).",
    assumptions = "Assumptions of {assumptions_text} were assessed.",
    interpretation = "There was a {strength} {direction} relationship between {var1} and {var2}."
  ),

  anova = list(
    methods = "A one-way analysis of variance (ANOVA) was conducted to compare {dv_name} across {n_groups} groups.",
    results = "The omnibus test {significance_text} (F({df1}, {df2}) = {f_stat}, {p_text}, {eta_sq_text}).",
    assumptions = "Assumptions of normality, homogeneity of variance, and independence were assessed.",
    interpretation = "There {significance_text_plain} differences in {dv_name} across groups."
  ),

  regression = list(
    methods = "A linear regression was conducted with {dv_name} as the outcome and {n_predictors} predictor(s).",
    results = "The overall model {significance_text} (F({df1}, {df2}) = {f_stat}, {p_text}, R² = {r_squared}).",
    assumptions = "Assumptions of linearity, normality of residuals, homoscedasticity, and independence were assessed.",
    interpretation = "The model explained {r_squared_pct}% of variance in {dv_name}."
  )
)

#' Render APA7 Text from Template
#'
#' Interpolates values into APA7-compliant text templates using glue syntax.
#'
#' @param test_type Character. One of: "t_test", "welch_t", "correlation", "anova", "regression"
#' @param section Character. One of: "methods", "results", "assumptions", "interpretation"
#' @param values Named list of values to interpolate into template
#'
#' @return Character string with interpolated values
#'
#' @examples
#' render_apa7_text(
#'   test_type = "t_test",
#'   section = "results",
#'   values = list(
#'     significance_text = "was significant",
#'     df = 98,
#'     t_stat = "2.05",
#'     p_text = "p = .042",
#'     ci_text = "95% CI [0.12, 3.45]"
#'   )
#' )
#'
#' @export
render_apa7_text <- function(test_type, section, values) {
  # Validate test_type
  if (!test_type %in% names(.apa7_templates)) {
    rlang::abort(
      paste0("Unknown test_type: ", test_type, ". Available: ", paste(names(.apa7_templates), collapse = ", ")),
      class = "unknown_test_type_error"
    )
  }

  # Validate section
  valid_sections <- c("methods", "results", "assumptions", "interpretation")
  if (!section %in% valid_sections) {
    rlang::abort(
      paste0("Unknown section: ", section, ". Available: ", paste(valid_sections, collapse = ", ")),
      class = "unknown_section_error"
    )
  }

  # Get template
  template <- .apa7_templates[[test_type]][[section]]

  if (is.null(template)) {
    rlang::abort(
      paste0("No template found for test_type '", test_type, "', section '", section, "'"),
      class = "missing_template_error"
    )
  }

  # Render using glue (will error if required values missing - good!)
  glue::glue(template, .envir = values)
}

#' Format P-Value for APA7
#'
#' Formats p-values according to APA7 rules (no leading zero, minimum threshold).
#'
#' @param p Numeric p-value
#' @param threshold Numeric. Report values below this as "< threshold" (default: 0.001)
#'
#' @return Character string formatted for APA7
#'
#' @examples
#' format_p_apa7(0.042)  # "p = .042"
#' format_p_apa7(0.0001) # "p < .001"
#'
#' @export
format_p_apa7 <- function(p, threshold = 0.001) {
  if (p < threshold) {
    return(paste0("p < ", sub("^0", "", sprintf("%.3f", threshold))))
  } else {
    # Remove leading zero
    p_str <- sub("^0", "", sprintf("%.3f", p))
    return(paste0("p = ", p_str))
  }
}
