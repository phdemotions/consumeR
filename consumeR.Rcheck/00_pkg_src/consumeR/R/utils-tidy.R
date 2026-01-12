#' @importFrom rlang .data :=
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr all_of desc group_by summarise ungroup
#' @importFrom ggplot2 annotate element_blank geom_hline geom_point scale_fill_brewer scale_x_continuous
NULL

# Declare global variables to avoid R CMD check notes
utils::globalVariables(c(
  # Tidyverse NSE variables
  ".", ".data", ":=", "%>%",

  # Common column names used in tidyverse pipelines
  "variable", "value", "n", "se", "estimate", "conf_low", "conf_high",
  "p_value", "p.value", "statistic", "parameter", "ci.lower", "ci.upper",
  "ci_lower", "ci_upper", "sd", "var", "mean_diff", "cohens_d",
  "interpretation", "comparison", "t_value", "t.ratio", "F_value",
  "effect", "df", "df_corrected", "p_value_corrected", "partial_eta_sq",
  "SE", "z", "significant", "contrast",

  # Factor analysis variables
  "item", "loading", "abs_loading", "factor_number", "eigenvalue",
  "ss_loadings", "proportion_var", "percent_var", "max_loading", "label",

  # Data import variables
  "n_missing", "n_unique", "percent_missing", "suggested_type",
  "current_type", "change_suggested", "suggestion_reason",

  # ggplot2 aesthetics (sometimes flagged)
  "group", "fill", "color", "label"
))
