# ============================================================================
# L1 Helper Functions: Usage Examples
# ============================================================================
# This file demonstrates how to use the new Level 1 (L1) internal helpers
# for SPSS labelled data and standardization.
#
# NOTE: These are INTERNAL helpers (not exported). They will be used by
# higher-level user-facing functions in future development.
#
# This file is for DEVELOPMENT ONLY and will not be included in the package build.
# ============================================================================

library(consumeR)

# ============================================================================
# PART 1: SPSS Labelled Data Helpers
# ============================================================================

# Scenario: You've imported SPSS data and need to understand the value labels
# and identify problematic response codes before analysis.

## Example 1: Extract value labels ----

if (requireNamespace("haven", quietly = TRUE)) {

  # Create a mock labelled variable (simulating SPSS import)
  gender <- haven::labelled(
    c(1, 2, 1, 2, 99, 1, 2, 99),
    labels = c(
      "Male" = 1,
      "Female" = 2,
      "Prefer not to say" = 99
    )
  )

  # Extract the value labels
  labels <- consumeR:::extract_value_labels(gender)
  print(labels)
  #   value label
  #   1     Male
  #   2     Female
  #   99    Prefer not to say


  ## Example 2: Identify problematic values ----

  # Check which values are problematic (should be recoded to NA)
  problems <- consumeR:::identify_problematic_values(labels)
  print(problems)
  #   value label              reason
  #   99    Prefer not to say  Prefer not to answer


  ## Example 3: Recode problematic values to NA ----

  # Recode 99 to NA and track what was changed
  result <- consumeR:::recode_to_na(
    gender,
    values_to_na = 99,
    variable_name = "gender"
  )

  # Check the recoded variable
  print(result$recoded)
  # [1]  1  2  1  2 NA  1  2 NA

  # Check the log (for transparency/peer review)
  print(result$log)
  #   variable value_recoded n_affected percent_affected
  #   gender   99            2          25%


  ## Example 4: Full workflow for SPSS data inspection ----

  satisfaction <- haven::labelled(
    c(1, 2, 3, 4, 5, 6, 7, 99, 99, -1),
    labels = c(
      "Very dissatisfied" = 1,
      "Dissatisfied" = 2,
      "Somewhat dissatisfied" = 3,
      "Neutral" = 4,
      "Somewhat satisfied" = 5,
      "Satisfied" = 6,
      "Very satisfied" = 7,
      "Don't know" = 99,
      "Not applicable" = -1
    )
  )

  # Step 1: Extract labels
  sat_labels <- consumeR:::extract_value_labels(satisfaction)
  print("All value labels:")
  print(sat_labels)

  # Step 2: Identify problematic values
  sat_problems <- consumeR:::identify_problematic_values(sat_labels)
  print("\nProblematic values to recode:")
  print(sat_problems)

  # Step 3: Recode to NA
  sat_clean <- consumeR:::recode_to_na(
    satisfaction,
    values_to_na = sat_problems$value,
    variable_name = "satisfaction"
  )

  print("\nCleaned variable:")
  print(sat_clean$recoded)

  print("\nRecoding log:")
  print(sat_clean$log)
}


# ============================================================================
# PART 2: Standardization Helpers (for Mixed-Scale Composites)
# ============================================================================

# Scenario: You want to create a composite measure from items on different scales.
# This is common in consumer research where items come from different sources.

## Example 5: Z-score standardization ----

# Items on different scales
satisfaction_5pt <- c(1, 2, 3, 4, 5)  # 1-5 scale
nps_100pt <- c(0, 25, 50, 75, 100)    # 0-100 scale

# Standardize to z-scores (mean=0, sd=1)
sat_z <- consumeR:::standardize_z(satisfaction_5pt)
nps_z <- consumeR:::standardize_z(nps_100pt)

print("Satisfaction z-scores:")
print(sat_z$standardized)
print(sat_z$meta)

print("\nNPS z-scores:")
print(nps_z$standardized)
print(nps_z$meta)

# Now both are on the same scale and can be averaged
composite_z <- (sat_z$standardized + nps_z$standardized) / 2
print("\nComposite (z-score method):")
print(composite_z)


## Example 6: Range standardization to [0,1] ----

sat_01 <- consumeR:::standardize_range(satisfaction_5pt, new_min = 0, new_max = 1)
nps_01 <- consumeR:::standardize_range(nps_100pt, new_min = 0, new_max = 1)

print("\nSatisfaction [0,1]:")
print(sat_01$standardized)

print("\nNPS [0,1]:")
print(nps_01$standardized)

composite_01 <- (sat_01$standardized + nps_01$standardized) / 2
print("\nComposite (0-1 scale):")
print(composite_01)


## Example 7: Scale conversion (e.g., all to 1-7 Likert) ----

# Convert both items to common 1-7 scale
sat_7pt <- consumeR:::standardize_scale(
  satisfaction_5pt,
  target_min = 1,
  target_max = 7
)

nps_7pt <- consumeR:::standardize_scale(
  nps_100pt,
  target_min = 1,
  target_max = 7
)

print("\nSatisfaction converted to 1-7:")
print(sat_7pt$standardized)

print("\nNPS converted to 1-7:")
print(nps_7pt$standardized)

composite_7pt <- (sat_7pt$standardized + nps_7pt$standardized) / 2
print("\nComposite (1-7 scale):")
print(composite_7pt)


## Example 8: Batch standardization with standardize_multiple() ----

# Create a data frame with items on different scales
df <- data.frame(
  satisfaction_5pt = c(1, 2, 3, 4, 5),
  nps_100pt = c(0, 25, 50, 75, 100),
  likelihood_7pt = c(1, 2.5, 4, 5.5, 7)
)

# Option A: Standardize all to z-scores
result_z <- consumeR:::standardize_multiple(
  data = df,
  vars = c("satisfaction_5pt", "nps_100pt", "likelihood_7pt"),
  method = "z-score"
)

print("\nAll items as z-scores:")
print(result_z$data)

# Create composite by averaging
df$composite_z <- rowMeans(result_z$data)
print("\nComposite from z-scores:")
print(df$composite_z)


# Option B: Standardize all to 1-7 scale
result_7pt <- consumeR:::standardize_multiple(
  data = df,
  vars = c("satisfaction_5pt", "nps_100pt", "likelihood_7pt"),
  method = "scale",
  target_min = 1,
  target_max = 7
)

print("\nAll items on 1-7 scale:")
print(result_7pt$data)

# Create composite by averaging
df$composite_7pt <- rowMeans(result_7pt$data)
print("\nComposite from 1-7 standardization:")
print(df$composite_7pt)


# Check metadata for transparency
print("\nMetadata for each variable:")
print(result_7pt$meta$satisfaction_5pt)
print(result_7pt$meta$nps_100pt)


# ============================================================================
# PART 3: Peer-Review-Ready Workflow
# ============================================================================

## Example 9: Complete workflow for creating a composite from mixed scales ----

# Scenario: Creating a "Customer Satisfaction Index" from 3 items on different scales

# Raw data
df_customer <- data.frame(
  id = 1:100,
  overall_satisfaction_5pt = sample(1:5, 100, replace = TRUE),
  recommend_likelihood_10pt = sample(1:10, 100, replace = TRUE),
  nps_100pt = sample(0:100, 100, replace = TRUE)
)

# Standardize all items to 1-7 scale (transparent, interpretable)
standardized <- consumeR:::standardize_multiple(
  data = df_customer,
  vars = c("overall_satisfaction_5pt", "recommend_likelihood_10pt", "nps_100pt"),
  method = "scale",
  target_min = 1,
  target_max = 7
)

# Create composite
df_customer$satisfaction_index <- rowMeans(standardized$data[, c(
  "overall_satisfaction_5pt",
  "recommend_likelihood_10pt",
  "nps_100pt"
)])

# Methods text for peer review:
cat("\n\nMETHODS TEXT FOR PAPER:\n")
cat("======================\n")
cat("A Customer Satisfaction Index was created by standardizing three items\n")
cat("to a common 1-7 scale using min-max transformation:\n")
cat("- Overall satisfaction (originally 1-5 scale)\n")
cat("- Likelihood to recommend (originally 1-10 scale)\n")
cat("- Net Promoter Score (originally 0-100 scale)\n")
cat("The three standardized items were then averaged to create the composite index.\n")
cat("\nOriginal scale ranges:\n")
cat(sprintf("  Overall satisfaction: [%d, %d]\n",
            standardized$meta$overall_satisfaction_5pt$original_min,
            standardized$meta$overall_satisfaction_5pt$original_max))
cat(sprintf("  Recommend likelihood: [%d, %d]\n",
            standardized$meta$recommend_likelihood_10pt$original_min,
            standardized$meta$recommend_likelihood_10pt$original_max))
cat(sprintf("  NPS: [%d, %d]\n",
            standardized$meta$nps_100pt$original_min,
            standardized$meta$nps_100pt$original_max))

cat("\nFinal composite: M =", round(mean(df_customer$satisfaction_index), 2),
    ", SD =", round(sd(df_customer$satisfaction_index), 2), "\n")


# ============================================================================
# SUMMARY
# ============================================================================
cat("\n\n======================\n")
cat("SUMMARY OF L1 HELPERS\n")
cat("======================\n\n")

cat("SPSS Labelled Data Helpers:\n")
cat("  extract_value_labels()         - Get value->label mappings\n")
cat("  identify_problematic_values()  - Find 'Don't know', 'Refused', etc.\n")
cat("  recode_to_na()                 - Convert problematic values to NA with logging\n\n")

cat("Standardization Helpers:\n")
cat("  standardize_z()                - Z-score standardization (mean=0, sd=1)\n")
cat("  standardize_range()            - Min-max to custom range (e.g., [0,1])\n")
cat("  standardize_scale()            - Convert to specific scale (e.g., 1-7)\n")
cat("  standardize_multiple()         - Batch standardize multiple variables\n\n")

cat("All functions include metadata for transparency and peer review.\n")
cat("These are L1 internal helpers - they will be used by L2/L3 user-facing functions.\n\n")
