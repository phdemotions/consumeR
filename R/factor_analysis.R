#' Perform Exploratory Factor Analysis with Beautiful Visualizations
#'
#' This function performs exploratory factor analysis (EFA) to understand the
#' underlying structure of your survey items. It uses tidyverse principles for
#' maximum clarity and creates publication-ready ggplot2 visualizations.
#'
#' Think of EFA like sorting Cloud 9 products into departments - it finds which
#' items naturally group together based on how people respond to them.
#'
#' @param data A data frame (tibble) containing your survey items
#' @param items Character vector of column names for items to analyze
#' @param n_factors Integer. Number of factors to extract. If NULL (default),
#'   the function will suggest the optimal number based on eigenvalues.
#' @param rotation Character string. Rotation method: "varimax" (default),
#'   "promax", "oblimin", or "none"
#' @param create_plots Logical. Should visualizations be created? Default TRUE
#' @param item_labels Optional named vector for readable item labels
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{factor_loadings}: Tibble with loadings for each item
#'     \item \code{scree_plot}: ggplot2 scree plot (if create_plots = TRUE)
#'     \item \code{loading_plot}: ggplot2 heatmap of loadings
#'     \item \code{variance_explained}: Tibble showing % variance per factor
#'     \item \code{interpretation}: Plain English summary
#'     \item \code{suggested_factors}: Recommended number of factors
#'   }
#'
#' @details
#' ## What is Exploratory Factor Analysis?
#' EFA identifies groups of items that correlate highly with each other.
#' Each group represents an underlying "factor" or dimension.
#'
#' For example, if you have 12 survey questions, EFA might reveal they
#' actually measure 3 different constructs (e.g., satisfaction, loyalty, value).
#'
#' ## How Many Factors?
#' The function uses the Kaiser criterion (eigenvalues > 1) to suggest
#' the number of factors. You can also specify n_factors manually.
#'
#' ## Rotation Methods:
#' - **varimax**: Assumes factors are uncorrelated (most common)
#' - **promax**: Allows factors to correlate
#' - **oblimin**: Another oblique rotation
#' - **none**: No rotation (harder to interpret)
#'
#' ## Interpreting Loadings:
#' - Loading > 0.70: Strong relationship
#' - Loading > 0.50: Moderate relationship
#' - Loading > 0.30: Weak but acceptable
#' - Loading < 0.30: Item doesn't belong to this factor
#'
#' @examples
#' \dontrun{
#' # Example: Analyze customer experience survey
#' # Suppose we have 9 items that might measure 3 dimensions
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' # Run EFA to discover factor structure
#' efa_results <- perform_efa(
#'   data = customer_data,
#'   items = c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"),
#'   item_labels = c(
#'     q1 = "Store cleanliness",
#'     q2 = "Product selection",
#'     q3 = "Staff friendliness",
#'     # ... etc
#'   )
#' )
#'
#' # View the scree plot
#' print(efa_results$scree_plot)
#'
#' # View loading heatmap
#' print(efa_results$loading_plot)
#'
#' # Get interpretation
#' cat(efa_results$interpretation)
#' }
#'
#' @export
#' @importFrom dplyr tibble mutate select arrange filter row_number
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_tile geom_text
#'   scale_fill_gradient2 theme_minimal labs theme element_text
perform_efa <- function(data,
                       items,
                       n_factors = NULL,
                       rotation = "varimax",
                       create_plots = TRUE,
                       item_labels = NULL) {

  # Load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install with: install.packages('dplyr')")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with: install.packages('ggplot2')")
  }

  library(dplyr, warn.conflicts = FALSE)
  library(ggplot2, warn.conflicts = FALSE)

  message("\n==== EXPLORATORY FACTOR ANALYSIS ====")
  message("Using tidyverse workflow for maximum clarity")

  # Step 1: Input Validation
  # -------------------------
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data frame or tibble")
  }

  if (length(items) < 3) {
    stop("Error: Need at least 3 items for factor analysis. ",
         "You provided ", length(items))
  }

  missing_items <- items[!items %in% names(data)]
  if (length(missing_items) > 0) {
    stop("Error: Items not found: ", paste(missing_items, collapse = ", "))
  }

  # Step 2: Prepare Data Using Tidyverse
  # -------------------------------------
  message("\nPreparing data using tidyverse pipelines...")

  # Select only the items we need and remove missing values
  item_data <- data %>%
    select(all_of(items)) %>%
    na.omit()

  n_complete <- nrow(item_data)
  n_missing <- nrow(data) - n_complete

  message("  Sample size: ", n_complete, " complete responses")
  if (n_missing > 0) {
    message("  Removed: ", n_missing, " cases with missing values")
  }

  # Check sample size adequacy
  if (n_complete < 50) {
    warning("Small sample size (n=", n_complete, "). ",
            "Recommend n ≥ 100 for stable factor analysis.")
  }

  # Step 3: Check KMO and Bartlett's Test
  # --------------------------------------
  message("\nChecking data suitability for factor analysis...")

  # Calculate correlation matrix
  cor_matrix <- cor(item_data)

  # Simple KMO approximation
  # (Full KMO calculation would require additional package)
  # Here we check if correlations are strong enough
  mean_cor <- mean(cor_matrix[lower.tri(cor_matrix)])

  message("  Average inter-item correlation: ", round(mean_cor, 3))

  if (mean_cor < 0.30) {
    warning("Low inter-item correlations (< 0.30). ",
            "Factor analysis may not be appropriate.")
  }

  # Step 4: Determine Number of Factors
  # ------------------------------------
  message("\nDetermining optimal number of factors...")

  # Calculate eigenvalues from correlation matrix
  eigenvalues <- eigen(cor_matrix)$values

  # Kaiser criterion: factors with eigenvalue > 1
  n_suggested <- sum(eigenvalues > 1)

  message("  Eigenvalues > 1: ", n_suggested, " factors")
  message("  Eigenvalues: ", paste(round(eigenvalues, 2), collapse = ", "))

  # Use suggested number if not specified
  if (is.null(n_factors)) {
    n_factors <- n_suggested
    message("  Using suggested ", n_factors, " factors")
  } else {
    message("  Using user-specified ", n_factors, " factors")
  }

  if (n_factors < 1) {
    stop("Error: Need at least 1 factor")
  }

  if (n_factors > length(items)) {
    stop("Error: Cannot extract more factors (", n_factors,
         ") than items (", length(items), ")")
  }

  # Step 5: Perform Factor Analysis
  # --------------------------------
  message("\nPerforming factor analysis with ", rotation, " rotation...")

  # Use factanal for factor analysis
  fa_result <- factanal(item_data,
                        factors = n_factors,
                        rotation = rotation,
                        scores = "regression")

  # Extract loadings as matrix
  loadings_matrix <- fa_result$loadings[]

  # Step 6: Create Loadings Tibble (Tidyverse Style)
  # -------------------------------------------------
  message("\nCreating tidy factor loading table...")

  # Convert loadings to long-format tibble
  loadings_tidy <- loadings_matrix %>%
    as.data.frame() %>%
    tibble::rownames_to_column("item") %>%
    tidyr::pivot_longer(
      cols = -item,
      names_to = "factor",
      values_to = "loading"
    ) %>%
    mutate(
      # Add absolute loading for sorting
      abs_loading = abs(loading),
      # Categorize loading strength
      strength = case_when(
        abs_loading >= 0.70 ~ "Strong (≥0.70)",
        abs_loading >= 0.50 ~ "Moderate (0.50-0.69)",
        abs_loading >= 0.30 ~ "Weak (0.30-0.49)",
        TRUE ~ "Very Weak (<0.30)"
      ),
      # Add item labels if provided
      label = if (!is.null(item_labels)) {
        ifelse(item %in% names(item_labels),
               item_labels[item],
               item)
      } else {
        item
      }
    ) %>%
    arrange(factor, desc(abs_loading))

  # Step 7: Calculate Variance Explained
  # -------------------------------------
  message("\nCalculating variance explained...")

  # Sum of squared loadings for each factor
  variance_explained <- loadings_tidy %>%
    group_by(factor) %>%
    summarise(
      ss_loadings = sum(loading^2),
      proportion_var = ss_loadings / length(items),
      percent_var = proportion_var * 100,
      .groups = "drop"
    ) %>%
    mutate(
      cumulative_var = cumsum(proportion_var),
      cumulative_percent = cumsum(percent_var)
    )

  message("  Total variance explained: ",
          round(sum(variance_explained$percent_var), 1), "%")

  # Step 8: Create Visualizations
  # ------------------------------
  scree_plot <- NULL
  loading_plot <- NULL

  if (create_plots) {
    message("\nCreating publication-ready visualizations...")

    # SCREE PLOT
    # Shows eigenvalues to help determine number of factors
    scree_data <- tibble(
      factor_number = 1:length(eigenvalues),
      eigenvalue = eigenvalues
    )

    scree_plot <- ggplot(scree_data, aes(x = factor_number, y = eigenvalue)) +
      geom_line(color = "#2E86AB", size = 1) +
      geom_point(color = "#2E86AB", size = 3) +
      geom_hline(yintercept = 1, linetype = "dashed",
                 color = "#A23B72", size = 0.8) +
      annotate("text", x = max(scree_data$factor_number) * 0.8, y = 1.2,
               label = "Kaiser Criterion\n(Eigenvalue = 1)",
               color = "#A23B72", size = 3.5) +
      scale_x_continuous(breaks = 1:length(eigenvalues)) +
      labs(
        title = "Scree Plot: Determining Number of Factors",
        subtitle = "Factors above the line (eigenvalue > 1) are retained",
        x = "Factor Number",
        y = "Eigenvalue",
        caption = "Steep drops indicate meaningful factors"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray30"),
        panel.grid.minor = element_blank()
      )

    # LOADING HEATMAP
    # Shows which items load on which factors
    loading_plot <- ggplot(loadings_tidy,
                           aes(x = factor, y = label, fill = loading)) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(aes(label = round(loading, 2)),
                size = 3, color = "white", fontface = "bold") +
      scale_fill_gradient2(
        low = "#d7191c",      # Red for negative
        mid = "#ffffbf",      # Yellow for zero
        high = "#2b83ba",     # Blue for positive
        midpoint = 0,
        limits = c(-1, 1),
        name = "Loading"
      ) +
      labs(
        title = "Factor Loading Heatmap",
        subtitle = paste("Rotation:", rotation),
        x = "Factor",
        y = "Item",
        caption = "Darker colors = stronger loadings | Values show exact loadings"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray30"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 9),
        panel.grid = element_blank()
      )

    message("  ✓ Scree plot created")
    message("  ✓ Loading heatmap created")
  }

  # Step 9: Generate Interpretation
  # --------------------------------
  message("\nGenerating interpretation...")

  # Find primary factor for each item (highest absolute loading)
  primary_loadings <- loadings_tidy %>%
    group_by(item) %>%
    filter(abs_loading == max(abs_loading)) %>%
    ungroup()

  interpretation <- paste0(
    "EXPLORATORY FACTOR ANALYSIS RESULTS\n",
    "====================================\n\n",
    "Number of factors extracted: ", n_factors, "\n",
    "Rotation method: ", rotation, "\n",
    "Sample size: ", n_complete, "\n\n",
    "VARIANCE EXPLAINED:\n"
  )

  for (i in 1:nrow(variance_explained)) {
    interpretation <- paste0(interpretation,
      "  ", variance_explained$factor[i], ": ",
      round(variance_explained$percent_var[i], 1), "% ",
      "(cumulative: ", round(variance_explained$cumulative_percent[i], 1), "%)\n"
    )
  }

  interpretation <- paste0(interpretation,
    "\nTOTAL VARIANCE EXPLAINED: ",
    round(sum(variance_explained$percent_var), 1), "%\n\n"
  )

  # Add factor interpretation
  interpretation <- paste0(interpretation,
    "ITEM-TO-FACTOR ASSIGNMENTS:\n"
  )

  for (f in unique(primary_loadings$factor)) {
    factor_items <- primary_loadings %>%
      filter(factor == f) %>%
      arrange(desc(abs_loading))

    interpretation <- paste0(interpretation,
      "\n", f, " (", nrow(factor_items), " items):\n"
    )

    for (i in 1:nrow(factor_items)) {
      interpretation <- paste0(interpretation,
        "  - ", factor_items$label[i],
        " (loading = ", round(factor_items$loading[i], 3), ")\n"
      )
    }
  }

  # Add recommendations
  interpretation <- paste0(interpretation,
    "\nRECOMMENDATIONS:\n"
  )

  if (n_factors != n_suggested) {
    interpretation <- paste0(interpretation,
      "• Consider using ", n_suggested, " factors based on eigenvalues\n"
    )
  }

  # Check for weak loadings
  weak_items <- loadings_tidy %>%
    group_by(item) %>%
    summarise(max_loading = max(abs_loading), .groups = "drop") %>%
    filter(max_loading < 0.50)

  if (nrow(weak_items) > 0) {
    interpretation <- paste0(interpretation,
      "• Consider removing weak items (max loading < 0.50):\n"
    )
    for (i in 1:nrow(weak_items)) {
      interpretation <- paste0(interpretation,
        "  - ", weak_items$item[i],
        " (max loading = ", round(weak_items$max_loading[i], 3), ")\n"
      )
    }
  }

  if (sum(variance_explained$percent_var) < 50) {
    interpretation <- paste0(interpretation,
      "• Low total variance explained (< 50%). ",
      "Consider adding more items or revising existing ones.\n"
    )
  }

  message("\n", interpretation)

  # Step 10: Return Results
  # ------------------------
  results <- list(
    n_factors = n_factors,
    suggested_factors = n_suggested,
    rotation = rotation,
    n_observations = n_complete,
    factor_loadings = loadings_tidy,
    variance_explained = variance_explained,
    scree_plot = scree_plot,
    loading_plot = loading_plot,
    interpretation = interpretation,
    eigenvalues = eigenvalues,
    raw_loadings = loadings_matrix
  )

  class(results) <- c("efa_results", "list")

  message("\n==== ANALYSIS COMPLETE ====\n")

  return(results)
}


#' Print Method for EFA Results
#'
#' @param x An efa_results object
#' @param ... Additional arguments (not used)
#' @export
print.efa_results <- function(x, ...) {
  cat(x$interpretation)
  invisible(x)
}
