test_that("run_mlm works with random intercept model", {
  skip_if_not_installed("lme4")

  set.seed(42)
  n_groups <- 20
  n_per_group <- 10

  df <- expand.grid(
    group = 1:n_groups,
    obs = 1:n_per_group
  )

  # Create group-level random effects
  group_effects <- rnorm(n_groups, mean = 0, sd = 2)
  df$group_effect <- group_effects[df$group]

  # Add individual-level predictors and outcome
  df$x <- rnorm(nrow(df))
  df$y <- 5 + 0.5 * df$x + df$group_effect + rnorm(nrow(df), sd = 1)

  result <- run_mlm(y ~ x + (1 | group), data = df)

  expect_s3_class(result, "mlm_result")
  expect_type(result, "list")
  expect_named(result, c("fixed_effects", "random_effects", "icc",
                         "model_fit", "n_groups", "n_obs", "formula",
                         "REML", "interpretation", "lmer_fit"))

  expect_s3_class(result$fixed_effects, "tbl_df")
  expect_s3_class(result$random_effects, "tbl_df")
  expect_equal(result$n_groups["group"], n_groups)
  expect_equal(result$n_obs, nrow(df))
})

test_that("run_mlm works with random slope model", {
  skip_if_not_installed("lme4")

  set.seed(42)
  n_groups <- 15
  n_per_group <- 20

  df <- expand.grid(
    group = 1:n_groups,
    obs = 1:n_per_group
  )

  # Random intercepts and slopes
  group_intercepts <- rnorm(n_groups, mean = 0, sd = 1.5)
  group_slopes <- rnorm(n_groups, mean = 0, sd = 0.5)

  df$x <- rnorm(nrow(df))
  df$y <- 5 + group_intercepts[df$group] +
    (0.5 + group_slopes[df$group]) * df$x +
    rnorm(nrow(df), sd = 1)

  result <- run_mlm(y ~ x + (x | group), data = df)

  expect_s3_class(result, "mlm_result")

  # Should have random intercept and slope
  random_terms <- result$random_effects$term[result$random_effects$group == "group"]
  expect_true("(Intercept)" %in% random_terms)
  expect_true("x" %in% random_terms)
})

test_that("run_mlm validates inputs", {
  skip_if_not_installed("lme4")

  df <- data.frame(
    y = rnorm(50),
    x = rnorm(50),
    group = rep(1:10, each = 5)
  )

  expect_error(
    run_mlm(y ~ x + (1 | group), data = "not a df"),
    "must be a data frame"
  )

  expect_error(
    run_mlm("not a formula", data = df),
    "must be a formula"
  )
})

test_that("run_mlm calculates ICC correctly", {
  skip_if_not_installed("lme4")

  set.seed(42)
  n_groups <- 25
  n_per_group <- 10

  df <- expand.grid(
    group = 1:n_groups,
    obs = 1:n_per_group
  )

  # Strong group effects (high ICC)
  group_effects <- rnorm(n_groups, mean = 0, sd = 3)
  df$y <- 10 + group_effects[df$group] + rnorm(nrow(df), sd = 1)

  result <- run_mlm(y ~ 1 + (1 | group), data = df)

  # ICC should be substantial (> 0.10) due to strong group effects
  expect_true(result$icc["group"] > 0.10)
  expect_true(result$icc["group"] < 1)
})

test_that("run_mlm works with crossed random effects", {
  skip_if_not_installed("lme4")

  set.seed(42)
  n_consumers <- 20
  n_products <- 15

  df <- expand.grid(
    consumer = 1:n_consumers,
    product = 1:n_products
  )

  consumer_effects <- rnorm(n_consumers, sd = 1)
  product_effects <- rnorm(n_products, sd = 1.5)

  df$rating <- 5 +
    consumer_effects[df$consumer] +
    product_effects[df$product] +
    rnorm(nrow(df), sd = 0.5)

  result <- run_mlm(rating ~ 1 + (1 | consumer) + (1 | product), data = df)

  expect_s3_class(result, "mlm_result")
  expect_equal(length(result$icc), 2)
  expect_true("consumer" %in% names(result$icc))
  expect_true("product" %in% names(result$icc))
})

test_that("run_mlm extracts fixed effects correctly", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(
    group = 1:15,
    obs = 1:10
  )
  df$x <- rnorm(nrow(df))
  df$y <- 5 + 0.7 * df$x + rnorm(15)[df$group] + rnorm(nrow(df))

  result <- run_mlm(y ~ x + (1 | group), data = df)

  fixed <- result$fixed_effects

  expect_true("term" %in% names(fixed))
  expect_true("estimate" %in% names(fixed))
  expect_true("ci_lower" %in% names(fixed))
  expect_true("ci_upper" %in% names(fixed))
  expect_true("p_value" %in% names(fixed))
  expect_true("significant" %in% names(fixed))

  # Should have intercept and x
  expect_equal(nrow(fixed), 2)
})

test_that("run_mlm extracts random effects correctly", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(
    group = 1:20,
    obs = 1:8
  )
  df$y <- 10 + rnorm(20, sd = 2)[df$group] + rnorm(nrow(df), sd = 1)

  result <- run_mlm(y ~ 1 + (1 | group), data = df)

  random <- result$random_effects

  expect_true("group" %in% names(random))
  expect_true("term" %in% names(random))
  expect_true("variance" %in% names(random))
  expect_true("sd" %in% names(random))

  # Should have group variance and residual variance
  expect_true("group" %in% random$group)
  expect_true("Residual" %in% random$group)
})

test_that("run_mlm supports REML and ML estimation", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(
    group = 1:10,
    obs = 1:10
  )
  df$x <- rnorm(nrow(df))
  df$y <- 5 + 0.5 * df$x + rnorm(10)[df$group] + rnorm(nrow(df))

  result_reml <- run_mlm(y ~ x + (1 | group), data = df, REML = TRUE)
  result_ml <- run_mlm(y ~ x + (1 | group), data = df, REML = FALSE)

  expect_true(result_reml$REML)
  expect_false(result_ml$REML)

  # Both should produce results
  expect_s3_class(result_reml, "mlm_result")
  expect_s3_class(result_ml, "mlm_result")
})

test_that("run_mlm generates interpretation", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(
    group = 1:15,
    obs = 1:10
  )
  df$x <- rnorm(nrow(df))
  df$y <- 5 + 0.6 * df$x + rnorm(15, sd = 1.5)[df$group] + rnorm(nrow(df))

  result <- run_mlm(y ~ x + (1 | group), data = df)

  expect_type(result$interpretation, "character")
  expect_true(nchar(result$interpretation) > 0)
  expect_true(grepl("fixed effect|clustering", result$interpretation,
                    ignore.case = TRUE))
})

test_that("icc_calculate works with formula", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(
    store = 1:20,
    customer = 1:10
  )

  # Strong store effects
  store_effects <- rnorm(20, sd = 2)
  df$satisfaction <- 7 + store_effects[df$store] + rnorm(nrow(df), sd = 1)

  icc_result <- icc_calculate(satisfaction ~ (1 | store), data = df)

  expect_type(icc_result, "list")
  expect_true("icc" %in% names(icc_result))
  expect_true("variance_components" %in% names(icc_result))
  expect_true("interpretation" %in% names(icc_result))

  # ICC should be > 0.10 (substantial)
  expect_true(icc_result$icc["store"] > 0.10)
})

test_that("icc_calculate works with mlm_result", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(
    group = 1:15,
    obs = 1:10
  )
  df$y <- 5 + rnorm(15, sd = 1)[df$group] + rnorm(nrow(df), sd = 2)

  model <- run_mlm(y ~ 1 + (1 | group), data = df)
  icc_result <- icc_calculate(model)

  expect_type(icc_result, "list")
  expect_true("icc" %in% names(icc_result))
  expect_equal(icc_result$icc, model$icc)
})

test_that("icc_calculate provides correct interpretation", {
  skip_if_not_installed("lme4")

  set.seed(42)

  # Minimal clustering (ICC < 0.05)
  df1 <- expand.grid(group = 1:50, obs = 1:10)
  df1$y <- 5 + rnorm(50, sd = 0.5)[df1$group] + rnorm(nrow(df1), sd = 5)

  icc1 <- icc_calculate(y ~ (1 | group), data = df1)
  expect_true(grepl("Minimal|OLS", icc1$interpretation))

  # Substantial clustering (ICC > 0.10)
  df2 <- expand.grid(group = 1:20, obs = 1:10)
  df2$y <- 5 + rnorm(20, sd = 3)[df2$group] + rnorm(nrow(df2), sd = 1)

  icc2 <- icc_calculate(y ~ (1 | group), data = df2)
  expect_true(grepl("Substantial|required", icc2$interpretation))
})

test_that("icc_calculate validates inputs", {
  expect_error(
    icc_calculate("not a formula or model"),
    "must be either mlm_result object or formula"
  )

  expect_error(
    icc_calculate(y ~ (1 | group)),
    "data.*required"
  )
})

test_that("tidy_mlm extracts results correctly", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(
    group = 1:15,
    obs = 1:10
  )
  df$x <- rnorm(nrow(df))
  df$y <- 5 + 0.5 * df$x + rnorm(15)[df$group] + rnorm(nrow(df))

  model <- run_mlm(y ~ x + (1 | group), data = df)
  tidy_result <- tidy_mlm(model)

  expect_type(tidy_result, "list")
  expect_true("fixed_effects" %in% names(tidy_result))
  expect_true("random_effects" %in% names(tidy_result))
  expect_true("icc" %in% names(tidy_result))
  expect_true("model_fit" %in% names(tidy_result))

  expect_s3_class(tidy_result$fixed_effects, "tbl_df")
  expect_s3_class(tidy_result$random_effects, "tbl_df")
  expect_s3_class(tidy_result$icc, "tbl_df")
})

test_that("tidy_mlm validates inputs", {
  expect_error(
    tidy_mlm("not a mlm_result"),
    "must be output from run_mlm"
  )
})

test_that("tidy_mlm respects component selection", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(group = 1:10, obs = 1:10)
  df$y <- 5 + rnorm(10)[df$group] + rnorm(nrow(df))

  model <- run_mlm(y ~ 1 + (1 | group), data = df)

  # Only fixed effects
  tidy_fixed <- tidy_mlm(model, fixed = TRUE, random = FALSE,
                         icc = FALSE, fit = FALSE)
  expect_equal(names(tidy_fixed), "fixed_effects")

  # Only ICC
  tidy_icc <- tidy_mlm(model, fixed = FALSE, random = FALSE,
                       icc = TRUE, fit = FALSE)
  expect_equal(names(tidy_icc), "icc")
})

test_that("mlm_assumptions checks residuals normality", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(group = 1:15, obs = 1:10)
  df$y <- 5 + rnorm(15)[df$group] + rnorm(nrow(df))

  model <- run_mlm(y ~ 1 + (1 | group), data = df)
  assumptions <- mlm_assumptions(model, plot = FALSE)

  expect_type(assumptions, "list")
  expect_true("residuals_normality" %in% names(assumptions))
  expect_true("random_effects_normality" %in% names(assumptions))
  expect_true("summary" %in% names(assumptions))

  expect_type(assumptions$summary, "character")
})

test_that("mlm_assumptions validates inputs", {
  expect_error(
    mlm_assumptions("not a mlm_result"),
    "must be output from run_mlm"
  )
})

test_that("print.mlm_result works", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(
    group = 1:12,
    obs = 1:10
  )
  df$x <- rnorm(nrow(df))
  df$y <- 5 + 0.5 * df$x + rnorm(12, sd = 1.5)[df$group] + rnorm(nrow(df))

  model <- run_mlm(y ~ x + (1 | group), data = df)

  output <- capture.output(print(model))

  expect_true(any(grepl("Multilevel Model", output)))
  expect_true(any(grepl("Formula", output)))
  expect_true(any(grepl("Intraclass Correlation", output)))
  expect_true(any(grepl("Fixed Effects", output)))
  expect_true(any(grepl("Random Effects", output)))
  expect_true(any(grepl("Model Fit", output)))
  expect_true(any(grepl("Interpretation", output)))
})

test_that("run_mlm handles multiple predictors", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(
    group = 1:20,
    obs = 1:10
  )
  df$x1 <- rnorm(nrow(df))
  df$x2 <- rnorm(nrow(df))
  df$x3 <- rnorm(nrow(df))
  df$y <- 5 + 0.5 * df$x1 + 0.3 * df$x2 - 0.2 * df$x3 +
    rnorm(20)[df$group] + rnorm(nrow(df))

  result <- run_mlm(y ~ x1 + x2 + x3 + (1 | group), data = df)

  expect_s3_class(result, "mlm_result")
  expect_equal(nrow(result$fixed_effects), 4)  # Intercept + 3 predictors
})

test_that("run_mlm works with nested random effects", {
  skip_if_not_installed("lme4")

  set.seed(42)
  n_regions <- 10
  n_stores_per_region <- 5
  n_obs_per_store <- 8

  df <- expand.grid(
    region = 1:n_regions,
    store_in_region = 1:n_stores_per_region,
    obs = 1:n_obs_per_store
  )

  # Create nested structure
  df$store <- paste(df$region, df$store_in_region, sep = "_")

  region_effects <- rnorm(n_regions, sd = 1.5)
  store_effects <- rnorm(n_regions * n_stores_per_region, sd = 1)

  df$y <- 10 +
    region_effects[df$region] +
    store_effects[as.numeric(factor(df$store))] +
    rnorm(nrow(df), sd = 0.5)

  result <- run_mlm(y ~ 1 + (1 | region/store), data = df)

  expect_s3_class(result, "mlm_result")
  # Should have region and store:region
  expect_gte(length(result$icc), 1)
})

test_that("run_mlm significance flags are correct", {
  skip_if_not_installed("lme4")

  set.seed(42)
  df <- expand.grid(
    group = 1:25,
    obs = 1:15
  )

  # Strong effect for x1, no effect for x2
  df$x1 <- rnorm(nrow(df))
  df$x2 <- rnorm(nrow(df))
  df$y <- 5 + 0.8 * df$x1 + 0.01 * df$x2 +
    rnorm(25, sd = 1)[df$group] + rnorm(nrow(df), sd = 1)

  result <- run_mlm(y ~ x1 + x2 + (1 | group), data = df)

  fixed <- result$fixed_effects

  expect_true("significant" %in% names(fixed))
  expect_type(fixed$significant, "logical")

  # x1 should likely be significant, x2 likely not
  x1_sig <- fixed$significant[fixed$term == "x1"]
  x2_sig <- fixed$significant[fixed$term == "x2"]

  # At least x1 should be significant given strong effect
  expect_true(x1_sig)
})
