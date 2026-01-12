test_that("mediation_parallel works with two mediators", {
  set.seed(42)
  n <- 200

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  # Create mediation structure
  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$x
  df$y <- df$y + 0.3 * df$m1 + 0.2 * df$m2 + 0.1 * df$x

  result <- mediation_parallel(
    data = df,
    x = "x",
    mediators = c("m1", "m2"),
    y = "y",
    boot_samples = 1000,
    seed = 123
  )

  expect_s3_class(result, "parallel_mediation")
  expect_type(result, "list")
  expect_named(result, c("paths", "specific_indirect", "total_indirect",
                         "direct_effect", "total_effect", "contrasts",
                         "boot_samples", "boot_method", "conf_level",
                         "interpretation", "variables"))

  expect_s3_class(result$specific_indirect, "tbl_df")
  expect_equal(nrow(result$specific_indirect), 2)
  expect_s3_class(result$total_indirect, "tbl_df")
})

test_that("mediation_parallel calculates specific indirect effects", {
  set.seed(42)
  n <- 250

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  # Strong effect through m1, weak through m2
  df$m1 <- df$m1 + 0.7 * df$x
  df$m2 <- df$m2 + 0.3 * df$x
  df$y <- df$y + 0.6 * df$m1 + 0.1 * df$m2

  result <- mediation_parallel(
    data = df,
    x = "x",
    mediators = c("m1", "m2"),
    y = "y",
    boot_samples = 1000,
    seed = 42
  )

  specific <- result$specific_indirect

  expect_true("mediator" %in% names(specific))
  expect_true("indirect_effect" %in% names(specific))
  expect_true("ci_lower" %in% names(specific))
  expect_true("ci_upper" %in% names(specific))
  expect_true("significant" %in% names(specific))

  # m1 should have larger effect than m2
  m1_effect <- specific$indirect_effect[specific$mediator == "m1"]
  m2_effect <- specific$indirect_effect[specific$mediator == "m2"]
  expect_true(abs(m1_effect) > abs(m2_effect))
})

test_that("mediation_parallel validates inputs", {
  df <- data.frame(
    x = rnorm(50),
    m1 = rnorm(50),
    m2 = rnorm(50),
    y = rnorm(50)
  )

  expect_error(
    mediation_parallel(data = "not a df", x = "x", mediators = c("m1", "m2"), y = "y"),
    "must be a data frame"
  )

  expect_error(
    mediation_parallel(data = df, x = "x", mediators = "m1", y = "y"),
    "at least 2 mediators"
  )

  expect_error(
    mediation_parallel(data = df, x = "missing", mediators = c("m1", "m2"), y = "y"),
    "not found in data"
  )

  expect_error(
    mediation_parallel(data = df, x = "x", mediators = c("m1", "m2"), y = "y",
                      boot_samples = 100),
    "at least 1000"
  )
})

test_that("mediation_parallel works with three mediators", {
  set.seed(42)
  n <- 300

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    m3 = rnorm(n),
    y = rnorm(n)
  )

  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$x
  df$m3 <- df$m3 + 0.3 * df$x
  df$y <- df$y + 0.3 * df$m1 + 0.2 * df$m2 + 0.1 * df$m3

  result <- mediation_parallel(
    data = df,
    x = "x",
    mediators = c("m1", "m2", "m3"),
    y = "y",
    boot_samples = 1000,
    seed = 42
  )

  expect_equal(nrow(result$specific_indirect), 3)
  expect_equal(result$specific_indirect$mediator, c("m1", "m2", "m3"))
})

test_that("mediation_parallel calculates contrasts", {
  set.seed(42)
  n <- 200

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  df$m1 <- df$m1 + 0.6 * df$x
  df$m2 <- df$m2 + 0.3 * df$x
  df$y <- df$y + 0.5 * df$m1 + 0.2 * df$m2

  result <- mediation_parallel(
    data = df,
    x = "x",
    mediators = c("m1", "m2"),
    y = "y",
    boot_samples = 1000,
    seed = 42
  )

  expect_s3_class(result$contrasts, "tbl_df")
  expect_true("contrast" %in% names(result$contrasts))
  expect_true("difference" %in% names(result$contrasts))
  expect_true("significant" %in% names(result$contrasts))

  # Should have one contrast for two mediators
  expect_equal(nrow(result$contrasts), 1)
})

test_that("mediation_parallel supports covariates", {
  set.seed(42)
  n <- 200

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n),
    cov1 = rnorm(n),
    cov2 = rnorm(n)
  )

  df$m1 <- df$m1 + 0.5 * df$x + 0.2 * df$cov1
  df$m2 <- df$m2 + 0.4 * df$x + 0.1 * df$cov2
  df$y <- df$y + 0.3 * df$m1 + 0.2 * df$m2 + 0.3 * df$cov1

  result <- mediation_parallel(
    data = df,
    x = "x",
    mediators = c("m1", "m2"),
    y = "y",
    covariates = c("cov1", "cov2"),
    boot_samples = 1000,
    seed = 42
  )

  expect_s3_class(result, "parallel_mediation")
  expect_equal(result$variables$covariates, c("cov1", "cov2"))
})

test_that("mediation_parallel supports different bootstrap methods", {
  set.seed(42)
  n <- 150

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$x
  df$y <- df$y + 0.3 * df$m1 + 0.2 * df$m2

  result_bca <- mediation_parallel(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, boot_method = "bca", seed = 42
  )

  result_perc <- mediation_parallel(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, boot_method = "perc", seed = 42
  )

  result_norm <- mediation_parallel(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, boot_method = "norm", seed = 42
  )

  expect_equal(result_bca$boot_method, "bca")
  expect_equal(result_perc$boot_method, "perc")
  expect_equal(result_norm$boot_method, "norm")
})

test_that("mediation_parallel is reproducible with seed", {
  set.seed(42)
  n <- 150

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$x
  df$y <- df$y + 0.3 * df$m1 + 0.2 * df$m2

  result1 <- mediation_parallel(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, seed = 999
  )

  result2 <- mediation_parallel(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, seed = 999
  )

  expect_equal(result1$specific_indirect$indirect_effect,
               result2$specific_indirect$indirect_effect)
  expect_equal(result1$specific_indirect$ci_lower,
               result2$specific_indirect$ci_lower)
})

test_that("mediation_serial works with two mediators", {
  set.seed(42)
  n <- 200

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  # Serial structure: X → M1 → M2 → Y
  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$m1 + 0.3 * df$x
  df$y <- df$y + 0.3 * df$m1 + 0.4 * df$m2 + 0.1 * df$x

  result <- mediation_serial(
    data = df,
    x = "x",
    mediators = c("m1", "m2"),
    y = "y",
    boot_samples = 1000,
    seed = 123
  )

  expect_s3_class(result, "serial_mediation")
  expect_type(result, "list")
  expect_named(result, c("paths", "specific_indirect", "total_indirect",
                         "direct_effect", "total_effect", "boot_samples",
                         "boot_method", "conf_level", "interpretation",
                         "variables"))

  expect_s3_class(result$specific_indirect, "tbl_df")
  # Should have 3 pathways: M1 only, M2 only, M1→M2
  expect_equal(nrow(result$specific_indirect), 3)
})

test_that("mediation_serial calculates pathway-specific effects", {
  set.seed(42)
  n <- 250

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  # Strong serial path
  df$m1 <- df$m1 + 0.6 * df$x
  df$m2 <- df$m2 + 0.5 * df$m1 + 0.2 * df$x
  df$y <- df$y + 0.2 * df$m1 + 0.5 * df$m2

  result <- mediation_serial(
    data = df,
    x = "x",
    mediators = c("m1", "m2"),
    y = "y",
    boot_samples = 1000,
    seed = 42
  )

  specific <- result$specific_indirect

  expect_true("pathway" %in% names(specific))
  expect_true("indirect_effect" %in% names(specific))
  expect_true("ci_lower" %in% names(specific))
  expect_true("ci_upper" %in% names(specific))
  expect_true("significant" %in% names(specific))

  # Check pathway labels
  expect_true(any(grepl("m1.*->.*y", specific$pathway, ignore.case = TRUE)))
  expect_true(any(grepl("m2.*->.*y", specific$pathway, ignore.case = TRUE)))
  expect_true(any(grepl("m1.*->.*m2.*->.*y", specific$pathway, ignore.case = TRUE)))
})

test_that("mediation_serial validates inputs", {
  df <- data.frame(
    x = rnorm(50),
    m1 = rnorm(50),
    m2 = rnorm(50),
    y = rnorm(50)
  )

  expect_error(
    mediation_serial(data = "not a df", x = "x", mediators = c("m1", "m2"), y = "y"),
    "must be a data frame"
  )

  expect_error(
    mediation_serial(data = df, x = "x", mediators = "m1", y = "y"),
    "at least 2 mediators"
  )

  expect_error(
    mediation_serial(data = df, x = "missing", mediators = c("m1", "m2"), y = "y"),
    "not found in data"
  )

  expect_error(
    mediation_serial(data = df, x = "x", mediators = c("m1", "m2"), y = "y",
                    boot_samples = 500),
    "at least 1000"
  )
})

test_that("mediation_serial supports covariates", {
  set.seed(42)
  n <- 200

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n),
    cov1 = rnorm(n)
  )

  df$m1 <- df$m1 + 0.5 * df$x + 0.2 * df$cov1
  df$m2 <- df$m2 + 0.4 * df$m1
  df$y <- df$y + 0.3 * df$m2

  result <- mediation_serial(
    data = df,
    x = "x",
    mediators = c("m1", "m2"),
    y = "y",
    covariates = "cov1",
    boot_samples = 1000,
    seed = 42
  )

  expect_s3_class(result, "serial_mediation")
  expect_equal(result$variables$covariates, "cov1")
})

test_that("mediation_serial is reproducible with seed", {
  set.seed(42)
  n <- 150

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$m1
  df$y <- df$y + 0.3 * df$m2

  result1 <- mediation_serial(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, seed = 888
  )

  result2 <- mediation_serial(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, seed = 888
  )

  expect_equal(result1$specific_indirect$indirect_effect,
               result2$specific_indirect$indirect_effect)
  expect_equal(result1$specific_indirect$ci_lower,
               result2$specific_indirect$ci_lower)
})

test_that("mediation_serial generates interpretation", {
  set.seed(42)
  n <- 200

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$m1
  df$y <- df$y + 0.3 * df$m2

  result <- mediation_serial(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, seed = 42
  )

  expect_type(result$interpretation, "character")
  expect_true(nchar(result$interpretation) > 0)
  expect_true(grepl("indirect|pathway", result$interpretation, ignore.case = TRUE))
})

test_that("print.parallel_mediation works", {
  set.seed(42)
  n <- 150

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$x
  df$y <- df$y + 0.3 * df$m1 + 0.2 * df$m2

  result <- mediation_parallel(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, seed = 42
  )

  output <- capture.output(print(result))

  expect_true(any(grepl("Parallel Mediation", output)))
  expect_true(any(grepl("Variables", output)))
  expect_true(any(grepl("Specific Indirect", output)))
  expect_true(any(grepl("Total Indirect", output)))
  expect_true(any(grepl("Interpretation", output)))
})

test_that("print.serial_mediation works", {
  set.seed(42)
  n <- 150

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$m1
  df$y <- df$y + 0.3 * df$m2

  result <- mediation_serial(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, seed = 42
  )

  output <- capture.output(print(result))

  expect_true(any(grepl("Serial Mediation", output)))
  expect_true(any(grepl("sequential", output)))
  expect_true(any(grepl("Indirect Pathways", output)))
  expect_true(any(grepl("Interpretation", output)))
})

test_that("mediation_parallel handles missing data", {
  set.seed(42)
  n <- 100

  df <- data.frame(
    x = c(rnorm(95), rep(NA, 5)),
    m1 = rnorm(100),
    m2 = rnorm(100),
    y = rnorm(100)
  )

  df$m1[!is.na(df$x)] <- df$m1[!is.na(df$x)] + 0.5 * df$x[!is.na(df$x)]
  df$m2[!is.na(df$x)] <- df$m2[!is.na(df$x)] + 0.4 * df$x[!is.na(df$x)]
  df$y[!is.na(df$x)] <- df$y[!is.na(df$x)] + 0.3 * df$m1[!is.na(df$x)]

  expect_message(
    result <- mediation_parallel(
      data = df, x = "x", mediators = c("m1", "m2"), y = "y",
      boot_samples = 1000, seed = 42
    ),
    "Removed.*missing"
  )

  expect_s3_class(result, "parallel_mediation")
})

test_that("mediation_serial handles missing data", {
  set.seed(42)
  n <- 100

  df <- data.frame(
    x = c(rnorm(95), rep(NA, 5)),
    m1 = rnorm(100),
    m2 = rnorm(100),
    y = rnorm(100)
  )

  expect_message(
    result <- mediation_serial(
      data = df, x = "x", mediators = c("m1", "m2"), y = "y",
      boot_samples = 1000, seed = 42
    ),
    "Removed.*missing"
  )

  expect_s3_class(result, "serial_mediation")
})

test_that("mediation_parallel total equals sum of specific effects", {
  set.seed(42)
  n <- 200

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$x
  df$y <- df$y + 0.3 * df$m1 + 0.2 * df$m2

  result <- mediation_parallel(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, seed = 42
  )

  sum_specific <- sum(result$specific_indirect$indirect_effect)
  total <- result$total_indirect$estimate

  # Should be approximately equal (allowing for numerical precision)
  expect_equal(sum_specific, total, tolerance = 1e-6)
})

test_that("mediation_serial total equals sum of pathway effects", {
  set.seed(42)
  n <- 200

  df <- data.frame(
    x = rnorm(n),
    m1 = rnorm(n),
    m2 = rnorm(n),
    y = rnorm(n)
  )

  df$m1 <- df$m1 + 0.5 * df$x
  df$m2 <- df$m2 + 0.4 * df$m1
  df$y <- df$y + 0.3 * df$m2

  result <- mediation_serial(
    data = df, x = "x", mediators = c("m1", "m2"), y = "y",
    boot_samples = 1000, seed = 42
  )

  sum_pathways <- sum(result$specific_indirect$indirect_effect)
  total <- result$total_indirect$estimate

  expect_equal(sum_pathways, as.numeric(total), tolerance = 1e-6)
})
