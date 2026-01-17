testthat::test_that("all APA7 templates have required sections", {
  templates <- getFromNamespace(".apa7_templates", "consumeR")
  required_sections <- c("methods", "results", "assumptions", "interpretation")

  testthat::expect_true(length(templates) > 0)

  for (test_type in names(templates)) {
    sections <- names(templates[[test_type]])
    testthat::expect_true(all(required_sections %in% sections))
  }
})

testthat::test_that("render_apa7_text interpolates valid values", {
  rendered <- consumeR::render_apa7_text(
    test_type = "t_test",
    section = "results",
    values = list(
      significance_text = "was significant",
      df = 98,
      t_stat = "2.05",
      p_text = "p = .042",
      ci_text = "95% CI [0.12, 3.45]"
    )
  )

  testthat::expect_true(is.character(rendered))
  testthat::expect_match(rendered, "t\\(98\\) = 2.05")
  testthat::expect_match(rendered, "p = \\.042")
})

testthat::test_that("render_apa7_text errors when values are missing", {
  testthat::expect_error(
    consumeR::render_apa7_text(
      test_type = "t_test",
      section = "results",
      values = list(
        significance_text = "was significant",
        df = 98,
        t_stat = "2.05"
      )
    ),
    "p_text|ci_text"  # Match either missing variable
  )
})

testthat::test_that("format_p_apa7 follows APA7 formatting", {
  testthat::expect_identical(consumeR::format_p_apa7(0.042), "p = .042")
  testthat::expect_identical(consumeR::format_p_apa7(0.5), "p = .500")
  testthat::expect_identical(consumeR::format_p_apa7(0.0001), "p < .001")

  testthat::expect_false(grepl("0\\.", consumeR::format_p_apa7(0.042)))
  testthat::expect_false(grepl("0\\.", consumeR::format_p_apa7(0.0001)))
})
