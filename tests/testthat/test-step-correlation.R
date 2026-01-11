# Tests for step_correlation

test_that("step_correlation creates a valid step", {
  step_obj <- step_correlation()
  expect_s3_class(step_obj, "glysmith_step")
  expect_equal(step_obj$id, "correlation")
  expect_equal(step_obj$label, "Correlation analysis")
})

test_that("step_correlation with on parameter creates proper id and label", {
  step_obj <- step_correlation(on = "sig_exp")
  expect_equal(step_obj$id, "correlation_sig")
  # Label format uses "of" instead of parentheses
  expect_true(grepl("significant variables", step_obj$label))
  expect_true(grepl("Correlation", step_obj$label))
})

test_that("step_correlation on sample creates proper id and label", {
  step_obj <- step_correlation(on_cor = "sample")
  expect_equal(step_obj$id, "correlation")
  expect_equal(step_obj$label, "Correlation analysis")
})

test_that("step_correlation with spearman method works", {
  step_obj <- step_correlation(method = "spearman")
  expect_s3_class(step_obj, "glysmith_step")
})

test_that("step_correlation with no p-adjustment works", {
  step_obj <- step_correlation(p_adj_method = NULL)
  expect_s3_class(step_obj, "glysmith_step")
})

test_that("step_correlation runs and generates results", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_correlation())
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("correlation" %in% names(res$tables))
  expect_true("correlation" %in% names(res$plots))
})

test_that("step_correlation on sample runs and generates results", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_correlation(on_cor = "sample"))
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("correlation" %in% names(res$tables))
  expect_true("correlation" %in% names(res$plots))
})

test_that("step_correlation generates correlation table with expected columns", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(5) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_correlation())
  suppressMessages(res <- forge_analysis(exp, bp))

  corr_table <- res$tables$correlation
  expect_true(nrow(corr_table) > 0)
  expect_true("cor" %in% colnames(corr_table))
  expect_true("p_val" %in% colnames(corr_table))
})

test_that("step_correlation with spearman generates results", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_correlation(method = "spearman"))
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("correlation" %in% names(res$tables))
})

test_that("step_correlation report function works", {
  bp <- blueprint(step_correlation())
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  suppressMessages(res <- forge_analysis(exp, bp))

  step <- res$blueprint$correlation
  report_output <- step$report(res)
  expect_type(report_output, "character")
  expect_true(nchar(report_output) > 0)
  expect_true(grepl("Correlation analysis", report_output))
})

test_that("step_correlation on trait_exp works", {
  suppressMessages(
    exp <- glyexp::real_experiment2 |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_derive_traits(),
    step_correlation(on = "trait_exp")
  )
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("correlation_trait" %in% names(res$tables))
})

test_that("step_correlation on motif_exp works", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_quantify_motifs(),
    step_correlation(on = "motif_exp")
  )
  suppressMessages(res <- forge_analysis(exp, bp))

  expect_true("correlation_motif" %in% names(res$tables))
})

test_that("step_correlation on sample runs and generates results", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_correlation(on_cor = "sample"))
  suppressMessages(res <- forge_analysis(exp, bp))

  # Should complete successfully
  expect_true("correlation" %in% names(res$tables))
})
