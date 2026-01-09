# ----- step_heatmap -----
test_that("step_heatmap generates plot", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_heatmap())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("heatmap" %in% names(res$plots))
})

test_that("step_heatmap works on sig_exp", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )
  # We need DEA to get sig_exp
  bp <- blueprint(
    step_dea_limma(),
    step_heatmap(on = "sig_exp")
  )
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_true("heatmap_sig" %in% names(res$plots))
})

# ----- step_logo -----
test_that("step_logo is skipped for glycomics experiments", {
  skip_if_not_installed("ggseqlogo")
  suppressMessages(
    exp <- glyexp::real_experiment2 |>
      glyexp::slice_head_var(20) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(step_logo())
  suppressMessages(res <- forge_analysis(exp, bp))
  expect_false("logo" %in% names(res$plots))
})
