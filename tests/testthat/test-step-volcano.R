# ----- step_volcano -----
test_that("step_volcano generates plot", {
  suppressMessages(
    exp <- glyexp::real_experiment |>
      glyexp::slice_head_var(10) |>
      glyclean::auto_clean()
  )
  bp <- blueprint(
    step_dea_limma(),
    step_volcano()
  )
  suppressMessages(res <- forge_analysis(exp, bp))
  # Should generate one plot per contrast or just one if single contrast?
  # step_volcano for limma generates plots for each contrast: volcano_Ref_vs_Test
  # Let's check if any plot starts with "volcano"
  expect_true(any(grepl("^volcano", names(res$plots))))
})
