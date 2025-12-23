test_that("dynamic argument works in step construction", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(n = 500)
  bp1 <- blueprint(step_preprocess(glyclean.auto_clean.remove_preset = "discovery"))
  bp2 <- blueprint(step_preprocess(glyclean.auto_clean.remove_preset = "biomarker"))
  suppressMessages(res1 <- forge_analysis(exp, blueprint = bp1))
  suppressMessages(res2 <- forge_analysis(exp, blueprint = bp2))
  expect_false(nrow(res1$exp) == nrow(res2$exp))
})

test_that("wrong dynamic arguments raise errors", {
  # forge-analysis-style dynamic arguments in steps should raise errors
  expect_snapshot(
    step_preprocess(preprocess.glyclean.auto_clean.remove_preset = "discovery"),
    error = TRUE
  )
})