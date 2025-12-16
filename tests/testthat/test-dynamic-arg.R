test_that("dynamic argument works in forge_analysis", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(n = 500)
  bp <- blueprint(step_preprocess())
  suppressMessages(res1 <- forge_analysis(
    exp, blueprint = bp,
    preprocess.glyclean.auto_clean.remove_preset = "discovery"
  ))
  suppressMessages(res2 <- forge_analysis(
    exp, blueprint = bp,
    preprocess.glyclean.auto_clean.remove_preset = "biomarker"
  ))
  expect_false(nrow(res1$exp) == nrow(res2$exp))
})

test_that("dynamic argument works in step construction", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(n = 500)
  bp1 <- blueprint(step_preprocess(glyclean.auto_clean.remove_preset = "discovery"))
  bp2 <- blueprint(step_preprocess(glyclean.auto_clean.remove_preset = "biomarker"))
  suppressMessages(res1 <- forge_analysis(exp, blueprint = bp1))
  suppressMessages(res2 <- forge_analysis(exp, blueprint = bp2))
  expect_false(nrow(res1$exp) == nrow(res2$exp))
})

test_that("dynamic arguments in forge_analysis overwrite step-local arguments", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(n = 500)
  bp1 <- blueprint(step_preprocess(glyclean.auto_clean.remove_preset = "discovery"))
  bp2 <- blueprint(step_preprocess(glyclean.auto_clean.remove_preset = "biomarker"))
  suppressMessages(res1 <- forge_analysis(
    exp, blueprint = bp1,
    preprocess.glyclean.auto_clean.remove_preset = "discovery"
  ))
  suppressMessages(res2 <- forge_analysis(
    exp, blueprint = bp2,
    preprocess.glyclean.auto_clean.remove_preset = "discovery"
  ))
  expect_true(nrow(res1$exp) == nrow(res2$exp))
})