test_that("dynamic argument works", {
  exp <- glyexp::real_experiment
  bp <- blueprint(step_preprocess())
  suppressMessages(res1 <- forge_analysis(exp, blueprint = bp, glyclean.auto_clean.remove_preset = "discovery"))
  suppressMessages(res2 <- forge_analysis(exp, blueprint = bp, glyclean.auto_clean.remove_preset = "biomarker"))
  expect_false(nrow(res1$exp) == nrow(res2$exp))
})