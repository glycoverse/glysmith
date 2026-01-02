test_that("plain arguments work in step construction", {
  exp <- glyexp::real_experiment |>
    glyexp::slice_head_var(n = 500)
  bp1 <- blueprint(step_preprocess(remove_preset = "discovery"))
  bp2 <- blueprint(step_preprocess(remove_preset = "biomarker"))
  suppressMessages(res1 <- forge_analysis(exp, blueprint = bp1))
  suppressMessages(res2 <- forge_analysis(exp, blueprint = bp2))
  expect_false(nrow(res1$exp) == nrow(res2$exp))
})

test_that("dynamic arguments are rejected", {
  expect_error(
    step_preprocess(glyclean.auto_clean.remove_preset = "discovery"),
    "unused argument"
  )
})
