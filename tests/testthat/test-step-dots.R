test_that("blueprint dots are passed into steps", {
  exp <- glyexp::real_experiment2
  captured <- NULL

  with_mocked_bindings({
    bp <- blueprint(step_preprocess(glyclean.auto_clean.batch_col = "plate"))
    forge_analysis(exp, blueprint = bp)
  }, glyclean::auto_clean = function(exp, batch_col = NULL, ...) {
    captured <<- batch_col
    exp
  })

  expect_equal(captured, "plate")
})

test_that("forge_analysis dots override blueprint dots", {
  exp <- glyexp::real_experiment2
  captured <- NULL

  with_mocked_bindings({
    bp <- blueprint(step_preprocess(glyclean.auto_clean.batch_col = "blueprint"))
    forge_analysis(
      exp,
      blueprint = bp,
      preprocess.glyclean.auto_clean.batch_col = "forge"
    )
  }, glyclean::auto_clean = function(exp, batch_col = NULL, ...) {
    captured <<- batch_col
    exp
  })

  expect_equal(captured, "forge")
})
