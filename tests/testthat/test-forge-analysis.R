test_that("forge_analysis works for glycomics data", {
  # This is an integration test that checks if all parts of the system work together.
  exp <- glyexp::real_experiment2
  suppressMessages(result <- forge_analysis(exp))
  expect_s3_class(result, "glysmith_result")
})

test_that("forge_analysis works for glycoproteomics data", {
  # This is an integration test that checks if all parts of the system work together.
  exp <- glyexp::real_experiment
  suppressMessages(result <- forge_analysis(exp))
  expect_s3_class(result, "glysmith_result")
})
